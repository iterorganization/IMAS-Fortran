# Multiversion shim integration tests

This directory contains the Tier-1 behavioral suite for issue #63. It drives a
DD 4.1.1 Fortran HLI through `ids_get` and `ids_put` family calls against the
checked-in DD 3.39.0 equilibrium fixture and asserts the contract in
`docs/SHIM_INTEGRATION_CONTRACT.md` rule by rule.

The suite is registered only when `AL_USE_MULTIVERSION_SHIM=ON`. Shim mode is
not enabled by any CI job in this repository today, so these tests do not gate
changes until CI is wired deliberately for a suite that starts red.

## Running the labels

From a configured shim build directory:

```sh
ctest --test-dir <shim-build> -L shim --output-on-failure
ctest --test-dir <shim-build> -L contract-assertion --output-on-failure
ctest --test-dir <shim-build> -L behaviour-pin --output-on-failure
```

`contract-assertion` tests state what the integration contract requires. They
remain red while the shim disagrees; they are not disabled or marked
`WILL_FAIL`. `behaviour-pin` tests preserve an accepted limitation whose shape
must not drift silently. CTest automatically includes the fixture setup tests
needed by a selected scenario.

## Contract assertions known to be red

These are the current reds against the shipped shim. Any other red is a
regression until this table is updated with a reviewed cause.

**Observed, not inferred.** The list below is what
`ctest --test-dir <shim-build> -L shim` printed on 2026-09-09 against the shim
at `install-debug`: **4 failures out of 29**. Do not reconstruct it from
tickets or commit messages — reds that emerge from the combination, or from the
shim changing underneath, appear in no ticket at all. Re-run the suite (it
takes about 2 seconds) and confirm linkage with `otool` first, or the failures
describe a build that converted nothing.

All four remaining reds are shim-side. Every red this repository owned has been
fixed; see "Reds that were this repository's" below. Read that section before
trusting a cause in this table: one of its rows named the shim for a defect
that was the suite's own, and two more named the shim correctly but described
what happened with the labels inverted.

| Test | Observed cause | Verdict printed |
|---|---|---|
| `al-fortran-test-shim-structural-rules` | The shim refuses the multi-candidate AOS paths behind `fold-constraints-j`, `fold-ggd-j` and `fold-ggd-bfield` — *"this path is served by several stored candidates, and only a data read can try them in turn"* — so the converted side is absent for all three. **3 rules, not 4:** `fold-axis-bphi` is now served and green. | `only4`, expected `same` |
| `al-fortran-test-shim-cocos-rules` | Refusing the `constraints/j_phi` merged AOS also leaves its `position/psi` COCOS rule unserved. 1 rule. | `only4`, expected `same` |
| `al-fortran-test-shim-right-only-rules` | **10 of the 13** DD-4-only rules come back as present, disagreeing readings while the loss file records them as `LOSSY`; the contract requires the converted DD 3 side to be absent. The shim serves a value for a path it has nothing to build one from. | `DIFF`, expected `only4` |
| `al-fortran-test-shim-full-put-stamp` | The shim refuses the stamp *delete* (contract section 6) but **not** the stamp *write* (section 5), and then stores `4.1.1` over the stored `3.39.0`. Two assertions are red: the missing write refusal, and the overwritten stamp. The pulse is left advertising a DD version it does not hold, so every later open reads it unconverted. | — |

`structural-rules` and `cocos-rules` used to be recorded here as reporting
`only3`. That was what the suite printed, and it was the wrong label: `only3`
means "a value on the DD 3 side only", and the absent side in both cases is the
converted one, which the module's vocabulary calls `only4`. The reds are the
shim's; the wrong label was in those two programs — see "Which side is which"
below.

The `al-fortran-test-shim-torn-write` behaviour pin is expected to pass: a
refused DD-4-only write leaves the already-written fields and widened
`time_slice` on disk while the traversal continues. `roundtrip-cross-dd` and
`nested-loss` also pass against the current shim; earlier notes that expected
them red predate the shim's `chi_squared` fix.

## Reds that were this repository's, and are fixed

Three rows left this table on 2026-09-09. Recorded rather than deleted: a red
list is also a record of what was believed about each red, and one of these
rows had the owner wrong.

- **`nested-loss` and `refusal-rules`** — both were red because the shim
  refused the four unit-redefined `{x_point,strike_point}/chi_squared_{r,z}`
  paths and recorded them `UNMAPPABLE` (issue #72). The shim now serves them,
  and both tests went green with no change here.

- **`stamp-malformed`** — recorded here as *"a present malformed DD-version
  stamp currently reaches occurrence open without the contract-required
  refusal"*. **That was wrong.** The shim refuses correctly, with exactly the
  reason the contract freezes. The test asserted the refusal on `imas_open`,
  which opens a *data entry* — an object that holds no DD-version stamp, so
  there is nothing there for the shim to read and nothing to refuse. Contract
  section 3 puts the refusal at the *occurrence* open
  (`al_begin_global_action`), which from Fortran is reached by `ids_get`. The
  program's own header said "refuses at occurrence open" while asserting
  somewhere else; the header was right. It now asserts the data-entry open
  forwards and the occurrence open refuses, and it passes.

- **`full-put-stamp`'s first assertion** — the row above is what remains of it.
  The shim's refusal of the stamp delete was real and correct, but the
  generated delete traversal discarded the status at all 7757 of its call
  sites, `ids_delete` had no `retstatus` to report through, and so a tolerated
  refusal never became a partial outcome. That was a defect in this
  repository's generated code, fixed in `IDSDef2F90Routines.xsl`; the
  diagnosis, the four decisions it required and the measured blast radius are
  in `playground/FINDINGS.md`. What is left red is the write half, which is the
  shim's.

## Which side is which: the suite reported inverted verdict labels

Worth its own note, because no test could catch it and it corrupted this file.

`only3` and `only4` name *which side* of a comparison was absent, so a verdict
is only as good as the caller's account of which reading it handed over.
`shim_comparison` took the two sides positionally and documented the order in a
comment. `test_shim_structural_rules` and `test_shim_cocos_rules` passed the
shim-served reading first for every rule they check. Both labels differ from
`same`, so every pass and fail in the suite was identical either way and no
Fortran assertion could be written that failed on the inversion — it was
detectable only by reading the two programs against the comment.

The fix removes the order rather than correcting it. Every public function in
`shim_comparison` now sits behind a dummy argument of a private type, which
makes a positional call a compile error and leaves keyword form as the only way
to call one, so `oracle=` and `converted=` are spelled at all 263 comparison
sites in the suite. The half the compiler cannot see — a call that names both
keywords and feeds them the wrong readings — is checked by
`al-fortran-test-shim-verdict-orientation`
(`check_verdict_orientation.cmake`): with the keyword adjacent to its value
that is a pattern, whereas the positional swap it replaced was not expressible
as one.

## Coverage boundaries

- Only DD 4.1.1 HLI -> DD 3.39.0 storage is tested. The reverse direction
  needs a separate from-scratch DD 3.39.0 build of this library. Consequently,
  the map's 23 `left_only` rules are unreachable; that boundary lifts when a
  two-version-library build exists.
- A rule, not every leaf, is the coverage unit. `new-contour-tree` samples four
  of ten paths and `new-constraints-j-parallel` samples three of thirteen.
  The alias-only merged AOS rules sample one populated child each. A shim that
  served only part of one of those subtrees could therefore escape detection.
- There are no C-ABI tests here. The shim repository owns seam-level tests and
  can inject failures that `ids_get` and `ids_put` cannot expose. This suite
  tests only the public experience of an HLI caller and introduces no binding
  to an `imas_mvdd_*` symbol.
- One assertion reaches below the `ids_get`/`ids_put` surface, and only for a
  message. `test_shim_stamp_malformed` needs the refusal *reason* the contract
  freezes in section 8.5, and `ids_get` has no argument that returns one: the
  refusal arrives at `al_begin_global_action`, whose message the generated
  routine prints and does not pass back. It is read from `al_get_policy`'s
  record of the last non-zero status, guarded by matching the status code
  first. That is this library's own read-side policy module, which the suite
  already uses for the skip log, so the boundary above still holds — but a
  `retmesg` on the generated `ids_get` would let the assertion sit where it
  belongs.

## Refusal and loss channels

The read-side skip log and the shim loss log file are complementary, not
duplicates. The skip log records refusals that the generated HLI traversal
tolerated. The loss file records every non-exact shim outcome, including lossy
successful calls and `UNMAPPABLE` entries; an `UNMAPPABLE` row can represent a
visible refusal or a missing candidate. Absence of a loss file means no loss.
Tests must therefore assert the channel promised for a scenario rather than
expect the two channels to contain the same paths.

Every test supplies a private, cleaned `IMAS_MVDD_LOSS_LOG_DIR`. The loss-log
consumer relies on the format marker, five-line preamble, tab-separated column
order, filename pattern, and directory behavior recorded in section 7 of the
integration contract.

## Asks of the shim

The suite depends on three surfaces that the shim must keep or promote:

1. Keep the loss file's format marker, preamble, column order, filename, and
   `IMAS_MVDD_LOSS_LOG_DIR` semantics as a versioned contract.
2. Publish a flattened machine-readable rule manifest. The externally
   available map has unresolved includes, so this suite currently maintains a
   hand-authored rule table rather than risk silent under-coverage.
3. Provide a preflight check for dynamic loading and ABI compatibility. Today
   a missing or incompatible core library appears as a generic failure that a
   contributor can mistake for a conversion-contract violation.
