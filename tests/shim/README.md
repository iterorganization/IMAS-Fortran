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

| Test | Current cause |
|---|---|
| `al-fortran-test-shim-nested-loss` | The shim still refuses the four unit-redefined `{x_point,strike_point}/chi_squared_{r,z}` paths and records them as `UNMAPPABLE`; issue #72 requires them to be served with no such entry. |
| `al-fortran-test-shim-structural-rules` | Candidate fallback does not serve `fold-axis-bphi`; the shim also refuses the multi-candidate AOS paths for `fold-constraints-j`, `fold-ggd-j`, and `fold-ggd-bfield`. All four therefore report `only3` instead of `same`. |
| `al-fortran-test-shim-cocos-rules` | Refusing the `constraints/j_phi` merged AOS also leaves its `position/psi` COCOS rule unserved, so it reports `only3` instead of `same`. |
| `al-fortran-test-shim-right-only-rules` | Eleven DD-4-only rules currently return present, disagreeing readings (`DIFF`) while the loss file records them as `LOSSY`; the contract requires the converted DD 3 side to be absent (`only4`). |
| `al-fortran-test-shim-refusal-rules` | The same four issue #72 unit-redefined paths are refused and absent from the converted read. Their contract verdict is `same`, and no unit-redefinition refusal should remain once the shim serves them. |
| `al-fortran-test-shim-full-put-stamp` | The shim refuses removal of the stored stamp during full put, but that tolerated refusal is not reflected as `PARTIAL_PUT`. |
| `al-fortran-test-shim-stamp-malformed` | A present malformed DD-version stamp currently reaches occurrence open without the contract-required refusal. |

The `al-fortran-test-shim-torn-write` behaviour pin is expected to pass: a
refused DD-4-only write leaves the already-written fields and widened
`time_slice` on disk while the traversal continues.

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
