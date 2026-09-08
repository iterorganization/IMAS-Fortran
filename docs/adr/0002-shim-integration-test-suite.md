# Testing the multiversion shim: one direction, one tier, red when the shim is wrong

ADR 0001 established that the Fortran HLI can link to the
[IMAS-Multiversion-DD-Loader](https://github.com/yohannmarguier/IMAS-Multiversion-DD-Loader).
Passing the existing suite through that link proves only linkage: it contains no
cross-DD read. The standalone `playground/` comparison is useful diagnosis but
is not a registered behavioural oracle.

`docs/SHIM_INTEGRATION_CONTRACT.md` is the reference for the shim's promises.

## Decision

The behavioural suite lives in `tests/shim/`, is registered with the main
`ctest`, and is built only when `AL_USE_MULTIVERSION_SHIM=ON`.

It has four deliberate boundaries:

- **In tree, not in `playground/`.** The suite must run with this repository's
  tests. `playground/` remains a scratch diagnostic, not the oracle.
- **One direction.** A DD 4.1.1 HLI reads and writes a DD 3.39.0 pulse. The
  reverse direction requires a separate from-scratch 3.39.0 HLI build that this
  repository does not have. The resulting 23 unreachable `left_only` rules are
  an explicit coverage boundary.
- **A rule is the unit, so a subtree rule is asserted on a sample of it.**
  `new-contour-tree` is asserted on four of the ten paths the map counts and
  `new-constraints-j-parallel` on three of thirteen. A `right_only` rule is
  served or it is not, and the sampled leaves are enough to say which, so this
  is the intended consequence of organising by rule rather than by leaf — but
  a shim serving *part* of such a subtree would not be caught. The three
  alias-only `merged` folds (`fold-constraints-j`, `fold-ggd-j`,
  `fold-ggd-bfield`) are likewise asserted on one populated child each.
- **Tier 1 only.** Tests use `ids_get` and `ids_put`, never the shim C ABI.
  C-ABI tests belong in the shim repository. `ids_get` owns and ends its action
  context, so Tier 1 cannot call `imas_mvdd_context_loss_*`; it reads the shim's
  loss-log file instead. No `imas_mvdd_*` symbols are added to `wrapper/`.
- **Contract assertions remain red while the shim disagrees.** An assertion
  states the integration contract, not today's implementation. It is not
  inverted with `WILL_FAIL`, removed from the default suite, or weakened to
  observed behaviour. A failure is a finding about the shim; a fixed shim turns
  that failure green without a custodian changing the test.

The rule table is hand-authored: the externally available conversion-map copy
has unresolved includes, so generation could silently under-cover. Fixtures are
load-bearing generated artifacts, not disposable test data; scenario-specific
stamp variants are derived at build time rather than committed.

**Where a count in the map and a count in a ticket disagree, the map wins.**
Issue #63 and issue #68 both say the map's `<cocos>` block holds 32 paths.
It holds 30, on every commit that has ever touched
`docs/3.39.0--4.1.1.xml` (c0beaaf, 333996f, 6a87941, 2022a17), so 32 is a
stale count rather than a moving target. `shim_rule_table.f90` carries 30 and
states its reasoning at the table. Padding it to 32 would assert agreement on
a path no fixture and no map actually flips — the suite would report a
coverage it does not have, which is worse than either number being wrong on
its own. The two other negated paths that make the total look like 32
(`constraints/j_parallel/position/psi`, `contour_tree/node/psi`) have no DD 3
source, so there is no conversion for a sign flip to be a statement about;
they are asserted as `right_only` rules, which is the whole of what the shim
owes for them.

**Issues #63 and #68 still carry the stale 32 and need correcting there.**
This ADR records the reconciliation; it does not close it.

## Consequences

- The suite is expected to have known contract-assertion failures on arrival,
  including the `chi_squared_{r,z}` paths under `x_point` and `strike_point`.
  The later suite README will record the current red list and its causes.
- Shim mode is not in CI. Adding it to CI requires revisiting this ADR first.
- Tests are organized by conversion rule rather than by leaf: a rule is the
  unit that can be right or wrong, and its fidelity declaration is the oracle.

## Rejected alternatives

- Keeping behavioural coverage in `playground/`: it would not run as part of
  this repository's suite.
- Testing both directions: it would require the unavailable second HLI build.
- Adding Tier 2 C-ABI tests: it duplicates the shim's own suite.
- Marking known-red checks `WILL_FAIL` or disabling them: either would make a
  green run prove less and turn red when the defect is fixed.
