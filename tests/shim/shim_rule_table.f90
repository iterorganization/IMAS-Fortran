! The hand-authored rule table for the shim contract suite's structural rules.
!
! The conversion map this table transcribes lives in the shim's own repository
! at docs/3.39.0--4.1.1.xml (IMAS-Multiversion-DD-Loader), not in this one. It
! is hand-authored here rather than generated from that file because the map
! itself says why generation would silently under-cover: it opens with
!
!   <include href="../common/error-model-3to4.xml"/>
!   <include href="../common/naming-3to4.xml"/>
!
! and neither `common/error-model-3to4.xml` nor `common/naming-3to4.xml`
! exists anywhere the map is reachable from outside the shim's own tree. The
! second one is the one that matters here: it is the include the map's own
! comments say carries the common cross-IDS renames, so a generator walking
! only what resolves would produce a table that looks complete while quietly
! missing an entire rename family. A hand-authored table fails to compile
! instead of failing to notice.
!
! Promoting a flattened, machine-readable rule manifest — so a table like this
! one could be generated and checked against it instead of merely trusted —
! is recorded here as an ask of the shim, alongside the loss-log-format ask
! docs/adr/0002 already names; both are to be gathered into the suite's own
! README (#78) once it lands.
!
! Every entry below cites either a rule id from that map or a section of
! imas-python-fixtures/README.md, so an entry can be checked without reading
! the shim's implementation. This module carries four tables:
!
!   - `structural_rules`, the kinds whose verdict is "the two sides agree"
!     and whose subject matter is structural: identical, renamed, moved,
!     merged (the eight folds where both DD3 spellings carry real data) and
!     split;
!   - `cocos_rules`, per ticket #68, every COCOS 11 -> 17 sign flip the map
!     declares — also expecting the two sides to agree, since a correct
!     conversion leaves the DD 4 HLI values equal;
!   - `right_only_rules`, per ticket #69, the kind whose verdict is "a value
!     on the DD 4 side only" — the paths DD 4 introduced with nothing on the
!     DD 3 side to build them from, so the shim correctly serves nothing;
!   - `refusal_rules`, per ticket #70, the paths the shim refuses to serve.
!
! They share this module because they share its shape and its citation
! discipline, and they are separate tables because their verdicts differ. For
! the refusal rules a refusal is in play for each entry -- correctly for the
! one `retyped` rule, and wrongly for the four unit-`redefined` ones, which
! are red by design until the shim serves them.
!
! Five further `merged` rules exist in the map (fold-constraints-j,
! fold-ggd-j, fold-ggd-bfield, fold-p1d-j, fold-p2d-j) whose only real DD3
! source is the deprecated `_tor` alias — imas-python-fixtures/README.md's
! own "Renames" table lists their targets alongside true renames for that
! reason. Two of those five targets (profiles_1d/j_phi, profiles_2d/j_phi)
! are themselves COCOS paths and are carried below as cocos rules, each
! noting the fold it also rides on; the remaining three are not claimed by
! any rule-table ticket today. That gap is noted here rather than silently
! widening this table's scope to cover it.
!
! ------------------------------------------------------------ COCOS 11 -> 17
!
! Source of truth: IMAS-Multiversion-DD-Loader's docs/3.39.0--4.1.1.xml,
! <transforms><cocos from="11" to="17">...</cocos></transforms>. That block
! lists exactly 30 <flip path="..."/> entries today, each transcribed below
! citing its own path — not 32. imas-python-fixtures/README.md's "COCOS 11 ->
! 17" section and this repository's parent spec (issue #63) both say "32
! paths in the map's <cocos> block"; every commit that has ever touched that
! file (c0beaaf, 333996f, 6a87941, 2022a17) carries the same 30, so 32 looks
! like a stale count rather than a moving target. This table follows the map
! itself rather than the stale count, per this suite's own design principle:
! a hand-authored table fails to compile instead of failing to notice, and
! silently padding it to 32 would be worse than either — it would assert
! agreement on a path nobody's fixture or map actually flips.
!
! Two paths outside the <cocos> block are deliberately not carried here even
! though they are also negated:
!
!   - `time_slice/constraints/j_parallel/position/psi` — flipped by
!     equilibrium_v4_1_1.py for the fixture's own physical self-consistency,
!     but the whole `constraints/j_parallel` subtree is `right_only` (rule
!     "new-constraints-j-parallel": DD3 has no j_parallel constraint at all),
!     so the shim never converts a DD3 value into it. It belongs to the
!     right_only rule kind, not this one.
!   - `time_slice/contour_tree/node/psi` — the fixtures README's own "COCOS
!     11 -> 17" section names this as one quantity outside the map that is
!     "also negated", since `contour_tree` has no DD3 source at all
!     (`new-contour-tree`, right_only). Same reasoning as above.
!
! Note what this means, since 30 + these 2 = 32 and the coincidence invites
! the conclusion that they are the missing two: a sign flip is a statement
! about a conversion, and for these two paths there is no conversion to make
! a statement about. The shim has no DD3 value to negate, so there is no
! COCOS behaviour of the shim's to assert — only the DD 4.1.1 fixture's own
! internal sign convention, which is the oracle's business and not this
! suite's. Asserting them here would test the fixture, not the shim. They are
! asserted as right_only rules, which is the whole of what the shim owes for
! them, and the map's <cocos> block remains the only source for this table.
!
! `global_quantities/psi_magnetic_axis` is both a cocos rule here and the
! split-psi-axis structural rule above: the map's own <flip> entry for it
! carries `note="target of split-psi-axis"`, and both are worth asserting
! independently rather than assuming one implies the other.
! `profiles_1d/j_phi` and `profiles_2d/j_phi` are similarly both a cocos rule
! here and (per the note above) an obsolescent-alias fold not otherwise
! claimed by the structural table.
module shim_rule_table
  use shim_comparison, only: verdict_len, VERDICT_SAME, VERDICT_ONLY4
  implicit none
  private

  integer, parameter, public :: rule_kind_identical = 1
  integer, parameter, public :: rule_kind_renamed   = 2
  integer, parameter, public :: rule_kind_moved     = 3
  integer, parameter, public :: rule_kind_merged    = 4
  integer, parameter, public :: rule_kind_split     = 5
  ! Numbered by ticket, so sibling branches off one base cannot claim the same
  ! integer: 6 is the COCOS kind (#68), 7 right_only (#69), and 8 and 9 the
  ! two kinds for the paths the shim refuses today (#70) -- one where refusing
  ! is right, one where it is the defect. That discipline is load-bearing: #68
  ! and #69 did both reach for 6 on branches off a shared base, and taken
  ! silently the tables would still have compiled with every right_only rule
  ! expecting `same`.
  integer, parameter, public :: rule_kind_cocos     = 6
  integer, parameter, public :: rule_kind_right_only = 7
  integer, parameter, public :: rule_kind_retyped   = 8
  integer, parameter, public :: rule_kind_redefined = 9

  type, public :: rule_entry
    ! Wide enough for the longest map rule id in either table
    ! ("new-global-quantities-rho-tor-boundary", 38). A structure constructor
    ! truncates silently, and a truncated id simply stops matching the name a
    ! test looks it up by, so this has headroom on purpose.
    character(len=48)  :: id
    integer             :: kind
    character(len=96)   :: hli_path
    character(len=200)  :: source
  end type rule_entry

  integer, parameter, public :: structural_rule_count = 20

  type(rule_entry), parameter, public :: structural_rules(structural_rule_count) = [ &
    ! -- identical: unclaimed paths falling through the map's own default rule --
    rule_entry('identical-vacuum-r0', rule_kind_identical, &
      'vacuum_toroidal_field/r0', &
      'map <default rel="identical"/>, confirmed by <coverage scope="vacuum_toroidal_field" forward="exact" reverse="exact"/>'), &
    rule_entry('identical-time', rule_kind_identical, &
      'time', &
      'map <default rel="identical"/>, confirmed by <coverage scope="time" forward="exact" reverse="exact"/>'), &
    rule_entry('identical-beta-pol', rule_kind_identical, &
      'time_slice/global_quantities/beta_pol', &
      'map <default rel="identical"/>; unclaimed by any explicit rule'), &
    ! -- renamed: five rules, one DD3 name replaced by one DD4 name --
    rule_entry('rename-beta-normal', rule_kind_renamed, &
      'time_slice/global_quantities/beta_tor_norm', &
      'map rule "rename-beta-normal"; fixtures README Renames row "global_quantities/beta_normal"'), &
    rule_entry('rename-bpol-probe', rule_kind_renamed, &
      'time_slice/constraints/b_field_pol_probe/measured', &
      'map rule "rename-bpol-probe"; fixtures README Renames row "constraints/bpol_probe"'), &
    rule_entry('rename-mse-polarisation-angle', rule_kind_renamed, &
      'time_slice/constraints/mse_polarization_angle/measured', &
      'map rule "rename-mse-polarisation-angle"; fixtures README Renames row "constraints/mse_polarisation_angle"'), &
    rule_entry('rename-magnetisation-r', rule_kind_renamed, &
      'time_slice/constraints/iron_core_segment/magnetization_r/measured', &
      'map rule "rename-magnetisation-r"; fixtures README Renames row "iron_core_segment/magnetisation_r"'), &
    rule_entry('rename-magnetisation-z', rule_kind_renamed, &
      'time_slice/constraints/iron_core_segment/magnetization_z/measured', &
      'map rule "rename-magnetisation-z"; fixtures README Renames row "iron_core_segment/magnetisation_z"'), &
    ! -- moved: three rules, a subtree relocated from boundary_separatrix to boundary --
    rule_entry('move-closest-wall-point', rule_kind_moved, &
      'time_slice/boundary/closest_wall_point', &
      'map rule "move-closest-wall-point"; fixtures README "Container and structure changes" row "boundary_separatrix/{closest_wall_point,...}"'), &
    rule_entry('move-dr-dz-zero-point', rule_kind_moved, &
      'time_slice/boundary/dr_dz_zero_point', &
      'map rule "move-dr-dz-zero-point"; fixtures README "Container and structure changes" row "boundary_separatrix/{...,dr_dz_zero_point,...}"'), &
    rule_entry('move-gap', rule_kind_moved, &
      'time_slice/boundary/gap', &
      'map rule "move-gap"; fixtures README "Container and structure changes" row "boundary_separatrix/{...,gap}"'), &
    ! -- merged: the eight folds where both DD3 spellings carry real data --
    rule_entry('fold-p2d-br', rule_kind_merged, &
      'time_slice/profiles_2d/b_field_r', &
      'map rule "fold-p2d-br"; fixtures README Folds "profiles_2d/b_r+b_field_r"'), &
    rule_entry('fold-p2d-bz', rule_kind_merged, &
      'time_slice/profiles_2d/b_field_z', &
      'map rule "fold-p2d-bz"; fixtures README Folds "profiles_2d/...b_z+b_field_z"'), &
    rule_entry('fold-p2d-bphi', rule_kind_merged, &
      'time_slice/profiles_2d/b_field_phi', &
      'map rule "fold-p2d-bphi"; fixtures README Folds "profiles_2d/...b_tor+b_field_tor"'), &
    ! Known red on arrival (contract assertion, not inverted or weakened —
    ! see docs/adr/0002): the DD 3.39.0 fixture holds a real value at
    ! precedence-2 (`global_quantities/magnetic_axis/b_field_tor`, confirmed
    ! on disk), but the shim's cross-version read of this path comes back
    ! not-found rather than falling back to it. The structurally identical
    ! 3-way merge fold-p2d-bphi, one struct level shallower (a profiles_2d
    ! array element rather than the scalar global_quantities/magnetic_axis
    ! sub-struct), resolves correctly, which narrows this to something about
    ! candidate fallback at this particular nesting shape. Not chased here —
    ! that is shim work.
    rule_entry('fold-axis-bphi', rule_kind_merged, &
      'time_slice/global_quantities/magnetic_axis/b_field_phi', &
      'map rule "fold-axis-bphi"; fixtures README Folds "magnetic_axis/b_tor+b_field_tor"'), &
    rule_entry('fold-p1d-baverage', rule_kind_merged, &
      'time_slice/profiles_1d/b_field_average', &
      'map rule "fold-p1d-baverage"; fixtures README Folds "profiles_1d/b_average+b_field_average"'), &
    rule_entry('fold-p1d-bmax', rule_kind_merged, &
      'time_slice/profiles_1d/b_field_max', &
      'map rule "fold-p1d-bmax"; fixtures README Folds "profiles_1d/...b_max+b_field_max"'), &
    rule_entry('fold-p1d-bmin', rule_kind_merged, &
      'time_slice/profiles_1d/b_field_min', &
      'map rule "fold-p1d-bmin"; fixtures README Folds "profiles_1d/...b_min+b_field_min"'), &
    rule_entry('fold-energy-mhd', rule_kind_merged, &
      'time_slice/global_quantities/energy_mhd', &
      'map rule "fold-energy-mhd"; fixtures README Folds "global_quantities/w_mhd+energy_mhd"'), &
    ! -- split: one rule, one DD3 source feeding two DD4 targets --
    rule_entry('split-psi-axis', rule_kind_split, &
      'time_slice/global_quantities/{psi_axis,psi_magnetic_axis}', &
      'map rule "split-psi-axis"; fixtures README "Container and structure changes" row "global_quantities/psi_axis"') &
  ]

  integer, parameter, public :: cocos_rule_count = 30

  ! Every entry cites the exact <flip path="..."/> line in the map's
  ! <transforms><cocos from="11" to="17"> block (see the module header for
  ! why this is 30 entries, not the 32 commonly quoted elsewhere) plus the
  ! fixtures README's "COCOS 11 -> 17" section, which is where
  ! equilibrium_v4_1_1.py's independently-authored oracle applies the same
  ! flip at the point of use.
  type(rule_entry), parameter, public :: cocos_rules(cocos_rule_count) = [ &
    rule_entry('cocos-boundary-psi', rule_kind_cocos, &
      'time_slice/boundary/psi', &
      'map <cocos> flip path="time_slice/boundary/psi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-flux-loop-measured', rule_kind_cocos, &
      'time_slice/constraints/flux_loop/measured', &
      'map <cocos> flip path="time_slice/constraints/flux_loop/measured"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-flux-loop-reconstructed', rule_kind_cocos, &
      'time_slice/constraints/flux_loop/reconstructed', &
      'map <cocos> flip path="time_slice/constraints/flux_loop/reconstructed"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-ip-measured', rule_kind_cocos, &
      'time_slice/constraints/ip/measured', &
      'map <cocos> flip path="time_slice/constraints/ip/measured"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-ip-reconstructed', rule_kind_cocos, &
      'time_slice/constraints/ip/reconstructed', &
      'map <cocos> flip path="time_slice/constraints/ip/reconstructed"; fixtures README "COCOS 11 -> 17"'), &
    ! Known red on arrival (contract assertion, not inverted or weakened —
    ! see docs/adr/0002): the shim refuses the whole `constraints/j_phi`
    ! subtree on the cross-version read rather than serving it, reason
    ! "this path is served by several stored candidates, and only a data
    ! read can try them in turn" — DD3 offers it two ways (`j_phi` itself
    ! and the obsolescent `j_tor` alias the fold-constraints-j merge folds
    ! in), and the shim's path-level resolution won't pick between them the
    ! way it does for a plain scalar fold such as fold-p2d-bphi. This
    ! leaves the AOS unassociated (not merely empty) on the cross side, so
    ! this rule's DD4-only verdict is a real, reproducible finding, not a
    ! flaky read — confirmed with a bounds- and pointer-checked build.
    ! Cousin of fold-axis-bphi above: both are candidate-fallback gaps at
    ! particular nesting/cardinality shapes, not this suite's to fix.
    rule_entry('cocos-j-phi-position-psi', rule_kind_cocos, &
      'time_slice/constraints/j_phi/position/psi', &
      'map <cocos> flip path="time_slice/constraints/j_phi/position/psi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-n-e-position-psi', rule_kind_cocos, &
      'time_slice/constraints/n_e/position/psi', &
      'map <cocos> flip path="time_slice/constraints/n_e/position/psi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-pf-current-measured', rule_kind_cocos, &
      'time_slice/constraints/pf_current/measured', &
      'map <cocos> flip path="time_slice/constraints/pf_current/measured"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-pf-current-reconstructed', rule_kind_cocos, &
      'time_slice/constraints/pf_current/reconstructed', &
      'map <cocos> flip path="time_slice/constraints/pf_current/reconstructed"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-pressure-position-psi', rule_kind_cocos, &
      'time_slice/constraints/pressure/position/psi', &
      'map <cocos> flip path="time_slice/constraints/pressure/position/psi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-pressure-rot-position-psi', rule_kind_cocos, &
      'time_slice/constraints/pressure_rotational/position/psi', &
      'map <cocos> flip path="time_slice/constraints/pressure_rotational/position/psi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-q-position-psi', rule_kind_cocos, &
      'time_slice/constraints/q/position/psi', &
      'map <cocos> flip path="time_slice/constraints/q/position/psi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-ggd-psi-values', rule_kind_cocos, &
      'time_slice/ggd/psi/values', &
      'map <cocos> flip path="time_slice/ggd/psi/values"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-gq-ip', rule_kind_cocos, &
      'time_slice/global_quantities/ip', &
      'map <cocos> flip path="time_slice/global_quantities/ip"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-psi-axis', rule_kind_cocos, &
      'time_slice/global_quantities/psi_axis', &
      'map <cocos> flip path="time_slice/global_quantities/psi_axis"; fixtures README "COCOS 11 -> 17"'), &
    ! Also the split-psi-axis structural rule's second target; the map's own
    ! <flip> entry for this path carries note="target of split-psi-axis".
    rule_entry('cocos-psi-magnetic-axis', rule_kind_cocos, &
      'time_slice/global_quantities/psi_magnetic_axis', &
      'map <cocos> flip path="time_slice/global_quantities/psi_magnetic_axis" note="target of split-psi-axis"; also rule "split-psi-axis"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-psi-boundary', rule_kind_cocos, &
      'time_slice/global_quantities/psi_boundary', &
      'map <cocos> flip path="time_slice/global_quantities/psi_boundary"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-psi-external-average', rule_kind_cocos, &
      'time_slice/global_quantities/psi_external_average', &
      'map <cocos> flip path="time_slice/global_quantities/psi_external_average"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-v-external', rule_kind_cocos, &
      'time_slice/global_quantities/v_external', &
      'map <cocos> flip path="time_slice/global_quantities/v_external"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-p1d-darea-dpsi', rule_kind_cocos, &
      'time_slice/profiles_1d/darea_dpsi', &
      'map <cocos> flip path="time_slice/profiles_1d/darea_dpsi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-p1d-dpressure-dpsi', rule_kind_cocos, &
      'time_slice/profiles_1d/dpressure_dpsi', &
      'map <cocos> flip path="time_slice/profiles_1d/dpressure_dpsi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-p1d-dpsi-drho-tor', rule_kind_cocos, &
      'time_slice/profiles_1d/dpsi_drho_tor', &
      'map <cocos> flip path="time_slice/profiles_1d/dpsi_drho_tor"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-p1d-dvolume-dpsi', rule_kind_cocos, &
      'time_slice/profiles_1d/dvolume_dpsi', &
      'map <cocos> flip path="time_slice/profiles_1d/dvolume_dpsi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-p1d-f-df-dpsi', rule_kind_cocos, &
      'time_slice/profiles_1d/f_df_dpsi', &
      'map <cocos> flip path="time_slice/profiles_1d/f_df_dpsi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-p1d-j-parallel', rule_kind_cocos, &
      'time_slice/profiles_1d/j_parallel', &
      'map <cocos> flip path="time_slice/profiles_1d/j_parallel"; fixtures README "COCOS 11 -> 17"'), &
    ! Also rides the fold-p1d-j obsolescent-alias merge (DD3 has only
    ! `j_tor`/`j_phi` aliases collapsing here; see the module header).
    rule_entry('cocos-p1d-j-phi', rule_kind_cocos, &
      'time_slice/profiles_1d/j_phi', &
      'map <cocos> flip path="time_slice/profiles_1d/j_phi"; also rule "fold-p1d-j"; fixtures README "COCOS 11 -> 17" and Renames'), &
    rule_entry('cocos-p1d-psi', rule_kind_cocos, &
      'time_slice/profiles_1d/psi', &
      'map <cocos> flip path="time_slice/profiles_1d/psi"; fixtures README "COCOS 11 -> 17"'), &
    rule_entry('cocos-p2d-j-parallel', rule_kind_cocos, &
      'time_slice/profiles_2d/j_parallel', &
      'map <cocos> flip path="time_slice/profiles_2d/j_parallel"; fixtures README "COCOS 11 -> 17"'), &
    ! Also rides the fold-p2d-j obsolescent-alias merge; see cocos-p1d-j-phi.
    rule_entry('cocos-p2d-j-phi', rule_kind_cocos, &
      'time_slice/profiles_2d/j_phi', &
      'map <cocos> flip path="time_slice/profiles_2d/j_phi"; also rule "fold-p2d-j"; fixtures README "COCOS 11 -> 17" and Renames'), &
    rule_entry('cocos-p2d-psi', rule_kind_cocos, &
      'time_slice/profiles_2d/psi', &
      'map <cocos> flip path="time_slice/profiles_2d/psi"; fixtures README "COCOS 11 -> 17"') &
  ]

  ! The map's section 6, "DD4-only: nothing on the left to build these from",
  ! carries thirteen `right_only` rules, every one of them rooted under
  ! `time_slice`. Three are subtrees — `contour_tree` (10 paths),
  ! `constraints/j_parallel` (13 paths) and `convergence/result` (2 paths) —
  ! and the remaining ten are single leaves. Issue #63's rule/verdict table
  ! names this set "contour_tree/* and the 13 under time_slice"; the count of
  ! rules is thirteen with `contour_tree` among them, not alongside them.
  !
  ! Nothing here is a shim defect: the DD 3.39.0 pulse genuinely has no source
  ! for any of these, so serving nothing is the correct behaviour and this
  ! table asserts it. What the assertion buys is the other direction — the DD
  ! 4.1.1 fixture fills every one of them (imas-python-fixtures/README.md,
  ! "One reality, not two": a one-sided field is filled with a value belonging
  ! to this equilibrium, never a placeholder), so a `only4` verdict proves the
  ! oracle holds a real value that the shim did not invent.
  !
  ! ------------------------------------------------------------------------
  ! Expected reds on arrival: five entries return a value where nothing at all
  ! should arrive (contract assertions, not inverted or weakened - docs/adr/0002
  ! and issue #63's "Red by design").
  !
  !   new-boundary-rho-tor, new-constraints-chi-squared-reduced,
  !   new-constraints-freedom-degrees-n, new-constraints-constraints-n and
  !   new-convergence-result
  !
  ! come back `DIFF` rather than `only4`: the cross-version read reports a
  ! value present. It is not stored data - `h5ls -r` on the checked-in DD
  ! 3.39.0 pulse shows no dataset for any of the five - and it is not the
  ! invalid sentinel either. It is uninitialised memory, and it reads as such:
  ! both affected reals come back as the same 6.0135E-154, both affected
  ! integers as the same -932149305, and convergence/result/index as
  ! 538976288, which is 0x20202020 - four ASCII spaces, a blank-padded string
  ! buffer landing on an integer field.
  !
  ! That is worse than the failure mode issue #63's user story 37 asks the
  ! suite to catch. A refused field arriving as a default value would at least
  ! be a recognisable number; arriving as uninitialised memory means a caller
  ! testing `/= ids_real_invalid` concludes the field was served.
  !
  ! One correlation is worth recording for whoever chases it, offered as an
  ! observation and not as a root cause: each of the five sits immediately
  ! after an array of structures in its containing derived type, and the three
  ! `constraints` leaves follow `strike_point(:)` - the very array whose
  ! chi_squared_{r,z} the shim refuses during this same read, printing SKIPPED
  ! twice before these fields are filled. It is not the arraystruct-refusal
  ! double close: that fix is already in this branch. Not chased here, in
  ! keeping with the standing rule that a defect this suite exposes is
  ! diagnosed and asserted against rather than repaired from inside the test
  ! tree.
  ! ------------------------------------------------------------------------
  integer, parameter, public :: right_only_rule_count = 13

  type(rule_entry), parameter, public :: right_only_rules(right_only_rule_count) = [ &
    ! -- subtrees: DD 4 containers with no DD 3 counterpart at all --
    rule_entry('new-contour-tree', rule_kind_right_only, &
      'time_slice/contour_tree', &
      'map rule "new-contour-tree" (subtree, 10 paths); fixtures README change table row "- | contour_tree | new" and its "One reality, not two" bullet'), &
    rule_entry('new-constraints-j-parallel', rule_kind_right_only, &
      'time_slice/constraints/j_parallel', &
      'map rule "new-constraints-j-parallel" (subtree, 13 paths; added in DD 3.40.0); DD3 constraints has j_phi/j_tor and no j_parallel entry at all'), &
    ! Known red on arrival; see "Expected reds on arrival" above.
    rule_entry('new-convergence-result', rule_kind_right_only, &
      'time_slice/convergence/result', &
      'map rule "new-convergence-result" (subtree, 2 paths: result and result/index; added in DD 3.41.0)'), &
    ! -- boundary: three leaves DD 4 added to the selected-boundary struct --
    ! Known red on arrival; see "Expected reds on arrival" above.
    rule_entry('new-boundary-rho-tor', rule_kind_right_only, &
      'time_slice/boundary/rho_tor', &
      'map rule "new-boundary-rho-tor"; fixtures README "One reality, not two" bullet "boundary/rho_tor (DD 4 only)"'), &
    rule_entry('new-boundary-phi', rule_kind_right_only, &
      'time_slice/boundary/phi', &
      'map rule "new-boundary-phi"; equilibrium_v4_1_1.py _boundary, "New in DD 4 (rules new-boundary-rho-tor / -phi / -phi-poloidal-current)"'), &
    rule_entry('new-boundary-phi-poloidal-current', rule_kind_right_only, &
      'time_slice/boundary/phi_poloidal_current', &
      'map rule "new-boundary-phi-poloidal-current"; equilibrium_v4_1_1.py _boundary, same "New in DD 4" block'), &
    ! -- global_quantities: leaves added in DD 3.40.0, so absent from 3.39.0 --
    rule_entry('new-q-min-psi', rule_kind_right_only, &
      'time_slice/global_quantities/q_min/psi', &
      'map rule "new-q-min-psi" (added in DD 3.40.0, no counterpart in 3.39.0); equilibrium_v4_1_1.py "g.q_min.psi = flip(...) # DD 4 only, COCOS"'), &
    rule_entry('new-q-min-psi-norm', rule_kind_right_only, &
      'time_slice/global_quantities/q_min/psi_norm', &
      'map rule "new-q-min-psi-norm" (added in DD 3.40.0); equilibrium_v4_1_1.py "g.q_min.psi_norm = ... # DD 4 only"'), &
    rule_entry('new-global-quantities-rho-tor-boundary', rule_kind_right_only, &
      'time_slice/global_quantities/rho_tor_boundary', &
      'map rule "new-global-quantities-rho-tor-boundary" (added in DD 3.40.0); equilibrium_v4_1_1.py "g.rho_tor_boundary = ... # DD 4 only"'), &
    ! -- constraints: the DD 4 goodness-of-fit summary of the reconstruction.
    !    All three are known red on arrival; see "Expected reds on arrival"
    !    above. All three sit immediately after strike_point(:) in the
    !    generated constraints type, which is where that note starts. --
    rule_entry('new-constraints-chi-squared-reduced', rule_kind_right_only, &
      'time_slice/constraints/chi_squared_reduced', &
      'map rule "new-constraints-chi-squared-reduced" (added in DD 3.40.0); equilibrium_v4_1_1.py _constraints, "New in DD 4" block'), &
    rule_entry('new-constraints-freedom-degrees-n', rule_kind_right_only, &
      'time_slice/constraints/freedom_degrees_n', &
      'map rule "new-constraints-freedom-degrees-n" (added in DD 3.40.0); equilibrium_v4_1_1.py _constraints, "New in DD 4" block'), &
    rule_entry('new-constraints-constraints-n', rule_kind_right_only, &
      'time_slice/constraints/constraints_n', &
      'map rule "new-constraints-constraints-n" (added in DD 3.40.0); equilibrium_v4_1_1.py _constraints, "New in DD 4" block'), &
    ! -- profiles_1d: a normalised flux coordinate DD 4 stores explicitly --
    rule_entry('new-profiles-1d-psi-norm', rule_kind_right_only, &
      'time_slice/profiles_1d/psi_norm', &
      'map rule "new-profiles-1d-psi-norm" (added in DD 3.40.0); equilibrium_v4_1_1.py "p.psi_norm = ... # DD 4 only; a ratio, so no flip"') &
  ]


  ! -------------------------------------------------------------------------
  ! The paths the shim refuses to serve today.
  !
  ! That is what collects these five entries: the refusal is an observation
  ! about current behaviour, not the expectation. The expectation differs per
  ! kind, and for four of the five the refusal is itself the defect:
  !
  !   retyped   -- the refusal is correct. No value transformation reshapes
  !                an INT_1D into an array of identifier structures, so the
  !                path genuinely cannot be served and the expected verdict
  !                is a tolerated refusal.
  !   redefined -- the refusal is wrong, and these entries are RED BY DESIGN
  !                until the shim is fixed. The map marks the four
  !                chi_squared paths fidelity="unmappable"; that marking is
  !                the defect. Both dictionaries hold the same number
  !                (imas-python-fixtures/README.md, "Redefinitions the map
  !                refuses", writes the DD 3 number unchanged into both
  !                fixtures), so the expected verdict is that the two sides
  !                agree, and the assertion says so rather than recording
  !                what the shim does today.
  !
  ! Per docs/adr/0002, a contract assertion is not inverted, quarantined or
  ! weakened to match observed behaviour: the four reds turn green by
  ! themselves when the shim serves these paths, with nobody having to
  ! remember to come back and flip an expectation. ADR 0002's Consequences
  ! already lists these four as contract assertions known to be red on arrival.
  ! -------------------------------------------------------------------------
  integer, parameter, public :: refusal_rule_count = 5

  type(rule_entry), parameter, public :: refusal_rules(refusal_rule_count) = [ &
    ! -- retyped: the map's one rel="retyped" rule.  Refused unconditionally,
    !    even though the rule declares itself fidelity="exact", because no
    !    value transformation reshapes an INT_1D into an identifier struct
    !    array (playground/FINDINGS.md, "The trigger"). --
    rule_entry('retype-coordinates-type', rule_kind_retyped, &
      'grids_ggd/grid/space/coordinates_type', &
      'map rule "retype-coordinates-type" rel="retyped" shape="int_1d:struct_array"; contract 8.2 "container changed shape"'), &
    ! -- redefined: the map's four <redefine> globs, declared unmappable
    !    there on the grounds that m -> m^-2 normalises chi-squared by a
    !    measurement variance no factor recovers.  Asserted as served
    !    regardless, because the two fixtures hold the same number and the
    !    contract expects it delivered: the unmappable marking is the thing
    !    these four entries are red about. --
    rule_entry('redefine-x-point-chi-sq-r', rule_kind_redefined, &
      'time_slice/constraints/x_point/chi_squared_r', &
      'map <redefine glob="time_slice/constraints/x_point/chi_squared_r" left-units="m" right-units="m^-2"> unmappable'), &
    rule_entry('redefine-x-point-chi-sq-z', rule_kind_redefined, &
      'time_slice/constraints/x_point/chi_squared_z', &
      'map <redefine glob="time_slice/constraints/x_point/chi_squared_z" left-units="m" right-units="m^-2"> unmappable'), &
    rule_entry('redefine-strike-pt-chi-sq-r', rule_kind_redefined, &
      'time_slice/constraints/strike_point/chi_squared_r', &
      'map <redefine glob="time_slice/constraints/strike_point/chi_squared_r" left-units="m" right-units="m^-2"> unmappable'), &
    rule_entry('redefine-strike-pt-chi-sq-z', rule_kind_redefined, &
      'time_slice/constraints/strike_point/chi_squared_z', &
      'map <redefine glob="time_slice/constraints/strike_point/chi_squared_z" left-units="m" right-units="m^-2"> unmappable') &
  ]

  ! The reason strings the contract froze for these two kinds
  ! (docs/SHIM_INTEGRATION_CONTRACT.md 8.2, "Path-resolution reasons").
  ! Asserting the reason rather than only the status code is what makes a
  ! failure say which rule fired instead of merely that something refused,
  ! and it is why these two strings cannot be reworded without a contract
  ! change.
  character(len=*), parameter, public :: retyped_refusal_reason = &
    "this path's container changed shape and cannot be served"
  character(len=*), parameter, public :: redefined_refusal_reason = &
    "this path's unit was redefined and cannot be converted"

  public :: expected_verdict_for_kind, kind_name

contains

  ! Kind-to-verdict is fixed and stated once, here, rather than per entry.
  !
  ! `retyped` and `right_only` are the two kinds that expect the shim to have
  ! served nothing; every other kind, `redefined` included, expects the two
  ! sides to agree.
  !
  ! `only4` is a verdict about argument order as much as about the data: the
  ! comparison primitives take the DD 4 side first and the shim-served side
  ! second, so `only4` means "the DD 4 oracle has a value and the shim served
  ! nothing". Callers asserting a right_only rule must pass the two reads in
  ! that order; the reverse order would report `only3` for the same reading.
  function expected_verdict_for_kind(kind) result(verdict)
    integer, intent(in) :: kind
    character(len=verdict_len) :: verdict

    select case (kind)
    case (rule_kind_identical, rule_kind_renamed, rule_kind_moved, rule_kind_merged, rule_kind_split, rule_kind_cocos)
      verdict = VERDICT_SAME
    case (rule_kind_right_only)
      ! Same shape as the retyped case below and for a different reason: not
      ! a refusal, but a path with no DD 3 source to serve from at all.  The
      ! skip log therefore does not name these, so unlike a retyped refusal
      ! the verdict is the whole assertion.
      verdict = VERDICT_ONLY4
    case (rule_kind_retyped)
      ! What a refusal looks like through the comparison primitives: the
      ! DD 4.1.1 control read holds the value and the shim's cross-version
      ! read holds nothing.  That reads as 'only4' only when the control is
      ! the first argument and the shim read the second, which is the order
      ! test_shim_refusal_rules uses and states.
      !
      ! The verdict is half the assertion.  A field can also be absent
      ! because the pulse never held it, so the refusal test pairs this
      ! 'only4' with a named entry in the read-side skip log.
      verdict = VERDICT_ONLY4
    case (rule_kind_redefined)
      ! Red by design: the shim refuses these today, so the observed verdict
      ! is 'only4' and this expectation fails until the shim serves them.
      ! Stated as the contract has it, not as the shim behaves.
      !
      ! Two tickets ask for opposite things here and this line follows one of
      ! them deliberately. Issue #70 AC6 (and issue #63 US38) say "a quantity
      ! whose unit the map records as redefined is asserted refused rather
      ! than served", which would make this 'only4'. Issue #63's own rule
      ! table and issue #72 say the four chi_squared paths must be asserted
      ! as served, which makes it 'same'. The rule table wins: 'refused' is
      ! what the shim does, and ADR 0002 says an assertion states the
      ! contract rather than today's behaviour, so writing 'only4' here would
      ! be exactly the weakening that ADR forbids -- the four reds would go
      ! green while the defect stayed. #70 AC6 and #63 US38 need withdrawing
      ! in the tracker.
      verdict = VERDICT_SAME
    case default
      error stop 'shim_rule_table: unhandled rule kind'
    end select
  end function expected_verdict_for_kind

  function kind_name(kind) result(name)
    integer, intent(in) :: kind
    character(len=10) :: name

    select case (kind)
    case (rule_kind_identical)
      name = 'identical'
    case (rule_kind_renamed)
      name = 'renamed'
    case (rule_kind_moved)
      name = 'moved'
    case (rule_kind_merged)
      name = 'merged'
    case (rule_kind_split)
      name = 'split'
    case (rule_kind_retyped)
      name = 'retyped'
    case (rule_kind_redefined)
      name = 'redefined'
    case (rule_kind_cocos)
      name = 'cocos'
    case (rule_kind_right_only)
      name = 'right_only'
    case default
      name = 'unknown'
    end select
  end function kind_name

end module shim_rule_table
