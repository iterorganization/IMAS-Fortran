! Assert, per rule rather than per leaf, that every `right_only` rule in
! shim_rule_table yields a value on the DD 4 side only when:
!
!   - a DD 3.39.0 pulse is read via a DD 4.1.1 HLI through the shim, and
!   - the DD 4.1.1 fixture is read directly, same-version, plain passthrough.
!
! These are the paths DD 4 introduced with nothing on the DD 3 side to build
! them from. Serving nothing is therefore the correct behaviour, and this
! program asserts it rather than treating it as a defect.
!
! ---------------------------------------------------------------------------
! Argument order matters here, and only here.
!
! The comparison primitives take the DD 4 side first and the shim-served side
! second. With that order a served-nothing reading comes back `only4` — "the
! DD 4 oracle has a value, the shim served nothing" — which is the verdict
! `expected_verdict_for_kind(rule_kind_right_only)` requires. Passing the two
! reads the other way round reports `only3` for the same reading and every
! assertion below would fail for a reason that has nothing to do with the
! shim. test_shim_structural_rules passes them in the opposite order, which
! does not change whether it passes — every verdict it expects is `same`, and
! the primitives reach `same` symmetrically — but it does change how it reads
! when it fails: a structural rule the shim served nothing for prints `only3`,
! "a value on the DD 3 side only", for a reading where the value is in fact on
! the DD 4 side. That is a legibility defect in a failure message rather than
! a wrong result, and it belongs to that test rather than this one.
!
! This is also why the loss log file is a second channel rather than the only
! one: a value comparison on its own distinguishes a served field from a
! refused one, provided the shim-read column is the second argument.
! ---------------------------------------------------------------------------
!
! A failing check names the rule that broke (id, kind and cited source), not
! only the field.
program test_shim_right_only_rules
  use ids_routines, only: ids_equilibrium, ids_real
  use shim_fixture_pair, only: fixture_root_from_command, read_cross_version, &
                               read_same_version, assert_reads_usable
  use shim_comparison, only: verdict_real, verdict_integer, verdict_real_vector_with_stated_presence
  use shim_rule_table, only: right_only_rules, structural_rules, &
                             expected_verdict_for_kind, rule_kind_right_only
  use shim_rule_check, only: rule_checker, verdicts_agree
  use shim_run_guard, only: assert_ran_count
  implicit none

  type(ids_equilibrium) :: eq_cross, eq_control
  character(len=512) :: fixture_root
  integer :: status_cross, status_control
  integer :: demonstrations
  ! Stated up front so a demonstration lost in an edit fails the run rather
  ! than quietly shrinking it.  Raise it when adding one.
  integer, parameter :: expected_demonstrations = 2
  type(rule_checker) :: checker
  logical :: has_cross, has_control
  real(ids_real), allocatable :: cross_values(:), control_values(:)
  character(len=6) :: served_nothing, agreement_expected

  fixture_root = fixture_root_from_command()
  call read_cross_version(fixture_root, eq_cross, status_cross)
  call read_same_version(fixture_root, eq_control, status_control)
  call assert_reads_usable(status_cross, status_control)
  ! Every check below indexes time_slice(1), so an associated but empty array
  ! would be an out-of-bounds read rather than a failed assertion.
  if (.not. has_time_slice(eq_control)) error stop 'control read produced no time slices'
  if (.not. has_time_slice(eq_cross)) error stop 'cross-version read produced no time slices'

  checker%rules = right_only_rules
  checker%marker = 'RIGHT-ONLY-FAILURE'
  demonstrations = 0

  ! -- subtrees: DD 4 containers with no DD 3 counterpart at all --
  !
  ! Presence of a subtree is decided by its array of structures, and the
  ! leaves below are gathered only when that array is associated, so the
  ! served-nothing side is never dereferenced.
  call gather_contour_nodes(eq_control, control_values, has_control)
  call gather_contour_nodes(eq_cross, cross_values, has_cross)
  call checker%check('new-contour-tree', combine([ &
       verdict_real_vector_with_stated_presence(has_control, control_values, has_cross, cross_values), &
       contour_edges_verdict(eq_control, eq_cross)]))

  call gather_j_parallel(eq_control, control_values, has_control)
  call gather_j_parallel(eq_cross, cross_values, has_cross)
  call checker%check('new-constraints-j-parallel', &
       verdict_real_vector_with_stated_presence(has_control, control_values, has_cross, cross_values))

  ! `convergence/result` is an identifier structure whose only leaf in the
  ! map's two-path note is `index`; the shim serving nothing leaves it at the
  ! integer sentinel.
  call checker%check('new-convergence-result', &
       verdict_integer(eq_control%time_slice(1)%convergence%result%index, &
                       eq_cross%time_slice(1)%convergence%result%index))

  ! -- boundary --
  call checker%check('new-boundary-rho-tor', &
       verdict_real(eq_control%time_slice(1)%boundary%rho_tor, &
                    eq_cross%time_slice(1)%boundary%rho_tor))
  call checker%check('new-boundary-phi', &
       verdict_real(eq_control%time_slice(1)%boundary%phi, &
                    eq_cross%time_slice(1)%boundary%phi))
  call checker%check('new-boundary-phi-poloidal-current', &
       verdict_real(eq_control%time_slice(1)%boundary%phi_poloidal_current, &
                    eq_cross%time_slice(1)%boundary%phi_poloidal_current))

  ! -- global_quantities --
  call checker%check('new-q-min-psi', &
       verdict_real(eq_control%time_slice(1)%global_quantities%q_min%psi, &
                    eq_cross%time_slice(1)%global_quantities%q_min%psi))
  call checker%check('new-q-min-psi-norm', &
       verdict_real(eq_control%time_slice(1)%global_quantities%q_min%psi_norm, &
                    eq_cross%time_slice(1)%global_quantities%q_min%psi_norm))
  call checker%check('new-global-quantities-rho-tor-boundary', &
       verdict_real(eq_control%time_slice(1)%global_quantities%rho_tor_boundary, &
                    eq_cross%time_slice(1)%global_quantities%rho_tor_boundary))

  ! -- constraints --
  call checker%check('new-constraints-chi-squared-reduced', &
       verdict_real(eq_control%time_slice(1)%constraints%chi_squared_reduced, &
                    eq_cross%time_slice(1)%constraints%chi_squared_reduced))
  call checker%check('new-constraints-freedom-degrees-n', &
       verdict_integer(eq_control%time_slice(1)%constraints%freedom_degrees_n, &
                       eq_cross%time_slice(1)%constraints%freedom_degrees_n))
  call checker%check('new-constraints-constraints-n', &
       verdict_integer(eq_control%time_slice(1)%constraints%constraints_n, &
                       eq_cross%time_slice(1)%constraints%constraints_n))

  ! -- profiles_1d --
  call gather_p1d_psi_norm(eq_control, control_values, has_control)
  call gather_p1d_psi_norm(eq_cross, cross_values, has_cross)
  call checker%check('new-profiles-1d-psi-norm', &
       verdict_real_vector_with_stated_presence(has_control, control_values, has_cross, cross_values))

  ! -------------------------------------------------------------------------
  ! The hole this closes, demonstrated rather than asserted in prose.
  !
  ! Every `same` expectation elsewhere in this suite is only worth something
  ! if agreement is unreachable when the shim serves nothing. If it were
  ! reachable, a refusal would masquerade as a match and a shim that served
  ! nothing at all would satisfy every structural rule.
  !
  ! So: take a reading the assertions above have just established the shim
  ! serves nothing for, and put it through `agrees` — the same predicate every
  ! `check` above decides on — against the expectation carried by a real entry
  ! of the structural table. The expectation is fetched through
  ! expected_verdict_for_kind from `structural_rules(1)%kind` rather than
  ! written out here, so this demonstrates the rule the suite actually
  ! asserts, and follows it automatically if that mapping ever changes.
  !
  ! The first expectation keeps the demonstration honest — it fails if the
  ! reading is not in fact a served-nothing one, so this cannot pass
  ! vacuously. The second is the property itself.
  !
  ! The anchor is deliberately `global_quantities/rho_tor_boundary`, one of
  ! the eight rules that hold today, and it must stay one of those: anchoring
  ! it on one of the five documented reds would fail the first expectation and
  ! turn a real finding into a confusing demonstration failure. Note it is not
  ! `boundary/rho_tor`, which is a different path and one of the five.
  ! -------------------------------------------------------------------------
  served_nothing = verdict_real(eq_control%time_slice(1)%global_quantities%rho_tor_boundary, &
                                eq_cross%time_slice(1)%global_quantities%rho_tor_boundary)
  agreement_expected = expected_verdict_for_kind(structural_rules(1)%kind)

  call demonstrate(verdicts_agree(served_nothing, expected_verdict_for_kind(rule_kind_right_only)), &
       'the demonstration reading is one the DD 4 side has and the shim served nothing for')
  call demonstrate(.not. verdicts_agree(served_nothing, agreement_expected), &
       'a rule expecting the two sides to agree fails on a reading the shim served nothing for')

  call checker%assert_every_rule_checked(checker%expectations)

  call assert_ran_count(checker%marker, 'demonstrations were run', &
                        demonstrations, expected_demonstrations, checker%failures)

  if (checker%failures > 0) then
    write(*, '(a,i0,a)') 'RIGHT-ONLY-FAILURE: ', checker%failures, ' right_only rule(s) failed'
    stop 1
  end if

contains


  ! A subtree rule combines several leaf verdicts into the one verdict the
  ! rule is judged on. It fails if any leaf does, and the first leaf that does
  ! not meet the rule's expectation is the verdict reported, so the failure
  ! message names what actually went wrong rather than the expectation.
  function combine(verdicts) result(combined)
    character(len=6), intent(in) :: verdicts(:)
    character(len=6) :: combined
    character(len=6) :: expected
    integer :: leaf

    expected = expected_verdict_for_kind(rule_kind_right_only)
    combined = expected
    do leaf = 1, size(verdicts)
      if (.not. verdicts_agree(verdicts(leaf), expected)) then
        combined = verdicts(leaf)
        return
      end if
    end do
  end function combine

  ! `edges` is the contour tree's integer connectivity array. Reusing the real
  ! vector primitive for it is exact at node-index magnitudes, and brings the
  ! subtree's other array under the same verdict as its nodes.
  !
  ! This rule is asserted on a sample of its subtree, not all of it: node
  ! psi/r/z and edges, four of the ten paths the map counts, leaving
  ! node/critical_type and node/levelset/{r,z}. Same for
  ! new-constraints-j-parallel, three leaves of thirteen. That is the point of
  ! asserting per rule rather than per leaf — a right_only rule is served or
  ! it is not, and the sampled leaves are enough to say which — but it is a
  ! sample, and a shim serving part of a subtree would not be caught here.
  function contour_edges_verdict(control, cross) result(verdict)
    type(ids_equilibrium), intent(in) :: control, cross
    character(len=6) :: verdict
    real(ids_real), allocatable :: control_edges(:), cross_edges(:)
    logical :: control_has, cross_has

    call gather_contour_edges(control, control_edges, control_has)
    call gather_contour_edges(cross, cross_edges, cross_has)
    verdict = verdict_real_vector_with_stated_presence(control_has, control_edges, cross_has, cross_edges)
  end function contour_edges_verdict

  logical function has_time_slice(equilibrium)
    type(ids_equilibrium), intent(in) :: equilibrium

    has_time_slice = .false.
    if (associated(equilibrium%time_slice)) then
      has_time_slice = size(equilibrium%time_slice) > 0
    end if
  end function has_time_slice

  ! An array the shim served nothing for may come back either unassociated or
  ! associated but empty, and the two must be treated alike: an empty array
  ! holds no value, so reporting it as present would turn a served-nothing
  ! reading into a `SHAPE` verdict against a populated oracle rather than the
  ! `only4` the rule expects. `.and.` is not short-circuiting in Fortran, so
  ! the size test is nested inside the association test throughout.
  !
  ! A boundary worth stating, because it is the array-valued form of the
  ! defect the five documented reds exhibit for scalars: presence here is
  ! decided by allocation, not by content. An array the shim allocated and
  ! filled with sentinels would be reported present and compared, yielding
  ! `DIFF` or `SHAPE` rather than `only4`. The rule would still fail, and its
  ! verdict would still name the disagreement — but it would not name it as a
  ! served-nothing reading.
  ! The reading a gather_* starts from and keeps if its container was never
  ! served: absent, and a zero-length array rather than an unallocated one, so
  ! a caller may pass it to the comparison primitives without checking first.
  !
  ! Every gather_* below opens with this and returns early, which is what
  ! makes the four of them say only what differs -- which container they probe
  ! and how they flatten it.  Assigning to an allocatable reallocates it, so
  ! the served path needs no deallocate.
  subroutine gather_nothing(values, is_present)
    real(ids_real), allocatable, intent(out) :: values(:)
    logical, intent(out) :: is_present

    is_present = .false.
    allocate(values(0))
  end subroutine gather_nothing

  subroutine gather_contour_nodes(equilibrium, values, is_present)
    type(ids_equilibrium), intent(in) :: equilibrium
    real(ids_real), allocatable, intent(out) :: values(:)
    logical, intent(out) :: is_present

    call gather_nothing(values, is_present)
    if (.not. associated(equilibrium%time_slice(1)%contour_tree%node)) return
    if (size(equilibrium%time_slice(1)%contour_tree%node) == 0) return

    is_present = .true.
    values = [equilibrium%time_slice(1)%contour_tree%node(:)%psi, &
              equilibrium%time_slice(1)%contour_tree%node(:)%r,   &
              equilibrium%time_slice(1)%contour_tree%node(:)%z]
  end subroutine gather_contour_nodes

  subroutine gather_contour_edges(equilibrium, values, is_present)
    type(ids_equilibrium), intent(in) :: equilibrium
    real(ids_real), allocatable, intent(out) :: values(:)
    logical, intent(out) :: is_present

    call gather_nothing(values, is_present)
    if (.not. associated(equilibrium%time_slice(1)%contour_tree%edges)) return
    if (size(equilibrium%time_slice(1)%contour_tree%edges) == 0) return

    is_present = .true.
    values = real(reshape(equilibrium%time_slice(1)%contour_tree%edges, &
                          [size(equilibrium%time_slice(1)%contour_tree%edges)]), ids_real)
  end subroutine gather_contour_edges

  subroutine gather_j_parallel(equilibrium, values, is_present)
    type(ids_equilibrium), intent(in) :: equilibrium
    real(ids_real), allocatable, intent(out) :: values(:)
    logical, intent(out) :: is_present

    call gather_nothing(values, is_present)
    if (.not. associated(equilibrium%time_slice(1)%constraints%j_parallel)) return
    if (size(equilibrium%time_slice(1)%constraints%j_parallel) == 0) return

    is_present = .true.
    values = [equilibrium%time_slice(1)%constraints%j_parallel(:)%measured,      &
              equilibrium%time_slice(1)%constraints%j_parallel(:)%reconstructed, &
              equilibrium%time_slice(1)%constraints%j_parallel(:)%position%psi]
  end subroutine gather_j_parallel

  subroutine gather_p1d_psi_norm(equilibrium, values, is_present)
    type(ids_equilibrium), intent(in) :: equilibrium
    real(ids_real), allocatable, intent(out) :: values(:)
    logical, intent(out) :: is_present

    call gather_nothing(values, is_present)
    if (.not. associated(equilibrium%time_slice(1)%profiles_1d%psi_norm)) return
    if (size(equilibrium%time_slice(1)%profiles_1d%psi_norm) == 0) return

    is_present = .true.
    values = equilibrium%time_slice(1)%profiles_1d%psi_norm
  end subroutine gather_p1d_psi_norm



  subroutine demonstrate(condition, what)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: what

    demonstrations = demonstrations + 1
    if (.not. condition) call checker%fail(what)
  end subroutine demonstrate

end program test_shim_right_only_rules
