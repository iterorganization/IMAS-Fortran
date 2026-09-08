! Assert, per rule rather than per leaf, that every structural rule in
! shim_rule_table produces agreement between:
!
!   - a DD 3.39.0 pulse read via a DD 4.1.1 HLI through the shim (converted),
!   - the DD 4.1.1 fixture read directly, same-version, plain passthrough.
!
! The second pulse is the oracle: imas-python-fixtures/README.md states it is
! the independently-authored expected result of converting the first, so
! comparing against it needs no value literals transcribed into Fortran.
!
! A failing check below names the rule that broke (id, kind and cited
! source), not only the field, per the suite's design (docs/adr/0002 and
! issue #63's acceptance criteria for this ticket).
program test_shim_structural_rules
  use ids_routines, only: ids_equilibrium, ids_real
  use shim_fixture_pair, only: fixture_root_from_command, read_cross_version, &
                               read_same_version, assert_reads_usable
  use shim_comparison, only: verdict_real, verdict_real_vector_as_read, verdict_real_matrix_as_read
  use shim_comparison, only: verdict_len
  use shim_rule_table, only: structural_rules
  use shim_rule_check, only: rule_checker
  implicit none

  type(ids_equilibrium) :: eq_cross, eq_control
  character(len=512) :: fixture_root
  integer :: status_cross, status_control
  type(rule_checker) :: checker

  fixture_root = fixture_root_from_command()
  call read_cross_version(fixture_root, eq_cross, status_cross)
  call read_same_version(fixture_root, eq_control, status_control)
  call assert_reads_usable(status_cross, status_control)

  checker%rules = structural_rules
  checker%marker = 'STRUCTURAL-FAILURE'

  ! -- identical --
  call checker%check('identical-vacuum-r0', &
       verdict_real(eq_cross%vacuum_toroidal_field%r0, eq_control%vacuum_toroidal_field%r0))
  call checker%check('identical-time', &
       verdict_real_vector_as_read(eq_cross%time, eq_control%time))
  call checker%check('identical-beta-pol', &
       verdict_real(eq_cross%time_slice(1)%global_quantities%beta_pol, &
                    eq_control%time_slice(1)%global_quantities%beta_pol))

  ! -- renamed --
  call checker%check('rename-beta-normal', &
       verdict_real(eq_cross%time_slice(1)%global_quantities%beta_tor_norm, &
                    eq_control%time_slice(1)%global_quantities%beta_tor_norm))
  call checker%check('rename-bpol-probe', &
       verdict_real(eq_cross%time_slice(1)%constraints%b_field_pol_probe(1)%measured, &
                    eq_control%time_slice(1)%constraints%b_field_pol_probe(1)%measured))
  call checker%check('rename-mse-polarisation-angle', &
       verdict_real(eq_cross%time_slice(1)%constraints%mse_polarization_angle(1)%measured, &
                    eq_control%time_slice(1)%constraints%mse_polarization_angle(1)%measured))
  call checker%check('rename-magnetisation-r', &
       verdict_real(eq_cross%time_slice(1)%constraints%iron_core_segment(1)%magnetization_r%measured, &
                    eq_control%time_slice(1)%constraints%iron_core_segment(1)%magnetization_r%measured))
  call checker%check('rename-magnetisation-z', &
       verdict_real(eq_cross%time_slice(1)%constraints%iron_core_segment(1)%magnetization_z%measured, &
                    eq_control%time_slice(1)%constraints%iron_core_segment(1)%magnetization_z%measured))

  ! -- moved (each rule combines a r/z pair into one verdict) --
  call checker%check('move-closest-wall-point', &
       combine_pair('move-closest-wall-point', &
                verdict_real(eq_cross%time_slice(1)%boundary%closest_wall_point%r, &
                              eq_control%time_slice(1)%boundary%closest_wall_point%r), &
                verdict_real(eq_cross%time_slice(1)%boundary%closest_wall_point%z, &
                              eq_control%time_slice(1)%boundary%closest_wall_point%z)))
  call checker%check('move-dr-dz-zero-point', &
       combine_pair('move-dr-dz-zero-point', &
                verdict_real(eq_cross%time_slice(1)%boundary%dr_dz_zero_point%r, &
                              eq_control%time_slice(1)%boundary%dr_dz_zero_point%r), &
                verdict_real(eq_cross%time_slice(1)%boundary%dr_dz_zero_point%z, &
                              eq_control%time_slice(1)%boundary%dr_dz_zero_point%z)))
  call checker%check('move-gap', &
       combine_pair('move-gap', &
                verdict_real(eq_cross%time_slice(1)%boundary%gap(1)%r, &
                              eq_control%time_slice(1)%boundary%gap(1)%r), &
                verdict_real(eq_cross%time_slice(1)%boundary%gap(1)%z, &
                              eq_control%time_slice(1)%boundary%gap(1)%z)))

  ! -- merged: the eight folds --
  call checker%check('fold-p2d-br', verdict_real_matrix_as_read(eq_cross%time_slice(1)%profiles_2d(1)%b_field_r, &
                                             eq_control%time_slice(1)%profiles_2d(1)%b_field_r))
  call checker%check('fold-p2d-bz', verdict_real_matrix_as_read(eq_cross%time_slice(1)%profiles_2d(1)%b_field_z, &
                                             eq_control%time_slice(1)%profiles_2d(1)%b_field_z))
  call checker%check('fold-p2d-bphi', verdict_real_matrix_as_read(eq_cross%time_slice(1)%profiles_2d(1)%b_field_phi, &
                                               eq_control%time_slice(1)%profiles_2d(1)%b_field_phi))
  call checker%check('fold-axis-bphi', &
       verdict_real(eq_cross%time_slice(1)%global_quantities%magnetic_axis%b_field_phi, &
                    eq_control%time_slice(1)%global_quantities%magnetic_axis%b_field_phi))
  call checker%check('fold-p1d-baverage', &
       verdict_real_vector_as_read(eq_cross%time_slice(1)%profiles_1d%b_field_average, &
                                   eq_control%time_slice(1)%profiles_1d%b_field_average))
  call checker%check('fold-p1d-bmax', &
       verdict_real_vector_as_read(eq_cross%time_slice(1)%profiles_1d%b_field_max, &
                                   eq_control%time_slice(1)%profiles_1d%b_field_max))
  call checker%check('fold-p1d-bmin', &
       verdict_real_vector_as_read(eq_cross%time_slice(1)%profiles_1d%b_field_min, &
                                   eq_control%time_slice(1)%profiles_1d%b_field_min))
  call checker%check('fold-energy-mhd', &
       verdict_real(eq_cross%time_slice(1)%global_quantities%energy_mhd, &
                    eq_control%time_slice(1)%global_quantities%energy_mhd))

  ! -- split: one DD3 source feeds two DD4 targets; both must agree --
  call checker%check('split-psi-axis', &
       combine_pair('split-psi-axis', &
                verdict_real(eq_cross%time_slice(1)%global_quantities%psi_axis, &
                              eq_control%time_slice(1)%global_quantities%psi_axis), &
                verdict_real(eq_cross%time_slice(1)%global_quantities%psi_magnetic_axis, &
                              eq_control%time_slice(1)%global_quantities%psi_magnetic_axis)))

  call checker%assert_every_rule_checked(checker%expectations)

  if (checker%failures > 0) then
    write(*, '(a,i0,a)') 'STRUCTURAL-FAILURE: ', checker%failures, ' structural rule(s) failed'
    stop 1
  end if

contains

  ! One rule can combine several leaf verdicts (e.g. an r/z pair); the rule
  ! fails if either does, and the first non-agreeing verdict is reported.
  !
  ! What counts as agreeing is the rule's own expectation, looked up exactly as
  ! check() looks it up.  Writing 'same' in here instead would be a second,
  ! silent copy of the kind-to-verdict mapping: correct only for as long as
  ! every kind reaching this function still expects agreement, and wrong with
  ! no error the day expected_verdict_for_kind gives one of them something
  ! else.  test_shim_right_only_rules derives it for the same reason.
  function combine_pair(id, first, second) result(combined)
    character(len=*), intent(in) :: id
    character(len=verdict_len), intent(in) :: first, second
    character(len=verdict_len) :: combined, expected

    expected = checker%expected_for(id)
    if (trim(first) /= trim(expected)) then
      combined = first
    else
      combined = second
    end if
  end function combine_pair




end program test_shim_structural_rules
