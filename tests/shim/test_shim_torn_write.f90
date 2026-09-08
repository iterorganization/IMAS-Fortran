! Behaviour pin (issue #74), not a contract assertion: this is an accepted
! limitation, not a bug to fix.  A DD 4-only leaf in a DD 3 occurrence is
! refused, yet put_slice carries on.  IMAS-Fortran has no rollback, so the
! slice remains torn: its container grows and earlier leaves remain readable.
!
! The write policy exposes only a refusal count today.  check_torn_write.cmake
! therefore asserts the printed refused path; replace that weak seam with a
! structured write-path accessor mirroring al_get_policy's read-side accessor
! when one is added (the follow-up requested by issue #74).
program test_shim_torn_write
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, &
                          ids_get, ids_put_slice, ids_real
  use al_defs, only: PARTIAL_PUT
  implicit none

  real(ids_real), parameter :: expected_ip = -7654321.0_ids_real
  real(ids_real), parameter :: expected_time = 3.0_ids_real
  real(ids_real), parameter :: refused_rho_tor_boundary = 9.25_ids_real
  type(ids_equilibrium) :: written, read_back
  character(len=512) :: fixture
  integer :: context, status

  call get_command_argument(1, fixture)
  if (len_trim(fixture) == 0) error stop 'SCENARIO-FAILURE: missing torn-write fixture'

  written%ids_properties%homogeneous_time = 1
  allocate(written%time(1))
  written%time = expected_time
  allocate(written%time_slice(1))
  written%time_slice(1)%time = expected_time
  ! `ip` is traversed before the DD 4-only boundary field.  Its read-back
  ! proves that a refusal did not roll the newly opened slice back.
  written%time_slice(1)%global_quantities%ip = expected_ip
  written%time_slice(1)%global_quantities%rho_tor_boundary = refused_rho_tor_boundary

  call imas_open('imas:hdf5?path='//trim(fixture), OPEN_PULSE, context)
  call ids_put_slice(context, 'equilibrium', written, status)
  if (status /= PARTIAL_PUT) then
    write(*, '(a,i0)') 'TORN-WRITE-FAILURE: right_only write status was ', status
    error stop 'SCENARIO-FAILURE: right_only write did not report PARTIAL_PUT'
  end if

  call ids_get(context, 'equilibrium', read_back, status)
  call imas_close(context)
  if (status < 0) error stop 'SCENARIO-FAILURE: torn-write read failed'
  if (.not. associated(read_back%time_slice)) error stop 'SCENARIO-FAILURE: torn-write has no time slices'
  ! The curated fixture has two slices.  The third is retained even though the
  ! right_only leaf was refused: this is the behaviour pin's torn shape.
  if (size(read_back%time_slice) /= 3) error stop 'SCENARIO-FAILURE: refusal did not leave a torn slice'
  if (read_back%time_slice(3)%time /= expected_time) error stop 'SCENARIO-FAILURE: torn slice lost its time'
  if (read_back%time_slice(3)%global_quantities%ip /= expected_ip) then
    error stop 'SCENARIO-FAILURE: leaf before refusal was not readable from the torn slice'
  end if
end program test_shim_torn_write
