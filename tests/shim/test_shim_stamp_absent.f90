! An occurrence without ids_properties/version_put/data_dictionary is presumed
! to match the HLI.  Its open and read must therefore be plain forwarding,
! rather than the malformed-stamp refusal asserted by the companion program.
program test_shim_stamp_absent
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get
  use al_get_policy, only: al_get_skipped_count
  implicit none

  type(ids_equilibrium) :: equilibrium
  character(len=512) :: fixture
  integer :: context, open_status, get_status

  call get_command_argument(1, fixture)
  if (len_trim(fixture) == 0) error stop 'SCENARIO-FAILURE: missing stamp-absent fixture'

  call imas_open('imas:hdf5?path='//trim(fixture), OPEN_PULSE, context, open_status)
  if (open_status /= 0) error stop 'SCENARIO-FAILURE: stamp-absent occurrence open did not forward'

  call ids_get(context, 'equilibrium', equilibrium, get_status)
  call imas_close(context)

  if (get_status /= 0) error stop 'SCENARIO-FAILURE: stamp-absent read did not forward cleanly'
  if (al_get_skipped_count() /= 0) error stop 'SCENARIO-FAILURE: stamp-absent read logged a skipped path'
  if (.not. associated(equilibrium%time)) error stop 'SCENARIO-FAILURE: stamp-absent read reached no data'
  if (size(equilibrium%time) /= 2) error stop 'SCENARIO-FAILURE: stamp-absent read returned an unexpected time base'
end program test_shim_stamp_absent
