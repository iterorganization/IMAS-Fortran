! Exercise the Tier-1 observable seam for a loss recorded while ids_get is
! traversing arraystruct children.  The CMake wrapper owns the log-file checks:
! the HLI intentionally does not bind the shim's imas_mvdd_context_loss_* ABI.
program test_shim_nested_loss
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get
  use al_get_policy, only: PARTIAL_READ, al_get_skipped_count
  implicit none

  type(ids_equilibrium) :: equilibrium
  character(len=512) :: fixture_root
  integer :: context, status

  call get_command_argument(1, fixture_root)
  if (len_trim(fixture_root) == 0) error stop 'SCENARIO-FAILURE: missing fixture root'

  call imas_open('imas:hdf5?path='//trim(fixture_root)//'/dd-3.39.0', OPEN_PULSE, context)
  call ids_get(context, 'equilibrium', equilibrium, status)
  call imas_close(context)

  if (status /= PARTIAL_READ) error stop 'SCENARIO-FAILURE: cross-DD nested read did not report PARTIAL_READ'
  if (al_get_skipped_count() == 0) error stop 'SCENARIO-FAILURE: cross-DD nested read skipped no refused paths'
end program test_shim_nested_loss
