! An occurrence stamp equal to the HLI version registers nothing (occurrence-
! open table, docs/SHIM_INTEGRATION_CONTRACT.md section 3, second row), so
! every later read forwards exactly as issued.
!
! Its sibling, test_shim_stamp_mismatch_no_artifact.f90, differs from this
! test only in the stamp: that program's stamp genuinely disagrees with the
! HLI version, this one's does not, and its header explains why the two are
! kept as separate tests rather than variants of one -- read it before
! changing either.
program test_shim_stamp_equal
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get
  use al_get_policy, only: al_get_skipped_count
  implicit none

  type(ids_equilibrium) :: equilibrium
  character(len=512) :: fixture_root
  integer :: context, open_status, get_status

  call get_command_argument(1, fixture_root)
  if (len_trim(fixture_root) == 0) error stop 'missing fixture root'

  call imas_open('imas:hdf5?path='//trim(fixture_root)//'/dd-4.1.1', OPEN_PULSE, context, open_status)
  if (open_status /= 0) error stop 'stamp-equal open did not forward'

  call ids_get(context, 'equilibrium', equilibrium, get_status)
  call imas_close(context)

  if (get_status /= 0) error stop 'stamp-equal read did not forward cleanly'
  if (al_get_skipped_count() /= 0) error stop 'stamp-equal read logged a skipped path'
  if (.not. associated(equilibrium%time)) error stop 'stamp-equal read reached no data'
  if (size(equilibrium%time) /= 2) error stop 'stamp-equal read returned an unexpected time base'
end program test_shim_stamp_equal
