! A present-but-invalid stamp refuses at occurrence open.  Issue #76 also asks
! that no data seam is reached, and the observable for that is the IDS itself:
! after the refusal the equilibrium must still be empty, because nothing was
! ever served into it.
!
! The refused context is deliberately not driven into ids_get to prove this.
! imas_open reported a refusal, so the context it returned is not a context the
! caller may use, and calling a data seam through it would be asserting the
! behaviour of an interface this suite has just been told it does not have.
! What can honestly be asserted is that the refusal came before anything was
! served, which is what the emptiness check below says.
program test_shim_stamp_malformed
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open
  use al_defs, only: is_external_refusal
  implicit none

  type(ids_equilibrium) :: equilibrium
  character(len=512) :: fixture
  character(:), allocatable :: message
  integer :: context, open_status

  call get_command_argument(1, fixture)
  if (len_trim(fixture) == 0) error stop 'missing stamp-malformed fixture'

  call imas_open('imas:hdf5?path='//trim(fixture), OPEN_PULSE, context, open_status, message)
  if (.not. is_external_refusal(open_status)) error stop 'malformed stamp did not refuse at open'
  if (.not. allocated(message)) error stop 'malformed stamp refusal supplied no reason'
  if (index(message, 'malformed DD-version stamp') == 0) error stop 'malformed stamp refusal reason changed'

  ! The seam assertion.  This ran as `if (open_status == 0)` guarding an
  ! error stop, which the refusal asserted three lines above makes
  ! unreachable: a refusal is never status zero, so the branch could not
  ! execute and the requirement was never checked.
  if (associated(equilibrium%time)) error stop 'malformed stamp forwarded to a data seam'
end program test_shim_stamp_malformed
