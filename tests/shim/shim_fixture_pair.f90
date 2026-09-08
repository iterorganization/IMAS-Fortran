! The two reads every rule-table program starts with.
!
! Each contract program compares one DD 3.39.0 pulse read through the shim by
! a DD 4.1.1 HLI against the DD 4.1.1 fixture read same-version.  That pair,
! its fixture-root argument and the two preconditions on its statuses were
! copied into test_shim_{structural,cocos,right_only}_rules verbatim and into
! test_shim_refusal_rules with one insertion, which is four places for the
! oracle's own setup to drift.
!
! The reads are offered separately rather than as one call because the
! refusal test has to copy the read-side skip log out between them:
! al_get_policy resets its log at the start of every ids_get, so the control
! read would erase the entries that test exists to assert.  A single
! read_both() would have had to grow a callback to accommodate that, which is
! more machinery than the duplication cost.
module shim_fixture_pair
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get
  use al_get_policy, only: PARTIAL_READ
  implicit none
  private

  public :: fixture_root_from_command, read_cross_version, read_same_version
  public :: assert_reads_usable

contains

  function fixture_root_from_command() result(fixture_root)
    character(len=512) :: fixture_root

    call get_command_argument(1, fixture_root)
    if (len_trim(fixture_root) == 0) error stop 'missing fixture root'
  end function fixture_root_from_command

  ! The DD 3.39.0 pulse read by a DD 4.1.1 HLI: the converted read, and the
  ! side under test.
  subroutine read_cross_version(fixture_root, equilibrium, status)
    character(len=*), intent(in) :: fixture_root
    type(ids_equilibrium), intent(inout) :: equilibrium
    integer, intent(out) :: status
    integer :: context

    call imas_open('imas:hdf5?path='//trim(fixture_root)//'/dd-3.39.0', OPEN_PULSE, context)
    call ids_get(context, 'equilibrium', equilibrium, status)
    call imas_close(context)
  end subroutine read_cross_version

  ! The DD 4.1.1 fixture read same-version: no conversion, so it is the
  ! oracle.  imas-python-fixtures/README.md states it is the independently
  ! authored expected result of converting the other, which is what lets the
  ! programs assert without transcribing value literals.
  subroutine read_same_version(fixture_root, equilibrium, status)
    character(len=*), intent(in) :: fixture_root
    type(ids_equilibrium), intent(inout) :: equilibrium
    integer, intent(out) :: status
    integer :: context

    call imas_open('imas:hdf5?path='//trim(fixture_root)//'/dd-4.1.1', OPEN_PULSE, context)
    call ids_get(context, 'equilibrium', equilibrium, status)
    call imas_close(context)
  end subroutine read_same_version

  ! Preconditions, asserted rather than assumed: a failed control read would
  ! make every verdict below absent-versus-absent, which proves nothing.  A
  ! PARTIAL_READ on the cross-version side is expected -- the shim refuses
  ! paths -- but any other failure is not.
  !
  ! test_shim_refusal_rules deliberately does not call this: it reports the
  ! same two conditions through its own marker, because a precondition failure
  ! there has to be legible as a failure of that test rather than a crash.
  subroutine assert_reads_usable(status_cross, status_control)
    integer, intent(in) :: status_cross, status_control

    if (status_control /= 0) error stop 'same-version control read did not succeed cleanly'
    if (status_cross /= 0 .and. status_cross /= PARTIAL_READ) error stop 'cross-version read failed outright'
  end subroutine assert_reads_usable

end module shim_fixture_pair
