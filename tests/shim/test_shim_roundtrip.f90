! The same executable drives both halves of the roundtrip evidence.  Its
! fixture is selected by CTest: DD 3.39.0 exercises conversion, while DD 4.1.1
! is the same-version control that proves the write/read pair is not vacuous.
program test_shim_roundtrip
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get, ids_put_slice, ids_real
  implicit none

  real(ids_real), parameter :: expected_ip = -7654321.0_ids_real
  real(ids_real), parameter :: expected_time = 2.0_ids_real
  type(ids_equilibrium) :: written, read_back
  character(len=512) :: fixture, control
  integer :: context, status
  logical :: require_clean_read

  call get_command_argument(1, fixture)
  if (len_trim(fixture) == 0) error stop 'SCENARIO-FAILURE: missing roundtrip fixture'
  call get_command_argument(2, control)
  require_clean_read = trim(control) == 'clean-read'

  ! `global_quantities/ip` is a mapped COCOS field, so the DD 3 case has to
  ! flip it on write and on read.  The generated slice writer needs a matching
  ! time coordinate for the one appended arraystruct element.
  written%ids_properties%homogeneous_time = 1
  allocate(written%time(1))
  written%time = expected_time
  allocate(written%time_slice(1))
  written%time_slice(1)%time = expected_time
  written%time_slice(1)%global_quantities%ip = expected_ip

  call imas_open('imas:hdf5?path='//trim(fixture), OPEN_PULSE, context)
  call ids_put_slice(context, 'equilibrium', written, status)
  if (status /= 0) error stop 'SCENARIO-FAILURE: curated slice write failed'

  call ids_get(context, 'equilibrium', read_back, status)
  call imas_close(context)
  ! The complete DD 3 fixture includes intentionally unservable fields, so its
  ! read may be positive PARTIAL_READ.  The same-version control must be clean:
  ! otherwise it could not prove that the cross-DD roundtrip is meaningful.
  if (status < 0 .or. (require_clean_read .and. status /= 0)) then
    error stop 'SCENARIO-FAILURE: roundtrip read failed'
  end if
  if (.not. associated(read_back%time_slice)) error stop 'SCENARIO-FAILURE: roundtrip has no time slices'
  ! The generated fixtures deliberately contain two slices before this test.
  if (size(read_back%time_slice) /= 3) error stop 'SCENARIO-FAILURE: curated slice was not appended'
  if (.not. associated(read_back%time)) error stop 'SCENARIO-FAILURE: roundtrip has no time values'
  if (size(read_back%time) /= 3) error stop 'SCENARIO-FAILURE: curated time was not appended'
  if (read_back%ids_properties%homogeneous_time /= 1) then
    error stop 'SCENARIO-FAILURE: roundtrip changed homogeneous_time'
  end if
  if (read_back%time(3) /= expected_time) error stop 'SCENARIO-FAILURE: roundtrip changed the time base'
  if (read_back%time_slice(3)%time /= expected_time) then
    error stop 'SCENARIO-FAILURE: roundtrip changed the slice time'
  end if
  if (read_back%time_slice(3)%global_quantities%ip /= expected_ip) then
    error stop 'SCENARIO-FAILURE: roundtrip changed the curated plasma current'
  end if
end program test_shim_roundtrip
