! A full put is the only generated write traversal that reaches
! ids_properties/version_put/data_dictionary: put_slice has an empty body for
! that field.  This test consequently must remain a full-put scenario.
!
! The DD 3.39.0 fixture is deliberately opened through the DD 4.1.1 HLI.  The
! shim must refuse its generated DD 4 stamp write, let the rest of the full
! traversal finish, and preserve the DD 3 stamp that was already stored.
program test_shim_full_put_stamp
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get, ids_put
  use al_defs, only: PARTIAL_PUT
  implicit none

  character(len=*), parameter :: stored_dd_version = '3.39.0'
  character(len=*), parameter :: completion_marker = 'full put reached code/name'
  type(ids_equilibrium) :: written, read_back
  character(len=512) :: fixture
  integer :: context, status

  call get_command_argument(1, fixture)
  if (len_trim(fixture) == 0) error stop 'SCENARIO-FAILURE: missing full-put fixture'

  call imas_open('imas:hdf5?path='//trim(fixture), OPEN_PULSE, context)
  call ids_get(context, 'equilibrium', written, status)
  if (status < 0) error stop 'SCENARIO-FAILURE: full-put setup read failed'

  ! code follows ids_properties in the generated full-put traversal.  Reading
  ! it back therefore proves this operation did not stop at the refused stamp.
  allocate(written%code%name(1))
  written%code%name(1) = completion_marker

  call ids_put(context, 'equilibrium', written, status)
  if (status /= PARTIAL_PUT) error stop 'SCENARIO-FAILURE: full put did not report the tolerated stamp refusal'

  call ids_get(context, 'equilibrium', read_back, status)
  call imas_close(context)
  if (status < 0) error stop 'SCENARIO-FAILURE: full-put verification read failed'

  if (.not. associated(read_back%code%name)) error stop 'SCENARIO-FAILURE: full put did not reach code/name'
  if (trim(read_back%code%name(1)) /= completion_marker) then
    error stop 'SCENARIO-FAILURE: full put stopped before code/name'
  end if

  if (.not. associated(read_back%ids_properties%version_put%data_dictionary)) then
    error stop 'SCENARIO-FAILURE: full put removed the stored DD version stamp'
  end if
  if (trim(read_back%ids_properties%version_put%data_dictionary(1)) /= stored_dd_version) then
    error stop 'SCENARIO-FAILURE: full put rewrote the stored DD version stamp'
  end if
end program test_shim_full_put_stamp
