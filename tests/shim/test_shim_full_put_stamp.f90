! A full put is the only generated write traversal that reaches
! ids_properties/version_put/data_dictionary: put_slice has an empty body for
! that field.  This test consequently must remain a full-put scenario.
!
! The DD 3.39.0 fixture is deliberately opened through the DD 4.1.1 HLI.  The
! shim must refuse its generated DD 4 stamp write, let the rest of the full
! traversal finish, and preserve the DD 3 stamp that was already stored.
!
! Why the two refusal counters are asserted separately.
!
! A full put refuses the stamp twice over, at two different seams, and the
! contract requires both.  `ids_put` opens by deleting the previous occurrence,
! and section 6 says a delete that would remove the stamp while data remains is
! refused; the traversal then writes the HLI's own stamp, and section 5 says a
! write to the stamp under a mismatch is refused always.  Either one on its own
! makes the operation partial, so `PARTIAL_PUT` no longer identifies which
! happened -- it did while the delete traversal discarded every status it got
! (see playground/FINDINGS.md, "the delete traversal was deaf"), and asserting
! the status alone was enough only because of that defect.  Now that a refused
! delete is recorded, the status would be satisfied by the delete refusal alone
! and this test would stop noticing whether the stamp write was refused at all.
! So each phase is asserted on its own counter, and the status is asserted as
! the derived summary it is.
!
! The write half is red today: the shim refuses the delete but not the write,
! and then stores 4.1.1 over the stored 3.39.0, which the final assertion
! catches.  That is a shim defect, diagnosed in playground/FINDINGS.md and
! owned upstream; per docs/adr/0002 the assertion stays as the contract states
! it rather than being softened to what the shim does today.
program test_shim_full_put_stamp
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get, ids_put
  use al_defs, only: PARTIAL_PUT
  use al_put_policy, only: al_get_refused_write_count, al_get_refused_delete_count
  implicit none

  character(len=*), parameter :: stored_dd_version = '3.39.0'
  character(len=*), parameter :: completion_marker = 'full put reached code/name'
  type(ids_equilibrium) :: written, read_back
  character(len=512) :: fixture
  integer :: context, status
  integer :: refused_writes, refused_deletes

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
  ! Read out before the verification read below, so that neither count can be
  ! disturbed by a later operation.
  refused_writes = al_get_refused_write_count()
  refused_deletes = al_get_refused_delete_count()

  ! The delete seam (contract section 6): the stamp could not be removed while
  ! the DD 3 data around it remains.
  if (refused_deletes < 1) then
    error stop 'SCENARIO-FAILURE: full put did not report the tolerated stamp delete refusal'
  end if

  ! The write seam (contract section 5): the HLI's own DD 4.1.1 stamp may not
  ! be stored over a mismatched occurrence's stamp.
  if (refused_writes < 1) then
    error stop 'SCENARIO-FAILURE: full put did not report the tolerated stamp write refusal'
  end if

  ! And the status the caller actually sees, derived from both phases.
  if (status /= PARTIAL_PUT) error stop 'SCENARIO-FAILURE: full put did not report a partial outcome'

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
