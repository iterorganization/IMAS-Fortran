! A present-but-invalid stamp refuses at occurrence open, and issue #76 asks for
! two things: that the refusal happens, and that no data seam is reached.
!
! Where "occurrence open" is, from Fortran.
!
! docs/SHIM_INTEGRATION_CONTRACT.md section 3 puts this refusal at the
! occurrence open and says to test it by opening rather than by reading.  The
! call it names is `al_begin_global_action`, and section 2.3 says the stamp is
! read there and only there.  `imas_open` is not that call: it opens a *data
! entry*, which holds no DD-version stamp, so there is nothing for the shim to
! read and nothing to refuse -- it forwards, and returns a usable context.  The
! first call that opens an occurrence, and therefore the first call that can
! carry this refusal to an HLI caller, is `ids_get`.
!
! This program asserted on `imas_open` instead, and its own header said
! "refuses at occurrence open" while doing so.  The header was right.  So the
! refusal was asserted at a seam that structurally cannot produce one, and the
! test was red against a shim that behaves exactly as the contract requires --
! then tests/shim/README.md recorded the shim as the cause.  Both are now
! asserted where they happen: the data-entry open forwards, and the occurrence
! open refuses.
!
! Reading the refused context is not "using an interface the suite was told it
! does not have", which was the argument for stopping at the open.  That
! argument applies to a refused *occurrence* context, and no occurrence context
! is ever returned here: the shim closes the one IMAS-Core opened before
! returning (section 3), and `ids_get` returns at its `al_begin_global_action`
! failure arm without touching a field.  What `ids_get` gives back is the
! refusal itself, plus an untouched IDS -- which is the observable issue #76's
! second ask needs, and the reason the emptiness check below is kept.
program test_shim_stamp_malformed
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get
  use al_defs, only: is_external_refusal
  use al_get_policy, only: al_last_status_code, al_last_status_message, al_get_skipped_count
  implicit none

  character(len=*), parameter :: malformed_reason = 'malformed DD-version stamp'
  type(ids_equilibrium) :: equilibrium
  character(len=512) :: fixture
  integer :: context, open_status, get_status

  call get_command_argument(1, fixture)
  if (len_trim(fixture) == 0) error stop 'SCENARIO-FAILURE: missing stamp-malformed fixture'

  ! The data-entry open forwards.  Asserted, not skipped over: it is what
  ! locates the refusal at the seam below rather than at this one, and a shim
  ! that started refusing here would be refusing before it had read any stamp.
  call imas_open('imas:hdf5?path='//trim(fixture), OPEN_PULSE, context, open_status)
  if (open_status /= 0) error stop 'SCENARIO-FAILURE: malformed stamp refused the data-entry open'

  ! The occurrence open, reached through the only HLI call that performs one.
  call ids_get(context, 'equilibrium', equilibrium, get_status)
  call imas_close(context)

  if (.not. is_external_refusal(get_status)) then
    error stop 'SCENARIO-FAILURE: malformed stamp did not refuse at occurrence open'
  end if

  ! The reason, which section 8.5 freezes as a named surface.  It comes from
  ! al_get_policy's record of the last non-zero status rather than from an
  ! output argument because `ids_get` has none: the refusal arrives at
  ! `al_begin_global_action`, whose message the generated routine prints and
  ! does not return.  The code is checked against the record before the message
  ! is trusted, exactly as al_note_skipped_path does -- otherwise a stale
  ! message from an earlier call would satisfy this.
  if (al_last_status_code /= get_status) then
    error stop 'SCENARIO-FAILURE: no recorded status message belongs to the refusal'
  end if
  if (index(al_last_status_message, malformed_reason) == 0) then
    error stop 'SCENARIO-FAILURE: malformed stamp refusal reason changed'
  end if

  ! Issue #76's second ask: no data seam was reached.  The IDS is the
  ! observable -- nothing was ever served into it, so it is still exactly as
  ! declared.
  if (associated(equilibrium%time)) error stop 'SCENARIO-FAILURE: malformed stamp forwarded to a data seam'

  ! And the refusal was not tolerated into a partial read.  A refusal at the
  ! occurrence open is barred from the read-side policy's tolerance (see
  ! al_get_policy's header: tolerating one would sail past an IDS that was
  ! never opened), so it must appear as a refusal status and not as a skipped
  ! path with a completed read.  `ids_get` returns before reaching
  ! al_reset_skip_log, so an entry here would mean the traversal ran.
  if (al_get_skipped_count() /= 0) then
    error stop 'SCENARIO-FAILURE: malformed stamp was tolerated as a skipped path'
  end if
end program test_shim_stamp_malformed
