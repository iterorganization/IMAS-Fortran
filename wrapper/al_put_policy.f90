! Write-side policy for paths an interposing layer refuses to store.
!
! Background. When al-fortran is linked against a DD-conversion layer (the
! IMAS-Multiversion-DD-Loader, see docs/adr/0001-multiversion-shim-linkage.md)
! rather than against IMAS-Core directly, a write into a pulse held under a
! different Data Dictionary version can reach a path that layer cannot serve.
! Its write policy is best-effort: store the value where it can be placed
! faithfully, refuse where it cannot, rather than store an approximation. It
! refuses a field with no stored slot, a non-primary source of a folded rule, a
! value transformation it cannot invert, and a retyped field.
!
!
! Why this is not al_get_policy under another name
!
! Every clause of the read side's argument turns over when the refused field is
! a destination rather than a source:
!
!   - "Leave the field unset and carry on" is a complete answer for a read: the
!     caller's field keeps its default, and no value was ever available. For a
!     write it means the caller's value is discarded. That data still exists in
!     their program, and nothing in the pulse records that it was dropped.
!
!   - "No retry can produce it" holds either way. But a read's caller loses
!     information that was never there, and a write's caller loses information
!     that was.
!
!   - "Aborting costs every other field for nothing" is the read side's
!     strongest clause and the weakest one here. Aborting a write does not
!     merely cost the remaining fields: the traversal has no rollback, so every
!     field it already wrote stays on disk. The caller is left with an error
!     *and* a partially written occurrence -- on a put_slice, a torn time slice.
!
! Both branches are lossy, in different ways, so which is preferable is a real
! decision rather than an inherited one.
!
!
! The decision
!
! Tolerate, per site, for one reason the read side did not have: aborting does
! not prevent the partial write. It only makes the occurrence partial at an
! arbitrary point -- wherever the traversal happened to be -- and then calls
! that a failure. Tolerating instead produces an occurrence that is complete
! except for the paths the stored dictionary has no room for, which is the most
! faithful write available and is what the interposing layer's own best-effort
! policy is already aiming at.
!
! What tolerating must not do is stay quiet about it. A tolerated refusal with
! no record turns a loud partial write into a silent one: the caller is told
! the write succeeded while their value was dropped. That is strictly worse than
! the behaviour it replaces, and it is the only combination on this side that is.
! So the tolerance and the record are one change, never two:
!
!   - every refused write prints, here, at the moment it is refused; and
!   - ids_put and ids_put_slice return PARTIAL_PUT instead of 0, which is
!     positive and so trips the `status.ne.0` test callers already write.
!
!
! What is deliberately not here yet
!
! A queryable, structured record of *which* paths were refused, of the kind
! al_get_policy keeps for reads. That belongs in one log carrying reads and
! writes together and attached to the IDS rather than to the process, and it has
! open questions this module must not pre-empt:
!
!   - its reset point. al_get_policy resets per operation, so its log describes
!     one get. A log attached to an IDS is per occurrence, and a get followed by
!     a put_slice would then share it. Choosing that changes read behaviour, so
!     it is not a choice to make from the write side alone.
!
!   - its path spelling. A read's entry records the path as the generated code
!     knows it, which inside a nested routine is relative (`chi_squared_r`, not
!     `time_slice/constraints/x_point/chi_squared_r`); the full DD path reaches
!     the entry only through the status message. One unified log wants one
!     convention.
!
! The counter below holds a strict subset of what that log will hold, so
! building it is a change of storage rather than a change of contract.
!
!
! Which sites may tolerate
!
! Site eligibility is structural, not a run-time test, and the read side's list
! of sites does not carry over -- it is stated in terms of read sites. Derived
! for the write path:
!
!   isErrorCritical                                        -- yes, per field
!   the failure arm of al_begin_arraystruct_action          -- yes, per subtree
!   the put_slice read of ids_properties/homogeneous_time   -- NO
!
! The first is every leaf write and every nested struct or AoS-element call, all
! of which funnel through checkErrorCtx into that one function.
!
! The second is a *sizing* call rather than a lookup, which is why it needed its
! own answer. A refusal there means the array of structures is never opened, so
! tolerating it drops every field beneath the path, not one. That is still the
! right answer -- the whole subtree has no stored slot, and no per-field attempt
! under it could have succeeded -- but the skip is subtree-shaped, and it says
! so when it prints.
!
! The third is the exception, and the reason the list had to be derived rather
! than assumed. That call is a *read*, in the preamble of ids_put_slice, and its
! result chooses between writing a slice and delegating to a full ids_put. A
! tolerated refusal there would leave storedtimemode at IDS_TIME_MODE_UNKNOWN,
! and ids_put opens by deleting the occurrence -- so tolerating one refused
! metadata read would turn a put_slice into a delete-and-rewrite of the whole
! IDS. That site tests status directly instead. The same reasoning bars
! al_begin_global_action and the data-entry seams, where the layer returns this
! same code for a malformed version stamp or a version-latch conflict.
!
!
! The refused delete, and why it is counted separately
!
! `ids_put` opens by deleting the previous occurrence, and an interposing layer
! can refuse a delete as well as a write -- the DD-version stamp is the case
! that exists today: removing it while stored data remains would leave a pulse
! holding data no reader can date, so the layer refuses it (see
! docs/SHIM_INTEGRATION_CONTRACT.md section 6).
!
! That refusal was invisible. The generated delete traversal called
! `al_delete_data` and discarded its status at all 7757 sites, `ids_delete` had
! no `retstatus` to report through, and the write counter was reset *after* the
! delete anyway -- so a put whose stale data was never removed returned 0, and
! a caller had no way to learn otherwise. Tolerating without recording is the
! one outcome this module exists to prevent, and it was happening on a whole
! phase of the operation.
!
! It is a second counter rather than more entries in the first, for two
! reasons. The consequence differs in kind: a refused write drops a value the
! caller supplied, while a refused delete leaves behind a value they did not,
! so an occurrence can end up complete, correct in everything the caller sent,
! and still not be what they asked for. And `ids_delete` is a public entry
! point in its own right -- one counter would make a standalone delete's report
! depend on whatever put ran before it. Each phase resets and reports its own
! count; `ids_put` derives its partial outcome from both, which is what
! CONTEXT.md's "partial outcome" requires of the operation the caller asked
! for.
!
! `ids_put_slice` deliberately does not consult the delete counter. It has no
! delete phase -- it never reaches the stamp field at all, whose generated
! put_slice body is empty -- and when it delegates to a full `ids_put` it
! returns that routine's status, derived there.
!
! Both counters are process-global and not thread-safe. That matches the rest
! of the generated code, which is already non-reentrant in the same way (the
! IDS-level routines declare `integer(ids_int) :: status = 0`, whose initialiser
! implies SAVE). The contract is one PUT at a time per process.
module al_put_policy

  use al_defs
  implicit none

  ! Number of paths the current write operation refused. No paths are retained:
  ! see "What is deliberately not here yet" above. This counter is what
  ! PARTIAL_PUT is derived from, so it is the whole of the machine-readable
  ! record for now, and the printed lines are the whole of the human-readable
  ! one.
  integer, save :: al_refused_write_total = 0

  ! Number of paths the current delete phase refused to remove. Kept apart from
  ! the count above for the reasons given in the header.
  integer, save :: al_refused_delete_total = 0

contains

  ! Record one leaf write the interposing layer refused.
  !
  ! The print is unconditional and lives here rather than at the call site: with
  ! no structured log yet, this line is the only place the dropped path is
  ! named, so nothing may make it optional. When the unified log arrives this
  ! routine is where it hooks in, and every tolerating site already calls it.
  subroutine al_note_refused_write(path, code)
    character(len=*), intent(in) :: path
    integer, intent(in) :: code

    al_refused_write_total = al_refused_write_total + 1
    write(*,*) "REFUSED WRITE: '", trim(path), &
               "' cannot be stored, value dropped (status ", code, ")"
  end subroutine al_note_refused_write

  ! Record an array of structures the layer refused to open for writing.
  !
  ! Kept apart from the leaf case because the consequence is different in kind:
  ! nothing below the path is written, and the caller reading this line must not
  ! take it for a single dropped value. Counted the same way, because one
  ! refusal is one refusal as far as PARTIAL_PUT is concerned.
  subroutine al_note_refused_write_subtree(path, code)
    character(len=*), intent(in) :: path
    integer, intent(in) :: code

    al_refused_write_total = al_refused_write_total + 1
    write(*,*) "REFUSED WRITE (subtree): '", trim(path), &
               "' cannot be stored, every field below it is unwritten (status ", &
               code, ")"
  end subroutine al_note_refused_write_subtree

  ! How many paths the current write operation refused. Zero means the put
  ! stored everything the caller supplied. This is the signal PARTIAL_PUT is
  ! derived from.
  pure integer function al_get_refused_write_count()
    al_get_refused_write_count = al_refused_write_total
  end function al_get_refused_write_count

  ! Called at the start of each ids_put / ids_put_slice, so the count describes
  ! one operation rather than the life of the process.
  subroutine al_reset_refused_writes()
    al_refused_write_total = 0
  end subroutine al_reset_refused_writes

  ! Record one path the interposing layer refused to delete.
  !
  ! The message says what a refused delete actually costs, which is not what a
  ! refused write costs: the caller's value is not dropped, the previously
  ! stored one survives, and the occurrence therefore still holds data this
  ! operation meant to replace. A reader who saw the write-side wording here
  ! would look for a value of their own that went missing and find none.
  !
  ! Prints for the same reason the write side does: with no structured log yet
  ! this line is the only place the path is named, so nothing may make it
  ! optional.
  subroutine al_note_refused_delete(path, code)
    character(len=*), intent(in) :: path
    integer, intent(in) :: code

    al_refused_delete_total = al_refused_delete_total + 1
    write(*,*) "REFUSED DELETE: '", trim(path), &
               "' cannot be removed, the stored value remains (status ", code, ")"
  end subroutine al_note_refused_delete

  ! How many paths the current delete phase refused to remove. Zero means the
  ! occurrence was cleared of everything the traversal asked to remove.
  pure integer function al_get_refused_delete_count()
    al_get_refused_delete_count = al_refused_delete_total
  end function al_get_refused_delete_count

  ! Called at the start of each ids_delete, so the count describes one delete
  ! traversal -- including the one ids_put performs before writing, which is
  ! why ids_put does not reset this itself: doing so where it resets the write
  ! counter would erase the delete phase's count, that reset being after the
  ! delete.
  subroutine al_reset_refused_deletes()
    al_refused_delete_total = 0
  end subroutine al_reset_refused_deletes

end module al_put_policy
