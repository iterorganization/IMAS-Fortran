! The guard that stops a program which asserted nothing from passing.
!
! CLAUDE.md's test conventions note that a test here passes by not printing,
! so a run that checked nothing at all looks exactly like a run that checked
! everything.  Every program in this suite therefore counts what it ran and
! compares that against a number stated up front.
!
! It was stated four different ways: rule_checker's own
! assert_every_rule_checked, `expectations == 32` in the oracle unit test,
! `demonstrations /= 2` in the right_only test, and a named
! expected_expectation_count parameter in the refusal test, each with its own
! message.  One shape, one message, one place to change what a shortfall reads
! like.
!
! Deliberately a leaf module with no dependency beyond the marker string it is
! handed, so the oracle unit test -- which ADR 0002 keeps self-contained,
! linking no rule table -- can use it too.
module shim_run_guard
  implicit none
  private

  public :: assert_ran_count

contains

  ! `what` names the thing counted, in the plural, so the message reads
  ! "only 3 of 5 rule table entries were checked".
  subroutine assert_ran_count(marker, what, ran, expected, failures)
    character(len=*), intent(in) :: marker, what
    integer, intent(in) :: ran, expected
    integer, intent(inout) :: failures

    if (ran == expected) return
    failures = failures + 1
    write(*, '(a,a,i0,a,i0,a,a)') trim(marker), ': only ', ran, ' of ', expected, &
      ' ', trim(what)
  end subroutine assert_ran_count

end module shim_run_guard
