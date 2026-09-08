! The rule-checking loop the contract programs share.
!
! test_shim_{structural,cocos,right_only,refusal}_rules each carried their own
! find_rule, and the first three their own check, identical but for the table
! searched, the marker printed and -- in the COCOS case -- one extra line of
! explanation.  Four copies of a lookup and three of a judgement is three
! chances for one of them to stop matching the others, which had already
! happened once: the structural test folded leaf verdicts against a literal
! 'same' while its own check derived the expectation from the rule's kind.
!
! rule_checker holds the table, the marker and the two counters, so a program
! states which rules it is asserting and against which channel, and the
! judging, the reporting and the "did every entry get checked" guard are
! written once.
!
! The counters stay public and are read directly by the programs: they mix
! rule checks with other assertions (a skip-log lookup, a demonstration) that
! this type has no business knowing about.
module shim_rule_check
  use shim_rule_table, only: rule_entry, expected_verdict_for_kind, kind_name
  use shim_comparison, only: verdict_len
  use shim_run_guard, only: assert_ran_count
  implicit none
  private

  public :: rule_checker, verdicts_agree

  type :: rule_checker
    type(rule_entry), allocatable :: rules(:)
    character(len=32) :: marker = ''
    integer :: failures = 0
    integer :: expectations = 0
  contains
    procedure :: find => checker_find
    procedure :: expected_for => checker_expected_for
    procedure :: check => checker_check
    procedure :: fail => checker_fail
    procedure :: assert_every_rule_checked => checker_assert_every_rule_checked
  end type rule_checker

contains

  ! The single place a verdict is judged against an expectation.
  !
  ! Public because test_shim_right_only_rules needs the very predicate its rule
  ! checks are decided by: its demonstration that a refusal cannot masquerade
  ! as a match is only worth something if it exercises the suite's real
  ! comparison rather than a restatement of it.
  logical function verdicts_agree(actual, expected)
    character(len=*), intent(in) :: actual, expected

    verdicts_agree = trim(actual) == trim(expected)
  end function verdicts_agree

  ! An unknown id is a mistake in the calling program, not a contract finding:
  ! it means an assertion names a rule the table does not carry, so there is
  ! nothing to judge it against.  It stops the run rather than counting as a
  ! failure, exactly as the per-program copies did.
  function checker_find(self, id) result(idx)
    class(rule_checker), intent(in) :: self
    character(len=*), intent(in) :: id
    integer :: idx
    integer :: i

    idx = 0
    do i = 1, size(self%rules)
      if (trim(self%rules(i)%id) == trim(id)) then
        idx = i
        return
      end if
    end do
    error stop trim(self%marker)//': rule id not found in shim_rule_table: '//trim(id)
  end function checker_find

  function checker_expected_for(self, id) result(expected)
    class(rule_checker), intent(in) :: self
    character(len=*), intent(in) :: id
    character(len=verdict_len) :: expected

    expected = expected_verdict_for_kind(self%rules(self%find(id))%kind)
  end function checker_expected_for

  ! `note` is the one thing the three copies did not share: COCOS adds a line
  ! naming the sign flip that stopped being applied.  Passing it in keeps that
  ! difference where it belongs -- at the call site that knows about it --
  ! rather than making this routine ask which table it is holding.
  subroutine checker_check(self, id, verdict, note)
    class(rule_checker), intent(inout) :: self
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: verdict
    character(len=*), intent(in), optional :: note
    integer :: idx
    character(len=verdict_len) :: expected

    idx = self%find(id)
    expected = expected_verdict_for_kind(self%rules(idx)%kind)
    self%expectations = self%expectations + 1
    if (verdicts_agree(verdict, expected)) return

    self%failures = self%failures + 1
    write(*, '(a,a,a,a,a,a,a,a,a,a,a)') trim(self%marker), ': rule ', trim(id), ' (', &
      trim(kind_name(self%rules(idx)%kind)), ') path ', trim(self%rules(idx)%hli_path), &
      ' verdict=', trim(verdict), ' expected=', trim(expected)
    write(*, '(a,a)') '  source: ', trim(self%rules(idx)%source)
    if (present(note)) write(*, '(a,a)') '  ', trim(note)
  end subroutine checker_check

  ! A failure this type did not judge itself, reported through the same marker
  ! so a program prints one prefix rather than two.
  subroutine checker_fail(self, what)
    class(rule_checker), intent(inout) :: self
    character(len=*), intent(in) :: what

    self%failures = self%failures + 1
    write(*, '(a,a,a)') trim(self%marker), ': ', trim(what)
  end subroutine checker_fail

  ! This type's use of the suite's shared run guard (shim_run_guard): the
  ! expected count is the table's own size, so a program cannot state it
  ! wrongly.
  !
  ! `checked` is passed in rather than read from self%expectations because the
  ! programs count other assertions in the same counter.
  subroutine checker_assert_every_rule_checked(self, checked)
    class(rule_checker), intent(inout) :: self
    integer, intent(in) :: checked

    call assert_ran_count(self%marker, 'rule table entries were checked', &
                          checked, size(self%rules), self%failures)
  end subroutine checker_assert_every_rule_checked

end module shim_rule_check
