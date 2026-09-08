! Comparison primitives for the registered shim contract suite.
!
! This is intentionally separate from playground/play_eq_two_dd.f90.  The
! playground is a diagnostic built as a standalone project, whereas these
! primitives are the suite's oracle and must always be built with its tests.
module shim_comparison
  use ids_routines, only: ids_real, ids_int, ids_int_invalid
  implicit none
  private

  real(ids_real), parameter :: tolerance = 1.0e-9_ids_real

  ! The closed set of verdicts, named rather than spelled at each site.
  !
  ! CONTEXT.md names "verdict" as a domain concept, and it is one: seven
  ! values, no others.  Carried as a fixed-width string because the failure
  ! messages and the rule table print it, but a producer that spells one
  ! wrong -- 'onlY4', 'Same' -- would not be caught by the comparison, which
  ! is trim-equality, nor by color_for_verdict, which used to fall through to
  ! a default colour.  Every site that produces a verdict now names one of
  ! these, so a typo is a compile error instead.
  integer, parameter, public :: verdict_len = 6
  character(len=verdict_len), parameter, public :: VERDICT_ABSENT = '--'
  character(len=verdict_len), parameter, public :: VERDICT_ONLY3  = 'only3'
  character(len=verdict_len), parameter, public :: VERDICT_ONLY4  = 'only4'
  character(len=verdict_len), parameter, public :: VERDICT_SAME   = 'same'
  character(len=verdict_len), parameter, public :: VERDICT_NOFLIP = 'NOFLIP'
  character(len=verdict_len), parameter, public :: VERDICT_DIFF   = 'DIFF'
  character(len=verdict_len), parameter, public :: VERDICT_SHAPE  = 'SHAPE'

  public :: verdict_real, verdict_integer, verdict_real_vector_with_stated_presence, color_for_verdict
  public :: verdict_real_vector_as_read, verdict_real_matrix_as_read
  public :: presence_verdict

contains

  logical function is_absent_real(value)
    real(ids_real), intent(in) :: value

    is_absent_real = value <= -1.0e40_ids_real .or. value /= value
  end function is_absent_real

  logical function near(left, right)
    real(ids_real), intent(in) :: left, right

    near = abs(left - right) <= tolerance * max(1.0_ids_real, abs(left), abs(right))
  end function near

  ! The absent/only3/only4 cascade every verdict function opens with, written
  ! once.  Returns blank when both sides are present, which is the caller's
  ! signal to go on and judge the values.
  !
  ! `only4` is a statement about argument order as much as about the data: the
  ! callers pass the DD 4 oracle first and the shim-served side second, so
  ! `only4` reads "the oracle has a value and the shim served nothing".
  function presence_verdict(has_left, has_right) result(verdict)
    logical, intent(in) :: has_left, has_right
    character(len=verdict_len) :: verdict

    if (.not. has_left .and. .not. has_right) then
      verdict = VERDICT_ABSENT
    else if (.not. has_right) then
      verdict = VERDICT_ONLY4
    else if (.not. has_left) then
      verdict = VERDICT_ONLY3
    else
      verdict = ''
    end if
  end function presence_verdict

  logical function all_near(left, right)
    real(ids_real), intent(in) :: left(:), right(:)
    integer :: index

    all_near = .false.
    if (size(left) /= size(right)) return
    do index = 1, size(left)
      if (.not. near(left(index), right(index))) return
    end do
    all_near = .true.
  end function all_near

  function verdict_real(left, right) result(verdict)
    real(ids_real), intent(in) :: left, right
    character(len=verdict_len) :: verdict

    verdict = presence_verdict(.not. is_absent_real(left), .not. is_absent_real(right))
    if (verdict /= '') return

    if (near(left, right)) then
      verdict = VERDICT_SAME
    else if (near(left, -right)) then
      ! A COCOS conversion was expected to yield equal HLI values.  Opposite
      ! signs therefore mean its required flip did not happen.
      verdict = VERDICT_NOFLIP
    else
      verdict = VERDICT_DIFF
    end if
  end function verdict_real

  function verdict_integer(left, right) result(verdict)
    integer(ids_int), intent(in) :: left, right
    character(len=verdict_len) :: verdict

    verdict = presence_verdict(left /= ids_int_invalid, right /= ids_int_invalid)
    if (verdict /= '') return

    if (left == right) then
      verdict = VERDICT_SAME
    else
      verdict = VERDICT_DIFF
    end if
  end function verdict_integer

  ! Presence of a vector quantity derived from the reading itself: a side that
  ! was never served comes back zero-length.
  !
  ! Callers used to pass `.true., .true.` for both sides where a value was
  ! simply expected to be there.  That is not an assertion, it is an assumption,
  ! and it disables the absence arm below: two zero-length arrays have equal
  ! size, all_near's loop then runs zero times and returns .true., and the
  ! verdict is `same`.  A rule whose quantity neither side served would pass as
  ! agreement.  Deriving presence here means no call site can claim a presence
  ! it has not checked.
  function verdict_real_vector_as_read(left, right) result(verdict)
    real(ids_real), intent(in) :: left(:), right(:)
    character(len=verdict_len) :: verdict

    verdict = verdict_real_vector_with_stated_presence(size(left) > 0, left, size(right) > 0, right)
  end function verdict_real_vector_as_read

  ! A 2-D quantity judged as its flattened elements, presence read off the
  ! data as above.  The structural and COCOS tests each carried a private copy
  ! of this.  The element count still decides, so a fold that changed the grid
  ! reports SHAPE rather than quietly comparing a different number of points
  ! -- but only the count survives the flatten, so a reshape that preserved it
  ! would not be distinguished.  test_shim_comparison pins both.
  function verdict_real_matrix_as_read(left, right) result(verdict)
    real(ids_real), intent(in) :: left(:,:), right(:,:)
    character(len=verdict_len) :: verdict

    verdict = verdict_real_vector_as_read(reshape(left, [size(left)]), reshape(right, [size(right)]))
  end function verdict_real_matrix_as_read

  ! Presence stated by the caller rather than read off the data, which is a
  ! trap wherever the caller does not genuinely know: passing `.true.` for a
  ! side the shim served nothing for makes two empty readings agree, for the
  ! reason set out above verdict_real_vector_as_read.  Prefer that function.
  !
  ! Public because test_shim_comparison demonstrates the trap, and because a
  ! caller that has checked presence some other way -- from the skip log, say
  ! -- is entitled to say so.  The name is deliberately long enough that a
  ! call site claiming a presence it has not established reads wrong.
  function verdict_real_vector_with_stated_presence(has_left, left, has_right, right) result(verdict)
    logical, intent(in) :: has_left, has_right
    real(ids_real), intent(in) :: left(:), right(:)
    character(len=verdict_len) :: verdict

    verdict = presence_verdict(has_left, has_right)
    if (verdict /= '') return

    if (size(left) /= size(right)) then
      verdict = VERDICT_SHAPE
    else if (all_near(left, right)) then
      verdict = VERDICT_SAME
    else if (all_near(left, -right)) then
      verdict = VERDICT_NOFLIP
    else
      verdict = VERDICT_DIFF
    end if
  end function verdict_real_vector_with_stated_presence

  function color_for_verdict(verdict) result(color)
    character(len=*), intent(in) :: verdict
    character(len=5) :: color
    character(len=*), parameter :: escape = achar(27)
    character(len=*), parameter :: mismatch = escape//'[31m'

    select case (trim(verdict))
    case (trim(VERDICT_SAME))
      color = escape//'[97m'
    case (trim(VERDICT_NOFLIP), trim(VERDICT_DIFF))
      ! A missing required sign flip is a failed contract assertion, not a
      ! warning.  Keep it visually equivalent to an ordinary mismatch.
      color = mismatch
    case (trim(VERDICT_SHAPE))
      color = escape//'[35m'
    case (trim(VERDICT_ONLY4))
      color = escape//'[36m'
    case (trim(VERDICT_ONLY3))
      color = escape//'[34m'
    case (trim(VERDICT_ABSENT))
      color = escape//'[90m'
    case default
      ! Not a colour choice: the verdict set is closed, so anything else is a
      ! misspelling at a producer, and greying it out is how such a typo used
      ! to reach a report looking like an ordinary absence.
      error stop 'shim_comparison: unknown verdict in color_for_verdict'
    end select
  end function color_for_verdict

end module shim_comparison
