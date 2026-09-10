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

  ! Why every public function below opens with a dummy nobody can pass.
  !
  ! `only3` and `only4` name *which side* was absent, so a verdict is only as
  ! trustworthy as the caller's knowledge of which reading it handed over
  ! first.  That knowledge used to live in a comment.  It did not survive:
  ! test_shim_structural_rules and test_shim_cocos_rules passed the
  ! shim-served reading first for every rule they check, so each of their
  ! served-nothing readings reported `only3` -- "a value on the DD 3 side
  ! only" -- for a reading whose value was in fact on the DD 4 side.  Pass and
  ! fail were unaffected, since both labels differ from `same`, so no
  ! assertion in the suite could notice; the wrong label went on to be
  ! transcribed into tests/shim/README.md as the account of what the shim did.
  !
  ! An argument order that is load-bearing, invisible at the call site, and
  ! unobservable in the result is not something a comment can protect.  So it
  ! is no longer an order: this type is private, a caller cannot construct one,
  ! and every value argument sits behind it -- which makes a positional call a
  ! type error naming this dummy, and leaves keyword form as the only way to
  ! call any of these functions.  Every call site therefore spells `oracle=`
  ! and `converted=`, and a reader checking one no longer has to count
  ! arguments.
  !
  ! What remains uncaught by the compiler is a call that names the sides
  ! correctly and feeds them the wrong readings.  That is what
  ! check_verdict_orientation.cmake looks for, registered as
  ! al-fortran-test-shim-verdict-orientation: with the keyword adjacent to its
  ! value, `oracle=eq_cross` is a grep away, whereas the positional swap it
  ! replaced was not expressible as a pattern at all.
  type :: name_the_sides_t
    ! Never read.  Present so the type is not empty.
    integer :: unused = 0
  end type name_the_sides_t

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
  ! `only4` and `only3` are statements about *roles*, not about positions:
  ! `only4` reads "the DD 4 oracle has a value and the shim served nothing",
  ! `only3` the reverse.  The roles are named by the caller because they
  ! cannot be recovered from the data -- see name_the_sides_t above for what
  ! happened while they were merely documented.
  function presence_verdict(name_the_sides, has_oracle, has_converted) result(verdict)
    type(name_the_sides_t), intent(in), optional :: name_the_sides
    logical, intent(in) :: has_oracle, has_converted
    character(len=verdict_len) :: verdict

    if (present(name_the_sides)) continue
    if (.not. has_oracle .and. .not. has_converted) then
      verdict = VERDICT_ABSENT
    else if (.not. has_converted) then
      verdict = VERDICT_ONLY4
    else if (.not. has_oracle) then
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

  function verdict_real(name_the_sides, oracle, converted) result(verdict)
    type(name_the_sides_t), intent(in), optional :: name_the_sides
    real(ids_real), intent(in) :: oracle, converted
    character(len=verdict_len) :: verdict

    if (present(name_the_sides)) continue
    verdict = presence_verdict(has_oracle = .not. is_absent_real(oracle), &
                               has_converted = .not. is_absent_real(converted))
    if (verdict /= '') return

    if (near(oracle, converted)) then
      verdict = VERDICT_SAME
    else if (near(oracle, -converted)) then
      ! A COCOS conversion was expected to yield equal HLI values.  Opposite
      ! signs therefore mean its required flip did not happen.
      verdict = VERDICT_NOFLIP
    else
      verdict = VERDICT_DIFF
    end if
  end function verdict_real

  function verdict_integer(name_the_sides, oracle, converted) result(verdict)
    type(name_the_sides_t), intent(in), optional :: name_the_sides
    integer(ids_int), intent(in) :: oracle, converted
    character(len=verdict_len) :: verdict

    if (present(name_the_sides)) continue
    verdict = presence_verdict(has_oracle = oracle /= ids_int_invalid, &
                               has_converted = converted /= ids_int_invalid)
    if (verdict /= '') return

    if (oracle == converted) then
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
  function verdict_real_vector_as_read(name_the_sides, oracle, converted) result(verdict)
    type(name_the_sides_t), intent(in), optional :: name_the_sides
    real(ids_real), intent(in) :: oracle(:), converted(:)
    character(len=verdict_len) :: verdict

    if (present(name_the_sides)) continue
    verdict = verdict_real_vector_with_stated_presence( &
                has_oracle = size(oracle) > 0, oracle = oracle, &
                has_converted = size(converted) > 0, converted = converted)
  end function verdict_real_vector_as_read

  ! A 2-D quantity judged as its flattened elements, presence read off the
  ! data as above.  The structural and COCOS tests each carried a private copy
  ! of this.  The element count still decides, so a fold that changed the grid
  ! reports SHAPE rather than quietly comparing a different number of points
  ! -- but only the count survives the flatten, so a reshape that preserved it
  ! would not be distinguished.  test_shim_comparison pins both.
  function verdict_real_matrix_as_read(name_the_sides, oracle, converted) result(verdict)
    type(name_the_sides_t), intent(in), optional :: name_the_sides
    real(ids_real), intent(in) :: oracle(:,:), converted(:,:)
    character(len=verdict_len) :: verdict

    if (present(name_the_sides)) continue
    verdict = verdict_real_vector_as_read(oracle = reshape(oracle, [size(oracle)]), &
                                          converted = reshape(converted, [size(converted)]))
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
  function verdict_real_vector_with_stated_presence(name_the_sides, has_oracle, oracle, &
                                                    has_converted, converted) result(verdict)
    type(name_the_sides_t), intent(in), optional :: name_the_sides
    logical, intent(in) :: has_oracle, has_converted
    real(ids_real), intent(in) :: oracle(:), converted(:)
    character(len=verdict_len) :: verdict

    if (present(name_the_sides)) continue
    verdict = presence_verdict(has_oracle = has_oracle, has_converted = has_converted)
    if (verdict /= '') return

    if (size(oracle) /= size(converted)) then
      verdict = VERDICT_SHAPE
    else if (all_near(oracle, converted)) then
      verdict = VERDICT_SAME
    else if (all_near(oracle, -converted)) then
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
