! Synthetic truth table for the comparison oracle used by the shim contract
! suite.  A pulse can never prove its own comparator right, so every verdict is
! driven here from literals with an independently stated expected verdict.
!
! Every case names its two sides -- `oracle=` for the DD 4.1.1 reading a rule
! is judged against, `converted=` for the shim-served one.  That is not a
! stylistic choice: shim_comparison rejects a positional call outright, because
! `only3` and `only4` say *which side* was absent and an argument order that
! decides which of them is produced is not something a comment can protect.
! See shim_comparison's name_the_sides_t for what happened while it was.
program test_shim_comparison
  use ids_routines, only: ids_real, ids_int, ids_int_invalid
  use shim_comparison, only: verdict_real, verdict_integer, verdict_real_vector_with_stated_presence, color_for_verdict
  use shim_comparison, only: verdict_real_vector_as_read, verdict_real_matrix_as_read
  use shim_run_guard, only: assert_ran_count
  implicit none

  integer :: failures, expectations, index
  ! Stated up front so a case lost in an edit fails the run rather than
  ! quietly shrinking it.  Raise it when adding one.
  integer, parameter :: expected_expectation_count = 34
  real(ids_real) :: absent_real, real_values(2), flipped_values(2), other_values(2), short_values(1)
  real(ids_real) :: empty_values(0)
  real(ids_real) :: real_matrix(2,2), flipped_matrix(2,2), other_matrix(2,2)
  real(ids_real) :: bigger_matrix(2,3), tall_matrix(4,1), empty_matrix(0,0)
  integer(ids_int) :: absent_integer

  failures = 0
  expectations = 0
  absent_real = -9.0e40_ids_real
  absent_integer = ids_int_invalid
  real_values = [1.0_ids_real, 2.0_ids_real]
  flipped_values = [-1.0_ids_real, -2.0_ids_real]
  other_values = [1.0_ids_real, 3.0_ids_real]
  short_values = [1.0_ids_real]
  real_matrix = reshape([1.0_ids_real, 2.0_ids_real, 3.0_ids_real, 4.0_ids_real], [2, 2])
  flipped_matrix = -real_matrix
  other_matrix = reshape([1.0_ids_real, 2.0_ids_real, 3.0_ids_real, 5.0_ids_real], [2, 2])
  bigger_matrix = reshape([(real(index, ids_real), index = 1, 6)], [2, 3])
  tall_matrix = reshape([1.0_ids_real, 2.0_ids_real, 3.0_ids_real, 4.0_ids_real], [4, 1])

  call expect(verdict_real(oracle = 3.0_ids_real, converted = 3.0_ids_real) == 'same', &
              'equal reals are same')
  call expect(verdict_real(oracle = 3.0_ids_real, converted = -3.0_ids_real) == 'NOFLIP', &
              'unflipped reals are NOFLIP')
  call expect(verdict_real(oracle = 3.0_ids_real, converted = 4.0_ids_real) == 'DIFF', &
              'different reals are DIFF')
  call expect(verdict_real(oracle = 3.0_ids_real, converted = absent_real) == 'only4', &
              'a real the shim served nothing for is only4')
  call expect(verdict_real(oracle = absent_real, converted = 3.0_ids_real) == 'only3', &
              'a real the oracle lacks is only3')
  call expect(verdict_real(oracle = absent_real, converted = absent_real) == '--', &
              'both-absent reals are --')
  call expect(verdict_integer(oracle = 7_ids_int, converted = 7_ids_int) == 'same', &
              'equal integers are same')
  call expect(verdict_integer(oracle = 7_ids_int, converted = 8_ids_int) == 'DIFF', &
              'different integers are DIFF')
  call expect(verdict_integer(oracle = 7_ids_int, converted = absent_integer) == 'only4', &
              'an integer the shim served nothing for is only4')
  call expect(verdict_integer(oracle = absent_integer, converted = 7_ids_int) == 'only3', &
              'an integer the oracle lacks is only3')
  call expect(verdict_integer(oracle = absent_integer, converted = absent_integer) == '--', &
              'both-absent integers are --')
  call expect(verdict_real_vector_with_stated_presence(has_oracle = .true., oracle = real_values, &
                                                       has_converted = .true., converted = real_values) == 'same', &
              'equal vectors are same')
  call expect(verdict_real_vector_with_stated_presence(has_oracle = .true., oracle = real_values, &
                                                       has_converted = .true., converted = flipped_values) == 'NOFLIP', &
              'unflipped vectors are NOFLIP')
  call expect(verdict_real_vector_with_stated_presence(has_oracle = .true., oracle = real_values, &
                                                       has_converted = .true., converted = other_values) == 'DIFF', &
              'different vectors are DIFF')
  call expect(verdict_real_vector_with_stated_presence(has_oracle = .true., oracle = real_values, &
                                                       has_converted = .true., converted = short_values) == 'SHAPE', &
              'different vector extents are SHAPE')
  call expect(verdict_real_vector_with_stated_presence(has_oracle = .true., oracle = real_values, &
                                                       has_converted = .false., converted = short_values) == 'only4', &
              'a vector the shim served nothing for is only4')
  call expect(verdict_real_vector_with_stated_presence(has_oracle = .false., oracle = short_values, &
                                                       has_converted = .true., converted = real_values) == 'only3', &
              'a vector the oracle lacks is only3')
  call expect(verdict_real_vector_with_stated_presence(has_oracle = .false., oracle = short_values, &
                                                       has_converted = .false., converted = short_values) == '--', &
              'both-absent vectors are --')
  call expect(color_for_verdict('NOFLIP') == color_for_verdict('DIFF'), &
              'NOFLIP has mismatch severity')

  ! The asymmetry the rest of the suite leans on, pinned rather than assumed.
  !
  ! `only3` and `only4` are the only two verdicts that depend on which reading
  ! is which, and the whole point of naming the sides is that naming them the
  ! other way round produces a different answer.  These two cases fail if the
  ! roles ever stop being distinguished -- if presence_verdict were made
  ! symmetric, say, or if both arms were changed to return the same label --
  ! which is the state in which a swapped call site would become undetectable
  ! again.  They are the primitive-level half of that guard;
  ! check_verdict_orientation.cmake is the call-site half.
  call expect(verdict_real(oracle = 3.0_ids_real, converted = absent_real) /= &
              verdict_real(oracle = absent_real, converted = 3.0_ids_real), &
              'naming the sides the other way round changes the verdict')
  call expect(verdict_real_matrix_as_read(oracle = real_matrix, converted = empty_matrix) /= &
              verdict_real_matrix_as_read(oracle = empty_matrix, converted = real_matrix), &
              'the same asymmetry holds for the array primitives')

  ! The trap verdict_real_vector_as_read exists to close.  Asserting presence
  ! that was never checked -- `.true.` for a side the shim served nothing for --
  ! makes two empty readings agree, because equal extents send all_near into a
  ! loop that runs zero times and returns .true.
  call expect(verdict_real_vector_with_stated_presence(has_oracle = .true., oracle = empty_values, &
                                                       has_converted = .true., converted = empty_values) == 'same', &
              'hardcoded presence makes two unserved vectors agree')
  call expect(verdict_real_vector_as_read(oracle = empty_values, converted = empty_values) == '--', &
              'size-derived presence calls two unserved vectors absent')
  call expect(verdict_real_vector_as_read(oracle = real_values, converted = empty_values) == 'only4', &
              'size-derived presence calls an unserved converted side only4')
  call expect(verdict_real_vector_as_read(oracle = empty_values, converted = real_values) == 'only3', &
              'size-derived presence calls an unserved oracle side only3')
  call expect(verdict_real_vector_as_read(oracle = real_values, converted = real_values) == 'same', &
              'size-derived presence still agrees on two served vectors')

  ! verdict_real_matrix_as_read decides the 2-D structural and COCOS rules, so
  ! it is driven from literals here like every other verdict.
  call expect(verdict_real_matrix_as_read(oracle = real_matrix, converted = real_matrix) == 'same', &
              'equal matrices are same')
  call expect(verdict_real_matrix_as_read(oracle = real_matrix, converted = flipped_matrix) == 'NOFLIP', &
              'unflipped matrices are NOFLIP')
  call expect(verdict_real_matrix_as_read(oracle = real_matrix, converted = other_matrix) == 'DIFF', &
              'different matrices are DIFF')
  call expect(verdict_real_matrix_as_read(oracle = real_matrix, converted = bigger_matrix) == 'SHAPE', &
              'a different element count is SHAPE')
  call expect(verdict_real_matrix_as_read(oracle = real_matrix, converted = empty_matrix) == 'only4', &
              'an unserved converted matrix is only4')
  call expect(verdict_real_matrix_as_read(oracle = empty_matrix, converted = real_matrix) == 'only3', &
              'an unserved oracle matrix is only3')
  call expect(verdict_real_matrix_as_read(oracle = empty_matrix, converted = empty_matrix) == '--', &
              'two unserved matrices are absent')

  ! The limit of judging a matrix by its flattened elements, pinned rather than
  ! left to be discovered: only the element count survives the flatten, so a
  ! 2x2 and a 4x1 holding the same four numbers agree.  Every 2-D rule in the
  ! table compares one fixture's grid against the other's, where a fold that
  ! changed the grid also changes the count -- but a fold that transposed it
  ! would not be caught here.
  call expect(verdict_real_matrix_as_read(oracle = real_matrix, converted = tall_matrix) == 'same', &
              'a reshape preserving the element count is not distinguished')

  call assert_ran_count('COMPARISON-FAILURE', 'synthetic verdict cases ran', &
                        expectations, expected_expectation_count, failures)

  if (failures > 0) then
    write(*, '(a,i0,a)') 'COMPARISON-FAILURE: ', failures, ' expectation(s) failed'
    stop 1
  end if

contains

  subroutine expect(condition, what)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: what

    expectations = expectations + 1
    if (.not. condition) then
      write(*, '(a,a)') 'COMPARISON-FAILURE: ', what
      failures = failures + 1
    end if
  end subroutine expect

end program test_shim_comparison
