! Synthetic truth table for the comparison oracle used by the shim contract
! suite.  A pulse can never prove its own comparator right, so every verdict is
! driven here from literals with an independently stated expected result.
program test_shim_comparison
  use ids_routines, only: ids_real, ids_int, ids_int_invalid
  use shim_comparison, only: verdict_real, verdict_integer, verdict_real_vector_with_stated_presence, color_for_verdict
  use shim_comparison, only: verdict_real_vector_as_read, verdict_real_matrix_as_read
  implicit none

  integer :: failures, expectations, index
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

  call expect(verdict_real(3.0_ids_real, 3.0_ids_real) == 'same', 'equal reals are same')
  call expect(verdict_real(3.0_ids_real, -3.0_ids_real) == 'NOFLIP', &
              'unflipped reals are NOFLIP')
  call expect(verdict_real(3.0_ids_real, 4.0_ids_real) == 'DIFF', 'different reals are DIFF')
  call expect(verdict_real(3.0_ids_real, absent_real) == 'only4', 'right-absent real is only4')
  call expect(verdict_real(absent_real, 3.0_ids_real) == 'only3', 'left-absent real is only3')
  call expect(verdict_real(absent_real, absent_real) == '--', 'both-absent reals are --')
  call expect(verdict_integer(7_ids_int, 7_ids_int) == 'same', 'equal integers are same')
  call expect(verdict_integer(7_ids_int, 8_ids_int) == 'DIFF', 'different integers are DIFF')
  call expect(verdict_integer(7_ids_int, absent_integer) == 'only4', 'right-absent integer is only4')
  call expect(verdict_integer(absent_integer, 7_ids_int) == 'only3', 'left-absent integer is only3')
  call expect(verdict_integer(absent_integer, absent_integer) == '--', 'both-absent integers are --')
  call expect(verdict_real_vector_with_stated_presence(.true., real_values, .true., real_values) == 'same', &
              'equal vectors are same')
  call expect(verdict_real_vector_with_stated_presence(.true., real_values, .true., flipped_values) == 'NOFLIP', &
              'unflipped vectors are NOFLIP')
  call expect(verdict_real_vector_with_stated_presence(.true., real_values, .true., other_values) == 'DIFF', &
              'different vectors are DIFF')
  call expect(verdict_real_vector_with_stated_presence(.true., real_values, .true., short_values) == 'SHAPE', &
              'different vector extents are SHAPE')
  call expect(verdict_real_vector_with_stated_presence(.true., real_values, .false., short_values) == 'only4', &
              'right-absent vector is only4')
  call expect(verdict_real_vector_with_stated_presence(.false., short_values, .true., real_values) == 'only3', &
              'left-absent vector is only3')
  call expect(verdict_real_vector_with_stated_presence(.false., short_values, .false., short_values) == '--', &
              'both-absent vectors are --')
  call expect(color_for_verdict('NOFLIP') == color_for_verdict('DIFF'), &
              'NOFLIP has mismatch severity')

  ! The trap verdict_real_vector_as_read exists to close.  Asserting presence
  ! that was never checked -- `.true.` for a side the shim served nothing for --
  ! makes two empty readings agree, because equal extents send all_near into a
  ! loop that runs zero times and returns .true.
  call expect(verdict_real_vector_with_stated_presence(.true., empty_values, .true., empty_values) == 'same', &
              'hardcoded presence makes two unserved vectors agree')
  call expect(verdict_real_vector_as_read(empty_values, empty_values) == '--', &
              'size-derived presence calls two unserved vectors absent')
  call expect(verdict_real_vector_as_read(real_values, empty_values) == 'only4', &
              'size-derived presence calls an unserved right side only4')
  call expect(verdict_real_vector_as_read(empty_values, real_values) == 'only3', &
              'size-derived presence calls an unserved left side only3')
  call expect(verdict_real_vector_as_read(real_values, real_values) == 'same', &
              'size-derived presence still agrees on two served vectors')

  ! verdict_real_matrix_as_read decides the 2-D structural and COCOS rules, so
  ! it is driven from literals here like every other verdict.
  call expect(verdict_real_matrix_as_read(real_matrix, real_matrix) == 'same', &
              'equal matrices are same')
  call expect(verdict_real_matrix_as_read(real_matrix, flipped_matrix) == 'NOFLIP', &
              'unflipped matrices are NOFLIP')
  call expect(verdict_real_matrix_as_read(real_matrix, other_matrix) == 'DIFF', &
              'different matrices are DIFF')
  call expect(verdict_real_matrix_as_read(real_matrix, bigger_matrix) == 'SHAPE', &
              'a different element count is SHAPE')
  call expect(verdict_real_matrix_as_read(real_matrix, empty_matrix) == 'only4', &
              'an unserved right matrix is only4')
  call expect(verdict_real_matrix_as_read(empty_matrix, real_matrix) == 'only3', &
              'an unserved left matrix is only3')
  call expect(verdict_real_matrix_as_read(empty_matrix, empty_matrix) == '--', &
              'two unserved matrices are absent')

  ! The limit of judging a matrix by its flattened elements, pinned rather than
  ! left to be discovered: only the element count survives the flatten, so a
  ! 2x2 and a 4x1 holding the same four numbers agree.  Every 2-D rule in the
  ! table compares one fixture's grid against the other's, where a fold that
  ! changed the grid also changes the count -- but a fold that transposed it
  ! would not be caught here.
  call expect(verdict_real_matrix_as_read(real_matrix, tall_matrix) == 'same', &
              'a reshape preserving the element count is not distinguished')

  call expect(expectations == 32, 'all synthetic verdict cases must run')

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
