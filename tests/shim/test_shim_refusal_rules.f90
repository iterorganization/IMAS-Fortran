! Assert the rules the conversion map refuses, and assert that a refusal
! arrives on all three of the channels that carry one.
!
! A refusal, a tolerated refusal and a partial outcome are three distinct
! things travelling on three different channels, and a suite that checked only
! one of them would pass while the other two were silently broken:
!
!   1. the value channel  -- the field is left unset, so a caller sees an
!      absent quantity rather than a converted one or a defaulted zero;
!   2. the skip log       -- al_get_policy names the refused path, so the
!      refusal is discoverable rather than merely survivable;
!   3. the status         -- ids_get returns PARTIAL_READ, so a caller's
!      `status .ne. 0` test fires.
!
! Tolerating a refusal without reporting it is therefore a failure here, not a
! success: a read that left the field unset, continued to the end and returned
! 0 would satisfy channel 1 and fail channels 2 and 3.
!
! The rules asserted are the map's one `retyped` rule and its four unit
! `redefine` globs (shim_rule_table's `refusal_rules`), and the check that
! matters most is the negative one -- that no value arrives. For the four
! redefined paths the two fixtures hold the *same number* by construction
! (imas-python-fixtures/README.md, "Redefinitions the map refuses"), because
! a redefinition has nothing to apply. A shim that passed the DD 3 number
! through would therefore look perfect to a value comparison while handing a
! physicist a number that no longer means what its label says. What separates
! the two is the absence of the value plus a named entry in the skip log,
! which is why every rule below is asserted on both.
!
! Argument order is load-bearing. The comparison primitives name their absent
! side `only3` / `only4`, so the DD 4.1.1 control read is passed FIRST and the
! shim's cross-version read SECOND; a served-nothing then reads as `only4`,
! which is the verdict shim_rule_table expects for both refusal kinds.
!
! A failing check names the rule that broke -- id, kind and cited source --
! not only the field.
!
! FOUR OF THESE ASSERTIONS ARE RED ON ARRIVAL, AND THAT IS DELIBERATE.
!
! The four `chi_squared_{r,z}` paths are refused by the shim today, and the
! conversion map marks them fidelity="unmappable" to say so. That marking is
! the defect. Both dictionaries hold the same number -- there is nothing to
! apply to a redefinition, so imas-python-fixtures writes the DD 3 number
! unchanged into both fixtures -- so the number should be delivered, and this
! test asserts that the two sides agree.
!
! It therefore fails until the shim stops refusing them. Per docs/adr/0002 the
! assertion is not inverted with WILL_FAIL, not moved out of the default run
! and not softened to match what the shim does: all three would make the suite
! green while the defect stands, and someone would have to remember to come
! back and flip it. As written it turns green by itself when the shim is
! fixed. ADR 0002's Consequences already lists these four paths among the
! suite's contract assertions known to be red on arrival, and #72 tracks the fix.
!
! The `retyped` rule above them is the opposite case: its refusal is correct,
! because no value transformation reshapes an INT_1D into an array of
! identifier structures, and it is asserted as a tolerated refusal.
program test_shim_refusal_rules
  use ids_routines, only: ids_equilibrium, ids_equilibrium_constraints_pure_position, &
                          ids_real, ids_real_invalid, ids_int, ids_int_invalid
  use al_defs, only: MAX_ERR_MSG_LEN, is_external_refusal
  use al_get_policy, only: PARTIAL_READ, al_get_skipped_count, al_get_skipped_path, &
                           AL_SKIP_PATH_LEN, AL_SKIP_LOG_CAPACITY
  use shim_fixture_pair, only: fixture_root_from_command, read_cross_version, &
                               read_same_version
  use shim_comparison, only: verdict_real, verdict_integer, verdict_len
  use shim_rule_check, only: rule_checker
  use shim_run_guard, only: assert_ran_count
  use shim_rule_table, only: refusal_rules, expected_verdict_for_kind, &
                             kind_name, retyped_refusal_reason, redefined_refusal_reason
  implicit none

  type(ids_equilibrium) :: eq_cross, eq_control
  character(len=512) :: fixture_root
  integer :: status_cross, status_control
  integer :: expectations, rules_checked
  type(rule_checker) :: checker

  ! A copy of the read-side skip log taken while it still describes the
  ! cross-version read.
  character(len=AL_SKIP_PATH_LEN) :: skip_path(AL_SKIP_LOG_CAPACITY)
  character(len=MAX_ERR_MSG_LEN)  :: skip_message(AL_SKIP_LOG_CAPACITY)
  integer :: skip_code(AL_SKIP_LOG_CAPACITY)
  integer :: skip_retained, skip_total

  ! How many expect() calls precede the guard at the end that checks this
  ! number, so that a check quietly lost in an edit fails the run instead of
  ! silently shrinking it. Raise it when adding one.
  integer, parameter :: expected_expectation_count = 11

  fixture_root = fixture_root_from_command()

  checker%rules = refusal_rules
  checker%marker = 'REFUSAL-FAILURE'
  expectations = 0
  rules_checked = 0

  ! The DD 3.39.0 pulse, read through the shim by a DD 4.1.1 HLI: the read
  ! that must refuse.
  call read_cross_version(fixture_root, eq_cross, status_cross)

  ! al_get_policy resets its log at the start of every ids_get, so the log
  ! has to be copied out here -- the control read below would otherwise erase
  ! the entries this test exists to assert.
  call snapshot_skip_log()

  ! The DD 4.1.1 pulse read same-version: no conversion, so it is the oracle
  ! for what each refused path would have held had it been servable.
  call read_same_version(fixture_root, eq_control, status_control)

  ! Preconditions, asserted rather than assumed: without either of these the
  ! verdicts below would be absent-versus-absent and would prove nothing.
  if (status_control /= 0) then
    write(*, '(a)') 'REFUSAL-FAILURE: the DD 4.1.1 control read did not succeed cleanly'
    stop 1
  end if
  if (.not. time_slice_reached(eq_control)) then
    write(*, '(a)') 'REFUSAL-FAILURE: the DD 4.1.1 control read produced no time_slice'
    stop 1
  end if

  ! The traversal declines to abort. grids_ggd precedes time_slice, so the
  ! refused `retyped` rule is passed through before time_slice is reached at
  ! all: arriving here with a time_slice is the proof that a refusal cost the
  ! read nothing but the refused path itself.
  if (.not. time_slice_reached(eq_cross)) then
    write(*, '(a)') 'REFUSAL-FAILURE: the cross-version read did not continue past the refusal into time_slice'
    stop 1
  end if

  ! The log is a fixed 64 slots and keeps only the first that many, while
  ! reporting the true total. Today's cross-version read skips 24, so an
  ! overflow would mean something changed substantially -- and would make the
  ! named-path checks below blame the shim for a refusal it did record.
  call expect(skip_total <= AL_SKIP_LOG_CAPACITY, &
              'the skip log retained every entry it recorded')

  ! -- Channel 3: the read reports a partial outcome. --------------------
  call expect(status_cross == PARTIAL_READ, &
              'the cross-version read reports PARTIAL_READ')

  ! -- The retyped rule. -------------------------------------------------
  ! The refusal fires on the coordinates_type arraystruct inside a space, so
  ! the containers around it must have survived for the refusal to be the
  ! thing being observed.
  call expect(space_reached(eq_cross), &
              'the cross-version read reached grids_ggd/grid/space')

  ! Channel 1: an absent field, not a defaulted one. An unassociated pointer
  ! is byte-for-byte what an absent path looks like, which is the point: no
  ! consumer can mistake an unserved coordinate list for a served empty one.
  call expect(space_reached(eq_cross) .and. .not. coordinates_type_present(eq_cross), &
              'the refused coordinates_type is absent rather than defaulted')

  ! Tolerated, and locally so: objects_per_dimension is read after
  ! coordinates_type within the same space, so its arrival proves the refusal
  ! was absorbed at the field rather than truncating the enclosing struct.
  call expect(objects_per_dimension_present(eq_cross), &
              'the space read continued past the refusal to objects_per_dimension')

  ! Channel 2: named in the skip log, with the reason the contract froze.
  call check_refused_in_skip_log('retype-coordinates-type', retyped_refusal_reason)

  call check_rule('retype-coordinates-type', &
                  verdict_integer(first_coordinates_index(eq_control), &
                                  first_coordinates_index(eq_cross)))

  ! A served field from after the refusal, to show the read carried on
  ! serving data and not merely carried on. beta_pol is the structural
  ! table's `identical-beta-pol`.
  call expect(verdict_real(eq_control%time_slice(1)%global_quantities%beta_pol, &
                           eq_cross%time_slice(1)%global_quantities%beta_pol) == 'same', &
              'a field after the refusal is still served')

  ! -- The four unit-redefinition rules: asserted served, red until the
  !    shim stops refusing them.  See the header. -------------------------
  call check_served('redefine-x-point-chi-sq-r', &
                       chi_squared_r_of(eq_control%time_slice(1)%constraints%x_point), &
                       chi_squared_r_of(eq_cross%time_slice(1)%constraints%x_point))
  call check_served('redefine-x-point-chi-sq-z', &
                       chi_squared_z_of(eq_control%time_slice(1)%constraints%x_point), &
                       chi_squared_z_of(eq_cross%time_slice(1)%constraints%x_point))
  call check_served('redefine-strike-pt-chi-sq-r', &
                       chi_squared_r_of(eq_control%time_slice(1)%constraints%strike_point), &
                       chi_squared_r_of(eq_cross%time_slice(1)%constraints%strike_point))
  call check_served('redefine-strike-pt-chi-sq-z', &
                       chi_squared_z_of(eq_control%time_slice(1)%constraints%strike_point), &
                       chi_squared_z_of(eq_cross%time_slice(1)%constraints%strike_point))

  ! -- Nothing here may pass by doing nothing. ---------------------------
  call assert_ran_count(checker%marker, 'expectations in this program ran', &
                        expectations, expected_expectation_count, checker%failures)

  call checker%assert_every_rule_checked(rules_checked)

  if (checker%failures > 0) then
    write(*, '(a,i0,a)') 'REFUSAL-FAILURE: ', checker%failures, ' refusal expectation(s) failed'
    stop 1
  end if

contains

  ! Copy the skip log out of al_get_policy's process-global storage while it
  ! still describes the read just finished.
  subroutine snapshot_skip_log()
    integer :: i
    logical :: found

    skip_total = al_get_skipped_count()
    skip_retained = min(skip_total, AL_SKIP_LOG_CAPACITY)
    do i = 1, skip_retained
      call al_get_skipped_path(i, skip_path(i), skip_code(i), skip_message(i), found)
      if (.not. found) then
        ! Fewer entries retained than the count promised; stop at what is
        ! actually there rather than reading uninitialised slots.
        skip_retained = i - 1
        return
      end if
    end do
  end subroutine snapshot_skip_log

  ! Is this path in the skip log, refused for the stated reason?
  !
  ! Three things are matched, and each earns its place. The log's own path
  ! field carries the field name the traversal was at, which does not say
  ! *which* x_point or strike_point path refused -- so the full DD path is
  ! matched inside the layer's message, which carries it. The reason string
  ! is matched because a test that accepted any refusal would not notice the
  ! wrong rule firing. And the status is matched against the refusal band, so
  ! an ordinary I/O error recorded here could not satisfy the assertion.
  !
  ! Substring matching on the message is what the contract prescribes
  ! (docs/SHIM_INTEGRATION_CONTRACT.md 8.1): the message is truncated to
  ! MAX_ERR_MSG_LEN, versions first and then the path from the left. These
  ! paths are short enough that neither happens, which 8.1 names as the
  ! condition for asserting more than the reason alone.
  logical function skip_log_names(field, reason, dd_path)
    character(len=*), intent(in) :: field, reason, dd_path
    integer :: i

    skip_log_names = .false.
    do i = 1, skip_retained
      if (.not. ends_with(trim(skip_path(i)), field)) cycle
      if (index(skip_message(i), reason) == 0) cycle
      if (index(skip_message(i), dd_path) == 0) cycle
      if (.not. is_external_refusal(skip_code(i))) cycle
      skip_log_names = .true.
      return
    end do
  end function skip_log_names

  ! The skip log records the field name the traversal was at, which today is
  ! the bare leaf name. Matching on the tail keeps the assertion honest if a
  ! future generator prefixes it with the containing path.
  logical function ends_with(text, tail)
    character(len=*), intent(in) :: text, tail

    ends_with = .false.
    if (len(tail) > len(text)) return
    ends_with = text(len(text) - len(tail) + 1:) == tail
  end function ends_with

  logical function time_slice_reached(eq)
    type(ids_equilibrium), intent(in) :: eq

    time_slice_reached = .false.
    if (.not. associated(eq%time_slice)) return
    if (size(eq%time_slice) < 1) return
    time_slice_reached = .true.
  end function time_slice_reached

  logical function space_reached(eq)
    type(ids_equilibrium), intent(in) :: eq

    space_reached = .false.
    if (.not. associated(eq%grids_ggd)) return
    if (size(eq%grids_ggd) < 1) return
    if (.not. associated(eq%grids_ggd(1)%grid)) return
    if (size(eq%grids_ggd(1)%grid) < 1) return
    if (.not. associated(eq%grids_ggd(1)%grid(1)%space)) return
    if (size(eq%grids_ggd(1)%grid(1)%space) < 1) return
    space_reached = .true.
  end function space_reached

  logical function coordinates_type_present(eq)
    type(ids_equilibrium), intent(in) :: eq

    coordinates_type_present = .false.
    if (.not. space_reached(eq)) return
    coordinates_type_present = associated(eq%grids_ggd(1)%grid(1)%space(1)%coordinates_type)
  end function coordinates_type_present

  logical function objects_per_dimension_present(eq)
    type(ids_equilibrium), intent(in) :: eq

    objects_per_dimension_present = .false.
    if (.not. space_reached(eq)) return
    objects_per_dimension_present = associated(eq%grids_ggd(1)%grid(1)%space(1)%objects_per_dimension)
  end function objects_per_dimension_present

  ! The coordinate-type code as this HLI sees it: DD 4.1.1 keeps in an
  ! identifier struct the integer DD 3.39.0 kept in a flat list, and it is
  ! that reshaping the map refuses. Absent reads back as ids_int_invalid, so
  ! the comparison primitives report it as an absent side rather than as a
  ! value of zero.
  function first_coordinates_index(eq) result(value)
    type(ids_equilibrium), intent(in) :: eq
    integer(ids_int) :: value

    value = ids_int_invalid
    if (.not. coordinates_type_present(eq)) return
    if (size(eq%grids_ggd(1)%grid(1)%space(1)%coordinates_type) < 1) return
    value = eq%grids_ggd(1)%grid(1)%space(1)%coordinates_type(1)%index
  end function first_coordinates_index

  function chi_squared_r_of(points) result(value)
    type(ids_equilibrium_constraints_pure_position), pointer, intent(in) :: points(:)
    real(ids_real) :: value

    value = ids_real_invalid
    if (.not. associated(points)) return
    if (size(points) < 1) return
    value = points(1)%chi_squared_r
  end function chi_squared_r_of

  function chi_squared_z_of(points) result(value)
    type(ids_equilibrium_constraints_pure_position), pointer, intent(in) :: points(:)
    real(ids_real) :: value

    value = ids_real_invalid
    if (.not. associated(points)) return
    if (size(points) < 1) return
    value = points(1)%chi_squared_z
  end function chi_squared_z_of

  ! One unit-redefinition rule, asserted on both channels at once: the value
  ! must not arrive, and the refusal must be discoverable by name.
  ! One path the contract expects delivered, asserted on both channels: the
  ! value must arrive, and no refusal for it may appear in the skip log.
  ! Both halves are red while the shim refuses the path -- the second one is
  ! what names the refusal that should not have happened.
  subroutine check_served(id, control_value, cross_value)
    character(len=*), intent(in) :: id
    real(ids_real), intent(in) :: control_value, cross_value
    character(len=96) :: dd_path

    dd_path = checker%rules(checker%find(id))%hli_path
    call expect(.not. skip_log_names(last_segment(trim(dd_path)), &
                                     redefined_refusal_reason, trim(dd_path)), &
                'no unit-redefinition refusal is recorded for '//trim(dd_path))
    call check_rule(id, verdict_real(control_value, cross_value))
  end subroutine check_served

  ! Channel 2 for one rule, with the path taken from the rule table rather
  ! than repeated here: the table already holds it, and a second copy at the
  ! call site is a copy that can drift away from the entry it describes.
  subroutine check_refused_in_skip_log(id, reason)
    character(len=*), intent(in) :: id, reason
    character(len=96) :: dd_path

    dd_path = checker%rules(checker%find(id))%hli_path
    call expect(skip_log_names(last_segment(trim(dd_path)), reason, trim(dd_path)), &
                'the skip log names '//trim(dd_path)//' as refused')
  end subroutine check_refused_in_skip_log

  ! The leaf name, which is what the skip log records as the path it was at.
  ! Deferred-length on purpose: a fixed-length result would pad the name with
  ! blanks, and the tail match below would then be looking for a string
  ! longer than any entry the log holds.
  function last_segment(path) result(segment)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: segment
    integer :: slash

    slash = index(path, '/', back=.true.)
    segment = path(slash + 1:)
  end function last_segment

  ! Compare one rule's observed verdict against the verdict its kind expects,
  ! and report the rule rather than the field when they differ.
  subroutine check_rule(id, verdict)
    character(len=*), intent(in) :: id
    character(len=verdict_len), intent(in) :: verdict

    rules_checked = rules_checked + 1
    call checker%check(id, verdict)
  end subroutine check_rule


  subroutine expect(condition, what)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: what

    expectations = expectations + 1
    if (.not. condition) call checker%fail(what)
  end subroutine expect

end program test_shim_refusal_rules
