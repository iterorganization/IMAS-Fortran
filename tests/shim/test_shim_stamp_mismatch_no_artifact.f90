! A stamp that differs from the HLI version but names a (IDS, stored, HLI)
! triple with no embedded artifact registers nothing (occurrence-open table,
! docs/SHIM_INTEGRATION_CONTRACT.md section 3, third row): the mismatch is
! detected, `known_artifacts::lookup` returns nothing, and every later data
! seam on the occurrence forwards completely unconverted.
!
! The contract is explicit (section 2.2's "a version mismatch alone does not
! imply conversion", and section 9's "cannot observe" list) that this is
! byte-for-byte indistinguishable at the ABI from test_shim_stamp_equal.f90's
! stamp-equal case: both forward the same bytes, the same way, with nothing
! logged. So this test is NOT distinguished from that one by anything it can
! observe -- there is no assertion that could tell "mismatch, no artifact"
! apart from "no mismatch at all" from outside the shim. It is distinguished
! by intent: this fixture's stamp (tests/shim/derive_stamp_variant.py's
! "mismatch" state, 3.40.0 -- a grammar-valid, known DD release the shipped
! artifact carries no rule for) is deliberately unequal to the HLI version,
! so this test exists to write down "a mismatch here still forwards
! unconverted" as a scenario in its own right, not to prove a difference from
! the stamp-equal case that the contract says cannot be proven.
program test_shim_stamp_mismatch_no_artifact
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get, ids_real
  use al_get_policy, only: al_get_skipped_count
  implicit none

  real(ids_real), parameter :: absent_threshold = -1.0e40_ids_real

  type(ids_equilibrium) :: equilibrium
  character(len=512) :: fixture
  integer :: context, open_status, get_status

  call get_command_argument(1, fixture)
  if (len_trim(fixture) == 0) error stop 'missing stamp-mismatch fixture'

  call imas_open('imas:hdf5?path='//trim(fixture), OPEN_PULSE, context, open_status)
  if (open_status /= 0) error stop 'stamp-mismatch-no-artifact open did not forward'

  call ids_get(context, 'equilibrium', equilibrium, get_status)
  call imas_close(context)

  if (get_status /= 0) error stop 'stamp-mismatch-no-artifact read did not forward cleanly'
  if (al_get_skipped_count() /= 0) error stop 'stamp-mismatch-no-artifact read logged a skipped path'
  if (.not. associated(equilibrium%time)) error stop 'stamp-mismatch-no-artifact read reached no data'
  if (size(equilibrium%time) /= 2) error stop 'stamp-mismatch-no-artifact read returned an unexpected time base'
  if (.not. associated(equilibrium%time_slice)) error stop 'stamp-mismatch-no-artifact read reached no time slices'

  ! Same renamed-field check as test_shim_version_unset.f90, for the same
  ! reason: a value here would mean a rule fired for a pair the artifact does
  ! not cover, which is a contradiction, not a pass.
  if (equilibrium%time_slice(1)%global_quantities%beta_tor_norm > absent_threshold) then
    error stop 'stamp-mismatch-no-artifact read populated a renamed field: conversion ran with no artifact'
  end if
end program test_shim_stamp_mismatch_no_artifact
