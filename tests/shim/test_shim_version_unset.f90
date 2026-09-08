! With IMAS_MVDD_HLI_DD_VERSION never set and no in-process setter call, the
! shim latches to "no conversion" and does zero version discovery on any seam
! (docs/SHIM_INTEGRATION_CONTRACT.md section 1, the "unset" row) -- not merely
! a version that happens to match.
!
! Reading the checked-in DD 3.39.0 fixture makes that concrete: its stamp
! names a version the shipped artifact *would* convert if discovery ran, so a
! renamed field staying unset here is proof nothing was looked up, not proof
! that nothing happened to differ.
program test_shim_version_unset
  use ids_routines, only: ids_equilibrium, OPEN_PULSE, imas_open, imas_close, ids_get, ids_real
  use al_get_policy, only: al_get_skipped_count
  implicit none

  real(ids_real), parameter :: absent_threshold = -1.0e40_ids_real

  type(ids_equilibrium) :: equilibrium
  character(len=512) :: fixture_root
  integer :: context, open_status, get_status

  call get_command_argument(1, fixture_root)
  if (len_trim(fixture_root) == 0) error stop 'missing fixture root'

  call imas_open('imas:hdf5?path='//trim(fixture_root)//'/dd-3.39.0', OPEN_PULSE, context, open_status)
  if (open_status /= 0) error stop 'version-unset open did not forward'

  call ids_get(context, 'equilibrium', equilibrium, get_status)
  call imas_close(context)

  if (get_status /= 0) error stop 'version-unset read did not forward cleanly'
  if (al_get_skipped_count() /= 0) error stop 'version-unset read logged a skipped path'
  if (.not. associated(equilibrium%time)) error stop 'version-unset read reached no data'
  if (size(equilibrium%time) /= 2) error stop 'version-unset read returned an unexpected time base'
  if (.not. associated(equilibrium%time_slice)) error stop 'version-unset read reached no time slices'

  ! beta_tor_norm is DD 4's renamed name for DD 3's beta_normal
  ! (shim_rule_table.f90, rule 'rename-beta-normal').  Undiscovered, the shim
  ! never consults that mapping, so the DD 4 name finds nothing in a DD 3.39.0
  ! file -- this stays at its ids_real_invalid default rather than arriving
  ! converted, which is what would happen if version discovery had run.
  if (equilibrium%time_slice(1)%global_quantities%beta_tor_norm > absent_threshold) then
    error stop 'version-unset read populated a renamed field: version discovery ran'
  end if
end program test_shim_version_unset
