module al_defs

  integer, parameter :: MAXDIM             = 7

  integer, parameter :: OP_INTERP_0        = 0
  integer, parameter :: BACKEND_ID_0       = 10
  integer, parameter :: OP_RANGE_0         = 20
  integer, parameter :: OP_ACCESS_0        = 30
  integer, parameter :: ACCESS_PULSE_0     = 40
  integer, parameter :: DATA_TYPE_0        = 50
  integer, parameter :: SERIALIZER_PROTOCOL_0 = 60
  integer, parameter :: ERR_0              = -1

  integer, parameter :: NO_BACKEND         = BACKEND_ID_0
  integer, parameter :: ASCII_BACKEND      = BACKEND_ID_0+1
  integer, parameter :: MDSPLUS_BACKEND    = BACKEND_ID_0+2
  integer, parameter :: HDF5_BACKEND       = BACKEND_ID_0+3
  integer, parameter :: MEMORY_BACKEND     = BACKEND_ID_0+4
  integer, parameter :: UDA_BACKEND        = BACKEND_ID_0+5

  integer, parameter :: TIMED              = 1
  integer, parameter :: NON_TIMED          = 0

  integer, parameter :: GLOBAL_OP          = OP_RANGE_0
  integer, parameter :: SLICE_OP           = OP_RANGE_0+1

  integer, parameter :: READ_OP            = OP_ACCESS_0
  integer, parameter :: WRITE_OP           = OP_ACCESS_0+1
  integer, parameter :: REPLACE_OP         = OP_ACCESS_0+2

  integer, parameter :: UNDEFINED_INTERP   = OP_INTERP_0
  integer, parameter :: CLOSEST_INTERP     = OP_INTERP_0+1
  integer, parameter :: PREVIOUS_INTERP    = OP_INTERP_0+2
  integer, parameter :: LINEAR_INTERP      = OP_INTERP_0+3

  real(8), parameter :: UNDEFINED_TIME     = -999.

  integer, parameter :: OPEN_PULSE         = ACCESS_PULSE_0
  integer, parameter :: FORCE_OPEN_PULSE   = ACCESS_PULSE_0+1
  integer, parameter :: CREATE_PULSE       = ACCESS_PULSE_0+2
  integer, parameter :: FORCE_CREATE_PULSE = ACCESS_PULSE_0+3
  integer, parameter :: CLOSE_PULSE        = ACCESS_PULSE_0+4
  integer, parameter :: ERASE_PULSE        = ACCESS_PULSE_0+5

  integer, parameter :: CHAR_DATA          = DATA_TYPE_0
  integer, parameter :: INTEGER_DATA       = DATA_TYPE_0+1
  integer, parameter :: DOUBLE_DATA        = DATA_TYPE_0+2
  integer, parameter :: COMPLEX_DATA       = DATA_TYPE_0+3

  integer, parameter :: ASCII_SERIALIZER_PROTOCOL = SERIALIZER_PROTOCOL_0
  integer, parameter :: FLEXBUFFERS_SERIALIZER_PROTOCOL = SERIALIZER_PROTOCOL_0+1
  integer, parameter :: DEFAULT_SERIALIZER_PROTOCOL = FLEXBUFFERS_SERIALIZER_PROTOCOL

  integer, parameter :: UNKNOWN_ERR        = ERR_0
  integer, parameter :: CONTEXT_ERR        = ERR_0-1
  integer, parameter :: BACKEND_ERR        = ERR_0-2
  integer, parameter :: LOWLEVEL_ERR       = ERR_0-3
  integer, parameter :: CONSISTENCY_ERR    = ERR_0-4

  ! IMAS-Core allocates only the ERR_0 family above, -1..-5. The band -1000..-1099
  ! is reserved for a layer interposed between this HLI and IMAS-Core; the
  ! IMAS-Multiversion-DD-Loader currently allocates just -1000 in it
  ! (IMAS_MVDD_CONVERSION_ERROR), meaning "this path cannot be served in the
  ! caller's dictionary". Disjoint from the family above by construction, so a
  ! build that links IMAS-Core directly can never produce one of these.
  integer, parameter :: AL_EXTERNAL_REFUSAL_MAX = -1000
  integer, parameter :: AL_EXTERNAL_REFUSAL_MIN = -1099

  ! Not an error: the read completed, but at least one path was refused and its
  ! field left unset. Positive, so it cannot collide with any status coming from
  ! the C ABI, while still tripping the `status.ne.0` test callers already write.
  ! See al_get_policy for the skip log that says which paths.
  integer, parameter :: PARTIAL_READ        = 1

  ! The write counterpart: the operation completed, but at least one path was
  ! refused. Positive for the same reason, and distinct from PARTIAL_READ so
  ! that a caller doing both can tell which half was incomplete. See
  ! al_put_policy for why a refused write is tolerated at all, and for what it
  ! does and does not record about the affected paths.
  !
  ! Two refusals reach a caller through this one code, and they are not the
  ! same thing. From ids_put or ids_put_slice, a refused *write*: a value the
  ! caller supplied was not stored. From ids_delete, or from the delete ids_put
  ! performs first, a refused *delete*: a value the caller did not supply was
  ! not removed, so the occurrence still holds data the operation meant to
  ! replace. The printed record distinguishes them by wording; the status
  ! deliberately does not, because the caller's question at this level is the
  ! same either way -- the occurrence is not what I asked for, go and read what
  ! was refused.
  integer, parameter :: PARTIAL_PUT         = 2

  integer, parameter :: IDS_TIME_MODE_UNKNOWN = -999999999
  integer, parameter :: IDS_TIME_MODE_HETEROGENEOUS = 0
  integer, parameter :: IDS_TIME_MODE_HOMOGENEOUS = 1
  integer, parameter :: IDS_TIME_MODE_INDEPENDENT = 2

  integer, parameter :: MAX_ERR_MSG_LEN    = 256

contains

  ! Is this status an interposing layer saying "I cannot serve this path"? Lives
  ! here rather than in either policy module because both need it and neither
  ! owns it: it is a fact about the status band declared just above, not a
  ! decision about what to do with one. al_get_policy and al_put_policy each
  ! re-export it by using this module, so there is one entity and no ambiguity
  ! in a scope that can see both.
  pure logical function is_external_refusal(code)
    integer, intent(in) :: code
    is_external_refusal = (code .le. AL_EXTERNAL_REFUSAL_MAX) .and. &
                          (code .ge. AL_EXTERNAL_REFUSAL_MIN)
  end function is_external_refusal

  function default_backend() 
    integer :: default_backend
    character (len=255) :: backend_value
    integer :: l
    call get_environment_variable("IMAS_AL_DEFAULT_BACKEND", backend_value, l)
    if (l.gt.0) then
       read(backend_value,"(I2)") default_backend
    else
       default_backend = MDSPLUS_BACKEND
    end if
  end function default_backend

  function fallback_backend()
    integer :: fallback_backend
    character (len=255) :: backend_value
    integer :: l
    call get_environment_variable("IMAS_AL_FALLBACK_BACKEND", backend_value, l)
    if (l.gt.0) then
       read(backend_value,"(I2)") fallback_backend
    else
       fallback_backend = NO_BACKEND
    end if
  end function fallback_backend
  
end module al_defs
