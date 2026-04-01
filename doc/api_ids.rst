IDS API
=======

.. highlight:: fortran

..
    Subroutines are generated, so choosing to document manual instead.

.. f:subroutine:: ids_get(pulsectx, name, IDS, retstatus)

    Read the contents of the an IDS into memory.

    This method fetches the IDS in its entirety, with all time slices it may
    contain. See :f:func:`ids_get_slice` for reading a specific time slice.

    Empty fields within the IDS in the Data Entry are returned with the
    default values indicated in :ref:`Default values`.

    :param integer pulsectx [in]: Data entry context created with
        :f:func:`imas_open`, :f:func:`imas_open_env` or
        :f:func:`imas_create_env`
    :param character(*) name [in]: name of the ids with optional occurrence
        number, e.g. ``"core_profiles"`` (for occurrence 0),
        ``"core_profiles/1"`` (for occurrence 1)
    :param IDS: IDS object to fill
    :option integer retstatus [out]: Status code: ``0`` on success, ``<0`` on failure
    :example: .. literalinclude:: code_samples/dbentry_get

.. f:subroutine:: ids_get_slice(pulsectx, name, IDS, twant, interpol, retstatus)

    Read a single time slice from an IDS in this Database Entry.

    This method fetches the IDS object with all constant/static data filled.
    The dynamic data is interpolated on the requested time slice. This means
    that the size of the time dimension in the returned data is 1.

    :param integer pulsectx [in]: Data entry context created with
        :f:func:`imas_open`, :f:func:`imas_open_env` or
        :f:func:`imas_create_env`
    :param character(*) name [in]: name of the ids with optional occurrence
        number, e.g. ``"core_profiles"`` (for occurrence 0),
        ``"core_profiles/1"`` (for occurrence 1)
    :param IDS: IDS object to fill
    :param real twant [in]: Requested time slice
    :param integer interpol [in]: Interpolation method to use, see :ref:`Load a
            single \`time slice\` of an IDS`
    :option integer retstatus [out]: Status code: ``0`` on success, ``<0`` on failure
    :example: .. literalinclude:: code_samples/dbentry_getslice

.. f:subroutine:: ids_put(pulsectx, name, IDS, retstatus)

    Write the contents of an IDS to the Database Entry.

    The IDS is written entirely, with all time slices it may contain.

    The IDS object can have none or many empty fields, empty fields are
    ignored and remain empty in the data entry. Some fields are required to
    be filled before calling this method, see :ref:`Mandatory and
    recommended IDS attributes`.

    .. caution::
        The put method deletes any previously existing data within the
        target IDS occurrence in the Database Entry.

    :param integer pulsectx [in]: Data entry context created with
        :f:func:`imas_open`, :f:func:`imas_open_env` or
        :f:func:`imas_create_env`
    :param character(*) name [in]: name of the ids with optional occurrence
        number, e.g. ``"core_profiles"`` (for occurrence 0),
        ``"core_profiles/1"`` (for occurrence 1)
    :param IDS: IDS object to put
    :option integer retstatus [out]: Status code: ``0`` on success, ``<0`` on failure
    :example: .. literalinclude:: code_samples/dbentry_put

.. f:subroutine:: ids_put_slice(pulsectx, name, IDS, retstatus)

    Append a time slice of the provided IDS to the Database Entry.

    Time slices must be appended in strictly increasing time order, since
    the Access Layer is not reordering time arrays. Doing otherwise will
    result in non-monotonic time arrays, which will create confusion and
    make subsequent :f:func:`ids_get_slice` commands to fail.

    Although being put progressively time slice by time slice, the final IDS
    must be compliant with the data dictionary. A typical error when
    constructing IDS variables time slice by time slice is to change the
    size of the IDS fields during the time loop, which is not allowed but
    for the children of an array of structure which has time as its
    coordinate.

    The :f:func:`ids_put_slice` command is appending data, so does not modify
    previously existing data within the target IDS occurrence in the Data
    Entry.

    It is possible possible to append several time slices to a node of the
    IDS in one :f:func:`ids_put_slice` call, however the user must ensure that
    the size of the time dimension of the node remains consistent with the
    size of its timebase.

    :param integer pulsectx [in]: Data entry context created with
        :f:func:`imas_open`, :f:func:`imas_open_env` or
        :f:func:`imas_create_env`
    :param character(*) name [in]: name of the ids with optional occurrence
        number, e.g. ``"core_profiles"`` (for occurrence 0),
        ``"core_profiles/1"`` (for occurrence 1)
    :param IDS: IDS object to put
    :option integer retstatus [out]: Status code: ``0`` on success, ``<0`` on failure
    :example: .. literalinclude:: code_samples/dbentry_put_slice

.. f:subroutine:: ids_copy(struct_in, struct_out)

    Create a copy of an IDS or structure inside an IDS.

    .. note::

        This method can only copy IDSs and structures. See below example on how
        to copy an array of structures.

    :param struct_in: IDS or sub-structure to copy from.
    :param struct_out: IDS or sub-structure to copy to. Must be of the same
        ``type`` as ``struct_in``.
    :example:
        .. code-block:: fortran

            type(ids_core_profiles) cp1, cp2;

            ! Assume cp1 is a filled IDS
            ! Copy a structure of cp1 to cp2
            call ids_copy(cp1%ids_properties, cp2%ids_properties)

            ! To copy an array of structures, we need to allocate the aos first
            if associated(cp1%profiles_1d) then
                allocate(cp2%profiles_1d (size(cp1%profiles_1d)) )
                do i = 1, size(cp1%profiles_1d)
                    call ids_copy(cp1%profiles_1d(i), cp2%profiles_1d(i))
                enddo
            endif

            ! Example copying the full IDS
            call ids_copy(cp1, cp2)

.. f:subroutine:: ids_deallocate(struct_in)

    Deallocate an IDS.

    .. note::

        IDS variables are allocated in C (using compatible types) when obtained
        from :f:func:`ids_get` or :f:func:`ids_get_slice` calls of the
        Access-layer, and should be deallocated through a call to
        :f:func:`ids_deallocate`. IDS variables directly allocated in Fortran
        can also be deallocated through :f:func:`ids_deallocate`. But one should
        not mix Fortran allocations on an IDS variable returned by a GET or
        GET_SLICE from the Access-layer, this will cause the
        :f:func:`ids_deallocate` to fail. See :f:func:`ids_deallocate_struct`
        for deallocating a node or sub-structure of an IDS.

    :param struct_in: IDS to deallocate.

.. f:subroutine:: ids_deallocate_struct(struct_in, c_data)

    Deallocate a substructure in an IDS.

    :param struct_in: Sub-structure to deallocate.
    :param logical c_data: should be :code:`.true.` if the data of the IDS (or
        specifically of the targeted substructure) was obtained through the
        Access Layer, and :code:`.false.` if it was allocated directly in
        Fortran.

.. f:function:: ids_is_valid(in)

    Verify if a data node is not empty.

    :param in: Data node to verify, supported data types are INT_0D and FLT_0D.
    :returns logical ids_is_valid: :code:`.true.` if the node is not empty.

.. f:function:: ids_is_defined(ids_in)

    Verifies if given IDS is 'defined' by checking if its field `ids_properties%homogeneous_time` is set

    :param ids_in: IDS to be checked.
    :returns logical ids_is_defined: :code:`.true.` if `ids_properties%homogeneous_time` is set
    :example:
        .. code-block:: fortran

            type(ids_core_profiles) :: ids
            logical :: is_defined

            is_defined = ids_is_defined(ids) ! .FALSE. 

            ! Set time mode
            ids%ids_properties%homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS

            ! Fill the ids fields with other data

            is_defined = ids_is_defined(ids) ! .TRUE. 



.. f:subroutine:: ids_serialize(ids_in, buffer, protocol)

    Serialize the contents of this IDS into binary data.

    There are currently two different serialization protocols. The ASCII protocol
    serializes the data though the ASCII backend. This is a simpler human readable
    protocol, but it's also less efficient than the (newer) Flexbuffers protocol.
    The latter is the default and should be preferred.

    The ID of the used serializer protocol is kept in the header of the serialized
    buffer, such that specifying the protocol is not necessary when deserializing.

    :param ids_in: IDS to serialize.
    :param character(len=1) buffer [dimension(:), allocatable]: Binary
        representation of this IDS.
    :option integer protocol: Which serialization protocol to use. Available
        options are: 

        - :f:var:`ASCII_SERIALIZER_PROTOCOL`
        - :f:var:`FLEXBUFFERS_SERIALIZER_PROTOCOL`
        - :f:var:`DEFAULT_SERIALIZER_PROTOCOL`
    :example:
        .. code-block:: fortran

            type(ids_pf_active) ids, ids2;
            character(len=1), dimension(:), allocatable :: buffer
            // populate the IDS
            // ...
            ids_serialize(ids, buffer)

            // move the binary data around, for example to another process using
            // memory communication, then deserialize
            ids_deserialize(buffer, ids2)

.. f:subroutine:: ids_deserialize(buffer, ids_out)

    Deserialize the provided binary data into an IDS.

    :param character(len=1) buffer [dimension(:), allocatable]: data representing a serialized IDS.
    :param ids_in: IDS to store the deserialized data in.
    :example: See :f:func:`ids_serialize`.

.. f:subroutine:: ids_validate(ids_in, status, err_msg)

    Validate the coordinate consistency of the data of the ids.

    If an exception is raised (inconsistency of one coordinate) status is returned -1. 
    The char array 'err_msg' store the explanation as an error message. 
    If the data are consistent err_msg is left empty and status = 0.

    :param ids_in: IDS to validate.
    :param integer status: final status of the validation
    :param character(len=:) err_msg [dimension(:), allocatable]: Error message if a validation exception is raised.

    :example: .. literalinclude:: code_samples/ids_validate

.. f:subroutine:: ids_get_sample(pulsectx, name, IDS, tmin, tmax, dtime, interpolMode, status)
        
    Read a range of time slices from an IDS in this Database Entry.

    This method has three different modes, depending on the provided arguments:

    1.  No interpolation. This method is selected when ``dtime`` is an empty 
        vector (``size(dtime) == 0``) and ``interpolMode`` is 0.

        This mode returns an IDS object with all constant/static data filled. The
        dynamic data is retrieved for the provided time range [tmin, tmax].

    2.  Interpolate dynamic data on a uniform time base. This method is selected
        when ``dtime`` and ``interpolMode`` are provided.
        ``dtime`` must be of size 1 (``real(ids_real), dimension(1)``).

        This mode will generate an IDS with a homogeneous time vector ``[tmin, tmin
        + dtime, tmin + 2*dtime, ...`` up to ``tmax``. The chosen interpolation
        method will have no effect on the time vector, but may have an impact on the
        other dynamic values. The returned IDS always has ``ids_properties.homogeneous_time = 1``.

    3.  Interpolate dynamic data on an explicit time base. This method is selected
        when ``dtime`` and ``interpolMode`` are provided.
        ``dtime`` must be a ``real(ids_real), dimension(:)`` of size larger than 1.

        This mode will generate an IDS with a homogeneous time vector equal to
        ``dtime``. ``tmin`` and ``tmax`` are ignored in this mode.
        The chosen interpolation method will have no effect on the time vector, but
        may have an impact on the other dynamic values. The returned IDS always has ``ids_properties.homogeneous_time = 1``.

        :param integer pulsectx [in]: Data entry context created with
            :f:func:`imas_open`, :f:func:`imas_open_env` or :f:func:`imas_create_env`
        :param character(*) name [in]: name of the ids with optional occurrence
            number, e.g. ``"core_profiles"`` (for occurrence 0), ``"core_profiles/1"`` (for occurrence 1)
        :param IDS [out]: IDS object to put
        :param tmin [in]: Lower bound of the requested time range
        :param tmax [in]: Upper bound of the requested time range, must be larger than or equal to ``tmin``
        :param dtime [in]: Interval to use when interpolating, must be a real(ids_real), dimension(:)
            containing an explicit time base to interpolate.
        :param interpolMode [in]: Interpolation method to use. Available options:

            - ``CLOSEST_INTERP``
            - ``PREVIOUS_INTERP``
            - ``LINEAR_INTERP``

        :option integer status [out]: Status code: ``0`` on success, ``<0`` on failure
