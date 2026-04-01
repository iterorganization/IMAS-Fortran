Getting Started with IMAS-Fortran
=================================

Welcome! This 5-minute guide will get you up and running with the IMAS-Fortran Access Layer.

**What is IMAS-Fortran?**

IMAS-Fortran is the IMAS data access library (formerly known as the Access Layer) for Fortran users/developers. It provides a high-level interface to read, write, and manipulate IMAS data structures.


Load the IMAS-Fortran Module
-------------------------------

On the ITER SDCC (supercomputing cluster), make the Access Layer available:

.. code-block:: bash

    module load IMAS-Fortran

To see available versions:

.. code-block:: bash

    module avail IMAS-Fortran

If you have a local installation, source the environment file instead:

.. code-block:: bash

    source <install_dir>/bin/al_env.sh


Connect to Data
-------------------------------------------

Start by opening a database entry using an IMAS URI. A URI tells the Access Layer 
where your data is stored and in what format.

.. code-block:: fortran

    use ids_routines
    implicit none
    
    integer :: ctx, status
    character(len=256) :: uri
    character(:), allocatable :: errmsg
    
    ! Open a database entry (read mode)
    uri = 'imas:hdf5?path=/path/to/data'
    call imas_open(uri, OPEN_PULSE, ctx, status, errmsg)
    
    if (status /= 0) then
        write(*,*) 'Unable to open database: ', errmsg
        error stop
    end if

**What's an IMAS URI?**

URIs follow the format: ``imas:backend?query_options``

For example:
- ``imas:hdf5?path=/path/to/data`` – Read from HDF5 files
- ``imas:mdsplus?path=./test_db`` – Read from MDSplus
- ``imas:uda?backend=...`` – Read from UDA backend

Learn more: :ref:`Data entry URIs`


Load and Display Data
------------------------

Fetch an IDS from your database entry:

.. code-block:: fortran

    use ids_routines
    implicit none
    
    type(ids_magnetics) :: magnetics
    integer :: status
    
    ! Load the magnetics IDS (occurrence 0)
    call ids_get(ctx, "magnetics", magnetics, status)
    
    if (status /= 0) then
        error stop 'Failed to load magnetics'
    end if
    
    ! Explore the data
    print *, 'Time points:', magnetics%time
    print *, 'Number of flux loops:', size(magnetics%flux_loop)
    print *, 'Flux data:', magnetics%flux_loop(1)%flux%data


Modify and Store Data
------------------------

You can create new data, modify existing data, and store it back:

.. code-block:: fortran

    use ids_routines
    implicit none
    
    type(ids_equilibrium) :: equilibrium
    integer :: status
    
    ! Initialize a new IDS
    equilibrium%ids_properties%homogeneous_time = IDS_TIME_MODE_HOMOGENEOUS
    
    ! Modify data
    allocate(equilibrium%time(4))
    equilibrium%time = [0.0, 1.0, 2.0, 3.0]
    
    allocate(equilibrium%time_slice(4))
    allocate(equilibrium%time_slice(1)%profiles_1d%q(10))
    equilibrium%time_slice(1)%profiles_1d%q = [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5]
    
    ! Store it to the database
    call ids_put(ctx, "equilibrium", equilibrium, status)
    
    if (status /= 0) then
        error stop 'Failed to store equilibrium'
    end if


Clean Up
-----------

Always close the database entry when you're done:

.. code-block:: fortran

    integer :: status
    
    call imas_close(ctx, status)
    if (status /= 0) then
        print *, 'Warning: error closing database'
    end if


Key Subroutines Reference
--------------------------

.. list-table::
   :header-rows: 1
   :widths: auto

   * - Subroutine
     - Purpose
   * - ``imas_open(uri, mode, ctx, status, errmsg)``
     - Open a database entry at the given URI
   * - ``imas_close(ctx, status)``
     - Close the database entry
   * - ``ids_get(ctx, name, ids_obj, status)``
     - Load an entire IDS by name
   * - ``ids_put(ctx, name, ids_obj, status)``
     - Store an IDS to disk
   * - ``ids_get_slice(ctx, name, ids_obj, time, interp, status)``
     - Load a specific time slice with interpolation
   * - ``ids_put_slice(ctx, name, ids_obj, status)``
     - Append a time slice to existing IDS
   * - ``ids_deallocate(ids_obj)``
     - Deallocate memory for an IDS

**Open Mode Constants:**

- ``OPEN_PULSE`` - Open existing data entry (read mode)
- ``CREATE_PULSE`` - Create new entry only if it doesn't exist
- ``FORCE_CREATE_PULSE`` - Force create/overwrite existing entry

**Interpolation Mode Constants:**

- ``CLOSEST_INTERP`` - Use closest time point
- ``PREVIOUS_INTERP`` - Use previous time point
- ``LINEAR_INTERP`` - Linear interpolation between time points


Common Use Cases
----------------

**Load data and extract a single time slice:**

.. code-block:: fortran

    use ids_routines
    implicit none
    
    type(ids_equilibrium) :: equilibrium
    integer :: status
    
    ! Use CLOSEST interpolation at time = 2.5
    call ids_get_slice(ctx, "equilibrium", equilibrium, 2.5, CLOSEST_INTERP, status)


**Check if data is allocated:**

.. code-block:: fortran

    if (allocated(magnetics%flux_loop)) then
        if (size(magnetics%flux_loop) > 0) then
            if (allocated(magnetics%flux_loop(1)%flux%data)) then
                print *, 'Flux data is defined'
            end if
        end if
    end if


**Access Fortran examples:**

The repository contains several example programs in the ``examples/`` directory:
- ``test_magnetics_get.f90`` – Load and display magnetics data
- ``test_magnetics_put.f90`` – Store new data
- ``test_core_profiles_put.f90`` – Practical core profiles example


Complete Example Program
------------------------

.. code-block:: fortran

    program example_imas_usage
        use ids_routines
        implicit none
        
        integer :: ctx, status
        type(ids_magnetics) :: magnetics
        character(len=256) :: uri
        character(:), allocatable :: errmsg
        
        ! Open database
        uri = 'imas:hdf5?path=/data/example.h5'
        call imas_open(uri, OPEN_PULSE, ctx, status, errmsg)
        
        if (status /= 0) then
            write(*,*) 'Failed to open database: ', errmsg
            error stop
        end if
        
        ! Load magnetics data
        call ids_get(ctx, "magnetics", magnetics, status)
        
        if (status == 0) then
            print *, 'Successfully loaded magnetics'
            if (allocated(magnetics%time)) then
                print *, 'Time points:', size(magnetics%time)
            end if
        else
            print *, 'Error loading magnetics:', status
        end if
        
        ! Deallocate and close
        call ids_deallocate(magnetics)
        call imas_close(ctx, status)
        
    end program example_imas_usage
