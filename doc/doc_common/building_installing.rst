Building and installing the IMAS-Fortran
========================================

This page describes how to build and install the IMAS-Fortran.

Documentation for developers wishing to contribute to the IMAS-Fortran can be found in
the :ref:`IMAS-Fortran development guide`. Please refer to that guide if you wish to set
up a development environment.

For more information about related components, see:

-   `IMAS Core Documentation <https://imas-core.readthedocs.io/en/latest/>`__
  
-   `IMAS Data Dictionary Documentation <https://imas-data-dictionary.readthedocs.io/en/latest/>`__


.. _`build prerequisites`:

Prerequisites
-------------

-   Git
-   CMake (3.16 or newer)
-   Python
-   PkgConfig


To build the IMAS-Core together with IMAS-Fortran refer to 
IMAS-Core's requirements `IMAS Core Installation <https://imas-core.readthedocs.io/en/latest/user_guide/installation.html#imas-core>`__


-   Fortran High Level Interface

    -   **Fortran High Level Interface**: A Fortran 2003 compliant compiler
        (gfortran 10.0 or later, ifort 2020 or later, ifx, NAGfor 6.2 or later, nvfortran)


Standard environments:

.. md-tab-set::

    .. md-tab-item:: SDCC ``intel-2023b``

        The following modules provide all the requirements when using the
        ``intel-2023b`` toolchain:

        .. code-block:: bash

            module load intel-compilers/2023.2.1 CMake/3.27.6-GCCcore-13.2.0 \
            Python/3.11.9-GCCcore-13.2.0

            # Additionally load the following modules if building IMAS-Core together with IMAS-Fortran
            module load Boost/1.83.0-iimpi-2023b HDF5/1.14.3-iimpi-2023b \
                MDSplus/7.132.0-GCCcore-13.2.0 \
                UDA/2.8.1-iimpi-2023b Blitz++/1.0.2-GCCcore-13.2.0 \
                SciPy-bundle/2023.11-intel-2023b \
                scikit-build-core/0.9.3-GCCcore-13.2.0

    .. md-tab-item:: SDCC ``foss-2023b``

        The following modules provide all the requirements when using the
        ``foss-2023b`` toolchain:

        .. code-block:: bash

            module load CMake/3.27.6-GCCcore-13.2.0 Python/3.11.9-GCCcore-13.2.0

            # Additionally load the following modules if building IMAS-Core together with IMAS-Fortran
            module load  Boost/1.83.0-GCC-13.2.0 HDF5/1.14.3-gompi-2023b \
                MDSplus/7.132.0-GCCcore-13.2.0 \
                UDA/2.8.1-GCC-13.2.0 Blitz++/1.0.2-GCCcore-13.2.0 \
                SciPy-bundle/2023.11-gfbf-2023b \
                build/1.0.3-foss-2023b scikit-build-core/0.9.3-GCCcore-13.2.0


    .. md-tab-item:: Ubuntu 22.04

        The following packages provide most requirements when using Ubuntu 22.04:

        .. code-block:: bash

            apt install git build-essential cmake pkg-config gfortran \
            
            # Additionally load the following modules if building IMAS-Core together with IMAS-Fortran
            apt install libboost-all-dev \
                libhdf5-dev libblitz0-dev  \
                default-jdk-headless python3-dev python3-venv python3-pip

        The following dependencies are not available from the package repository,
        you will need to install them yourself if building IMAS-Core together with IMAS-Fortran:

        -   MDSplus: see their `GitHub repository
            <https://github.com/MDSplus/mdsplus>`__ or `home page
            <https://mdsplus.org/>`__ for installation instructions.
        -   UDA: see their `GitHub repository <https://github.com/ukaea/UDA>`__ for more
            details.


Building and installing IMAS-Fortran
------------------------------------

This section explains how to install a Fortran High Level Interface. Please make sure you
have the :ref:`build prerequisites` installed.


Clone the repository
````````````````````

First you need to clone the repository of the High Level Interface you want to build:

.. code-block:: bash

    # For the Fortran HLI use:
    git clone git@github.com:iterorganization/IMAS-Fortran.git


.. _configuration-hli:

Configuration
`````````````

Once you have cloned the repository, navigate your shell to the folder and run cmake.
You can pass configuration options with ``-D OPTION=VALUE``. See below list for an
overview of configuration options.

.. code-block:: bash

    cd IMAS-Fortran
    cmake -B build -D CMAKE_INSTALL_PREFIX=$HOME/IMAS-Fortran -D OPTION1=VALUE1 -D OPTION2=VALUE2 [...]

.. note:: 

    CMake will automatically fetch dependencies from other IMAS-Fortran GIT repositories
    for you. You may need to provide credentials to clone the following repositories:

    -   `imas-core (git@github.com:iterorganization/IMAS-Core.git)
        <https://github.com/iterorganization/IMAS-Core>`__
    -   `imas-core-plugins (https://github.com/iterorganization/IMAS-Core-Plugins.git) 
        <https://github.com/iterorganization/IMAS-Core-Plugins>`__
    -   `imas-data-dictionary (git@github.com:iterorganization/IMAS-Data-Dictionary.git)
        <https://github.com/iterorganization/IMAS-Data-Dictionary>`__

    If you need to change the git repositories, for example to point to a mirror of the
    repository or to use a HTTPS URL instead of the default SSH URLs, you can update the
    :ref:`configuration options`. For example, add the following options to your
    ``cmake`` command to download the repositories over HTTPS instead of SSH:
    
    .. code-block:: text
        :caption: Use explicit options to download dependent repositories over HTTPS

        cmake -B build \
            -D AL_CORE_GIT_REPOSITORY=git@github.com:iterorganization/IMAS-Core.git \
            -D AL_PLUGINS_GIT_REPOSITORY=git@github.com:iterorganization/IMAS-Core-Plugins.git \
            -D DD_GIT_REPOSITORY=git@github.com:iterorganization/IMAS-Data-Dictionary.git

    If you use CMake 3.21 or newer, you can also use the ``https`` preset:

    .. code-block:: text
        :caption: Use CMake preset to set to download dependent repositories over HTTPS

        cmake -B build --preset=https


Choosing the compilers
''''''''''''''''''''''

You can instruct CMake to use compilers with the following environment variables:

-   ``CC``: C compiler, for example ``gcc`` or ``icc``.
-   ``CXX``: C++ compiler, for example ``g++`` or ``icpc``.

If you don't specify a compiler, CMake will take a default (usually from the Gnu
Compiler Collection).

.. important::

    These environment variables must be set before the first time you configure
    ``cmake``!

    If you have an existing ``build`` folder and want to use a different compiler, you
    should delete the ``build`` folder first, or use a differently named folder for the
    build tree.


Configuration options
'''''''''''''''''''''

For a complete list of available configuration options, please see the `IMAS Core Configuration Options <https://imas-core.readthedocs.io/en/latest/user_guide/installation.html#configuration-options>`__.


Build IMAS-Fortran
``````````````````````````````

Use ``make`` to build everything. You can speed things up by using parallel compiling
as shown with the ``-j`` option. Be careful with the amount of parallel processes
though: it's easy to exhaust your machine's available hardware (CPU or memory) which may
cause the build to fail. This is especially the case with the C++ High Level Interface.

.. code-block:: bash

    # Instruct make to build "all" in the "build" folder, using at most "8" parallel
    # processes:
    make -C build -j8 all

.. note::

    By default CMake on Linux will create ``Unix Makefiles`` for actually building
    everything, as assumed in this section.

    You can select different generators (such as Ninja) if you prefer, but these are not
    tested. See the `CMake documentation
    <https://cmake.org/cmake/help/latest/manual/cmake-generators.7.html>`__ for more
    details.


Optional: Test IMAS-Fortran
```````````````````````````````````````

If you set either of the options ``AL_EXAMPLES`` or ``AL_TESTS`` to ``ON``, you can run
the corresponding test programs as follows:

.. code-block:: bash

    # Use make:
    make -C build test
    # Directly invoke ctest
    ctest --test-dir build

This executes ``ctest`` to run all test and example programs. Note that this may take a
long time to complete.


Install IMAS-Fortran
````````````````````````````````

Run ``make install`` to install the high level interface in the folder that you chose in
the configuration step above.


Use IMAS-Fortran
````````````````````````````

After installing the HLI, you need to ensure that your code can find the installed
IMAS-Fortran. To help you with this, a file ``al_env.sh`` is installed. You can
``source`` this file to set all required environment variables:

.. code-block:: bash
    :caption: Set environment variables (replace ``<install_dir>`` with your install folder)

    source <install_dir>/bin/al_env.sh

You may want to add this to your ``$HOME/.bashrc`` file to automatically make the Access
Layer installation available for you.

.. note:: 

    To use a ``public`` dataset, you also need to set the ``IMAS_HOME`` environment
    variable. For example, on SDCC, this would be ``export IMAS_HOME=/work/imas``.

    Some programs may rely on an environment variable ``IMAS_VERSION`` to detect which
    version of the data dictionary is used in the current IMAS environment. You may set
    it manually with the DD version you've build the HLI with, for example: ``export
    IMAS_VERSION=3.41.0``.

Once you have set the required environment variables, you may continue :ref:`Using the
IMAS-Fortran`.


.. code-block:: bash

    # Compile your program with pkg-config
    gfortran your_program.f90 $(pkg-config --cflags --libs al-fortran)

Or manually specify the include and library paths:

.. code-block:: bash

    gfortran your_program.f90 \
        -I/path/to/install/include/fortran \
        -L/path/to/install/lib \
        -lal-fortran-<version>


Troubleshooting
```````````````

**Problem:** ``Target Boost::log already has an imported location``
    Add the CMake configuration option ``-D Boost_NO_BOOST_CMAKE=ON`` to work around
    the problem.

