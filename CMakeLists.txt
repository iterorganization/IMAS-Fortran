# CMake build configuration for the Access Layer Fortran HLI
cmake_minimum_required( VERSION 3.16 )
if(POLICY CMP0144)
  cmake_policy(SET CMP0144 NEW)
endif()

# Local paths for IMAS-Fortran
################################################################################
# Use full path since CMAKE_MODULE_PATH hasn't been configured yet
include(${CMAKE_CURRENT_SOURCE_DIR}/common/cmake/ALLocalPaths.cmake)

# Configuration options for common assets
################################################################################
option( AL_DOWNLOAD_DEPENDENCIES "Automatically download assets from the AL git repository" ON )
set( AL_CORE_GIT_REPOSITORY "https://github.com/iterorganization/IMAS-Core.git" CACHE STRING "Git repository of AL-core" )
set( AL_CORE_VERSION "main" CACHE STRING "Git commit/tag/branch of AL-core" )

include(FetchContent)

# Load dependency
################################################################################
if( ${AL_DOWNLOAD_DEPENDENCIES} )
  # Download common assets from the ITER git:
  FetchContent_Declare(
    al-core
    GIT_REPOSITORY "${AL_CORE_GIT_REPOSITORY}"
    GIT_TAG "${AL_CORE_VERSION}"
  )
  FetchContent_MakeAvailable( al-core )
elseif ( ${AL_DEVELOPMENT_LAYOUT} )
  FetchContent_Declare(
    al-core
    SOURCE_DIR      "${CMAKE_CURRENT_SOURCE_DIR}/../al-core"
  )
  FetchContent_MakeAvailable( al-core )
endif()

# Define project
################################################################################

# Set VERSION and FULL_VERSION from `git describe`:
set( GIT_ARCHIVE_DESCRIBE [[$Format:%(describe)$]] )
include( ALDetermineVersion )
project( al-fortran
  VERSION ${VERSION}
  DESCRIPTION "Fortran High Level Interface of the IMAS Access Layer"
  HOMEPAGE_URL "https://imas.iter.org/"
  LANGUAGES C CXX Fortran
)
if( NOT PROJECT_VERSION_TWEAK EQUAL 0 )
  message( "Building a development version of the Access Layer Fortran HLI" )
endif()

# Compiler settings
include( ALSetCompilerFlags )
# Common HLI options
include( ALCommonConfig )
# Data dictionary
include( ALBuildDataDictionary )
# AL core, plugins and documentation
include( ALCore )

# Stop processing when only building documentation
if( AL_DOCS_ONLY )
  return()
endif()


# Generate sources
################################################################################

set( GENERATED_SRC_DIR "${CMAKE_CURRENT_BINARY_DIR}/src" )
file( MAKE_DIRECTORY ${GENERATED_SRC_DIR} )

# Generated source files
set( GENERATED_SOURCES
  "${GENERATED_SRC_DIR}/ids_routines.f90"
  "${GENERATED_SRC_DIR}/ids_schemas.f90"
  "${GENERATED_SRC_DIR}/ids_types.f90"
  "${GENERATED_SRC_DIR}/ids_utilities.f90"
  "${GENERATED_SRC_DIR}/utilities_copy_struct.f90"
  "${GENERATED_SRC_DIR}/utilities_deallocate_struct.f90"
  "${GENERATED_SRC_DIR}/utilities_get_struct.f90"
  "${GENERATED_SRC_DIR}/utilities_put_slice_struct.f90"
  "${GENERATED_SRC_DIR}/utilities_put_struct.f90"
  "${GENERATED_SRC_DIR}/utilities_validate_struct.f90"
)
# Names of routines that generate separate source files
set( ROUTINES
  copy_struct
  deallocate_struct
  delete
  get
  get_slice
  put
  put_slice
  validate
)
foreach( IDS_NAME IN LISTS IDS_NAMES )
  foreach( ROUTINE IN LISTS ROUTINES )
      list( APPEND GENERATED_SOURCES "${GENERATED_SRC_DIR}/${IDS_NAME}_${ROUTINE}.f90" )
  endforeach()
  list( APPEND GENERATED_SOURCES "${GENERATED_SRC_DIR}/${IDS_NAME}_schema.f90" )
endforeach()

# Note: saxonche is provided by dd_build_env (set up in ALBuildDataDictionary)
# _VENV_PYTHON points to that environment and is used for all XSLT transformations

add_custom_command(
  # Use a dummy output file: when listing all actual output files (which are
  # source files of target `al-fortran`), they would be re-created with every build.
  OUTPUT ${GENERATED_SRC_DIR}/dummy.txt
  # Generate routine files with IDSDef2F90Routines.xsl
  COMMAND 
    ${_VENV_PYTHON} "${AL_LOCAL_XSLTPROC_SCRIPT}"
      -xsl ${CMAKE_CURRENT_SOURCE_DIR}/IDSDef2F90Routines.xsl
      -s ${IDSDEF}
      -o ${GENERATED_SRC_DIR}/routines_output.xml
      DD_GIT_DESCRIBE=${DD_VERSION}
      AL_GIT_DESCRIBE=${FULL_VERSION}
  # Generate schema files with IDSDef2F90TypeDef.xsl
  COMMAND 
    ${_VENV_PYTHON} "${AL_LOCAL_XSLTPROC_SCRIPT}"
      -xsl ${CMAKE_CURRENT_SOURCE_DIR}/IDSDef2F90TypeDef.xsl
      -s ${IDSDEF}
      -o ${GENERATED_SRC_DIR}/schemas_output.xml
      DD_GIT_DESCRIBE=${DD_VERSION}
      AL_GIT_DESCRIBE=${FULL_VERSION}
  # Create/update our dummy file
  COMMAND ${CMAKE_COMMAND} -E touch ${GENERATED_SRC_DIR}/dummy.txt
  DEPENDS IDSDef2F90Routines.xsl IDSDef2F90TypeDef.xsl ${IDSDEF}
  WORKING_DIRECTORY ${GENERATED_SRC_DIR}
)
add_custom_target( al-fortran-sources
  COMMAND ${CMAKE_COMMAND} -E echo ""
  DEPENDS ${GENERATED_SRC_DIR}/dummy.txt
  # Actually these are results of the custom_command above, but with this trick
  # we don't rebuild it always (only when IDSDef.xml or the xslts have changed)
  BYPRODUCTS ${GENERATED_SOURCES}
)

set( SOURCES ${GENERATED_SOURCES} )
list( APPEND SOURCES 
  wrapper/al_defs.f90
  wrapper/al_low_level_wrap.f90
)


# Generate sources (identifiers)
################################################################################

# Note: identifiers.xsl tries to import identifiers.common.xsl
# Unfortunately, xsl:include doesn't support parameters, so we need to put them together
# in the build folder:
file( CREATE_LINK
  "${CMAKE_CURRENT_SOURCE_DIR}/common/identifiers.common.xsl" 
  "${CMAKE_CURRENT_BINARY_DIR}/identifiers.common.xsl"
  SYMBOLIC COPY_ON_ERROR
)
file( CREATE_LINK
  "${CMAKE_CURRENT_SOURCE_DIR}/identifiers.xsl" 
  "${CMAKE_CURRENT_BINARY_DIR}/identifiers.xsl"
  SYMBOLIC COPY_ON_ERROR
)
set( IDENTIFIER_SRC_DIR ${CMAKE_CURRENT_BINARY_DIR}/identifiers/src/ )
file( MAKE_DIRECTORY ${IDENTIFIER_SRC_DIR} )

set( IDENTIFIER_SOURCES )
foreach( ID_FILE IN LISTS DD_IDENTIFIER_FILES )
  # cmake_path( GET ID_FILE STEM ID_NAME)
  string( REGEX REPLACE "(^.*[/\\]|[.]xml$)" "" ID_NAME ${ID_FILE} )
  set( SOURCE_NAME "${IDENTIFIER_SRC_DIR}/${ID_NAME}.f90" )
  add_custom_command(
    OUTPUT ${SOURCE_NAME}
    COMMAND ${_VENV_PYTHON} "${AL_LOCAL_XSLTPROC_SCRIPT}"
      -xsl ${CMAKE_CURRENT_BINARY_DIR}/identifiers.xsl
      -s ${ID_FILE}
      -o ${CMAKE_CURRENT_BINARY_DIR}/${ID_NAME}_identifier_output.xml
      name=${ID_NAME}
      prefix=${IDENTIFIER_SRC_DIR}
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/identifiers.xsl
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
  list( APPEND IDENTIFIER_SOURCES ${SOURCE_NAME} )
endforeach()


# Targets
################################################################################

add_library( al-fortran ${SOURCES} )
add_dependencies( al-fortran al-fortran-sources )

if( CMAKE_Fortran_COMPILER_ID STREQUAL "NAG" )
  # Work around NAG compiler not wanting to link against libal.so.x.y.z
  add_dependencies( al-fortran al )
  get_target_property( AL_BINARY_DIR al BINARY_DIR )
  target_link_options( al-fortran PRIVATE -lal -L${AL_BINARY_DIR} "-Wl,-Xlinker,-rpath,-Xlinker,${AL_BINARY_DIR}" )
else()
  target_link_libraries( al-fortran al )
endif()
# Set target output library name to include the DD version
set_target_properties( al-fortran PROPERTIES LIBRARY_OUTPUT_NAME "al-fortran-${DD_VERSION}" )

target_include_directories( al-fortran PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/wrapper>
  $<BUILD_INTERFACE:${GENERATED_SRC_DIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include/fortran>  # <prefix>/include/fortran
)

add_library( al-identifiers-fortran ${IDENTIFIER_SOURCES} )
add_dependencies( al-identifiers-fortran al-fortran )
target_include_directories( al-identifiers-fortran PUBLIC
  $<BUILD_INTERFACE:${IDENTIFIER_SRC_DIR}>
  $<BUILD_INTERFACE:${GENERATED_SRC_DIR}>
  $<INSTALL_INTERFACE:include/fortran>  # <prefix>/include/fortran
)


# Install
################################################################################

install( TARGETS al-fortran al-identifiers-fortran )
# Install module files
install( DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/ DESTINATION include/fortran )

# Generate and install .pc files
configure_file( pkgconfig/al-fortran.pc.in al-fortran.pc @ONLY )
configure_file( pkgconfig/al-fortran.pc.in imas-fortran.pc @ONLY )
configure_file( pkgconfig/al-fortran-DD_VERSION.pc.in al-fortran-${DD_VERSION}.pc @ONLY )
configure_file( pkgconfig/al-identifiers-fortran.pc.in al-identifiers-fortran.pc @ONLY )
install( FILES
    ${CMAKE_CURRENT_BINARY_DIR}/al-fortran.pc
    ${CMAKE_CURRENT_BINARY_DIR}/imas-fortran.pc
    ${CMAKE_CURRENT_BINARY_DIR}/al-fortran-${DD_VERSION}.pc
    ${CMAKE_CURRENT_BINARY_DIR}/al-identifiers-fortran.pc
  DESTINATION lib/pkgconfig
)


# Tests
################################################################################

if( AL_TESTS )
  add_subdirectory( tests/generator )
endif()

if( AL_EXAMPLES )
  add_subdirectory( examples )
endif()
