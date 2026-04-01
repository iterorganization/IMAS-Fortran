
# Sources
################################################################################

# Test settings
set( TEST_BACKENDS "MEMORY_BACKEND" "ASCII_BACKEND" )
if( AL_BACKEND_MDSPLUS )
  get_target_property( MDSPLUS_MODEL_DIR al-mdsplus-model BINARY_DIR )
  list( APPEND TEST_BACKENDS "MDSPLUS_BACKEND" )
endif()
if( AL_BACKEND_HDF5 )
  list( APPEND TEST_BACKENDS "HDF5_BACKEND" )
endif()
list( JOIN TEST_BACKENDS ", " TEST_ARRAY_ALL_BACKENDS )
list( LENGTH TEST_BACKENDS TEST_ARRAY_ALL_BACKENDS_LENGTH )

configure_file( test_defs.f90.in test_defs.f90 )

# Generate sources
set( GENERATED_SRC_DIR "${CMAKE_CURRENT_BINARY_DIR}/src" )
set( GENERATED_SOURCES )

foreach( IDS_NAME IN LISTS IDS_NAMES )
  if( NOT IDS_NAME STREQUAL "")
    list( APPEND GENERATED_SOURCES
      "${GENERATED_SRC_DIR}/${IDS_NAME}_all_tests.f90"
      "${GENERATED_SRC_DIR}/${IDS_NAME}_test_routines.f90"
      "${GENERATED_SRC_DIR}/${IDS_NAME}_test.f90"
      "${GENERATED_SRC_DIR}/${IDS_NAME}_init.f90"
    )
  endif()
endforeach()

# Note: saxonche is provided by dd_build_env (set up in ALBuildDataDictionary)
# _VENV_PYTHON points to that environment and is used for all XSLT transformations

add_custom_command(
  # Use a dummy output file: when listing all actual output files,
  # they would be re-created with every build.
  OUTPUT ${GENERATED_SRC_DIR}/dummy.txt
  COMMAND 
    ${_VENV_PYTHON} "${AL_LOCAL_XSLTPROC_SCRIPT}"
      -xsl ${CMAKE_CURRENT_SOURCE_DIR}/IDSDef2TestSuite.xsl
      -s ${IDSDEF}
      -o ${CMAKE_CURRENT_BINARY_DIR}/test_output.xml
      newLowLevel=yes
  # Create/update our dummy file
  COMMAND ${CMAKE_COMMAND} -E touch ${GENERATED_SRC_DIR}/dummy.txt
  DEPENDS IDSDef2TestSuite.xsl ${IDSDEF}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
add_custom_target( al-fortran-test-sources
  COMMAND ${CMAKE_COMMAND} -E echo ""
  DEPENDS ${GENERATED_SRC_DIR}/dummy.txt ${CMAKE_CURRENT_BINARY_DIR}/test_defs.f90
  # Actually these are results of the custom_command above, but with this trick
  # we don't rebuild it always (only when IDSDef.xml or the xslts have changed)
  BYPRODUCTS ${GENERATED_SOURCES}
)


# Targets and tests
################################################################################

add_custom_target( al-fortran-tests )

add_library( al-fortran-test-helpers OBJECT
  comparator.f90
  generator.f90
  helper.f90
  setter.f90
  ${CMAKE_CURRENT_BINARY_DIR}/test_defs.f90
)
target_link_libraries( al-fortran-test-helpers PUBLIC al-fortran )
# Ensure the mod dir is available when using nagfor
target_include_directories( al-fortran-test-helpers PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/include )

foreach( IDS_NAME IN LISTS IDS_NAMES )
  if( NOT IDS_NAME STREQUAL "")
    set( TEST_NAME al-fortran-test-${IDS_NAME} )

    add_executable( ${TEST_NAME}
      "${GENERATED_SRC_DIR}/${IDS_NAME}_all_tests.f90"
      "${GENERATED_SRC_DIR}/${IDS_NAME}_test_routines.f90"
      "${GENERATED_SRC_DIR}/${IDS_NAME}_test.f90"
      "${GENERATED_SRC_DIR}/${IDS_NAME}_init.f90"
    )
    target_link_libraries( ${TEST_NAME} al-fortran-test-helpers )
    add_dependencies( ${TEST_NAME} al-fortran-test-sources )
    add_dependencies( al-fortran-tests ${TEST_NAME} )

    add_test( NAME ${TEST_NAME} COMMAND ${TEST_NAME} )
    set_tests_properties( ${TEST_NAME} PROPERTIES
      FAIL_REGULAR_EXPRESSION "[Ee][Rr][Rr][Oo][Rr]"
      # Disable coordinate consistency validation and warnings on obsolete nodes
      ENVIRONMENT "IMAS_VERSION=${DD_VERSION};IMAS_AL_DISABLE_VALIDATE=1;IMAS_AL_DISABLE_OBSOLESCENT_WARNING=1;ids_path=${MDSPLUS_MODEL_DIR};MDSPLUS_MODELS_PATH=${MDSPLUS_MODEL_DIR}"
    )

  endif()
endforeach()
