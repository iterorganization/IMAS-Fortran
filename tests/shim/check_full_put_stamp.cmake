# The refusal path is reported on stdout, while the rest of the contract lives
# in the Fortran program.  Keep both pass conditions: CTest's
# PASS_REGULAR_EXPRESSION would otherwise accept the expected line even if a
# later Fortran assertion error-stopped.
if( NOT DEFINED EXPECTED_OUTPUT OR "${EXPECTED_OUTPUT}" STREQUAL "" )
  message(FATAL_ERROR "SCENARIO-FAILURE: EXPECTED_OUTPUT is required")
endif()

include("${CMAKE_CURRENT_LIST_DIR}/run_scenario.cmake")

# Unlike the torn-write pin, both channels are searched: this assertion is
# about the refusal being named at all, not about which stream named it.
string(FIND "${_output}" "${EXPECTED_OUTPUT}" _expected_output_position)
if( _expected_output_position EQUAL -1 )
  message(FATAL_ERROR
    "SCENARIO-FAILURE: full put did not name the expected refused path '${EXPECTED_OUTPUT}':\n${_output}"
  )
endif()
