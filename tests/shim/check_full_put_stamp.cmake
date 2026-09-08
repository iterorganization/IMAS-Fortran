# The refusal path is reported on stdout, while the rest of the contract lives
# in the Fortran program.  Keep both pass conditions: CTest's
# PASS_REGULAR_EXPRESSION would otherwise accept the expected line even if a
# later Fortran assertion error-stopped.
foreach(_var COMMAND_TO_RUN EXPECTED_OUTPUT)
  if(NOT DEFINED ${_var} OR "${${_var}}" STREQUAL "")
    message(FATAL_ERROR "SCENARIO-FAILURE: ${_var} is required")
  endif()
endforeach()

execute_process(
  COMMAND ${COMMAND_TO_RUN}
  RESULT_VARIABLE command_result
  OUTPUT_VARIABLE command_stdout
  ERROR_VARIABLE command_stderr
)
set(command_output "${command_stdout}${command_stderr}")

if(NOT command_result EQUAL 0)
  message(FATAL_ERROR "SCENARIO-FAILURE: full-put stamp assertion program failed:\n${command_output}")
endif()

string(FIND "${command_output}" "${EXPECTED_OUTPUT}" expected_output_position)
if(expected_output_position EQUAL -1)
  message(FATAL_ERROR
    "SCENARIO-FAILURE: full put did not name the expected refused path '${EXPECTED_OUTPUT}':\n${command_output}"
  )
endif()
