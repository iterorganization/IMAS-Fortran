# The write policy currently exposes only a count, not the refused paths.  Pin
# the latter through its deliberately stable diagnostic until it gains a
# read-side-like path accessor (follow-up requested by issue #74).
if( NOT DEFINED COMMAND_TO_RUN OR NOT DEFINED EXPECTED_REFUSED_PATH )
  message(FATAL_ERROR "SCENARIO-FAILURE: COMMAND_TO_RUN and EXPECTED_REFUSED_PATH are required")
endif()

execute_process(
  COMMAND ${COMMAND_TO_RUN}
  RESULT_VARIABLE _result
  OUTPUT_VARIABLE _stdout
  ERROR_VARIABLE _stderr
)
if( NOT _result EQUAL 0 )
  message(FATAL_ERROR "SCENARIO-FAILURE: torn-write scenario failed (${_result})\nstdout:\n${_stdout}\nstderr:\n${_stderr}")
endif()

# Do not merely accept any refused write: a traversal that dropped another
# field would have the same PARTIAL_PUT status and must still fail this pin.
set(_expected_line "REFUSED WRITE: '${EXPECTED_REFUSED_PATH}'")
string(FIND "${_stdout}" "${_expected_line}" _refusal_at)
if( _refusal_at EQUAL -1 )
  message(FATAL_ERROR
    "SCENARIO-FAILURE: the write did not name ${EXPECTED_REFUSED_PATH} as refused\nstdout:\n${_stdout}\nstderr:\n${_stderr}")
endif()
