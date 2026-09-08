# Run one scenario program and require it to exit zero.
#
# Shared by the runners that then go on to assert something about what the
# program left behind -- its output, or the loss-log directory.  Included
# rather than called, so it leaves _stdout, _stderr and _output in the
# includer's scope for that second assertion.
#
# SCENARIO names the test, so a failure says which one broke rather than
# describing the family the runner happens to serve.
if( NOT DEFINED COMMAND_TO_RUN OR NOT DEFINED SCENARIO )
  message(FATAL_ERROR "SCENARIO-FAILURE: COMMAND_TO_RUN and SCENARIO are required")
endif()

execute_process(
  COMMAND ${COMMAND_TO_RUN}
  RESULT_VARIABLE _result
  OUTPUT_VARIABLE _stdout
  ERROR_VARIABLE _stderr
)
set(_output "${_stdout}${_stderr}")

if( NOT _result EQUAL 0 )
  message(FATAL_ERROR
    "SCENARIO-FAILURE: ${SCENARIO} failed (${_result})\nstdout:\n${_stdout}\nstderr:\n${_stderr}")
endif()
