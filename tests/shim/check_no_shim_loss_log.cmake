# Run an HLI-side scenario with a private loss-log directory and prove the
# shim left no loss log file behind.  No file is the Tier-1 observable for no
# loss.
#
# SCENARIO names the scenario under test, so a failure here says which one
# broke.  Its callers are not all stamp scenarios: al-fortran-test-shim-
# version-unset runs through here too.
if( NOT DEFINED COMMAND_TO_RUN OR NOT DEFINED LOSS_LOG_DIR OR NOT DEFINED SCENARIO )
  message(FATAL_ERROR "COMMAND_TO_RUN, LOSS_LOG_DIR and SCENARIO are required")
endif()

file(MAKE_DIRECTORY "${LOSS_LOG_DIR}")
file(GLOB _old_logs "${LOSS_LOG_DIR}/imas-mvdd-loss-*.txt")
if( _old_logs )
  file(REMOVE ${_old_logs})
endif()

execute_process(
  COMMAND ${COMMAND_TO_RUN}
  RESULT_VARIABLE _result
  OUTPUT_VARIABLE _stdout
  ERROR_VARIABLE _stderr
)
if( NOT _result EQUAL 0 )
  message(FATAL_ERROR "${SCENARIO} failed (${_result})\nstdout:\n${_stdout}\nstderr:\n${_stderr}")
endif()

file(GLOB _logs "${LOSS_LOG_DIR}/imas-mvdd-loss-*.txt")
list(LENGTH _logs _log_count)
if( NOT _log_count EQUAL 0 )
  message(FATAL_ERROR "${SCENARIO} logged loss despite plain forwarding/refusal: ${_logs}")
endif()
