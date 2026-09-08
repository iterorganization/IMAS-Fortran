# Run an HLI-side scenario with a private loss-log directory and prove the
# shim left no loss log file behind.  No file is the Tier-1 observable for no
# loss.
#
# SCENARIO names the scenario under test, so a failure here says which one
# broke.  Its callers are not all stamp scenarios: al-fortran-test-shim-
# version-unset runs through here too.
if( NOT DEFINED LOSS_LOG_DIR )
  message(FATAL_ERROR "SCENARIO-FAILURE: LOSS_LOG_DIR is required")
endif()

include("${CMAKE_CURRENT_LIST_DIR}/clean_loss_log_dir.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/run_scenario.cmake")

file(GLOB _logs "${LOSS_LOG_DIR}/imas-mvdd-loss-*.txt")
list(LENGTH _logs _log_count)
if( NOT _log_count EQUAL 0 )
  message(FATAL_ERROR "SCENARIO-FAILURE: ${SCENARIO} logged loss despite plain forwarding/refusal: ${_logs}")
endif()
