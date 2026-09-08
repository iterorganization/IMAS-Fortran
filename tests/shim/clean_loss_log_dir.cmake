# Empty a test's private loss-log directory, creating it if it is not there.
#
# The shim writes imas-mvdd-loss-<UTC>-<pid>.txt into IMAS_MVDD_LOSS_LOG_DIR
# and never truncates: a second ctest run leaves the first run's files in
# place.  A test that asserts on the directory would then be reading a mixture
# of runs, and a test that does not assert on it still accumulates artifacts
# under the build tree.  Both are avoided by starting every run from empty.
#
# Two ways in, both before the run and neither at configure time (configure
# runs once, ctest runs many times): as a CTest fixture for a test that runs
# its program directly, and by include() from a runner that wraps one.
if( NOT DEFINED LOSS_LOG_DIR )
  message(FATAL_ERROR "LOSS_LOG_DIR is required")
endif()

file(MAKE_DIRECTORY "${LOSS_LOG_DIR}")
file(GLOB _stale_logs "${LOSS_LOG_DIR}/imas-mvdd-loss-*.txt")
if( _stale_logs )
  file(REMOVE ${_stale_logs})
endif()
