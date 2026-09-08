# The write policy currently exposes only a count, not the refused paths.  Pin
# the latter through its deliberately stable diagnostic until it gains a
# read-side-like path accessor (follow-up requested by issue #74).
if( NOT DEFINED EXPECTED_REFUSED_PATH )
  message(FATAL_ERROR "SCENARIO-FAILURE: EXPECTED_REFUSED_PATH is required")
endif()

include("${CMAKE_CURRENT_LIST_DIR}/run_scenario.cmake")

# Do not merely accept any refused write: a traversal that dropped another
# field would have the same PARTIAL_PUT status and must still fail this pin.
#
# Only stdout is searched.  The refusal diagnostic is written there, and a
# match found on stderr would mean the path was named by an error rather than
# by the traversal that tolerated it.
set(_expected_line "REFUSED WRITE: '${EXPECTED_REFUSED_PATH}'")
string(FIND "${_stdout}" "${_expected_line}" _refusal_at)
if( _refusal_at EQUAL -1 )
  message(FATAL_ERROR
    "SCENARIO-FAILURE: the write did not name ${EXPECTED_REFUSED_PATH} as refused\nstdout:\n${_stdout}\nstderr:\n${_stderr}")
endif()
