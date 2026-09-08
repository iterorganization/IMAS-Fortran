# Run a cross-DD HLI read and inspect its Tier-1 loss-log file.  This stays at
# the HLI boundary: direct imas_mvdd_context_loss_* calls are a shim-repository
# concern (ADR 0002), while the file is the only loss channel this binding owns.
if( NOT DEFINED LOSS_LOG_DIR OR NOT DEFINED FIXTURE_ROOT )
  message(FATAL_ERROR "SCENARIO-FAILURE: LOSS_LOG_DIR and FIXTURE_ROOT are required")
endif()

# Issue #66 asks that the checked-in fixture is byte-identical after a
# cross-version read.  Every other assertion in this suite reads the fixtures
# and trusts them; if a converting read ever wrote back to the pulse it read,
# nothing here would notice, and every later comparison would be against a
# pulse the suite itself had modified.
#
# Content, not mtime: HDF5 rewrites can leave the size alone, and a checkout
# sets timestamps arbitrarily.
function( fixture_digest out_var )
  file(GLOB_RECURSE _files "${FIXTURE_ROOT}/*")
  list(SORT _files)
  set(_digest "")
  foreach(_file IN LISTS _files)
    if( NOT IS_DIRECTORY "${_file}" )
      file(MD5 "${_file}" _hash)
      string(APPEND _digest "${_file} ${_hash}\n")
    endif()
  endforeach()
  set(${out_var} "${_digest}" PARENT_SCOPE)
endfunction()

fixture_digest(_fixture_before)
if( _fixture_before STREQUAL "" )
  message(FATAL_ERROR "SCENARIO-FAILURE: no fixture files found under ${FIXTURE_ROOT}: the immutability check would pass vacuously")
endif()

include("${CMAKE_CURRENT_LIST_DIR}/clean_loss_log_dir.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/run_scenario.cmake")

fixture_digest(_fixture_after)
if( NOT _fixture_after STREQUAL _fixture_before )
  message(SEND_ERROR "SCENARIO-FAILURE: the cross-version read modified the checked-in fixture under ${FIXTURE_ROOT}")
endif()

file(GLOB _logs "${LOSS_LOG_DIR}/imas-mvdd-loss-*.txt")
list(LENGTH _logs _log_count)
if( NOT _log_count EQUAL 1 )
  message(FATAL_ERROR "SCENARIO-FAILURE: expected one isolated loss log file, found ${_log_count} in ${LOSS_LOG_DIR}")
endif()
list(GET _logs 0 _log)
file(READ "${_log}" _contents)
string(ASCII 9 _tab)
string(REPLACE "\n" ";" _lines "${_contents}")

# These two lines are a versioned interface, not incidental diagnostics.  In
# particular, reading data below without first pinning this marker could make a
# format change look like a test with no losses.
list(GET _lines 0 _format_marker)
if( NOT _format_marker STREQUAL "# imas-mvdd loss log format 1" )
  message(FATAL_ERROR "SCENARIO-FAILURE: unexpected loss-log format marker: ${_format_marker}")
endif()

# The header is tab-separated data, not a comment.  Skip it by its fixed
# position after the four-line preamble; filtering comment-prefixed lines would
# incorrectly feed the header to the loss-entry parser.
list(GET _lines 4 _header)
if( NOT _header STREQUAL "uri${_tab}ids${_tab}stored-dd${_tab}hli-dd${_tab}operation${_tab}fidelity${_tab}path" )
  message(FATAL_ERROR "SCENARIO-FAILURE: unexpected loss-log column header: ${_header}")
endif()
list(SUBLIST _lines 5 -1 _records)

set(_expected_records
  "read${_tab}LOSSY${_tab}time_slice/boundary/rho_tor"
  "read${_tab}LOSSY${_tab}time_slice/boundary/phi"
  "read${_tab}LOSSY${_tab}time_slice/boundary/phi_poloidal_current"
  "read${_tab}LOSSY${_tab}time_slice/contour_tree/edges"
  "read${_tab}LOSSY${_tab}time_slice/constraints/chi_squared_reduced"
  "read${_tab}LOSSY${_tab}time_slice/constraints/freedom_degrees_n"
  "read${_tab}LOSSY${_tab}time_slice/constraints/constraints_n"
  "read${_tab}LOSSY${_tab}time_slice/global_quantities/rho_tor_boundary"
  "read${_tab}LOSSY${_tab}time_slice/global_quantities/q_min/psi_norm"
  "read${_tab}LOSSY${_tab}time_slice/global_quantities/q_min/psi"
  "read${_tab}LOSSY${_tab}time_slice/profiles_1d/psi_norm"
  "read${_tab}LOSSY${_tab}time_slice/convergence/result/name"
  "read${_tab}LOSSY${_tab}time_slice/convergence/result/index"
  "read${_tab}LOSSY${_tab}time_slice/convergence/result/description"
)
set(_actual_records)
foreach(_record IN LISTS _records)
  if( _record STREQUAL "" )
    continue()
  endif()
  string(REPLACE "${_tab}" ";" _columns "${_record}")
  list(LENGTH _columns _column_count)
  if( NOT _column_count EQUAL 7 )
    message(FATAL_ERROR "SCENARIO-FAILURE: loss-log entry has ${_column_count} columns: ${_record}")
  endif()
  list(GET _columns 4 _operation)
  list(GET _columns 5 _fidelity)
  list(GET _columns 6 _path)
  list(APPEND _actual_records "${_operation}${_tab}${_fidelity}${_tab}${_path}")
endforeach()
# The four unit-redefined chi_squared_{r,z} paths are deliberately absent from
# the expected set above, even though the shim emits them as UNMAPPABLE today.
#
# Listing them would make the defect the expected result: this test would pass
# for exactly as long as the shim refuses those paths and go red the day it
# starts serving them -- red on the fix, green on the bug.  It would also
# contradict test_shim_refusal_rules, which asserts the same four paths agree
# (issue #70) and stays red until they do; between them the suite would then
# hold two opposite expectations of one defect, with no state of the shim that
# satisfies both.
#
# Naming them here instead keeps the reason for the failure legible, and both
# tests now fail on the defect and both pass once it is fixed.  See #72.
set(_known_defect_records
  "read${_tab}UNMAPPABLE${_tab}time_slice/constraints/x_point/chi_squared_r"
  "read${_tab}UNMAPPABLE${_tab}time_slice/constraints/x_point/chi_squared_z"
  "read${_tab}UNMAPPABLE${_tab}time_slice/constraints/strike_point/chi_squared_r"
  "read${_tab}UNMAPPABLE${_tab}time_slice/constraints/strike_point/chi_squared_z"
)
set(_defect_hits)
foreach(_record IN LISTS _known_defect_records)
  if( "${_record}" IN_LIST _actual_records )
    list(APPEND _defect_hits "${_record}")
    list(REMOVE_ITEM _actual_records "${_record}")
  endif()
endforeach()
if( _defect_hits )
  list(JOIN _defect_hits "\n" _defect_display)
  message(SEND_ERROR
    "SCENARIO-FAILURE: the shim still refuses the unit-redefined chi_squared paths, so it records "
    "them as unmappable rather than serving them (issue #72):\n${_defect_display}")
endif()

list(SORT _expected_records)
list(SORT _actual_records)
if( NOT _actual_records STREQUAL _expected_records )
  list(JOIN _expected_records "\n" _expected_display)
  list(JOIN _actual_records "\n" _actual_display)
  message(FATAL_ERROR "SCENARIO-FAILURE: unexpected operation-fidelity-path set\nexpected:\n${_expected_display}\nactual:\n${_actual_display}")
endif()
