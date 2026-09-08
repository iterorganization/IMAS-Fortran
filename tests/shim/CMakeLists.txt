# Linkage test for the multiversion shim
#
# Registered only in a shim build: with AL_USE_MULTIVERSION_SHIM off there is
# nothing to assert and the test list stays exactly what it was.

if( WIN32 )
  # The shim resolves AL core through dlopen/dlsym, so a shim build is POSIX-only.
  message( STATUS "Not registering al-fortran-test-shim-linkage: not supported on Windows" )
  return()
endif()

# otool and objdump report a shared library's dependencies in different formats,
# but both spell the dependency's file name, which is all this test looks for.
if( APPLE )
  find_program( AL_SHIM_INSPECT_TOOL otool )
  set( AL_SHIM_INSPECT_ARGS -L )
elseif( CMAKE_OBJDUMP )
  # What CMake itself uses for dependency inspection.
  set( AL_SHIM_INSPECT_TOOL ${CMAKE_OBJDUMP} )
  set( AL_SHIM_INSPECT_ARGS -p )
else()
  find_program( AL_SHIM_INSPECT_TOOL objdump )
  set( AL_SHIM_INSPECT_ARGS -p )
endif()

if( NOT AL_SHIM_INSPECT_TOOL )
  message( STATUS "Not registering al-fortran-test-shim-linkage: no dependency inspection tool found" )
  return()
endif()

add_test( NAME al-fortran-test-shim-linkage
  COMMAND ${CMAKE_COMMAND}
    -D "LIBRARY=$<TARGET_FILE:al-fortran>"
    -D "INSPECT_TOOL=${AL_SHIM_INSPECT_TOOL}"
    -D "INSPECT_ARGS=${AL_SHIM_INSPECT_ARGS}"
    -D "SHIM_LIBRARY_NAME=${AL_SHIM_LIBRARY_NAME}"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/check_shim_linkage.cmake"
)

# Each executable below compiles the suite's shared modules for itself, and
# ALSetCompilerFlags gives this whole directory a single relative
# CMAKE_Fortran_MODULE_DIRECTORY.  Left alone the targets would write the same
# .mod files to the same path and race under a parallel build -- observed as
# `Error copying Fortran module "tests/shim/include/shim_comparison.mod"` with
# three targets in play.  Every Fortran target here therefore gets a module
# directory of its own; add one for any target added later.
function( al_shim_private_module_dir target )
  set_target_properties( ${target} PROPERTIES
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include/${target}"
  )
endfunction()

# The comparison oracle is tested before any pulse-based contract assertion uses
# it.  It is deliberately self-contained in this registered suite rather than
# sharing the playground's diagnostic-only table code.
add_executable( al-fortran-test-shim-comparison
  shim_comparison.f90
  test_shim_comparison.f90
)
target_link_libraries( al-fortran-test-shim-comparison PRIVATE al-fortran )
al_shim_private_module_dir( al-fortran-test-shim-comparison )
add_test( NAME al-fortran-test-shim-comparison COMMAND al-fortran-test-shim-comparison )
# Refusals and other error-shaped text are legitimate output of later shim
# tests.  A distinctive marker plus the program's non-zero exit is the pass
# condition, and the count assertion in the program prevents a no-op test from
# passing.
# This program makes no AL call today, so it would run without the shim's
# run-time environment.  It is given it anyway: it links al-fortran, and so the
# shim, and the day an assertion here touches the C ABI the missing
# IMAS_CORE_LIBRARY would surface as an unrelated-looking dlopen failure.
set_tests_properties( al-fortran-test-shim-comparison PROPERTIES
  ENVIRONMENT "${AL_CORE_TEST_ENVIRONMENT}"
  FAIL_REGULAR_EXPRESSION "COMPARISON-FAILURE"
  LABELS "shim;contract-assertion"
)

# Fixture provenance (issue #64): the checked-in pulses must be what their
# generator produces, or every assertion in this suite is against numbers
# nobody can re-derive.  verify_fixtures.sh regenerates them outside the
# checkout and compares every dataset with h5diff; registering it here is what
# turns that from a script somebody may remember to run into a criterion the
# suite enforces.
#
# It needs the fixtures' own virtualenv and h5diff, neither of which this
# project builds, so it registers only when both are present -- the same
# pattern as the linkage test above.  A missing venv means the check is
# skipped, never that it silently passes.
set(AL_SHIM_FIXTURE_VENV_PYTHON "${CMAKE_SOURCE_DIR}/imas-python-fixtures/.venv/bin/python")
find_program(AL_SHIM_H5DIFF h5diff)
if( EXISTS "${AL_SHIM_FIXTURE_VENV_PYTHON}" AND AL_SHIM_H5DIFF )
  add_test(NAME al-fortran-test-shim-fixture-provenance
    COMMAND "${CMAKE_SOURCE_DIR}/imas-python-fixtures/verify_fixtures.sh"
  )
  set_tests_properties(al-fortran-test-shim-fixture-provenance PROPERTIES
    LABELS "shim;contract-assertion"
  )
else()
  message( STATUS
    "Not registering al-fortran-test-shim-fixture-provenance: needs "
    "imas-python-fixtures/.venv (see its README) and h5diff" )
endif()

# The HLI ends ids_get's root action itself, so it observes nested fidelity
# through the shim's process-local loss-log file, not the shim C ABI.  Give the
# test an isolated directory so the dynamic log filename cannot pick up a prior
# run's entries.
set(AL_SHIM_FIXTURE_ROOT "${CMAKE_SOURCE_DIR}/imas-python-fixtures/fixtures")
set(AL_SHIM_NESTED_LOSS_LOG_DIR "${CMAKE_CURRENT_BINARY_DIR}/nested-loss-log")
add_executable(al-fortran-test-shim-nested-loss test_shim_nested_loss.f90)
target_link_libraries(al-fortran-test-shim-nested-loss PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-nested-loss)
add_test(NAME al-fortran-test-shim-nested-loss
  COMMAND ${CMAKE_COMMAND}
    -D "COMMAND_TO_RUN=$<TARGET_FILE:al-fortran-test-shim-nested-loss>;${AL_SHIM_FIXTURE_ROOT}"
    -D "LOSS_LOG_DIR=${AL_SHIM_NESTED_LOSS_LOG_DIR}"
    -D "FIXTURE_ROOT=${AL_SHIM_FIXTURE_ROOT}"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/check_nested_loss_log.cmake"
)
set_tests_properties(al-fortran-test-shim-nested-loss PROPERTIES
  ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;IMAS_MVDD_LOSS_LOG_DIR=${AL_SHIM_NESTED_LOSS_LOG_DIR};${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;contract-assertion"
)

# Structural rules (identical, renamed, moved, merged, split): each asserted
# by comparing a shim-converted read of the DD 3.39.0 fixture against a
# direct, same-version read of the DD 4.1.1 fixture, so no expected value is
# transcribed into this file by hand. See shim_rule_table.f90 for the table
# and its citations.
add_executable(al-fortran-test-shim-structural-rules
  shim_comparison.f90
  shim_rule_table.f90
  shim_rule_check.f90
  test_shim_structural_rules.f90
)
target_link_libraries(al-fortran-test-shim-structural-rules PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-structural-rules)
add_test(NAME al-fortran-test-shim-structural-rules
  COMMAND al-fortran-test-shim-structural-rules "${AL_SHIM_FIXTURE_ROOT}"
)
set_tests_properties(al-fortran-test-shim-structural-rules PROPERTIES
  ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;contract-assertion"
  FAIL_REGULAR_EXPRESSION "STRUCTURAL-FAILURE"
)

# COCOS 11 -> 17 rules: every path the map's <cocos> block flips, each
# asserted the same way as the structural rules above. A sign flip that
# stops being applied reports 'NOFLIP', which shim_comparison gives mismatch
# severity, not warning severity, and this test's FAIL_REGULAR_EXPRESSION
# catches it exactly as it would any other rule failure. See
# shim_rule_table.f90 for the table, its citations, and why it carries 30
# entries rather than the 32 commonly quoted elsewhere.
add_executable(al-fortran-test-shim-cocos-rules
  shim_comparison.f90
  shim_rule_table.f90
  shim_rule_check.f90
  test_shim_cocos_rules.f90
)
target_link_libraries(al-fortran-test-shim-cocos-rules PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-cocos-rules)
add_test(NAME al-fortran-test-shim-cocos-rules
  COMMAND al-fortran-test-shim-cocos-rules "${AL_SHIM_FIXTURE_ROOT}"
)
set_tests_properties(al-fortran-test-shim-cocos-rules PROPERTIES
  ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;contract-assertion"
  FAIL_REGULAR_EXPRESSION "COCOS-FAILURE"
)

# The right_only rules: paths DD 4 introduced with no DD 3 source, where the
# expected verdict is a value on the DD 4 side only rather than agreement.
# Same two reads as the structural rules, compared the other way round -- the
# DD 4 oracle first, the shim-served side second -- which is what makes a
# served-nothing reading report `only4`; test_shim_right_only_rules.f90 says
# why at length.
add_executable(al-fortran-test-shim-right-only-rules
  shim_comparison.f90
  shim_rule_table.f90
  shim_rule_check.f90
  test_shim_right_only_rules.f90
)
target_link_libraries(al-fortran-test-shim-right-only-rules PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-right-only-rules)
add_test(NAME al-fortran-test-shim-right-only-rules
  COMMAND al-fortran-test-shim-right-only-rules "${AL_SHIM_FIXTURE_ROOT}"
)
# This read refuses paths, so it is a run with a non-exact outcome, and
# docs/SHIM_INTEGRATION_CONTRACT.md section 7 says such a run writes a loss
# file into IMAS_MVDD_LOSS_LOG_DIR -- or into the working directory when that
# is unset, which would drop it in the build tree and leave it for whichever
# test looks there next. This test asserts nothing about the loss log; it
# points it somewhere private purely so it cannot be mistaken for another
# test's artifact. The directory has to exist before the run, hence
# file(MAKE_DIRECTORY) at configure time.
set(AL_SHIM_RIGHT_ONLY_LOSS_LOG_DIR "${CMAKE_CURRENT_BINARY_DIR}/right-only-loss-log")
file(MAKE_DIRECTORY "${AL_SHIM_RIGHT_ONLY_LOSS_LOG_DIR}")
set_tests_properties(al-fortran-test-shim-right-only-rules PROPERTIES
  ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;IMAS_MVDD_LOSS_LOG_DIR=${AL_SHIM_RIGHT_ONLY_LOSS_LOG_DIR};${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;contract-assertion"
  FAIL_REGULAR_EXPRESSION "RIGHT-ONLY-FAILURE"
)

# The rules the map refuses: the one `retyped` rule and the four unit
# `redefine` globs.  Same two reads as the structural rules, asserted the
# other way round -- the two sides must not agree -- and paired with the
# read-side skip log, because for a redefinition both fixtures hold the same
# number and only the skip log tells a refusal from a passthrough.
#
# This test asserts the Fortran-side skip log, which is a different channel
# from the shim's loss log file, but it still needs its own loss-log
# directory: leaving IMAS_MVDD_LOSS_LOG_DIR unset is not neutral, because the
# shim then writes the loss log into the current working directory.  Giving
# it a directory of its own -- rather than the empty value the shim reads as
# "disable" -- keeps the suite parallel-safe and leaves the channel there for
# a later test to assert.  It is not cleaned before the run because nothing
# here reads it; the test that does own that (al-fortran-test-shim-nested-loss)
# cleans its own through a CMake wrapper.
set(AL_SHIM_REFUSAL_LOSS_LOG_DIR "${CMAKE_CURRENT_BINARY_DIR}/refusal-loss-log")
file(MAKE_DIRECTORY "${AL_SHIM_REFUSAL_LOSS_LOG_DIR}")
add_executable(al-fortran-test-shim-refusal-rules
  shim_comparison.f90
  shim_rule_table.f90
  shim_rule_check.f90
  test_shim_refusal_rules.f90
)
target_link_libraries(al-fortran-test-shim-refusal-rules PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-refusal-rules)
add_test(NAME al-fortran-test-shim-refusal-rules
  COMMAND al-fortran-test-shim-refusal-rules "${AL_SHIM_FIXTURE_ROOT}"
)
# The program prints refusal- and error-shaped text as expected output, so the
# pass condition is its own marker plus a zero exit, never the absence of the
# word "error".
set_tests_properties(al-fortran-test-shim-refusal-rules PROPERTIES
  ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;IMAS_MVDD_LOSS_LOG_DIR=${AL_SHIM_REFUSAL_LOSS_LOG_DIR};${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;contract-assertion"
  FAIL_REGULAR_EXPRESSION "REFUSAL-FAILURE"
)
# The write tests must mutate only private fixture copies.  Each setup test is
# a CTest fixture, so a selected scenario is prepared too and parallel runs do
# not share a pulse directory.
add_executable(al-fortran-test-shim-roundtrip test_shim_roundtrip.f90)
target_link_libraries(al-fortran-test-shim-roundtrip PRIVATE al-fortran)

function(add_shim_roundtrip_test name fixture read_control)
  set(copy_test "al-fortran-test-shim-${name}-fixture")
  set(test_name "al-fortran-test-shim-${name}")
  set(fixture_copy "${CMAKE_CURRENT_BINARY_DIR}/${name}-fixture")

  add_test(NAME ${copy_test}
    COMMAND ${CMAKE_COMMAND}
      -D "SOURCE=${AL_SHIM_FIXTURE_ROOT}/${fixture}"
      -D "DESTINATION=${fixture_copy}"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/copy_fixture.cmake"
  )
  set_tests_properties(${copy_test} PROPERTIES FIXTURES_SETUP ${test_name})

  add_test(NAME ${test_name}
    COMMAND al-fortran-test-shim-roundtrip ${fixture_copy} ${read_control}
  )
  set_tests_properties(${test_name} PROPERTIES
    FIXTURES_REQUIRED ${test_name}
    ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;${AL_CORE_TEST_ENVIRONMENT}"
    LABELS "shim;contract-assertion"
  )
endfunction()

# This pair is one argument: only the DD 3 pulse exercises the map, and the
# DD 4 control rules out a vacuous roundtrip that succeeds without conversion.
add_shim_roundtrip_test(roundtrip-cross-dd dd-3.39.0 partial-read-allowed)
add_shim_roundtrip_test(roundtrip-same-dd dd-4.1.1 clean-read)

# Behaviour pin, not a contract assertion (issue #74): a DD 4-only leaf is
# refused while the rest of this slice continues.  The resulting torn slice is
# an accepted limitation, so its label must keep it distinct from assertions of
# the shim contract above.
set(AL_SHIM_TORN_WRITE_FIXTURE "${CMAKE_CURRENT_BINARY_DIR}/torn-write-fixture")
add_executable(al-fortran-test-shim-torn-write test_shim_torn_write.f90)
target_link_libraries(al-fortran-test-shim-torn-write PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-torn-write)
add_test(NAME al-fortran-test-shim-torn-write-fixture
  COMMAND ${CMAKE_COMMAND}
    -D "SOURCE=${AL_SHIM_FIXTURE_ROOT}/dd-3.39.0"
    -D "DESTINATION=${AL_SHIM_TORN_WRITE_FIXTURE}"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/copy_fixture.cmake"
)
set_tests_properties(al-fortran-test-shim-torn-write-fixture PROPERTIES
  FIXTURES_SETUP al-fortran-test-shim-torn-write
)
add_test(NAME al-fortran-test-shim-torn-write
  COMMAND ${CMAKE_COMMAND}
    -D "COMMAND_TO_RUN=$<TARGET_FILE:al-fortran-test-shim-torn-write>;${AL_SHIM_TORN_WRITE_FIXTURE}"
    -D "EXPECTED_REFUSED_PATH=global_quantities/rho_tor_boundary"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/check_torn_write.cmake"
)
set_tests_properties(al-fortran-test-shim-torn-write PROPERTIES
  FIXTURES_REQUIRED al-fortran-test-shim-torn-write
  ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;behaviour-pin"
)

# Unlike ids_put_slice, a full put visits the DD-version stamp.  Run this
# contract assertion against a private DD 3 copy: it proves the shim names and
# tolerates that refusal, finishes the generated traversal, and leaves the
# original DD 3 stamp in place.
add_executable(al-fortran-test-shim-full-put-stamp test_shim_full_put_stamp.f90)
target_link_libraries(al-fortran-test-shim-full-put-stamp PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-full-put-stamp)
add_test(NAME al-fortran-test-shim-full-put-stamp-fixture
  COMMAND ${CMAKE_COMMAND}
    -D "SOURCE=${AL_SHIM_FIXTURE_ROOT}/dd-3.39.0"
    -D "DESTINATION=${CMAKE_CURRENT_BINARY_DIR}/full-put-stamp-fixture"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/copy_fixture.cmake"
)
set_tests_properties(al-fortran-test-shim-full-put-stamp-fixture PROPERTIES
  FIXTURES_SETUP al-fortran-test-shim-full-put-stamp
)
add_test(NAME al-fortran-test-shim-full-put-stamp
  COMMAND ${CMAKE_COMMAND}
    -D "COMMAND_TO_RUN=$<TARGET_FILE:al-fortran-test-shim-full-put-stamp>;${CMAKE_CURRENT_BINARY_DIR}/full-put-stamp-fixture"
    -D "EXPECTED_OUTPUT=REFUSED WRITE: 'ids_properties/version_put/data_dictionary'"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/check_full_put_stamp.cmake"
)
set_tests_properties(al-fortran-test-shim-full-put-stamp PROPERTIES
  FIXTURES_REQUIRED al-fortran-test-shim-full-put-stamp
  ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;contract-assertion"
)

# A stamp is read while opening an occurrence, so these three states have to
# be present in the pulse before the program begins.  The checked-in DD 3
# pulse is copied and its one stamp dataset is removed, malformed, or set to
# a version the shipped artifact does not cover, at build time; no
# scenario-specific HDF5 artifact is kept in the repository.
set(AL_SHIM_STAMP_VARIANT_PYTHON "${CMAKE_SOURCE_DIR}/imas-python-fixtures/.venv/bin/python")
set(AL_SHIM_STAMP_VARIANT_TOOLING_AVAILABLE FALSE)
if( EXISTS "${AL_SHIM_STAMP_VARIANT_PYTHON}" )
  execute_process(
    COMMAND "${AL_SHIM_STAMP_VARIANT_PYTHON}" -c "import h5py"
    RESULT_VARIABLE AL_SHIM_STAMP_VARIANT_H5PY_RESULT
    OUTPUT_QUIET
    ERROR_QUIET
  )
  if( AL_SHIM_STAMP_VARIANT_H5PY_RESULT EQUAL 0 )
    set(AL_SHIM_STAMP_VARIANT_TOOLING_AVAILABLE TRUE)
  endif()
endif()

if( NOT AL_SHIM_STAMP_VARIANT_TOOLING_AVAILABLE )
  message( STATUS
    "Not registering shim stamp-variant tests: needs imas-python-fixtures/.venv with h5py" )
else()
  set(AL_SHIM_STAMP_VARIANT_SOURCE "${AL_SHIM_FIXTURE_ROOT}/dd-3.39.0")
  set(AL_SHIM_STAMP_ABSENT_FIXTURE "${CMAKE_CURRENT_BINARY_DIR}/stamp-absent-fixture")
  set(AL_SHIM_STAMP_MALFORMED_FIXTURE "${CMAKE_CURRENT_BINARY_DIR}/stamp-malformed-fixture")
  set(AL_SHIM_STAMP_MISMATCH_FIXTURE "${CMAKE_CURRENT_BINARY_DIR}/stamp-mismatch-fixture")
  set(AL_SHIM_STAMP_VARIANT_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/derive_stamp_variant.py")

  add_custom_command(
    OUTPUT
      "${AL_SHIM_STAMP_ABSENT_FIXTURE}/equilibrium.h5"
      "${AL_SHIM_STAMP_MALFORMED_FIXTURE}/equilibrium.h5"
      "${AL_SHIM_STAMP_MISMATCH_FIXTURE}/equilibrium.h5"
    COMMAND "${AL_SHIM_STAMP_VARIANT_PYTHON}" "${AL_SHIM_STAMP_VARIANT_SCRIPT}"
      "${AL_SHIM_STAMP_VARIANT_SOURCE}" "${AL_SHIM_STAMP_ABSENT_FIXTURE}" absent
    COMMAND "${AL_SHIM_STAMP_VARIANT_PYTHON}" "${AL_SHIM_STAMP_VARIANT_SCRIPT}"
      "${AL_SHIM_STAMP_VARIANT_SOURCE}" "${AL_SHIM_STAMP_MALFORMED_FIXTURE}" malformed
    COMMAND "${AL_SHIM_STAMP_VARIANT_PYTHON}" "${AL_SHIM_STAMP_VARIANT_SCRIPT}"
      "${AL_SHIM_STAMP_VARIANT_SOURCE}" "${AL_SHIM_STAMP_MISMATCH_FIXTURE}" mismatch
    DEPENDS
      "${AL_SHIM_STAMP_VARIANT_SCRIPT}"
      "${AL_SHIM_STAMP_VARIANT_SOURCE}/equilibrium.h5"
      "${AL_SHIM_STAMP_VARIANT_SOURCE}/master.h5"
    VERBATIM
  )
  add_custom_target(al-fortran-test-shim-stamp-fixtures ALL DEPENDS
    "${AL_SHIM_STAMP_ABSENT_FIXTURE}/equilibrium.h5"
    "${AL_SHIM_STAMP_MALFORMED_FIXTURE}/equilibrium.h5"
    "${AL_SHIM_STAMP_MISMATCH_FIXTURE}/equilibrium.h5"
  )

  function(add_shim_stamp_test name fixture require_no_loss_log)
    set(target "al-fortran-test-shim-${name}")
    string(REPLACE "-" "_" source_name "${name}")
    add_executable(${target} "test_shim_${source_name}.f90")
    target_link_libraries(${target} PRIVATE al-fortran)
    al_shim_private_module_dir(${target})
    add_dependencies(${target} al-fortran-test-shim-stamp-fixtures)

    if( require_no_loss_log )
      set(loss_log_dir "${CMAKE_CURRENT_BINARY_DIR}/${name}-loss-log")
      add_test(NAME ${target}
        COMMAND ${CMAKE_COMMAND}
          -D "COMMAND_TO_RUN=$<TARGET_FILE:${target}>;${fixture}"
          -D "LOSS_LOG_DIR=${loss_log_dir}"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/check_no_shim_loss_log.cmake"
      )
      set(environment
        "IMAS_MVDD_HLI_DD_VERSION=4.1.1;IMAS_MVDD_LOSS_LOG_DIR=${loss_log_dir};${AL_CORE_TEST_ENVIRONMENT}")
    else()
      add_test(NAME ${target} COMMAND ${target} "${fixture}")
      set(environment "IMAS_MVDD_HLI_DD_VERSION=4.1.1;${AL_CORE_TEST_ENVIRONMENT}")
    endif()

    set_tests_properties(${target} PROPERTIES
      ENVIRONMENT "${environment}"
      LABELS "shim;contract-assertion"
    )
  endfunction()

  add_shim_stamp_test(stamp-absent "${AL_SHIM_STAMP_ABSENT_FIXTURE}" TRUE)
  add_shim_stamp_test(stamp-malformed "${AL_SHIM_STAMP_MALFORMED_FIXTURE}" FALSE)
  # Third passthrough scenario of issue #77 -- see the comment block below
  # this whole if/else for the other two, which need no derived fixture and
  # so are not gated on this stamp-variant tooling.
  add_shim_stamp_test(stamp-mismatch-no-artifact "${AL_SHIM_STAMP_MISMATCH_FIXTURE}" TRUE)
endif()

# Passthrough scenarios (issue #77): the three ways nothing is supposed to
# happen, each proven rather than assumed by asserting the loss directory
# stays empty via check_no_shim_loss_log.cmake -- the only way "no file means
# no loss" becomes enforced, and the only way the third scenario becomes
# assertable at all (its forwarding is otherwise indistinguishable from the
# second's, by contract).
#
#   1. al-fortran-test-shim-version-unset      -- IMAS_MVDD_HLI_DD_VERSION unset
#   2. al-fortran-test-shim-stamp-equal         -- stamp equals the HLI version
#   3. al-fortran-test-shim-stamp-mismatch-no-artifact (above, needs a derived
#      fixture, so it lives inside the stamp-variant-tooling block)
#
# Each scenario needs its own HLI DD version for the whole process (ADR 0005's
# one-version-per-process rule: a second, different value is refused, not
# applied), and a ctest test is its own process, so registering these as three
# separate executables/tests -- rather than branching inside one program -- is
# what satisfies that constraint. No in-test mechanism is needed or possible.
add_executable(al-fortran-test-shim-version-unset test_shim_version_unset.f90)
target_link_libraries(al-fortran-test-shim-version-unset PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-version-unset)
set(AL_SHIM_VERSION_UNSET_LOSS_LOG_DIR "${CMAKE_CURRENT_BINARY_DIR}/version-unset-loss-log")
add_test(NAME al-fortran-test-shim-version-unset
  COMMAND ${CMAKE_COMMAND}
    -D "COMMAND_TO_RUN=$<TARGET_FILE:al-fortran-test-shim-version-unset>;${AL_SHIM_FIXTURE_ROOT}"
    -D "LOSS_LOG_DIR=${AL_SHIM_VERSION_UNSET_LOSS_LOG_DIR}"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/check_no_shim_loss_log.cmake"
)
# IMAS_MVDD_HLI_DD_VERSION is deliberately absent from this ENVIRONMENT: that
# is the scenario. IMAS_MVDD_LOSS_LOG_DIR is still given, and still private to
# this test, so "no version discovery happened" is asserted as an empty
# directory rather than mistaken for a missing one.
set_tests_properties(al-fortran-test-shim-version-unset PROPERTIES
  ENVIRONMENT "IMAS_MVDD_LOSS_LOG_DIR=${AL_SHIM_VERSION_UNSET_LOSS_LOG_DIR};${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;contract-assertion"
)

add_executable(al-fortran-test-shim-stamp-equal test_shim_stamp_equal.f90)
target_link_libraries(al-fortran-test-shim-stamp-equal PRIVATE al-fortran)
al_shim_private_module_dir(al-fortran-test-shim-stamp-equal)
set(AL_SHIM_STAMP_EQUAL_LOSS_LOG_DIR "${CMAKE_CURRENT_BINARY_DIR}/stamp-equal-loss-log")
add_test(NAME al-fortran-test-shim-stamp-equal
  COMMAND ${CMAKE_COMMAND}
    -D "COMMAND_TO_RUN=$<TARGET_FILE:al-fortran-test-shim-stamp-equal>;${AL_SHIM_FIXTURE_ROOT}"
    -D "LOSS_LOG_DIR=${AL_SHIM_STAMP_EQUAL_LOSS_LOG_DIR}"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/check_no_shim_loss_log.cmake"
)
set_tests_properties(al-fortran-test-shim-stamp-equal PROPERTIES
  ENVIRONMENT "IMAS_MVDD_HLI_DD_VERSION=4.1.1;IMAS_MVDD_LOSS_LOG_DIR=${AL_SHIM_STAMP_EQUAL_LOSS_LOG_DIR};${AL_CORE_TEST_ENVIRONMENT}"
  LABELS "shim;contract-assertion"
)
