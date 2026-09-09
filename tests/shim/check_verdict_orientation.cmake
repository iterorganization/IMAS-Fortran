# Fails if a shim-suite comparison names one of its two readings as the other.
#
# Why a source check rather than a Fortran assertion.
#
# `only3` and `only4` name *which side* of a comparison was absent, so a
# verdict is only as trustworthy as the caller's account of which reading it
# handed over. Nothing observable at run time distinguishes a correct account
# from an inverted one: both labels differ from `same`, so every pass/fail in
# the suite is identical either way and no Fortran assertion can be written
# that fails on the inversion. That is exactly how it survived --
# test_shim_structural_rules and test_shim_cocos_rules named the shim-served
# reading as the oracle for every rule they check, their served-nothing
# readings reported `only3` where the vocabulary calls that situation `only4`,
# and the inverted labels were transcribed into tests/shim/README.md as the
# account of what the shim did.
#
# shim_comparison closed the positional half of the hole: its public functions
# sit behind a dummy of a private type, so a positional call is a compile error
# and `oracle=` / `converted=` are the only way to call one. This closes the
# other half, which the compiler cannot see: a call that spells both keywords
# and feeds them the wrong readings. With the keyword adjacent to its value
# that is a pattern; the positional swap it replaced was not.
#
# The rule, stated once: the suite reads the DD 4.1.1 fixture same-version into
# a `control` variable and the DD 3.39.0 fixture through the shim into a
# `cross` one. So an `oracle`-named argument may never be fed from a `cross`
# reading, and a `converted`-named argument never from a `control` one. That
# covers the module primitives and the per-program helpers alike, since the
# helpers now take the same two keywords.
#
# Boundary: a *new* internal helper called positionally is invisible here, as
# it is to the compiler. Give any such helper `oracle`/`converted` dummies and
# call it by keyword, which is what every helper in the suite now does.
#
# Invoked by tests/shim/CMakeLists.txt.

if( NOT DEFINED SOURCE_DIR )
  message( FATAL_ERROR "check_verdict_orientation.cmake: -D SOURCE_DIR=... is required" )
endif()

file( GLOB _sources "${SOURCE_DIR}/*.f90" )
if( _sources STREQUAL "" )
  message( FATAL_ERROR "check_verdict_orientation.cmake: no Fortran sources under ${SOURCE_DIR}" )
endif()

set( _violations "" )
# Counted so the check cannot pass by matching nothing at all -- the failure
# mode of every grep-shaped test.
set( _named_sides 0 )

foreach( _source IN LISTS _sources )
  file( STRINGS "${_source}" _lines )
  get_filename_component( _name "${_source}" NAME )
  set( _line_number 0 )

  foreach( _line IN LISTS _lines )
    math( EXPR _line_number "${_line_number} + 1" )

    # Comments are stripped first: this file's own prose, and the explanation
    # in shim_comparison, both spell the inverted forms in order to name them.
    string( REGEX REPLACE "!.*$" "" _code "${_line}" )

    # A role keyword and the reading it is fed, for either role. The value is
    # taken up to the next comma, closing paren or continuation marker.
    if( _code MATCHES "(has_)?oracle(_value)?[ \t]*=[ \t]*([^,)&]*)" )
      set( _fed "${CMAKE_MATCH_3}" )
      math( EXPR _named_sides "${_named_sides} + 1" )
      if( _fed MATCHES "cross" )
        list( APPEND _violations
          "${_name}:${_line_number}: an oracle-named argument is fed the cross-version reading:\n      ${_line}" )
      endif()
    endif()

    if( _code MATCHES "(has_)?converted(_value)?[ \t]*=[ \t]*([^,)&]*)" )
      set( _fed "${CMAKE_MATCH_3}" )
      math( EXPR _named_sides "${_named_sides} + 1" )
      if( _fed MATCHES "control" )
        list( APPEND _violations
          "${_name}:${_line_number}: a converted-named argument is fed the control reading:\n      ${_line}" )
      endif()
    endif()
  endforeach()
endforeach()

# The three rule programs plus the refusal program and the oracle's own unit
# test all name their sides, so the true figure is in the hundreds. A floor
# well under it still fails if the keywords are removed wholesale, which is the
# only way this check could go quiet without a real change of design.
set( _minimum_named_sides 40 )
if( _named_sides LESS _minimum_named_sides )
  message( FATAL_ERROR
    "check_verdict_orientation.cmake found only ${_named_sides} named comparison "
    "sides under ${SOURCE_DIR}, fewer than the ${_minimum_named_sides} expected. "
    "Either the suite stopped naming its sides -- in which case the inversion "
    "this check exists to catch is undetectable again -- or the keywords were "
    "renamed and this check needs to follow them." )
endif()

if( NOT _violations STREQUAL "" )
  string( REPLACE ";" "\n  " _report "${_violations}" )
  message( FATAL_ERROR
    "VERDICT-ORIENTATION-FAILURE: a comparison names one of its readings as the other.\n"
    "  The DD 4.1.1 same-version read is the oracle; the read through the shim is\n"
    "  the converted side. Swapping the names turns every only4 into only3 without\n"
    "  changing whether any test passes.\n\n  ${_report}" )
endif()

message( STATUS
  "check_verdict_orientation: ${_named_sides} named comparison sides, none inverted" )
