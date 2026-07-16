# =============================================================================
# MOM6 Cap OutputLog Unit Tests Build Configuration
# =============================================================================

add_executable(test_readnml test_outputlog_readnml.F90)

target_link_libraries(test_readnml PRIVATE mom6)

target_compile_definitions(test_readnml PRIVATE UNIT_TESTING)

add_test(NAME MOM6_Outputlog_Readnml COMMAND test_readnml)
