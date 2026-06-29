# =============================================================================
# MOM6 Cap OutputLog Unit Tests Build Configuration
# =============================================================================

# 1. Compile the standalone test driver into an executable
add_executable(test_outputlog test_outputlog_methods.F90)

# 2. Link against the main mom6 library target.
#    This automatically inherits the central Fortran module paths,
#    NetCDF, ESMF, FMS, and all underlying dependencies.
target_link_libraries(test_outputlog PRIVATE mom6)

# 3. Inject the preprocessor flag to unlock public visibility for:
#    setrequest, settype, and setrootname
target_compile_definitions(test_outputlog PRIVATE UNIT_TESTING)

# 4. Register the executable with CMake's CTest harness
add_test(NAME MOM6_OutputLog_Methods COMMAND test_outputlog)
