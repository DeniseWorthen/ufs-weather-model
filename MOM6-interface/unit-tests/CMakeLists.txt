# =============================================================================
# MOM6 Cap OutputLog Unit Tests Build Configuration
# =============================================================================
if(NOT PROJECT_NAME)
  cmake_minimum_required(VERSION 3.19)
  project(MOM6_UnitTests LANGUAGES Fortran)
  enable_testing()
  find_package(ESMF REQUIRED)
endif()

add_executable(test_readnml test_outputlog_readnml.F90)

# Link against the mom6 library and ESMF
target_link_libraries(test_readnml PRIVATE mom6 ESMF::ESMF)

# Ensure the build can find the MOM6 .mod files (only needed for standalone)
if(MOM6_INC_DIR)
  target_include_directories(test_readnml PRIVATE ${MOM6_INC_DIR})
endif()

target_compile_definitions(test_readnml PRIVATE UNIT_TESTING)

add_test(NAME MOM6_Outputlog_Readnml COMMAND test_readnml)
