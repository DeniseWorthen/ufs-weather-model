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
add_executable(test_methods test_outputlog_methods.F90 outputlog_tracker.F90)

# ---------------------------------------------------------
# LINK LIBRARIES (One line per target!)
# ---------------------------------------------------------
target_link_libraries(test_readnml PRIVATE mom6 ESMF::ESMF)
target_link_libraries(test_methods PRIVATE mom6 ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran)

# ---------------------------------------------------------
# INCLUDE DIRECTORIES (One line per target!)
# ---------------------------------------------------------
if(MOM6_INC_DIR)
  target_include_directories(test_readnml PRIVATE ${MOM6_INC_DIR})
  target_include_directories(test_methods PRIVATE ${MOM6_INC_DIR})
endif()

# ---------------------------------------------------------
# COMPILE DEFINITIONS (One line per target!)
# ---------------------------------------------------------
target_compile_definitions(test_readnml PRIVATE UNIT_TESTING)
target_compile_definitions(test_methods PRIVATE UNIT_TESTING)

add_test(NAME MOM6_Outputlog_Readnml COMMAND test_readnml)
add_test(NAME MOM6_Outputlog_Methods COMMAND test_methods)
