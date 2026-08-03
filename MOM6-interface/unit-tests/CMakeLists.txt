# =============================================================================
# MOM6 Cap OutputLog Unit Tests Build Configuration
# =============================================================================
if(NOT PROJECT_NAME)
  cmake_minimum_required(VERSION 3.19)
  project(MOM6_UnitTests LANGUAGES Fortran)
  enable_testing()
  find_package(ESMF REQUIRED)
endif()

add_executable(test_readnml test_utils.F90 test_outputlog_readnml.F90)
add_executable(test_freqn test_outputlog_freqn.F90 nc_fixture_mod.F90)
add_executable(test_alarm test_alarminit.F90 test_utils.F90)

# ---------------------------------------------------------
# LINK LIBRARIES (One line per target!)
# ---------------------------------------------------------
target_link_libraries(test_readnml PRIVATE mom6 ESMF::ESMF)
target_link_libraries(test_alarm PRIVATE mom6 ESMF::ESMF)
target_link_libraries(test_freqn PRIVATE mom6 ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran)

# ---------------------------------------------------------
# INCLUDE DIRECTORIES (One line per target!)
# ---------------------------------------------------------
if(MOM6_INC_DIR)
  target_include_directories(test_readnml PRIVATE ${MOM6_INC_DIR})
  target_include_directories(test_freqn PRIVATE ${MOM6_INC_DIR})
  target_include_directories(test_alarm PRIVATE ${MOM6_INC_DIR})
endif()

# ---------------------------------------------------------
# COMPILE DEFINITIONS (One line per target!)
# ---------------------------------------------------------
target_compile_definitions(test_readnml PRIVATE UNIT_TESTING)
target_compile_definitions(test_freqn PRIVATE UNIT_TESTING)
target_compile_definitions(test_alarm PRIVATE UNIT_TESTING)

add_test(NAME MOM6_Outputlog_Readnml COMMAND test_readnml)
add_test(NAME MOM6_Outputlog_Freqn COMMAND test_freqn)
add_test(NAME MOM6_Outputlog_Alarm COMMAND test_alarm)

# =============================================================================
# Option B: file_is_complete / get_file_state contract test
# =============================================================================
add_executable(test_outputlog_completion
  test_outputlog_completion.F90
  nc_fixture_mod.F90
)

target_link_libraries(test_outputlog_completion PRIVATE
  mom6
  ESMF::ESMF
  NetCDF::NetCDF_Fortran
  MPI::MPI_Fortran
)

if(MOM6_INC_DIR)
  target_include_directories(test_outputlog_completion PRIVATE ${MOM6_INC_DIR})
endif()

target_compile_definitions(test_outputlog_completion PRIVATE UNIT_TESTING)

add_test(NAME MOM6_Outputlog_Completion
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 1 $<TARGET_FILE:test_outputlog_completion>)
