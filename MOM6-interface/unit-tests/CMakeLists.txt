# =============================================================================
# MOM6 Cap OutputLog Unit Tests Build Configuration
# =============================================================================
if(NOT PROJECT_NAME)
  cmake_minimum_required(VERSION 3.19)
  project(MOM6_UnitTests LANGUAGES Fortran)
  enable_testing()
  find_package(ESMF REQUIRED)
endif()

# -----------------------------------------------------------------------------
# add_mom6_unit_test(<exe_name>
#     SOURCES <src1> [<src2> ...]
#     [EXTRA_LIBS <lib1> ...]     # anything beyond mom6 + ESMF::ESMF
#     [CTEST_NAME <name>]         # ctest display name; defaults to <exe_name>
#     [USE_MPI])                  # launch via MPIEXEC -- required for any
#                                  # test that makes real MPI calls (Bcast,
#                                  # Barrier, etc.), not just one that links
#                                  # MPI::MPI_Fortran
#
# Registers the executable, its common link libraries / include dir /
# UNIT_TESTING compile definition, and its ctest entry in one call, so the
# four test targets below don't each repeat the same five CMake calls.
# -----------------------------------------------------------------------------
function(add_mom6_unit_test exe_name)
  cmake_parse_arguments(ARG "USE_MPI" "CTEST_NAME" "SOURCES;EXTRA_LIBS" ${ARGN})

  if(NOT ARG_CTEST_NAME)
    set(ARG_CTEST_NAME ${exe_name})
  endif()

  add_executable(${exe_name} ${ARG_SOURCES})
  target_link_libraries(${exe_name} PRIVATE mom6 ESMF::ESMF ${ARG_EXTRA_LIBS})
  target_compile_definitions(${exe_name} PRIVATE UNIT_TESTING)
  if(MOM6_INC_DIR)
    target_include_directories(${exe_name} PRIVATE ${MOM6_INC_DIR})
  endif()

  if(ARG_USE_MPI)
    add_test(NAME ${ARG_CTEST_NAME}
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 1 $<TARGET_FILE:${exe_name}>)
  else()
    add_test(NAME ${ARG_CTEST_NAME} COMMAND ${exe_name})
  endif()
endfunction()

add_mom6_unit_test(test_readnml
  CTEST_NAME MOM6_Outputlog_Readnml
  SOURCES test_utils.F90 test_outputlog_readnml.F90)

add_mom6_unit_test(test_alarm
  CTEST_NAME MOM6_Outputlog_Alarm
  SOURCES test_alarminit.F90 test_utils.F90)

add_mom6_unit_test(test_freqn
  CTEST_NAME MOM6_Outputlog_Freqn
  SOURCES test_outputlog_freqn.F90 nc_fixture_mod.F90
  EXTRA_LIBS NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  USE_MPI)

# Option B: file_is_complete / get_file_state contract test
add_mom6_unit_test(test_outputlog_completion
  CTEST_NAME MOM6_Outputlog_Completion
  SOURCES test_outputlog_completion.F90 nc_fixture_mod.F90
  EXTRA_LIBS NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  USE_MPI)
