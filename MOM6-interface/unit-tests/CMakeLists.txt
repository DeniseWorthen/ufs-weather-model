# =============================================================================
# MOM6 Cap OutputLog Unit Tests Build Configuration
# =============================================================================
if(NOT PROJECT_NAME)
  cmake_minimum_required(VERSION 3.19)
  project(MOM6_UnitTests LANGUAGES Fortran)
  enable_testing()
  find_package(ESMF REQUIRED)
endif()

# mom6_test_utils is shared by test_readnml and test_alarm -- same reasoning as
# nc_fixture_mod above: build once, link, don't compile it twice.
add_library(mom6_test_utils STATIC test_utils.F90)
target_link_libraries(mom6_test_utils PRIVATE mom6 ESMF::ESMF)
if(MOM6_INC_DIR)
  target_include_directories(mom6_test_utils PRIVATE ${MOM6_INC_DIR})
endif()

# nc_fixture_mod is shared by test_freqn and test_outputlog_completion.
# Build it once, here, rather than listing it as a source of both -- under
# parallel builds, two targets independently compiling the same source both
# write to the same .mod file at the same time and race on it (this is what
# caused the "cannot rename module file" failure once CI started building
# every target together instead of one at a time).
add_library(nc_fixture_mod STATIC nc_fixture_mod.F90)
target_link_libraries(nc_fixture_mod PRIVATE mom6 ESMF::ESMF NetCDF::NetCDF_Fortran MPI::MPI_Fortran)
if(MOM6_INC_DIR)
  target_include_directories(nc_fixture_mod PRIVATE ${MOM6_INC_DIR})
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

  # Umbrella target: every test executable registered through this function
  # becomes a dependency automatically. CI builds just `mom6_unit_tests` and
  # never needs to grow a --target list as tests are added -- only this file
  # does, via the call below each new add_mom6_unit_test() adds itself to.
  if(NOT TARGET mom6_unit_tests)
    add_custom_target(mom6_unit_tests)
  endif()
  add_dependencies(mom6_unit_tests ${exe_name})
endfunction()

add_mom6_unit_test(test_outputlog_readnml
  CTEST_NAME MOM6_Outputlog_Readnml
  SOURCES test_outputlog_readnml.F90
  EXTRA_LIBS mom6_test_utils)

add_mom6_unit_test(test_outputlog_alarm
  CTEST_NAME MOM6_Outputlog_Alarm
  SOURCES test_alarminit.F90
  EXTRA_LIBS mom6_test_utils)

add_mom6_unit_test(test_outputlog_freqn
  CTEST_NAME MOM6_Outputlog_Freqn
  SOURCES test_outputlog_freqn.F90
  EXTRA_LIBS mom6_test_utils nc_fixture_mod NetCDF::NetCDF_Fortran MPI::MPI_Fortran
  USE_MPI)

#add_mom6_unit_test(test_outputlog_completion
#  CTEST_NAME MOM6_Outputlog_Completion
#  SOURCES test_outputlog_completion.F90
#  EXTRA_LIBS nc_fixture_mod NetCDF::NetCDF_Fortran MPI::MPI_Fortran
#  USE_MPI)

#add_mom6_unit_test(test_outputlog_finalize
#   CTEST_NAME MOM6_Outputlog_Finalize
#   SOURCES test_outputlog_finalize.F90
#   EXTRA_LIBS mom6_test_utils nc_fixture_mod NetCDF::NetCDF_Fortran MPI::MPI_Fortran
#   USE_MPI)

add_mom6_unit_test(test_driver
   CTEST_NAME MOM6_Outputlog_Finalize
   SOURCES test_driver.F90
   EXTRA_LIBS mom6_test_utils nc_fixture_mod NetCDF::NetCDF_Fortran MPI::MPI_Fortran
   USE_MPI)
