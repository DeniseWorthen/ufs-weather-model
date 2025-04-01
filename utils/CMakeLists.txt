cmake_minimum_required(VERSION 3.19)

add_library(ufs_utils
  ufs_trace_mod.F90
)

set_target_properties(ufs_utils PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(ufs_utils INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                            $<INSTALL_INTERFACE:mod>)
target_link_libraries(ufs_utils MPI::MPI_Fortran)


###############################################################################
### Install
###############################################################################
install(
  TARGETS ufs_utils
  EXPORT ufs_utils-config
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod DESTINATION ${CMAKE_INSTALL_PREFIX})

install(EXPORT ufs_utils-config
  DESTINATION lib/cmake)

export(EXPORT ufs_utils-config)
