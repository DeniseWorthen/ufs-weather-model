help([[
loads UFS Model prerequisites for Cheyenne/Intel
]])

prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/spack-stack-1.3.0/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/glade/work/jedipara/cheyenne/spack-stack/modulefiles/misc")

stack_intel_ver=os.getenv("stack_intel_ver") or "19.1.1.217"
load(pathJoin("stack-intel", stack_intel_ver))

stack_mpi_ver=os.getenv("stack_mpi_ver") or "2019.7.217"
load(pathJoin("stack-intel-mpi", stack_mpi_ver))

stack_python_ver=os.getenv("stack_python_ver") or "3.9.12"
load(pathJoin("stack-python", stack_python_ver))

cmake_ver=os.getenv("cmake_ver") or "3.22.0"
load(pathJoin("cmake", cmake_ver))

load("ufs_common")

setenv("CC", "mpicc")
setenv("CXX", "mpicxx")
setenv("FC", "mpif90")
setenv("CMAKE_Platform", "cheyenne.intel")

whatis("Description: UFS build environment")
