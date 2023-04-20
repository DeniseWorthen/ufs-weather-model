help([[
loads UFS Model prerequisites for Cheyenne/GNU
]])

prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/spack-stack-1.3.0/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/glade/work/jedipara/cheyenne/spack-stack/modulefiles/misc")

stack_gnu_ver=os.getenv("stack_gnu_ver") or "10.1.0"
load(pathJoin("stack-gcc", stack_gnu_ver))

stack_openmpi_ver=os.getenv("stack_openmpi_ver") or "4.1.1"
load(pathJoin("stack-openmpi", stack_openmpi_ver))

stack_python_ver=os.getenv("stack_python_ver") or "3.9.12"
load(pathJoin("stack-python", stack_python_ver))

cmake_ver=os.getenv("cmake_ver") or "3.22.0"
load(pathJoin("cmake", cmake_ver))

load("ufs_common")

setenv("CC", "mpicc")
setenv("CXX", "mpicxx")
setenv("FC", "mpif90")
setenv("CMAKE_Platform", "cheyenne.gnu")

whatis("Description: UFS build environment")
