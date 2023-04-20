help([[
Load environment to build UFS on Acorn with Intel compiler
]])

prepend_path("MODULEPATH", "/lfs/h1/emc/nceplibs/noscrub/spack-stack/spack-stack-1.3.0/envs/unified-env-compute/install/modulefiles/Core")

load("stack-intel")
load("stack-cray-mpich")
load("stack-python")

--Avoid conflicts for several packages by unusing system spack installation:
remove_path("MODULEPATH", "/apps/ops/prod/libs/modulefiles/compiler/intel/19.1.3.304")
remove_path("MODULEPATH", "/apps/ops/prod/libs/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.4")
remove_path("MODULEPATH", "/apps/ops/prod/libs/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.7")

load("ufs_common")

prepend_path("MODULEPATH", "/lfs/h1/emc/nceplibs/noscrub/UPP_IFI/modulefiles")
load("ifi/20230118-intel-19.1.3.304")

setenv("CMAKE_Platform", "acorn")

whatis("Description: UFS build environment")
