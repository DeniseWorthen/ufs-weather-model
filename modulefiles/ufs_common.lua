help([[
loads UFS Model common libraries
]])

jasper_ver=os.getenv("jasper_ver") or "2.0.32"
load(pathJoin("jasper", jasper_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.13"
load(pathJoin("zlib", zlib_ver))

libpng_ver=os.getenv("libpng_ver") or "1.6.37"
load(pathJoin("libpng", libpng_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.14.0"
load(pathJoin("hdf5", hdf5_ver))

netcdf_c_ver=os.getenv("netcdf_ver") or "4.9.2"
load(pathJoin("netcdf-c", netcdf_c_ver))

netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "4.6.0"
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))

pio_ver=os.getenv("pio_ver") or "2.5.9"
load(pathJoin("parallelio", pio_ver))

esmf_ver=os.getenv("esmf_ver") or "8.3.0b09"
load(pathJoin("esmf", esmf_ver))

fms_ver=os.getenv("fms_ver") or "2022.04"
load(pathJoin("fms",fms_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

crtm_ver=os.getenv("crtm_ver") or "2.4.0"
load(pathJoin("crtm", crtm_ver))

g2_ver=os.getenv("g2_ver") or "3.4.5"
load(pathJoin("g2", g2_ver))

g2tmpl_ver=os.getenv("g2tmpl_ver") or "1.10.2"
load(pathJoin("g2tmpl", g2tmpl_ver))

ip_ver=os.getenv("ip_ver") or "3.3.3"
load(pathJoin("ip", ip_ver))

sp_ver=os.getenv("sp_ver") or "2.3.3"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
load(pathJoin("w3emc", w3emc_ver))

gftl_shared_ver=os.getenv("gftl_shared_ver") or "1.5.0"
load(pathJoin("gftl-shared", gftl_shared_ver))

mapl_ver=os.getenv("mapl_ver") or "2.22.0-esmf-8.3.0b09"
load(pathJoin("mapl", mapl_ver))

scotch_ver=os.getenv("scotch_ver") or "7.0.3"
load(pathJoin("scotch", scotch_ver))

whatis("Description: UFS build environment common libraries")
