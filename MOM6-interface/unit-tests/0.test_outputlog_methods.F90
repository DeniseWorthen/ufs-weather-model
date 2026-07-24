program test_outputlog_methods
  ! Driver to set up and exercise file-based methods

  use mom_outputlog_methods, only : get_file_state, file_is_complete
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm_rank, MPI_Barrier, MPI_COMM_WORLD
  use netcdf

  implicit none

  integer, parameter :: root_pe = 0
  logical :: is_root
  integer :: my_rank, ierr
  character(len=80) :: fname
  integer :: nlen, fsize,rc
  integer :: xdim = 1e3

  ! Initialize the MPI execution environment first
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  is_root = (my_rank == root_pe)

!#ifdef test
  ! datm case should show unlimited 0 at creation and only 1 when full !check
  fname = 'testdatm.nc'
  if (is_root) call create_file(trim(fname))
  call get_file_state(MPI_COMM_WORLD, is_root, root_pe, fname, nlen=nlen, &
       fsize=fsize, rc=rc)
  print *,'create ',trim(fname),nlen,fsize

  !call populate_timestep_data(trim(fname))
  !call get_file_state(MPI_COMM_WORLD, is_root, root_pe, fname, nlen=nlen, &
  !    fsize=fsize, rc=rc)
  !print *,'fill ',trim(fname),nlen,fsize
!#endif
!#ifdef test
  ! fv3 diag case shows time = UNLIMITED ; // (1 currently) and time = _ ; at creation
  fname = 'testfv3.nc'
  if (is_root) call create_file(trim(fname),preallocate=.true.)
  call get_file_state(MPI_COMM_WORLD, is_root, root_pe, fname, nlen=nlen, &
       fsize=fsize, rc=rc)

  print *,'create ',trim(fname),nlen,fsize

  !call populate_timestep_data(trim(fname))
  !call get_file_state(MPI_COMM_WORLD, is_root, root_pe, fname, nlen=nlen, &
  !     fsize=fsize, rc=rc)
  !print *,'fill ',trim(fname),nlen,fsize
!#endif

  call MPI_Finalize(ierr)

contains

  subroutine create_file(fname, preallocate)

    character(len=*), intent(in) :: fname
    logical, intent(in), optional :: preallocate

    integer :: ncid, xdimid, timedimid, varid, timevarid
    logical :: do_prealloc
    integer :: rc
    real :: dummy_field(xdim)

    dummy_field = nf90_fill_real
    do_prealloc = .false.
    if (present(preallocate)) do_prealloc = preallocate

    if (nf90_create(trim(fname), nf90_clobber, ncid) /= nf90_noerr) stop "NC_FAIL: create"
    if (nf90_def_dim(ncid, 'x', xdim, xdimid) /= nf90_noerr) stop "NC_FAIL: def_dim x"
    if (nf90_def_dim(ncid, 'time', nf90_unlimited, timedimid) /= nf90_noerr) stop "NC_FAIL: def_dim time"

    if (nf90_def_var(ncid, 'time', nf90_double, (/timedimid/), timevarid) /= nf90_noerr) stop "NC_FAIL: def_var time"
    !if (nf90_def_var(ncid, 'field', nf90_real, (/xdimid, timedimid/), varid) /= nf90_noerr) stop "NC_FAIL: def_var field"
    if (nf90_enddef(ncid) /= nf90_noerr) stop "NC_FAIL: enddef"

     if (do_prealloc) then
    !    !dummy_field(1) = 42.0
        if (nf90_put_var(ncid, timevarid, (/nf90_fill_double/), count=(/1/)) /= nf90_noerr) stop "NC_FAIL: put fill time"
    !    rc = nf90_put_var(ncid, varid, dummy_field)
    !    print *,trim(nf90_strerror(rc))
    ! else
    !    if (nf90_redef(ncid) /= nf90_noerr) stop "NC_FAIL: redef"
    !    if (nf90_def_var(ncid, 'field', nf90_real, (/xdimid, timedimid/), varid) /= nf90_noerr) stop "NC_FAIL: def_var field"
     endif
    if (nf90_close(ncid) /= nf90_noerr) stop "NC_FAIL: close"

    rc = nf90_open(trim(fname), nf90_write, ncid)
    rc = nf90_redef(ncid)
    if (nf90_def_var(ncid, 'field', nf90_real, (/xdimid, timedimid/), varid) /= nf90_noerr) stop "NC_FAIL: def_var field"
    if (nf90_enddef(ncid) /= nf90_noerr) stop "NC_FAIL: enddef"
    if (nf90_close(ncid) /= nf90_noerr) stop "NC_FAIL: close"

  end subroutine create_file

  !> Universal Advance Helper: Writes spatial payload to index 1, growing file size and setting final time
  subroutine populate_timestep_data(fname)
    character(len=*), intent(in) :: fname
    integer :: ncid, varid, timevarid
    real :: dummy_field(xdim)
    real(kind=8) :: valid_time = 3600.0

    dummy_field = 42.0

    if (nf90_open(trim(fname), nf90_write, ncid) /= nf90_noerr) stop "NC_FAIL: open"
    if (nf90_inq_varid(ncid, 'time', timevarid) /= nf90_noerr) stop "NC_FAIL: inq time"
    if (nf90_inq_varid(ncid, 'field', varid) /= nf90_noerr) stop "NC_FAIL: inq field"

    ! Populate the spatial grid at timestep slot 1 -> This physically expands the file size on disk
    if (nf90_put_var(ncid, varid, dummy_field, start=(/1, 1/), count=(/xdim, 1/)) /= nf90_noerr) stop "NC_FAIL: put field"

    ! Finalize the timestep value at slot 1 (overwriting any previous fill value)
    if (nf90_put_var(ncid, timevarid, (/valid_time/), start=(/1/)) /= nf90_noerr) stop "NC_FAIL: put valid time"

    if (nf90_close(ncid) /= nf90_noerr) stop "NC_FAIL: close"
  end subroutine populate_timestep_data

  subroutine cleanup_file(fname)
    character(len=*), intent(in) :: fname
    open(unit=99, file=trim(fname), status='old')
    close(unit=99, status='delete')
  end subroutine cleanup_file

end program test_outputlog_methods
