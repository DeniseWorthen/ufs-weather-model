program test_outputlog_completion

  use mom_outputlog_methods, only : get_file_state, file_is_complete
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm_rank, MPI_Barrier, MPI_COMM_WORLD
  use netcdf

  implicit none

  ! Global test bookkeeping variables
  integer :: nt, npass, nfail
  logical :: is_passing
  character(len=256) :: testname
  character(len=256), allocatable :: msg(:)

  ! MPI Environment Variables
  integer :: my_rank, ierr
  integer, parameter :: root_pe = 0
  logical :: is_root

  ! Initialize the MPI execution environment first
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  is_root = (my_rank == root_pe)

  ! Initialize bookkeeping
  nt = 0
  npass = 0
  nfail = 0
  allocate(msg(1:100)) ! Room for tracking test strings

  ! ===========================================================================
  ! EXISTING TESTS (Your previous tests for setrequest, settype, etc. go here)
  ! ===========================================================================

  ! nt = nt + 1
  ! write(testname, ...)
  ! ... existing test logic ...

  ! ===========================================================================
  ! NEW DYNAMIC LOGIC TESTS (Option 2)
  ! ===========================================================================

  ! ------------------
  ! Test 14: DATM Environment -> Safe to check nlen only (chk4size = .false.)
  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)') 'test ', nt, ' file_is_complete: DATM stream'

  block
    logical :: is_complete
    integer :: test_rc, test_createsize
    character(len=256) :: test_nc_file = 'dummy_datm_stream.nc'

    ! Time 0: Clean initialization (nlen = 0)
    if (is_root) call create_mock_history_file(test_nc_file)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    call get_file_state(MPI_COMM_WORLD, is_root, root_pe, test_nc_file, fsize=test_createsize, rc=test_rc)

    is_complete = file_is_complete(MPI_COMM_WORLD, is_root, root_pe, test_nc_file, &
                                   chk4size=.false., createsize=test_createsize, rc=test_rc)

    if (is_complete) then
       is_passing = .false.
    else
       ! Time 1: Data written (nlen becomes > 0)
       if (is_root) call advance_mock_history_file(test_nc_file)
       call MPI_Barrier(MPI_COMM_WORLD, ierr)

       is_complete = file_is_complete(MPI_COMM_WORLD, is_root, root_pe, test_nc_file, &
                                      chk4size=.false., createsize=test_createsize, rc=test_rc)

       is_passing = (is_complete .eqv. .true. .and. test_rc == 0)
    endif

    if (is_root) call cleanup_mock_history_file(test_nc_file)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    if (is_passing) then
       npass = npass + 1
       msg(nt) = trim(testname)//' PASS'
    else
       nfail = nfail + 1
       msg(nt) = trim(testname)//' FAIL (DATM check failed)'
    endif
  end block

  ! ------------------
  ! Test 15: Active ATM (FMS Coupled) -> Size Guarded Protection (chk4size = .true.)
  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)') 'test ', nt, ' file_is_complete: Active ATM size-guard'

  block
    logical :: is_complete
    integer :: test_rc, test_createsize
    character(len=256) :: test_nc_file = 'dummy_active_atm_stream.nc'

    ! Time 0: Pre-allocated file initialization (nlen > 0 immediately)
    if (is_root) then
       call create_mock_history_file(test_nc_file)
       call advance_mock_history_file(test_nc_file)
    endif
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    call get_file_state(MPI_COMM_WORLD, is_root, root_pe, test_nc_file, fsize=test_createsize, rc=test_rc)

    ! Must be .false. because size equals createsize, despite nlen > 0
    is_complete = file_is_complete(MPI_COMM_WORLD, is_root, root_pe, test_nc_file, &
                                   chk4size=.true., createsize=test_createsize, rc=test_rc)

    if (is_complete) then
       is_passing = .false.
    else
       ! Time 1: MOM writes true data frame, expanding file size
       if (is_root) call advance_mock_history_file(test_nc_file)
       call MPI_Barrier(MPI_COMM_WORLD, ierr)

       is_complete = file_is_complete(MPI_COMM_WORLD, is_root, root_pe, test_nc_file, &
                                      chk4size=.true., createsize=test_createsize, rc=test_rc)

       is_passing = (is_complete .eqv. .true. .and. test_rc == 0)
    endif

    if (is_root) call cleanup_mock_history_file(test_nc_file)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    if (is_passing) then
       npass = npass + 1
       msg(nt) = trim(testname)//' PASS'
    else
       nfail = nfail + 1
       msg(nt) = trim(testname)//' FAIL (Active ATM guard failed)'
    endif
  end block

  ! ===========================================================================
  ! SCOREBOARD PRINTING
  ! ===========================================================================
  if (is_root) then
     print *, "=================================================="
     print *, "             MOM6 OUTPUTLOG TEST RESULTS          "
     print *, "=================================================="
     do ierr = 1, nt
        print '(A)', trim(msg(ierr))
     enddo
     print *, "--------------------------------------------------"
     print '(A,I3,A,I3)', " Tests Passing: ", npass, " / Total: ", nt
     print *, "=================================================="
  endif

  ! Finalize MPI cleanly before exiting the program
  call MPI_Finalize(ierr)

contains

  ! ===========================================================================
  ! INTERNAL HELPER SUBROUTINES (Only the root PE calls these to modify disk)
  ! ===========================================================================

  subroutine create_mock_history_file(fname)
    character(len=*), intent(in) :: fname
    integer :: ncid, dimid, varid

    ! Simple stub wrapper to fail cleanly on NetCDF errors
    if (nf90_create(trim(fname), nf90_clobber, ncid) /= nf90_noerr) stop "NC_FAIL: create"
    if (nf90_def_dim(ncid, 'time', nf90_unlimited, dimid) /= nf90_noerr) stop "NC_FAIL: def_dim"
    if (nf90_def_var(ncid, 'dummy', nf90_int, (/dimid/), varid) /= nf90_noerr) stop "NC_FAIL: def_var"
    if (nf90_enddef(ncid) /= nf90_noerr) stop "NC_FAIL: enddef"
    if (nf90_close(ncid) /= nf90_noerr) stop "NC_FAIL: close"
  end subroutine create_mock_history_file

  subroutine advance_mock_history_file(fname)
    character(len=*), intent(in) :: fname
    integer :: ncid, varid, current_len
    integer :: dummy_data(1) = 42

    ! Open the file, check how long it currently is, and append an integer to grow it
    if (nf90_open(trim(fname), nf90_write, ncid) /= nf90_noerr) stop "NC_FAIL: open"
    if (nf90_inq_varid(ncid, 'dummy', varid) /= nf90_noerr) stop "NC_FAIL: inq_varid"

    ! Using get_file_state logic conceptually to find out our append target location
    current_len = 0
    block
       integer :: dimid
       if (nf90_inquire(ncid, unlimiteddimid=dimid) == nf90_noerr) then
          call nf90_inquire_dimension(ncid, dimid, len=current_len)
       endif
    end block

    ! Append 1 integer frame at the next slot index
    if (nf90_put_var(ncid, varid, dummy_data, start=(/current_len + 1/)) /= nf90_noerr) stop "NC_FAIL: put"
    if (nf90_close(ncid) /= nf90_noerr) stop "NC_FAIL: close"
  end subroutine advance_mock_history_file

  subroutine cleanup_mock_history_file(fname)
    character(len=*), intent(in) :: fname
    ! Standard Fortran intrinsic to remove the temporary file from the operating system
    open(unit=99, file=trim(fname), status='old')
    close(unit=99, status='delete')
  end subroutine cleanup_mock_history_file

end program test_outputlog_completion
