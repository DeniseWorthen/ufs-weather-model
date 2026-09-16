!> @file test_track_restn.F90
!> @brief Tests track_restn -- the real per-part restart completion check
!!
!! Calls track_restn directly (mom_outputlog_methods.F90), against real
!! restart-fixture files (nc_fixture_mod.F90's restart_part_fname/
!! make_restart_fixture), verifying both the per-part allDone(:) result
!! and the filenames track_restn itself constructs.
!!
!! Deliberately does NOT test outputlog_restart itself: unlike track_freqn,
!! outputlog_restart still reads mpicomm/restartdir/debug_onroot/lastrestart
!! from mom_cap_outputlog.F90's module scope rather than taking them as
!! explicit arguments -- the same module-state barrier that originally
!! blocked testing this routine at all. That means the all(allDone) gate
!! and the resulting log_restart_fh call (real 3-line restart log, no
!! lastrestart/lastoutput) remain untested here. If that coverage is still
!! wanted, outputlog_restart would need the same explicit-argument
!! treatment track_freqn/outputlog_run already got.
!!
!> @date 09-15-2026
program test_track_restn

  use ESMF
  use mpi_f08,                only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD, MPI_Barrier
  use test_utils
  use mom_outputlog_methods,  only : track_restn
  use nc_fixture_mod,         only : restart_part_fname, make_restart_fixture
  use test_helpers,           only : base_yy, base_mm, base_dd

  implicit none

  integer, parameter :: maxtests = 20

  type(MPI_Comm) :: comm
  integer        :: rank, ierr, rootpe
  logical        :: isroot

  character(len=128) :: testname
  character(len=256) :: assertmsg
  character(len=20)  :: subname = 'test_track_restn'
  character(len=256) :: restartdir = './'

  type(testsummary) :: restntests

  logical :: assertrc
  integer :: n

  comm = MPI_COMM_WORLD
  rootpe = 0

  call restntests%init(maxtests)

  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  isroot = (rank == rootpe)
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Initialize")

  if (isroot) call execute_command_line('rm -f '//trim(restartdir)//'*.MOM.res*.nc', wait=.true.)
  call MPI_Barrier(comm, ierr)
  if (ierr /= 0) then
     write(0,'(A)') "FATAL ("//trim(subname)//"): MPI_Barrier (post-cleanup) failed"
     stop 99
  endif

  ! ===========================================================================
  ! Test cases
  ! ===========================================================================

  ! ------------------
  ! Case 1: 3 parts, 2 complete, 1 not -- confirms track_restn correctly
  ! reports each part's own state, and that all(allDone) stays false with
  ! even one part still incomplete.
  ! ------------------
  call check_partial(hour=0)

  ! ------------------
  ! Case 2: 3 parts, all complete -- confirms all(allDone) becomes true
  ! only once every part genuinely is.
  ! ------------------
  call check_all_complete(hour=6)

  ! ------------------
  ! Case 3: num_rest_files=1 boundary -- both complete and incomplete.
  ! ------------------
  call check_single_file(hour=12, complete=.true.)
  call check_single_file(hour=18, complete=.false.)

  ! ------------------
  ! Test results
  ! ------------------
  if (restntests%nfail > 0) then
     print '(A)', 'FAIL: At least one test failed '
     do n = 1,restntests%count
        if (.not. restntests%teststatus(n)) print '(A)', trim(restntests%testmessage(n)%str)
     enddo
  else
     do n = 1,restntests%count
        print '(A)', trim(restntests%testmessage(n)%str)
     enddo
  endif
  print '(3(A,I0))','Total tests = ',restntests%count,' Passing = ',restntests%npass, &
       ' Failing = ',restntests%nfail

  call MPI_Barrier(comm, ierr)
  if (isroot) call execute_command_line('rm -f '//trim(restartdir)//'*.MOM.res*.nc', wait=.true.)

  call ESMF_Finalize(rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Finalize")

  if (restntests%nfail > 0) then
     print '(A)','Test failures! '
     stop 1
  endif

contains

  !> Confirm track_restn builds the expected filename for a given part,
  !! matching restart_part_fname's own naming convention directly (not a
  !! second, independent formula -- same base string, same helper).
  function expected_fname(hour, part_index) result(fname)
    integer, intent(in) :: hour
    integer, intent(in) :: part_index
    character(len=256) :: fname
    type(ESMF_Time) :: nextTime
    integer :: yr, mon, day, hr, minute, sec, rc
    character(len=15) :: base_timestr
    character(len=256) :: base

    call ESMF_TimeSet(nextTime, yy=base_yy, mm=base_mm, dd=base_dd, h=hour, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeSet(nextTime)")
    call ESMF_TimeGet(nextTime, yy=yr, mm=mon, dd=day, h=hr, m=minute, s=sec, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeGet(nextTime)")
    write(base_timestr,'(I4.4,2(I2.2),A,3(I2.2))') yr, mon, day, ".", hr, minute, sec
    base = trim(restartdir)//trim(base_timestr)//'.MOM.res'
    fname = restart_part_fname(base, part_index)
  end function expected_fname

  !> Case 1: 3 parts, 2 complete, 1 not.
  subroutine check_partial(hour)
    integer, intent(in) :: hour
    integer, parameter :: num_rest_files = 3
    logical, parameter :: part_complete(num_rest_files) = [.true., .false., .true.]

    type(ESMF_Time) :: nextTime
    logical,             allocatable :: allDone(:)
    character(len=256), allocatable :: fnames(:)
    integer :: n, rc
    character(len=256) :: fname

    call ESMF_TimeSet(nextTime, yy=base_yy, mm=base_mm, dd=base_dd, h=hour, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeSet(nextTime)")

    do n = 1, num_rest_files
       fname = expected_fname(hour, n-1)
       if (isroot) call make_restart_fixture(fname, complete=part_complete(n))
    enddo
    call MPI_Barrier(comm, ierr)

    call track_restn(nextTime, num_rest_files, comm, isroot, rootpe, restartdir, allDone, fnames, rc)

    write(testname,'(A)') 'test track_restn: 3 parts, 2 complete/1 not -- rc'
    call assert_equal(rc, 0, testname, assertrc, assertmsg)
    call addresult(restntests, assertrc, trim(assertmsg), '')

    do n = 1, num_rest_files
       write(testname,'(A,I0,A)') 'test track_restn: 3 parts, 2 complete/1 not -- part ', n, ' fname'
       call assert_equal(trim(fnames(n))==trim(expected_fname(hour,n-1)), .true., &
            testname, assertrc, assertmsg)
       call addresult(restntests, assertrc, trim(assertmsg), '')

       write(testname,'(A,I0,A)') 'test track_restn: 3 parts, 2 complete/1 not -- part ', n, ' allDone'
       call assert_equal(allDone(n), part_complete(n), testname, assertrc, assertmsg)
       call addresult(restntests, assertrc, trim(assertmsg), '')
    enddo

    testname = 'test track_restn: 3 parts, 2 complete/1 not -- all(allDone) should be false'
    call assert_equal(all(allDone), .false., testname, assertrc, assertmsg)
    call addresult(restntests, assertrc, trim(assertmsg), '')
  end subroutine check_partial

  !> Case 2: 3 parts, all complete.
  subroutine check_all_complete(hour)
    integer, intent(in) :: hour
    integer, parameter :: num_rest_files = 3

    type(ESMF_Time) :: nextTime
    logical,             allocatable :: allDone(:)
    character(len=256), allocatable :: fnames(:)
    integer :: n, rc
    character(len=256) :: fname

    call ESMF_TimeSet(nextTime, yy=base_yy, mm=base_mm, dd=base_dd, h=hour, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeSet(nextTime)")

    do n = 1, num_rest_files
       fname = expected_fname(hour, n-1)
       if (isroot) call make_restart_fixture(fname, complete=.true.)
    enddo
    call MPI_Barrier(comm, ierr)

    call track_restn(nextTime, num_rest_files, comm, isroot, rootpe, restartdir, allDone, fnames, rc)

    testname = 'test track_restn: 3 parts, all complete -- rc'
    call assert_equal(rc, 0, testname, assertrc, assertmsg)
    call addresult(restntests, assertrc, trim(assertmsg), '')

    do n = 1, num_rest_files
       write(testname,'(A,I0,A)') 'test track_restn: 3 parts, all complete -- part ', n, ' allDone'
       call assert_equal(allDone(n), .true., testname, assertrc, assertmsg)
       call addresult(restntests, assertrc, trim(assertmsg), '')
    enddo

    testname = 'test track_restn: 3 parts, all complete -- all(allDone) should be true'
    call assert_equal(all(allDone), .true., testname, assertrc, assertmsg)
    call addresult(restntests, assertrc, trim(assertmsg), '')
  end subroutine check_all_complete

  !> Case 3: num_rest_files=1 boundary, both complete and incomplete.
  subroutine check_single_file(hour, complete)
    integer, intent(in) :: hour
    logical, intent(in) :: complete
    integer, parameter :: num_rest_files = 1

    type(ESMF_Time) :: nextTime
    logical,             allocatable :: allDone(:)
    character(len=256), allocatable :: fnames(:)
    integer :: rc
    character(len=256) :: fname
    character(len=16) :: tag

    call ESMF_TimeSet(nextTime, yy=base_yy, mm=base_mm, dd=base_dd, h=hour, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeSet(nextTime)")

    fname = expected_fname(hour, 0)
    if (isroot) call make_restart_fixture(fname, complete=complete)
    call MPI_Barrier(comm, ierr)

    call track_restn(nextTime, num_rest_files, comm, isroot, rootpe, restartdir, allDone, fnames, rc)

    if (complete) then
       tag = 'complete'
    else
       tag = 'incomplete'
    endif

    write(testname,'(A)') 'test track_restn: num_rest_files=1, '//trim(tag)//' -- rc'
    call assert_equal(rc, 0, testname, assertrc, assertmsg)
    call addresult(restntests, assertrc, trim(assertmsg), '')

    write(testname,'(A)') 'test track_restn: num_rest_files=1, '//trim(tag)//' -- allDone'
    call assert_equal(allDone(1), complete, testname, assertrc, assertmsg)
    call addresult(restntests, assertrc, trim(assertmsg), '')

    write(testname,'(A)') 'test track_restn: num_rest_files=1, '//trim(tag)//' -- all(allDone)'
    call assert_equal(all(allDone), complete, testname, assertrc, assertmsg)
    call addresult(restntests, assertrc, trim(assertmsg), '')
  end subroutine check_single_file

end program test_track_restn
