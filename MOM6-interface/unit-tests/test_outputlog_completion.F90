!> Test code for the outputlog file-completion contract
!!
!! Probes get_file_state / file_is_complete directly against real netCDF-4
!! fixture files (see nc_fixture_mod.F90), independent of the alarm/clock
!! machinery in outputlog_freqn -- this file only asks: given a file in a
!! known nlen/fsize state, does file_is_complete correctly report whether
!! it's complete?
!!
!! A test like this only has value if its notion of "correct" doesn't come
!! from the same place as the code being tested -- otherwise it just checks
!! that the code agrees with itself. Two things keep this one independent:
!!
!! 1. The fixtures are built with plain netCDF library calls (nf90_create,
!!    nf90_put_var, nf90_sync) -- never by calling file_is_complete or
!!    anything in mom_outputlog_methods. There's no shared code between the
!!    files under test and the function testing them.
!!
!! 2. The nlen/fsize completion contract itself predates this test, and
!!    predates file_is_complete's implementation even existing in a form
!!    anyone could read: it was discovered by hand, empirically, while the
!!    outputlog feature was first being built -- printing nlen during real
!!    DATM runs, then finding nlen alone insufficient for active ATM, which
!!    is specifically what led to tracking fsize too. file_is_complete's
!!    formula is downstream of that discovery, not the other way around.
!!    A second, separate confirmation of the same contract came from
!!    ncdump -c snapshots of real production files (a wholly different tool,
!!    sharing no code with either file_is_complete or these fixtures).
!!
!! So the fixtures here aren't constructed to match file_is_complete's own
!! logic -- they're constructed to match a pattern established independently
!! of it, twice over, and file_is_complete is checked against that.
!!
program test_outputlog_completion

  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD
  use mom_outputlog_methods, only : get_file_state, file_is_complete
  use nc_fixture_mod,        only : make_datm_incomplete, make_datm_complete
 use nc_fixture_mod,         only : make_atm_incomplete,  make_atm_complete

  implicit none

  type(MPI_Comm) :: comm
  integer        :: rank, ierr, rootpe
  logical        :: isroot
  integer        :: total_errors
  logical        :: verbose

  comm   = MPI_COMM_WORLD
  rootpe = 0
  total_errors = 0

  verbose = .true.

  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  isroot = (rank == rootpe)

  call check_datm_incomplete()
  call check_datm_complete()
  call check_atm_incomplete()
  call check_atm_complete()
  call check_file_does_not_exist()

  if (isroot) then
    print *, "========================================================"
    if (total_errors == 0) then
      print *, "SUCCESS: all completion-contract cases passed"
    else
      print *, "FAILURE: ", total_errors, " assertions failed"
    end if
    print *, "========================================================"
  end if

  call MPI_Finalize(ierr)
  if (total_errors == 0) then
    stop 0
  else
    stop 1
  end if

contains

  subroutine check_datm_incomplete()
    character(len=*), parameter :: fname = "test_datm_incomplete.nc"
    integer :: rc, nlen, fsize
    logical :: complete

    if (isroot) call make_datm_incomplete(fname)
    call get_file_state(comm, isroot, rootpe, fname, nlen=nlen, rc=rc)

    call assert_equal(0, rc, "DATM incomplete: get_file_state rc")
    call assert_equal(0, nlen, "DATM incomplete: nlen should be 0")

    complete = file_is_complete(comm, isroot, rootpe, fname, .false., 0, rc)
    if (isroot .and. verbose) print '(A,i6,A,L2)',fname//',  nlen = ',nlen,', fsize value ignored, complete = ',complete

    call assert_equal(0, rc, "DATM incomplete: file_is_complete rc")
    call assert_false(complete, "DATM incomplete: should NOT be complete (nlen=0)")
  end subroutine check_datm_incomplete

  subroutine check_datm_complete()
    character(len=*), parameter :: fname = "test_datm_complete.nc"
    integer :: rc, nlen, fsize
    logical :: complete

    if (isroot) call make_datm_complete(fname)

    call get_file_state(comm, isroot, rootpe, fname, nlen=nlen, fsize=fsize, rc=rc)

    call assert_equal(0, rc, "DATM complete: get_file_state rc")
    call assert_equal(1, nlen, "DATM complete: nlen should be 1")

    complete = file_is_complete(comm, isroot, rootpe, fname, .false., 0, rc)
    if (isroot .and. verbose) print '(A,i6,A,L2)',fname//',  nlen = ',nlen,', fsize value ignored, complete = ',complete

    call assert_equal(0, rc, "DATM complete: file_is_complete rc")
    call assert_true(complete, "DATM complete: SHOULD be complete (nlen=1, use_filesize=F)")
  end subroutine check_datm_complete

  subroutine check_atm_incomplete()
    character(len=*), parameter :: fname = "test_atm_incomplete.nc"
    integer :: rc, nlen, fsize, createsize
    logical :: complete

    createsize = 0
    if (isroot) call make_atm_incomplete(fname, createsize)

    call get_file_state(comm, isroot, rootpe, fname, nlen=nlen, fsize=fsize, rc=rc)

    call assert_equal(0, rc, "ATM incomplete: get_file_state rc")
    call assert_equal(1, nlen, "ATM incomplete: nlen should be 1 (record written)")
    call assert_equal(createsize, fsize, "ATM incomplete: fsize should equal createsize (no bulk data yet)")

    complete = file_is_complete(comm, isroot, rootpe, fname, .true., createsize, rc)
    if (isroot .and. verbose) print '(2(A,i6),A,L2)',fname//',  nlen = ',nlen,', fsize = ',fsize,', complete = ',complete

    call assert_equal(0, rc, "ATM incomplete: file_is_complete rc")
    call assert_false(complete, "ATM incomplete: should NOT be complete (nlen>0 but size==createsize)")
  end subroutine check_atm_incomplete

  subroutine check_atm_complete()
    character(len=*), parameter :: fname = "test_atm_complete.nc"
    integer :: rc, nlen, fsize, createsize
    logical :: complete

    createsize = 0
    if (isroot) call make_atm_complete(fname, createsize)

    call get_file_state(comm, isroot, rootpe, fname, nlen=nlen, fsize=fsize, rc=rc)

    call assert_equal(0, rc, "ATM complete: get_file_state rc")
    call assert_equal(1, nlen, "ATM complete: nlen should be 1")
    call assert_true(fsize > createsize, "ATM complete: fsize should exceed createsize (bulk data written)")

    complete = file_is_complete(comm, isroot, rootpe, fname, .true., createsize, rc)
    if (isroot .and. verbose) print '(2(A,i6),A,L2)',fname//',  nlen = ',nlen,', fsize = ',fsize,', complete = ',complete

    call assert_equal(0, rc, "ATM complete: file_is_complete rc")
    call assert_true(complete, "ATM complete: SHOULD be complete (nlen>0 and size>createsize)")
  end subroutine check_atm_complete

  !> Edge case: get_file_state/file_is_complete asked about a file that was
  !> never created. get_file_state's isroot-only inquire() leaves nlen/fsize
  !> at nf90_fill_int when the file doesn't exist -- confirm that propagates
  !> correctly and that file_is_complete treats it as incomplete either way.
  subroutine check_file_does_not_exist()
    character(len=*), parameter :: fname = "test_does_not_exist.nc"
    integer :: rc, nlen
    logical :: complete_datm, complete_atm

    call get_file_state(comm, isroot, rootpe, fname, nlen=nlen, rc=rc)
    call assert_equal(0, rc, "Nonexistent file: get_file_state rc")
    ! nf90_fill_int is a large negative sentinel -- assert it's NOT a
    ! plausible real record count, rather than hardcoding the exact constant
    call assert_true(nlen < 0, "Nonexistent file: nlen should be the fill-value sentinel, not a real count")

    complete_datm = file_is_complete(comm, isroot, rootpe, fname, .false., 0, rc)
    call assert_false(complete_datm, "Nonexistent file: use_filesize=F must not report complete")

    complete_atm = file_is_complete(comm, isroot, rootpe, fname, .true., 0, rc)
    call assert_false(complete_atm, "Nonexistent file: use_filesize=T must not report complete")
  end subroutine check_file_does_not_exist

  ! --- Assertion helpers ---
  subroutine assert_true(condition, msg)
    logical,          intent(in) :: condition
    character(len=*), intent(in) :: msg
    if (.not. condition .and. isroot) then
      print *, "  -> ASSERTION FAILED: ", trim(msg)
      total_errors = total_errors + 1
    end if
  end subroutine assert_true

  subroutine assert_false(condition, msg)
    logical,          intent(in) :: condition
    character(len=*), intent(in) :: msg
    if (condition .and. isroot) then
      print *, "  -> ASSERTION FAILED: ", trim(msg)
      total_errors = total_errors + 1
    end if
  end subroutine assert_false

  subroutine assert_equal(expected, actual, msg)
    integer,          intent(in) :: expected, actual
    character(len=*), intent(in) :: msg
    if (expected /= actual .and. isroot) then
      print *, "  -> ASSERTION FAILED: ", trim(msg), " (expected ", expected, ", got ", actual, ")"
      total_errors = total_errors + 1
    end if
  end subroutine assert_equal

end program test_outputlog_completion
