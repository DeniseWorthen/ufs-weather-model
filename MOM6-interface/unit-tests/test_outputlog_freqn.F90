!> Narrow orchestration test for the outputlog_freqn machinery
!!
!! Ring timing (AlarmInit/set_toffset) is validated in test_alarminit.F90.
!! Completion criteria (file_is_complete/get_file_state) are validated in
!! test_outputlog_completion.F90. Given both are already independently
!! trusted, this file's job is to confirm get_ring_state and
!! check_file_completion correctly coordinate through state_n:
!!   1. at a ring, computes the right filename (the nextTime-
!!      filename_fhoffset lookback) and correctly initializes state_n from
!!      a real check of that file
!!   2. correctly reports completion by genuinely polling the real fixture
!!      (driving the real code, not re-deriving completion logic)
!!   3. when a second ring fires, correctly re-initializes and starts
!!      tracking the new file, superseding whatever it was tracking before
!!
!! DESIGN NOTE: this file calls get_ring_state/check_file_completion
!! DIRECTLY -- not outputlog_freqn itself. outputlog_freqn is now just a
!! thin dispatcher (check ringing, call get_ring_state if so, always call
!! check_file_completion) with no real logic of its own left to test; all
!! the actual state-tracking/completion/supersession behavior lives in the
!! two routines this file exercises directly. Since get_ring_state takes a
!! plain nextTime (not a clock), and check_file_completion needs no
!! clock/alarm at all, this file needs NO real advancing ESMF_Clock and NO
!! alarm-ringing detection -- "this is a ring" is simply asserted by each
!! case's own ring_hours(:) data, not derived from ESMF. Ring TIMING
!! correctness itself is out of scope here (owned by test_alarminit.F90);
!! ring_hours(:) values below are literals matching what's already been
!! independently confirmed correct elsewhere, not re-derived here.
!!
!! Explicitly OUT OF SCOPE here, left to their own dedicated tests:
!!   - restart pairing (state_n%time_lastrestart) -- lastrestart is a fixed
!!     dummy value below, never asserted on
!!   - finalize behavior (both the plain and lstop-specific finalize calls)
!!     -- get_lstop_ring_state is never called here; see
!!     test_outputlog_finalize.F90
!!
!! Coverage matrix (timereduce x use_filesize) is deliberately complete,
!! not just DATM: ATM (use_filesize=.true.) is the operational
!! configuration; DATM is a real code path needing correctness coverage but
!! isn't what's actually deployed. Cases 1-4 (DATM) each have an ATM
!! sibling (5, 6, 7, 8) rather than being inferred as "probably fine" from
!! one config alone -- notably case 7, where createsize is functionally
!! inert under DATM (only nlen matters) but load-bearing under ATM,
!! meaning DATM's version of that case cannot catch a stale-createsize-on-
!! supersession bug even in principle.
!!
!> @date 08-12-2026
program test_outputlog_freqn

  use ESMF
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD, MPI_Barrier
  use test_utils
  use mom_outputlog_methods, only : outputlog_config_type, outputlog_state_type, get_timestr
  use mom_outputlog_methods, only : get_ring_state, check_file_completion
  use MOM_cap_time,          only : AlarmInit
  use nc_fixture_mod,        only : create_schema, write_record, write_padding, write_bulk_data

  implicit none

  type(MPI_Comm) :: comm
  integer        :: rank, ierr, rootpe
  logical        :: isroot
  integer, parameter :: base_yy = 2021, base_mm = 3, base_dd = 22
  integer, parameter :: maxtests = 10

  character(len=128) :: testname
  character(len=256) :: assertmsg
  character(len=20)  :: subname = 'test_outputlog_freqn'

  type(testsummary) :: freqntests

  logical :: is_passing, assertrc
  integer :: n
  logical :: verbose = .false.

  comm = MPI_COMM_WORLD
  rootpe = 0

  call freqntests%init(maxtests)

  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  isroot = (rank == rootpe)
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Initialize")

  ! ------------------
  ! Case 1: single ring closing a REAL (non-phantom) window -- ring at
  ! hour 12 closes the phantom [0,6] window (predates model start=6, never
  ! completes); ring at hour 18 closes the real [6,12] window, and does
  ! complete. Literals confirmed via the earlier orchestration-test work.
  ! ------------------
  testname = 'single ring, real window, completes correctly'
  call run_case(freq=6, start_hour=6, ring_hours=[12,18], ring_ticks=[-1,1], &
       expected_completions=1, is_passing=is_passing)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 2: single ring, fixture never completes -- confirms
  ! check_file_completion correctly keeps reporting not-complete rather
  ! than falsely completing or erroring.
  ! ------------------
  testname = 'test 02 single ring never completes'
  call run_case(freq=6, start_hour=6, ring_hours=[12], ring_ticks=[-1], &
       expected_completions=0, is_passing=is_passing)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 3: two rings -- the first never completes, the second does.
  ! Confirms the second ring's tracking correctly supersedes the first
  ! (state_n re-initialized to the new file) rather than the still-
  ! incomplete first file blocking or double-counting anything.
  ! ------------------
  testname = 'test 03 second ring supersedes first'
  call run_case(freq=6, start_hour=0, ring_hours=[6,12], ring_ticks=[-1,1], &
       expected_completions=1, is_passing=is_passing)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 4: single ring, snapshot ('none') type -- exercises the 1x
  ! (60*freq) filename lookback, distinct from 'average's 1.5x (90*freq).
  ! ------------------
  testname = 'test 04 single ring snapshot (none) type completes correctly'
  call run_case(freq=6, start_hour=6, ring_hours=[12,18], ring_ticks=[-1,1], &
       expected_completions=1, is_passing=is_passing, timereduce='none')
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 5: single ring, ATM-style fixture (nlen=1 immediately at ring
  ! time, completion via fsize growth instead of a nlen flip) -- exercises
  ! get_ring_state's own use_filesize=.true. INFERENCE from a real ring-
  ! time check, distinct from test_outputlog_completion.F90 (which only
  ! tests file_is_complete given use_filesize already known as an input).
  ! ------------------
  testname = 'test 05 single ring ATM-style (use_filesize) completes correctly'
  call run_case(freq=6, start_hour=6, ring_hours=[12,18], ring_ticks=[-1,1], &
       expected_completions=1, is_passing=is_passing, use_filesize=.true.)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 6: ATM sibling of case 2 -- never completes, but under the
  ! use_filesize=.true. scheme (fsize stuck at createsize forever, never
  ! growing) rather than DATM's nlen-stuck-at-0. Added deliberately: ATM is
  ! the operational configuration; DATM coverage alone isn't sufficient
  ! evidence this mechanism works under what's actually running.
  ! ------------------
  testname = 'test 06 single ring never completes (ATM-style)'
  call run_case(freq=6, start_hour=6, ring_hours=[12], ring_ticks=[-1], &
       expected_completions=0, is_passing=is_passing, use_filesize=.true.)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 7: ATM sibling of case 3 -- supersession, under use_filesize=.true.
  ! This is the one case where DATM structurally CANNOT stand in for ATM:
  ! createsize is functionally inert under DATM (completion only checks
  ! nlen), but load-bearing under ATM (fsize > createsize IS the check).
  ! Confirms get_ring_state correctly resets createsize to ring 2's own
  ! value when superseding ring 1's tracking, and that check_file_completion
  ! then correctly evaluates fsize against the NEW createsize, not a stale
  ! one left over from ring 1.
  ! ------------------
  testname = 'test 07 second ring supersedes first (ATM-style)'
  call run_case(freq=6, start_hour=0, ring_hours=[6,12], ring_ticks=[-1,1], &
       expected_completions=1, is_passing=is_passing, use_filesize=.true.)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 8: ATM sibling of case 4 -- snapshot ('none') type under
  ! use_filesize=.true., completing the {average,none} x {DATM,ATM} matrix.
  ! This combination (snapshot + active ATM) is the real operational case
  ! that originally motivated this refactor (per user, alongside
  ! average+freq=6) -- the specific frequency doesn't matter here (ring
  ! timing itself is exhaustively covered elsewhere, across all valid
  ! frequencies, by test_alarminit.F90); what matters is that this
  ! timereduce/use_filesize COMBINATION is directly exercised, not merely
  ! inferred from testing each half separately.
  ! ------------------
  testname = 'test 08 single ring snapshot (none) type, ATM-style'
  call run_case(freq=6, start_hour=6, ring_hours=[12,18], ring_ticks=[-1,1], &
       expected_completions=1, is_passing=is_passing, timereduce='none', use_filesize=.true.)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Test results
  ! ------------------
  if (freqntests%nfail > 0) then
     print '(A)', 'FAIL: At least one test failed '
     do n = 1,freqntests%count
        if (.not. freqntests%teststatus(n)) print '(A)', trim(freqntests%testmessage(n)%str)
     enddo
  else
     do n = 1,freqntests%count
        print '(A)', trim(freqntests%testmessage(n)%str)
     enddo
  endif
  print '(3(A,I0))','Total tests = ',freqntests%count,' Passing = ',freqntests%npass,' Failing = ',freqntests%nfail

  call ESMF_Finalize(rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Finalize")

  if (freqntests%nfail > 0) stop 1

contains

  !> Drives get_ring_state/check_file_completion through a sequence of
  !! abstract "hours" (no real clock advances -- this is just an integer
  !! counter, matched against ring_hours(:) to decide when a ring
  !! "happens"). ring_ticks(i) controls when ring_hours(i)'s fixture
  !! completes: a positive N schedules the fixture's completing write N
  !! hours after that ring; -1 means never. Counts how many completions
  !! are reported and compares to expected_completions.
  subroutine run_case(freq, start_hour, ring_hours, ring_ticks, expected_completions, is_passing, &
       timereduce, use_filesize)
    integer, intent(in)  :: freq, start_hour
    integer, intent(in)  :: ring_hours(:)
    integer, intent(in)  :: ring_ticks(:)
    integer, intent(in)  :: expected_completions
    logical, intent(out) :: is_passing
    character(len=*), optional, intent(in) :: timereduce
    logical,          optional, intent(in) :: use_filesize

    character(len=7) :: this_timereduce
    logical          :: this_use_filesize

    type(ESMF_Clock)         :: clock
    type(ESMF_Time)          :: startTime, nextTime, lastrestart
    type(ESMF_TimeInterval)  :: timeStep, tincrement, hourInterval
    type(outputlog_config_type) :: cf_n
    type(outputlog_state_type)  :: state_n

    integer :: ierr, rc
    integer :: hour, max_hour, ring_index
    integer :: num_completions
    logical :: filecomplete
    character(len=16)  :: timestr
    character(len=256) :: ring_filename, outputdir
    character(len=3)   :: chour

    type :: pending_transition_type
       character(len=256) :: filename = ""
       integer :: target_hour = -1
       logical :: active = .false.
    end type pending_transition_type
    type(pending_transition_type) :: pending(size(ring_hours))
    integer :: n_pending, i

    outputdir = "./"
    num_completions = 0
    n_pending = 0

    this_timereduce = 'average'
    if (present(timereduce)) this_timereduce = timereduce
    this_use_filesize = .false.
    if (present(use_filesize)) this_use_filesize = use_filesize

    ! Fixture files (*.nc) AND log_restart_fh's real output (a genuine side
    ! effect of check_file_completion whenever a fixture completes -- format
    ! YYYYMMDD.HHMMSS.mom6.<chour>, no .nc extension) from a prior case must
    ! not leak into this one.
    if (isroot) call execute_command_line('rm -f '//trim(outputdir)//'*.nc '//trim(outputdir)//'*.mom6.*', &
         wait=.true.)
    call MPI_Barrier(comm, ierr)
    if (ierr /= 0) then
       write(0,'(A)') "FATAL ("//trim(subname)//"): MPI_Barrier (post-cleanup) failed"
       stop 99
    end if

    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeSet(startTime)")
    call ESMF_TimeIntervalSet(timeStep, s=1800, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalSet(timeStep)")
    call ESMF_TimeIntervalSet(tincrement, m=1, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalSet(tincrement)")
    call ESMF_TimeIntervalSet(hourInterval, h=1, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalSet(hourInterval)")

    ! Minimal clock + alarm, purely to get a VALID ESMF_Alarm handle for
    ! get_ring_state to call ESMF_AlarmRingerOff on -- NEVER advanced.
    ! Ring timing itself is out of scope here (owned by test_alarminit.F90);
    ! ring_hours(:) above are literals, not derived from this alarm at all.
    clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_ClockCreate")
    call AlarmInit(clock, alarm=cf_n%alarm, option='nhours', opt_n=freq, opt_ymd=-999, &
         RefTime=startTime, alarmname='test_alarm', rc=ierr)
    call esmf_err(ierr, subname, "AlarmInit")

    ! --- Build the rest of cf_n exactly as outputlog_init would ---
    cf_n%alarm_name        = "test_alarm"
    cf_n%opt_n             = freq
    cf_n%requested         = .true.
    cf_n%timereduce        = this_timereduce
    cf_n%fnameprefix       = "ocn_"
    cf_n%fnamesuffix       = ""
    if (trim(this_timereduce) == 'none') then
       cf_n%logname_fhoffset  = 0*tincrement
       cf_n%filename_fhoffset = 60*freq*tincrement
    else
       cf_n%logname_fhoffset  = 60*freq*tincrement
       cf_n%filename_fhoffset = 90*freq*tincrement
    end if

    state_n%chkfile_nextAdvance = .false.
    state_n%use_filesize        = .false.
    state_n%filename            = ""
    state_n%createsize          = 0

    write(chour,'(I2.2,A)') freq,'h'

    ! Restart pairing is out of scope for this file -- fixed dummy value,
    ! never asserted on. See the dedicated restart-pairing test.
    lastrestart = startTime

    max_hour = maxval(ring_hours) + maxval([ring_ticks, 1]) + 1

    ring_index = 0
    do hour = 1, max_hour
       nextTime = startTime + hour*hourInterval

       ring_index = findloc_int(ring_hours, hour)
       if (ring_index > 0) then
          timestr = get_timestr(nextTime - cf_n%filename_fhoffset, rc=ierr)
          call esmf_err(ierr, subname, "get_timestr")
          ring_filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
               //trim(cf_n%fnamesuffix)

          ! Place the fixture in its INCOMPLETE state before get_ring_state's
          ! own get_file_state call, so use_filesize is inferred correctly:
          ! write_padding leaves nlen=0 (DATM-style); write_record sets
          ! nlen=1 immediately (ATM-style), and also sets createsize.
          if (isroot) then
             call create_schema(trim(ring_filename))
             if (this_use_filesize) then
                call write_record(trim(ring_filename))
             else
                call write_padding(trim(ring_filename))
             end if
          end if

          call get_ring_state(nextTime, cf_n%alarm, cf_n, state_n, comm, isroot, rootpe, outputdir, rc)
          call esmf_err(rc, subname, "get_ring_state")

          ! Schedule this ring's completion transition, per ring_ticks.
          if (ring_ticks(ring_index) > 0) then
             n_pending = n_pending + 1
             pending(n_pending)%filename    = ring_filename
             pending(n_pending)%target_hour = hour + ring_ticks(ring_index)
             pending(n_pending)%active      = .true.
          end if
       end if

       ! Fire any pending transitions scheduled for THIS hour -- independent
       ! of whichever ring is currently tracked, so a still-pending write
       ! from a superseded ring still lands on disk (just never seen).
       do i = 1, n_pending
          if (pending(i)%active .and. pending(i)%target_hour == hour) then
             if (isroot) then
                if (this_use_filesize) then
                   call write_bulk_data(trim(pending(i)%filename))   ! fsize grows past createsize
                else
                   call write_record(trim(pending(i)%filename))      ! nlen 0->1
                end if
             end if
             pending(i)%active = .false.
          end if
       end do

       call check_file_completion(state_n, comm, isroot, rootpe, startTime, &
            nextTime - cf_n%logname_fhoffset, 'mom6.'//trim(chour), lastrestart, filecomplete, rc)
       call esmf_err(rc, subname, "check_file_completion")
       if (filecomplete) num_completions = num_completions + 1
    end do

    is_passing = (num_completions == expected_completions)
  end subroutine run_case

  !> Index of target in arr, or 0 if not present -- plain integer lookup,
  !! no ESMF/production logic involved.
  function findloc_int(arr, target) result(idx)
    integer, intent(in) :: arr(:)
    integer, intent(in) :: target
    integer :: idx
    integer :: k

    idx = 0
    do k = 1, size(arr)
       if (arr(k) == target) then
          idx = k
          return
       end if
    end do
  end function findloc_int

end program test_outputlog_freqn
