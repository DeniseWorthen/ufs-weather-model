!> Narrow orchestration test for outputlog_freqn
!!
!! Ring timing (AlarmInit/set_toffset) is validated in test_alarminit.F90.
!! Completion criteria (file_is_complete/get_file_state) are validated in
!! test_outputlog_completion.F90. Given both are already independently
!! trusted, this file's only job is to confirm outputlog_freqn correctly
!! WIRES them together:
!!   1. at ring time, computes the right filename (the nextTime-
!!      filename_fhoffset lookback) and correctly initializes state_n from
!!      a real check of that file
!!   2. correctly reports completion by genuinely polling the real fixture
!!      (driving the real code, not re-deriving completion logic)
!!   3. when a second ring fires, correctly re-initializes and starts
!!      tracking the new file, superseding whatever it was tracking before
!!
!! Explicitly OUT OF SCOPE here, left to their own dedicated tests:
!!   - restart pairing (state_n%time_lastrestart) -- lastrestart is a fixed
!!     dummy value below, never asserted on
!!   - lstop / finalize behavior -- outputlog_freqn is never called with
!!     atStopTime=.true. here
!!
!! Each ring's expected filename is computed independently only as
!! plumbing (to know where to place the fixture), using the same offset
!! formula + the real get_timestr production itself uses. This is not the
!! oracle: if the computed path were wrong, outputlog_freqn would look for
!! a different file, find nothing, and the test would fail loudly (wrong
!! completion count), not silently pass. The completion COUNT -- the actual
!! oracle -- is never independently recomputed; it's read directly from
!! outputlog_freqn's own filecomplete_out/filecomplete_lstop_out arguments.
!!
!> @date 08-08-2026
program test_outputlog_freqn

  use ESMF
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD, MPI_Barrier
  use test_utils
  use mom_outputlog_methods, only : outputlog_config_type, outputlog_state_type, get_timestr, set_toffset
  use mom_outputlog_methods, only : get_ring_state
  use MOM_cap_outputlog,     only : outputlog_freqn
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
  ! Case 1: single ring, fixture completes on the confirmed next tick
  ! ------------------
  testname = 'test 01 single ring completes correctly'
  call run_case(freq=6, start_hour=6, run_hours=9, ring_ticks=[1], &
       expected_completions=1, is_passing=is_passing)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 2: single ring, fixture never completes within the run -- confirms
  ! outputlog_freqn correctly keeps reporting not-complete rather than
  ! falsely completing or erroring.
  ! ------------------
  testname = 'test 02 single ring never completes'
  call run_case(freq=6, start_hour=6, run_hours=9, ring_ticks=[-1], &
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
  call run_case(freq=6, start_hour=0, run_hours=15, ring_ticks=[-1,1], &
       expected_completions=1, is_passing=is_passing)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 4: single ring, snapshot ('none') type -- exercises the 1x
  ! (60*freq) filename lookback, distinct from 'average's 1.5x (90*freq).
  ! This code path has never been exercised by this file before.
  ! ------------------
  testname = 'test 04 single ring snapshot (none) type completes correctly'
  call run_case(freq=6, start_hour=6, run_hours=9, ring_ticks=[1], &
       expected_completions=1, is_passing=is_passing, timereduce='none')
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 5: single ring, ATM-style fixture (nlen=1 immediately at ring
  ! time, completion via fsize growth instead of a nlen flip) -- exercises
  ! outputlog_freqn's own use_filesize=.true. INFERENCE from a real ring-
  ! time check, distinct from test_outputlog_completion.F90 (which only
  ! tests file_is_complete given use_filesize already known as an input).
  ! ------------------
  testname = 'test 05 single ring ATM-style (use_filesize) completes correctly'
  call run_case(freq=6, start_hour=6, run_hours=9, ring_ticks=[1], &
       expected_completions=1, is_passing=is_passing, use_filesize=.true.)
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

  !> Drives outputlog_freqn through one run. ring_ticks(ring_index) controls
  !! when that ring's fixture completes: a positive value N schedules the
  !! fixture's completing write N ticks after that ring fires; -1 means
  !! never scheduled at all. Any ring beyond size(ring_ticks) also never
  !! completes. Counts how many completions outputlog_freqn itself reports
  !! and compares to expected_completions.
  subroutine run_case(freq, start_hour, run_hours, ring_ticks, expected_completions, is_passing, &
       timereduce, use_filesize)
    integer, intent(in)  :: freq, start_hour, run_hours
    integer, intent(in)  :: ring_ticks(:)
    integer, intent(in)  :: expected_completions
    logical, intent(out) :: is_passing
    character(len=*), optional, intent(in) :: timereduce
    logical,          optional, intent(in) :: use_filesize

    character(len=7) :: this_timereduce
    logical          :: this_use_filesize

    type(ESMF_Clock)         :: clock
    type(ESMF_Time)          :: startTime, stopTime, nextTime, refTime, lastrestart
    type(ESMF_TimeInterval)  :: timeStep, runOffset, tincrement, alarmoffset
    type(outputlog_config_type) :: cf_n
    type(outputlog_state_type)  :: state_n
    integer :: toffset

    integer :: ierr, rc
    integer :: ring_index, absolute_tick, this_ticks
    integer :: num_completions
    logical :: ringing, filecomplete, filecomplete_lstop
    character(len=16)  :: timestr
    character(len=256) :: ring_filename, outputdir

    type :: pending_transition_type
       character(len=256) :: filename = ""
       integer :: target_tick = -1
       logical :: active = .false.
    end type pending_transition_type
    type(pending_transition_type) :: pending(size(ring_ticks))
    integer :: n_pending, i

    outputdir = "./"
    num_completions = 0
    ring_index = 0
    absolute_tick = 0
    n_pending = 0

    this_timereduce = 'average'
    if (present(timereduce)) this_timereduce = timereduce
    this_use_filesize = .false.
    if (present(use_filesize)) this_use_filesize = use_filesize

    ! Fixture files from a prior case must not leak into this one.
    if (isroot) call execute_command_line('rm -f '//trim(outputdir)//'*.nc', wait=.true.)
    call MPI_Barrier(comm, ierr)
    if (ierr /= 0) then
       write(0,'(A)') "FATAL ("//trim(subname)//"): MPI_Barrier (post-cleanup) failed"
       stop 99
    end if

    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeSet(startTime)")
    call ESMF_TimeIntervalSet(timeStep, s=1800, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalSet(timeStep)")
    call ESMF_TimeIntervalSet(runOffset, h=run_hours, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalSet(runOffset)")
    stopTime = startTime + runOffset

    clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_ClockCreate")

    call ESMF_TimeIntervalSet(tincrement, m=1, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalSet(tincrement)")

    ! Real production toffset -- not a reimplementation.
    toffset = set_toffset(start_hour, freq)
    alarmoffset = toffset*60*tincrement
    refTime = startTime + alarmoffset

    call AlarmInit(clock, alarm=cf_n%alarm, option='nhours', opt_n=freq, opt_ymd=-999, &
         RefTime=refTime, alarmname='test_alarm', rc=ierr)
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

    ! Restart pairing is out of scope for this file -- fixed dummy value,
    ! never asserted on. See the dedicated restart-pairing test.
    lastrestart = startTime

    do while (.not. ESMF_ClockIsStopTime(clock, rc=ierr))
       call esmf_err(ierr, subname, "ESMF_ClockIsStopTime")

       ! Check ring state BEFORE this tick's advance, using the same
       ! shared get_ring_state production itself calls -- confirmed
       ! against real PET-log output that mclock's currTime/nextTime stay
       ! FIXED for the whole duration of one ModelAdvance call (nothing
       ! inside the cap ever advances the clock itself; only the external
       ! NUOPC driver does, between calls). Advancing first and checking
       ! after (the original structure here) does not match that -- it
       ! silently computes a nextTime one full timestep later than
       ! production would, which only surfaced once test_outputlog_lstop.F90
       ! needed exact filename correctness rather than just self-consistent
       ! completion counts.
       call get_ring_state(clock, cf_n%alarm, ringing, nextTime, rc=ierr)
       call esmf_err(ierr, subname, "get_ring_state")

       if (ringing) then
          ring_index = ring_index + 1

          timestr = get_timestr(nextTime - cf_n%filename_fhoffset, rc=ierr)
          call esmf_err(ierr, subname, "get_timestr")
          ring_filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
               //trim(cf_n%fnamesuffix)

          ! Place the fixture in its INCOMPLETE state before outputlog_freqn's
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

          ! Schedule this ring's completion transition, per ring_ticks.
          if (ring_index <= size(ring_ticks)) then
             this_ticks = ring_ticks(ring_index)
          else
             this_ticks = -1
          end if
          if (this_ticks > 0) then
             n_pending = n_pending + 1
             pending(n_pending)%filename    = ring_filename
             pending(n_pending)%target_tick = absolute_tick + this_ticks
             pending(n_pending)%active      = .true.
          end if
       end if

       ! Fire any pending transitions scheduled for THIS tick -- independent
       ! of whichever ring is currently tracked, so a still-pending write
       ! from a superseded ring still lands on disk (just never seen).
       do i = 1, n_pending
          if (pending(i)%active .and. pending(i)%target_tick == absolute_tick) then
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

       call outputlog_freqn(clock, cf_n, state_n, comm, isroot, rootpe, outputdir, tincrement, &
            lastrestart, verbose, atStopTime=.false., rc=rc, &
            filecomplete_out=filecomplete, filecomplete_lstop_out=filecomplete_lstop)
       call esmf_err(rc, subname, "outputlog_freqn (main loop)")
       if (filecomplete) num_completions = num_completions + 1

       ! Advance LAST, preparing the clock for the next iteration.
       call ESMF_ClockAdvance(clock, rc=ierr)
       call esmf_err(ierr, subname, "ESMF_ClockAdvance")
       absolute_tick = absolute_tick + 1
    end do

    is_passing = (num_completions == expected_completions)
  end subroutine run_case

end program test_outputlog_freqn
