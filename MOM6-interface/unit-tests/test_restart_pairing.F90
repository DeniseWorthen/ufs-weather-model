!> Restart-pairing test for track_freqn/outputlog_run
!!
!! Verifies that state_n%time_lastrestart, and the real log_restart_fh
!! output's "last restart:" line, correctly reflect the most recent
!! restart-cadence point at each history completion. Restart files have NO
!! completion lag (confirmed with the feature owner: written complete,
!! instantly, at their own indicated hour -- unlike history files, which
!! have the familiar ~1-tick FMS lag) -- so lastrestart is driven directly
!! here as "the most recent restart_hours(:) entry reached so far," no
!! fixture files needed for restarts themselves.
!!
!! Reuses setup_case/handlefiles from outputlog_test_helpers (shared with
!! test_freqn) for all the real ESMF clock/alarm/cf_n/state_n setup and
!! history-fixture handling -- this file adds ONLY the restart-cadence
!! driving and the log-content verification on top.
!!
!! Since track_freqn (this refactor) no longer calls log_restart_fh itself
!! -- that now happens in outputlog_run, outside track_freqn -- this test
!! replicates that same call directly after track_freqn returns, exactly
!! matching outputlog_run's own logic (complog naming, argument list).
!!
!! Case 1 only (freq=6, start=6, restart_freq=15, runhours=36) -- case 2
!! (freq=24, start=0, restart_freq=15, runhours=50) still needs its own
!! diagram confirmed before being added.
!!
!> @date 08-28-2026
program test_restart_pairing

  use ESMF
  use mpi_f08,                only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD, MPI_Barrier
  use test_utils
  use mom_cap_outputlog,      only : track_freqn
  use mom_outputlog_methods,  only : outputlog_config_type, outputlog_state_type, outputlog_modeltime_type
  use mom_outputlog_methods,  only : get_timestr, get_importexport, set_toffset, get_file_state
  use outputlog_test_helpers, only : base_yy, base_mm, base_dd, setup_case, handlefiles
  use shr_is_restart_fh_mod,  only : log_restart_fh

  implicit none

  integer, parameter :: maxtests = 10

  type(MPI_Comm) :: comm
  integer        :: rank, ierr, rootpe
  logical        :: isroot

  character(len=128) :: testname
  character(len=256) :: assertmsg
  character(len=20)  :: subname = 'test_restart_pairing'
  character(len=256) :: outputdir = ''

  type(testsummary) :: pairingtests

  logical :: debug_onroot
  logical :: assertrc
  integer :: n
  logical :: verbose = .true.

  comm = MPI_COMM_WORLD
  rootpe = 0
  outputdir = "./"

  call pairingtests%init(maxtests)

  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  isroot = (rank == rootpe)
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Initialize")

  debug_onroot = verbose .and. isroot

  ! ------------------
  ! Case 1: freq=6, start=6, restart every 15h, run 36h -- confirmed
  ! against the user's own diagram. 6 real completions: 09/15/21/27 via
  ! ordinary mid-run polling, 33 (plain finalize) and 39 (lstop), both
  ! resolved at the same final tick. First completion (09) happens before
  ! the first restart ever occurs (21) -- lastrestart there should
  ! correctly show the model's original start time, not a restart point.
  ! ------------------
  testname = 'test 01 restart pairing, freq=6 start=6 restart=15h run=36h'
  call run_case(trim(testname), freq=6, start_hour=6, runhours=36, restart_freq=15, assertrc, assertmsg)
  call addresult(pairingtests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Test results
  ! ------------------
  if (pairingtests%nfail > 0) then
     print '(A)', 'FAIL: At least one test failed '
     do n = 1,pairingtests%count
        if (.not. pairingtests%teststatus(n)) print '(A)', trim(pairingtests%testmessage(n)%str)
     enddo
  else
     do n = 1,pairingtests%count
        print '(A)', trim(pairingtests%testmessage(n)%str)
     enddo
  endif
  print '(3(A,I0))','Total tests = ',pairingtests%count,' Passing = ',pairingtests%npass,' Failing = ',pairingtests%nfail

  call ESMF_Finalize(rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Finalize")

  if (pairingtests%nfail > 0) then
     print '(A)','Test failures! '
     stop 1
  endif

contains

  !> Drives track_freqn through one run, with a real ESMF clock/alarm and
  !! realistic FMS-lag history fixtures (matching test_freqn's own
  !! structure), while independently driving lastrestart per restart_freq
  !! (no lag -- restarts are complete the instant their own hour arrives).
  !! At every real completion, asserts state_n%time_lastrestart against an
  !! externally-derived expected value, then replicates outputlog_run's
  !! own log_restart_fh call and reads back the real resulting file to
  !! confirm its "last restart:" line matches too.
  subroutine run_case(test, freq, start_hour, runhours, restart_freq, is_passing, failmsg)

    character(len=*), intent(in)  :: test
    integer,           intent(in)  :: freq, start_hour, runhours, restart_freq
    logical,           intent(out) :: is_passing
    character(len=*),  intent(out) :: failmsg

    type(ESMF_Clock)         :: modelClock
    type(ESMF_Time)          :: startTime, stopTime
    type(ESMF_Time)          :: lastrestart
    type(ESMF_TimeInterval)  :: hourInterval, elapsedtime

    type(outputlog_config_type)    :: cf_n
    type(outputlog_state_type)     :: state_n
    type(outputlog_modeltime_type) :: modeltime

    integer :: ierr, rc
    integer :: count, minutes, elapsedhours, toffset
    integer :: restart_idx, n_restarts
    integer, allocatable :: restart_hours(:)
    type(ESMF_Time), allocatable :: restart_times(:)

    integer :: completion_count
    integer, parameter :: max_completions = 10
    type(ESMF_Time) :: expected_lastrestart(max_completions)

    logical :: phantom_file, lstop, pending
    logical :: found_firstcompletion

    character(len=16)  :: timestr, logfile
    character(len=40)  :: importexport
    character(len=256) :: logname
    character(len=256) :: readline

    integer :: logunit, ios
    integer :: yr, mon, day, hr, minute, sec

    is_passing = .true.
    failmsg = ''
    completion_count = 0
    pending = .false.
    found_firstcompletion = .false.

    if (isroot) call execute_command_line('rm -f '//trim(outputdir)//'*.nc '//trim(outputdir)//'*.mom6.*', &
         wait=.true.)
    call MPI_Barrier(comm, ierr)
    if (ierr /= 0) then
       write(0,'(A)') "FATAL ("//trim(subname)//"): MPI_Barrier (post-cleanup) failed"
       stop 99
    endif

    call setup_case(start_hour, runhours, freq, 1, 'average', debug_onroot, modelClock, cf_n, state_n, rc)
    call esmf_err(rc, subname, "setup_case")

    call ESMF_ClockGet(modelClock, startTime=startTime, stopTime=stopTime, rc=rc)
    call esmf_err(rc, subname, "ESMF_ClockGet(startTime,stopTime)")
    call ESMF_TimeIntervalSet(hourInterval, h=1, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeIntervalSet(hourInterval)")
    call ESMF_TimeIntervalSet(modeltime%tincrement, m=1, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeIntervalSet(tincrement)")

    ! --- Restart cadence, elapsed hours from start, driven with NO lag
    ! (confirmed: restarts are complete instantly at their own hour).
    n_restarts = runhours/restart_freq   ! how many cadence points fall within the run
    allocate(restart_hours(n_restarts), restart_times(n_restarts))
    do n = 1, n_restarts
       restart_hours(n) = n*restart_freq
       restart_times(n) = startTime + restart_hours(n)*hourInterval
    enddo
    restart_idx  = 0
    lastrestart  = startTime   ! matches outputlog_init's own initialization

    ! --- Expected lastrestart per completion, confirmed against the
    ! user's own diagram for this exact case (freq=6,start=6,restart=15,
    ! run=36): completions 1 (09) happens before any restart -- expects
    ! the original start time. Completions 2-3 (15,21) expect restart_times(1)
    ! (=21). Completions 4-6 (27,33,39) expect restart_times(2) (=36).
    expected_lastrestart(1) = startTime
    expected_lastrestart(2) = restart_times(1)
    expected_lastrestart(3) = restart_times(1)
    expected_lastrestart(4) = restart_times(2)
    expected_lastrestart(5) = restart_times(2)
    expected_lastrestart(6) = restart_times(2)

    do while (.not. ESMF_ClockIsStopTime(modelClock, rc=rc))
       call esmf_err(rc, subname, "advance to stop time")

       call ESMF_ClockGet(modelClock, startTime=modeltime%startTime, currTime=modeltime%currTime, rc=rc)
       call esmf_err(rc, subname, "get start and currTime")
       call ESMF_ClockGetNextTime(modelClock, modeltime%nextTime, rc=rc)
       call esmf_err(rc, subname, "get nextTime")
       importexport = get_importexport(modeltime%currTime, modeltime%nextTime, rc=rc)
       call esmf_err(rc, subname, "get importexport")

       state_n%ringing = .false.
       count = 0
       call ESMF_ClockAdvance(modelClock, ringingalarmcount=count, rc=rc)
       call esmf_err(rc, subname, "ringing alarm count")

       if (count > 0) then
          state_n%ringing = .true.
          call ESMF_AlarmRingerOff(cf_n%alarm, rc=rc)
          call esmf_err(rc, subname, "alarm ringer off")
       endif
       call ESMF_AlarmGet(cf_n%alarm, prevRingTime=state_n%prevring, rc=rc)
       call esmf_err(rc, subname, "get prevRing")

       lstop = (modeltime%nextTime == stopTime)

       ! --- Drive lastrestart: no lag, so update the MOMENT elapsed time
       ! reaches (or passes) the next cadence point.
       elapsedtime = modeltime%nextTime - startTime
       call ESMF_TimeIntervalGet(elapsedtime, m=minutes, rc=rc)
       call esmf_err(rc, subname, "get elapsedtime for restart cadence")
       elapsedhours = minutes/60
       if (restart_idx < n_restarts) then
          if (elapsedhours >= restart_hours(restart_idx+1)) then
             restart_idx = restart_idx + 1
             lastrestart = restart_times(restart_idx)
          endif
       endif

       ! ======================================================================
       ! set up file states to mimic FMS (identical to test_freqn's own logic)
       ! ======================================================================
       if (pending .and. len_trim(state_n%filename)>0) then
          call handlefiles(isroot, state_n%filename, .false., 'complete')
          pending = .false.
       endif

       if (state_n%ringing) then
          phantom_file = .false.
          found_firstcompletion = .false.
          timestr = get_timestr(modeltime%nextTime - cf_n%filename_fhoffset, rc=rc)
          if (modeltime%nextTime - cf_n%filename_fhoffset <= modeltime%startTime) phantom_file = .true.

          state_n%filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
               //trim(cf_n%fnamesuffix)

          if (phantom_file) then
             pending = .false.
          else
             call handlefiles(isroot, state_n%filename, .false., 'create')
             pending = .true.
          endif
       endif

       call track_freqn(modeltime, cf_n, state_n, comm, isroot, rootpe, outputdir, lastrestart, &
            debug_onroot, .false., rc)
       call esmf_err(rc, subname, "track_freqn (main loop)")
       if (state_n%filecomplete .and. .not.found_firstcompletion) then
          found_firstcompletion = .true.
          write(logfile,'(A,I2.2,A)') 'mom6.',cf_n%opt_n,'h'
          call verify_completion(logfile)
          if (.not. is_passing) return
       endif

       ! --- plain finalize: resolves whatever's still pending
       if (modeltime%nextTime == stopTime) then
          if (pending) then
             call handlefiles(isroot, state_n%filename, .false., 'complete')
             pending = .false.
             found_firstcompletion = .false.

             state_n%ringing = .false.
             state_n%chkfile_nextAdvance = .true.
             call track_freqn(modeltime, cf_n, state_n, comm, isroot, rootpe, outputdir, lastrestart, &
                  debug_onroot, .false., rc)
             call esmf_err(rc, subname, "track_freqn (plain finalize)")
             if (state_n%filecomplete .and. .not.found_firstcompletion) then
                found_firstcompletion = .true.
                write(logfile,'(A,I2.2,A)') 'mom6.',cf_n%opt_n,'h'
                call verify_completion(logfile)
                if (.not. is_passing) return
             endif
          endif
       endif

       ! --- lstop: currently-open interval, no closing ring of its own
       toffset = set_toffset(start_hour, freq)
       elapsedtime = modeltime%nextTime - modeltime%startTime
       call ESMF_TimeIntervalGet(elapsedtime, m=minutes, rc=rc)
       call esmf_err(rc, subname, "get elapsedtime at lstop")
       elapsedhours = toffset + minutes/60

       if (modeltime%nextTime == stopTime) then
          if (mod(elapsedhours,freq) == 0) then
             timestr = get_timestr(state_n%prevring-30*cf_n%opt_n*modeltime%tincrement, rc=rc)
             state_n%filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
                  //trim(cf_n%fnamesuffix)
             call handlefiles(isroot, state_n%filename, .false., 'create-complete')
          endif

          state_n%ringing = .false.
          state_n%chkfile_nextAdvance = .false.
          found_firstcompletion = .false.
          call track_freqn(modeltime, cf_n, state_n, comm, isroot, rootpe, outputdir, lastrestart, &
               debug_onroot, .true., rc)
          call esmf_err(rc, subname, "track_freqn (lstop)")
          if (state_n%filecomplete .and. .not.found_firstcompletion) then
             found_firstcompletion = .true.
             write(logfile,'(A,I2.2,A)') 'mom6.lstop.',cf_n%opt_n,'h'
             call verify_completion(logfile)
             if (.not. is_passing) return
          endif
       endif

    enddo ! do while

    if (completion_count /= 6) then
       is_passing = .false.
       write(failmsg,'(A,I0,A)') 'Fail: '//trim(test)//' | expected 6 completions, got ', completion_count, ''
    endif

  contains

    !> Called once per real completion: replicates outputlog_run's own
    !! log_restart_fh call, then verifies both state_n%time_lastrestart
    !! and the real resulting log file's "last restart:" line against the
    !! externally-derived expected value for this completion.
    subroutine verify_completion(complog)
      character(len=*), intent(in) :: complog

      completion_count = completion_count + 1
      if (completion_count > max_completions) then
         is_passing = .false.
         failmsg = 'Fail: '//trim(test)//' | more completions than expected'
         return
      endif

      if (isroot) then
         call log_restart_fh(state_n%time_logfile, startTime, complog=trim(complog), &
              prefixtime=.true., lastrestart=state_n%time_lastrestart, lastoutput=state_n%filename, rc=rc)
         call esmf_err(rc, subname, "log_restart_fh")
      endif

      ! --- state_n%time_lastrestart, against the externally-derived
      ! expected value for this completion (not re-derived from
      ! production's own logic -- driven independently above from
      ! restart_hours(:)/restart_times(:)).
      if (state_n%time_lastrestart /= expected_lastrestart(completion_count)) then
         is_passing = .false.
         write(failmsg,'(A,I0,A)') 'Fail: '//trim(test)//' | completion ', completion_count, &
              ' state_n%time_lastrestart does not match expected'
         return
      endif

      ! --- Real log file content, read back directly (matches
      ! test_outputlog_finalize.F90's established pattern: predict the
      ! filename/expected lines using the SAME format specifiers
      ! log_restart_fh itself uses, applied to values already known here).
      call ESMF_TimeGet(state_n%time_logfile, yy=yr, mm=mon, dd=day, h=hr, m=minute, s=sec, rc=rc)
      call esmf_err(rc, subname, "ESMF_TimeGet(time_logfile)")
      write(logname,'(i4.4,2(i2.2),A,3(i2.2),A)') yr, mon, day, '.', hr, minute, sec, '.'//trim(complog)

      open(newunit=logunit, file=trim(logname), status='old', action='read', iostat=ios)
      if (ios /= 0) then
         is_passing = .false.
         write(failmsg,'(A,I0,A)') 'Fail: '//trim(test)//' | completion ', completion_count, &
              ' expected log file not found: '//trim(logname)
         return
      endif

      ! last restart: <lastrestart, 6i8> -- skip completed:/forecast hour:/valid time:/last output:
      read(logunit,'(a)',iostat=ios) readline
      read(logunit,'(a)',iostat=ios) readline
      read(logunit,'(a)',iostat=ios) readline
      read(logunit,'(a)',iostat=ios) readline
      call ESMF_TimeGet(expected_lastrestart(completion_count), yy=yr, mm=mon, dd=day, h=hr, m=minute, s=sec, rc=rc)
      call esmf_err(rc, subname, "ESMF_TimeGet(expected_lastrestart)")
      write(logname,'(a,6i8)') 'last restart: ', yr, mon, day, hr, minute, sec   ! reuse logname as scratch
      read(logunit,'(a)',iostat=ios) readline
      close(logunit)

      if (ios /= 0 .or. trim(readline) /= trim(logname)) then
         is_passing = .false.
         write(failmsg,'(A,I0,A)') 'Fail: '//trim(test)//' | completion ', completion_count, &
              ' log file last restart: line does not match expected'
         return
      endif
    end subroutine verify_completion

  end subroutine run_case

end program test_restart_pairing
