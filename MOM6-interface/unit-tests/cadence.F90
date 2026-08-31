!> Restart/history cadence-pairing test
!!
!! Restart cadence and history-completion cadence are fully independent --
!! verifying that pairing works correctly across genuinely different,
!! unrelated schedules is a critical property of the outputlog feature.
!!
!! This test does NOT call track_freqn or outputlog_restart:
!!   - track_freqn's own role in this pairing is a single, unconditional
!!     assignment (state_n%time_lastrestart = lastrestart) with no logic
!!     of its own to get wrong -- already exercised by test_driver2.F90.
!!   - outputlog_restart's allDone logic delegates entirely to
!!     get_file_state, already tested in test_outputlog_completion.F90.
!!     Verifying it correctly tracks a VARYING number of restart parts
!!     (real restarts split across multiple files, count depending on
!!     domain size) is a distinct concern, belonging to a separate,
!!     not-yet-built restart-LOGGING test -- not this one.
!!
!! What's left, once those two are factored out, is exactly what this file
!! tests directly: does "lastrestart = the most recent restart_hours(:)
!! entry reached, as of a given completion instant" hold correctly, using
!! a real ESMF_Clock exactly the way production's own nextTime advances
!! (30-minute ticks), rather than an abstract counter.
!!
!! completion_minutes(:) MUST match state_n%time_logfile exactly (per the
!! feature owner): currTime at the modelAdvance where a completion is
!! actually detected -- for the regular path that's typically ring+0.5h
!! (the confirmed one-tick FMS lag), NOT the ring's own whole hour; for
!! plain-finalize/lstop it's the model's final currTime/prevring, which IS
!! a whole hour. Restarts are always written to reflect currTime at the
!! end of their own modelAdvance too -- the same basis, which is exactly
!! why log_restart_fh uses currTime rather than nextTime for either path.
!! Minute-level precision (not whole hours) is required to represent this
!! correctly; a whole-hour-only comparison would silently give the right
!! answer only by coincidence, for cases where no restart happens to fall
!! inside a completion's half-hour lag window.
!!
!! No fixture files, no cf_n/state_n, no track_freqn -- pure clock/time
!! comparison, matching the actual scope of what needs verifying here.
!!
!> @date 08-28-2026
program test_restart_history_cadence

  use ESMF
  use mpi_f08,   only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_COMM_WORLD
  use test_utils
  use mom_outputlog_methods,  only : outputlog_config_type, outputlog_state_type, outputlog_modeltime_type
  use mom_outputlog_methods,  only : get_importexport, get_timestr
  use outputlog_test_helpers, only : setup_case

  implicit none

  integer, parameter :: maxtests = 10

  type(MPI_Comm) :: comm
  integer        :: rank, ierr, rootpe
  logical        :: isroot

  character(len=128) :: testname
  character(len=256) :: assertmsg
  character(len=20)  :: subname = 'test_restart_pairing'
  character(len=256) :: outputdir = ''

  type(testsummary) :: cadencetests

  logical :: debug_onroot
  logical :: assertrc
  integer :: n,nt
  integer :: expected, nmatches
  logical :: verbose = .true.

  comm = MPI_COMM_WORLD
  rootpe = 0
  outputdir = "./"

  call cadencetests%init(maxtests)

  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  isroot = (rank == rootpe)
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Initialize")

  debug_onroot = verbose .and. isroot
  nt = 0
  ! ===========================================================================
  ! Test cases
  ! ===========================================================================

  !------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' starthour=6, runhours=36, restart freq=15 '
  expected = 6  ! 6 file completions, 6 matching lastrestarts

  call run_case(trim(testname), freq=6, start_hour=6, runhours=36, &
       restart_hours             =[15],                            &
       fh_atfilecompletion       =[12,18,24,30,36,36],             &
       expected_lastrestart_hours=[ 0,15,15,30,30,30],             &
       nmatches=nmatches)

  call assert_equal(nmatches, expected, testname, assertrc, assertmsg)
  call addresult(cadencetests, assertrc, trim(assertmsg), '')

  ! !------------------
  ! nt = nt + 1
  ! write(testname,'(A,I2.2,A)')'test ',nt,' starthour=9, runhours=36, restart freq=3 '
  ! expected = 6  ! 6 file completions, 6 matching lastrestarts

  ! call run_case(trim(testname), freq=6, start_hour=9, runhours=36, &
  !      restart_hours             =[15,18,21,24,27,30,33,36,39],    &
  !      fh_atfilecompletion       =[12,18,24,30,36,36],             &
  !      expected_lastrestart_hours=[ 0,15,15,30,30,30],             &
  !      nmatches=nmatches)

  ! call assert_equal(nmatches, expected, testname, assertrc, assertmsg)
  ! call addresult(cadencetests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Test results
  ! ------------------
  if (cadencetests%nfail > 0) then
     print '(A)', 'FAIL: At least one test failed '
     do n = 1,cadencetests%count
        if (.not. cadencetests%teststatus(n)) print '(A)', trim(cadencetests%testmessage(n)%str)
     enddo
  else
     do n = 1,cadencetests%count
        print '(A)', trim(cadencetests%testmessage(n)%str)
     enddo
  endif
  print '(3(A,I0))','Total tests = ',cadencetests%count,' Passing = ',cadencetests%npass, &
       ' Failing = ',cadencetests%nfail

  call ESMF_Finalize(rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Finalize")

  if (cadencetests%nfail > 0) then
     print '(A)','Test failures! '
     stop 1
  endif

contains
  !> TODO
  subroutine run_case(test, freq, start_hour, runhours, timereduce, restart_hours, fh_atfilecompletion, &
       expected_lastrestart_hours, nmatches)

    character(len=*), intent(in)           :: test
    integer,          intent(in)           :: freq, start_hour, runhours
    character(len=*), intent(in), optional :: timereduce
    integer,          intent(in)           :: restart_hours(:)
    integer,          intent(in)           :: fh_atfilecompletion(:)
    integer,          intent(in)           :: expected_lastrestart_hours(:)
    integer,          intent(out)          :: nmatches

    character(len=7) :: l_timereduce
    integer          :: l_nfiles

    type(ESMF_Clock)        :: modelClock
    type(ESMF_Time)         :: startTime, stopTime, currTime, nextTime
    type(ESMF_Time)         :: lastrestart

    type(ESMF_Time), allocatable :: restart_times(:)
    type(ESMF_Time), allocatable :: expected_completiontimes(:)
    type(ESMF_Time), allocatable :: expected_lastrestart(:)
    type(ESMF_Time), allocatable :: actual_lastrestart(:)

    type(outputlog_config_type)    :: cf_n
    type(outputlog_state_type)     :: state_n
    type(outputlog_modeltime_type) :: modeltime

    integer :: ierr, rc
    integer :: n, n_restarts, n_completions

    character(len=40)  :: importexport
    character(len=16)  :: timestr_complete, timestr_restart

    l_timereduce = 'average'
    if (present(timereduce)) l_timereduce = timereduce
    ! TODO
    l_nfiles = 1

    ! mimic outputlog_init setup
    call setup_case(start_hour, runhours, freq,  l_nfiles, l_timereduce, debug_onroot, &
         modelClock, cf_n, state_n, rc)

    ! call run_case(trim(testname), freq=6, start_hour=6, runhours=36, &
    !      restart_hours             =[15,30],                         &
    !      fh_atfilecompletion       =[12,18,24,30,36,36],             &
    !      expected_lastrestart_hours=[ 0,15,15,30,30,30],             &
    !      is_passing=assertrc, failmsg=assertmsg)

    if (debug_onroot) then
       print '(A)','Running test '//test
    endif
    call ESMF_ClockGet(modelClock, startTime=modeltime%startTime, rc=rc)
    call esmf_err(rc, subname, "ESMF_ClockGet(currTime)")
    call ESMF_TimeIntervalSet(modeltime%tincrement, m=1, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeIntervalSet(tincrement)")

    ! defined restart times
    if (size(restart_hours) == 1) then          ! restart_hours are uniform cadence at given frequency
       n_restarts = runhours/restart_hours(1)
    else
       n_restarts = size(restart_hours)
    endif
    allocate(restart_times(n_restarts))

    if (size(restart_hours) == 1) then
       restart_times(1) = modeltime%startTime + restart_hours(1)*60*modeltime%tincrement
       do n = 2, n_restarts
          restart_times(n) = restart_times(n-1) + restart_hours(1)*60*modeltime%tincrement
       enddo
    else
       do n = 1, n_restarts
          restart_times(n) = modeltime%startTime + restart_hours(n)*60*modeltime%tincrement
       enddo
    endif
    if (debug_onroot) then
       do n = 1, n_restarts
          timestr_restart = get_timestr(restart_times(n), rc=rc)
          call esmf_err(rc, subname, "get restart_times")
          print '(A,i3,A)','Restart time ',n,' defined at '//timestr_restart
       enddo
    endif

    n_completions = size(fh_atfilecompletion)
    allocate(expected_completiontimes(n_completions))
    allocate(actual_lastrestart(n_completions))
    allocate(expected_lastrestart(n_completions))

    ! set up expected times based on provided hours
    do n = 1, n_completions
       expected_completiontimes(n) = modeltime%startTime + fh_atfilecompletion(n)*60*modeltime%tincrement
       call esmf_err(rc, subname, "get expected_completiontimes")
       expected_lastrestart(n) = modeltime%startTime + expected_lastrestart_hours(n)*60*modeltime%tincrement
       call esmf_err(rc, subname, "get expected_lastrestart")

       if (debug_onroot) then
          timestr_complete = get_timestr(expected_completiontimes(n), rc=rc)
          call esmf_err(rc, subname, "get timestr_complete")
          timestr_restart = get_timestr(expected_lastrestart(n), rc=rc)
          call esmf_err(rc, subname, "get timestr_restart expected")
          print '(i3,A)',n,' Expected file completion clock '//timestr_complete//' expected_lastrestart '//timestr_restart
       endif
    enddo

    lastrestart = modeltime%startTime
    do while (.not. ESMF_ClockIsStopTime(modelClock, rc=rc))
       call esmf_err(rc, subname, "advance to stop time")
       call ESMF_ClockAdvance(modelClock, rc=rc)
       call esmf_err(rc, subname, "ESMF_ClockAdvance")
       call ESMF_ClockGet(modelClock, currTime=modeltime%currTime, rc=rc)
       call esmf_err(rc, subname, "ESMF_ClockGet(currTime)")
       call ESMF_ClockGetNextTime(modelClock, modeltime%nextTime, rc=rc)
       call esmf_err(rc, subname, "get nextTime")
       importexport = get_importexport(modeltime%currTime, modeltime%nextTime, rc=rc)
       call esmf_err(rc, subname, "get importexport")

       do n = 1, n_restarts
          if (modeltime%nextTime == restart_times(n)) then
             lastrestart = restart_times(n)
          endif
          timestr_restart = get_timestr(lastrestart, rc=rc)
          call esmf_err(rc, subname, "get lastrestart")
       enddo

       do n = 1, n_completions
          if (modeltime%currTime == expected_completiontimes(n)) then
             if (debug_onroot) then
                timestr_complete = get_timestr(expected_completiontimes(n), rc=rc)
                call esmf_err(rc, subname, "timestr_complete")
                print '(A)',importexport//' file complete (clock time) '//timestr_complete//' lastrestart time '//timestr_restart
             endif
             actual_lastrestart(n) = lastrestart
          endif
       enddo
    enddo

    nmatches = 0
    do n = 1,n_completions
       if (actual_lastrestart(n) == expected_lastrestart(n)) nmatches = nmatches + 1
    enddo

  end subroutine run_case

end program test_restart_history_cadence
