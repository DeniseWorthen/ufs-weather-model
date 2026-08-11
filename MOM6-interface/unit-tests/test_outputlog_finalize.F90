!> Dedicated test for outputlog_freqn's finalize behavior -- both halves of
!! mom_cap.F90's two-call finalize sequence (the ordinary per-ring
!! mechanism's last chance, and the lstop-only currently-open interval),
!! since production always uses them as a pair. Scoped separately from
!! test_outputlog_freqn.F90, which covers the same regular mechanism during
!! an ONGOING run and never touches atStopTime at all.
!!
!! At finalize, mom_cap.F90 calls outputlog_run TWICE at the same clock
!! state (see the actual call site, ocean_model_finalize):
!!     call outputlog_run(clock, rc=rc)          ! plain
!!     call outputlog_run(clock, .true., rc=rc)  ! lstop
!! (comment there: "need to call twice to force logging of last output file")
!!
!! Why two calls, and what each is for (confirmed against the real code and
!! worked through with the user, since this was initially misunderstood):
!! the regular per-ring mechanism tracks exactly ONE interval per ring --
!! when ring N fires, it checks/completes the interval that just closed
!! (ring N-1 to ring N). The interval that's CURRENTLY open when the model
!! stops (ring N to the ring N+1 that will now never happen) is never
!! tracked by the regular mechanism at all -- only the lstop call, using
!! ESMF_AlarmGet's prevRingTime, can ever reach it.
!!
!! This creates two genuinely different scenarios, both real, both needing
!! their own fixture:
!!   - a file whose regular tracking began on the model's OWN LAST tick
!!     (so it only ever got one chance to be checked before the loop
!!     ended) -- resolved by the PLAIN finalize call giving it one more
!!     legitimate look, not by lstop
!!   - a file with NO regular tracking at all, since its own closing ring
!!     never happens -- resolved ONLY by the lstop call
!!
!! Worked example (start=6, freq=6, run_hours=18, average): rings at
!! 12/18/day2-00 (=stopTime). Ring day2-00 is the model's last tick --
!! it starts tracking "15" (window [12,18]), which gets exactly one
!! (incomplete) check before the loop ends. The window [18,day2-00] never
!! gets its own ring at all (would be day2-06, past stopTime) -- lstop's
!! prevRingTime (day2-00) is the only way to ever reach its file ("21",
!! via prevRingTime-0.5*freq).
!!
!> @date 08-10-2026
program test_outputlog_finalize

  use ESMF
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD, MPI_Barrier
  use test_utils
  use mom_outputlog_methods, only : outputlog_config_type, outputlog_state_type, get_timestr, set_toffset
  use MOM_cap_outputlog,     only : outputlog_freqn
  use MOM_cap_time,          only : AlarmInit
  use nc_fixture_mod,        only : create_schema, write_record, write_padding

  implicit none

  type(MPI_Comm) :: comm
  integer        :: rank, ierr, rootpe
  logical        :: isroot
  integer, parameter :: base_yy = 2021, base_mm = 3, base_dd = 22
  integer, parameter :: maxtests = 10

  character(len=128) :: testname
  character(len=256) :: assertmsg
  character(len=24)  :: subname = 'test_outputlog_finalize'

  type(testsummary) :: finalizetests

  logical :: is_passing, assertrc
  integer :: n
  logical :: verbose = .false.

  comm = MPI_COMM_WORLD
  rootpe = 0

  call finalizetests%init(maxtests)

  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  isroot = (rank == rootpe)
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Initialize")

  ! ------------------
  ! The double-close: "15" resolved by the plain finalize call (its
  ! regular tracking began on the model's own last tick), "21" resolved
  ! only by the lstop call (its own closing ring never happens).
  ! ------------------
  testname = 'test 01 double-close at finalize: plain catches 15, lstop catches 21'
  call run_lstop_case(freq=6, start_hour=6, run_hours=18, is_passing=is_passing)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(finalizetests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Test results
  ! ------------------
  if (finalizetests%nfail > 0) then
     print '(A)', 'FAIL: At least one test failed '
     do n = 1,finalizetests%count
        if (.not. finalizetests%teststatus(n)) print '(A)', trim(finalizetests%testmessage(n)%str)
     enddo
  else
     do n = 1,finalizetests%count
        print '(A)', trim(finalizetests%testmessage(n)%str)
     enddo
  endif
  print '(3(A,I0))','Total tests = ',finalizetests%count,' Passing = ',finalizetests%npass,' Failing = ',finalizetests%nfail

  call ESMF_Finalize(rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Finalize")

  if (finalizetests%nfail > 0) stop 1

contains

  !> Drives outputlog_freqn through one run, then replays mom_cap.F90's
  !! real two-call finalize sequence (plain, then atStopTime=.true.) at the
  !! final clock state, asserting each call catches the file only it can.
  subroutine run_lstop_case(freq, start_hour, run_hours, is_passing)
    integer, intent(in)  :: freq, start_hour, run_hours
    logical, intent(out) :: is_passing

    type(ESMF_Clock)         :: clock
    type(ESMF_Time)          :: startTime, stopTime, nextTime, refTime, lastrestart, prevring
    type(ESMF_TimeInterval)  :: timeStep, runOffset, tincrement, alarmoffset, elapsedTime
    type(outputlog_config_type) :: cf_n
    type(outputlog_state_type)  :: state_n
    integer :: toffset

    integer :: ierr, rc
    logical :: ringing, filecomplete, filecomplete_lstop
    character(len=16)  :: timestr, chour
    character(len=256) :: ring_filename, lstop_filename, pending_filename, outputdir
    character(len=256) :: lstop_logname
    character(len=256) :: line, expline
    integer :: logunit, ios
    integer :: yr, mon, day, hour, minute, sec
    real(kind=ESMF_KIND_R8) :: fhour

    outputdir = "./"

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

    toffset = set_toffset(start_hour, freq)
    alarmoffset = toffset*60*tincrement
    refTime = startTime + alarmoffset

    call AlarmInit(clock, alarm=cf_n%alarm, option='nhours', opt_n=freq, opt_ymd=-999, &
         RefTime=refTime, alarmname='test_alarm', rc=ierr)
    call esmf_err(ierr, subname, "AlarmInit")

    cf_n%alarm_name        = "test_alarm"
    cf_n%opt_n             = freq
    cf_n%requested         = .true.
    cf_n%timereduce        = 'average'
    cf_n%fnameprefix       = "ocn_"
    cf_n%fnamesuffix       = ""
    cf_n%logname_fhoffset  = 60*freq*tincrement
    cf_n%filename_fhoffset = 90*freq*tincrement

    write(chour,'(I2.2,A)') freq,'h'

    state_n%chkfile_nextAdvance = .false.
    state_n%use_filesize        = .false.
    state_n%filename            = ""
    state_n%createsize          = 0

    ! Restart pairing is out of scope here -- fixed dummy, never asserted on.
    lastrestart = startTime

    ! --- Main loop. Capture nextTime BEFORE advancing (equal to what
    ! currTime becomes after this tick's advance) -- matches production's
    ! actual pairing at the point outputlog_run reads the clock (confirmed
    ! against real PET-log output: ringing is reported together with the
    ! PRE-advance currTime/nextTime pair, e.g. 11:30/12:00, not the
    ! post-advance 12:00/12:30). Every ring's fixture is created
    ! (schema+padding, incomplete) but NEVER completed in-loop -- both "15"
    ! and "21"'s fixtures are deliberately left pending until after the
    ! loop, matching the real FMS-lag scenario this test exists to check.
    do while (.not. ESMF_ClockIsStopTime(clock, rc=ierr))
       call esmf_err(ierr, subname, "ESMF_ClockIsStopTime")

       call ESMF_ClockGetNextTime(clock, nextTime, rc=ierr)
       call esmf_err(ierr, subname, "ESMF_ClockGetNextTime (pre-advance)")

       call ESMF_ClockAdvance(clock, rc=ierr)
       call esmf_err(ierr, subname, "ESMF_ClockAdvance")

       ringing = ESMF_AlarmIsRinging(cf_n%alarm, rc=ierr)
       call esmf_err(ierr, subname, "ESMF_AlarmIsRinging")

       if (ringing) then
          timestr = get_timestr(nextTime - cf_n%filename_fhoffset, rc=ierr)
          call esmf_err(ierr, subname, "get_timestr")
          ring_filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
               //trim(cf_n%fnamesuffix)

          if (isroot) then
             call create_schema(trim(ring_filename))
             call write_padding(trim(ring_filename))
          end if
       end if

       call outputlog_freqn(clock, cf_n, state_n, comm, isroot, rootpe, outputdir, tincrement, &
            lastrestart, verbose, atStopTime=.false., rc=rc, &
            filecomplete_out=filecomplete, filecomplete_lstop_out=filecomplete_lstop)
       call esmf_err(rc, subname, "outputlog_freqn (main loop)")
    end do

    ! --- Loop has ended at stopTime. state_n%filename now holds "15" --
    ! the file whose regular tracking began on the model's own last tick
    ! (the ring at stopTime itself). Capture it directly from state_n
    ! rather than recomputing independently, since that's exactly what
    ! production itself is tracking -- no risk of an independent-formula
    ! mismatch here.
    pending_filename = state_n%filename

    ! --- Compute "21"'s filename the same way the real lstop block does
    ! (prevRingTime, not currTime/nextTime), since nothing has created it
    ! yet -- its own ring never happens.
    call ESMF_AlarmGet(cf_n%alarm, prevRingTime=prevring, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_AlarmGet(prevRingTime)")
    timestr = get_timestr(prevring - 30*freq*tincrement, rc=ierr)
    call esmf_err(ierr, subname, "get_timestr(lstop)")
    lstop_filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
         //trim(cf_n%fnamesuffix)

    ! --- Both fixtures reach genuine completion now, matching FMS actually
    ! finishing the writes by the time finalize runs.
    if (isroot) then
       call create_schema(trim(lstop_filename))
       call write_padding(trim(lstop_filename))
       call write_record(trim(pending_filename))
       call write_record(trim(lstop_filename))
    end if
    call MPI_Barrier(comm, ierr)

    ! --- Replay mom_cap.F90's real finalize sequence exactly: plain call
    ! first, then atStopTime=.true. -- same clock state, no advance
    ! between them.
    call outputlog_freqn(clock, cf_n, state_n, comm, isroot, rootpe, outputdir, tincrement, &
         lastrestart, verbose, atStopTime=.false., rc=rc, &
         filecomplete_out=filecomplete, filecomplete_lstop_out=filecomplete_lstop)
    call esmf_err(rc, subname, "outputlog_freqn (finalize, plain)")
    is_passing = filecomplete   ! plain call must catch "15"

    call outputlog_freqn(clock, cf_n, state_n, comm, isroot, rootpe, outputdir, tincrement, &
         lastrestart, verbose, atStopTime=.true., rc=rc, &
         filecomplete_out=filecomplete, filecomplete_lstop_out=filecomplete_lstop)
    call esmf_err(rc, subname, "outputlog_freqn (finalize, lstop)")
    is_passing = is_passing .and. filecomplete_lstop   ! lstop call must catch "21"
    if (.not. is_passing) return

    ! --- Verify log_restart_fh's REAL output directly: the call in
    ! question is log_restart_fh(prevring, startTime, 'mom6.lstop.'//chour,
    ! prefixtime=.true., lastrestart=state_n%time_lastrestart,
    ! lastoutput=state_n%filename, rc=rc). log_restart_fh's own internals
    ! (CDEPS) aren't our concern -- whether THIS call passes the right
    ! arguments is. Expected filename/content built using the SAME
    ! standard Fortran format specifiers log_restart_fh itself uses
    ! (i4.4/i2.2/i8/f10.3 -- mechanical rendering, not logic under test),
    ! applied to values this test already independently knows -- not a
    ! re-derivation of anything log_restart_fh decides.
    call ESMF_TimeGet(prevring, yy=yr, mm=mon, dd=day, h=hour, m=minute, s=sec, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeGet(prevring)")
    write(lstop_logname,'(i4.4,2(i2.2),A,3(i2.2),A)') yr, mon, day, '.', hour, minute, sec, &
         '.mom6.lstop.'//trim(chour)

    open(newunit=logunit, file=trim(lstop_logname), status='old', action='read', iostat=ios)
    if (ios /= 0) then
       is_passing = .false.
       return
    end if

    ! line 1: completed:
    read(logunit,'(a)',iostat=ios) line
    is_passing = is_passing .and. (ios == 0) .and. (trim(line) == 'completed: mom6.lstop.'//trim(chour))

    ! line 2: forecast hour: <elapsed hours from startTime to prevring, f10.3>
    elapsedTime = prevring - startTime
    call ESMF_TimeIntervalGet(elapsedTime, h_r8=fhour, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalGet(elapsedTime)")
    write(expline,'(a,f10.3)') 'forecast hour:', fhour
    read(logunit,'(a)',iostat=ios) line
    is_passing = is_passing .and. (ios == 0) .and. (trim(line) == trim(expline))

    ! line 3: valid time: <prevring, 6i8>
    write(expline,'(a,6i8)') 'valid time: ', yr, mon, day, hour, minute, sec
    read(logunit,'(a)',iostat=ios) line
    is_passing = is_passing .and. (ios == 0) .and. (trim(line) == trim(expline))

    ! line 4: last output: <state_n%filename, which is lstop_filename by
    ! this point -- the lstop block reassigns it before calling
    ! log_restart_fh>
    write(expline,'(a)') 'last output: '//trim(lstop_filename)
    read(logunit,'(a)',iostat=ios) line
    is_passing = is_passing .and. (ios == 0) .and. (trim(line) == trim(expline))

    ! line 5: last restart: <lastrestart (our fixed dummy = startTime), 6i8>
    call ESMF_TimeGet(lastrestart, yy=yr, mm=mon, dd=day, h=hour, m=minute, s=sec, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeGet(lastrestart)")
    write(expline,'(a,6i8)') 'last restart: ', yr, mon, day, hour, minute, sec
    read(logunit,'(a)',iostat=ios) line
    is_passing = is_passing .and. (ios == 0) .and. (trim(line) == trim(expline))

    ! confirm EXACTLY 5 lines -- no more (would mean an unexpected extra
    ! optional field got included)
    read(logunit,'(a)',iostat=ios) line
    is_passing = is_passing .and. (ios /= 0)

    close(logunit)
  end subroutine run_lstop_case

end program test_outputlog_finalize
