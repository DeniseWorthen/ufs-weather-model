!>  Narrow orchestration test for the outputlog_freqn machinery
!!
!> @date 08-12-2026
program test_driver

  use ESMF
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD, MPI_Barrier
  use test_utils
  use mom_cap_outputlog,     only : outputlog_freqn
  use mom_outputlog_methods, only : outputlog_config_type, outputlog_state_type, outputlog_modeltime_type
  use mom_outputlog_methods, only : get_timestr, get_importexport, set_toffset, get_file_state, debug_info
  use mom_outputlog_methods, only : get_ring_state
  use MOM_cap_time,          only : AlarmInit
  use nc_fixture_mod,        only : create_schema, write_record, write_padding, write_bulk_data

  implicit none

  integer, parameter :: base_yy = 2021, base_mm = 3, base_dd = 22
  integer, parameter :: maxtests = 10

  type(MPI_Comm) :: comm
  integer        :: rank, ierr, rootpe
  logical        :: isroot

  character(len=128) :: testname
  character(len=256) :: assertmsg
  character(len=20)  :: subname = 'test_outputlog_freqn'

  type(testsummary) :: freqntests

  logical :: debug_onroot
  logical :: assertrc
  integer :: n,nt
  integer :: expected, completions
  logical :: verbose = .true.

  comm = MPI_COMM_WORLD
  rootpe = 0

  call freqntests%init(maxtests)

  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  isroot = (rank == rootpe)
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=ierr)
  call esmf_err(ierr, subname, "ESMF_Initialize")

  debug_onroot = verbose .and. isroot
  nt = 0
  ! ===========================================================================
  ! Test cases
  ! - ring_hours is clock hour when rings happen
  ! - ring_ticks is whether the ring_hour index will create a file
  ! ===========================================================================

  ! ------------------
  nt = nt + 1
  expected = 1  ! ring at hour=18 completes 09 file
  write(testname,'(A,I2.2,A)')'test ',nt,' two rings, one tracked averaging window completes '

  call run_case(trim(testname),               &
       freq=6, start_hour=6, runhours=13,     &
       ring_hours=[12,18], ring_ticks=[-1,1], &
       use_filesize=.true.,                   &
       completions=completions)

  call assert_equal(completions, expected, testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  nt = nt + 1
  expected = 1 ! ring at hour 18 tracks 09 file for DATM
  write(testname,'(A,I2.2,A)')'test ',nt,' two rings, one tracked averaging window completes (DATM)'

  call run_case(trim(testname),               &
       freq=6, start_hour=6, runhours=13,     &
       ring_hours=[12,18], ring_ticks=[-1,1], &
       completions=completions)

  call assert_equal(completions, expected, testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  nt = nt + 1
  expected = 3 ! rings at hour=8,9,10 complete 07,08,09 files
  write(testname,'(A,I2.2,A)')'test ',nt,' snapshots, multiple rings complete correctly'

  call run_case(trim(testname),                      &
       freq=1, start_hour=6, runhours=5,             &
       ring_hours=[7,8,9,10], ring_ticks=[-1,1,1,1], &
       use_filesize=.true., timereduce='none',       &
       completions=completions)

  call assert_equal(completions, expected, testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  nt = nt + 1
  expected = 0 ! ring at hour 12 tracks 03 phantom file
  write(testname,'(A,I2.2,A)')'test ',nt,' single ring tracks phantom 03 file'

  call run_case(trim(testname),          &
       freq=6, start_hour=6, runhours=7, &
       ring_hours=[12], ring_ticks=[-1], &
       use_filesize=.true.,              &
       completions = completions)

  call assert_equal(completions, expected, testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  nt = nt + 1
  expected = 1 ! rings at hours 12,18 track phantom files 03,09; hour=24 tracks real 15 file
  write(testname,'(A,I2.2,A)')'test ',nt,' three ring, but only one real window, completes correctly'

  call run_case(trim(testname),                     &
       freq=6, start_hour=9, runhours=18,           &
       ring_hours=[12,18,24], ring_ticks=[-1,-1,1], &
       use_filesize=.true.,                         &
       completions=completions)

  call assert_equal(completions, 1, testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' two ring, one real window, completes correctly'

  call run_case(trim(testname),               &
       freq=24, start_hour=6, runhours=56,    &
       ring_hours=[30,54], ring_ticks=[-1,1], &
       use_filesize=.true.,                   &
       completions=completions)

  call assert_equal(completions, 1, testname, assertrc, assertmsg)
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
  !> TODO
  subroutine run_case(test, freq, start_hour, runhours, ring_hours, ring_ticks, &
       timereduce, use_filesize, nfiles, completions)

    character(len=*), intent(in)           :: test
    integer,          intent(in)           :: freq, start_hour, runhours
    integer,          intent(in)           :: ring_hours(:)
    integer,          intent(in)           :: ring_ticks(:)
    character(len=*), intent(in), optional :: timereduce
    logical,          intent(in), optional :: use_filesize
    integer,          intent(in), optional :: nfiles
    integer,          intent(out)          :: completions

    character(len=7) :: l_timereduce
    logical          :: l_use_filesize
    integer          :: l_nfiles

    type(ESMF_Clock)         :: modelClock
    type(ESMF_Time)          :: startTime, currTime, nextTime, stopTime, lastrestart
    type(ESMF_TimeInterval)  :: timeStep, tincrement, elapsedtime

    type(outputlog_config_type)    :: cf_n
    type(outputlog_state_type)     :: state_n
    type(outputlog_modeltime_type) :: modeltime

    integer :: minutes
    integer :: elapsedhours
    integer :: ring_index
    integer :: ierr, rc
    integer :: toffset, count
    logical :: pending, firstcomplete

    character(len=16)  :: timestr
    character(len=256) :: outputdir
    character(len=256) :: cmdstr = ''
    character(len=40)  :: importexport
    character(len=20)  :: subname = 'run_case'

    !debug
    integer :: nlen, fsize

    outputdir = "./"
    completions = 0

    l_timereduce = 'average'
    if (present(timereduce)) l_timereduce = timereduce
    l_use_filesize = .false.
    if (present(use_filesize)) l_use_filesize = use_filesize
    l_nfiles = 1
    if (present(nfiles)) l_nfiles = nfiles

    if (isroot) then
       cmdstr = 'rm -f '//trim(outputdir)//'*.nc '//trim(outputdir)//'*.mom6.*'//'  ./PET*'
       call execute_command_line(trim(cmdstr), wait=.true.)
    endif
    call MPI_Barrier(comm, ierr)
    if (ierr /= 0) then
       write(0,'(A)') "FATAL ("//trim(subname)//"): MPI_Barrier (post-cleanup) failed"
       stop 99
    endif

    ! mimic outputlog_init setup
    call setup_case(start_hour, runhours, freq, l_nfiles, l_timereduce, modelClock, cf_n, state_n, rc)

    completions = 0
    ! dummy value, restart pairing is out of scope for this test
    lastrestart = state_n%time_lastrestart

    do while (.not. ESMF_ClockIsStopTime(modelClock, rc=rc))
       call esmf_err(rc, subname, "advance to stop time")

       call ESMF_ClockGet(modelClock, startTime=modeltime%startTime, currTime=modeltime%currTime, rc=rc)
       call esmf_err(rc, subname, "get start and currTime")
       call ESMF_ClockGetNextTime(modelClock, modeltime%nextTime, rc=rc)
       call esmf_err(rc, subname, "get nextTime")
       importexport = get_importexport(modeltime%currTime, modeltime%nextTime, rc=rc)
       call esmf_err(rc, subname, "get importexport")
       call ESMF_TimeIntervalSet(modeltime%tincrement, m=1, rc=rc)
       call esmf_err(rc, subname, "ESMF_TimeIntervalSet(tincrement)")

       elapsedtime = modeltime%nextTime - modeltime%startTime
       call ESMF_TimeIntervalGet(elapsedtime, m=minutes, rc=rc)
       elapsedhours = minutes/60
       ! ring_index serves to match ring hour with whether a file predates start
       ring_index = findloc_int(ring_hours, start_hour+elapsedhours)
       !if (ring_index > 0) then
       !   if (debug_onroot) print '(A,3i4)',importexport//' elapsedhours = ',elapsedhours,ring_index,ring_ticks(ring_index)
       !endif

       state_n%ringing = .false.
       count = 0
       call ESMF_ClockAdvance(modelClock,ringingalarmcount=count,rc=rc)
       call esmf_err(rc, subname, "ringing alarm count")
       if (count > 0) then
          state_n%ringing = .true.
          call ESMF_AlarmRingerOff(cf_n%alarm, rc=rc)
          call esmf_err(rc, subname, "alarm ringer off")
       endif

       ! complete any pending file from previous advance (mimics FMS complete)
       if (pending .and. len_trim(state_n%filename) > 0) then
          if (isroot) then
             if (l_use_filesize) then
                call write_bulk_data(state_n%filename)   ! fsize grows past createsize
             else
                call write_record(state_n%filename)      ! nlen 0->1
             endif
          endif
          pending = .false.
          firstcomplete = .false.
          ! if (debug_onroot) then
          !    call get_file_state(comm, isroot, rootpe, state_n%filename, nlen=nlen, fsize=fsize, rc=rc)
          !    print '(A,i4,i12,2(A,L))',trim(subname)//' complete file '//state_n%filename//'  '//importexport, &
          !         nlen,fsize,' pending ',pending,' ringing ',state_n%ringing
          ! endif
       endif

       if (ring_index > 0) then
          if (state_n%ringing .and. ring_ticks(ring_index) > 0) then
             timestr = get_timestr(modeltime%nextTime - cf_n%filename_fhoffset, rc=rc)
             state_n%filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
                  //trim(cf_n%fnamesuffix)
             if (isroot) then
                call create_schema(state_n%filename)
                if (l_use_filesize) then
                   call write_record(state_n%filename)
                else
                   call write_padding(state_n%filename)
                endif
             endif
             pending = .true.
             ! if (debug_onroot) then
             !    call get_file_state(comm, isroot, rootpe, state_n%filename, nlen=nlen, fsize=fsize, rc=rc)
             !    print '(A,i4,i12,2(A,L))',trim(subname)//' create file '//state_n%filename//'  '//importexport,  &
             !         nlen,fsize,' pending ',pending,' ringing ',state_n%ringing
             ! endif
          endif
       endif

       call outputlog_freqn(modeltime, cf_n, state_n, comm, isroot, rootpe, outputdir, lastrestart, &
            debug_onroot, .false., rc)

       if (state_n%filecomplete .and. .not.firstcomplete) then
          completions = completions + 1
          firstcomplete = .true.
       endif
    enddo ! do while

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
       endif
    enddo
  end function findloc_int

  subroutine setup_case(start_hour, runhours, freq, l_nfiles, l_timereduce, modelClock, cf_n, state_n, rc)

    integer,                     intent(in)  :: start_hour, runhours, freq, l_nfiles
    character(len=*),            intent(in)  :: l_timereduce
    type(ESMF_Clock),            intent(out) :: modelClock
    type(outputlog_config_type), intent(out) :: cf_n
    type(outputlog_state_type),  intent(out) :: state_n
    integer,                     intent(out) :: rc

    type(ESMF_TimeInterval) :: alarmoffset
    type(ESMF_Time)         :: startTime, currTime, nextTime, stopTime, refTime
    type(ESMF_TimeInterval) :: timeStep, tincrement, elapsedhours

    integer :: toffset
    character(len=16)  :: startstr, stopstr, timestr
    character(len=120) :: subname = 'setup_case'

    rc = ESMF_SUCCESS

    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeSet(startTime)")
    call ESMF_TimeSet(stopTime,  yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour+runhours, rc=rc)

    call ESMF_TimeIntervalSet(timeStep, s=1800, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeIntervalSet(timeStep)")
    call ESMF_TimeIntervalSet(tincrement, m=1, rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeIntervalSet(tincrement)")
    modelClock  = ESMF_ClockCreate(name="Model",timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)

    call ESMF_ClockGet(modelclock, currTime=currTime, startTime=startTime, stopTime=stopTime, rc=rc)
    call esmf_err(rc, subname, "ESMF_ClockGet start,stop time")
    startstr = get_timestr(startTime, rc=rc)
    stopstr = get_timestr(stopTime, rc=rc)
    if (debug_onroot) then
       print '(A)','Clock will run from '//startstr//' to '//stopstr
    endif

    ! --- Build the rest of cf_n exactly as outputlog_init would ---
    cf_n%alarm_name           = 'test_alarm'
    cf_n%opt_n                = freq
    cf_n%requested            = .true.
    cf_n%timereduce           = l_timereduce
    cf_n%fnameprefix          = 'ocn_'
    if (l_nfiles == 1) then
       cf_n%fnamesuffix       = ''
    else
       cf_n%fnamesuffix       = '.000'
    endif
    if (trim(l_timereduce) == 'none') then
       cf_n%logname_fhoffset  = 0*tincrement
       cf_n%filename_fhoffset = 60*freq*tincrement
    else
       cf_n%logname_fhoffset  = 60*freq*tincrement
       cf_n%filename_fhoffset = 90*freq*tincrement
    endif

    state_n%chkfile_nextAdvance = .false.
    state_n%use_filesize        = .false.
    state_n%filecomplete        = .false.
    state_n%createsize          = 0
    state_n%completesize        = 0
    state_n%filecomplete        = .false.

    ! Restart pairing is out of scope for this file -- fixed dummy value,
    ! never asserted on. See the dedicated restart-pairing test.
    state_n%time_lastrestart         = startTime

    ! the time offset in hours required to ensure the alarm rings at multiples of freq(n)
    ! regardless of start day/hour
    toffset = set_toffset(start_hour, freq)
    alarmoffset = toffset*60*tincrement

    call AlarmInit(modelclock,              &
         alarm     = cf_n%alarm,            &
         option    = 'nhours',              &
         opt_n     = cf_n%opt_n,            &
         opt_ymd   = -999,                  &
         RefTime   = currTime+alarmoffset,  &
         alarmname = cf_n%alarm_name, rc=rc)
    call esmf_err(rc, subname, "call AlarmInit")
  end subroutine setup_case

end program test_driver
