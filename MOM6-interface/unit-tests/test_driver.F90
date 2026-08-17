!> Narrow orchestration test for the outputlog_freqn machinery
!!
!> @date 08-12-2026
program test_driver

  use ESMF
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD, MPI_Barrier
  use test_utils
  use mom_outputlog_methods, only : outputlog_config_type, outputlog_state_type
  use mom_outputlog_methods, only : get_timestr, get_importexport, set_toffset, get_file_state, debug_info
  use mom_outputlog_methods, only : get_ring_state, check_file_completion
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
  logical :: is_passing, assertrc
  integer :: n,nt
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
#ifdef test
  ! ------------------
  ! Case 1: single ring closing a REAL (non-phantom) window -- ring at
  ! hour 12 closes the phantom [0,6] window (predates model start=6, never
  ! completes); ring at hour 18 closes the real [6,12] window, and does
  ! complete
  ! ------------------

  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' single ring, real window, completes correctly'
  call run_case(trim(testname), freq=6, start_hour=6, ring_hours=[6,12], ring_ticks=[-1,1], &
       use_filesize = .true.,    &
       expected_completions = 1, &
       is_passing=is_passing)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 2: single ring, fixture never completes -- confirms
  ! check_file_completion correctly keeps reporting not-complete rather
  ! than falsely completing or erroring.
  ! ------------------

  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' single ring never completes'
  call run_case(trim(testname), freq=6, start_hour=6, ring_hours=[12], ring_ticks=[-1], &
       use_filesize = .true.,    &
       expected_completions = 0, &
       is_passing=is_passing)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')
#endif
  ! ------------------
  ! Case 3: two rings, the first never completes, the second does.
  ! ------------------

  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' two consecutive rings, second completes'
  call run_case(trim(testname), freq=6, start_hour=0, ring_hours=[6,12], ring_ticks=[-1,1], &
       use_filesize = .true.,    &
       expected_completions = 1, &
       is_passing=is_passing)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 4: single ring, snapshot ('none') type -- exercises the 1x
  ! (60*freq) filename lookback, distinct from 'average's 1.5x (90*freq).
  ! ------------------

  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' single ring snapshot (none) type completes correctly'
  call run_case(trim(testname), freq=6, start_hour=6, ring_hours=[6,12], ring_ticks=[-1,1], &
       use_filesize = .true.,    &
       expected_completions = 1, &
       timereduce='none',        &
       is_passing=is_passing)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(freqntests, assertrc, trim(assertmsg), '')

  ! ------------------
  ! Case 5: single ring, DATM-style fixture (nlen flips at completion)
  ! ------------------

  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' single ring DATM-style completes correctly'
  call run_case(trim(testname), freq=6, start_hour=6, ring_hours=[6,12], ring_ticks=[-1,1], &
       expected_completions=1, &
       is_passing=is_passing)

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
  subroutine run_case(test, freq, start_hour, ring_hours, ring_ticks, expected_completions, is_passing, &
       timereduce, use_filesize)

    character(len=*), intent(in)  :: test
    integer,          intent(in)  :: freq, start_hour
    integer,          intent(in)  :: ring_hours(:)
    integer,          intent(in)  :: ring_ticks(:)
    integer,          intent(in)  :: expected_completions
    logical,          intent(out) :: is_passing
    character(len=*), intent(in), optional :: timereduce
    logical,          intent(in), optional :: use_filesize

    character(len=7) :: l_timereduce
    logical          :: l_use_filesize

    type(ESMF_Clock)         :: modelclock
    type(ESMF_Time)          :: startTime, currTime, nextTime, stopTime, lastrestart
    type(ESMF_TimeInterval)  :: timeStep, tincrement, alarmoffset

    type(outputlog_config_type) :: cf_n
    type(outputlog_state_type)  :: state_n

    integer :: max_hour, day, hour, ring_index
    integer :: ierr, rc
    integer :: toffset, count
    integer :: num_completions
    logical :: filecomplete
    logical :: pending

    character(len=16)  :: timestr
    character(len=256) :: outputdir
    character(len=256) :: cmdstr = ''
    character(len=40)  :: importexport

    !debug
    integer :: nlen, fsize

    outputdir = "./"
    num_completions = 0

    l_timereduce = 'average'
    if (present(timereduce)) l_timereduce = timereduce
    l_use_filesize = .false.
    if (present(use_filesize)) l_use_filesize = use_filesize

    if (isroot) then
       cmdstr = 'rm -f '//trim(outputdir)//'*.nc'
       call execute_command_line(trim(cmdstr), wait=.true.)
    endif
    call MPI_Barrier(comm, ierr)
    if (ierr /= 0) then
       write(0,'(A)') "FATAL ("//trim(subname)//"): MPI_Barrier (post-cleanup) failed"
       stop 99
    endif

    max_hour = maxval(ring_hours) + maxval([ring_ticks, 1]) + 1
    if (debug_onroot) then
       print '(A,i4,A)','TEST: '//test//', run for ',max_hour,' hours'
    endif

    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeSet(startTime)")
    call ESMF_TimeSet(stopTime,  yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour+max_hour, rc=ierr)

    call ESMF_TimeIntervalSet(timeStep, s=3600, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalSet(timeStep)")
    call ESMF_TimeIntervalSet(tincrement, m=1, rc=ierr)
    call esmf_err(ierr, subname, "ESMF_TimeIntervalSet(tincrement)")
    modelClock  = ESMF_ClockCreate(name="Model",timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)

    call ESMF_ClockGet(modelclock, currTime=currTime, rc=ierr)
    call esmf_err(ierr, subname, "get currTime")

    ! --- Build the rest of cf_n exactly as outputlog_init would ---
    cf_n%alarm_name           = 'test_alarm'
    cf_n%opt_n                = freq
    cf_n%requested            = .true.
    cf_n%timereduce           = l_timereduce
    cf_n%fnameprefix          = 'ocn_'
    if (trim(l_timereduce) == 'none') then
       cf_n%logname_fhoffset  = 0*tincrement
       cf_n%filename_fhoffset = 60*freq*tincrement
    else
       cf_n%logname_fhoffset  = 60*freq*tincrement
       cf_n%filename_fhoffset = 90*freq*tincrement
    endif

    state_n%chkfile_nextAdvance = .false.
    state_n%use_filesize        = .false.
    state_n%filename            = ''
    state_n%createsize          = 0

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
         alarmname = cf_n%alarm_name, rc=ierr)
    call esmf_err(ierr, subname, "call AlarmInit")

    do while (.not. ESMF_ClockIsStopTime(modelClock, rc=ierr))
       call esmf_err(ierr, subname, "clock at stop time")

       call ESMF_ClockGet(modelclock, currTime=currTime, rc=ierr)
       call esmf_err(rc, subname, "get currTime")
       call ESMF_ClockGetNextTime(modelclock, nextTime, rc=ierr)
       call esmf_err(rc, subname, "get nextTime")
       importexport = get_importexport(currTime, nextTime, rc=ierr)
       call esmf_err(rc, subname, "get importexport")

       call ESMF_TimeGet(nextTime, d=day, h=hour, rc=ierr)
       call esmf_err(rc, subname, "get time")
       ring_index = findloc_int(ring_hours, hour-start_hour)
       !if (ring_index > 0) then
       !   if (debug_onroot) print *,'ring info hour = ',hour,ring_index,ring_ticks(ring_index)
       !endif

       state_n%ringing = .false.
       count = 0
       call ESMF_ClockAdvance(modelClock,ringingalarmcount=count,rc=ierr)
       call esmf_err(rc, subname, "ringing alarm count")
       if (count > 0) then
          state_n%ringing = .true.
          call ESMF_AlarmRingerOff(cf_n%alarm, rc=ierr)
          call esmf_err(rc, subname, "alarm ringer off")
       endif

       pending = .false.
       if (ring_index > 0 ) then
          if (ring_ticks(ring_index) > 0) then
             timestr = get_timestr(nextTime - cf_n%filename_fhoffset, rc=ierr)
             state_n%filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
                  //trim(cf_n%fnamesuffix)

             ! write_padding leaves nlen=0 (DATM-style)
             ! write_record sets nlen=1 immediately (ATM-style), and also sets createsize.
             if (isroot) then
                call create_schema(trim(state_n%filename))
                if (l_use_filesize) then
                   call write_record(trim(state_n%filename))
                else
                   call write_padding(trim(state_n%filename))
                endif
                pending = .true.
             endif
             !if (debug_onroot) then
             !   call get_file_state(comm, isroot, rootpe, state_n%filename, nlen=nlen, fsize=fsize, rc=rc)
             !   print '(A,i4,i12)','at '//importexport//' create file '//trim(state_n%filename),nlen,fsize
             !endif

          endif
       endif

       call outputlog_run(startTime, currTime, nextTime, state_n, cf_n, comm, isroot, rootpe, outputdir, filecomplete, ierr)

       ! complete file for the next advance
       if (pending .and. len_trim(state_n%filename) > 0) then
          if (isroot) then
             if (l_use_filesize) then
                call write_bulk_data(trim(state_n%filename))   ! fsize grows past createsize
             else
                call write_record(trim(state_n%filename))      ! nlen 0->1
             endif
          endif
          pending = .false.
          !if (debug_onroot) then
          !   call get_file_state(comm, isroot, rootpe, state_n%filename, nlen=nlen, fsize=fsize, rc=rc)
          !   print '(A,i4,i12)','at '//importexport//' wrote file '//trim(state_n%filename),nlen,fsize
          !endif
       endif

       if (filecomplete) num_completions = num_completions + 1
    enddo

    is_passing = (num_completions == expected_completions)

  end subroutine run_case

  subroutine outputlog_run(startTime,currTime,nextTime,state_n,cf_n,comm,isroot,rootpe,outputdir,filecomplete,ierr)

    type(ESMF_Time),             intent(in)    :: startTime, currTime, nextTime
    type(outputlog_state_type),  intent(inout) :: state_n
    type(outputlog_config_type), intent(inout) :: cf_n
    type(MPI_Comm),              intent(in)    :: comm
    logical,                     intent(in)    :: isroot
    integer,                     intent(in)    :: rootpe
    character(len=*),            intent(in)    :: outputdir
    logical,                     intent(out)   :: filecomplete
    integer,                     intent(out)   :: ierr

    character(len=40) :: importexport
    character(len=16) :: timestr

    integer :: nlen,fsize,rc

    ierr = 0

    importexport = get_importexport(currTime, nextTime, rc=ierr)

    if (state_n%ringing) then
       call get_ring_state(nextTime, cf_n%alarm, cf_n, state_n, comm, isroot, rootpe, outputdir, rc)
       call esmf_err(rc, subname, "get_ring_state")

       if (debug_onroot) then
          print '(A,2(A,L),A,i16)',trim(subname)//' fname '//trim(state_n%filename)//'  '        &
               //trim(importexport),' checkflag ',state_n%chkfile_nextAdvance,' use_filesize ',  &
               state_n%use_filesize, '  ',state_n%createsize
       endif
    endif

    call check_file_completion(state_n, comm, isroot, rootpe, startTime, state_n%time_lastrestart, &
         filecomplete, rc)
    call esmf_err(rc, subname, "check_file_completion")

    if (debug_onroot) call debug_info(trim(subname)//'  ',trim(state_n%filename), &
         state_n%chkfile_nextAdvance, state_n%createsize, importexport)

end subroutine outputlog_run
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

end program test_driver
