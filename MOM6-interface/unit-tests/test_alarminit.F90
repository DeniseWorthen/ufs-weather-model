!> Test code for outputlog Alarm Initialization
!!
!! Probes the use of AlarmInit from MOM6 NUOPC cap to intialize alarms for
!! use by outputlog feature. Tests to ensure the outputlog alarms will trigger
!! for start hours which are not multiples of 6 (eg IAU use cases)
!!
!> @authorDenise.Worthen@noaa.gov
!> @date 08-01-2026
program test_alarminit

  use test_utils

  use ESMF,  only : ESMF_Initialize, ESMF_Finalize, ESMF_SUCCESS, ESMF_FAILURE
  use ESMF,  only : ESMF_CALKIND_GREGORIAN, ESMF_Calendar, ESMF_CalendarCreate, ESMF_Alarm
  use ESMF,  only : ESMF_Clock, ESMF_ClockCreate, ESMF_ClockGet, ESMF_ClockAdvance
  use ESMF,  only : ESMF_Time, ESMF_TimeSet, ESMF_TimeGet, ESMF_TimeInterval, ESMF_TimeIntervalSet
  use ESMF,  only : operator(==), operator(/=), operator(+), operator(-), operator(*)
  use MOM_cap_time, only : AlarmInit

  implicit none

  integer, parameter :: base_yy = 2021, base_mm = 3, base_dd = 22
  integer, parameter :: maxtests = 25

  character(len=128) :: testname
  character(len=256) :: errmsg
  character(len=256) :: assertmsg
  character(len=20)  :: subname = 'test_alarminit'

  type(testsummary)  :: alarmtests

  logical :: is_passing, assertrc, chk
  integer :: teststart, testfreq, ring_hour
  integer :: rc,nt,n,ierr
  ! debug printing
  logical :: verbose = .false.

  ! initialize test tracker
  call alarmtests%init(maxtests)

  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
  call esmf_err(rc, subname, "ESMF_Initialize")

  nt = 0
  ! ===========================================================================
  ! test capture of ringtime via test
  ! ===========================================================================

  nt = nt + 1
  teststart = 0; testfreq = 6
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ring_hour, errmsg, ierr, nsteps=4)

  is_passing = (ierr /= 0)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ===========================================================================
  ! test IAU offset for start hours, freqs=6,24; Ring hour must land on a
  ! 6h boundary
  ! ===========================================================================

  testfreq = 6
  do n = 1,8
     nt = nt + 1
     teststart = (n-1)*3
     write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
     call run_case(testfreq, teststart, ring_hour, errmsg, ierr, manualchk=chk)

     is_passing = (ierr == 0 .and. mod(ring_hour,6) == 0 .and. chk)
     call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
     call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))
  enddo

  ! ------------------
  testfreq = 24
  do n = 1,8
     nt = nt + 1
     teststart = (n-1)*3
     write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
     call run_case(testfreq, teststart, ring_hour, errmsg, ierr, manualchk=chk)

     is_passing = (ierr == 0 .and. mod(ring_hour,6) == 0 .and. chk)
     call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
     call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))
  enddo

  ! ===========================================================================
  ! test no IAU offset for start hours, freq=1,3; Ring hour must be start+freq
  ! exactly
  ! ===========================================================================

  nt = nt + 1
  teststart = 0; testfreq = 1
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ring_hour, errmsg, ierr, manualchk=chk)

  is_passing = (ierr == 0 .and. ring_hour == teststart+testfreq)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  teststart = 9; testfreq = 1
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ring_hour, errmsg, ierr, manualchk=chk)

  is_passing = (ierr == 0 .and. ring_hour == teststart+testfreq)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  teststart = 0; testfreq = 3
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ring_hour, errmsg, ierr, manualchk=chk)

  is_passing = (ierr == 0 .and. ring_hour == teststart+testfreq)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  teststart = 9; testfreq = 3
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ring_hour, errmsg, ierr, manualchk=chk)

  is_passing = (ierr == 0 .and. ring_hour == teststart+testfreq)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  teststart = 21; testfreq = 3
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ring_hour, errmsg, ierr, manualchk=chk)

  is_passing = (ierr == 0 .and. ring_hour == 0)   ! ring at start of day
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  ! Test results
  ! ------------------

  print '(3(A,I0))','Total tests = ',alarmtests%count,' Passing = ',alarmtests%npass,' Failing = ',alarmtests%nfail
  if (alarmtests%nfail > 0) then
     print '(A)', 'FAIL: At least one test failed '
     do n = 1,alarmtests%count
        if (.not. alarmtests%teststatus(n)) print '(A)', trim(alarmtests%testmessage(n)%str)//'  [' &
             //trim(alarmtests%errmessage(n)%str)//']'
     enddo
     stop 1
  else
     do n = 1,alarmtests%count
        if (verbose .and. len_trim(alarmtests%errmessage(n)%str) > 0) then
           print '(A)', trim(alarmtests%testmessage(n)%str)//'  ['//trim(alarmtests%errmessage(n)%str)//']'
        else
           print '(A)', trim(alarmtests%testmessage(n)%str)
        endif
     enddo
  endif

  call ESMF_Finalize(rc=rc)
  call esmf_err(rc, subname, "ESMF_Finalize")

contains

  subroutine run_case(freq, start_hour, ring_hour, errmsg, ierr, nsteps, manualchk)

    integer,           intent(in)  :: freq, start_hour
    integer,           intent(out) :: ring_hour
    character(len=*),  intent(out) :: errmsg
    integer,           intent(out) :: ierr
    logical, optional, intent(out) :: manualchk
    integer, optional, intent(in)  :: nsteps

    type(ESMF_Clock)        :: clock
    type(ESMF_Calendar)     :: cal
    type(ESMF_Time)         :: startTime, refTime, ringTime
    type(ESMF_TimeInterval) :: timeStep, ringInterval, tincrement, alarmoffset
    type(ESMF_Alarm)        :: alarm

    integer :: max_steps, rc
    integer :: toffset, ring_day
    integer :: expected_hour
    integer :: rcnt, step
    logical :: rang

    ierr = 0
    errmsg = ''

    if (present(nsteps)) then
       max_steps = nsteps
    else
       max_steps = 200 ! 100h at test dt=30min
    endif

    cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, rc=rc)
    call esmf_err(rc, subname,  "ESMF_CalendarCreate")
    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, calendar=cal, rc=rc)
    call esmf_err(rc, subname,  "ESMF_TimeSet(startTime)")
    call ESMF_TimeIntervalSet(timeStep, s=1800, rc=rc)
    call esmf_err(rc, subname,  "ESMF_TimeIntervalSet(timeStep)")

    clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, rc=rc)
    call esmf_err(rc, subname,  "ESMF_ClockCreate")
    call ESMF_TimeIntervalSet(tincrement, m=1, rc=rc)
    call esmf_err(rc, subname,  "ESMF_TimeIntervalSet(tincrement)")

    if (mod(start_hour,6) /= 0) then
       toffset = start_hour - 6
    else
       toffset = 0
    endif

    if (freq >= 6) then
       alarmoffset = toffset*60*tincrement
    else
       alarmoffset = 0*tincrement
    endif
    refTime = startTime + alarmoffset

    call AlarmInit(clock,      &
         alarm     = alarm,    &
         option    = 'nhours', &
         opt_n     = freq,     &
         opt_ymd   = -999,     &
         RefTime   = refTime,  &
         alarmname = 'test_alarm', rc=rc)
    call esmf_err(rc, subname,  "AlarmInit")

    ! Find first ring
    rang = .false.
    do step = 1, max_steps
       call ESMF_ClockAdvance(clock, ringingAlarmCount=rcnt, rc=rc)
       call esmf_err(rc, subname,  "ESMF_ClockAdvance")
       if (rcnt > 0) then
          rang = .true.
          exit
       endif
    end do
    if (.not. rang) then
       ierr = 1
       errmsg = 'ERROR: alarm never rang within '//itoa(max_steps)//' steps'
       return
    end if

    if (verbose) then
       print '(A,I0,A)', "  alarm first rang after ", step, " steps of stepping the clock forward"
       print '(A,I0,A,/)', "  RefTime passed to AlarmInit is shifted by that same ", toffset, "h"
    endif

    call ESMF_ClockGet(clock, currTime=ringTime, rc=rc)
    call esmf_err(rc, subname,  "ESMF_ClockGet(currTime at ringTime)")

    call ESMF_TimeGet(ringTime, dd=ring_day, h=ring_hour, rc=rc)
    call esmf_err(rc, subname,  "ESMF_TimeGet(ringTime)")

    ! Validate ring hour against manually derived value
    expected_hour = predicted_ring_hour(start_hour, freq, toffset)
    manualchk = (ring_hour == expected_hour)
  end subroutine run_case

  function predicted_ring_hour(start_hour, freq, toffset) result(h)
    integer, intent(in) :: start_hour, freq, toffset
    integer :: h
    integer :: eff_offset, val

    eff_offset = merge(toffset, 0, freq >= 6)
    val = eff_offset - freq
    do while (val <= 0)
       val = val + freq
    end do
    h = modulo(start_hour + val, 24)
  end function predicted_ring_hour
end program test_alarminit
