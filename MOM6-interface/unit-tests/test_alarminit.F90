program test_alarminit

  use ESMF
  use MOM_cap_time, only : AlarmInit

  implicit none

  ! ============================================================================
  ! Standalone test of AlarmInit -- specifically the toffset/alarmoffset
  ! correction applied in outputlog_init (believed to be IAU-related): for
  ! freq >= 6, the alarm's first ring is pulled onto the nominal 6-hour grid
  ! regardless of the model's actual (possibly non-6-aligned) start hour; for
  ! freq < 6, no correction is applied at all.
  !
  ! Two-tier assertions per the same principle used for the completion
  ! contract test:
  !   PRIMARY (independent of any re-derived formula): the code's own stated
  !     purpose -- "ensure the alarm rings at multiples of 6" for freq>=6;
  !     for freq<6, ring = start_hour + freq exactly, no correction.
  !   SECONDARY (regression only, re-derives the same toffset/RefTime formula
  !     production uses -- flagged explicitly as carrying oracle-mirroring
  !     risk, unlike the primary check): the exact expected ring hour.
  ! ============================================================================

  integer :: total_errors
  logical :: verbose

  integer, parameter :: base_yy = 2021, base_mm = 3, base_dd = 22

  total_errors = 0
  verbose = .true.

  call esmf_err(init_esmf(), "ESMF_Initialize")

  print *, "========================================================"
  print *, " AlarmInit unit test: start_hour x freq grid alignment"
  print *, "========================================================"

  ! freq >= 6: expect 6h-grid alignment across the realistic IAU domain
  ! (start hours are always multiples of 3 -- confirmed; hours like 1 or 23
  ! are not physically meaningful IAU offsets and are intentionally excluded)
  call run_case(freq=6,  start_hour=0,  err_count=total_errors)
  call run_case(freq=6,  start_hour=3,  err_count=total_errors)
  call run_case(freq=6,  start_hour=6,  err_count=total_errors)
  call run_case(freq=6,  start_hour=9,  err_count=total_errors)
  call run_case(freq=6,  start_hour=12, err_count=total_errors)
  call run_case(freq=6,  start_hour=15, err_count=total_errors)
  call run_case(freq=6,  start_hour=18, err_count=total_errors)
  call run_case(freq=6,  start_hour=21, err_count=total_errors)

  call run_case(freq=24, start_hour=0,  err_count=total_errors)
  call run_case(freq=24, start_hour=3,  err_count=total_errors)
  call run_case(freq=24, start_hour=6,  err_count=total_errors)
  call run_case(freq=24, start_hour=9,  err_count=total_errors)
  call run_case(freq=24, start_hour=12, err_count=total_errors)
  call run_case(freq=24, start_hour=15, err_count=total_errors)
  call run_case(freq=24, start_hour=18, err_count=total_errors)
  call run_case(freq=24, start_hour=21, err_count=total_errors)

  ! freq < 6: no correction should be applied, ever
  call run_case(freq=1,  start_hour=0,  err_count=total_errors)
  call run_case(freq=1,  start_hour=9,  err_count=total_errors)
  call run_case(freq=3,  start_hour=0,  err_count=total_errors)
  call run_case(freq=3,  start_hour=9,  err_count=total_errors)
  call run_case(freq=3,  start_hour=21, err_count=total_errors)

  print *, "========================================================"
  if (total_errors == 0) then
    print *, "SUCCESS: all AlarmInit cases passed"
  else
    print *, "FAILURE: ", total_errors, " assertions failed"
  end if
  print *, "========================================================"

  call esmf_err(finalize_esmf(), "ESMF_Finalize")

  if (total_errors == 0) then
    stop 0
  else
    stop 1
  end if

contains

  function init_esmf() result(rc)
    integer :: rc
    call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
  end function init_esmf

  function finalize_esmf() result(rc)
    integer :: rc
    call ESMF_Finalize(rc=rc)
  end function finalize_esmf

  subroutine run_case(freq, start_hour, err_count)
    integer, intent(in)    :: freq, start_hour
    integer, intent(inout) :: err_count

    type(ESMF_Clock)        :: clock
    type(ESMF_Calendar)     :: cal
    type(ESMF_Time)         :: startTime, refTime, ringTime, expectedTime
    type(ESMF_TimeInterval) :: timeStep, ringInterval, tincrement, alarmoffset
    type(ESMF_Alarm)        :: alarm

    integer :: ierr, rc
    integer :: toffset, ring_hour, ring_day
    integer :: expected_hour
    integer :: rcnt, step
    integer, parameter :: max_steps = 200   ! generous bound (100h at dt=30min); well beyond
                                              ! anything reachable in our tested start_hour/freq domain
    logical :: rang
    character(len=64) :: label

    write(label,'(A,I0,A,I0,A)') "freq=", freq, "h start_hour=", start_hour, "h"

    if (verbose) then
      print *, ""
      print '(A)', "=== "//trim(label)//" ==="
    end if

    cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, rc=ierr)
    call esmf_err(ierr, "ESMF_CalendarCreate")
    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, &
         calendar=cal, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeSet(startTime)")
    call ESMF_TimeIntervalSet(timeStep, s=1800, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeIntervalSet(timeStep)")

    clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, rc=ierr)
    call esmf_err(ierr, "ESMF_ClockCreate")

    ! --- Reproduce outputlog_init's toffset/alarmoffset computation exactly
    ! (plumbing to build RefTime -- the thing under test is AlarmInit's
    ! response to it, not this computation itself) ---
    call ESMF_TimeIntervalSet(tincrement, m=1, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeIntervalSet(tincrement)")

    if (mod(start_hour,6) /= 0) then
      toffset = start_hour - 6
    else
      toffset = 0
    end if

    if (freq >= 6) then
      alarmoffset = toffset*60*tincrement
    else
      alarmoffset = 0*tincrement
    end if
    refTime = startTime + alarmoffset

    if (verbose) then
      if (freq >= 6) then
        print '(A,I0,A)', "  model start hour is not on a 6h boundary by ", toffset, &
             "h -- production's toffset/alarmoffset correction applies (freq>=6)"
        print '(A,I0,A)', "  RefTime passed to AlarmInit is shifted by that same ", toffset, "h"
      else
        print '(A)', "  freq<6: no toffset correction applies, regardless of start_hour alignment"
      end if
    end if

    call AlarmInit(clock, alarm=alarm, option='nhours', opt_n=freq, opt_ymd=-999, &
         RefTime=refTime, alarmname='test_alarm', rc=ierr)
    call esmf_err(ierr, "AlarmInit")

    ! --- Find where the alarm ACTUALLY first rings, by stepping the clock
    ! the normal way (small dt, checked every step) -- NOT by querying the
    ! alarm's internal ringTime attribute directly (that produced garbage --
    ! either a real misuse or an ESMF quirk, not chased further), and NOT by
    ! taking one big jump of size freq and reading currTime afterward (that
    ! always reports start+freq by construction regardless of where the
    ! alarm actually rang inside the jump, so it can't detect a broken
    ! alignment correction -- exactly the thing this test needs to catch).
    ! This mirrors exactly how outputlog_freqn itself detects a ring.
    rang = .false.
    do step = 1, max_steps
      call ESMF_ClockAdvance(clock, ringingAlarmCount=rcnt, rc=ierr)
      call esmf_err(ierr, "ESMF_ClockAdvance")
      if (rcnt > 0) then
        rang = .true.
        exit
      end if
    end do
    call assert_true(rang, trim(label)//": alarm never rang within "//itoa(max_steps)//" steps", err_count)
    if (.not. rang) return

    if (verbose) then
      print '(A,I0,A)', "  alarm first rang after ", step, " steps of stepping the clock forward"
    end if

    call ESMF_ClockGet(clock, currTime=ringTime, rc=ierr)
    call esmf_err(ierr, "ESMF_ClockGet(currTime at ring)")

    call ESMF_TimeGet(ringTime, dd=ring_day, h=ring_hour, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeGet(ringTime)")

    if (verbose) then
      print '(A,I0,A,I0)', "  ring occurred on day ", ring_day, " at hour ", ring_hour
    end if

    ! --- PRIMARY: independent structural check (the code's own stated intent) ---
    if (freq >= 6) then
      if (verbose) print '(A)', "  PRIMARY check: does the ring hour land on a multiple of 6? "// &
           "(this is the whole point of the toffset correction)"
      call assert_true(mod(ring_hour,6) == 0, &
           trim(label)//": ring hour must land on a 6h boundary (got h="//itoa(ring_hour)//")", &
           err_count)
    else
      if (verbose) print '(A)', "  PRIMARY check: freq<6, so ring must be EXACTLY start_hour+freq, "// &
           "no correction applied"
      call ESMF_TimeIntervalSet(ringInterval, h=freq, rc=ierr)
      call esmf_err(ierr, "ESMF_TimeIntervalSet(ringInterval)")
      expectedTime = startTime + ringInterval
      call assert_time_equal(expectedTime, ringTime, &
           trim(label)//": freq<6 must have NO correction (ring = start+freq exactly)", err_count)
    end if

    ! --- SECONDARY: exact re-derived value (regression check; re-derives the
    ! same rewind-then-advance formula AlarmInit itself uses -- carries real
    ! oracle-mirroring risk, unlike the primary check above) ---
    expected_hour = predicted_ring_hour(start_hour, freq, toffset)
    if (verbose) print '(A)', "  SECONDARY (regression) check: does the exact ring hour match "// &
         "a hand-derived re-simulation of AlarmInit's own rewind-then-advance loop?"
    call assert_true(ring_hour == expected_hour, &
         trim(label)//": [regression] expected ring hour "//itoa(expected_hour)// &
         ", got "//itoa(ring_hour), err_count)

    if (verbose) then
      print '(A,I0)', "  -> final ring_hour=", ring_hour
    end if
  end subroutine run_case

  !> Independent re-derivation of AlarmInit's rewind-then-advance loop, for
  !> the regression check only. NOT used for the primary assertion.
  !> Directly simulates the loop (verified against an independent Python
  !> check) rather than a closed-form formula -- an earlier closed-form
  !> attempt had a sign/off-by-one error for large |toffset| cases.
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

  subroutine assert_true(condition, msg, err_count)
    logical,          intent(in)    :: condition
    character(len=*), intent(in)    :: msg
    integer,          intent(inout) :: err_count
    if (.not. condition) then
      print *, "  -> ASSERTION FAILED: ", trim(msg)
      err_count = err_count + 1
    end if
  end subroutine assert_true

  subroutine assert_time_equal(expected, actual, msg, err_count)
    type(ESMF_Time), intent(in)    :: expected, actual
    character(len=*), intent(in)    :: msg
    integer,          intent(inout) :: err_count
    if (.not. (expected == actual)) then
      print *, "  -> ASSERTION FAILED: ", trim(msg)
      err_count = err_count + 1
    end if
  end subroutine assert_time_equal

  function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=12) :: s
    write(s,'(I0)') i
  end function itoa

  subroutine esmf_err(rc, context)
    integer,          intent(in) :: rc
    character(len=*), intent(in) :: context
    if (rc /= ESMF_SUCCESS) then
      write(0,'(A,I0)') "FATAL (test_alarminit): "//trim(context)//": rc=", rc
      stop 99
    end if
  end subroutine esmf_err

end program test_alarminit
