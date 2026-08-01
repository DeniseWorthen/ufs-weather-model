program test_outputlog_freqn

  use ESMF
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm, MPI_Comm_rank, MPI_COMM_WORLD, MPI_Barrier
  use mom_outputlog_methods, only : outputlog_config_type, outputlog_state_type, get_timestr
  use MOM_cap_outputlog,     only : outputlog_freqn
  use MOM_cap_time,          only : AlarmInit
  use nc_fixture_mod,        only : create_schema, write_record, write_bulk_data, write_padding

  implicit none

  ! ============================================================================
  ! Option A: drives the real outputlog_freqn through a real ESMF_Clock, with
  ! the alarm built via the real production AlarmInit (not a hand-rolled
  ! ESMF_AlarmCreate -- validated separately in test_alarminit.F90 first),
  ! keeping a real fixture file on disk in sync with
  ! wherever the clock currently is, and counting how many completions
  ! outputlog_freqn itself reports (via the filecomplete_out/filecomplete_lstop_out
  ! observability arguments added to outputlog_freqn for this purpose).
  !
  ! The test independently computes each ring's expected FILENAME (using the
  ! same offset formula + the real get_timestr function production uses) only
  ! so it knows where to place fixture files -- this is plumbing, not the
  ! oracle. If the guess were wrong, outputlog_freqn would look for a
  ! different path, find nothing, and the test would fail loudly (wrong
  ! use_filesize / never-completing), not silently pass. The actual
  ! completion COUNT (the real oracle) is never independently recomputed.
  !
  ! Completion timing is scheduled per-ring via independent "pending
  ! transition" entries (target_tick, filename), not a single reset-on-next-
  ! ring scalar -- this lets a specific ring's fixture be scheduled to
  ! complete AFTER a later ring has already superseded it (an "overrun" case:
  ! the write does eventually land on disk, but too late for outputlog_freqn
  ! to ever see it, since state_n%filename has already moved on), alongside
  ! the normal case of completing promptly after its own ring.
  !
  ! Restart pairing: lastrestart is driven from a test-supplied, ascending
  ! list of restart hours (elapsed hours from start, matching the "forecast
  ! hour" convention in production's log format) rather than a fixed dummy.
  ! No lag/lookback is modeled for restarts (confirmed: MOM writes restarts
  ! synchronously at the end of the advance that reaches the target hour).
  ! ============================================================================

  type(MPI_Comm) :: comm
  integer        :: rank, ierr, rootpe
  logical        :: isroot
  integer        :: total_errors, observed_errors, hypothesis_errors
  logical        :: verbose

  integer, parameter :: base_yy = 2021, base_mm = 3, base_dd = 22

  comm = MPI_COMM_WORLD
  rootpe = 0
  total_errors = 0
  observed_errors = 0
  hypothesis_errors = 0
  verbose = .false.

  call MPI_Init(ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  isroot = (rank == rootpe)
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=ierr)
  call esmf_err(ierr, "ESMF_Initialize")

  print *, "========================================================"
  print *, " Option A: outputlog_freqn ESMF Clock/Alarm-driven tests"
  print *, "========================================================"

  ! --- OBSERVED: configurations confirmed against real production debug logs.
  ! ticks_to_complete=1 in every OBSERVED case -- confirmed as the actual
  ! observed behavior (completion detected on the very next advance after the
  ! ring), though NOT guaranteed by construction; production keeps checking
  ! until complete, however long that takes. ---

  call run_case('OBSERVED', dt=1800, freq=6, timereduce='average', use_filesize=.false., &
       start_hour=6, run_hours=24, ticks_to_complete=1, &
       expected_completions=3, err_count=total_errors, is_hypothesis=.false.)

  call run_case('OBSERVED', dt=1800, freq=24, timereduce='average', use_filesize=.true., &
       start_hour=0, run_hours=54, ticks_to_complete=1, &
       expected_completions=3, err_count=total_errors, is_hypothesis=.false.)

  call run_case('OBSERVED', dt=1800, freq=1, timereduce='none', use_filesize=.false., &
       start_hour=6, run_hours=6, ticks_to_complete=1, &
       expected_completions=6, err_count=total_errors, is_hypothesis=.false.)

  ! --- HYPOTHESIS: probing sequencing edge cases not directly observed ---

  ! Ring 2's file completes LATE -- specifically after ring 3 has already
  ! fired and superseded it (an "overrun": ticks_to_complete deliberately
  ! exceeds one full interval's worth of ticks). The write does eventually
  ! land on disk, but outputlog_freqn should never see it, since
  ! state_n%filename has already moved on to ring 3's file by the time it
  ! completes. Confirms this is silently (and safely) dropped, not
  ! double-counted or a crash.
  call run_case('HYPOTHESIS', dt=1800, freq=6, timereduce='average', use_filesize=.false., &
       start_hour=6, run_hours=24, ticks_to_complete=1, &
       override_ring=[2], override_ticks=[20], &
       expected_completions=3, err_count=total_errors, is_hypothesis=.true.)

  ! A ring's file NEVER completes at all (override_ticks=-1 = no transition
  ! ever scheduled) -- confirms the phantom/never-completing file just sits
  ! pending harmlessly and doesn't block subsequent rings.
  call run_case('HYPOTHESIS', dt=1800, freq=6, timereduce='average', use_filesize=.false., &
       start_hour=6, run_hours=24, ticks_to_complete=1, &
       override_ring=[2], override_ticks=[-1], &
       expected_completions=3, err_count=total_errors, is_hypothesis=.true.)

  ! Restart pairing: fast history output (6h) against a much slower restart
  ! cadence (36h) -- many history completions should pair with the SAME
  ! restart until the next restart hour is reached.
  call run_case('HYPOTHESIS', dt=1800, freq=6, timereduce='average', use_filesize=.true., &
       start_hour=0, run_hours=48, ticks_to_complete=1, &
       restart_hours=[36], &
       expected_completions=2, err_count=total_errors, is_hypothesis=.true.)

  ! Misconfigured diag_table: nml says 'average' (mid-point offset, freq*1.5),
  ! but the file actually on disk is end-of-window labeled (offset=freq) --
  ! e.g. someone specified end-of-period naming in diag_table without
  ! updating input_nml to match. outputlog_freqn has no way to detect this
  ! mismatch (it doesn't read diag_table); the file it looks for never
  ! exists under the name it computes. Confirms the failure mode is safe:
  ! permanently pending, never falsely reported complete, no crash --
  ! not "does it still work" (there's no code path for that), but "does a
  ! configuration mismatch fail safely."
  call run_case('HYPOTHESIS', dt=1800, freq=6, timereduce='average', use_filesize=.false., &
       start_hour=6, run_hours=24, ticks_to_complete=1, &
       misconfigured_diag_table=.true., &
       expected_completions=0, err_count=total_errors, is_hypothesis=.true.)

  print *, "========================================================"
  print *, "Observed-case errors:   ", observed_errors
  print *, "Hypothesis-case errors: ", hypothesis_errors
  print *, "========================================================"

  call ESMF_Finalize(rc=ierr)
  call esmf_err(ierr, "ESMF_Finalize")

  if (total_errors == 0) then
    if (isroot) print *, "SUCCESS: all Option A cases passed"
    stop 0
  else
    if (isroot) print *, "FAILURE: ", total_errors, " assertions failed"
    stop 1
  end if

contains

  subroutine run_case(case_kind, dt, freq, timereduce, use_filesize, start_hour, run_hours, &
       ticks_to_complete, expected_completions, err_count, is_hypothesis, &
       override_ring, override_ticks, restart_hours, misconfigured_diag_table)
    character(len=*), intent(in)    :: case_kind
    integer,           intent(in)    :: dt, freq, start_hour, run_hours
    character(len=*), intent(in)    :: timereduce
    logical,           intent(in)    :: use_filesize
    integer,           intent(in)    :: ticks_to_complete, expected_completions
    integer,           intent(inout) :: err_count
    logical,           intent(in)    :: is_hypothesis
    integer, optional, intent(in)    :: override_ring(:), override_ticks(:)
    integer, optional, intent(in)    :: restart_hours(:)
    logical, optional, intent(in)    :: misconfigured_diag_table

    integer :: ierr_local
    ierr_local = 0

    if (isroot) then
      print *, ""
      print '(A)', "["//trim(case_kind)//"] dt="//itoa(dt)//" freq="//itoa(freq)// &
           " "//trim(timereduce)//" use_filesize="//merge('T','F',use_filesize)// &
           " start="//itoa(start_hour)//"h run="//itoa(run_hours)//"h"
    end if

    call run_test(dt, freq, timereduce, use_filesize, start_hour, run_hours, &
         ticks_to_complete, expected_completions, ierr_local, &
         override_ring, override_ticks, restart_hours, misconfigured_diag_table)

    err_count = err_count + ierr_local
    if (is_hypothesis) then
      hypothesis_errors = hypothesis_errors + ierr_local
    else
      observed_errors = observed_errors + ierr_local
    end if
  end subroutine run_case

  subroutine run_test(dt, freq, timereduce, use_filesize, start_hour, run_hours, &
       ticks_to_complete, expected_completions, err_count, &
       override_ring, override_ticks, restart_hours, misconfigured_diag_table)
    integer,           intent(in)    :: dt, freq, start_hour, run_hours
    character(len=*), intent(in)    :: timereduce
    logical,           intent(in)    :: use_filesize
    integer,           intent(in)    :: ticks_to_complete, expected_completions
    integer,           intent(inout) :: err_count
    integer, optional, intent(in)    :: override_ring(:), override_ticks(:)
    integer, optional, intent(in)    :: restart_hours(:)
    logical, optional, intent(in)    :: misconfigured_diag_table

    type(ESMF_Clock)         :: clock
    type(ESMF_Time)          :: startTime, stopTime, currTime, nextTime, refTime, lastrestart
    type(ESMF_TimeInterval)  :: timeStep, ringOffset, tincrement, alarmoffset
    type(outputlog_config_type) :: cf_n
    type(outputlog_state_type)  :: state_n
    integer                  :: toffset

    integer :: ierr, rc
    integer :: ring_index, absolute_tick, this_ticks
    integer :: num_completions
    logical :: ringing, filecomplete, filecomplete_lstop
    character(len=16)  :: timestr
    character(len=256) :: ring_filename, outputdir

    integer, parameter :: max_pending = 50
    type :: pending_transition_type
      character(len=256) :: filename = ""
      integer :: target_tick = -1
      logical :: active = .false.
    end type pending_transition_type
    type(pending_transition_type) :: pending(max_pending)
    integer :: n_pending, i

    type(ESMF_Time), allocatable :: restart_targets(:)
    type(ESMF_TimeInterval) :: restart_offset
    type(ESMF_TimeInterval) :: end_of_window_offset
    logical :: misconfigured

    misconfigured = .false.
    if (present(misconfigured_diag_table)) misconfigured = misconfigured_diag_table

    num_completions = 0
    ring_index = 0
    absolute_tick = 0
    n_pending = 0
    outputdir = "./"

    ! Each ctest invocation runs every case in ONE executable, sequentially --
    ! fixture files from a prior case (or a prior run left on disk) must not
    ! leak into this one. A stale file matching this case's computed name
    ! was confirmed to cause a false completion. Clean up first, and
    ! synchronize before any rank proceeds (execute_command_line only blocks
    ! the calling rank, not the others).
    if (isroot) then
      call execute_command_line('rm -f '//trim(outputdir)//'*.nc', wait=.true.)
    end if
    call MPI_Barrier(comm, ierr)
    if (ierr /= 0) then
      write(0,'(A)') "FATAL (test_outputlog_freqn): MPI_Barrier (post-cleanup) failed"
      stop 99
    end if

    ! --- Build the real ESMF_Clock, then a real production AlarmInit call
    ! (not a hand-rolled ESMF_AlarmCreate) -- validated separately in
    ! test_alarminit.F90 first, per plan, before being incorporated here.
    ! tincrement must be built before alarmoffset, since alarmoffset is
    ! expressed in units of tincrement (1 minute). ---
    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeSet(startTime)")
    call ESMF_TimeIntervalSet(timeStep, s=dt, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeIntervalSet(timeStep)")
    call ESMF_TimeIntervalSet(ringOffset, h=run_hours, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeIntervalSet(ringOffset)")
    stopTime = startTime + ringOffset

    clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=ierr)
    call esmf_err(ierr, "ESMF_ClockCreate")

    call ESMF_TimeIntervalSet(tincrement, m=1, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeIntervalSet(tincrement)")

    ! Same toffset/alarmoffset computation as outputlog_init and
    ! test_alarminit.F90 -- validated there for the realistic (multiple-of-3
    ! start_hour) domain.
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

    call AlarmInit(clock, alarm=cf_n%alarm, option='nhours', opt_n=freq, opt_ymd=-999, &
         RefTime=refTime, alarmname='test_alarm', rc=ierr)
    call esmf_err(ierr, "AlarmInit")

    ! --- Build the rest of cf_n exactly as outputlog_init would ---
    call ESMF_TimeIntervalSet(end_of_window_offset, h=freq, rc=ierr)
    call esmf_err(ierr, "ESMF_TimeIntervalSet(end_of_window_offset)")
    cf_n%alarm_name  = "test_alarm"
    cf_n%opt_n       = freq
    cf_n%requested   = .true.
    cf_n%timereduce  = timereduce
    cf_n%fnameprefix = "ocn_"
    cf_n%fnamesuffix = ""
    if (trim(timereduce) == 'none') then
      cf_n%logname_fhoffset  = 0*tincrement
      cf_n%filename_fhoffset = 60*freq*tincrement
    else
      cf_n%logname_fhoffset  = 60*freq*tincrement
      cf_n%filename_fhoffset = 90*freq*tincrement
    end if

    state_n%chkfile_nextAdvance = .false.
    state_n%use_filesize        = .false.
    state_n%filename             = ""
    state_n%createsize            = 0

    ! --- Restart schedule: assumed ascending elapsed-hours-from-start list.
    ! No lag modeled -- a restart hour becomes valid the same tick currTime
    ! reaches it (confirmed: MOM writes restarts synchronously). ---
    if (present(restart_hours)) then
      allocate(restart_targets(size(restart_hours)))
      do i = 1, size(restart_hours)
        call ESMF_TimeIntervalSet(restart_offset, h=restart_hours(i), rc=ierr)
        call esmf_err(ierr, "ESMF_TimeIntervalSet(restart_offset)")
        restart_targets(i) = startTime + restart_offset
      end do
    else
      allocate(restart_targets(0))
    end if
    lastrestart = startTime

    do while (.not. ESMF_ClockIsStopTime(clock, rc=ierr))
      call esmf_err(ierr, "ESMF_ClockIsStopTime")
      call ESMF_ClockAdvance(clock, rc=ierr)
      call esmf_err(ierr, "ESMF_ClockAdvance")
      absolute_tick = absolute_tick + 1

      call ESMF_ClockGet(clock, currTime=currTime, rc=ierr)
      call esmf_err(ierr, "ESMF_ClockGet(currTime)")
      lastrestart = current_lastrestart(restart_targets, currTime, startTime)

      ringing = ESMF_AlarmIsRinging(cf_n%alarm, rc=ierr)   ! non-destructive query
      call esmf_err(ierr, "ESMF_AlarmIsRinging")

      if (ringing) then
        ring_index = ring_index + 1

        ! Determine this ring's filename using the same offset + the real
        ! get_timestr (plumbing only -- see module header comment).
        ! IMPORTANT: production computes this from ESMF_ClockGetNextTime
        ! (currTime + timeStep), called INSIDE outputlog_freqn, NOT from
        ! currTime directly -- must match exactly or the fixture lands at
        ! the wrong path and outputlog_freqn never finds it.
        call ESMF_ClockGetNextTime(clock, nextTime, rc=ierr)
        call esmf_err(ierr, "ESMF_ClockGetNextTime")

        ! When misconfigured, deliberately place the fixture under the WRONG
        ! (end-of-window) name -- the name outputlog_freqn itself computes
        ! (mid-point, via cf_n%filename_fhoffset) will never match it.
        if (misconfigured) then
          timestr = get_timestr(nextTime - end_of_window_offset, rc=ierr)
        else
          timestr = get_timestr(nextTime - cf_n%filename_fhoffset, rc=ierr)
        end if
        call esmf_err(ierr, "get_timestr")
        ring_filename = trim(outputdir)//trim(cf_n%fnameprefix)//trim(timestr)//'.nc' &
             //trim(cf_n%fnamesuffix)

        ! Place the fixture in its INCOMPLETE state before outputlog_freqn's
        ! own get_file_state call, so use_filesize is inferred correctly.
        if (isroot) then
          call create_schema(trim(ring_filename))
          if (use_filesize) then
            call write_record(trim(ring_filename))
          else
            call write_padding(trim(ring_filename))
          end if
        end if

        ! Schedule this ring's completion transition -- per-ring override if
        ! given, else the default. -1 = never scheduled at all.
        this_ticks = lookup_override(ring_index, override_ring, override_ticks, ticks_to_complete)
        if (this_ticks > 0) then
          n_pending = n_pending + 1
          pending(n_pending)%filename    = ring_filename
          pending(n_pending)%target_tick = absolute_tick + this_ticks
          pending(n_pending)%active      = .true.
        end if
      end if

      ! Fire any pending transitions scheduled for THIS tick -- independent
      ! of whichever ring is currently tracked, so a late (overrun) write
      ! still lands on disk even after being superseded.
      do i = 1, n_pending
        if (pending(i)%active .and. pending(i)%target_tick == absolute_tick) then
          if (isroot) then
            if (use_filesize) then
              call write_bulk_data(trim(pending(i)%filename))
            else
              call write_record(trim(pending(i)%filename))
            end if
          end if
          pending(i)%active = .false.
        end if
      end do

      call outputlog_freqn(clock, cf_n, state_n, comm, isroot, rootpe, outputdir, tincrement, &
           lastrestart, verbose, atStopTime=.false., rc=rc, &
           filecomplete_out=filecomplete, filecomplete_lstop_out=filecomplete_lstop)
      call esmf_err(rc, "outputlog_freqn (main loop)")
      if (filecomplete) then
        num_completions = num_completions + 1
        call assert_time_equal(lastrestart, state_n%time_lastrestart, &
             "Restart pairing at completion (main loop)", err_count)
      end if
      if (filecomplete_lstop) num_completions = num_completions + 1
    end do

    ! --- Finalize: mirrors mom_cap.F90's real double-call pattern ---
    call ESMF_ClockGet(clock, currTime=currTime, rc=ierr)
    call esmf_err(ierr, "ESMF_ClockGet(currTime, finalize)")
    lastrestart = current_lastrestart(restart_targets, currTime, startTime)

    call outputlog_freqn(clock, cf_n, state_n, comm, isroot, rootpe, outputdir, tincrement, &
         lastrestart, verbose, atStopTime=.false., rc=rc, &
         filecomplete_out=filecomplete, filecomplete_lstop_out=filecomplete_lstop)
    call esmf_err(rc, "outputlog_freqn (finalize call 1)")
    if (filecomplete) then
      num_completions = num_completions + 1
      call assert_time_equal(lastrestart, state_n%time_lastrestart, &
           "Restart pairing at completion (finalize call 1)", err_count)
    end if

    call outputlog_freqn(clock, cf_n, state_n, comm, isroot, rootpe, outputdir, tincrement, &
         lastrestart, verbose, atStopTime=.true., rc=rc, &
         filecomplete_out=filecomplete, filecomplete_lstop_out=filecomplete_lstop)
    call esmf_err(rc, "outputlog_freqn (finalize call 2, atStopTime)")
    if (filecomplete) then
      num_completions = num_completions + 1
      call assert_time_equal(lastrestart, state_n%time_lastrestart, &
           "Restart pairing at completion (finalize call 2, regular)", err_count)
    end if
    if (filecomplete_lstop) then
      num_completions = num_completions + 1
      call assert_time_equal(lastrestart, state_n%time_lastrestart, &
           "Restart pairing at completion (finalize call 2, lstop)", err_count)
    end if

    call assert_equal(expected_completions, num_completions, &
         "Total completions must match expected_completions", err_count)

    if (isroot) then
      if (err_count == 0) then
        print *, "  -> Passed (", num_completions, " completions)"
      else
        print *, "  -> FAILED (", num_completions, " completions, expected ", expected_completions, ")"
      end if
    end if
  end subroutine run_test

  !> Looks up a per-ring override from paired (ring, ticks) arrays; falls
  !> back to default_val if no override is given for this ring_index.
  function lookup_override(ring_index, override_ring, override_ticks, default_val) result(ticks)
    integer,           intent(in) :: ring_index, default_val
    integer, optional, intent(in) :: override_ring(:), override_ticks(:)
    integer :: ticks
    integer :: i

    ticks = default_val
    if (present(override_ring) .and. present(override_ticks)) then
      do i = 1, size(override_ring)
        if (override_ring(i) == ring_index) then
          ticks = override_ticks(i)
          return
        end if
      end do
    end if
  end function lookup_override

  !> Latest restart target <= currTime, or startTime if none reached yet.
  !> Assumes targets are ascending.
  function current_lastrestart(targets, currTime, startTime) result(t)
    type(ESMF_Time), intent(in) :: targets(:)
    type(ESMF_Time), intent(in) :: currTime, startTime
    type(ESMF_Time) :: t
    integer :: i

    t = startTime
    do i = 1, size(targets)
      if (targets(i) <= currTime) then
        t = targets(i)
      else
        exit
      end if
    end do
  end function current_lastrestart

  subroutine assert_equal(expected, actual, msg, err_count)
    integer,          intent(in)    :: expected, actual
    character(len=*), intent(in)    :: msg
    integer,          intent(inout) :: err_count
    if (expected /= actual .and. isroot) then
      print *, "  -> ASSERTION FAILED: ", trim(msg), " (expected ", expected, ", got ", actual, ")"
      err_count = err_count + 1
    end if
  end subroutine assert_equal

  subroutine assert_time_equal(expected, actual, msg, err_count)
    type(ESMF_Time), intent(in)    :: expected, actual
    character(len=*), intent(in)    :: msg
    integer,          intent(inout) :: err_count
    integer :: rc
    character(len=16) :: exp_str, act_str
    if (.not. (expected == actual) .and. isroot) then
      exp_str = get_timestr(expected, rc)
      act_str = get_timestr(actual, rc)
      print *, "  -> ASSERTION FAILED: ", trim(msg), " (expected ", trim(exp_str), ", got ", trim(act_str), ")"
      err_count = err_count + 1
    end if
  end subroutine assert_time_equal

  function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=12) :: s
    write(s,'(I0)') i
  end function itoa

  !> Fail loudly on any ESMF (or ESMF-wrapping, e.g. outputlog_freqn's own rc)
  !> error, rather than silently continuing with an invalid clock/alarm/state.
  subroutine esmf_err(rc, context)
    integer,          intent(in) :: rc
    character(len=*), intent(in) :: context
    if (rc /= ESMF_SUCCESS) then
      write(0,'(A,I0)') "FATAL (test_outputlog_freqn): "//trim(context)//": rc=", rc
      stop 99
    end if
  end subroutine esmf_err

end program test_outputlog_freqn
