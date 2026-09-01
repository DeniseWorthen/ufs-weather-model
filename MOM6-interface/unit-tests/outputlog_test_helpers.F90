!> @file outputlog_test_helpers.F90
!> @brief Shared setup for outputlog unit tests.
!!
!> @date 08-12-2026
module outputlog_test_helpers

  use ESMF
  use mom_outputlog_methods, only : outputlog_config_type, outputlog_state_type
  use mom_outputlog_methods, only : get_timestr, set_toffset
  use MOM_cap_time,          only : AlarmInit
  use nc_fixture_mod,        only : create_schema, write_record, write_padding, write_bulk_data
  use test_utils,            only : esmf_err

  implicit none
  private

  public :: base_yy, base_mm, base_dd
  public :: setup_case, handlefiles

  integer, parameter :: base_yy = 2021   !< a standard start year
  integer, parameter :: base_mm = 3      !< a standard start month
  integer, parameter :: base_dd = 22     !< a standard start day

contains
  !> Build a real ESMF_Clock/alarm plus cf_n/state_n for one test case,
  !! exactly as outputlog_init would.
  !!
  !! @param[in]     start_hour     model start hour
  !! @param[in]     runhours       total run length in hours
  !! @param[in]     freq           output frequency in hours
  !! @param[in]     l_nfiles       number of IO-layout files (1 = single file)
  !! @param[in]     l_timereduce   'average' or 'none'
  !! @param[in]     debug_onroot   enable verbose setup printing
  !! @param[out]    modelClock     the constructed ESMF_Clock
  !! @param[out]    cf_n           this frequency's config
  !! @param[out]    state_n        this frequency's state
  !! @param[out]    rc             return code
  subroutine setup_case(start_hour, runhours, freq, l_nfiles, l_timereduce, debug_onroot, &
       modelClock, cf_n, state_n, rc)

    integer,                     intent(in)  :: start_hour, runhours, freq, l_nfiles
    character(len=*),            intent(in)  :: l_timereduce
    logical,                     intent(in)  :: debug_onroot
    type(ESMF_Clock),            intent(out) :: modelClock
    type(outputlog_config_type), intent(out) :: cf_n
    type(outputlog_state_type),  intent(out) :: state_n
    integer,                     intent(out) :: rc

    type(ESMF_TimeInterval) :: alarmoffset
    type(ESMF_Time)         :: startTime, currTime, stopTime
    type(ESMF_TimeInterval) :: timeStep, tincrement

    integer :: toffset
    character(len=16)  :: startstr, stopstr
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
       print '(/,A)','Clock will run from '//startstr//' to '//stopstr
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
       cf_n%filename_fhoffset = 60*freq*tincrement
    else
       cf_n%filename_fhoffset = 90*freq*tincrement
    endif

    state_n%filename            = ' '
    state_n%chkfile_nextAdvance = .false.
    state_n%use_filesize        = .false.
    state_n%filecomplete        = .false.
    state_n%createsize          = 0
    state_n%completesize        = 0

    ! Fixed dummy value -- each test drives/overrides this as needed
    ! (test_driver2.F90/test_freqn.F90 never asserts on it; the dedicated
    ! restart-pairing test drives it explicitly to match a real cadence).
    state_n%time_lastrestart    = startTime

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
  !> Create/complete a netCDF fixture file, matching the real DATM/ATM
  !! completion contract.
  !!
  !! @param[in]  isroot        .true. on the root PE
  !! @param[in]  fname         the fixture's path
  !! @param[in]  use_filesize  .true. for ATM-style (fsize-based) completion,
  !!                           .false. for DATM-style (nlen-based)
  !! @param[in]  mode          'create', 'complete', or 'create-complete'
  subroutine handlefiles(isroot, fname, use_filesize, mode)

    logical,          intent(in)  :: isroot
    character(len=*), intent(in)  :: fname
    logical,          intent(in)  :: use_filesize
    character(len=*), intent(in)  :: mode

    select case (mode)
    case ('create')
       if (isroot) then
          call create_schema(fname)
          if (use_filesize) then
             call write_record(fname)
          else
             call write_padding(fname)
          endif
       endif

    case ('complete')
       if (isroot) then
          if (use_filesize) then
             call write_bulk_data(fname)   ! fsize grows past createsize
          else
             call write_record(fname)      ! nlen 0->1
          endif
       endif

    case('create-complete')
       if (isroot) then
          call create_schema(fname)
          if (use_filesize) then
             call write_record(fname)
             call write_bulk_data(fname)   ! fsize grows past createsize
          else
             call write_padding(fname)
             call write_record(fname)      ! nlen 0->1
          endif
       endif

    case default
       if (isroot) then
          print '(A)',' ERROR: unknown case '
          stop
       endif
    end select

  end subroutine handlefiles

end module outputlog_test_helpers
