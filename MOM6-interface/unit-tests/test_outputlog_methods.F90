program test_time_axis_log
  use outputlog_tracker_mod
  implicit none

  type(output_file_state) :: tracker
  integer :: num_errors = 0

  ! Time variables (in seconds)
  integer :: dt
  integer :: current_time, end_time, start_time, start_hour
  integer :: elapsed_sec, time_in_window, window_index
  integer :: curr_hour, curr_min, next_time, next_hour, next_min

  ! Mock file variables
  character(len=256) :: mock_filename, timestring

  integer :: mock_size
  integer :: file_hour
  integer :: freq

  ! 1. Setup the Time Axis
  dt = 1800 !
  start_time = 6 * 3600 ! Start at 06:00:00
  end_time = start_time + (18 * 3600) ! Run for 18 hours
  freq = 6 ! output frequency in hours

  start_hour = start_time / 3600

  print *, "Starting Time-Axis Simulation..."
  print *, "dt = ", dt, " seconds. Running for 18 hours."
  print *, "--------------------------------------------------------"

  current_time = start_time

  ! 2. The Main Model Advance Loop
  do while (current_time <= end_time)

     ! Current time breakdown
     curr_hour = mod(current_time / 3600, 24)
     curr_min  = mod(current_time, 3600) / 60

     ! Advance (next) time breakdown
     next_time = current_time + dt
     next_hour = mod(next_time / 3600, 24)
     next_min  = mod(next_time, 3600) / 60

     ! --- A. Mock the FMS/Disk Environment ---
     elapsed_sec = current_time - start_time

     ! A freq-hour history file changes every (freq * 3600) seconds
     window_index = elapsed_sec / (freq * 3600)
     time_in_window = mod(elapsed_sec, freq * 3600)

     ! Construct the filename for a TIME AVERAGE.
     ! It is named at the midpoint of the current window.
     ! e.g., Start=6, freq=6 -> window_start=6, midpoint=6+(6/2)=9
     file_hour = mod(start_hour + (window_index * freq) + (freq / 2), 24)

     write(mock_filename, '("./MOM6_OUTPUT/ocn_2021_03_22_", I2.2, "_00.nc")') file_hour

     ! Mock the file size: small at the exact start of the window, large afterwards
     if (time_in_window == 0) then
        mock_size = 199276
     else
        mock_size = 90532460
     end if

     ! --- B. Call the Feature Being Tested ---
     call update_file_state(tracker, mock_filename, mock_size)

     write(timestring,'(4(A,I2.2))') "Time: ", curr_hour, ":", curr_min, " -> ", next_hour, ":", next_min

     ! Print the log exactly like your MOM_cap output (currTime -> advanceTime)
     print '(A, L1, A, I10)', trim(timestring)//" | " //trim(mock_filename)// " | chkflag: ", &
          tracker%chkflag, " | size: ", mock_size

     ! --- C. Dynamic Assertions ---
     if (time_in_window == 0) then
        ! Exactly at the new 6-hour boundary: file just appeared
        call assert_false(tracker%is_complete, "File should NOT be complete on creation", num_errors)
        call assert_true(tracker%chkflag, "chkflag MUST be true on new file", num_errors)
     else
        ! Any subsequent step in the 6-hour window: file has grown and should be locked
        call assert_true(tracker%is_complete, "File should be complete after first step", num_errors)
        call assert_false(tracker%chkflag, "chkflag MUST flip to false", num_errors)
     end if

     ! Advance the clock
     current_time = current_time + dt

  end do

  ! 3. Final Report
  print *, "--------------------------------------------------------"
  if (num_errors == 0) then
     print *, "SUCCESS: Time-axis simulation passed with zero errors!"
     stop 0
  else
     print *, "FAILURE: ", num_errors, " tests failed during simulation."
     stop 1
  end if

contains

  ! Lightweight assertion routines
  subroutine assert_true(condition, msg, err_count)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: msg
    integer, intent(inout) :: err_count
    if (.not. condition) then
       print *, "  -> ASSERTION FAILED: ", trim(msg)
       err_count = err_count + 1
    end if
  end subroutine assert_true

  subroutine assert_false(condition, msg, err_count)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: msg
    integer, intent(inout) :: err_count
    if (condition) then
       print *, "  -> ASSERTION FAILED: ", trim(msg)
       err_count = err_count + 1
    end if
  end subroutine assert_false

end program test_time_axis_log
