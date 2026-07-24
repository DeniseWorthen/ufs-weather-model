module outputlog_tracker_mod
  implicit none

  type :: output_file_state
    character(len=256) :: current_filename = ""
    logical :: chkflag = .true.
    logical :: is_complete = .false.
    integer :: initial_size = 0
  end type output_file_state

contains

  subroutine update_file_state(state, filename, current_size)
    type(output_file_state), intent(inout) :: state
    character(len=*), intent(in)           :: filename
    integer, intent(in)                    :: current_size

    ! Detect a new file period
    if (trim(filename) /= trim(state%current_filename)) then
      state%current_filename = trim(filename)
      state%chkflag = .true.
      state%is_complete = .false.
      state%initial_size = current_size
      return
    end if

    ! If we are checking this file, see if it grew
    if (state%chkflag) then
      if (current_size > state%initial_size) then
        state%is_complete = .true.
        state%chkflag = .false.
      else
        state%is_complete = .false.
      end if
    end if

  end subroutine update_file_state

end module outputlog_tracker_mod
