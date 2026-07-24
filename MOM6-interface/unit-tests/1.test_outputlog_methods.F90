program time_axis_arrays
  implicit none

  integer, parameter :: SECONDS_PER_DAY = 86400
  integer, parameter :: SECONDS_PER_HOUR = 3600
  integer, parameter :: SECONDS_PER_MINUTE = 60

  integer :: dt
  integer :: timebeg, timeend
  integer :: num_steps
  integer :: i
  integer :: index

  ! Output frequency variables
  integer :: outputfh
  integer :: output_interval_sec

  character(len=60) :: fname
  logical :: use_filesize

  ! Arrays
  integer, allocatable :: t1(:), t2(:)
  logical, allocatable :: ring(:)
  integer, allocatable :: nlen(:), fsize(:)

  ! Temporary arrays to hold the dhms output for printing
  integer, dimension(4) :: d1,d2

  ! 1. Setup initial conditions
  dt = 900
  timebeg = 6 * SECONDS_PER_HOUR
  timeend = 1 * SECONDS_PER_DAY + 18 * SECONDS_PER_HOUR

  ! Setup output frequency (e.g., 1 for every hour, 6 for every 6 hours)
  outputfh = 6
  output_interval_sec = outputfh * SECONDS_PER_HOUR

  ! 2. Find the number of timesteps
  num_steps = (timeend - timebeg) / dt

  ! 3. Allocate the arrays
  allocate(t1(num_steps))
  allocate(t2(num_steps))
  allocate(ring(num_steps))
  allocate(nlen(num_steps))
  allocate(fsize(num_steps))

  ! 4. Derive the arrays and the ring logicals
  do i = 1, num_steps
     t1(i) = timebeg + (i - 1) * dt
     t2(i) = t1(i) + dt

     !Set the ring logical:
     !True if the total elapsed seconds at t2 divides evenly by the output interval
     ring(i) = (mod(t2(i), output_interval_sec) == 0)
  end do

  print *, "Testing Output Frequency: Every ", outputfh, " hour(s)"
  print *, "Step | t2 (D:H:M:S)      | Ring"
  print *, "-----------------------------------"

  ! 5. Print out ONLY the steps where ring is true, plus the first step as a baseline
  do i = 1, num_steps
     if (i == 1 .or. ring(i)) then
        d2 = get_dhms(t2(i))

        print '(I4, A, I2,A,I2.2,A,I2.2,A,I2.2, A, L1)', &
             i, " | ", &
             d2(1), ":", d2(2), ":", d2(3), ":", d2(4), " | ", &
             ring(i)
     end if
  end do

  fname = 'test.fh09.nc'; nlen = -1; fsize = -1; use_filesize = .true.
  index = extract_index( (/1, 12, 0, 0/), timebeg, dt)
  nlen(index+1) = 1
  fsize(index+1) = 1e3
  print *,index

  index = extract_index( (/1, 18, 0, 0/), timebeg, dt)
  nlen(index+1) = 1
  fsize(index+1) = 1e6
  print *,index

  do i = 1,num_steps
     d2 = get_dhms(t2(i))
     print *,i,d2(1:4),nlen(i),fsize(i)
  end do


  ! Clean up memory
  deallocate(t1)
  deallocate(t2)
  deallocate(ring)

contains


  ! Function to convert total seconds into a 4-element array [Day, Hour, Min, Sec]
  function get_dhms(total_sec) result(dhms)
    integer, intent(in) :: total_sec
    integer, dimension(4) :: dhms
    integer :: rem_sec

    dhms(1) = (total_sec / SECONDS_PER_DAY) + 1
    rem_sec = mod(total_sec, SECONDS_PER_DAY)

    dhms(2) = rem_sec / SECONDS_PER_HOUR
    rem_sec = mod(rem_sec, SECONDS_PER_HOUR)

    dhms(3) = rem_sec / SECONDS_PER_MINUTE
    dhms(4) = mod(rem_sec, SECONDS_PER_MINUTE)

  end function get_dhms

  function extract_index(dhms_arr, start_time, timestep) result(idx)
    integer, dimension(4), intent(in) :: dhms_arr
    integer, intent(in) :: start_time, timestep
    integer :: idx
    integer :: target_sec

    print *,dhms_arr

    ! 1. Convert dhms array back to continuous elapsed seconds
    ! Subtract 1 from day since Day 1 represents 0-86399 seconds
    target_sec = (dhms_arr(1) - 1) * SECONDS_PER_DAY + &
         dhms_arr(2) * SECONDS_PER_HOUR + &
         dhms_arr(3) * SECONDS_PER_MINUTE + &
         dhms_arr(4)
    print *,'extract 1 ',target_sec

    ! 2. Solve for 'i' using the set_t1 formula: t1 = start + (i - 1) * dt
    ! i = ((t1 - start) / dt) + 1
    idx = ((target_sec - start_time) / timestep) + 1
    print *,'extract 2 ',idx

  end function extract_index
end program time_axis_arrays
