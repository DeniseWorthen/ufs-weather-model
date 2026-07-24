program test_file

  use mom_outputlog_methods, only : get_file_state, file_is_complete
  use mpi_f08,               only : MPI_Init, MPI_Finalize, MPI_Comm_rank, MPI_Barrier, MPI_COMM_WORLD
  use netcdf

  implicit none
                               ! 00+1d  ! 06+1d
  !------6------12------18-------24-----30

  integer, parameter :: nhours = 5
  real(kind=8), parameter ::  hours(nhours) = (/6.0, 12.0, 18.0, 24.0, 30.0/)

  real(kind=8) :: dtadvance, time

  integer, allocatable :: nlens(:), fsizes(:)
  logical, allocatable :: present(:)

  integer :: n, hour, cnt, nsteps
  character(len=7) :: timereduce

  dtadvance = 0.5   ! 30min
  time = 0.0
  cnt = 0

  nsteps = nint(hours(3) / dtadvance)

  do n = 1,nsteps
     time = time + dtadvance
     print *, n, time
  enddo

  allocate(nlens(1:nsteps), source = nf90_fill_int)
  allocate(fsizes(1:nsteps), source = nf90_fill_int)
  allocate(present(1:nsteps), source = .false.)

  timereduce = 'average'



end program test_file
