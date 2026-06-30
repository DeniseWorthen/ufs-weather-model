program test_outputlog_methods

  use mom_outputlog_methods, only : setrequest, settype, setrootname

  implicit none

  integer, parameter :: nfreq = 4
  integer, parameter :: maxtests = 25

  integer :: validfreqs(nfreq) = (/1, 3, 6, 24/)

  integer           :: output_fh(nfreq)
  character(len=12) :: output_type(nfreq)
  character(len=12) :: output_rootname(nfreq)
  character(len=24) :: overlength_rootname(nfreq)
  character(len=34) :: longrootname

  logical           :: requested(nfreq)
  character(len=7)  :: timereduce(nfreq)
  character(len=12) :: fnameroot(nfreq)

  character(len=128) :: msg(maxtests)
  character(len=128) :: testname
  character(len=256) :: errmsg
  integer            :: i,nt,n,ierr

  logical :: is_passing
  integer :: npass = 0
  integer :: nfail = 0
  integer :: ntests = 0

  nt = 0
  ! ===========================================================================
  ! test setrequest
  ! ===========================================================================
#ifdef test
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: outputfh==0 disables logging'
  output_fh = (/0,0,0,0/)
  requested = setrequest(validfreqs, output_fh, errmsg, ierr)

  is_passing = (ierr == 0 .and. .not. any(requested))
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: map request to canonical order'
  output_fh = (/6,0,0,0/)
  requested = setrequest(validfreqs, output_fh, errmsg, ierr)

  is_passing = (ierr == 0 .and. requested(3) .and. .not. any(requested((/1,2,4/))))
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: map request to canonical order'
  output_fh = (/0,24,0,1/)
  requested = setrequest(validfreqs, output_fh, errmsg, ierr)

  is_passing = (ierr == 0 .and. requested(1) .and. requested(4) .and. .not. any(requested((/2,3/))))
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: invalid frequency blocked'
  output_fh = (/18,0,0,0/)
  requested = setrequest(validfreqs, output_fh, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: duplicate frequencies blocked'
  output_fh = (/24,24, 0, 0/)
  requested = setrequest(validfreqs, output_fh, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif

  ! ===========================================================================
  ! test settype
  ! ===========================================================================

  nt = nt+1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: lower-case strings map correctly'
  output_fh = (/1,6,0,0/)
  requested = (/.true., .false., .true., .false./)
  output_type = (/ character(len=12) :: 'none', 'average', '', '' /)

  timereduce = settype(validfreqs, requested, output_fh, output_type, errmsg, ierr)

  is_passing = (ierr == 0 .and. trim(timereduce(1)) == 'none' .and. trim(timereduce(2)) == 'average')
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: upper-case strings not allowed'
  output_fh = (/3,24,0,0/)
  requested = (/.false., .true., .false., .true./)
  output_type = (/ character(len=12) :: '', 'NONE', '', 'AVERAGE' /)

  timereduce = settype(validfreqs, requested, output_fh, output_type, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: invalid string or typo'
  output_fh = (/3,24,0,0/)
  output_type = (/ character(len=12) :: '', 'snapshot', '', 'avg' /)
  requested = (/.false., .true., .false., .true./)

  timereduce = settype(validfreqs, requested, output_fh, output_type, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: empty string on an active frequency defaults to average'
  output_fh = (/1,6,0,0/)
  requested = (/ .true., .false., .true., .false. /)
  output_type = (/ character(len=12) :: 'none', '', '', '' /)

  timereduce = settype(validfreqs, requested, output_fh, output_type, errmsg, ierr)

  is_passing = (ierr == 0 .and. trim(timereduce(1)) == 'none' .and. trim(timereduce(3)) == 'average')

  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: mis-aligned type for active frequency'
  output_fh = (/1,6,0,0/)
  requested = (/ .true., .false., .true., .false. /)
  output_type = (/ character(len=12) :: 'none', 'average', '', '' /)

  timereduce = settype(validfreqs, requested, output_fh, output_type, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif

  ! ===========================================================================
  ! test setrootname
  ! ===========================================================================

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrootname: single request, blank rootname defaults to ocn'
  output_fh = (/1,0,0,0/)
  requested = (/ .true., .false., .false., .false. /)
  output_rootname = (/ character(len=12) :: '', '', '', '' /) ! Active slot 1 is empty

  fnameroot = setrootname(validfreqs, requested, output_fh, output_rootname, errmsg, ierr)

  is_passing = (ierr == 0 .and. trim(fnameroot(1)) == 'ocn')
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrootname: multi-request, each must set rootname'
  output_fh = (/1,3,0,0/)
  requested = (/ .true., .true., .false., .false. /)
  output_rootname = (/ character(len=12) :: 'ocn_1h', 'ocn_3h', '', '' /)

  fnameroot = setrootname(validfreqs, requested, output_fh, output_rootname, errmsg, ierr)

  is_passing = (ierr == 0 .and. trim(fnameroot(1)) == 'ocn_1h' .and. trim(fnameroot(2)) == 'ocn_3h')
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrootname: multi-request, only one rootname'
  output_fh = (/1,3,0,0/)
  requested = (/ .true., .true., .false., .false. /)
  output_rootname = (/ character(len=12) :: 'ocn_1h', '', '', '' /)

  fnameroot = setrootname(validfreqs, requested, output_fh, output_rootname, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrootname: misaligned rootname on active slot fails strictly'
  output_fh = (/1,24,0,0/)
  requested = (/ .true., .false., .false., .true. /)
  output_rootname = (/ character(len=12) :: 'ocn_1h', 'ocn_daily', '', '' /)

  fnameroot = setrootname(validfreqs, requested, output_fh, output_rootname, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif
#endif
  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrootname: specified rootname too long'
  output_fh = (/6,0,0,0/)
  requested = (/ .false., .false., .true., .false. /)
  longrootname = 'rootname_too_long_and_is_truncated'
  overlength_rootname(:) = ''
  overlength_rootname(3) = longrootname
  print *,overlength_rootname

  fnameroot = setrootname(validfreqs, requested, output_fh, overlength_rootname, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif

  ! ------------------
  ! Test results
  ! ------------------
  ntests = nt
  !if (ntests > maxtests)
  !do n = 1,ntests
  !   print '(i4,A)',n,' '//trim(msg(n))
  !enddo

  !if (nfail /= 0) tests failed


end program test_outputlog_methods
