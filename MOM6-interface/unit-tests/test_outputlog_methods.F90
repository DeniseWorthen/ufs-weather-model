program test_outputlog_methods

  use mom_outputlog_methods, only : setrequest, settype, setprefix

  implicit none

  integer, parameter :: nfreq = 4
  integer, parameter :: maxtests = 25

  integer :: validfreqs(nfreq) = (/1, 3, 6, 24/)

  integer           :: nml_fh(nfreq)
  character(len=12) :: nml_type(nfreq)
  character(len=12) :: nml_fnameprefix(nfreq)
  character(len=24) :: overlength_fnameprefix(nfreq)
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
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: outputfh==0 disables logging :'
  nml_fh = (/0,0,0,0/)
  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: map request to canonical order :'
  nml_fh = (/6,0,0,0/)
  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: map request to canonical order :'
  nml_fh = (/0,24,0,1/)
  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: invalid frequency blocked :'
  nml_fh = (/18,0,0,0/)
  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: duplicate frequencies blocked :'
  nml_fh = (/24,24, 0, 0/)
  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: lower-case strings map correctly :'
  nml_fh = (/1,6,0,0/)
  requested = (/.true., .false., .true., .false./)
  nml_type = (/ character(len=12) :: 'none', 'average', '', '' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: upper-case strings not allowed :'
  nml_fh = (/3,24,0,0/)
  requested = (/.false., .true., .false., .true./)
  nml_type = (/ character(len=12) :: '', 'NONE', '', 'AVERAGE' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: invalid string or typo :'
  nml_fh = (/3,24,0,0/)
  nml_type = (/ character(len=12) :: '', 'snapshot', '', 'avg' /)
  requested = (/.false., .true., .false., .true./)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: empty string on an active frequency defaults to average :'
  nml_fh = (/1,6,0,0/)
  requested = (/ .true., .false., .true., .false. /)
  nml_type = (/ character(len=12) :: 'none', '', '', '' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: mis-aligned type for active frequency :'
  nml_fh = (/1,6,0,0/)
  requested = (/ .true., .false., .true., .false. /)
  nml_type = (/ character(len=12) :: 'none', 'average', '', '' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL (error not caught)'
  endif

  ! ===========================================================================
  ! test setprefix
  ! ===========================================================================

  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: single request, blank rootname defaults to ocn :'
  nml_fh = (/1,0,0,0/)
  requested = (/ .true., .false., .false., .false. /)
  nml_fnameprefix = (/ character(len=12) :: '', '', '', '' /) ! Active slot 1 is empty

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: multi-request, each must set rootname :'
  nml_fh = (/1,3,0,0/)
  requested = (/ .true., .true., .false., .false. /)
  nml_fnameprefix = (/ character(len=12) :: 'ocn_01h', 'ocn_03h', '', '' /)

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)

  is_passing = (ierr == 0 .and. trim(fnameroot(1)) == 'ocn_01h' .and. trim(fnameroot(2)) == 'ocn_03h')
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' PASS'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' FAIL'
  endif

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: multi-request, only one rootname :'
  nml_fh = (/1,3,0,0/)
  requested = (/ .true., .true., .false., .false. /)
  nml_fnameprefix = (/ character(len=12) :: 'ocn_01h', '', '', '' /)

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: misaligned rootname on active slot fails :'
  nml_fh = (/1,24,0,0/)
  requested = (/ .true., .false., .false., .true. /)
  nml_fnameprefix = (/ character(len=12) :: 'ocn_1h', 'ocn_daily', '', '' /)

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)

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
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: specified rootname too long :'
  nml_fh = (/6,0,0,0/)
  requested = (/ .false., .false., .true., .false. /)
  longrootname = 'rootname_too_long_and_is_truncated'
  overlength_fnameprefix(:) = ''
  overlength_fnameprefix(3) = longrootname
  print *,overlength_fnameprefix

  fnameroot = setprefix(validfreqs, requested, nml_fh, overlength_fnameprefix, errmsg, ierr)

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
  if (ntests > maxtests) then
     print '(A)', 'FAIL: ntests > maxtests '
     stop 1
  else
     if (nfail == 0) then
        print '(A)', 'All tests passed '
     else
        print '(A)', 'FAIL: At least one test failed '
        stop 1
     endif
  endif

end program test_outputlog_methods
