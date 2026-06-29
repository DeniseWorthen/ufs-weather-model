program test_outputlog_methods

  use mom_outputlog_methods, only : setrequest, settype, setrootname

  implicit none

  integer, parameter :: nfreq = 4
  integer :: validfreqs(nfreq) = (/1, 3, 6, 24/)
  integer :: mock_fh(nfreq)
  character(len=32) :: mock_type(nfreq)

  logical :: requested(nfreq)
  logical :: avgtype(nfreq)

  ! Scoreboard tracking
  integer :: n_pass = 0
  integer :: n_fail = 0

  character(len=128) :: testmsg
  character(len=256) :: errmsg
  integer            :: i,ierr

  !
  ! test setrequest
  !
  testmsg = 'setrequest: all-zero array gracefully disables logging'
  mock_fh = (/0, 0, 0, 0/)
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  if (ierr == 0 .and. .not. any(requested)) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
    print *, "      -> Expected ierr=0 and all-false mask. Got ierr=", ierr
  endif

  !
  testmsg = 'setrequest: single standard frequency maps to canonical tier'
  mock_fh = (/6, 0, 0, 0/)
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  if (ierr == 0 .and. requested(3) .and. .not. any(requested((/1,2,4/)))) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
    print *, "      -> Mask mapping leaked or failed. Mask states: ", requested
  endif

  !
  testmsg = 'setrequest: multi standard frequency maps to canonical tier'
  mock_fh = (/24, 1, 0, 0/)
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  if (ierr == 0 .and. requested(4) .and. requested(1) .and. .not. any(requested((/2,3/)))) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
    print *, "      -> Mask mapping leaked or failed. Mask states: ", requested
  endif

  !
  testmsg = 'setrequest: illegal frequency (12h) '
  mock_fh = (/12, 0, 0, 0/)
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  if (ierr /= 0) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
  endif

  !
  testmsg = 'setrequest: duplicate frequencies (24, 24) are blocked'
  mock_fh = (/24, 24, 0, 0/)
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  if (ierr /= 0) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
    print *, "      -> CRITICAL: Allowed duplicate output_fh variables to conflict!"
  endif

  !
  ! test settype
  !
  testmsg = 'settype: standard lower-case strings map correctly'
  mock_fh = (/1, 6/)
  requested = (/.true., .true., .false., .false./)
  mock_type = (/ character(len=32) :: 'none', 'average', '', '' /)

  avgtype = settype(validfreqs, requested, mock_fh, mock_type, errmsg, ierr)

  if (ierr == 0 .and. trim(avgtype(1)) == 'none' .and. trim(avgtype(2)) == 'average') then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
  endif

  testmsg = 'settype: invalid/typo '
  mock_fh = (/3,24/)
  mock_type = (/ character(len=32) :: 'snapshot', 'average', '', '' /)
  requested = (/.false., .true., .false., .true./)
  avgtype = settype(validfreqs, requested, mock_fh, mock_type, errmsg, ierr)

  if (ierr /= 0) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
  endif

  testmsg = 'settype: empty string on an active frequency defaults to average'
  mock_fh = (/1,6/)
  requested = (/ .true., .false., .true., .false. /)
  mock_type = (/ character(len=32) :: 'none', '', '', '' /)

  requested = settype(requested, mock_type, errmsg, ierr)

  if (ierr == 0 .and. trim(avgtype(1)) == 'none' .and. trim(avgtype(2)) == 'average') then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
    print *, "      -> Default fallback failed. Res=", avgtype
  endif

  testmsg = 'settype: mis-aligned active requests and types fail strictly'
  requested = (/ .true., .false., .false., .true. /)
  mock_type = (/ character(len=32) :: 'none', 'none', '', '' /)

  avgtype = settype(requested, mock_type, errmsg, ierr)

  ! Verification: ierr MUST be non-zero because an active slot was left unconfigured
  if (ierr /= 0) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
    print *, "      -> CRITICAL: Allowed misaligned array to pass without error!"
    print *, "         Returned Types: ", (/ (trim(avgtype(i)), i=1,nfreq) /)
  endif

  print *, "-------------------------------------------------------"
  print '(A, I4)', " TOTAL TESTS PASSED: ", n_pass
  print '(A, I4)', " TOTAL TESTS FAILED: ", n_fail
  print *, "-------------------------------------------------------"

  if (n_fail > 0) then
    stop 1
  else
    print *, "ALL TESTS PASSED SUCCESSFULLY."
 endif

end program test_outputlog_methods
