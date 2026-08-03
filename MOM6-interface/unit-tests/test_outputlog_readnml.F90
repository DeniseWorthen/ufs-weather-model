!> Test code for outputlog_methods readnml
!!
!! Probes readnml's logic guards for permitted namelist uses for
!! configuring the outputlog functionality in the MOM6 NUOPC cap
!!
!> @authorDenise.Worthen@noaa.gov
!> @date 07-01-2026
program test_outputlog_readnml

  use test_utils
  use mom_outputlog_methods, only : setrequest, settype, setprefix

  implicit none

  integer, parameter :: nfreq = 4
  integer, parameter :: maxtests = 25

  integer :: validfreqs(nfreq) = (/1, 3, 6, 24/)

  integer           :: nml_fh(nfreq)
  character(len=12) :: nml_type(nfreq)
  character(len=12) :: nml_fnameprefix(nfreq)
  character(len=24) :: overlength_fnameprefix(nfreq)
  character(len=35) :: longfileprefix

  logical           :: requested(nfreq)
  character(len=7)  :: timereduce(nfreq)
  character(len=12) :: fnameroot(nfreq)

  character(len=128) :: testname
  character(len=256) :: errmsg
  character(len=256) :: assertmsg

  type(testsummary)  :: nmltests

  logical :: is_passing, assertrc
  integer :: nt,n,ierr
  ! debug printing
  logical :: verbose = .true.

  ! initialize test tracker
  call nmltests%init(maxtests)

  nt = 0
  ! ===========================================================================
  ! test setrequest
  ! ===========================================================================

  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: outputfh==0 disables logging'
  nml_fh = (/0,0,0,0/)

  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)
  is_passing = (ierr == 0 .and. .not. any(requested))

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: map request to canonical order'
  nml_fh = (/6,0,0,0/)

  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)
  is_passing = (ierr == 0 .and. requested(3) .and. .not. any(requested((/1,2,4/))))

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: map request to canonical order'
  nml_fh = (/0,24,0,1/)

  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)
  is_passing = (ierr == 0 .and. requested(1) .and. requested(4) .and. .not. any(requested((/2,3/))))

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: invalid frequency blocked'
  nml_fh = (/18,0,0,0/)

  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setrequest: duplicate frequencies blocked'
  nml_fh = (/24,24, 0, 0/)

  requested = setrequest(validfreqs, nml_fh, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ===========================================================================
  ! test settype
  ! ===========================================================================

  nt = nt+1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: out-of-order inputs map correctly to canonical slots'
  nml_fh = (/24, 3, 0, 0/)
  requested = (/.false., .true., .false., .true./)
  nml_type = (/ character(len=12) :: 'average', 'none', '', '' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)
  is_passing = (ierr == 0 .and. trim(timereduce(2)) == 'none' .and. trim(timereduce(4)) == 'average')

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt+1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: lower-case strings map correctly'
  nml_fh = (/1,6,0,0/)
  requested = (/.true., .false., .true., .false./)
  nml_type = (/ character(len=12) :: 'none', 'average', '', '' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)
  is_passing = (ierr == 0 .and. trim(timereduce(1)) == 'none' .and. trim(timereduce(3)) == 'average')

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: upper-case strings not allowed'
  nml_fh = (/3,24,0,0/)
  requested = (/.false., .true., .false., .true./)
  nml_type = (/ character(len=12) :: '', 'NONE', '', 'AVERAGE' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: invalid string or typo'
  nml_fh = (/3,24,0,0/)
  nml_type = (/ character(len=12) :: '', 'snapshot', '', 'avg' /)
  requested = (/.false., .true., .false., .true./)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: empty string on an active frequency defaults to average'
  nml_fh = (/1,6,0,0/)
  requested = (/ .true., .false., .true., .false. /)
  nml_type = (/ character(len=12) :: 'none', '', '', '' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)
  is_passing = (ierr == 0 .and. trim(timereduce(1)) == 'none' .and. trim(timereduce(3)) == 'average')

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' settype: mis-aligned type for active frequency'
  nml_fh = (/1,6,0,0/)
  requested = (/ .true., .false., .true., .false. /)
  nml_type = (/ character(len=12) :: 'none', '', 'average', '' /)

  timereduce = settype(validfreqs, requested, nml_fh, nml_type, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ===========================================================================
  ! test setprefix
  ! ===========================================================================

  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: single request, blank file prefix defaults to ocn'
  nml_fh = (/1,0,0,0/)
  requested = (/ .true., .false., .false., .false. /)
  nml_fnameprefix = (/ character(len=12) :: '', '', '', '' /) ! Active slot 1 is empty

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)
  is_passing = (ierr == 0 .and. trim(fnameroot(1)) == 'ocn_')

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: multi-request, each must set file prefix'
  nml_fh = (/1,3,0,0/)
  requested = (/ .true., .true., .false., .false. /)
  nml_fnameprefix = (/ character(len=12) :: 'ocn_01h', 'ocn_03h', '', '' /)

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)
  is_passing = (ierr == 0 .and. trim(fnameroot(1)) == 'ocn_01h' .and. trim(fnameroot(2)) == 'ocn_03h')

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: multi-request, only one file prefix'
  nml_fh = (/1,3,0,0/)
  requested = (/ .true., .true., .false., .false. /)
  nml_fnameprefix = (/ character(len=12) :: 'ocn_01h', '', '', '' /)

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: multi-request, duplicate file prefix'
  nml_fh = (/1,3,0,0/)
  requested = (/ .true., .true., .false., .false. /)
  nml_fnameprefix = (/ character(len=12) :: 'ocn', 'ocn', '', '' /)

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: misaligned file prefix on active slot fails'
  nml_fh = (/1,24,0,0/)
  requested = (/ .true., .false., .false., .true. /)
  nml_fnameprefix = (/ character(len=12) :: 'ocn_1h', '', 'ocn_daily', '' /)

  fnameroot = setprefix(validfreqs, requested, nml_fh, nml_fnameprefix, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  write(testname,'(A,I2.2,A)')'test ',nt,' setprefix: specified file prefix too long'
  nml_fh = (/6,0,0,0/)
  requested = (/ .false., .false., .true., .false. /)
  longfileprefix = 'prefix_is_too_long_and_is_truncated'
  overlength_fnameprefix(:) = ''
  overlength_fnameprefix(1) = longfileprefix

  fnameroot = setprefix(validfreqs, requested, nml_fh, overlength_fnameprefix, errmsg, ierr)
  is_passing = (ierr /= 0)

  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(nmltests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  ! Test results
  ! ------------------

  print '(3(A,I0))','Total tests = ',nmltests%count,' Passing = ',nmltests%npass,' Failing = ',nmltests%nfail
  if (nmltests%nfail > 0) then
     print '(A)', 'FAIL: At least one test failed '
     stop 1
  else
     do n = 1,nmltests%count
        if (verbose .and. len_trim(nmltests%errmessage(n)%str) > 0) then
           print '(A)', trim(nmltests%testmessage(n)%str)//'  ['//trim(nmltests%errmessage(n)%str)//']'
        else
           print '(A)', trim(nmltests%testmessage(n)%str)
        endif
     enddo
  endif

end program test_outputlog_readnml
