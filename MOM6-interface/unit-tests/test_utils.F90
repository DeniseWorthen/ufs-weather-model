module test_utils

  use ESMF, only : ESMF_SUCCESS

  implicit none

  private

  integer, parameter :: real_kind = selected_real_kind( 6) !< 4 byte real
  integer, parameter ::  dbl_kind = selected_real_kind(12) !< 8 byte real
  integer, parameter ::  int_kind = selected_int_kind ( 6) !< 4 byte integer
  integer, parameter :: int8_kind = selected_int_kind (13) !< 8 byte integer

  public :: testsummary, addresult, esmf_err, itoa
  public :: assert_equal

  type :: msg_type
     character(len=:), allocatable :: str
  end type msg_type

  type testsummary
     integer :: count = 0
     integer :: npass = 0
     integer :: nfail = 0
     logical,        allocatable :: teststatus(:)
     type(msg_type), allocatable :: testmessage(:)
     type(msg_type), allocatable :: errmessage(:)
   contains
     procedure :: init => init_summary
  end type testsummary

  interface assert_equal
     module procedure assert_int_scalar
     module procedure assert_real_scalar
     module procedure assert_double_scalar
     module procedure assert_double_1d
     module procedure assert_logical
  end interface assert_equal

  interface addresult
     module procedure add_test_result
  end interface addresult

contains

  subroutine init_summary(this, maxtests)

    class(testsummary), intent(inout) :: this
    integer,            intent(in)    :: maxtests

    allocate(this%teststatus(maxtests))
    allocate(this%testmessage(maxtests))
    allocate(this%errmessage(maxtests))

  end subroutine init_summary

  subroutine add_test_result(summary, passed, message, errormsg)

    type(testsummary), intent(inout) :: summary
    logical,           intent(in)    :: passed
    character(len=*),  intent(in)    :: message
    character(len=*),  intent(in)    :: errormsg

    summary%count = summary%count + 1
    if (passed) then
       summary%npass = summary%npass + 1
    else
       summary%nfail = summary%nfail + 1
    end if

    summary%teststatus(summary%count) = passed
    summary%testmessage(summary%count)%str = message
    summary%errmessage(summary%count)%str = errormsg
  end subroutine add_test_result

  subroutine assert_logical(actual, expected, msg, rc, returnmsg)

    logical,          intent(in)  :: actual, expected
    character(len=*), intent(in)  :: msg
    logical,          intent(out) :: rc
    character(len=*), intent(out) :: returnmsg

    rc = (actual .eqv. expected)
    if (rc) then
       returnmsg = "Pass: "// trim(msg)
    else
       write(returnmsg, '(2(a,L2))') "Fail: " // trim(msg) // " | Expected ", expected, ", got ", actual
    endif
  end subroutine assert_logical

  subroutine assert_int_scalar(actual, expected, msg, rc, returnmsg)
    integer(int_kind), intent(in)  :: actual, expected
    character(len=*),  intent(in)  :: msg
    logical,           intent(out) :: rc
    character(len=*),  intent(out) :: returnmsg

    rc = (actual == expected)
    if (rc) then
       returnmsg = "Pass: " // trim(msg)
    else
       write(returnmsg, '(2(a,i0))') "Fail: " // trim(msg) // " | Expected ", expected, ", got ", actual
    end if
  end subroutine assert_int_scalar

  subroutine assert_real_scalar(actual, expected, tol, msg, rc, returnmsg)
    real(real_kind),   intent(in)  :: actual, expected, tol
    character(len=*),  intent(in)  :: msg
    logical,           intent(out) :: rc
    character(len=*),  intent(out) :: returnmsg

    rc = (abs(actual - expected) <= tol)
    if (rc) then
       returnmsg = "Pass: " // trim(msg)
    else
       write(returnmsg, '(2(a,g15.8))') "Fail: " // trim(msg) // " | Expected ", expected, ", got ", actual
    end if
  end subroutine assert_real_scalar

  subroutine assert_double_scalar(actual, expected, tol, msg, rc, returnmsg)
    real(dbl_kind),    intent(in)  :: actual, expected, tol
    character(len=*),  intent(in)  :: msg
    logical,           intent(out) :: rc
    character(len=*),  intent(out) :: returnmsg

    rc = (abs(actual - expected) <= tol)
    if (rc) then
       returnmsg = "Pass: " // trim(msg)
    else
       write(returnmsg, '(2(a,g20.13))') "Fail: " // trim(msg) // " | Expected ", expected, ", got ", actual
    end if
  end subroutine assert_double_scalar

  subroutine assert_double_1d(actual, expected, tol, msg, rc, returnmsg)
    real(dbl_kind),    intent(in)  :: actual(:), expected(:), tol
    character(len=*),  intent(in)  :: msg
    logical,           intent(out) :: rc
    character(len=*),  intent(out) :: returnmsg

    rc = all(abs(actual - expected) <= tol)
    if (rc) then
       returnmsg = "Pass: " // trim(msg)
    else
       returnmsg = "Fail: " // trim(msg) // " | At least one element mismatched."
    end if
  end subroutine assert_double_1d

  function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=4) :: s

    write(s,'(I0)') i
  end function itoa

  subroutine esmf_err(rc, subname, context)
    integer,          intent(in) :: rc
    character(len=*), intent(in) :: subname
    character(len=*), intent(in) :: context

    if (rc /= ESMF_SUCCESS) then
       write(0,'(A,I0)') "FATAL ("//trim(subname)//") : "//trim(context)//": rc=", rc
       stop 99
    end if
  end subroutine esmf_err

end module test_utils
