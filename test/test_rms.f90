module test_rms
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use mod_stat, only : rms
    implicit none
    private
    public :: collect_rms

contains

    subroutine collect_rms(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("valid", test_rms_int5_4),&
            new_unittest("valid", test_rms_real5_4)&
            ]
    end subroutine collect_rms

    subroutine test_rms_int5_4(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: x(5) = [0, 5, -8,  7, -3]
        call check(error, nint(rms(x)*10.0) / 10.0 == 5.4)
        if(allocated(error)) return
    end subroutine test_rms_int5_4

    subroutine test_rms_real5_4(error)
        type(error_type), allocatable, intent(out) :: error
        real :: x(5) = [0.0, 5.0, -8.0,  7.0, -3.0]
        call check(error, nint(rms(x)*10.0) / 10.0 == 5.4)
        if(allocated(error)) return
    end subroutine test_rms_real5_4

end module test_rms
