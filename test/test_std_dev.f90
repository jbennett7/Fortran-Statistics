module test_std_dev
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use mod_stat, only : std_dev
    implicit none
    private
    public :: collect_std_dev

contains

    subroutine collect_std_dev(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("valid", test_std_dev1),&
            new_unittest("valid", test_std_dev2)&
            ]
    end subroutine collect_std_dev

    subroutine test_std_dev1(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: x(7) = [0, 20, 40, 50, 60, 80, 100]
        call check(error, nint(std_dev(x)*100.0) / 100.0 == 31.62)
        if(allocated(error)) return
    end subroutine test_std_dev1

    subroutine test_std_dev2(error)
        type(error_type), allocatable, intent(out) :: error
        real :: x(7) = [0.0, 20.0, 40.0, 50.0, 60.0, 80.0, 100.0]
        call check(error, nint(std_dev(x)*100.0) / 100.0 == 31.62)
        if(allocated(error)) return
    end subroutine test_std_dev2

end module test_std_dev
