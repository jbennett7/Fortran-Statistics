module test_mean
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use mod_stat, only : mean
    implicit none
    private
    public :: collect_mean

contains

    subroutine collect_mean(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("valid", test_mean_int5),&
            new_unittest("valid", test_int2_5),&
            new_unittest("valid", test_int_neg_5),&
            new_unittest("valid", test_real5),&
            new_unittest("valid", test_real2_5),&
            new_unittest("valid", test_real_neg_5),&
            new_unittest("valid", test_logical5),&
            new_unittest("valid", test_logical6),&
            new_unittest("valid", test_logical25)&
            !new_unittest("invalid", test_invalid, should_fail=.true.) &
            ]
    end subroutine collect_mean

    subroutine test_mean_int5(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: x(5) = [3, 8, 7, 2, 5]
        call check(error, mean(x) == 5.0)
        if(allocated(error)) return
    end subroutine test_mean_int5

    subroutine test_int2_5(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: x(6) = [1, 1, 3, 2, 5, 3]
        call check(error, mean(x) == 2.5)
    end subroutine test_int2_5
        
    subroutine test_int_neg_5(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: x(5) = [-1, -2, 6, 12, 10]
        call check(error, mean(x) == 5.0)
    end subroutine test_int_neg_5

    subroutine test_real5(error)
        type(error_type), allocatable, intent(out) :: error
        real :: x(5) = [3.0, 8.0, 7.0, 2.0, 5.0]
        call check(error, mean(x) == 5.0)
    end subroutine test_real5

    subroutine test_real2_5(error)
        type(error_type), allocatable, intent(out) :: error
        real :: x(6) = [1.0, 1.0, 3.0, 2.0, 5.0, 3.0]
        call check(error, mean(x) == 2.5)
    end subroutine test_real2_5

    subroutine test_real_neg_5(error)
        type(error_type), allocatable, intent(out) :: error
        real :: x(5) = [-1.0, -2.0, 6.0, 12.0, 10.0]
        call check(error, mean(x) == 5.0)
    end subroutine test_real_neg_5

    subroutine test_logical5(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: x(6) = [.true., .true., .false., .true., .false., .false.]
        call check(error, mean(x) == .5)
    end subroutine test_logical5

    subroutine test_logical6(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: x(5) = [.true., .true., .false., .true., .false.]
        call check(error, mean(x) == .6)
    end subroutine test_logical6

    subroutine test_logical25(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: x(4) = [.true., .false., .false., .false.]
        call check(error, mean(x) == .25)
    end subroutine test_logical25
end module test_mean
