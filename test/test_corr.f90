module test_corr
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use mod_data_io, only : read_pearson
    use mod_stat, only : corr, mean, std_dev, std_unit
    implicit none
    private
    public :: collect_corr

contains

    subroutine collect_corr(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("valid", test_corr_int),&
            new_unittest("valid", test_corr_real) &
            ]
    end subroutine collect_corr

    subroutine test_corr_int(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: x(5) = [1, 3, 4, 5, 7]
        integer :: y(5) = [5, 9, 7, 1, 13]
        call check(error, corr(x,y) == .40)
        if(allocated(error)) deallocate(error)
    end subroutine test_corr_int

    subroutine test_corr_real(error)
        type(error_type), allocatable, intent(out) :: error
        real :: x(5) = [1.0, 3.0, 4.0, 5.0, 7.0]
        real :: y(5) = [5.0, 9.0, 7.0, 1.0, 13.0]
        call check(error, corr(x,y) == .40)
        if(allocated(error)) deallocate(error)
    end subroutine test_corr_real
end module test_corr
