module test_std_unit
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use mod_data_io, only : read_women18ht, read_pearson
    use mod_stat, only : std_unit, mean, std_dev
    implicit none
    private
    public :: collect_std_unit

contains

    subroutine collect_std_unit(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("valid", test_std_unit1),&
            new_unittest("valid", test_std_unit2) &
            ]
    end subroutine collect_std_unit

    subroutine test_std_unit1(error)
        type(error_type), allocatable, intent(out) :: error
        real, allocatable :: data(:)
        call read_women18ht(data)
        call check(error, nint(std_unit(data, mean(data) + std_dev(data))*10.0) / 10.0 == 1.0)
        call check(error, nint(std_unit(data, mean(data) + 2*std_dev(data))*10.0) / 10.0 == 2.0)

        if(allocated(data)) deallocate(data)
    end subroutine test_std_unit1

    subroutine test_std_unit2(error)
        type(error_type), allocatable, intent(out) :: error
        real, allocatable :: data(:,:)
        call read_pearson(data)
        call check(error, std_unit(data(:,1), mean(data(:,1)) + std_dev(data(:,1))) == 1.0)
        if(allocated(data)) deallocate(data)
    end subroutine test_std_unit2

end module test_std_unit

