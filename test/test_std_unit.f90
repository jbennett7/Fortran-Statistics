module test_std_unit
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use mod_data_io, only : read_women18ht, read_pearson
    use mod_stat, only : std_unit
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
        print *, std_unit(data, 50.00)

        if(allocated(data)) deallocate(data)
    end subroutine test_std_unit1

    subroutine test_std_unit2(error)
        type(error_type), allocatable, intent(out) :: error
        real, allocatable :: data(:,:)
        call read_pearson(data)
        print *, std_unit(data(:,1), 63.0)
        print *, std_unit(data(:,2), 63.0)
        if(allocated(data)) deallocate(data)
    end subroutine test_std_unit2

end module test_std_unit

