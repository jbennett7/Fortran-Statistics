module test_bin
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use mod_data_io, only : read_women18ht
    use mod_stat, only : bin
    implicit none
    private
    public :: collect_bin

contains

    subroutine collect_bin(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("valid", test_bin_int),&
            new_unittest("valid", test_bin_real) &
            ]
    end subroutine collect_bin

    subroutine test_bin_int(error)
        type(error_type), allocatable, intent(out) :: error
        real, allocatable :: data(:), bin1(:)
        call read_women18ht(data)
        call bin(data, 60.0, 61.0, bin1)
        call check(error, all(bin1 >= 60.0 .and. bin1 < 61.0))
        if(allocated(error)) deallocate(error)
        if(allocated(data)) deallocate(data)
        if(allocated(bin1)) deallocate(bin1)
    end subroutine test_bin_int

    subroutine test_bin_real(error)
        type(error_type), allocatable, intent(out) :: error
        integer, allocatable :: bin1(:)
        integer :: data(20) = [1, 2, 4, 5, 6, 2, 1, 4, 5, 9,&
                               9, 8, 9, 2, 3, 4, 5, 3, 7, 2]
        call bin(data, 4, 7, bin1)
        call check(error, all(bin1 >= 4 .and. bin1 < 7))
        if(allocated(error)) deallocate(error)
        if(allocated(bin1)) deallocate(bin1)
    end subroutine test_bin_real

end module test_bin
