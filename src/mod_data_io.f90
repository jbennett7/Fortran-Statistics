module mod_data_io
    implicit none

    private
    public :: read_women18ht, read_pearson

contains

    integer function num_records(filename, headers)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: headers
        integer :: u
        open(newunit=u, file=filename)
        if(headers) read(u, fmt=*)
        num_records = 0
        do
            read(u, fmt=*, end=1)
            num_records = num_records + 1
        end do
        1 continue
        close(u)
    end function num_records

    subroutine read_women18ht(data)
        real, allocatable, intent(out) :: data(:)
        integer :: u, i, n
        character(len=*), parameter :: filename = "data/women18ht.csv"
        n = num_records(filename, .true.)
        open(newunit=u, file=filename)
        read(u, fmt=*)
        allocate(data(n))
        do i = 1, n
            read(u, fmt='(f5.2)') data(i)
        end do
        close(u)
    end subroutine read_women18ht

    subroutine read_pearson(data)
        real, allocatable, intent(out) :: data(:,:)
        integer :: u, i, n
        character(len=*), parameter :: filename = "data/pearson.csv"
        n = num_records(filename, .true.)
        open(newunit=u, file=filename)
        read(u, fmt=*)
        allocate(data(n,2))
        do i = 1, n
            read(u, fmt=*) data(i,1), data(i,2)
        end do
        close(u)
    end subroutine read_pearson

end module mod_data_io
