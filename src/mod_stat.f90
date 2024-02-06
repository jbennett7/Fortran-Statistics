module mod_stat
    implicit none

    private
    public ::     &
        mean,     &
        rms,      &
        std_dev,  &
        std_unit, &
        corr,     &
        bin,      &
        summary

    interface mean
        module procedure :: mean_int
        module procedure :: mean_real
        module procedure :: mean_logical
    end interface mean

    interface rms
        module procedure :: rms_int
        module procedure :: rms_real
    end interface rms

    interface std_dev
        module procedure :: std_dev_int
        module procedure :: std_dev_real
    end interface std_dev

    interface std_unit
        module procedure :: std_unit_int
        module procedure :: std_unit_real
    end interface std_unit

    interface corr
        module procedure :: corr_int
        module procedure :: corr_real
    end interface corr

    interface bin
        module procedure :: bin_int
        module procedure :: bin_real
    end interface bin

contains

    pure real function mean_int(x) result(res)
        integer, intent(in) :: x(:)
        res = real(sum(x), kind=kind(res)) / size(x)
    end function mean_int

    pure real function mean_real(x) result(res)
        real, intent(in) :: x(:)
        res = sum(x) / size(x)
    end function mean_real

    pure real function mean_logical(x) result(res)
        logical, intent(in) :: x(:)
        res = real(count(x), kind=kind(res)) / size(x)
    end function mean_logical

    pure real function rms_int(x) result(res)
        integer, intent(in) :: x(:)
        res = sqrt(sum(real(x, kind=kind(res))**2) / size(x))
    end function rms_int

    pure real function rms_real(x) result(res)
        real, intent(in) :: x(:)
        res = sqrt(sum(x**2) / size(x))
    end function rms_real

    pure real function std_dev_int(x) result(res)
        integer, intent(in) :: x(:)
        res = sqrt(sum((x-mean(x))**2) / size(x))
    end function std_dev_int

    pure real function std_dev_real(x) result(res)
        real, intent(in) :: x(:)
        res = sqrt(sum((x-mean(x))**2) / size(x))
    end function std_dev_real

    pure real function std_unit_int(x, p) result(res)
        integer, intent(in) :: x(:)
        integer, intent(in) :: p
        res = (p - mean(x)) / std_dev(x)
    end function std_unit_int

    pure real function std_unit_real(x, p) result(res)
        real, intent(in) :: x(:)
        real, intent(in) :: p
        res = (p - mean(x)) / std_dev(x)
    end function std_unit_real

    pure real function corr_int(x, y) result(res)
        integer, intent(in) :: x(:), y(:)
        res = mean(((x - mean(x)) / std_dev(x))*&
                   ((y - mean(y)) / std_dev(y)))
    end function corr_int
 
    pure real function corr_real(x, y) result(res)
        real, intent(in) :: x(:), y(:)
        res = mean(((x - mean(x)) / std_dev(x))*&
                   ((y - mean(y)) / std_dev(y)))
    end function corr_real

    subroutine bin_int(x, low, high, output)
        integer, intent(in) :: x(:), low, high
        integer, allocatable, intent(out) :: output(:)
        logical, allocatable :: mask(:)
        allocate(mask(size(x)))
        mask = (x >= low) .and. (x < high)
        output = pack(x, mask)
        deallocate(mask)
    end subroutine bin_int

    subroutine bin_real(x, low, high, output)
        real, intent(in) :: x(:), low, high
        real, allocatable, intent(out) :: output(:)
        logical, allocatable :: mask(:)
        allocate(mask(size(x)))
        mask = (x >= low) .and. (x < high)
        output = pack(x, mask)
        deallocate(mask)
    end subroutine bin_real

end module mod_stat
