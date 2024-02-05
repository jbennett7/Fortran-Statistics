module mod_stat
    implicit none

    private
    public ::     &
        mean,     &
        rms,      &
        std_dev,  &
        std_unit, &
        corr

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
        !module procedure :: corr_real
    end interface corr

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
        res = abs(p - mean(x)) / std_dev(x)
    end function std_unit_int

    pure real function std_unit_real(x, p) result(res)
        real, intent(in) :: x(:)
        real, intent(in) :: p
        res = abs(p - mean(x)) / std_dev(x)
    end function std_unit_real

   pure real function corr_int(x, y) result(res)
       integer, intent(in) :: x(:), y(:)
       res = mean((abs(x - mean(x)) / std_dev(x))*&
                  (abs(y - mean(y) / std_dev(y))))
   end function corr_int

   pure real function corr_real(x, y) result(res)
       real, intent(in) :: x(:), y(:)
       res = mean((abs(x - mean(x)) / std_dev(x))*&
                  (abs(y - mean(y)) / std_dev(y)))
   end function corr_real

end module mod_stat
