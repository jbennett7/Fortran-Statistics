program main
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_mean, only : collect_mean
    use test_rms, only : collect_rms
    use test_std_dev, only : collect_std_dev
    use test_std_unit, only : collect_std_unit
    use test_corr, only : collect_corr
    use test_bin, only : collect_bin
    use test_denan, only : collect_denan
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("mean", collect_mean),&
        new_testsuite("rms", collect_rms),&
        new_testsuite("std_dev", collect_std_dev),&
        new_testsuite("std_unit", collect_std_unit),&
        new_testsuite("corr", collect_corr),&
        new_testsuite("bin", collect_bin),&
        new_testsuite("denan", collect_denan) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program main
