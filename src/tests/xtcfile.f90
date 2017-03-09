program xtcfile_test

    use gmxfort_trajectory
    use gmxfort_utils
    use gmxfort_tests

    call trj%read(xtcfile)

    ! TEST 1
    x = trj%x(1, 1)
    ans = [1.749, 0.752, 0.220]
    call check(x, ans, passed, total)

    ! TEST 2
    x = trj%x(50, 50)
    ans = [0.359, 1.999, 1.816]
    call check(x, ans, passed, total)

    ! TEST 3
    x = trj%x(trj%NFRAMES, trj%natoms())
    ans = [4.060, 0.155, 0.262]
    call check(x, ans, passed, total)

    write(output_unit,*)
    write(output_unit,'(a,i0,a,i0,a)') "Passed ", passed, " out of ", total, " tests"
    write(output_unit,*)

    if (passed .ne. total) then
        write(output_unit, '(a)') "WARNING: Some tests failed!"
        call abort()
    end if

end program xtcfile_test
