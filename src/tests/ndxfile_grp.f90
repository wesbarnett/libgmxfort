program ndxfile_grp_test 

    use gmxfort_trajectory
    use gmxfort_utils
    use gmxfort_tests

    ! TEST 1
    call trj%read(xtcfile, ndxfile, "OW")
    x = trj%x(100, 100)
    ans = [0.115, 1.048, 3.222]
    call check(x, ans, passed, total)

    ! TEST 2
    a = trj%natoms()
    ans_val = 4125
    call check(a, ans_val, passed, total) 

    write(output_unit,*)
    write(output_unit,'(a,i0,a,i0,a)') "Passed ", passed, " out of ", total, " tests"
    write(output_unit,*)

    if (passed .ne. total) then
        write(output_unit, '(a)') "WARNING: Some tests failed!"
        call abort()
    end if

end program ndxfile_grp_test
