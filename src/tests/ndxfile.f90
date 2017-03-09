program ndxfile_test 

    use gmxfort_trajectory
    use gmxfort_utils
    use gmxfort_tests

    call trj%read(xtcfile, ndxfile)
    ! TEST 1
    x = trj%x(100, 100, "OW")
    ans = [0.115, 1.048, 3.222]
    call check(x, ans, passed, total)

    ! TEST 2
    box = trj%box(10)
    ans_box = reshape((/4.96972179, 0.00000000, 0.00000000, 0.00000000, 4.96972179, 0.00000000, 0.00000000, 0.00000000, &
        4.96972179/), shape(ans_box))
    call check(box, ans_box, passed, total)

    ! TEST 3
    a = trj%natoms()
    ans_val = 16500
    call check(a, ans_val, passed, total) 

    ! TEST 4
    a = trj%natoms("System")
    ans_val = 16500
    call check(a, ans_val, passed, total) 

    ! TEST 5
    a = trj%natoms("OW")
    ans_val = 4125
    call check(a, ans_val, passed, total) 

    ! TEST 6
    a = trj%natoms("TEST")
    ans_val = 1
    call check(a, ans_val, passed, total) 

    ! TEST 7
    a = trj%nframes
    ans_val = 101
    call check(a, ans_val, passed, total) 

    write(output_unit,*)
    write(output_unit,'(a,i0,a,i0,a)') "Passed ", passed, " out of ", total, " tests"
    write(output_unit,*)

    if (passed .ne. total) then
        write(output_unit, '(a)') "WARNING: Some tests failed!"
        call abort()
    end if

end program ndxfile_test
