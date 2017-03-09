program ndxfile_grp_test 

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

    call finished_tests(passed, total)

end program ndxfile_grp_test
