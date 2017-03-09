program read_next_test 

    use gmxfort_tests

    call trj%open(xtcfile, ndxfile)

    ! TEST 1
    a = trj%read_next(100)
    x = trj%x(100, 100)
    ans = [1.455, 0.374, 0.358]
    call check(x, ans, passed, total)

    ! TEST 2
    ans_val = 100
    call check(a, ans_val, passed, total) 

    ! TEST 3
    a = trj%read_next(200)
    ans_val = 1
    call check(a, ans_val, passed, total) 

    ! TEST 4
    a = trj%read_next()
    ans_val = 0
    call check(a, ans_val, passed, total) 
    call trj%close()

    call finished_tests(passed, total)

end program read_next_test
