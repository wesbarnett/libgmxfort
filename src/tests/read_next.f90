program read_next_test 

    use gmxfort_trajectory
    use gmxfort_utils
    use gmxfort_tests

    ! TEST 1
    x = [5.5, 5.5, 3.5]
    box = reshape((/5.0, 0.0, 0.0, &
                    0.0, 5.0, 0.0, &
                    2.5, 2.5, 3.5/), shape(box))
    x = pbc(dble(x), dble(box))
    ans = [-2.000, -2.000, 0.000]
    call check(x, ans, passed, total) 

    ! TEST 2
    x = [5.5, 5.5, 3.5]
    b = distance(dble(x), dble(ans), dble(box))
    call check(b, 0.0, passed, total)

    ! TEST 3
    x = [5.5, 5.5, 3.5]
    y = [3.6, 4.7, 5.0]
    box = reshape((/3.5, 0.0, 0.0, &
                    0.0, 4.5, 0.0, &
                    0.0, 0.0, 4.0/), shape(box))
    b = distance(dble(x), dble(y), dble(box))
    call check(b, 2.33452, passed, total)

    ! TEST 4
    b = magnitude(dble(x))
    call check(b, 8.52936, passed, total)

    ! TEST 5
    x = [0.0, 0.0, 0.0]
    y = [0.0, 1.0, 0.0]
    z = [1.0, 1.0, 0.0]
    b = bond_angle(dble(x), dble(y), dble(z), dble(box))
    call check(b, real(PI/2.0d0), passed, total)

    ! TEST 6
    w = [1.0, 1.0, 1.0]
    b = dihedral_angle(dble(x), dble(y), dble(z), dble(w), dble(box))
    call check(b, real(PI/2.0d0), passed, total)

    ! TEST 7
    w = [1.0, 1.0, -1.0]
    b = dihedral_angle(dble(x), dble(y), dble(z), dble(w), dble(box))
    call check(b, real(-PI/2.0d0), passed, total)

    ! TEST 8
    call trj%open(xtcfile, ndxfile)
    a = trj%read_next(50, "OW")
    a = trj%read_next(200, "OW")
    x = trj%x(50, 100)
    ans = [0.115, 1.048, 3.222]
    call check(x, ans, passed, total)
    call trj%close()

    write(output_unit,*)
    write(output_unit,'(a,i0,a,i0,a)') "Passed ", passed, " out of ", total, " tests"
    write(output_unit,*)

    if (passed .ne. total) then
        write(output_unit, '(a)') "WARNING: Some tests failed!"
        call abort()
    end if

end program read_next_test
