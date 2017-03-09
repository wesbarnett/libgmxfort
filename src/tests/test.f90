program test

    use gmxfort_trajectory
    use gmxfort_utils
    use gmxfort_tests

    implicit none

    type(Trajectory) :: trj
    real(8), parameter :: PI = 2.0d0*acos(0.0d0)
    character (len=8), parameter :: xtcfile = "test.xtc"
    character (len=8), parameter :: ndxfile = "test.ndx"
    real :: x(3), y(3), z(3), w(3), ans(3), box(3,3), ans_box(3,3), b, c
    integer :: passed = 0, total = 0, a, ans_val

    ! Reading xtcfile alone
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

    ! Reading xtcfile and ndxfile
    call trj%read(xtcfile, ndxfile)
    ! TEST 4
    x = trj%x(100, 100, "OW")
    ans = [0.115, 1.048, 3.222]
    call check(x, ans, passed, total)

    ! TEST 5
    box = trj%box(10)
    ans_box = reshape((/4.96972179, 0.00000000, 0.00000000, 0.00000000, 4.96972179, 0.00000000, 0.00000000, 0.00000000, &
        4.96972179/), shape(ans_box))
    call check(box, ans_box, passed, total)

    ! TEST 6
    a = trj%natoms()
    ans_val = 16500
    call check(a, ans_val, passed, total) 

    ! TEST 7
    a = trj%natoms("System")
    ans_val = 16500
    call check(a, ans_val, passed, total) 

    ! TEST 8
    a = trj%natoms("OW")
    ans_val = 4125
    call check(a, ans_val, passed, total) 

    ! TEST 9
    a = trj%natoms("TEST")
    ans_val = 1
    call check(a, ans_val, passed, total) 

    ! TEST 10
    a = trj%nframes
    ans_val = 101
    call check(a, ans_val, passed, total) 

    ! Reading just a selected index group
    call trj%read(xtcfile, ndxfile, "OW")
    ! TEST 11
    x = trj%x(100, 100)
    ans = [0.115, 1.048, 3.222]
    call check(x, ans, passed, total)

    ! TEST 12
    a = trj%natoms()
    ans_val = 4125
    call check(a, ans_val, passed, total) 

    ! Using read_next()
    call trj%open(xtcfile, ndxfile)
    ! TEST 13
    a = trj%read_next(100)
    x = trj%x(100, 100)
    ans = [1.455, 0.374, 0.358]
    call check(x, ans, passed, total)

    ! TEST 14
    ans_val = 100
    call check(a, ans_val, passed, total) 

    ! TEST 15
    a = trj%read_next(200)
    ans_val = 1
    call check(a, ans_val, passed, total) 

    ! TEST 16
    a = trj%read_next()
    ans_val = 0
    call check(a, ans_val, passed, total) 
    call trj%close()

    ! Test utilities
    ! TEST 17
    x = [5.5, 5.5, 3.5]
    box = reshape((/5.0, 0.0, 0.0, &
                    0.0, 5.0, 0.0, &
                    2.5, 2.5, 3.5/), shape(box))
    x = pbc(dble(x), dble(box))
    ans = [-2.000, -2.000, 0.000]
    call check(x, ans, passed, total) 

    ! TEST 18
    x = [5.5, 5.5, 3.5]
    b = distance(dble(x), dble(ans), dble(box))
    call check(b, 0.0, passed, total)

    ! TEST 19
    x = [5.5, 5.5, 3.5]
    y = [3.6, 4.7, 5.0]
    box = reshape((/3.5, 0.0, 0.0, &
                    0.0, 4.5, 0.0, &
                    0.0, 0.0, 4.0/), shape(box))
    b = distance(dble(x), dble(y), dble(box))
    call check(b, 2.33452, passed, total)

    ! TEST 20
    b = magnitude(dble(x))
    call check(b, 8.52936, passed, total)

    ! TEST 21
    x = [0.0, 0.0, 0.0]
    y = [0.0, 1.0, 0.0]
    z = [1.0, 1.0, 0.0]
    b = bond_angle(dble(x), dble(y), dble(z), dble(box))
    call check(b, real(PI/2.0d0), passed, total)

    ! TEST 22
    w = [1.0, 1.0, 1.0]
    b = dihedral_angle(dble(x), dble(y), dble(z), dble(w), dble(box))
    call check(b, real(PI/2.0d0), passed, total)

    ! TEST 23
    w = [1.0, 1.0, -1.0]
    b = dihedral_angle(dble(x), dble(y), dble(z), dble(w), dble(box))
    call check(b, real(-PI/2.0d0), passed, total)

    ! TEST 24
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

end program test
