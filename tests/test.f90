module subs


    interface check
        module procedure check_int, check_real, check_array, check_array_2d
    end interface check

contains

    subroutine do_output(total, passed, test_result)

        implicit none
        integer, intent(inout) :: total, passed
        logical, intent(in) :: test_result
        integer :: I
        character (len=6) :: passfail(0:1) = ["FAILED", "PASSED"]
        
        I = merge(1, 0, test_result)
        total = total + 1
        passed = passed + I

        write(0, '(a,i2,a,a)') "TEST ", total, ": ", passfail(I)

    end subroutine do_output

    subroutine check_int(x, y, passed, total)

        implicit none

        integer, intent(inout) :: total, passed
        integer, intent(in) :: x, y

        call do_output(total, passed, x .eq. y)

    end subroutine check_int

    subroutine check_real(x, y, passed, total)

        implicit none

        integer, intent(inout) :: total, passed
        real, intent(in) :: x, y
        real :: tol = 1e-4

        call do_output(total, passed, abs(x-y) .le. tol)

    end subroutine check_real

    subroutine check_array(x, y, passed, total)

        implicit none

        integer, intent(inout) :: total, passed
        real, intent(in) :: x(:), y(:)
        real :: tol = 1e-4

        call do_output(total, passed, all(abs(x - y) .le. tol))

    end subroutine check_array

    subroutine check_array_2d(x, y, passed, total)

        implicit none

        integer, intent(inout) :: total, passed
        real, intent(in) :: x(:,:), y(:,:)
        real :: tol = 1e-6

        call do_output(total, passed, all(abs(x - y) .le. tol))

    end subroutine check_array_2d

end module subs

program test

    use gmxfort_trajectory
    use gmxfort_utils
    use subs

    implicit none

    type(Trajectory) :: trj
    real(8), parameter :: PI = 2.0d0*acos(0.0d0)
    character (len=:), parameter :: xtcfile = "tests/test.xtc"
    character (len=:), parameter :: ndxfile = "tests/test.ndx"
    real :: x(3), y(3), z(3), w(3), ans(3), box(3,3), ans_box(3,3), b, c
    integer :: passed = 0, total = 0, a, ans_val

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
    a = trj%natoms("OW")
    ans_val = 4125
    call check(a, ans_val, passed, total) 


    a = trj%natoms("TEST")
    ans_val = 1
    call check(a, ans_val, passed, total) 

    ! TEST 8
    a = trj%nframes
    ans_val = 101
    call check(a, ans_val, passed, total) 

    ! TEST 9
    call trj%open(xtcfile, ndxfile)
    a = trj%read_next(100)
    x = trj%x(100, 100)
    ans = [1.455, 0.374, 0.358]
    call check(x, ans, passed, total)

    ! TEST 10
    ans_val = 100
    call check(a, ans_val, passed, total) 

    ! TEST 11
    a = trj%read_next(200)
    ans_val = 1
    call check(a, ans_val, passed, total) 

    ! TEST 12
    a = trj%read_next()
    ans_val = 0
    call check(a, ans_val, passed, total) 
    call trj%close()

    ! TEST 13
    x = [5.5, 5.5, 3.5]
    box = reshape((/5.0, 0.0, 0.0, &
                    0.0, 5.0, 0.0, &
                    2.5, 2.5, 3.5/), shape(box))
    x = pbc(dble(x), dble(box))
    ans = [-2.000, -2.000, 0.000]
    call check(x, ans, passed, total) 

    ! TEST 14
    x = [5.5, 5.5, 3.5]
    b = distance(dble(x), dble(ans), dble(box))
    call check(b, 0.0, passed, total)

    ! TEST 15
    x = [5.5, 5.5, 3.5]
    y = [3.6, 4.7, 5.0]
    box = reshape((/3.5, 0.0, 0.0, &
                    0.0, 4.5, 0.0, &
                    0.0, 0.0, 4.0/), shape(box))
    b = distance(dble(x), dble(y), dble(box))
    call check(b, 2.33452, passed, total)

    ! TEST 16
    b = magnitude(dble(x))
    call check(b, 8.52936, passed, total)

    ! TEST 17
    x = [0.0, 0.0, 0.0]
    y = [0.0, 1.0, 0.0]
    z = [1.0, 1.0, 0.0]
    b = bond_angle(dble(x), dble(y), dble(z), dble(box))
    call check(b, real(PI/2.0d0), passed, total)

    ! TEST 18
    w = [1.0, 1.0, 1.0]
    b = dihedral_angle(dble(x), dble(y), dble(z), dble(w), dble(box))
    call check(b, real(PI/2.0d0), passed, total)

    ! TEST 19
    w = [1.0, 1.0, -1.0]
    b = dihedral_angle(dble(x), dble(y), dble(z), dble(w), dble(box))
    call check(b, real(-PI/2.0d0), passed, total)

    write(0,*)
    write(0,'(a,i0,a,i0,a)') "Passed ", passed, " out of ", total, " tests"
    write(0,*)

    if (passed .ne. total) then
        write(0, '(a)') "WARNING: Some tests failed!"
        call abort()
    end if

end program test
