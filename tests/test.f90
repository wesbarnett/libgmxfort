module subs

contains

    subroutine check_int(x, y, passed, total)

        implicit none

        integer, intent(inout) :: total, passed
        integer, intent(in) :: x, y
        write(0, '(a,i0)') "TEST ", total+1

        if (x .eq. y) then
            write(0, '(a)') "  PASSED"
            passed = passed + 1
        else
            write(0, '(a)') "  FAILED"
        end if      

        total = total + 1

    end subroutine check_int

    subroutine check_array(x, y, passed, total)

        implicit none

        integer, intent(inout) :: total, passed
        real, intent(in) :: x(:), y(:)
        real :: tol = 1e-4

        write(0, '(a,i0)') "TEST ", total+1

        if (all(abs(x - y) .le. tol)) then
            write(0, '(a)') "  PASSED"
            passed = passed + 1
        else
            write(0, '(a)') "  FAILED"
        end if      

        total = total + 1

    end subroutine check_array

    subroutine check_array_2d(x, y, passed, total)

        implicit none

        integer, intent(inout) :: total, passed
        real, intent(in) :: x(:,:), y(:,:)
        real :: tol = 1e-6

        write(0, '(a,i0)') "TEST ", total+1

        if (all(abs(x - y) .le. tol)) then
            write(0, '(a)') "  PASSED"
            passed = passed + 1
        else
            write(0, '(a)') "  FAILED"
        end if      

        total = total + 1

    end subroutine check_array_2d

end module subs

program test

    use gmxfort_trajectory
    use gmxfort_utils
    use subs

    implicit none

    type(Trajectory) :: trj
    character (len=:), parameter :: xtcfile = "tests/test.xtc"
    character (len=:), parameter :: ndxfile = "tests/test.ndx"
    real :: x(3), ans(3), box(3,3), ans_box(3,3)
    integer :: passed = 0, total = 0, a, ans_val

    call trj%open(xtcfile, ndxfile)
    call trj%read()
    call trj%close()

    x = trj%x(1, 1)
    ans = [1.749, 0.752, 0.220]
    call check_array(x, ans, passed, total)

    x = trj%x(50, 50)
    ans = [0.359, 1.999, 1.816]
    call check_array(x, ans, passed, total)

    x = trj%x(trj%NFRAMES, trj%natoms())
    ans = [4.060, 0.155, 0.262]
    call check_array(x, ans, passed, total)

    x = trj%x(100, 100, "OW")
    ans = [0.115, 1.048, 3.222]
    call check_array(x, ans, passed, total)

    box = trj%box(10)
    ans_box = reshape((/4.96972179, 0.00000000, 0.00000000, 0.00000000, 4.96972179, 0.00000000, 0.00000000, 0.00000000, &
        4.96972179/), shape(ans_box))
    call check_array_2d(box, ans_box, passed, total)

    a = trj%natoms()
    ans_val = 16500
    call check_int(a, ans_val, passed, total) 

    a = trj%natoms("OW")
    ans_val = 4125
    call check_int(a, ans_val, passed, total) 

    a = trj%nframes
    ans_val = 101
    call check_int(a, ans_val, passed, total) 

    call trj%open(xtcfile, ndxfile)
    a = trj%read_next(100)

    x = trj%x(100, 100)
    ans = [1.455, 0.374, 0.358]
    call check_array(x, ans, passed, total)

    ans_val = 100
    call check_int(a, ans_val, passed, total) 

    call trj%close()

    x = [5.5, 5.5, 3.5]
    box = reshape((/5.0, 0.0, 0.0, &
                    0.0, 5.0, 0.0, &
                    2.5, 2.5, 3.5/), shape(box))
    x = pbc(dble(x), dble(box))
    ans = [-2.000, -2.000, 0.000]
    call check_array(x, ans, passed, total) 

    write(0,'(a,i0,a,i0,a)') "Passed ", passed, " out of ", total, " tests"

    if (passed .ne. total) then
        write(0, '(a)') "WARNING: Some tests failed!"
        stop 1
    end if

end program test
