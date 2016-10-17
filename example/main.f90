! Example program for use with libgmxfort
! Calculates the dihedral angles of some chain (going down the chain)

program angles

    use gmxfort_trajectory
    use gmxfort_utils

    implicit none

    character (len=*), parameter :: index_grp = "Site"

    type(Trajectory) :: trj
    integer :: I, J, K, U
    integer :: NSITES
    integer :: NANGLES
    integer :: NCOMP
    real(8), allocatable :: ang(:)
    real(8), allocatable :: indata(:)
    real(8), dimension(3) :: a, b, c, d
    real(8) :: box(3,3)

    call trj%open("traj.xtc", "index.ndx")
    call trj%read()
    call trj%close()

    NSITES = trj%n(index_grp)
    NANGLES = NSITES - 3

    allocate(ang(NANGLES))
    allocate(indata(NCOMP))

    open(newunit=U, file="angles.dat")

    do I = 1, trj%NFRAMES

        do J = 1, NANGLES
            
            a = trj%x(I, J, index_grp)
            b = trj%x(I, J+1, index_grp)
            c = trj%x(I, J+2, index_grp)
            d = trj%x(I, J+3, index_grp)
            ang(J) = dihedral_angle(a, b, c, d, trj%b(I))

        end do

        write(U, *) ang

    end do

    close(U)

end program angles
