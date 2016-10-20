! Example program for use with libgmxfort
! Calculates the dihedral angles of some chain (going down the chain)

program angles

    use gmxfort_trajectory
    use gmxfort_utils

    implicit none

    character (len=*), parameter :: index_grp = "Site"

    type(Trajectory) :: trj
    integer :: I, J, U
    integer :: NSITES
    integer :: NANGLES
    character (len=10) :: nchar
    real(8), allocatable :: ang(:)
    real(8), dimension(3) :: a, b, c, d
    real(8) :: box(3,3)

    call trj%read("traj.xtc", "index.ndx")

    NSITES = trj%natoms(index_grp)
    NANGLES = NSITES - 3

    write(nchar,"(i0)") NANGLES

    allocate(ang(NANGLES))

    open(newunit=U, file="angles.dat")

    do I = 1, trj%NFRAMES

        do J = 1, NANGLES
            
            a = trj%x(I, J, index_grp)
            b = trj%x(I, J+1, index_grp)
            c = trj%x(I, J+2, index_grp)
            d = trj%x(I, J+3, index_grp)
            box = trj%box(I)
            ang(J) = dihedral_angle(a, b, c, d, box)

        end do

        write(U, "("//trim(nchar)//"f12.6)") ang

    end do

    close(U)

end program angles
