program test

    use gmx

    type(Trajectory) :: trj

    call trj%open("traj.xtc")
    call trj%read()

    do I=1, trj%NFRAMES
        print *, trj%frameArray(I)%box(:,:)
    end do

end program test
