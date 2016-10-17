program test

    use gmxfort_trajectory
    use gmxfort_index

    type(Trajectory) :: trj
    type(IndexFile) :: ndx

    !call trj%open("traj.xtc")
    !call trj%read()

    call ndx%read("index.ndx")

    !do I=1, trj%NFRAMES
    !    print *, trj%frameArray(I)%box(:,:)
    !end do

end program test
