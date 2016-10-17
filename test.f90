program test

    use gmxfort_trajectory
    use gmxfort_index

    type(Trajectory) :: trj

    call trj%open("traj.xtc", "index.ndx")
    call trj%read()
    call trj%close()

    do I=1, trj%NFRAMES
        print *, trj%x(I,1,"HW2")
    end do

end program test
