program test

    use gmxfort_trajectory
    use gmxfort_index

    type(Trajectory) :: trj

    call trj%open("traj.xtc", "index.ndx")

    do while (trj%read_next() .ne. 0)
        print *, trj%x(1,1,"HW2")
    end do

    call trj%close()

end program test
