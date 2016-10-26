# libgmxfort

This is a Fortran library for reading in an analyzing GROMACS compressed
trajectory files (.xtc) and index files (.ndx). 

## Requirements

[xdrfile](https://github.com/wesbarnett/libxdrfile) is required.

## Installation

    $ make
    # make install

## Usage

Compile your program with `-lgmxfort`. You may also need to use `-I` to point to
where the modules files are (by default at `/usr/include`).

To use the library always put `use gmxfort_trajectory` for using the
`Trajectory` class and `use gmxfort_utils` for using any of the other utilities.
There is an example in the `example` folder on how to do this.

Typically you will open a trajectory file (and optionally a corresponding index
file). Then you will read in the entire trajectory file at once, or you can read
it in in chunks. Then you should close the trajectory file when done.

The simplest way to use this library is to construct a `Trajectory` object and
then use the `read()` method:

```fortran
    use gmxfort_trajectory

    implicit none

    type(Trajectory) :: trj

    call trj%read("traj.xtc")
```

The `read()` method opens the xtc file, reads in all information, and then
closes it. The `trj` object in this example now stores all of the coordinates and
information from the .xtc file.

If you have a corresponding index file you would add a second argument to
`open`:

```fortran
    call trj%read("traj.xtc", "index.ndx")
```

If you want to read in the trajectory file in frame-by-frame use `read_next()`
instead of `read()`. To use this, you must additionally open and close the xtc
file on your own. By default it reads in one frame:

```fortran
    integer :: n
    call trj%open("traj.xtc", "index.ndx")
    n = trj%read_next()
    call trj%close()
```

To read in more than one, specify an argument. The following reads in 10 frames:

```fortran
    n = trj%read_next(10)
```

`read_next()` returns the number of frames actually read in. It is a function,
and not a subroutine. This is useful for using it with a `do while` loop. For
example:

```fortran
    use gmxfort_trajectory

    implicit none

    type(Trajectory) :: trj
    integer :: i, n

    call trj%open("traj.xtc", "index.ndx")

    n = trj%read_next(10)
    do while (n > 0)
        do i = 1, n
            ! do some things with the frames read in
        end do
        n = trj%read_next(10)
    end do

    call trj%close()
```

After calling `read()` or `read_next()` every atom's coordinates are accessible
via the `x()` method. For example, to get the coordinates of the first atom in
the first frame you would do the following. The frame is the first argument and
the atom number is the second argument. 

```fortran
    real :: myatom(3)
    ! ...
    myatom = trj%x(1, 1)
```

**NOTE**: Fortran uses one-based indexing, and that convention is retained here.

If you read in an index file, you can get atom coordinates in relationship to
that. The following gets the fifth atom in index group `C` in the `10`th frame:

```fortran
    myatom = trj%x(10, 5, "C")
```

**Note**: If you have more than one group in your index file with the same name,
this will simply use the first group with that name. It's best not to repeat
group names in your index file.

Note that when you use `x()` you will still have to give it the frame number as
the first argument even if you only read in one frame with `read_next()`.  You
can always get the number of frames in a trajectory file object with the
`nframes` member:

```fortran
    integer :: n
    ! ...
    n = trj%nframes
```

You can also get the number of atoms with the `natoms()` method:

```fortran
    integer :: n
    ! ...
    n = trj%natoms()
```

If you want to know how many atoms are in an index group include the group name
as an argument. In this example the group name is "C":

```fortran
    n = trj%natoms("C")
```

To get the box coordinates, use `box`. The following gets the box of the `2`nd
frame:

```fortran
    real :: mybox(3,3)
    ! ...
    mybox = trj%box(2)
```

You can also get the simulation time and step corresponding with a frame you
read in, using `time` and `step`, respectively. The following get the time associated
with the first frame read in:

```fortran
    real :: mytime
    ! ...
    mytime = trj%time(1)
```

And now the step for the same:

```fortran
    integer :: mystep
    ! ...
    mystep = trj%step(1)
```

There are several functions and subroutines in the `gmxfort_utils` module,
including periodic boundary and distance calculations. Check out the source file
for what is available.

## License

libgmxfort

Copyright (C) 2016 James W. Barnett

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301 USA.
