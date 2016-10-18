# libgmxfort

This is a Fortran library for reading in an analyzing GROMACS compressed
trajectory files (.xtc) and index files (.ndx). 

## Requirements

xdrfile is required.

## Installation

    $ make
    # make install

## Usage

Compile your program with `-lgmxfort`. You may also need to use `-I` to point to
where the modules files are (by default at `/usr/include`.

To use the library always put `use gmxfort_trajectory` for using the
`Trajectory` class and `use gmxfort_utils` for using any of the other utilities.
There is an example in the `example` folder on how to do this.

Typically you will open a trajectory file (and optionally a corresponding index
file). Then you will read in the entire trajectory file at once, or you can read
it in in chunks. Then you should close the trajectory file when done.

First, always construct a `Trajectory` object:

    type(Trajectory) :: trj

Then open the file:

    call trj%open("traj.xtc")

If you have a corresponding index file you would do this instead:

    call trj%open("traj.xtc", "index.ndx")

After opening you can now read the xtc file in. Here's how to read it in all at
once:

    call trj%read()

Now every atom's coordinates are accessible via `x`. For exmaple, to get the
coordinates of the first atom in the first frame you would do the following. The
frame is the first argument and the atom number is the second argument. 

    real :: myatom(3)
    myatom = trj%x(1, 1)

If you read in an index file, you can get atom coordinates in relationship to
that. The following gets the fifth atom in index group `C` in the `10`th frame:

    myatom = trj%x(10, 5, "C")

If you want to read in the trajectory file in chunks use `read_next`. By default
it reads in one frame:

    trj%read_next()

To read in more than one, specify an argument. The following reads in 10 frames:

    trj%read_next(10)

Note that when you access `x` you will still have to give it the frame number as
the first argument, even if you only read in one frame. `read_next` actually
returns the number of frames read in. This is useful to near the end of the
file. Additionally you can always get the number of frames stored in a
`Trajectory` object with the `nframes` member:

    trj%nframes

You can also get the number of atoms with the `natoms` method:

    trj%natoms

If you want to know how many atoms are in an index group include the group name
as an argument. In this example the group name is "C":

    trj%natoms("C")

To get the box coordinates, use `box`. The following gets the box of the `2`nd
frame:

    real :: mybox(3,3)
    mybox = trj%box(2)

You can also get the simulation time and step corresponding with a frame you
read in, using `time` and `step`, respectively. The following get the time associated
with the first frame read in:

    real :: mytime
    mytime = trj%time(1)

And now the step for the same:

    integer :: mystep
    mystep = trj%step(1)

Finally, when done with the `Trajectory` object, you can close the file:

    call trj%close()

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
