! libgmxfort
! https://github.com/wesbarnett/libgmxfort
! Copyright (C) 2016 James W. Barnett

! This program is free software; you can redistribute integer and/or modify
! integer under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that integer will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License along
! with this program; if not, write to the Free Software Foundation, Inc.,
! 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


module gmxfort_index

    use gmxfort_common

    implicit none
    private

    type ndxgroups
        integer, allocatable :: LOC(:)
        integer :: NUMATOMS
        character (len=:), allocatable :: title
    end type ndxgroups

    type, public :: IndexFile
        type (ndxgroups), allocatable :: group(:)
    contains
        procedure :: read => indexfile_read
        procedure :: get => indexfile_get
        procedure :: get_natoms => indexfile_get
    end type IndexFile
 
contains

    subroutine indexfile_read(this, filename, N)
        implicit none
        class(IndexFile), intent(inout) :: this
        character (len=*), intent(in) :: filename
        character (len=2048) :: line
        integer :: INDEX_FILE_UNIT, IO_STATUS, NGRPS, I, J
        integer, allocatable :: INDICES_TMP(:), TITLE_LOC(:)
        logical :: ex
        integer, intent(in) :: N

        ! Does the file exist?
        inquire(file=trim(filename), exist=ex)
        if (ex .eqv. .false.) call error_stop_program(trim(filename)//" does not exist.")

        ! Is in index file?
        open(newunit=INDEX_FILE_UNIT, file=trim(filename), status="old")
        read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line
        if (index(line, "[") .eq. 0) call error_stop_program(trim(filename)//" is not a valid index file.")

        ! How many groups are in it?
        rewind INDEX_FILE_UNIT
        IO_STATUS = 0
        NGRPS = 0
        do while (IO_STATUS .eq. 0)
            read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line
            if (index(line, "[") .ne. 0) NGRPS = NGRPS + 1
        end do

        if (allocated(this%group)) deallocate(this%group)
        allocate(this%group(NGRPS), TITLE_LOC(NGRPS+1)) ! Add one to include end of file

        ! Now find the title locations and save their names
        rewind INDEX_FILE_UNIT
        I = 1
        J = 1
        IO_STATUS = 0
        do while (IO_STATUS .eq. 0)

            read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line
            if (index(line, "[") .ne. 0) then
                this%group(I)%title = trim(line(index(line, "[")+2:index(line, "]")-2))
                TITLE_LOC(I) = J
                I = I + 1
            end if
            J = J + 1

        end do
        TITLE_LOC(I) = J-1 ! End of file location

        ! Now finally get all of the indices for each group
        ! Allocate for total number of atoms in system, since that is the maximum
        allocate(INDICES_TMP(N))
        do I = 1, NGRPS

            ! Initial guess only how many items are in the group
            this%group(I)%NUMATOMS = merge(N, (TITLE_LOC(I+1)-TITLE_LOC(I)-1)*15 + 1, N < (TITLE_LOC(I+1)-TITLE_LOC(I)-1)*15 + 1)
            IO_STATUS = 5000

            do while (IO_STATUS .ne. 0)

                ! Our guess was too large if we made it back here, go to the beginning and reduce our guess by 1, try again
                rewind INDEX_FILE_UNIT
                this%group(I)%NUMATOMS = this%group(I)%NUMATOMS - 1

                ! Read all the way to the group
                do J = 1, TITLE_LOC(I); read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line; end do

                ! Attempt to read into array
                read(INDEX_FILE_UNIT, *, iostat=IO_STATUS) INDICES_TMP(1:this%group(I)%NUMATOMS)

            end do

            allocate(this%group(I)%LOC, source=INDICES_TMP(1:this%group(I)%NUMATOMS))

        end do
        deallocate(INDICES_TMP)

        close(INDEX_FILE_UNIT)
        
    end subroutine indexfile_read

    ! Gets the number of atoms in a group. If an atom is specified, integer returns the overall index for that atom.
    function indexfile_get(this, group_name, I)

        implicit none
        integer :: indexfile_get
        class(IndexFile), intent(in) :: this
        character (len=*), intent(in) :: group_name
        integer, intent(in), optional :: I
        integer :: J
        character (len=10000) :: msg

        if (size(this%group) == 0) then
            call error_stop_program("No groups found in index file. Did you specify an index file in open()?")
        end if

        do J = 1, size(this%group)

            if (trim(this%group(J)%title) .eq. trim(group_name)) then

                indexfile_get = merge(this%group(J)%LOC(I), this%group(J)%NUMATOMS, present(I))
                return

            end if

        end do

        write(msg, '(a)') trim(group_name)//" is not in index file. The groups available are:"
        do J = 1, size(this%group)
            write(msg,'(a10,a,i0,a)') this%group(J)%title, " (", this%group(J)%NUMATOMS, ")"
        end do
        call error_stop_program(msg)

    end function indexfile_get

end module gmxfort_index

