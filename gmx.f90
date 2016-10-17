!  XDR Fortran Interface
!  2013 (c) James W. Barnett <jbarnet4@tulane.edu>
!  https://github.com/wesbarnett/

module gmx

    use, intrinsic :: iso_c_binding, only: C_PTR, C_CHAR, C_FLOAT, C_INT

    implicit none
    public


! TODO: fix documentation
!   box     - triclinic pbc box of the configuration
!   NATOMS  - number of atoms in the configuration.
!   xyz     - positions read in (3,NATOMS)
!   prec    - precision of the coordinates read in
!   STEP    - step number of configuration.
!   STAT    - status of operation. 0 = good
!   time    - time of the configuration
!   xd      - pointer from libxdrfile.


    type, public :: Frame
        real(C_FLOAT), allocatable :: xyz(:,:)
        integer(C_INT) :: NATOMS, STEP, STAT
        real(C_FLOAT) :: box(3,3), prec, time
    end type

    type, public :: Trajectory
        type(xdrfile), pointer :: xd
        type(Frame), allocatable :: frameArray(:)
        integer :: NFRAMES
        integer :: NATOMS
    contains
        procedure :: open => trajectory_constructor !TODO, make a true constructor
        procedure :: read => trajectory_read
        procedure :: getXYZ => trajectory_get_xyz
    end type

    ! the data type located in libxdrfile
    type, bind(C) :: xdrfile
      type(C_PTR) :: fp, xdr
      character(kind=C_CHAR) :: mode
      integer(C_INT) :: buf1, buf1size, buf2, buf2size
    end type xdrfile

    ! interface with libxdrfile
    interface 

      integer(C_INT) function read_xtc_natoms(filename,NATOMS) bind(C, name='read_xtc_natoms')
        import
        character(kind=C_CHAR), intent(in) :: filename
        integer(C_INT), intent(out) :: NATOMS
      end function

      type(C_PTR) function xdrfile_open(filename,mode) bind(C, name='xdrfile_open')
        import
        character(kind=C_CHAR), intent(in) :: filename(*), mode(*)
      end function

      integer(C_INT) function read_xtc(xd,NATOMS,STEP,time,box,x,prec) bind(C, name='read_xtc')
        import
        type(xdrfile), intent(in) :: xd
        integer(C_INT), intent(out) :: NATOMS, STEP
        real(C_FLOAT), intent(out) :: time, prec, box(*), x(*)
      end function

      integer(C_INT) function write_xtc(xd,NATOMS,STEP,time,box,x,prec) bind(C, name='write_xtc')
        import
        type(C_PTR), intent(in) :: xd
        integer(C_INT), value, intent(in) :: NATOMS, STEP
        real(C_FLOAT), intent(in) :: box(*), x(*)
        real(C_FLOAT), value, intent(in) :: time, prec
      end function

      integer(C_INT) function xdrfile_close(xd) bind(C,name='xdrfile_close')
        import
        type(xdrfile), intent(in) :: xd
      end function

    end interface

contains

    subroutine trajectory_constructor(trj, filename_in)

        use, intrinsic :: iso_c_binding, only: C_NULL_CHAR, C_CHAR, c_f_pointer

        implicit none
        class(Trajectory), intent(inout) :: trj
        type(C_PTR) :: xd_c
        character (len=*), intent(in) :: filename_in
        character (len=206) :: filename
        logical :: ex
        integer :: STAT

        inquire(file=trim(filename_in), exist=ex)

        if (ex .eqv. .false.) then
            write(0,*)
            write(0,'(a)') "Error: "//trim(filename_in)//" does not exist."
            write(0,*)
            stop
        end if

        ! Set the file name to be read in for C.
        filename = trim(filename_in)//C_NULL_CHAR

        ! Get number of atoms in system and allocate xyzition array.
        STAT = read_xtc_natoms(filename, trj%NATOMS)

        if (STAT .ne. 0) then
            write(0,*)
            write(0,'(a)') "Error reading in "//trim(filename_in)//". Is it really an xtc file?"
            write(0,*)
            stop
        end if

        ! Open the file for reading. Convert C pointer to Fortran pointer.
        xd_c = xdrfile_open(filename,"r")
        call c_f_pointer(xd_c, trj % xd)

        write(0,'(a)') " Opened "//trim(filename)//" for reading."
        write(0,'(a,i0,a)') " ", trj % NATOMS, " atoms present in system."
        write(0,*)

    end subroutine trajectory_constructor

    subroutine trajectory_read(this)

        implicit none
        class(Trajectory), intent(inout) :: this
        type(Frame), allocatable :: tmpFrameArray(:)
        real :: box_trans(3,3)
        integer :: STAT = 0
        integer :: I = 0

        do while (STAT .eq. 0)

            I = I + 1
            write(0,'(a,i0)') "Frame: ", I

            if (allocated(this%frameArray)) then
                allocate(tmpFrameArray(size(this%frameArray)+1))
                tmpFrameArray(1:size(this%frameArray)) = this%frameArray
                deallocate(this%frameArray)
                call move_alloc(tmpFrameArray, this%frameArray)
            else
                allocate(this%frameArray(1))
            end if

            allocate(this%frameArray(I)%xyz(3,this%NATOMS))
            STAT = read_xtc(this%xd, this%frameArray(I)%NATOMS, this%frameArray(I)%STEP, this%frameArray(I)%time, box_trans, &
                this%frameArray(I)%xyz, this%frameArray(I)%prec)
            ! C is row-major, whereas Fortran is column major. Hence the following.
            this%frameArray(I)%box = transpose(box_trans)

        end do

        this%NFRAMES = I-1

    end subroutine trajectory_read

    subroutine close_xtc(trj)

        implicit none
        class(Trajectory), intent(inout) :: trj
        integer :: STAT

        STAT = xdrfile_close(trj % xd)

    end subroutine close_xtc

    subroutine trajectory_get_xyz(trj, frame, atom, xyz)

        implicit none
        integer, intent(in) :: frame
        integer, intent(in) :: atom
        class(Trajectory), intent(in) :: trj
        real, intent(out) :: xyz(3)

        xyz = trj%frameArray(frame)%xyz(:,atom)

    end subroutine trajectory_get_xyz

end module gmx

