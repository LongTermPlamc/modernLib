module mod_io
    use iso_fortran_env, only: real32, int32
    use mod_arrays, only: alloc
    implicit none

    contains
    !! This function just parses the file until EOF and records how many lines are there.
    integer function num_records(filename) 
        character(len=*), intent(in):: filename
        integer :: fileunit

        open(newunit=fileunit, file=filename)
        num_records = 0
        do
            read(unit=fileunit, fmt=*, end=1) !! This end = 1 enable the program to jump to line labeled with 1 when EOF is reached.
            num_records = num_records+1
        end do
        1 continue
        close(unit=fileunit)
    end function num_records

    !! This function reads the file "filename" and stores each column in a diferent array. 
    subroutine readStock(filename, time, open, high, low, close, adjclose, volume)
        character(*), intent(in) :: filename
        character(:), allocatable, intent(inout) :: time(:)
        real, allocatable, intent(inout):: open(:), &
        high(:), low(:), close(:), adjclose(:), volume(:)
        integer :: fileunit
        integer :: n, nm
        
        !! Counts how many lines has the file. doesn't count the header. 
        nm = num_records(filename)-1
        
        !! Allocates the arrays to the found number of records. 
        if (allocated(time)) deallocate(time)
        allocate(character(10):: time(nm))
        call alloc(open, nm)
        call alloc(high, nm)
        call alloc(low, nm)
        call alloc(close, nm)
        call alloc(adjclose, nm)
        call alloc(volume, nm)
        
        !! Opens the file again and reads each line storing each element into the nth position
        open(newunit=fileunit, file=filename)
        read(fileunit, fmt=*, end=1) !! When EOF is reached, it jumps to close the file
        do n=1, nm
            read(fileunit, fmt=*, end=1) time(n), open(n), high(n), low(n), close(n), adjclose(n), volume(n)
        end do
        
        1 close(fileunit)
    end subroutine readStock

    subroutine writeStock(filename, time, adjClose, movAverage, movStd)
        character(*), intent(in):: filename
        character(:), intent(in), allocatable :: time(:)
        real, intent(in), allocatable:: adjClose(:),movAverage(:), movStd(:)

        integer :: len, i, fileunit
        character(40) :: frmt

        frmt = "(A10,f12.6,f12.6,f12.6)"

        open(newunit=fileunit, file=filename)

        len = size(time)
        do i = 1, len
        write(unit =fileunit, fmt= trim(frmt)) time(i), adjClose(i),movAverage(i), movStd(i)
        end do
        
        close(fileunit)

    end subroutine writeStock


end module mod_io