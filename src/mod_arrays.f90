module mod_arrays
    use iso_fortran_env, only: int32, real32

    implicit none

    contains

    subroutine alloc(a, n)
        real, allocatable, intent(inout) :: a(:)
        integer, intent(in):: n
        integer :: stat
        character(len=100) :: errmsg

        if (allocated(a)) call myfree(a) !! if allocated, free memory
        allocate(a(n), stat= stat, errmsg=errmsg) !! allocate storing status in stat and erromsg in errmsg
        if (stat>0) error stop errmsg !! if status is not success, stop execution.
    end subroutine alloc

    !! free memory of an allocated array
    subroutine myfree(a)
        real, allocatable, intent(inout) :: a(:)
        integer :: stat
        character(len=100):: errmsg

        if (.not. allocated(a)) return !! if array is not allocated nothing happens
        deallocate(a, stat=stat, errmsg = errmsg) !! else, deallocates storing stat an errmsg
        if (stat>0) error stop errmsg

    end subroutine myfree

    function reverse(arr)
        real, allocatable, intent(in):: arr(:)
        real, allocatable :: reverse(:)
        integer i, ln

        ln = size(arr)
        allocate(reverse(ln))
        
        do i = 1, ln
        reverse(i) = arr(ln+1-i)
        end do

    end function reverse


end module mod_arrays