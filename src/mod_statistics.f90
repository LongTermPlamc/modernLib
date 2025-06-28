module mod_statistics
    use iso_fortran_env, only: real32, int32
    use mod_arrays, only: alloc
    implicit none

    contains

    pure real function average(array)
        real, intent(in):: array(:)
        average = sum(array)/size(array)
    end function average

    pure real function std(array)
        real, intent(in) :: array(:)
        std = sqrt(average((array-average(array))**2))
    end function std

    function movingAverage(array, window)
        real, intent(in) :: array(:)
        integer, intent(in) :: window
        real, allocatable :: movingAverage(:)
        integer :: length, i, upBound, lowBound

        length = size(array)

        call alloc(movingAverage, length)

        do i=1, length
            upBound = i
            lowBound = max(1,i-window+1)
            movingAverage(i) = average( array(lowBound:upBound) )
        end do

    end function movingAverage

    function movingStd(array, window)
        real, intent(in) :: array(:)
        integer, intent(in) :: window
        real, allocatable :: movingStd(:)
        integer :: length, i, upBound, lowBound

        length = size(array)

        call alloc(movingStd, length)

        do i=1, length
            upBound = i
            lowBound = max(1,i-window+1)
            movingStd(i) = std( array(lowBound:upBound) )
        end do
    end function movingStd

    function crossPoss(x, w) result (res)
        real, intent(in):: x(:)
        integer, intent(in) :: w
        integer, allocatable :: res(:)
        logical, allocatable :: greaterThan(:), lessThan(:)
        real, allocatable :: mvgAvg(:), mvgStd(:)
        integer :: i

        allocate(res(size(x-2)), mvgAvg(size(x)), mvgStd(size(x)))
        allocate(greaterThan(size(x)), lessThan(size(x)))

        res= [(i, i =2, size(x))]
        greaterThan = x > movingAverage(x, w)
        lessThan = x < movingAverage(x, w)

        res = pack(res, greaterThan(2:) .and. lessThan(:size(x)-1))
        deallocate(mvgAvg, mvgStd)

    end function crossPoss

    function crossNeg(x, w) result (res)
        real, intent(in):: x(:)
        integer, intent(in) :: w
        integer, allocatable :: res(:)
        logical, allocatable :: greaterThan(:), lessThan(:)
        real, allocatable :: mvgAvg(:), mvgStd(:)
        integer :: i

        allocate(res(size(x-2)), mvgAvg(size(x)), mvgStd(size(x)))
        allocate(greaterThan(size(x)), lessThan(size(x)))

        res= [(i, i =2, size(x))]
        greaterThan = x > movingAverage(x, w)
        lessThan = x < movingAverage(x, w)

        res = pack(res, lessThan(2:) .and. greaterThan(:size(x)-1))
        deallocate(mvgAvg, mvgStd)

    end function crossNeg

end module mod_statistics