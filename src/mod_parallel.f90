module mod_parallel
    use iso_fortran_env, only: real32, int32
    implicit none

    contains

    pure function tile_indices(dims)
        ! Given input global array size, return start and end index
        ! of a parallel 1-d tile that correspond to this image.
        integer, intent(in) :: dims
        integer :: tile_indices(2)
        integer :: offset, tile_size

        tile_size = dims / num_images()

        ! start and end indices assuming equal tile sizes
        tile_indices(1) = (this_image() - 1) * tile_size + 1 !! 3 -> 5, 6
        tile_indices(2) = tile_indices(1) + tile_size - 1 !!  4 -> 7, 8

        ! if we have any remainder, distribute it to the tiles at the end 
        offset = num_images() - mod(dims, num_images()) ! 2
        if (this_image() > offset) then
        tile_indices(1) = tile_indices(1) + this_image() - offset - 1 !5  !8
        tile_indices(2) = tile_indices(2) + this_image() - offset ! 7  !10
        end if

    end function tile_indices

    function tile_neighbors()
        integer :: leftN, rightN
        integer:: tile_neighbors(2)

        if (num_images() == 1) then
        leftN = 1
        rightN = 1
        else if (num_images()>1) then
        if (this_image() == 1) then
            leftN = num_images()
        else
            leftN = this_image() -1
        endif
        if (this_image()==num_images())then
            rightN = 1
        else
            rightN = this_image() +1
        endif
        end if

        tile_neighbors = [leftN, rightN]

    end function tile_neighbors


end module mod_parallel