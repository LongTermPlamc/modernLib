module mod_initial
    use iso_fortran_env, only: real32, int32
    implicit none

    contains

        pure subroutine setGaussian(x, icenter, decay)
            real(real32), intent(inout) :: x(:)
            integer(int32), intent(in) :: icenter 
            real(real32), intent(in) :: decay
            integer(int32) :: i
            
            do concurrent (i = 1:size(x))
                x(i) = exp(-decay * (i - icenter)**2)
            end do  
        end subroutine setGaussian 

end module mod_initial