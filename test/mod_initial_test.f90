program mod_initial_test
    use mod_initial, only: setGaussian
    implicit none

    real:: x(100)
    integer:: center = 40
    real:: decay = 10


    call setGaussian(x, center, decay)

    print*, x
    


end program mod_initial_test