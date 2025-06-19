program mod_diff_test
    use mod_diff, only: diff
    implicit none

    real:: x(10), y(10)

    x=[1,2,3,4,5,6,7,8,9,10]
    y = diff(x)

    print*, x
    print*, y
    


end program mod_diff_test