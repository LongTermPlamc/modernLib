program mod_diff_test
    use mod_diff, only: diff, diff2Cent
    implicit none

    real:: x(10), y(10), yprime(2:9)

    x=[1,2,3,4,5,6,7,8,9,10]
    y = diff(x)

    !!print*, x
    !!print*, y
    x = x**2
    yprime = diff2Cent(x)
    
    print*, x
    print*, yprime

end program mod_diff_test