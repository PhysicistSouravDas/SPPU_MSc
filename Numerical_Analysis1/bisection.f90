! Bisection Method program
program bisection_method
    implicit none
    real :: tol, a, b, c, f, x
    ! f(x) = x**2 - 1 ! function define
    f(x) = sin(x) - x*cos(x)

    tol = 1e-6
    write(*,*) "Enter a, b:"
    read(*,*) a, b
    do
        if ( f(a)*f(b) > 0.0 ) then
            write(*,*) "The root doesn't lie in this interval. Enter a, b again:"
            read(*,*) a, b
        else
            exit
        end if
    end do

    do
        c = (a+b)/2.0   ! Bisection of a and b in every iteration
        if ( f(a)*f(c) < 0.0 ) then
            b = c   ! Then, c is the new b
        else
            a = c   ! Else c is the new a
        end if
        if (abs(f(c)) < tol) exit
    end do
    write(*,*) "The root of function in this interval is:", c

end program bisection_method