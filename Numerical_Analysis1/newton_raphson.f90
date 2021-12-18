program newton_raphson
    implicit none
    real :: x, x0, x_i, tol, lim, f, g

    ! defining function f(x) and its derivative g(x)
    f(x) = sin(x) + exp(x)
    g(x) = cos(x) + exp(x)

    write(*,"(a16)") "Enter tolerance:"
    read(*,*) tol
    write(*,"(a18)") "Enter guess value:"
    read(*,*) x0
    ! a limiting value for checking if derivative nearly equals zero
    lim = 1e-6

    do
        if ( abs(f(x0)) < tol ) exit  ! exit loop if accuracy reached
        if ( abs(g(x0)) < lim ) write(*,"(a20)") "Derivative too small!"
        x_i = x0 - (f(x0) / g(x0))
        x0 = x_i
    end do

    write(*,100) "The root is", x0
    100 format(a11, 1x, f7.4)

end program newton_raphson

! OUTPUT
! Enter tolerance:
! 0.00001
! Enter guess value:
! 1.0
! The root is -0.5885