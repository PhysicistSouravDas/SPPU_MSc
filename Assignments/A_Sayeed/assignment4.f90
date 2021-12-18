! Sourav Das (1st Sem, MSc); ID: 21021085
! Bisection Method program
program bisection_method
    implicit none
    real :: tol, a, b, c, f, x

    f(x) = cos(x) - x   ! defining given function

    write(*,100) "Enter interval a, b:"
    read(*,*) a, b
    100 format(a20)
    do  ! loop to retake interval if root doesn't lie between a, b
        if ( f(a)*f(b) > 0.0 ) then
            write(*,200) "The root doesn't lie in this interval. Enter a, b again:"
            read(*,*) a, b
        else
            exit
        end if
    end do
    200 format(a56)

    write(*,100) "Enter the tolerance:"
    read(*,*) tol

    do
        c = (a+b)/2.0   ! Bisection of a and b in every iteration
        if ( f(a)*f(c) < 0.0 ) then
            b = c   ! Then, c is the new b
        else
            a = c   ! Else c is the new a
        end if
        if (abs(f(c)) < tol) exit
    end do
    write(*,300) "The root of function in this interval is:", c
    300 format(a41, 1x, f7.5)

end program bisection_method

! OUTPUT
! Enter interval a, b:
! 0, 5
! Enter the tolerance:
! 0.000001
! The root of function in this interval is: 0.73909