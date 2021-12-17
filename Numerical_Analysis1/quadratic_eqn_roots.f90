! Write a program to find out roots of quadratic equation (ax^2 + bx + c = 0) where
! a = 4, b = 4, c = 1
! a = 1, b = 1, c = 4
! a = c = 1, b = +10000.01; and b = -10000.01
! discriminant: D = b^2 - 4ac decides type of roots

program quad_roots
    implicit none
    real :: a, b, c, x1, x2, D

    do  ! sanitizing user input
        write(*,*) "Enter coefficients of quadratic eqn (a, b, c):"
        read(*,*) a, b, c
        if ( a /= 0 ) then
            exit
        end if
        write(*,*) "a = 0 is not possible."
    end do

    D = b**2 - 4*a*c ! discriminant

    if ( D < 0 ) then
        write(*,*) "Roots are complex numbers."
        x1 = -b/(2*a)
        x2 = sqrt(-D)/(2*a)
        ! Roots are x1 + ix2 and x1 - ix2
        write(*,100) "First root: ", x1, "+ i", x2
        write(*,100) "Second root:", x1, "- i", x2
    else if ( D > 0 ) then
        write(*,*) "Roots are real and distinct."
        x1 = (-b + sqrt(D))/(2*a)
        x2 = (-b - sqrt(D))/(2*a)
        write(*,100) "Roots are", x1, "and", x2
    else
        write(*,*) "Roots are real and NOT distinct."
        x1 = -b/(2*a)
        write(*,200) "Root is", x1
    end if
    100 format(a12, f9.2, a3, f9.2)
    200 format(a7, f9.2)
end program quad_roots

! OUTPUT1
! Enter coefficients of quadratic eqn (a, b, c):
! 4, 4, 1
!  Roots are real and NOT distinct.
! Root is    -0.50

! OUTPUT2
! Enter coefficients of quadratic eqn (a, b, c):
! 1, 1, 4
!  Roots are complex numbers.
! First root:     -0.50+ i     1.94
! Second root:    -0.50- i     1.94

! OUTPUT3
! Enter coefficients of quadratic eqn (a, b, c):
! 1, 10000.01, 1
!  Roots are real and distinct.
!    Roots are     0.00and-10000.01