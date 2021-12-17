! Write a program to evaluate 10 order polynomial (modify the do loop)
! to comply to HORNER's method.
program polynomial
    implicit none
    real*8 :: ans, x, poly(20)
    integer :: i, n

    write(*,*) "Enter degree of polynomial (n): "
    read(*,*) n
    write(*,*) "The degree you entered is:", n
    write(*,*) "Enter the polynomial coefficients (a_n, a_(n-1), ..., a_0):"
    ! for an nth degree polynomial, there are (n+1) coefficients, including x^0 coeff.
    read(*,*) (poly(i), i = 1, n+1)
    write(*,*) "The coefficients you entered are:"
    write(*,*) (poly(i), i = 1, n+1)
    write(*,*) "Find the value of polynomial at x = ?:"
    read(*,*) x
    write(*,*) "You entered x =", x

    ! Applying HORNER's method.
    ans = poly(1)  ! Initializing to coefficient a_n
    do i = 2, n+1
        ans = ans*x + poly(i) ! (a_n*x + a_(n-1))*x + ...)*x + a_2)*x + a_1)*x + a_0
    end do
    write(*,*) "The value of polynomial at x=", x, "is", ans
end program polynomial

! OUTPUT1
! Enter degree of polynomial (n): 
! 10
!  The degree you entered is:          10
!  Enter the polynomial coefficients (a_n, a_(n-1), ..., a_0):
! 11 10 9 8 7 6 5 4 3 2 1
!  The coefficients you entered are:
!    11.0000000       10.0000000       9.00000000       8.00000000       7.00000000       6.00000000       5.00000000       
! 4.00000000       3.00000000       2.00000000       1.00000000
!  Find the value of polynomial at x = ?:
! 5
!  You entered x =   5.00000000    
!  The value of polynomial at x=   5.00000000     is   131225592.
!!!!! Actual result is 131225586 !!!!!

! OUTPUT2
! Enter degree of polynomial (n):
! 8
!  The degree you entered is:           8
!  Enter the polynomial coefficients (a_n, a_(n-1), ..., a_0):
! 1 2 3 -7 6 3 0 1 5
!  The coefficients you entered are:
!    1.00000000       2.00000000       3.00000000      -7.00000000       6.00000000       3.00000000       0.00000000       
! 1.00000000       5.00000000
!  Find the value of polynomial at x = ?:
! 2
!  You entered x =   2.00000000
!  The value of polynomial at x=   2.00000000     is   607.000000
!!!!! Actual result is 607 !!!!!
