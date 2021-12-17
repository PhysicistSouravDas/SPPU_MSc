! HORNER's method.
! 4x^4 + 5x^3 - 3x^2 + 6x - 1
program polynomial
    implicit none
    real :: ans, x, poly(0:20)
    integer :: i, n

    write(*,*) "Enter degree of polynomial (n): "
    read(*,*) n
    write(*,*) "Enter the polynomial coefficients (a_n, a_(n-1), ..., a_0):"
    ! for an nth degree polynomial, there are (n+1) coefficients, including x^0 coeff.
    read(*,*) (poly(i), i = 0, n)
    write(*,*) "Find the value of polynomial at x = ?:"
    read(*,*) x

    ! Applying HORNER's method.
    ans = poly(0)  ! Initializing to coefficient a_n
    do i = 1, n
        ans = ans*x + poly(i) ! (a_n*x + a_(n-1))*x + ...)*x + a_2)*x + a_1)*x + a_0
    end do
    write(*,*) "The value of polynomial at x=", x, "is", ans
end program polynomial
