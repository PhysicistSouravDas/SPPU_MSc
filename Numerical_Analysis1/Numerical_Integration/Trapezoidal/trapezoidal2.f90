! Trapezoidal method
! Sourav Das (MSc 1st Sem); 27 Dec 2021

program trapezoidal_method
    implicit none
    real :: f, x0, xn, h, ans
    integer :: n, i

    write(*,"(a18)") "Enter limits a, b:"
    read(*,*) x0, xn
    write(*,"(a27)") "Enter no. of intervals (n):"
    read(*,*) n

    h = (xn - x0) / n  ! => xn = x0 + nh; therefore x(n-1) = x0 + (n-1)h
    ans = f(x0) + f(xn)

    do i = 1, n-1
        ans = ans + 2*f(x0 + i*h)
    end do
    ans = (h/2.0) * ans

    write(*,100) "Estimated value of integral:", ans
    100 format(a28, 1x, f7.3)

end program trapezoidal_method

function f(x) result(result)
    implicit none
    real :: x, result
    result = sin(x)
end function f