program trapz
    implicit none
    real, external :: f
    real :: a, b, trapezoidal, area
    integer :: n    
    
    write(*,"(a14)") "Enter n, a, b:"
    read(*,*) n, a, b
    ! n = 100
    ! a = 0.0
    ! b = 3.14159    
    area = trapezoidal(f, a, b, n)
    write(*,"(a19, f7.2)") "Value of integral: ", area
    
end program trapz

function trapezoidal(f, x0, xn, n) result(sum)
    ! This function returns value of ∫f(x)dx from x1 to xn using Trapezoidal method
    ! Working formula: I = (h/2)*[f(x0) + f(xn) + 2*(f(x1) + f(x2) + ... + f(xn-1))]
    ! Input:
    !    f   - functional form of integrand
    !    x1  - lower limit
    !    xn  - upper limit
    !    n   - no. of intervals
    ! Output:
    !    sum - numerically computed integration value
    ! ------------------------------------------------------------------------------
    implicit none
    real :: f, x0, xn, h, sum
    integer :: n, i
    
    h = (xn - x0) / n  ! => xn = x0 + nh; therefore x(n-1) = x0 + (n-1)h
    sum = f(x0) + f(xn)

    do i = 1, n-1
        sum = sum + 2*f(x0 + i*h)
    end do
    sum = (h/2) * sum
end function trapezoidal

function f(x) result(result)
    implicit none
    real :: x
    real :: result
    result = sin(x)
end function f