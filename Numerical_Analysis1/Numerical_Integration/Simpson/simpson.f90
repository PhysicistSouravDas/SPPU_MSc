program simpson_method
    implicit none
    real, external :: f
    real :: a, b, simpson, area
    integer :: n    
    
    write(*,"(a14)") "Enter n, a, b:"
    read(*,*) n, a, b
    ! n = 100
    ! a = 0.0
    ! b = 3.14159    
    area = simpson(f, a, b, n)
    write(*,"(a19, f7.2)") "Value of integral: ", area
    
end program simpson_method

function simpson(f, x0, xn, n) result(area)
    implicit none
    real :: f, x0, xn, h, area
    integer :: n, i
    
    h = (xn - x0) / n  ! => xn = x0 + nh; therefore x(n-1) = x0 + (n-1)h
    area = f(x0) + f(xn)

    do i = 1, n-1, 2
        area = area + 4*f(x0 + i*h)
    end do
    do i = 2, n-2, 2
        area = area + 2*f(x0 + i*h)
    end do
    area = (h/3.0) * area
end function simpson

function f(x) result(result)
    implicit none
    real :: x
    real :: result
    result = sin(x)
end function f