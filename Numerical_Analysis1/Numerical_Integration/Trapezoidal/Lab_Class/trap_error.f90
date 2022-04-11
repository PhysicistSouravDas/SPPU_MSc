! Trapezoidal method
! Sourav Das (MSc 1st Sem); 27 Dec 2021
! We will try to plot the absolute error as a function of x

program trapezoidal_method_error
    implicit none
    real, external :: f
    real :: a, b, trapezoidal, true_val, calc_val, err, h, h2, h3
    integer :: n, nmin, nmax, nstep
    logical :: exist
    ! f(x) = x**3
    
    ! Our f(x) is x^3, hence, I = ∫ x^3 dx from 0 to 1; I = 1/4 => true_val = 1/4
    nmin = 10
    nmax = 100
    nstep = 10
    true_val = 2.0
    a = 0.0
    b = 3.14159
    ! we are appending to the file at every iteration
    ! but, if someone run the program again, same data will get appended again
    ! so we have to clear the file before writing
    inquire(file="trap.txt", exist=exist)
    if ( exist ) then  ! the file exist, and has old data
        ! empty the file
        open(unit=1, file='trap.txt', status='replace')
        close(1)
    end if

    do n = nmin, nmax, nstep
        h = (b-a)/n
        h2 = h*h
        h3 = h**3
        calc_val = trapezoidal(f, a, b, n)
        err = (abs(true_val - calc_val)/calc_val) * 100
        open(unit=1, file='trap.txt', action='write', position='append')
        write(1, *) n, h, h2, h3, err, calc_val
        close(1)
    end do
    write(*,*) "trap.txt generated and ready for plotting"
end program trapezoidal_method_error

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
    result = x*x*x
end function f