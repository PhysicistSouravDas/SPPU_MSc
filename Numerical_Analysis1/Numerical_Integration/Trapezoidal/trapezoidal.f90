! Trapezoidal method
! Sourav Das (MSc 1st Sem); 21 Dec 2021
! We will try to plot the absolute error as a function of x

program trapezoidal_method_error
    implicit none
    real, external :: f
    real :: a, b, trapezoidal, true_val, calc_val, err, h
    integer :: n
    logical :: exist
    ! f(x) = x**3
    
    ! Our f(x) is x^3, hence, I = ∫ x^3 dx from 0 to 1; I = 1/4 => true_val = 1/4
    true_val = 0.25
    a = 0
    b = 1
    ! we are appending to the file at every iteration
    ! but, if someone run the program again, same data will get appended again
    ! so we have to clear the file before writing
    ! Reference for inquire, exist: https://stackoverflow.com/a/15530715
    inquire(file="data3.txt", exist=exist)
    if ( exist ) then  ! the file exist, and has old data
        ! empty the file
        open(unit=1, file='data3.txt', status='replace')
        close(1)
    end if

    n = 10
    do while (n <= 1000)
        h = (b-a)/n
        calc_val = trapezoidal(f, a, b, n)
        err = (abs(true_val - calc_val)/calc_val) * 100
        open(unit=1, file='data3.txt', action='write', position='append')
        write(1, *) h, err
        close(1)
        n = 2*n
    end do
    write(*,*) "data3.txt generated and ready for plotting"
end program trapezoidal_method_error

function trapezoidal(f, x1, xn, n) result(sum)
    ! This function returns value of ∫f(x)dx from x1 to xn using Trapezoidal method
    ! Working formula: I = (h/2)*[f(x1) + f(xn) + 2*(f(x2) + f(x3) + ... + f(xn-1))]
    ! Input:
    !    f   - functional form of integrand
    !    x1  - lower limit
    !    xn  - upper limit
    !    n   - no. of intervals
    ! Output:
    !    sum - numerically computed integration value
    ! ------------------------------------------------------------------------------
    implicit none
    real :: f, x1, xn, h, sum
    integer :: n, i
    
    h = (xn - x1) / n  ! => xn = x1 + nh; therefore xn-1 = x1 + (n-1)h
    sum = f(x1) + f(xn)

    do i = 1, n-1
        sum = sum + 2*f(x1 + i*h)
    end do
    sum = (h/2) * sum
end function trapezoidal

function f(x) result(result)
    implicit none
    real :: x
    real :: result
    result = x*x*x
end function f