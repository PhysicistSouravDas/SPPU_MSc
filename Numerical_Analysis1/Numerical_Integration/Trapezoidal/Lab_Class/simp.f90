! Simpson method
! Sourav Das (MSc 1st Sem); 27 Dec 2021
! We will try to plot the absolute error as a function of x

program simpson_method_error
    implicit none
    real, external :: f
    real :: a, b, simpson, true_val, calc_val, err, h, h4, h5
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
    inquire(file="simp_dat.txt", exist=exist)
    if ( exist ) then  ! the file exist, and has old data
        ! empty the file
        open(unit=1, file='simp_dat.txt', status='replace')
        close(1)
    end if

    do n = nmin, nmax, nstep
        h = (b-a)/n
        h4 = h**4
        h5 = h**5
        calc_val = simpson(f, a, b, n)
        err = (abs(true_val - calc_val)/calc_val) * 100
        open(unit=1, file='simp_dat.txt', action='write', position='append')
        write(1, *) n, h, h4, h5, err, calc_val
        close(1)
    end do
    write(*,*) "simp_dat.txt generated and ready for plotting"
end program simpson_method_error

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