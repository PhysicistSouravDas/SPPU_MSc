! Sourav Das (MSc 1st semester)
! Simpson method
program simpson_method_error
    implicit none
    real, external :: f
    real :: a, b, simpson, true_val, calc_val, err, h, h5
    integer :: n, nmin, nmax, nstep
    logical :: exist
    
    ! reading variable values from user
    write(*,"(a24)") "Enter nmin, nmax, nstep:"
    read(*,*) nmin, nmax, nstep

    write(*,"(a30)") "Enter integration limits a, b:"
    read(*,*) a, b

    ! Our f(x) is sin(x), hence, I = ∫ sin(x) dx from 0 to pi; I = 2.0 => true_val = 2.0
    true_val = 2.0

    ! we are appending to the file at every iteration
    ! but, if someone run the program again, same data will get appended again
    ! so we have to clear the file before writing
    inquire(file="simpson_data.txt", exist=exist)
    if ( exist ) then  ! the file exist, and has old data
        ! empty the file
        open(unit=1, file='simpson_data.txt', status='replace')
        close(1)
    end if

    do n = nmin, nmax, nstep
        h = (b-a)/n  ! calculating h to find h^3
        h5 = h**5
        calc_val = simpson(f, a, b, n)  ! using our simpson function, written below
        err = calc_val - true_val
        open(unit=1, file='simpson_data.txt', action='write', position='append')
        write(1, *) n, h5, calc_val, err
        write(*, *) n, h5, calc_val, err  ! also writing to the terminal
        close(1)
    end do
    write(*,"(a49)") "simpson_data.txt generated and ready for plotting"
end program simpson_method_error

function simpson(f, x0, xn, n) result(area)
    ! This function returns value of ∫f(x)dx from x0 to xn using Simpson method
    ! Input:
    !    f   - functional form of integrand
    !    x0  - lower limit
    !    xn  - upper limit
    !    n   - no. of intervals
    ! Output:
    !    sum - numerically computed integration value
    ! ------------------------------------------------------------------------------
    implicit none
    real :: f, x0, xn, h, s1, s2, area
    integer :: n, i
    
    h = (xn - x0) / n  ! => xn = x0 + nh; therefore x(n-1) = x0 + (n-1)h
    area = f(x0) + f(xn)
    s1 = 0.0
    s2 = 0.0
    do i = 1, n-1, 2
        s1 = s1 + f(x0 + i*h)
    end do
    do i = 2, n-2, 2
        s2 = s2 + f(x0 + i*h)
    end do
    area = (h/3.0) * (area + 4*s1 + 2*s2)
end function simpson

function f(x) result(result)
    ! This is the function to integrate using Simpson method
    implicit none
    real :: x
    real :: result
    result = sin(x)
end function f
!! OUTPUT
! Enter nmin, nmax, nstep:
! 10, 100, 10
! Enter integration limits a, b:
! 0.0, 3.14159
!           10   3.06018419E-03   2.00010967       1.09672546E-04
!           20   9.56307558E-05   2.00000691       6.91413879E-06
!           30   1.25933502E-05   2.00000143       1.43051147E-06
!           40   2.98846112E-06   2.00000048       4.76837158E-07
!           50   9.79259084E-07   2.00000024       2.38418579E-07
!           60   3.93542194E-07   2.00000000       0.00000000    
!           70   1.82077940E-07   2.00000000       0.00000000    
!           80   9.33894100E-08   2.00000000       0.00000000    
!           90   5.18245038E-08   2.00000024       2.38418579E-07
!          100   3.06018464E-08   2.00000000       0.00000000    
! simpson_data.txt generated and ready for plotting
