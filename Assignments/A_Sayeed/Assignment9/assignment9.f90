! Sourav Das (MSc 1st semester)
! Trapezoidal method
program trapezoidal_method_error
    implicit none
    real, external :: f
    real :: a, b, trapezoidal, true_val, calc_val, err, h, h3
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
    inquire(file="trap.txt", exist=exist)
    if ( exist ) then  ! the file exist, and has old data
        ! empty the file
        open(unit=1, file='trap.txt', status='replace')
        close(1)
    end if

    do n = nmin, nmax, nstep
        h = (b-a)/n  ! calculating h to find h^3
        h3 = h**3
        calc_val = trapezoidal(f, a, b, n)  ! using our trapezoidal function, written below
        err = true_val - calc_val
        open(unit=1, file='trap.txt', action='write', position='append')
        write(1, *) n, h3, calc_val, err
        write(*, *) n, h3, calc_val, err  ! also writing to the terminal
        close(1)
    end do
    write(*,"(a41)") "trap.txt generated and ready for plotting"
end program trapezoidal_method_error

function trapezoidal(f, x0, xn, n) result(sum)
    ! This function returns value of ∫f(x)dx from x0 to xn using Trapezoidal method
    ! Working formula: I = (h/2)*[f(x0) + f(xn) + 2*(f(x1) + f(x2) + ... + f(xn-1))]
    ! Input:
    !    f   - functional form of integrand
    !    x0  - lower limit
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
    sum = (h/2.0) * sum
end function trapezoidal

function f(x) result(result)
    ! This is the function to integrate using Trapezoidal method
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
!           10   3.10062002E-02   1.98352385       1.64761543E-02
!           20   3.87577503E-03   1.99588621       4.11379337E-03
!           30   1.14837778E-03   1.99817216       1.82783604E-03
!           40   4.84471879E-04   1.99897182       1.02818012E-03
!           50   2.48049619E-04   1.99934220       6.57796860E-04
!           60   1.43547222E-04   1.99954271       4.57286835E-04
!           70   9.03970795E-05   1.99966431       3.35693359E-04
!           80   6.05589848E-05   1.99974275       2.57253647E-04
!           90   4.25325197E-05   1.99979699       2.03013420E-04
!          100   3.10062023E-05   1.99983525       1.64747238E-04
! trap.txt generated and ready for plotting
