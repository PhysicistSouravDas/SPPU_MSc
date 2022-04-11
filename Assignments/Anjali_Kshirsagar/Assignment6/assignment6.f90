! Sourav Das (MSc 1st semester)
program capacitor_current
    implicit none
    real,external :: I
    real :: time, capacitance, Q, Qtot, a, b

    ! Q : Charge per interval (1-5 sec, 5-10 sec and so on)
    ! Qtot : Total charge until some time, including the charge of previous intervals
    
    ! We will integrate I(t) from t=1 to t=20.
    ! But integrating just from 1 to 20 will give us just a number Q,
    ! which we can't plot with time.
    ! We will divide t=1 to 20 in several intervals (say, 1-5, 5-10, ...)
    ! And, we can plot Q(t) vs t. We have to include charge of previous intervals.
    
    ! do integration to find charge Q
    time = 1.00
    Qtot = 0.0
    open(unit=1, file='output.txt', action='write')
    do
        a = time    ! lower limit
        if ( time < 5.0 ) then
        ! since 1 to 5 is not equidistant as 5-10, 10-15, so implementing logic below
            time = time + 4.0
        else
            time = time + 5.0
        end if
        b = time    ! upper limit

        call trapezoidal(I, a, b, 100, Q)   ! calling our subroutine trapezoidal

        capacitance = 0.025        
        Qtot = Qtot + Q    ! adding up charge of previous interval
        ! writing to file for plotting
        write(1,*) time, Q, Q/capacitance, Qtot, Qtot/capacitance
        ! We will use 1st, 4th and 5th column for plotting
        if ( time > 20.0 ) exit   ! integrating upto 20
    end do
    close(2)
    
    ! writing final charge (Qtot) and Voltage across capacitor to terminal
    write(*,100) "Charge, Q =", Qtot
    write(*,200) "Voltage, V =", Qtot/capacitance
    100 format(A11, 1x, F5.2)
    200 format(A12, 1x, F7.2)

end program capacitor_current


function I(t) result(val)
    ! Current function provided to us in the problem
    ! This I and subroutine's integer i (line 77), won't do any conflict,
    ! because that i is defined in that subroutine's scope only
    implicit none
    real :: t
    real :: val
    val = 4*(1-exp(-0.5))*exp(-0.5*(t-1))*(1-exp(-t))
end function I

subroutine trapezoidal(f, x0, xn, n, result)
    ! This function returns value of ∫f(x)dx from x0 to xn using Trapezoidal method
    ! Working formula: I = (h/2)*[f(x0) + f(xn) + 2*(f(x1) + f(x2) + ... + f(xn-1))]
    ! Input:
    !    f   - functional form of integrand
    !    x0  - lower limit
    !    xn  - upper limit
    !    n   - no. of intervals
    ! Output:
    !    result - numerically computed integration value
    ! ------------------------------------------------------------------------------
implicit none
real,external :: f
real,intent(in) :: x0, xn
integer,intent(in) :: n
real,intent(out) :: result
real :: sum, h
integer :: i

h = (xn - x0) / n   ! => xn = x0 + nh; therefore x(n-1) = x0 + (n-1)h
sum = f(x0) + f(xn)

do i = 1, n-1
    sum = sum + 2*f(x0 + i*h)
end do
result = (h/2.0) * sum
end subroutine trapezoidal

! OUTPUT
! Charge, Q =  2.76
! Voltage, V =  110.47
