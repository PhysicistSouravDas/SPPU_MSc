! Sourav Das (MSc 1st Semester)
! Least square fitting (Exponential curve)
! We will convert the exponential curve to straight line
! Exponential fitting (y = ae^(bx))
! => log(y) = log(a) + bx     => z = c + mx
! m = (N*Sxy - Sx*Sy) / (N*Sx2 - Sx**2)
! c = (Sx2*Sy - Sx*Sxy) / (N*Sx2 - Sx**2)
! Sxy = Σ x_i * y_i; Sx = Σ x_i; Sy = Σ y_i; Sx2 = Σ (x_i)^2
! Steps:
! * Read x, y from input.txt, and add them in array. Read N (no. of data points)
! * Calculate Sxy, Sx, Sy, Sx2
! * Calculate a1, a0
! * Find y_est using m, c:=> y_est = mx + c
! * Generate output.txt with 3 columns. First 2 columns from input.txt, and last 
!   column having correspoding y_est
! * Plot data points (col 1, 2) and fitted points (col 1, 3) 

program least_fitting_exp
    implicit none
    integer, parameter :: MAX_SIZE = 100
    real, dimension(MAX_SIZE) :: x, y, y_est, z, z_est
    real :: Sx2, Sxz, Sx, Sz, A, b, c, m
    integer :: N, i

    N = 11  ! 11 data points in input.txt
    ! opening & reading file
    open(unit=1, file="input7.txt", status="old", action="read")
    do i = 1, N
        read(1,*) x(i), y(i)
    end do
    close(1)    ! closing file after reading

    ! conversion from y to z (z = log(y))
    do i = 1, N
        z(i) = log(y(i))
    end do

    ! initialization of sums
    Sx = 0.0
    Sx2 = 0.0
    Sz = 0.0
    Sxz = 0.0
    ! calculating sums
    do i = 1, N
        Sx = Sx + x(i)
        Sz = Sz + z(i)
        Sx2 = Sx2 + (x(i))**2
        Sxz = Sxz + x(i)*z(i)
    end do
    
    ! calculating slope m and intercept c
    m = (N*Sxz - Sx*Sz) / (N*Sx2 - (Sx**2))
    c = (Sx2*Sz - Sx*Sxz) / (N*Sx2 - (Sx**2))

    ! calculating z_est = y_est
    do i = 1, N
        z_est(i) = m*x(i) + c
    end do

    ! conversion of b = m and c = log(a) => a = e^c
    b = m
    a = exp(c)

    ! conversion of z_est to y_est (z_est = log(y_est) => y_est = exp(z_est))
    do i = 1, n
        y_est(i) = exp(z_est(i))
    end do

    ! writing first(x), second(y), third(y_est) column in output.txt
    open(unit=2, file="output7.txt")
    write(2,100) (x(i), y(i), y_est(i), i=1,N)
    close(2)    
    100 format(3f10.5)
    write(*,"(a20)") "output.txt generated"
    write(*,*) "a =", a
    write(*,*) "b =", b
end program least_fitting_exp
! OUTPUT
! output.txt generated
!  a =   2.26691031    
!  b =   1.27928185  