! Least square fitting
! fit data points from input.txt along line y = ax + b
! a = (N*Sxy - Sx*Sy) / (N*Sx2 - Sx**2)
! b = (Sx2*Sy - Sx*Sxy) / (N*Sx2 - Sx**2)
! Sxy = Σ x_i * y_i; Sx = Σ x_i; Sy = Σ y_i; Sx2 = Σ (x_i)^2
! Steps:
! * Read x, y from input.txt, and add them in array. Read N (no. of data points)
! * Calculate Sxy, Sx, Sy, Sx2
! * Calculate a, b
! * Find y_est using a, b:=> y_est = ax + b
! * Generate output.txt with 3 columns. First 2 columns from input.txt, and last 
!   column having correspoding y_est
! * Plot data points (col 1, 2) and fitted points (col 1, 3) 
program least_fitting
    implicit none
    integer, parameter :: MAX_SIZE = 100
    real, dimension(MAX_SIZE) :: x, y, y_est
    real :: Sx2, Sxy, Sx, Sy, a, b
    integer :: N, i

    N = 10  ! 10 data points in input.txt
    ! opening & reading file
    open(unit=1, file="input.txt", status="old", action="read")
    do i = 1, N
        read(1,*) x(i), y(i)
    end do
    close(1)    ! closing file after reading

    ! initialization of sums
    Sx = 0.0
    Sx2 = 0.0
    Sy = 0.0
    Sxy = 0.0
    ! calculating sums
    do i = 1, N
        Sx = Sx + x(i)
        Sy = Sy + y(i)
        Sx2 = Sx2 + (x(i))**2
        Sxy = Sxy + x(i)*y(i)
    end do
    
    ! calculating slope a and intercept b
    a = (N*Sxy - Sx*Sy) / (N*Sx2 - (Sx**2))
    b = (Sx2*Sy - Sx*Sxy) / (N*Sx2 - (Sx**2))

    ! calculating y_est and
    ! writing first(x), second(y), third(y_est) column in output.txt
    do i = 1, N
        y_est(i) = a*x(i) + b
    end do

    open(unit=2, file="output.txt")
    write(2,100) (x(i), y(i), y_est(i), i=1,N)
    close(2)    
    100 format(3f10.5)
    write(*,"(a20)") "output.txt generated"
    write(*,*) "a =", a
    write(*,*) "b =", b
end program least_fitting