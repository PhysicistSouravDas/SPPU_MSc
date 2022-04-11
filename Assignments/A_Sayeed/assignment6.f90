! Sourav Das (MSc 1st Semester)
! Least square fitting
! fit data points from input.txt along line y = a1*x + a0
! a1 = (N*Sxy - Sx*Sy) / (N*Sx2 - Sx**2)
! a0 = (Sx2*Sy - Sx*Sxy) / (N*Sx2 - Sx**2)
! Sxy = Σ x_i * y_i; Sx = Σ x_i; Sy = Σ y_i; Sx2 = Σ (x_i)^2
! Steps:
! * Read x, y from input.txt, and add them in array. Read N (no. of data points)
! * Calculate Sxy, Sx, Sy, Sx2
! * Calculate a1, a0
! * Find y_est using a1, a0:=> y_est = a1*x + a0
! * Generate output.txt with 3 columns. First 2 columns from input.txt, and last 
!   column having corresponding y_est
! * Plot data points (col 1, 2) and fitted points (col 1, 3) 
program least_fitting
    implicit none
    integer, parameter :: MAX_SIZE = 100
    real, dimension(MAX_SIZE) :: x, y, y_est
    real :: Sx2, Sxy, Sx, Sy, a1, a0
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
    
    ! calculating slope a1 and intercept a0
    a1 = (N*Sxy - Sx*Sy) / (N*Sx2 - (Sx**2))
    a0 = (Sx2*Sy - Sx*Sxy) / (N*Sx2 - (Sx**2))

    ! calculating y_est and
    ! writing first(x), second(y), third(y_est) column in output.txt
    do i = 1, N
        y_est(i) = a1*x(i) + a0
    end do

    open(unit=2, file="output.txt")
    write(2,100) (x(i), y(i), y_est(i), i=1,N)
    close(2)    
    100 format(3f10.5)
    write(*,"(a20)") "output.txt generated"
    write(*,*) "a0 =", a0
    write(*,*) "a1 =", a1
end program least_fitting
! OUTPUT
! output.txt generated
! a0 =  -1.23367953    
! a1 =   6.14878893    
