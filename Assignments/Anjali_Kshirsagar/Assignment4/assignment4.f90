! Sourav Das (MSc 1st semester)
! ---------------------------------------------------------------------------------------------
! Program to fit the exponential data using least square fitting, and 
! to find activity (A) and half-life (T_HALF)
! To fit: N(t) = N(0)*exp(-L*t), where L = decay-const (lambda)
! We will convert the exponential problem to a linear problem, and then do linear fitting
! ln(N(t)) = ln(N(0)) + (-L*t)  =>   z = c + mt
! We have to find intercept c, and slope m, from which, we will find N(0) and L
! N(0) = exp(c); L = -m; => z_est = mx + c; => N(t)_est = exp(z_est)
! ---------------------------------------------------------------------------------------------
! This method won't be much accurate, because we are starting with higher values of N(t) (decay),
! and the fit gives less weight to high N(t) values, and more to low N(t) values, which is also
! visible on the plot. This is due to the logarithmic nature.
program EXPONENTIAL_FIT
    implicit none
    integer,parameter :: MAX_SIZE = 100
    real,dimension(MAX_SIZE) :: x, N, N_est, z, z_est
    real :: Sxx, Sxz, Sx, Sz, N0, L, c, m
    integer :: NUM, I

    NUM = 26  ! 26 number of data points in input.txt
    ! opening & reading file
    open(unit=1, file="input.txt", status="old", action="read")
    do I = 1, NUM
        read(1,*) x(I), N(I)
    end do
    close(1)  ! closing file after reading

    ! conversion from N(t) = N to z (z = ln(N))
    do I = 1, NUM
        z(I) = log(N(I))  ! natural logarithm
    end do

    ! initialization of sums
    Sx = 0.0
    Sxx = 0.0
    Sz = 0.0
    Sxz = 0.0
    
    ! calculating sums
    do I = 1, NUM
        Sx = Sx + x(I)
        Sz = Sz + z(I)
        Sxx = Sxx + (x(I))**2
        Sxz = Sxz + x(I)*z(I)        
    end do

    ! calculating slope m and intercept c
    m = (NUM*Sxz - Sx*Sz) / (NUM*Sxx - (Sx**2))
    c = (Sxx*Sz - Sx*Sxz) / (NUM*Sxx - (Sx**2))

    ! calculating z_est
    do I = 1, NUM
        z_est(I) = m*x(I) + c
    end do

    ! conversion of L = -m and N0 = exp(c)
    L = -m
    N0 = exp(c)

    ! conversion of z_est to N_est (z_est = ln(N_est)) => N_est = exp(z_est)
    do I = 1, NUM
        N_est(I) = exp(z_est(I))
    end do

    ! writing first(t = x), second(N(t)), third(N_est) column in output.txt
    open(unit=2, file="output.txt")
    write(2,100) (x(I), N(I), N_est(I), I=1,NUM)
    close(2)
    100 format(3F12.5)
    write(*,200) "Initial number of nuclei, N0 = ", N0
    write(*,300) "Decay constant, λ = ", L, "per second"
    write(*,400) "Activity, A = ", L*N0, "Bq"
    write(*,500) "Half-life, (T1/2) = ", log(2.0)/L, "second"
    200 format(A31, F6.2)
    300 format(A20, 1x, F5.3, 1x, A10)
    400 format(A14, F7.3, 1x, A2)
    500 format(A20, F5.3, 1x, A6)
end program EXPONENTIAL_FIT
! OUTPUT
! Initial number of nuclei, N0 = 842.34
! Decay constant, λ = 0.207 per second
! Activity, A = 174.647 Bq
! Half-life, (T1/2) = 3.343 second
