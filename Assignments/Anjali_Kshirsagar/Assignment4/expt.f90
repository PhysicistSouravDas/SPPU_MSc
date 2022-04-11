! Sourav Das (MSc 1st semester)
! December 2021
! Program to fit the exponential data using least square fitting, and 
! to find activity (A) and half-life (T_HALF)
! To fit: N(t) = N(0)*exp(-L*t), where L = decay-const (lambda)
! We will convert the exponential problem to a linear problem, and then do linear fitting
! ln(N(t)) = ln(N(0)) + (-L*t)  =>   z = c + mt
! We have to find intercept c, and slope m, from which, we will find N(0) and L
! N(0) = exp(c); L = -m; => z_est = 
program EXPONENTIAL_FIT
    implicit none
    integer,parameter :: MAX_SIZE = 100
    real,dimension(MAX_SIZE) :: x, N, N_est, z, z_est, N_est2
    real :: N0, L, c, m, Sx2n, Snz, Sxn, Sxnz, Sn
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
        write(*,*) "z,N,log(N)", z(I), N(I), log(N(I))
    end do

    ! initialization of sums
    ! Sx = 0.0
    ! Sxx = 0.0
    ! Sz = 0.0
    ! Sxz = 0.0
    ! modified sums
    Sx2n = 0.0
    Snz = 0.0
    Sxn = 0.0
    Sxnz = 0.0
    Sn = 0.0
    
    ! calculating sums (modified sums)
    ! https://mathworld.wolfram.com/LeastSquaresFittingExponential.html
    ! https://stats.stackexchange.com/questions/47266/fitting-an-exponential-function-using-least-squares-vs-generalized-linear-model
    ! https://archive.lib.msu.edu/crcmath/math/math/l/l142.htm

    do I = 1, NUM
        Sx2n = Sx2n + (x(I)**2)*N(I)
        Snz = Snz + N(I)*z(I)
        Sxn = Sxn + x(I)*N(I)
        Sxnz = Sxnz + x(I)*N(I)*z(I)
        Sn = Sn + N(I)
    end do

    ! calculating slope m and intercept c
    c = (Sx2n*Snz - Sxn*Sxnz) / (Sn*Sx2n - (Sxn**2))
    m = (Sn*Sxnz - Sxn*Snz) / (Sn*Sx2n - (Sxn**2))
    ! m = (NUM*Sxz - Sx*Sz) / (NUM*Sxx - (Sx**2))
    ! c = (Sxx*Sz - Sx*Sxz) / (NUM*Sxx - (Sx**2))

    ! calculating z_est
    do I = 1, NUM
        z_est(I) = m*x(I) + c
        write(*,*) "zest,x", z_est(I), x(I)
    end do

    ! conversion of L = -m and N0 = exp(c)
    L = -m
    N0 = exp(c)
    ! N0 = 1000

    ! conversion of z_est to N_est (z_est = ln(N_est)) => N_est = exp(z_est)
    do I = 1, NUM
        N_est(I) = exp(z_est(I))
        write(*,*) "Nest,zest,e^zest", N_est(I), z_est(I), exp(z_est(I))
    end do
    do I = 1, NUM
        N_est2(I) = N0*exp(-L*x(I))
    end do

    ! writing first(t = x), second(N(t)), third(N_est) column in output.txt
    open(unit=2, file="output2.txt")
    write(2,100) (x(I), N(I), N_est(I), N_est2(I), I=1,NUM)
    write(*,200) (I, x(I), N(I), N_est(I), N_est2(I), I=1,NUM)
    close(2)
    100 format(4F12.5)
    200 format(I3, 4F12.5)
    write(*,"(A20)") "output2.txt generated"
    write(*,*) "N0 =", N0
    write(*,*) "L =", L
end program EXPONENTIAL_FIT

!! OUTPUT
! output2.txt generated
!  N0 =   986.149902    
!  L =  0.223548695
