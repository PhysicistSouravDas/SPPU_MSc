program name
    implicit none
    real :: f(10), x(10), x_i, s, L
    integer :: n, i, j

    write(*,*) "Enter no. of data points:"
    read(*,*) n

    write(*,*) "Enter x values:"
    read(*,*) (x(i), i=1,n)

    write(*,*) "Enter f(x) values:"
    read(*,*) (f(i), i=1,n)

    write(*,*) "Find f(x) at x = __?"
    read(*,*) x_i

    ! Lagrange's Interpolation
    s = 0.0  ! Initializing sum(s)
    do i = 1, n
        L = 1.0 ! Initializing to 1 for product
        do j = 1, n
            if ( i /= j ) then
                L = L*((x_i -x(j)) / (x(i) - x(j)))
            end if
        end do
        s = s + L*f(i)
    end do
    write(*,100) "At x = ", x_i, " f(x) = ", s
    100 format(a7, f5.3, a7, f7.4)
    open(unit=1, file="interp_out.txt")
    write(1,"(2f7.4)") (x(i), f(i), i=1,n)
    ! write(*,*) x, f, x_i, L, sum
end program name
! OUTPUT
! Enter no. of data points:
! 4
!  Enter x values:
! 1, 2, 3, 4
!  Enter f(x) values:
! 2.6, 5.8, 7.9, 12.0
!  Find f(x) at x = __?
! 2.5
! At x = 2.500 f(x) = 6.7937