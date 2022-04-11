! Sourav Das (MSc 1st Semester)
! Lagrange Interpolation

program lagrange_interpolation
    implicit none
    real :: f(0:10), x(0:10), x_i, s, L
    integer :: n, i, j

    write(*,"(a25)") "Enter no. of data points:"
    read(*,*) n
    
    ! because we are starting from 0 index of arrays, we need to decrease n by 1
    ! so that we can iterate from n = 0 to n = 3 for four values
    n = n - 1

    write(*,"(a15)") "Enter x values:"
    read(*,*) (x(i), i = 0, n)

    write(*,"(a18)") "Enter f(x) values:"
    read(*,*) (f(i), i = 0, n)

    write(*,"(a20)") "Find f(x) at x = __?"
    read(*,*) x_i

    ! Lagrange's Interpolation
    s = 0.0  ! Initializing sum(s)

    do i = 0, n
        L = 1.0 ! Initializing to 1 for product
        do j = 0, n
            if ( i /= j ) then
                L = L*((x_i -x(j)) / (x(i) - x(j)))
            end if
        end do
        s = s + L*f(i)
    end do

    write(*,100) "At x = ", x_i, " f(x) = ", s
    100 format(a7, f5.3, a7, f7.4)

end program lagrange_interpolation

! OUTPUT
! Enter no. of data points:
! 4
! Enter x values:
! 1, 2, 3, 4
! Enter f(x) values:
! 2.6, 5.8, 7.9, 12.0
! Find f(x) at x = __?
! 2.5
! At x = 2.500 f(x) = 6.7937
