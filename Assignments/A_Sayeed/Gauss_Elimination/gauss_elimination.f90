! Sourav Das (MSc 1st Sem)
! 6th Jan 2021
program gauss_elimination
    implicit none
    integer, parameter :: MAX_SIZE = 100
    real :: tol, A(MAX_SIZE, MAX_SIZE), X(MAX_SIZE), summ
    integer :: i, j, n

    write(*,"(a59)") "Enter order of matrix/System of linear eqns, and tolerance:"
    read(*,*) n, tol

    open(unit=1, file='input.txt' ,action='read')
    write(*,"(a51)") "Reading following augmented matrix from 'input.txt'"
    do i = 1, n
        read(1,*) (A(i, j), j = 1, n+1)
        write(*,*) (A(i, j), j = 1, n+1)    ! writing to terminal just after reading
    end do
    close(1)

    ! starting upper triangularization
    do j = 1, n-1
        if ( abs( A(j,j) ) < tol ) then
            write(*,"(a41)") "Diagonal element close to 0 (Stopping)..."
            stop
        end if
        do i = j+1, n
            ! A(i,:) means all elements of i-th row. A(:,j) means all elements of j-th column
            A(i,:) = A(i,:) - (A(i,j)/A(j,j)) * A(j,:)
        end do
    end do

    ! writing upper triangular matrix
    write(*,"(a24)") "Upper triangular matrix:"
    do i = 1, n
        write(*,*) (A(i, j), j = 1, n+1)
    end do
    
    ! starting back-substitution
    do j = n, 1, -1
        summ = 0.0
        do i = j+1, n
            summ = summ + A(j,i)*X(i)
        end do
        X(j) = (A(j,n+1) - summ) / (A(j,j))
    end do
    write(*,"(a10)") "Solutions:"
    ! writing X array (solution)
    do i = 1, n
        write(*,*) X(i)
    end do

end program gauss_elimination

!! INPUT/OUTPUT
! Enter order of matrix/System of linear eqns, and tolerance:
! 3, 0.0001
! Reading following augmented matrix from 'input.txt'
!    3.00000000       1.00000000       1.00000000       8.00000000    
!    2.00000000       5.00000000       1.00000000       15.0000000    
!    1.00000000      -1.00000000       4.00000000       11.0000000    
! Upper triangular matrix:
!    3.00000000       1.00000000       1.00000000       8.00000000    
!    0.00000000       4.33333349      0.333333313       9.66666603    
!    0.00000000       0.00000000       3.76923084       11.3076916    
! Solutions:
!    1.00000012    
!    1.99999976    
!    2.99999976    
