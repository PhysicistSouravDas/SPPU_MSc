! Sourav Das (MSc 1st Sem)
! 28th Dec 2021
program upper_triangular_decomposition
    implicit none
    integer, parameter :: MAX_SIZE = 100
    real :: tol, A(MAX_SIZE, MAX_SIZE), X(MAX_SIZE), summ! , C(MAX_SIZE)
    integer :: i, j, n

    tol = 1e-3

    write(*,"(a38)") "Order of matrix/System of linear eqns:"
    read(*,*) n

    open(unit=1, file='input5.txt' ,action='read')
    do i = 1, n
        read(1,*) (A(i, j), j = 1, n+1)
    end do
    close(1)

    do j = 1, n-1
        if ( abs( A(j,j) ) < tol ) then
            write(*,"(a41)") "Diagonal element close to 0 (Stopping)..."
            stop
        end if
        do i = j+1, n
            A(i,:) = A(i,:) - (A(i,j)/A(j,j)) * A(j,:)
        end do
    end do

    ! writing upper triangular matrix
    write(*,*) "Upper triangular matrix:"
    do i = 1, n
        write(*,*) (A(i, j), j = 1, n+1)
    end do
    
    ! X = 0.0
    ! starting back-substitution
    do j = n, 1, -1
        summ = 0.0
        do i = j+1, n
            summ = summ + A(j,i)*X(i)
        end do
        X(j) = (A(j,n+1) - summ) / (A(j,j))
    end do
    write(*,*) "Solutions:"
    ! writing X array (solution)
    do i = 1, n
        write(*,*) X(i)
    end do

end program upper_triangular_decomposition





    ! write(*,*) "Enter coefficient matrix elements row-wise:"
    ! do i = 1, n
    !     read(*,*) (A(i, j), j = 1, n)
    ! end do
    ! write(*,*) "Enter elements of constant matrix:"
    ! read(*,*) (C(i), i = 1, n)
    ! Making A matrix augmented matrix