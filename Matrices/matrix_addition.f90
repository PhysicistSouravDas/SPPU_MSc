program matrix_addition
    implicit none
    ! program for addition of two matrices
    integer :: m, n, i, j
    real, dimension(10, 10) :: A, B, C

    write(*,*) "Enter the number of rows and column of matrix:"
    read(*,*) m, n
    write(*, *) "Enter matrix elements of A separated by space or comma:"
    ! reading A matrix elements
    do i = 1, m
        read(*,*) (A(i, j), j=1,n)
    end do
    write(*,*) "Matrix A is:"
    do i = 1, m
        write(*,*) (A(i, j), j=1,n)
    end do

    write(*, *) "Enter matrix elements of B separated by space or comma:"
    ! reading B matrix elements
    do i = 1, m
        read(*,*) (B(i, j), j=1,n)
    end do
    write(*,*) "Matrix B is:"
    do i = 1, m
        write(*,*) (B(i, j), j=1,n)
    end do

    ! summing A and B
    do i = 1, m
        do j = 1, n
            C(i, j) = A(i, j) + B(i, j)
        end do
    end do
    ! writing C
    write(*,*) "Matrix C = A+B is:"
    do i = 1, m
        write(*,*) (C(i, j), j=1,n)
    end do

end program matrix_addition