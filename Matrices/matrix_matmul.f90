program matrix_matmul
    implicit none
    ! program for addition of two matrices
    integer :: p, q, m, n, i, j
    real, dimension(9, 9) :: A, B, C

    write(*,*) "Enter the number of rows and column of A matrix:"
    read(*,*) p, q
    write(*, *) "Enter matrix elements of A separated by space or comma:"
    ! reading A matrix elements (p x q)
    do i = 1, p
        read(*,*) (A(i, j), j=1,q)
    end do
    write(*,*) "Matrix A is:"
    do i = 1, p
        write(*,*) (A(i, j), j=1,q)
    end do

    write(*,*) "Enter the number of rows and column of B matrix:"
    read(*,*) m, n
    ! checking whether both matrices can be multiplied or not
    if ( q .ne. m ) then
        write(*,*) "These two matrices can't be multiplied."
        stop
    end if

    write(*, *) "Enter matrix elements of B separated by space or comma:"
    ! reading B matrix elements (m x n)
    do i = 1, m
        read(*,*) (B(i, j), j=1,n)
    end do
    write(*,*) "Matrix B is:"
    do i = 1, m
        write(*,*) (B(i, j), j=1,n)
    end do

    ! multiplying A and B
    C = matmul(A,B)
    ! writing C (p x n)
    write(*,*) "Matrix C = AB is:"
    do i = 1, p
        write(*,*) (C(i, j), j=1,n)
    end do

end program matrix_matmul