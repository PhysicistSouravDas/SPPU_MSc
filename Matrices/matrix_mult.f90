program matrix_multiplication
    implicit none
    ! program for addition of two matrices
    integer :: p, q, m, n, i, j, k
    real, dimension(10, 10) :: A, B, C

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
    do i = 1, p  ! iterating through rows of A
        do j = 1, n  ! iterating through columns of B
            C(i, j) = 0    ! initialization of (i, j) element of C
            do k = 1, m  ! either m or q can be used to sum over k
                C(i, j) = C(i, j) + (A(i, k) * B(k, j))
            end do            
        end do
    end do
    ! writing C (p x n)
    write(*,*) "Matrix C = AB is:"
    do i = 1, p
        write(*,*) (C(i, j), j=1,n)
    end do

end program matrix_multiplication