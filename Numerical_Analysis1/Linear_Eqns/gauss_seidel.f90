! Sourav Das (MSc 1st Sem)
! 4th Jan 2022
! This program is NOT COMPLETE TILL NOW
program gauss_seidel
    implicit none
    integer, parameter :: MAX_SIZE = 100
    real :: tol, A(MAX_SIZE, MAX_SIZE), X(MAX_SIZE), X_OLD(MAX_SIZE), summ! , C(MAX_SIZE)
    integer :: i, j, n, MAX_ITER, k

    tol = 1e-3

    write(*,"(a38)") "Order of matrix/System of linear eqns:"
    read(*,*) n

    open(unit=1, file='input5.txt' ,action='read')
    do i = 1, n
        read(1,*) (A(i, j), j = 1, n+1)
    end do
    close(1)

    MAX_ITER = 50
    X = 0.0
    ! do k = 1, MAX_ITER
    do while ( abs(X_OLD(i)-X(i)) > tol )
        X_OLD = X
        do i = 1, n
            summ = 0.0
            do j = 1, n
                if ( j /= i ) then
                    summ = summ + A(i,j)*X(j)
                end if
            end do
            X(i) = (A(i,n+1) - summ) / A(i,i)
            ! write(*,*) X(i)
            ! if ( (X_OLD(i)-X(i)) / X(i) < tol ) then
            !     write(*,*) "Converged"
            !     exit
            ! end if
        end do        
    end do

    write(*,*) "Solutions:"
    ! writing X array (solution)
    do i = 1, n
        write(*,*) X(i)
    end do

end program gauss_seidel





    ! write(*,*) "Enter coefficient matrix elements row-wise:"
    ! do i = 1, n
    !     read(*,*) (A(i, j), j = 1, n)
    ! end do
    ! write(*,*) "Enter elements of constant matrix:"
    ! read(*,*) (C(i), i = 1, n)
    ! Making A matrix augmented matrix