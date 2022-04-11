! Sourav Das (MSc 1st Sem)
! 6th Jan 2021
program gauss_seidel
    implicit none
    integer, parameter :: MAX_SIZE = 100
    real, dimension(MAX_SIZE) :: X, X_OLD, diff
    real :: A(MAX_SIZE, MAX_SIZE), tol, summ, epsilon
    integer :: i, j, n

    write(*,"(a55)") "Enter order of matrix/System of linear eqns, tolerance:"
    read(*,*) n, tol

    open(unit=1, file='input.txt' ,action='read')
    write(*,"(a51)") "Reading following augmented matrix from 'input.txt'"
    do i = 1, n
        read(1,*) (A(i, j), j = 1, n+1)
        write(*,*) (A(i, j), j = 1, n+1)    ! writing the array to terminal after reading
    end do
    close(1)

    X = 0.0
    do
        X_OLD = X
        do i = 1, n
            summ = 0.0
            do j = 1, n
                if ( j /= i ) then
                    summ = summ + A(i,j)*X(j)
                end if
            end do
            X(i) = (A(i,n+1) - summ) / A(i,i)
        end do
        diff = X - X_OLD  ! subtracting each corresponding element, and storing to diff array
        epsilon = sqrt( dot_product(diff, diff) / dot_product(x, x) )  ! we 'll compare tol with epsilon to exit loop
        if ( epsilon < tol ) exit  ! epsilon is always +ve, so no need to use abs()
    end do

    write(*,"(a10)") "Solutions:"
    ! writing X array (solution)
    do i = 1, n
        write(*,*) X(i)
    end do

end program gauss_seidel

!! INPUT/OUTPUT
! Enter order of matrix/System of linear eqns, tolerance:
! 3, 0.00001
! Reading following augmented matrix from 'input.txt'
!    3.00000000       1.00000000       1.00000000       8.00000000    
!    2.00000000       5.00000000       1.00000000       15.0000000    
!    1.00000000      -1.00000000       4.00000000       11.0000000    
! Solutions:
!    1.00000000    
!    1.99999976    
!    3.00000000   
