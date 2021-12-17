! Author: Sourav Das 
! (1st Sem, MSc)


program magicsquare
    implicit none
    integer :: n, start, end, vacant, i, j, k, oldi, oldj
    integer, dimension(15, 15) :: magic
    write(*,*) "Give the order of Magic Square: "
    do  ! This loop will sanitize user input
        read(*,*) n
        if (mod(n, 2) == 0) then
            write(*,*) "Give any value which is odd: "
        else
            exit
        end if        
    end do
    write(*,*) "Give the starting value: "
    read(*,*) start
    write(*,*) "Order of Magic Square is", n
    write(*,*) "The first number to be filled in is", start
    vacant = start - 1  ! variable to do test whether element in array was changed or not
    end = start + n*n - 1   ! pre-calculating the ending value of magic square
    magic = vacant  ! All the values of MAGIC array assigned to VACANT

    i = 1       ! starting from 1st row
    j = (N+1)/2 ! starting from middle-column
    ! iterating from start value to end value to be filled in magic array
    do k = start, end
        magic(i, j) = k
        oldi = i    ! remembering old values
        oldj = j
        i = i - 1
        j = j + 1
        if (i == 0) i = n   ! periodic boundary condition tests
        if (j > n) j = 1
        if ( magic(i, j) /= vacant ) then   ! checking if element is not already filled at this i,j
            i = oldi + 1    ! assigning the new indices using old remembered values
            j = oldj
        end if
    end do
    ! writing the magic square
    do i = 1, n
        write(*, 100) (magic(i, j), j = 1, n)
    end do

    100 format(15I4)
    ! INPUT/OUTPUT
    ! Give the order of Magic Square:
    ! 3
    !  Give the starting value:
    ! 5
    !  Order of Magic Square is           3
    !  The first number to be filled in is           5
    !   12   5  10
    !    7   9  11
    !    8  13   6    
end program MAGICSQUARE