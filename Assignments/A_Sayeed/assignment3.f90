! Sourav Das (1st Sem, MSc); ID: 21021085
program magicsquare
    implicit none
    integer :: n, start, end, vacant, i, j, k, oldi, oldj, sum
    integer, dimension(15, 15) :: magic
    write(*,"(a32)") "Enter the order of Magic Square:"
    do  ! This loop will sanitize user input
        read(*,*) n
        if (mod(n, 2) == 0) then
            write(*,"(a29)") "Enter any value which is odd:"
        else
            exit    ! exit if user input is valid
        end if        
    end do
    write(*,"(a25)") "Enter the starting value:"
    read(*,*) start
    write(*, 100) "Order of Magic Square is", n
    write(*, 200) "The first number to be filled in is", start
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
        if ( magic(i, j) /= vacant ) then   ! checking if element is already filled at this i,j
            i = oldi + 1    ! then the number to be added below the occupied number
            j = oldj
        end if
    end do
    do i=1, n   ! calculating sum of magic square nos. in 1st row
        sum = sum + magic(1, i)
    end do
    write(*, 300) "The sum of numbers is each row/column/diagonal is:", sum
    ! writing the magic square
    write(*, "(a20)") "The magic square is:"
    do i = 1, n
        write(*, 400) (magic(i, j), j = 1, n)
    end do
    ! Format descriptors
    100 format(a24, 1x, i2)
    200 format(a35, 1x, i4)
    300 format(a50, 1x, i5)
    400 format(15i4)  ! Max. 15 numbers can be in same row if 15 order matrix
end program MAGICSQUARE
!! OUTPUT1 (n=3) !!
! Enter the order of Magic Square:
! 2
! Enter any value which is odd:
! 3
! Enter the starting value:
! -3
! Order of Magic Square is  3
! The first number to be filled in is   -3
! The sum of numbers is each row/column/diagonal is:     3
! The magic square is:
!    4  -3   2
!   -1   1   3
!    0   5  -2
!! OUTPUT2 (n=5) !!
! Enter the order of Magic Square:
! 5
! Enter the starting value:
! 1
! Order of Magic Square is  5
! The first number to be filled in is    1
! The sum of numbers is each row/column/diagonal is:    65
! The magic square is:
!   17  24   1   8  15
!   23   5   7  14  16
!    4   6  13  20  22
!   10  12  19  21   3
!   11  18  25   2   9
!! OUTPUT3 (n=7) !!
! Enter the order of Magic Square:
! 7
! Enter the starting value:
! 5
! Order of Magic Square is  7
! The first number to be filled in is    5
! The sum of numbers is each row/column/diagonal is:   203
! The magic square is:
!   34  43  52   5  14  23  32
!   42  51  11  13  22  31  33
!   50  10  12  21  30  39  41
!    9  18  20  29  38  40  49
!   17  19  28  37  46  48   8
!   25  27  36  45  47   7  16
!   26  35  44  53   6  15  24