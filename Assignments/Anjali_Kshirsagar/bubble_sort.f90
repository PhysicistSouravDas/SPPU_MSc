! Sourav Das (MSc 1st semester)
! Program to sort numbers in descending order using bubble sort algorithm
program BUBBLE_SORT
    implicit none
    
    integer, parameter :: MAX_SIZE = 1000
    integer :: I, N, TEMP
    integer, dimension(MAX_SIZE) :: LIST
    logical :: SWITCH

    write(*,'(a24)') "No. of elements to sort:"
    read(*,*) N
    write(*,'(a40)') "Enter unsorted numbers with space/comma:"
    read(*,*) (LIST(I), I = 1, N)
    write(*,'(a17, 1x, 14i5)') "Entered numbers: ", (LIST(I), I = 1, N)

    ! implementing bubble-sorting algorithm (descending order)    
    ! implementation using while loop/indeterminate do loop
    do        
        SWITCH = .false.
        do I = 1, N-1   ! traversing until 2nd last element, it will be compared with last
            if (LIST(I) < LIST(I+1)) then
                ! swapping ith element with (i+1)th
                TEMP = LIST(I)
                LIST(I) = LIST(I+1)
                LIST(I+1) = TEMP
                SWITCH = .true.
                ! if this IF block executes, then swapping done, so switch = True
            end if
        end do
        if (SWITCH .eqv. .false.) then
            write(*,'(a36)') "The sorted numbers are (descending): "
            write(*,100) (list(I), I = 1, N) ! writing row-wise
            stop    ! stopping execution
        end if
    end do
    100 format(14i5)
end program BUBBLE_SORT

! INPUT/OUTPUT
! No. of elements to sort:
! 14
! Enter unsorted numbers with space/comma:
! -1, 5, 9, 200, -10, 197, 35, 0, -1, 9, 250, -15, 50, 14
! Entered numbers:     -1    5    9  200  -10  197   35    0   -1    9  250  -15   50   14
! The sorted numbers are (descending):
!   250  200  197   50   35   14    9    9    5    0   -1   -1  -10  -15