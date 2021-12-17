! Sourav Das (1st Semester); ID: 21021085
program bubble_sort
    implicit none
    
    integer :: i, n
    real :: list(100), temp, swapped

    write(*,"(a24)") "No. of elements to sort:"
    read(*,*) n
    write(*,"(a40)") "Enter unsorted numbers with space/comma:"
    read(*,*) (list(i), i=1,n)

    ! implementing bubble-sorting algorithm (ascending order)    
    ! implementation using while loop/indeterminate do loop
    do        
        swapped = 0
        do i = 1, n-1   ! traversing until 2nd last element, it will be compared with last
            if (list(i) > list(i+1)) then
                ! swapping ith element with (i+1)th
                temp = list(i)
                list(i) = list(i+1)
                list(i+1) = temp
                swapped = 1
                ! if this IF block executes, then swapping done, so swapped = 1
            end if
        end do
        if (swapped == 0) then  ! no more swapping to be done
            write(*,"(a23)") "The sorted numbers are:"
            write(*, 10) (list(i), i=1,n) ! writing column-wise
            stop    ! stopping execution
        end if
    end do
    10 format(100f10.2) ! Maximum 100 numbers can be written with this format
end program bubble_sort
! OUTPUT
! No. of elements to sort:
! 10
! Enter unsorted numbers with space/comma:
! 3.14, 2.72, 1729, -42.0, 6.63, -6.02, 58.47, -0.09, 1.41, -496 
! The sorted numbers are:
!    -496.00    -42.00     -6.02     -0.09      1.41      2.72      3.14      6.63     58.47   1729.00