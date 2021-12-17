program bubble_sort
    implicit none
    
    integer :: i, n
    real :: list(1000), temp, swapped

    write(*,*) "No. of elements to sort:"
    read(*,*) n
    write(*,*) "Enter unsorted numbers with space/comma:"
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
            ! write(*,"(f7.2)") (list(i), i=1,n) ! writing column-wise
            write(*,*) (list(i), i=1,n) ! writing row-wise
            stop    ! stopping execution
        end if
    end do
end program bubble_sort