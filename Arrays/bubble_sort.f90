program bubble_sort
    implicit none
    
    integer :: i, j, n
    real :: list(10), temp

    write(*,*) "No. of elements to sort:"
    read(*,*) n
    write(*,*) "Enter unsorted numbers with space/comma:"
    read(*,*) (list(i), i=1,n)
    ! write(*,*) (list(j), j=1,n)

    ! implementing bubble-sorting algorithm (ascending order)
    do j = 1, n
        do i = 1, n
            if (list(i) > list(i+1)) then
                ! swapping ith element with (i+1)th
                temp = list(i)
                list(i) = list(i+1)
                list(i+1) = temp
            end if
        end do    
    end do
    
    write(*,*) (list(j), j=1,n)
end program bubble_sort