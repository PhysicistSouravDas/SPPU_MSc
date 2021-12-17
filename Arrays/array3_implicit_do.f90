program array3
    implicit none

    integer :: n, i
    real :: x(10)

    write(*,*) "How many elements you want to input? (<10)"
    read(*,*) n
    write(*,*) "Enter ", n, "elements"
    
        read(*,*) (x(i), i = 1, n) ! implicit do loop

    write(*,*) "Printing the values..."
    
    do i = 1, n
        write(*,*) x(i)    ! explicit do loop
    end do
    
end program array3
