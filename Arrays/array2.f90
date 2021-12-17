program array2
    implicit none

    integer :: n, i
    real :: x(10)

    write(*,*) "How many elements you want to input? (<10)"
    read(*,*) n
    write(*,*) "Enter ", n, "elements"
    
    do i = 1, n
        read(*,*) x(i)    ! ( x(i), i=1,4 ) ! implicit do loop
    end do

    write(*,*) "Printing the values..."
    
    do i = 1, n
        write(*,*) x(i)    ! ( x(i), i=1,4 )
    end do
    
end program array2