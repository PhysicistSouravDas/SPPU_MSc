program array4  ! program to determine biggest number
    implicit none

    integer :: n, i
    real :: x(10), big

    write(*,*) "How many elements you want to compare? (<10)"
    read(*,*) n
    write(*,*) "Enter", n, "elements"
    
    read(*,*) (x(i), i = 1, n)

    big = x(1)
    do i = 2, n
        if ( x(i) > big ) then
            big = x(i)
        end if
    end do

    write(*,*) "The biggest value is", big    
    
end program array4