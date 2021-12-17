program do_loop
    implicit none

    integer :: i

    do i = 1, 10
        write(*,*) i
        write(*,*) "Looping"
    end do
    
end program do_loop