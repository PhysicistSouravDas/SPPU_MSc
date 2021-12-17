program infinite_loop
    implicit none
    
    integer :: x

    x = 1
    do
        x = x + 1
        write(*,*) x
        ! if ( x == 10 ) then
        !     stop
        ! end if
    end do
end program infinite_loop