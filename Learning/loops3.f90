program loops3
    implicit none
    integer :: i

    i = 0

    do
        i = i + 1
        write(*,*) i  ! 100
        
        if ( i == 100 ) then
            exit
        end if
        
    end do

end program loops3