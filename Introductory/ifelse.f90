program ifelse

    implicit none
    real :: x

    write(*,*) "Enter x: "
    read(*,*) x
    write(*,*) "You entered ", x

    if ( x < 10.0 ) then
        write(*,*) "x is less than 10"
    else    ! if (x >= 10)
        write(*,*) "x is greater than or equal to 10"
    end if


end program ifelse