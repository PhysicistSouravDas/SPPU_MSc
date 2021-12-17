program ifelse2

    implicit none
    real :: x, y

    write(*,*) "Enter x and y: "
    read(*,*) x, y
    write(*,*) "You entered ", x, " and " ,y 

    if ( x < y ) then
        write(*,*) "x is less than y"
    else    ! if (x >= y)
        write(*,*) "x is greater than or equal to y"
    end if


end program ifelse2