program ifelse_program
    implicit none
    integer :: num_X, num_Y
    
    write(*,*) "Enter num_x, num_y:"
    read(*,*) num_X, num_y 
    

    if (num_x > num_y ) then
        write(*,*) "num_x is greater than num_y"
    elseif ( num_x == num_y ) then
        write(*,*) "num_x is equal to num_y"
    elseif ( num_x <num_y) then
        write(*,*) "num_x is less than num_y"
    end if

end program ifelse_program

