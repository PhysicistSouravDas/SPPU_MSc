program ifelse_program
    implicit none
    integer :: num_X
    
    write(*,*) "Enter num_x:"
    read(*,*) num_X 
    

    if (num_x > 10 ) then
        write(*,*) "num_x is greater than 10"
    elseif ( num_x == 10 ) then
        write(*,*) "num_x is equal to 10"
    elseif ( num_x <10) then
        write(*,*) "num_x is less than 10"
    end if

end program ifelse_program

