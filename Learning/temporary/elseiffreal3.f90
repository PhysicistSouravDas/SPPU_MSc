program ifelse_program
    implicit none
    real:: X, Y, Z
    
    write(*,*) "Enter X,Y,Z:"
    read(*,*) X, Y, Z 
    

    if (X > Y ) then
        write(*,*) "x is greater than y"
    
    elseif ( X < Y) then
        write(*,*) "x is less than y"
    else if ( x > z)   then
        write(*,*)  " x is greater than z"
    else if ( x < Z) then
        write(*,*) " x is less than z"
    else if ( z > y) then
        write(*,*) " z is greater than y"
    else if ( z < y) then
        write(*,*) " z is less than y"
    
    end if

end program ifelse_program

