program extendedif
    implicit none
    integer :: n

    write(*,*) "Enter an integer (n): "
    read(*,*) n
    write(*,*) "You entered n =", n

    if ( mod(n,7) == 0 ) then
        write(*,*) "n is divisible by 7"
    else if ( n < 100 ) then
        write(*,*) "n is less than 100"
    else if ( n > 300 ) then
        write(*,*) "n is greater than 300"
    else
        write(*,*) "None of the above conditions were met"        
    end if
    
end program extendedif