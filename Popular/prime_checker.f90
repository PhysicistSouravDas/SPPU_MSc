program prime_checker
    implicit none
    
    integer :: n, nsqrt, div

    write(*,*) "Enter a positive integer to be checked: "
    read(*,*) n

    if ( n < 2 ) then
        write(*,*) "Enter a number greater than 2."
    else if (n == 2) then
        write(*,100) n, "is a prime number."
    else if (mod(n, 2) == 0) then
        write(*,100) n, "is NOT prime."
    else
        nsqrt = int(sqrt(real(n)) + 1)
        do div = 3, nsqrt, 2
            if ( mod(n, div) == 0 ) then
                write(*,100) n, "is NOT prime."
                exit
                stop
            else if ( div >= nsqrt ) then ! No division succeeded in any iteration.
                write(*,100) n, "is prime."
                exit
                stop
            end if
        end do
    end if
    100 format(i5, 1x, a20)
end program prime_checker