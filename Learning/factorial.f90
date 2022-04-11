! program to find facrorial of natural number n

program factorial
    implicit none
    integer :: n, i, prod

    write(*,*) "Enter n"
    read(*,*) n

    prod = 1  ! initialization

    do i = 1, n         ! i = 5
        prod = prod * i ! prod = 24*5 = 120
    end do
    
    write(*,*) prod

end program factorial