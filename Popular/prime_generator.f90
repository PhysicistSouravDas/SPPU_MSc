program prime_generator
    implicit none
    
    integer :: n, incr, nsqrt, i, j, k, div, flag, prime(10000000), row, col, istart, iend, rows
    write(*,*) "Enter the number of primes to generate:"
    do  ! This loop will continue taking inputs until n > 0
        read(*,*) k
        if ( k <= 0 ) then
            write(*,*) "Enter a positive integer:"
        else
            exit
        end if
    end do
    ! predefining few first primes
    prime(1) = 2
    prime(2) = 3
    n = 5   ! n is the number to check, whether it is prime or not
    incr = 2  ! initializing incr variable to increase n after each iteration    
    i = 3   ! index variable to add prime no. n at index i in array prime
    do
        flag = 1    ! flag will determine whether to add n to prime
        nsqrt = int(sqrt(real(n)) + 1)
        do div = 3, nsqrt, 2    ! checking all factors from 3 to sqrt(n)
            if ( mod(n, div) == 0 ) then
                ! do nothing
                flag = 0    ! flag set to 0 will avoid n being added to prime
                exit    ! exit that loop which has div as iterator
            end if
        end do
        ! If no division succeeds for n, then flag must not be 0
        if (flag == 1) then 
            prime(i) = n    ! add n to prime array at i-th index
            i = i + 1       ! increase i to add new n at (i+1)th index
        end if
        ! since i = i+1 above, then i exceeds k after adding k numbers to prime
        if (i > k) then
            write(*,'(a5, 1x, i8, 1x, 18a)') "First", k, "prime numbers are:"
            ! printing 1-D array in rows and columns
            col = 7
            rows = int(k/col) + 1
            istart = 1
            do row = 1, rows
                iend = row * col
                if ( row == rows ) then
                    iend = k
                end if
                write(*,*) (prime(j), j = istart, iend)
                istart = iend + 1
            end do
            stop
        end if
        n = n + incr    ! Incrementing next no. to be checked by 'incr' value.
        incr = 6 - incr    ! This expression makes incr alternate to 2 or 4, avoiding factors of 2, 3, 6
    end do

end program prime_generator