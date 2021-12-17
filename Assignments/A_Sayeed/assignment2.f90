! Sourav Das (1st Semester); ID: 21021085
program prime_generator
    implicit none
    
    integer :: n, incr, nsqrt, i, j, k, div, flag, prime(10000), row, rows, col, istart, iend
    write(*,"(a39)") "Enter the number of primes to generate:"
    do  ! This loop will continue taking inputs until k > 0
        read(*,*) k ! No. of primes
        if ( k <= 0 ) then
            write(*,"(a25)") "Enter a positive integer:"
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
                flag = 0    ! flag set to 0 will avoid n being added to prime
                exit    ! exit inner loop which has div as iterator
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
            ! also work for e.g: 24 numbers in 3 rows of 10 columns
            ! first 2 rows contains 10 numbers each, and the last only 4
            col = 10    ! rows of 10 number each
            rows = int(k/col) + 1
            istart = 1
            do row = 1, rows    ! writing elements in each row
                iend = row * col    ! determining iend for each row
                if ( row == rows ) then
                    iend = k    ! iend = k for last row
                end if
                write(*,"(10i8)") (prime(j), j = istart, iend)
                istart = iend + 1   ! istart is 1 more than iend in next iteration
            end do
            stop
        end if
        n = n + incr    ! Incrementing next no. to be checked by 'incr' value.
        incr = 6 - incr    
        ! Above expression makes incr alternate to 2 or 4, avoiding factors of 2, 3, 6
    end do
end program prime_generator
! OUTPUT
! Enter the number of primes to generate:
! 100
! First      100 prime numbers are:
!        2       3       5       7      11      13      17      19      23      29
!       31      37      41      43      47      53      59      61      67      71
!       73      79      83      89      97     101     103     107     109     113
!      127     131     137     139     149     151     157     163     167     173
!      179     181     191     193     197     199     211     223     227     229
!      233     239     241     251     257     263     269     271     277     281
!      283     293     307     311     313     317     331     337     347     349
!      353     359     367     373     379     383     389     397     401     409
!      419     421     431     433     439     443     449     457     461     463
!      467     479     487     491     499     503     509     521     523     541