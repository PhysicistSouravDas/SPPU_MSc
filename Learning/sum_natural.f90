! program to find sum of natural numbers

program sum_natural
    implicit none
    integer :: n, i, count

    write(*,*) "Enter value of n:"
    read(*,*) n
    count = 0  ! initialization
    do i = 1, n   ! i = 5
        count = count + i  ! count = 15
    end do
    write(*,*) count
end program sum_natural


! 1, 2, ..., n

! n = 3

! 1 + 2 + 3 = 6
! output: 6

! n = 5

! 1 + 2 + 3 + 4 + 5 = 15