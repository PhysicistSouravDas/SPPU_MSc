program loops2
    implicit none
    integer :: i, count

    count = 0

    do i = 1, 2
        count = count + 2*i**2
        write(*,*) count
    end do

end program loops2

! OUTPUT
! 2
! 10