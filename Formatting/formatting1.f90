program formatting1
    implicit none
    
    integer :: i
    real :: x

    do i = 1, 10
        x = i
        write(*,10) x, x*x    ! writing number and its square
        ! 10 format(f5.1, 2x, f7.2)
    end do
10 format(f5.1, 2x, f7.2)

end program formatting1