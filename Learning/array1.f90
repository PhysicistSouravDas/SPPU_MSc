program array
    implicit none
    real :: x(1000, 1000), y(1000, 1000), c(1000,1000)
    integer :: row, col, i, j

    write(*,*) "How many rows, columns:"
    read(*,*) row, col

    write(*,*) "Enter matrix X:"
    do i = 1, row
        read(*,*) (x(i,j), j=1,col)
    end do

    write(*,*) "Enter matrix Y:"
    do i = 1, row
        read(*,*) (y(i,j), j=1,col)
    end do

    ! matrix addition
    do i = 1, row
        do j = 1, col
            c(i,j) = x(i,j) + y(i,j)
        end do
    end do












    do i = 1, row
        write(*,*) (c(i,j), j=1,col)
    end do



    ! explicit do loop
    ! do i = 1, n
    !     write(*,*) x(i)
    ! end do

    ! implicit do loop
    ! write(*,*) (x(i), i = 1,n)

end program array


! 10 20 30
! 40 50 60
! 70 80 90

! x11 x12
! x21 x22

!   X          Y           C=X+Y
! 10  20    1    2         11  22
! 30  40    3    4         33  44
! x(i,j)




! c(1,2) = x(1,2) + y(1,2)