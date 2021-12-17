program matrix1
    implicit none
    integer :: i, j, n
    real :: a(3,3)

    write(*,*) "Give array size:"
    read(*,*) n
    write(*,*) "Give array element:"
    do i = 1, n
        read(*,*) (a(i, j), j=1,n)
    end do
    
    write(*,*) "The given array is:"
    do i = 1, n
        write(*,*) (a(i, j), j=1,n)
    end do
    
end program matrix1