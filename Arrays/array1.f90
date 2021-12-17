program array1
    implicit none

    ! integer :: i, j, k
    ! real :: a, b, c
    ! declaring array having real datatype
    real :: x(10)

    ! a = 10.1
    write(*,*) "Supply the values of x1 and x2"
    read(*,*) x(1), x(2)

    write(*,*) "x1 and x2 =", x(1), x(2)
    write(*,*) "Printing x"
    write(*,*) x
    
end program array1