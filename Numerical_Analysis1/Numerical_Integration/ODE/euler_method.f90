! Sourav Das (MSc 1st Sem)
program euler_method
    implicit none
    
    real :: yn, y0, x, x0, h, f
    integer :: n, i
    ! logical :: exist

    write(*,*) "Enter initial values (x0, y0):"
    read(*,*) x0, y0
    write(*,*) "Value of x at which y to be found:"
    read(*,*) x
    write(*,*) "Number of intervals to reach x from x0:"
    read(*,*) n
    
    h = (x - x0) / n

    ! inquire(file='euler.txt', exist=exist)
    ! if (exist) then
    !     ! empty the file and close it
    !     open(unit=1, file='euler.txt', status='replace')
    !     close(1)
    ! end if
    
    open(unit=1, file='euler.txt', action='write', position='rewind')
    write(1,*) x0, y0
    
    do i = 1, n
        yn = y0 + h*f(x0, y0)
        y0 = yn
        x0 = x0 + h
        write(1,*) x0, y0
    end do
    close(1)

    write(*,*) 'euler.txt generated and is ready for plotting'
    write(*,100) 'Value of y at x =', x, 'is', yn
    100 format(a17, 1x, f5.2, 1x, a2, 1x, f5.2)

end program euler_method



function f(x, y) result(val)
    implicit none
    real :: x, y
    real :: val
    val = x*y
end function f