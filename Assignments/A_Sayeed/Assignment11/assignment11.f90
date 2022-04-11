! Sourav Das (MSc 1st Sem)
! Euler method
program euler_method
    implicit none    
    real :: yn, y0, x0, h, f
    integer :: n, i

    write(*,"(a30)") "Enter initial values (x0, y0):"
    read(*,*) x0, y0
    write(*,"(a11)") "Enter n, h:"
    read(*,*) n, h
    
    open(unit=1, file='euler.txt', action='write', position='rewind')
    write(1,*) x0, y0  ! writing initial conditions before the solutions
    write(*,*) x0, y0
    
    do i = 1, n
        yn = y0 + h*f(x0, y0)
        y0 = yn
        x0 = x0 + h
        write(1,*) x0, y0
        write(*,*) x0, y0
    end do
    close(1)

    write(*,*) 'euler.txt generated and is ready for plotting'

end program euler_method

function f(x, y) result(val)
    ! dy/dx = xy
    ! Exact solution: y = exp((x^2)/2)
    implicit none
    real :: x, y
    real :: val
    val = x*y
end function f
