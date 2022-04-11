program a_prog
    implicit none
    real :: f, g, x0, tol
    integer :: flag

    x0 = 1
    tol = 1e-10

    call nr_method(f, g, x0, tol, flag)
    if ( flag == 0 ) then
        write(*,*) "No roots found!"
        stop
    end if
    ! otherwise flag == 1
    write(*,*) "Root is: ", x0
    stop
end program a_prog

function f(x) result(val)
    implicit none
    real :: x
    real :: val
    val = sin(x) + exp(x)
end function f

function f(x)
    implicit none
    real :: x, f, val
    f = x**2
    return
end function f


function g(x) result(val)
    implicit none
    real :: x
    real :: val
    val = x**2
end function g