program find_roots
    ! using nr_method subroutine to calc root
    ! this program can be modified to use bisection method
    ! if root can't be found using nr_method
    ! external nr_method  ! you can uncomment this line as well, still it will work. when program and 
                          ! subroutine are in same file, this is not required, else, required.
    real,external :: f, g ! because these func will be arguments of another func/subroutine
    real :: root
    integer :: success
    tol = 1e-6
    
    write(*,*) "Enter guess value:"
    read(*,*) root

    call nr_method(f, g, root, tol, success)

    if ( success == 0 ) then
        write(*,*) "No roots can be found by this method."
    else
        write(*,*) "Estimated root of f(x) is", root
    end if
end program find_roots

! -----------------------------------------------------------

subroutine nr_method(f, g, x0, tol, flag)
    ! ==========================================
    ! Calculates the approximate root of function f
    ! Method: Newton-Raphson
    ! Sourav Das, December 2021
    ! ------------------------------------------
    ! input ...
    ! f   - function
    ! g   - derivative of f
    ! x0  - initial guess
    ! tol - desired uncertainty of the root as |f(x0)| < tol
    ! output ...
    ! x0  - root of f(x) = 0
    ! flag  - indicator of success
    !         1 - root found
    !         0 - root cannot be found using this method
    ! ==========================================
    implicit none
    real :: f, g
    real,intent(in) :: tol
    real,intent(inout) :: x0 ! because it is taken as input, as well as will be in output
    integer,intent(out) :: flag
    real :: lim, xi

    lim = 1e-6  ! for checking if f'(x) ~ 0
    flag = 1

    do
        if ( abs(f(x0)) < tol ) exit
        if ( abs(g(x0)) < lim ) then
            flag = 0
            exit
        end if
        xi = x0 - ( f(x0)/g(x0) )
        x0 = xi
    end do

end subroutine nr_method

! -----------------------------------------------------------

! one way to define function
function f(x) result(val)
    implicit none
    real :: x
    real :: val
    val = sin(x) + exp(x)
end function f

! another way
function g(x)
    implicit none
    real :: x, g
    g = cos(x) + exp(x)
    return
end function g
