! Sourav Das (MSc 1st semester)
program particle_in_a_box
    implicit none
    real,external :: f, wave_func, wave_func_sq
    integer,parameter :: infty = 500  ! We 'll use this value to choose x from -50 to 50
    real :: a, V0, lambda, root, energy, q, alpha, uI(1000), uII(1000), uIII(1000), x(-infty:infty)
    real :: low_guess, hi_guess, tol
    real :: x0, xn, area_uI, area_uII, area_uIII, tot_area, norm_const
    integer :: i, j
    logical :: success
    
    write(*,"(a45)") "Enter width of potential well, 2a (angstrom):"
    read(*,*) a
    a = a/2.0   ! since 2a is entered, we are storing value of a
    write(*,"(a32)") "Enter finite potential, V0 (eV):"
    read(*,*) V0
    
    ! converting the units from angstrom, eV to atomic units bohr, Hartree respectively
    ! 1 bohr = 0.529 angstrom  =>  x angstrom = (x/0.529) bohr
    ! 1 Hartree = 27.2 eV      =>  x eV = (x/27.2) Hartree
    a = (a / 0.529)  ! bohr unit
    V0 = (V0 / 27.2) ! Hartree

    lambda = 2*V0*a**2

    ! reading values from user to find root
    write(*,"(a57)") "Enter lower guess, higher guess, tolerance, to find root:"
    read(*,*) low_guess, hi_guess, tol  ! 1.0, 1.5, 1e-3 (0.001) may be a good value
    
    call bisection(f, low_guess, hi_guess, tol, success, root, lambda)

    if ( success ) then
        write(*,"(a11, 1x, f7.3)") "Root Found:", root
        ! Proceeding further, to find energy and wave function
        call find_energy(V0, a, root, energy)
        energy = abs(energy)  ! overwriting energy by abs(energy)
        write(*,"(a20, 1x, f7.3)") "Energy (in Hartree):", energy

        ! q = sqrt(2mV0 - |E|)/ℏ = sqrt(2V0 - |E|)
        ! alpha = sqrt(2m|E|)/ℏ = sqrt(2|E|)
        q = sqrt(2*V0 - energy)
        alpha = sqrt(2*energy)
        ! let infinity = infty = 50.0
        ! creating a grid for x using array constructor        
        x = [( 0.1*i, i = -infty, infty )]  ! x = (-50.0, ..., 50.0), elements separated 0.1 apart        
        
        open(unit=1, file='non_normalized_wave.txt')
        ! initializing indices
        i = 1
        j = -infty
        ! uI exist from -infty to -a; uII --> -a to a; uIII --> a to infty
        do
            if ( x(j) < -a ) then
                uI(i) = wave_func(x(j), q, alpha, a, 1)
                write(1,*) x(j), uI(i)
            elseif ( x(j) >= -a .and. x(j) <= a ) then
                uII(i) = wave_func(x(j), q, 0.0, 0.0, 2)    ! alpha, a = 0
                write(1,*) x(j), uII(i)
            elseif ( x(j) > a ) then
                uIII(i) = wave_func(x(j), q, alpha, a, 3)
                write(1,*) x(j), uIII(i)
            end if
            i = i + 1
            j = j + 1
            if ( j > infty ) exit
        end do
        ! generated data points
        close(1)
        write(*,"(a48)") "'non_normalized_wave.txt' generated for plotting"
        
        ! Normalization
        ! For uI
        x0 = x(-infty)
        xn = -a
        call simpson(wave_func_sq, x0, xn, 100, q, alpha, a, 1, area_uI)
        
        ! For uII
        x0 = -a
        xn = a
        ! a = 0, and alpha = 0
        call simpson(wave_func_sq, x0, xn, 100, q, 0.0, 0.0, 2, area_uII)
        
        ! For uIII
        x0 = a
        xn = x(infty)
        call simpson(wave_func_sq, x0, xn, 100, q, alpha, a, 3, area_uIII)
        
        tot_area = area_uI + area_uII + area_uIII
        write(*,100) "Total area under non-normalized wave:", tot_area
        100 format(a37, 1x, f5.2)
        
        ! If tot_area = N
        ! Normalization constant, A = 1/sqrt(N)
        norm_const = 1/(sqrt(tot_area))
        write(*,"(a23, 1x, f7.3)") "Normalization constant:", norm_const
        
        ! multiplying all elements of uI, uII, uIII by norm_const
        uI = uI * norm_const
        uII = uII * norm_const
        uIII = uIII * norm_const
        
        ! generating data points of normalized wave-functions
        open(unit=2, file='normalized_wave.txt')
        ! again initializing indices for writing normalized values
        i = 1
        j = -infty
        do
            if ( x(j) < -a ) then
                write(2,*) x(j), uI(i)
            elseif ( x(j) >= -a .and. x(j) <= a ) then
                write(2,*) x(j), uII(i)
            elseif ( x(j) > a ) then
                write(2,*) x(j), uIII(i)
            end if
            i = i + 1
            j = j + 1
            if ( j > infty ) exit
        end do
        close(2)
        write(*,"(a44)") "'normalized_wave.txt' generated for plotting"
    else
        write(*,"(a52)") "Root not found. Try different bracket for bisection."
    end if

end program particle_in_a_box

function f(y, lambda) result(val)
    ! Function whose root is to be found
    implicit none
    real :: y, val, lambda
    val = (y**2) * (1/cos(y))**2 - lambda
end function f

function wave_func(x, q, alpha, a, region) result(val)
    ! function for wave function
    ! for uII, alpha, a = 0
    ! returns a value based upon a region (1, 2, or 3)
    implicit none
    real :: x, q, alpha, a, term1, term2, term3
    integer :: region
    real :: val
    ! uI   = A*cos(q*a)*exp(alpha*a)*exp(alpha*x)
    ! uII  = A*cos(q*x)
    ! uIII = A*cos(q*a)*exp(alpha*a)*exp(-alpha*x)
    term1 = cos(q*a)*exp(alpha*a)*exp(alpha*x)
    term2 = cos(q*x)
    term3 = cos(q*a)*exp(alpha*a)*exp(-alpha*x)
    if ( region == 1 ) then
        val = term1
    elseif ( region == 2 ) then
        val = term2
    elseif ( region == 3 ) then
        val = term3
    end if
end function wave_func

function wave_func_sq(x, q, alpha, a, region) result(final_val)
    ! square of wave function for integration
    ! same to wave_func, but just squaring the final returning value
    implicit none
    real :: x, q, alpha, a, term1, term2, term3
    integer :: region
    real :: val, final_val
    term1 = cos(q*a)*exp(alpha*a)*exp(alpha*x)
    term2 = cos(q*x)
    term3 = cos(q*a)*exp(alpha*a)*exp(-alpha*x)
    if ( region == 1 ) then
        val = term1
    elseif ( region == 2 ) then
        val = term2
    elseif ( region == 3 ) then
        val = term3
    end if
    final_val = val**2
end function wave_func_sq

subroutine find_energy(V0, a, y, energy)
    ! ==========================================
    ! Calculates the energy of particle in finite square well
    ! -|E| = (y/a)^2 - 2V0, considering m = ħ = 1
    ! Sourav Das, January 2022
    ! ------------------------------------------
    ! input ...
    ! V0  - value of finite potential
    ! a   - 2a is width of square well
    ! y   - value of root of f(y) = y^2 - (sec(y))^2 - λ
    ! output ...
    ! energy  - energy of the particle for the given inputs
    ! ==========================================
    implicit none
    real,intent(in) :: V0, a, y
    real,intent(out) :: energy
    energy = 2*V0 - (y/a)**2
    energy = abs(energy)
end subroutine find_energy

subroutine bisection(f, a, b, tol, success, root, constant)
    ! ==========================================
    ! Calculates the approximate root of function f
    ! Method: Bisection
    ! Sourav Das, December 2021
    ! ------------------------------------------
    ! input ...
    ! f   - function
    ! a   - lower limit/guess
    ! b   - upper limit/guess
    ! tol - desired uncertainty of the root as |f(root)| < tol
    ! constant - constant argument for passing lambda
    ! output ...
    ! root    - root of f(x) = 0
    ! success - indicator of success
    !         .true.  - root found
    !         .false. - root cannot be found in the given interval
    ! ==========================================
    implicit none
    real :: f, c
    real,intent(in) :: a, b, tol, constant
    real,intent(out) :: root
    logical,intent(out) :: success
    real :: a_lim, b_lim

    a_lim = a
    b_lim = b
    success = .true.

    if ( f(a_lim, constant)*f(b_lim, constant) > 0.0 ) then
        success = .false.
        stop
    else
        do
            c = (a_lim + b_lim)/2.0
            if ( f(a_lim, constant)*f(c, constant) < 0 ) then
                b_lim = c
            else
                a_lim = c
            end if
            if ( abs(f(c, constant)) < tol ) exit
        end do
        root = c
    end if
end subroutine bisection

subroutine simpson(f, x0, xn, n, q, alpha, a, region, area)
    ! This is special subroutine to integrate wave-functions
    ! That's why we need to pass more arguments, for q, a, alpha etc.
    ! ==========================================
    ! Special subroutine to integrate wave-functions
    ! That's why we need to pass more arguments, for q, a, alpha etc.
    ! Sourav Das, January 2022
    ! ------------------------------------------
    ! input ...
    ! f   - function
    ! x0  - lower limit
    ! xn  - upper limit
    ! n   - no. of divisions/segments to integrate
    ! q   - q = sqrt(2mV0 - |E|)/ℏ = sqrt(2V0 - |E|)
    ! alpha  - alpha = sqrt(2m|E|)/ℏ = sqrt(2|E|)
    ! region - region of wave function
    ! output ...
    ! area  - area under the wave function/square of wave function
    ! ==========================================
    implicit none
    real,intent(in) :: x0, xn, q, alpha, a
    integer,intent(in) :: n, region
    real,intent(out) :: area
    real :: f, h, s1, s2
    integer :: i
    
    h = (xn - x0) / n  ! => xn = x0 + nh; therefore x(n-1) = x0 + (n-1)h
    area = f(x0, q, alpha, a, region) + f(xn, q, alpha, a, region)
    s1 = 0.0
    s2 = 0.0
    do i = 1, n-1, 2
        area = area + 4*f(x0 + i*h, q, alpha, a, region)
    end do
    do i = 2, n-2, 2
        area = area + 2*f(x0 + i*h, q, alpha, a, region)
    end do
    area = (h/3.0) * area
end subroutine simpson

!!! INPUT/OUTPUT 1 !!
! Enter width of potential well, 2a (angstrom):
! 10
! Enter finite potential, V0 (eV):
! 5
! Enter lower guess, higher guess, tolerance, to find root:
! 1.0, 1.5, 0.001
! Root Found:   1.336
! Energy (in Hartree):   0.348
! 'non_normalized_wave.txt' generated for plotting
! Total area under non-normalized wave: 11.12
! Normalization constant:   0.300
! 'normalized_wave.txt' generated for plotting

!!! INPUT/OUTPUT 2 !!!
! Enter width of potential well, 2a (angstrom):
! 10
! Enter finite potential, V0 (eV):
! 5
! Enter lower guess, higher guess, tolerance, to find root:
! 3.5, 4.0, 0.0001
! Root Found:   3.952
! Energy (in Hartree):   0.193
! 'non_normalized_wave.txt' generated for plotting
! Total area under non-normalized wave: 11.41
! Normalization constant:   0.296
! 'normalized_wave.txt' generated for plotting
