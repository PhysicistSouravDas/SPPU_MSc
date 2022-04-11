program comparison
    implicit none
    integer :: x, y, z
    
    write(*,*) "Enter x, y, z:"
    read(*,*) x, y, z

    ! to find the largest number
    if ( x > z ) then

        if ( x > y ) then
            write(*,*) "x is largest"
        elseif ( x < y ) then
            write(*,*) "y is largest"
        end if
        
    elseif ( x < z ) then

        if ( z > y ) then
            write(*,*) "z is largest"
        elseif ( z < y ) then
            write(*,*) "y is largest"
        end if

    end if
    
    
    
    
    
    
    ! if ( kuldeep_age > sourav_age ) then
    !     write(*,*) "Kuldeep is elder than Sourav"
    ! elseif ( kuldeep_age == sourav_age ) then
    !     write(*,*) "Kuldeep has same age as that of Sourav"
    ! elseif ( kuldeep_age < sourav_age ) then
    !     write(*,*) "Kuldeep is younger than Sourav"
    ! end if

end program comparison

! ----------------------------------------
! operators
! == - check equality
! >
! <
! >=
! <=
! /=
! 1) kuldeep > sourav
! 2) kuldeep < sourav
! 3) kuldeep == sourav