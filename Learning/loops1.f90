program loops1
    implicit none
    INTEGER :: age, i

    write(*,*) "Enter your age:"
    read(*,*) age

    if ( age >= 60 ) then
        do i = 1, 10
            write(*,*) "Good Morning, respected sir!"
        end do
    elseif (age < 60 .and. age > 40) then
        do i = 1, 5
            write(*,*) "Good Morning, Uncle ;)"    
        end do
    elseif (age <= 40 .and. age > 20) then
        write(*,*) "Good Morning young man!"
    ! elseif (age )
    end if

    ! do i = start, end
    !     if ( mod(i,2) == 0 ) then
    !         write(*,*) i
    !     end if
    ! end do

end program loops1


