program remainder

    implicit none
    integer :: p, q, rem

    write(*,*) "Enter two integers p, q: "
    read(*,*) p, q
    write(*,*) "You entered p, q = ", p, ",", q

    rem = (p - q * (p/q))

    write(*,*) "Dividing p by q, remainder is ", rem

end program remainder