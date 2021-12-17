program compare_numbers

implicit none
real :: x, y, z

write(*,*)"Enter the value of x, y, z "
read(*,*) x, y, z

write(*,*)x, y, z

if (x>y .and. x>z) then
    write(*,*)"x is largest"
else if (y>x .and. y>z) then
    write(*,*)"y is largest"
else if (z>y .and. z>x) then
    write(*,*)"z is largest"
else
    write(*,*)"x,y,z not unique"
end if

end program