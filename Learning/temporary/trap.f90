program trapz    
implicit none 
real::Area,h,s,x,x0,xn,f,error
integer::i,n,nmax,nmin,nstep
write(*,*)'enter the lower limit'
read(*,*) x0
write(*,*)'enter the upper limit'
read(*,*) xn
write(*,*)'enter the nmax'
read(*,*) nmax
write(*,*)'enter the nmin'
read(*,*) nmin
write(*,*)'enter the nstep'
read(*,*) nstep

open(unit=1, file='trap.txt')


do n=nmin,nmax,nstep    
    h=(xn-x0)/n
    s=0.0
    do i=1,n-1
        x=x0+h*i 
        s=s+f(x)
        write(*,*) s
    end do
    
    Area=(h/2.0)*(f(x0)+f(xn)+2.0*s)
    error=Area-2.0
    write(1,*) n,h,h**2,h**3,Area,error
    write(*,*) n,h,h**2,h**3,Area,error
end do
end program trapz

function f(x)
implicit none
real::f,x
    f= sin(x)
    return
end function 
!output
!enter the lower limit
!0
! enter the upper limit
!3.14159
! enter the nmax
!100
! enter the nmin
!10
! enter the nstep
!10
!          10   3.10062002E-02   1.98352373      -1.64762735E-02
!          20   3.87577503E-03   1.99588597      -4.11403179E-03
!          30   1.14837778E-03   1.99817193      -1.82807446E-03
!          40   4.84471879E-04   1.99897206      -1.02794170E-03
!          50   2.48049619E-04   1.99934196      -6.58035278E-04
!          60   1.43547222E-04   1.99954271      -4.57286835E-04
!          70   9.03970795E-05   1.99966431      -3.35693359E-04
!          80   6.05589848E-05   1.99974275      -2.57253647E-04
!          90   4.25325197E-05   1.99979699      -2.03013420E-04
!         100   3.10062023E-05   1.99983501      -1.64985657E-04


