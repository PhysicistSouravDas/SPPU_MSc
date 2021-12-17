! Sourav Das (M.Sc. 1st Semester)
program CHOOSE_4_PTS
    implicit none
    real :: X0, X(20), PTS(4)
    integer :: I, J, N

    N = 15 ! 15 values in X
    ! filling non-uniform values in X
    do I = 1, N
        X(I) = I*I*0.1
    end do
    write(*,*) "The points are:"
    do J = 1, 3
        write(*,*) (X(I), I = 5*J-4, 5*J)
    end do
    
    write(*,*) "Enter a value of X0:"
    read(*,*) X0

    ! Sanitizing input, whether belongs withing range or not
    do
        if ( X0 < X(1) .or. X0 > X(N) ) then
            write(*,100) "Enter a value within the interval:(", 0.1, ", ", N*N*0.1, ")"
            read(*,*) X0
        else
            exit
        end if
    end do
    100 format(a35, f4.2, a2, f5.2, a1)

    ! Loop to find four numbers on either side of X0
    do I = 1, N
        if ( X0 - X(I) < 0 ) then   ! X0 just crossed a no. bigger than it
            ! That means, X0 is before the I-th no.
            if ( I == 2 ) then    ! X0 is between 1st and 2nd no.
                PTS = X(1:4)    ! first 4 numbers
            else if (I == N) then    ! X0 is between 2nd last and last no.
                PTS = X(N-3:N)  ! last 4 numbers
            else
                ! 2 numbers each on either side of X0
                ! I is the next number, so add (I-2)th, (I-1)th, Ith and (I+1)th no.
                PTS = X(I-2:I+1)
            end if
            exit
        end if
    end do
    write(*,*) "Four points for interpolation are:"
    write(*,*) (PTS(I), I = 1, 4)   ! writing the four points
    stop
end program CHOOSE_4_PTS
! OUTPUT1 (First four points case)
! The points are:
! 0.100000001      0.400000006      0.900000036       1.60000002       2.50000000    
!  3.60000014       4.90000010       6.40000010       8.10000038       10.0000000    
!  12.1000004       14.4000006       16.8999996       19.6000004       22.5000000    
! Enter a value of X0:
! 0.3
! Four points for interpolation are:
! 0.100000001      0.400000006      0.900000036       1.60000002 

! OUTPUT2 (Any four points in between)
! The points are:
! 0.100000001      0.400000006      0.900000036       1.60000002       2.50000000    
!  3.60000014       4.90000010       6.40000010       8.10000038       10.0000000
!  12.1000004       14.4000006       16.8999996       19.6000004       22.5000000
! Enter a value of X0:
! 1
! Four points for interpolation are:
! 0.400000006      0.900000036       1.60000002       2.50000000

! OUTPUT3 (Last four points case)
! The points are:
! 0.100000001      0.400000006      0.900000036       1.60000002       2.50000000    
!  3.60000014       4.90000010       6.40000010       8.10000038       10.0000000
!  12.1000004       14.4000006       16.8999996       19.6000004       22.5000000
! Enter a value of X0:
! 20
! Four points for interpolation are:
!  14.4000006       16.8999996       19.6000004       22.5000000
