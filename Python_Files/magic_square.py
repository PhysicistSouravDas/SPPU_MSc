# Author: Sourav Das 
# (1st Sem, MSc)

n = int(input("Enter order of Magic Square:"))
while n%2 == 0:
    n = int(input("Kindly enter an odd value: "))

start = int(input("Enter the starting value: "))
vacant = start - 1
end = start + (n*n) -1 # pre-calculating the ending value of magic square 
magic = [[vacant for _ in range(n)] for _ in range(n)]
# for row in magic:
#     print(row)    # prints magic square with vacant elements.
i = 0  # starting from first row
j = (n+1-1)//2
# print(j)
for k in range(start, end+1):
    magic[i][j] = k
    oldi = i
    oldj = j
    print(f"The number will be filled at: i={i+1}, j={j+1}")
    i = i - 1
    j = j + 1
    print(f"Before applying boundary conditions: i={i+1}; j={j+1} for next loop")
    if (i == 0-1): i = n-1
    if (j >= n): j = 0
    print(f"After any boundary cond., i={i+1}; j={j+1} for next loop")
    if (magic[i][j] != vacant):
        print(f"The number is already filled at i={i+1}, j={j+1}. So filling number in the row below in next loop.")
        i = oldi + 1
        j = oldj
    for row in magic:
        print(row)    
    print("-------------")
    dummy = input()

#         if (i == 0) i = n   ! periodic boundary condition tests
#         if (j > n) j = 1
#         if ( magic(i, j) /= vacant ) then   ! checking if element is not already filled at this i,j
#             i = oldi + 1    ! assigning the new indices using old remembered values
#             j = oldj
#         end if
#     end do
#     ! writing the magic square
#     do i = 1, n
#         write(*, 100) (magic(i, j), j = 1, n)
#     end do

#     100 format(15I4)
#     ! INPUT/OUTPUT
#     ! Give the order of Magic Square:
#     ! 3
#     !  Give the starting value:
#     ! 5
#     !  Order of Magic Square is           3
#     !  The first number to be filled in is           5
#     !   12   5  10
#     !    7   9  11
#     !    8  13   6    
# end program MAGICSQUARE