program prime_check_caller
implicit none

integer :: num, a, m

a = 0

print *, "I will find whether the number was prime. Enter a number and wait."

read *,num

print *, "The number you entered was", num 

prime_number_checker(num)

end program prime_check_caller

function prime_number_checker(m)
implicit none

integer :: i, b

if(num = 0 .OR. num = 1) then
	print *, "The number is neither prime nor composite."
else if (num == 2) then
	print *, "The number is prime."

do i = 2, m-1
	b =  mod(m,i)
	if (b = 0) then
		a = 1
	end if
end do

if (a = 0) then
	print *, "The number is prime."
else 
	print *, "The number is not prime."

end if

end function prime_number_checker
