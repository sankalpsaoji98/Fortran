program loop_testing
implicit none

real, dimension(5) :: numbers
integer, dimension(5,5) :: matrix
integer :: i

do i = 1,5
	numbers(i) = 2.0*i
end do 

do i = 1,5
	print *,numbers(i)
end do

end program loop_testing
