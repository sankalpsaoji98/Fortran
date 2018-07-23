program test
implicit none
integer :: i
real, dimension(10) :: values
open(10, file = 'node_arc_info.txt', status = 'old')

do i = 1, 7
	read(10, *) values(i)
end do

do i = 1, 7
	print *, values(i)
end do 
end program test
