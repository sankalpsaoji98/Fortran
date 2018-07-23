program MatMul

print *,'Enter the number of rows and columns of the first matrix'
read *, m
read *, n

print *,'Enter the number of columns of the second matrix'
read *, k


do i = 1, m
	do j = 1, n
	A(i,j) = i + j
	end do
end do

print *, A

do i = 1, n
	do j = 1, k
	B(i, j) = i*j
	end do
end do

print *, B

end program MatMul
