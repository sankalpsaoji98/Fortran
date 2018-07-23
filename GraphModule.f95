!Module for creating a structure for storage of graph info
module GraphStructure
implicit none

type Graph
	integer :: d										!1 for directed, 0 for undirected
      integer :: n   										!number of nodes
      integer :: a   										!number of arcs
      integer, allocatable, dimension(:,:) :: arcs  					!array of nodes which join to form arcs
end type Graph

end module GraphStructure

!Module which contains subroutines and functions for creation of matrices
module MatrixCreator
use GraphStructure
implicit none

contains 

!Subroutine for reading from a file into the structure
subroutine read_data(G,input_file)
implicit none
  
type(Graph), intent(out) :: G  
character(len = *), intent(in) :: input_file
integer :: i,p,q

open(unit = 10, file = input_file, status = 'old')
 
read(10,*) G%d
read(10,*) G%n
read(10,*) G%a

allocate(G%arcs(G%a,2))

do i = 1, G%a
	read(10,*) p,q
      G%arcs(i,1) = p
      G%arcs(i,2) = q
end do
  
end subroutine read_data

!Subroutine for Matrix Printing
subroutine matrix_printer(G)
implicit none

integer, allocatable, dimension(:,:) :: G  
integer, allocatable, dimension(:) :: z  
integer :: l, m

z = shape(G)
do l = 1, z(1)
	write(*,*) (G(l,m),m = 1, z(2))
end do

write(*,*)  

end subroutine matrix_printer

!Function for Adjacency Matrix Creation
function adjacency_creator(G) result(A)								!A is adjacency matrix of G
implicit none

type(Graph), intent(in) :: G   
integer, allocatable, dimension(:,:) :: A 
integer :: j
  
allocate(A(G%n,G%n))
A = 0  													!initialization to 0

do j=1, G%a
	A(G%arcs(j,2), G%arcs(j,1)) = 1
      if(G%d==0) then
      	A(G%arcs(j,1), G%arcs(j,2)) = 1
      else
    	      A(G%arcs(j,1), G%arcs(j,2)) = -1
      end if
end do

end function adjacency_creator

!Function for Incidence Matrix Creation
function incidence_creator(G) result(I)								!I is incidence matrix of G							
implicit none

type(Graph), intent(in) :: G  
integer, allocatable, dimension(:,:) :: I
integer :: k

allocate(I(G%n,G%a))
I = 0														!initialization to 0

do k=1, G%a
	I(G%arcs(k,2),k) = 1
      if(G%d==0) then
      	I(G%arcs(k,1),k) = 1
      else
      	I(G%arcs(k,1),k) = -1
      end if
end do

end function incidence_creator

end module MatrixCreator
