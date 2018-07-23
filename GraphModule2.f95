!Module for creating a structure for storage of graph info
module GraphStructure
implicit none

type Graph
	integer :: d									!1 for directed, 0 for undirected
	integer :: n									!number of nodes
	integer :: a									!number of arcs
	integer, allocatable, dimension (:,:) :: arcs				!array of nodes which form arcs - columns 1 and 2, cost - column 3, capacities - column 4  
end type Graph

end module GraphStructure

!Module which contains various subroutines and functions for solving various problems
module ProblemSolver
use GraphStructure
implicit none

contains

!Subroutine for reading from a file into the structure
subroutine read_data(G,input_file)
implicit none
  
type(Graph), intent(out) :: G  
character(len = *), intent(in) :: input_file
integer :: i,p,q,r,s

open(unit = 10, file = input_file, status = 'old')
 
read(10,*) G%d
read(10,*) G%n
read(10,*) G%a

allocate(G%arcs(G%a,4))

do i = 1, G%a
	read(10,*) p,q,r,s
      G%arcs(i,1) = p
      G%arcs(i,2) = q
	G%arcs(i,3) = r
	G%arcs(i,4) = s
end do
  
end subroutine read_data

!Function for finding the shortest path in a graph
subroutine shortest_path_finder(G) 							!Using Dijkstra's Algorithm
implicit none

type(Graph), intent(in) :: G								!Input Graph
integer :: cost
integer, dimension(G%n) :: P								!Shortest path nodes storage
integer, parameter :: infinity = 2**30						
integer, dimension(100,100) :: C							!Cost Storage
integer, dimension(100) :: D, N							!D = Distance storage, N = Visited node storage
integer :: i, j, start_node, end_node, k, u, l, z

do i = 1, G%n										!Initialization
	do j = 1, G%n
		C(i,j) = infinity
	end do
	N(i) = 0
	D(i) = infinity
	P(i) = 0
end do

do i = 1, G%a										!Storing Cost
	C(G%arcs(i,1), G%arcs(i,2)) = G%arcs(i,3)
end do

start_node = G%arcs(1,1)
end_node =G%arcs(G%a,2)
D(start_node) = 0
P(start_node) = -1

do
	k = infinity
	do i = 1, G%n
		if(N(i) == 0 .and. D(i)<k) then
			k = D(i)
			u = i									!u is for storing visited nodes and handling loop decisions
		end if
	end do		
	if (k == infinity .or. u == end_node) exit
	N(u) = 1
	do i = 1, G%n
		if(N(i) == 1 .or. C(u,i) == infinity) cycle
			l = C(u,i) + D(u)
		if (D(i) > l) then
			D(i) = l
			P(i) = u
		end if
	end do
end do

print *, 'Least cost'
cost = D(end_node)
print *, cost

print *, 'Path followed for least cost'
z = size(P)
do i = 4, z
	print *, P(i)
end do
print *, end_node

end subroutine shortest_path_finder

end module ProblemSolver

