program CallingProgram
use GraphStructure
use MatrixCreator
implicit none

type(Graph) :: G 
integer, allocatable, dimension(:,:) :: AdjacencyMatrix   
integer, allocatable, dimension(:,:) :: IncidenceMatrix   

call read_data(G, "input_graph_info.dat")           							!reading

AdjacencyMatrix = adjacency_creator(G)				                 			!final adjacency matrix
print *, 'Below shown is the adjacency matrix'
call matrix_printer(AdjacencyMatrix)									!printing
IncidenceMatrix = incidence_creator(G)  									!final incidence matrix
print *, 'Below shown is the incidence matrix'
call matrix_printer(IncidenceMatrix)									!printing

deallocate (AdjacencyMatrix)
deallocate (IncidenceMatrix)

end program CallingProgram
