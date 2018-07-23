program CallingProgram
use GraphStructure
use ProblemSolver
implicit none

type(Graph) :: G 

call read_data(G, "network_info.dat")           								!reading

print *, 'Below shown is the shortest path length in the graph along with the followed path'
call shortest_path_finder(G)

end program CallingProgram
