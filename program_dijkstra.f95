     program dijkstra
      implicit none

      integer,parameter :: INF=2**30
      integer V, E, newLen, u, mc
      integer f, t, cost, s, g
      integer i, j
      integer,dimension(100,100) :: adjMatrix
      integer,dimension(100) :: dist,prev,visited
      character answer*100
      write(*,*) 'Enter number of nodes.'
      read(*,*) V
      write(*,*) 'Enter number of edges.'
      read(*,*) E
      
!     initialize
      do i=1,V
         do j=1,V
            adjMatrix(i,j) = INF
         end do
         dist(i) = INF
         visited(i) = 0
      end do
      
!     Input graph --
      write(*,*) 'Enter start node and target node'
      read(*,*) s,g
      write(*,*) 'Enter cost of u to v.(u v cost) '
      write(*,*) 'u and v range of [1,100].'
      do i=1,E
         read(*,*) f,t,cost
         adjMatrix(f,t) = cost
      end do
      
!     Dijkstra's algorithm
      dist(s) = 0
      prev(s) = -1
      do
         mc = INF
         do i=1,V
            if(visited(i)==0 .and. dist(i) < mc) then
               mc = dist(i)
               u = i
            endif
         end do
         if(mc == INF .or. u == g) exit
         visited(u) = 1
         do i=1,V
            if(visited(i) == 1 .or. adjMatrix(u,i) == INF) cycle
            newLen = adjMatrix(u,i)+dist(u)
            if(dist(i) > newLen) then
               dist(i) = newLen
               prev(i) = u
            endif
         end do
      end do
      
!     Display
      write(*,*) 'min cost',dist(g)
      i = 1
      do 
         write(*,*) g
         g = prev(g)
         if(g == -1) exit
         write(*,*) '<-'
      end do
      
      end
