program calling_func

   real :: a
   a = area_of_circle(2.0) 
   
   print *, "The area of a circle with radius 2.0 is"
   print *, a
   
end program calling_func
 
function area_of_circle(r)  
implicit none      

   real :: area_of_circle   
   real :: r     
   real :: pi
   
   pi = 4*atan(1.0)     
   area_of_circle = pi*r**2  
   
end function area_of_circle
