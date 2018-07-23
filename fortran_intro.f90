program fortran_practice
implicit none

   integer :: total, defval
   integer(kind = 8) :: long_number		      ! This is a 8 bit number.    
   real :: average 
   complex :: cx  
   logical :: done 
   character(len=80) :: message 
   
   total = 20000  
   average = 1666.67   
   done = .true.   
   message = "A Big Hello" 
   cx = (3.0, 5.0) ! 

   print *, total
   print *, average
   print *, cx
   print *, done
   print *, message

   print *, huge(total)  				! This number is 2^31 - 1. This is default value also.	
   print *, huge(long_number)
   print *, huge(defval)

   
end program fortran_practice
