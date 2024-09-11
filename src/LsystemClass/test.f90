program main
   use LsystemClass
   implicit none

   type(soybean_)  :: soy1

   call soy1%init(growth_habit="determinate", Max_Num_Of_Node=100)
   print *, "growth stage of soy1 is :: ", soy1%growth_stage

end program
