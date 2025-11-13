program main
   use MeristemClass
   implicit none
    
   type(Meristem_) :: ms

   call ms%init(Meristem_type=PF_MERISTEM_TYPE_SHOOT, radius=1.0d0, length = 1.0d0)
   call ms%vtk("meristem")

end program main