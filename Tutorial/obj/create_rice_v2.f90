program create_rice
    use RiceClass
    implicit none
    
    type(rice_) :: rice
    integeR(int32) :: i
    
    call rice%create("Tutorial/obj/rice_v2.json")
    call rice%vtk("rice_v2",single_file=True)
    
end program