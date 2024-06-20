program create_rice
    use RiceClass
    implicit none
    
    type(rice_) :: rice
    integeR(int32) :: i
    
    call rice%create("Tutorial/obj/rice_v2.json")
    do i=1,size(Rice%rice_shoots)
        call rice%rice_shoots(i)%vtk("rice_shoot_e"+zfill(i,4),single_file=True)   
    enddo 

end program