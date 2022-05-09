use FEMDomainClass
use CivilItemClass
implicit none

type(CivilItem_) :: ci
type(FEMdomain_) :: rigidFrames(3), ground, Piers(5), Girders(4)

call ground%create("Cube3D",x_num=30,y_num=30,z_num=30)
call ground%resize(x=600.0d0,y=600.0d0,z=400.0d0)
call ground%move(x=-100.0d0,y=-100.0d0,z=-ground%zmax() )
call ground%vtk("ground")

! create RigidFrameViaduct
do i_i=1,3
    rigidFrames(i_i) = ci%RigidFrameViaduct("Tutorial/obj/RigidFrameViaduct.json")
    call rigidFrames(i_i)%move(y=60.0d0*(i_i-1) )
    call rigidFrames(i_i)%vtk("RigidFrames_" +str(i_i) )
enddo

! Create Bridge Piers
do i_i=1,5
    height = 18.0d0
    Piers(i_i) = ci%BridgePier("Tutorial/obj/BridgePier.json")
    call Piers(i_i)%move(y = 60.0d0*3.0d0 + 30.0d0*i_i )
    call Piers(i_i)%vtk("Piers" +str(i_i) )
enddo

! Create Bridge Girders
do i_i=1,4
    Girders(i_i) = ci%BridgeGirder(&
        From=Piers(i_i),&
        To  =Piers(i_i  + 1 ),&
        Thickness=3.0d0,Width=14.0d0,&
        Divisions=[20,50,5], &
        fitPiers=[.false.,.false.]&
    )
    call Girders(i_i)%vtk("Girders" +str(i_i) )
enddo

end