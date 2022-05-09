use FEMDomainClass
use CivilItemClass
implicit none

type(CivilItem_) :: ci
type(FEMdomain_) :: rigidFrames(3), ground, Piers(5), Girders(4)
real(real64)     :: height 

call ground%create("Cube3D",x_num=30,y_num=30,z_num=30)
call ground%resize(x=600.0d0,y=600.0d0,z=400.0d0)
call ground%move(x=-100.0d0,y=-100.0d0,z=-ground%zmax() )
call ground%vtk("ground")

! create RigidFrameViaduct
do i_i=1,3
    rigidFrames(i_i) = ci%RigidFrameViaduct(&
            NumPiers = [2, 5]  ,&! 2 x 10 piers
            width =  14.0d0 ,&
            length = 50.0d0 , & ! 200 m
            height = 20.0d0  ,&
            PierThickness =2.0d0  ,&
            division = [14,50,20]*2 ,&
            MiddlePierHeights = [7.0d0,11.0d0]  &
        )
    call rigidFrames(i_i)%move(y=60.0d0*(i_i-1) )
    call rigidFrames(i_i)%vtk("RigidFrames_" +str(i_i) )
enddo

do i_i=1,5
    height = 18.0d0
    Piers(i_i) = ci%BridgePier(&
        Bottom=[8.0d0  , 3.0d0],&
        Top   =[14.0d0 , 3.0d0],&
        Divisions=[20,15,30],&
        Transition=[height-4.0d0,height-1.0d0],&
        height= height  &
    )
    call Piers(i_i)%move(y = 60.0d0*3.0d0 + 30.0d0*i_i )
    call Piers(i_i)%vtk("Piers" +str(i_i) )
enddo

do i_i=1,4
    Girders(i_i) = ci%BridgeGirder(&
        From=Piers(i_i),&
        To  =Piers(i_i  + 1 ),&
        Thickness=3.0d0,Width=14.0d0,&
        Divisions=[20,50,5], &
        fitPiers=[.false.,.false.]&
    )
    call Girders(i_i)%vtk("Gierders" +str(i_i) )
enddo




end