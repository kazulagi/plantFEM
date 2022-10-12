program main
    use CivilItemClass
    implicit none

    type(CivilItem_) :: ci
    type(FEMDomain_) :: Pier(2), Girder(1),Shoe(2,8)
    real(real64) :: height
    
    height = 5.0d0
    Pier(1) = ci%BridgePier(&
        Bottom=[3.0d0,2.0d0],&
        Top   =[5.0d0,2.0d0],&
        Divisions=[20,15,20],&
        Transition=[height-2.0d0,height-1.0d0],&
        height= height  )
    call Pier(1)%move(y=0.0d0 )
    
    Pier(2) = ci%BridgePier(&
        Bottom=[3.0d0,2.0d0],&
        Top   =[5.0d0,2.0d0],&
        Divisions=[20,15,20],&
        Transition=[height-2.0d0,height-1.0d0],&
        height= height  )
    call Pier(2)%move(y=10.0d0 )
    
        
    girder(1) = ci%BridgeGirder(&
        From=Pier(1),&
        To  =Pier(2),&
        Thickness=1.0d0,Width=5.0d0,&
        Divisions=[20,50,5], &
        fitPiers=[.true.,.true.]&
        )

    shoe(1,1:8) = ci%BridgeShoes(&
        num_shoes = [4,2] ,&
        Pier=Pier(1) ,     &
        Thickness=0.10d0, &
        Width=0.40d0,      &
        Divisions=[20,20,2] )
    shoe(2,1:8) = ci%BridgeShoes(&
        num_shoes = [4,2] ,&
        Pier=Pier(2) ,     &
        Thickness=0.10d0, &
        Width=0.40d0,      &
        Divisions=[20,20,2] )

    call pier(1)%vtk("pier" +str(1) )
    call pier(2)%vtk("pier" +str(2) )
    do i_i=1,2
        do j_j=1,8
            call shoe(i_i,j_j)%vtk("shoe" +str(i_i)+"_"+str(j_j) )
        enddo
    enddo
    call girder(1)%move(z=0.09d0)
    call girder(1)%vtk("girder" +str(1) )
    


end program