program main
    use CivilItemClass
    implicit none

    type(CivilItem_) :: ci
    type(FEMDomain_) :: Pier(10), Girder(9)

    do i_i=1,10
        Pier(i_i) = ci%BridgePier(&
            Bottom=[3.0d0,2.0d0],&
            Top   =[5.0d0,2.0d0],&
            Divisions=[20,15,20],&
            Transition=[3.0d0,4.0d0],&
            height=5.0d0)
        call Pier(i_i)%move(x=3.0d0*(i_i-1),y=10.0d0*(i_i-1),z=1.0d0*i_i )
        
    enddo
    
    do i_i=1,9
        girder(i_i) = ci%BridgeGirder(&
        From=Pier(i_i),&
        To  =Pier(i_i+1),&
        Thickness=1.0d0,Width=5.0d0,&
        Divisions=[20,50,5], &
        fitPiers=.true. &
         )
        call girder(i_i)%vtk("girder" +str(i_i) )
        call Pier(i_i)%vtk("Pier" +str(i_i) )
        call Pier(i_i+1)%vtk("Pier" +str(i_i+1) )
    enddo
end program