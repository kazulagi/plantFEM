program main
    use CivilItemClass
    implicit none

    type(CivilItem_) :: ci
    type(FEMDomain_) :: Pier(10), Girder(9)
    real(real64) :: height
    logical :: fitPiers(2)

    do i_i=1,10
        height = - 0.050d0*(dble(i_i) - 10.0d0)**2 + 5.50d0
        Pier(i_i) = ci%BridgePier(&
            Bottom=[3.0d0,2.0d0],&
            Top   =[5.0d0,2.0d0],&
            Divisions=[20,15,20],&
            Transition=[height-2.0d0,height-1.0d0],&
            height= height  )
        call Pier(i_i)%move(x=3.0d0*(i_i-1),y=10.0d0*(i_i-1) )
    enddo
    
    do i_i=1,9
        if(i_i/=9)then
            fitPiers = [.true., .false.]
        else
            fitPiers = [.true., .true.]
        endif
        
        girder(i_i) = ci%BridgeGirder(&
        From=Pier(i_i),&
        To  =Pier(i_i+1),&
        Thickness=1.0d0,Width=5.0d0,&
        Divisions=[20,50,5], &
        fitPiers=fitPiers&
        )
        call girder(i_i)%vtk("girder" +str(i_i) )
        call Pier(i_i)%vtk("Pier" +str(i_i) )
        call Pier(i_i+1)%vtk("Pier" +str(i_i+1) )
    enddo

end program