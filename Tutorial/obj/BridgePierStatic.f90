program main
    use COOClass
    use FEMDomainClass
    use FEMSolverClass
    use CivilItemClass
    implicit none
    
    type(FEMDomain_) :: cube(5)
    type(FEMSolver_) :: solver
    type(CivilItem_) :: ci
    integer(int32),allocatable   :: FixBoundary(:)
    integer(int32),allocatable   :: Overset_Elements(:,:)
    integer(int32)   :: ElementID, DomainID,i
    type(IO_) :: f
    type(MPI_) :: mpid
    
    integer(int32),allocatable :: row(:)
    integer(int32),allocatable :: col(:)
    real(real64),allocatable :: val(:),all_disp(:),sigma(:)
    real(real64) :: height
    integer(int32) :: nn = 1000000
    
    call mpid%start()
    
    height=6.0d0 ! m
    cube(1) = ci%BridgePier(&
        Bottom=[3.0d0,2.0d0],&
        Top   =[5.0d0,2.0d0],&
        Divisions=[20,15,30],&
        Transition=[height-2.0d0,height-1.0d0],&
        height= height  )

    cube(2) = ci%BridgePier(&
        Bottom=[3.0d0,2.0d0],&
        Top   =[5.0d0,2.0d0],&
        Divisions=[20,15,30],&
        Transition=[height-2.0d0,height-1.0d0],&
        height= height  )

    cube(3) = ci%BridgePier(&
        Bottom=[3.0d0,2.0d0],&
        Top   =[5.0d0,2.0d0],&
        Divisions=[20,15,30],&
        Transition=[height-2.0d0,height-1.0d0],&
        height= height  )
    
    call cube(1)%move(y= 0.0d0 )    
    call cube(2)%move(y=10.0d0 )    
    call cube(3)%move(y=20.0d0 )

    cube(4) = ci%BridgeGirder(&
        From=cube(1),&
        To  =cube(2),&
        Thickness=1.0d0,Width=5.0d0,&
        Divisions=[20,50,5], &
        fitPiers=[.false.,.false.]&
    )
    cube(5) = ci%BridgeGirder(&
        From=cube(2),&
        To  =cube(3),&
        Thickness=1.0d0,Width=5.0d0,&
        Divisions=[20,50,5], &
        fitPiers=[.false.,.false.]&
    )
    
    call cube(4)%move(z=-0.50d0)
    call cube(5)%move(z=-0.50d0)

    call cube(1)%vtk("cube_no1")
    call cube(2)%vtk("cube_no2")
    call cube(3)%vtk("cube_no3")
    call cube(4)%vtk("cube_no4")
    call cube(5)%vtk("cube_no5")

    
    
    ! overset
    call cube(1)%overset(cube(4),DomainID=4, &
        algorithm=FEMDomain_Overset_P2P,MyDomainID=1) ! or "P2P"
    
    call cube(2)%overset(cube(4),DomainID=4, &
        algorithm=FEMDomain_Overset_P2P,MyDomainID=2) ! or "P2P"

    call cube(2)%overset(cube(5),DomainID=5, &
        algorithm=FEMDomain_Overset_P2P,MyDomainID=2) ! or "P2P"

    call cube(3)%overset(cube(5),DomainID=5, &
        algorithm=FEMDomain_Overset_P2P,MyDomainID=3) ! or "P2P"

    call cube(4)%overset(cube(1),DomainID=1, &
        algorithm=FEMDomain_Overset_P2P,MyDomainID=4) ! or "P2P"

    call cube(5)%overset(cube(3),DomainID=3, &
        algorithm=FEMDomain_Overset_P2P,MyDomainID=5) ! or "P2P"
    ! debug
    ! confirmed :: no bug in %overset
    !do i_i=1,5
    !    print *, cube(i_i)%num_oversetconnect
    !    j_j = cube(i_i)%num_oversetconnect
    !    print *, cube(i_i)%oversetconnect(1)%DomainIDs12(1),"<->",&
    !        cube(i_i)%oversetconnect(1)%DomainIDs12(16)
    !    print *, cube(i_i)%oversetconnect(j_j)%DomainIDs12(1),"<->",&
    !        cube(i_i)%oversetconnect(j_j)%DomainIDs12(16)
    !    
    !enddo
    !stop
    
    call solver%init(NumDomain=5)
    call solver%setDomain(FEMDomains=cube(1:5),DomainIDs=[1,2,3,4,5])
    call solver%setCRS(DOF=3)
    

    !$OMP parallel 
    !$OMP do
    do DomainID=1,size(cube)
        do ElementID = 1, cube(DomainID)%ne()
            call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
               Matrix=cube(DomainID)%StiffnessMatrix(ElementID=ElementID,E=1000000.0d0, v=0.10d0) )
            call solver%setVector(DomainID=DomainID,ElementID=ElementID,DOF=3,&
                Vector=cube(DomainID)%MassVector(&
                    ElementID=ElementID,&
                    DOF=cube(DomainID)%nd() ,&
                    Density=2.700d0,&
                    Accel=[0.0d0, 0.0d0, -9.80d0]&
                    ) & 
                )
        enddo
    enddo
    !$OMP end do
    !$OMP end parallel

    print *, "[ok]Element-matrices done"
    call solver%setEbOM(penalty=10000000.0d0, DOF=3)
    !
    !print *, "matrices imported."
    ! disp. boundary
    FixBoundary = cube(1)%select(z_max = cube(1)%z_min() )*3-2
    call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
    FixBoundary = cube(1)%select(z_max = cube(1)%z_min() )*3-1
    call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)
    FixBoundary = cube(1)%select(z_max = cube(1)%z_min() )*3-0
    call solver%fix(DomainID=1,IDs=FixBoundary,FixValue=0.0d0)


    FixBoundary = cube(2)%select(z_max = cube(2)%z_min() )*3-2 + cube(1)%nn()*3
    call solver%fix(DomainID=2,IDs=FixBoundary,FixValue=0.0d0)
    FixBoundary = cube(2)%select(z_max = cube(2)%z_min() )*3-1 + cube(1)%nn()*3
    call solver%fix(DomainID=2,IDs=FixBoundary,FixValue=0.0d0)
    FixBoundary = cube(2)%select(z_max = cube(2)%z_min() )*3-0 + cube(1)%nn()*3
    call solver%fix(DomainID=2,IDs=FixBoundary,FixValue=0.0d0)
    

    FixBoundary = cube(3)%select(z_max = cube(3)%z_min() )*3-2 + cube(1)%nn()*3 + cube(2)%nn()*3
    call solver%fix(DomainID=3,IDs=FixBoundary,FixValue=0.0d0)
    FixBoundary = cube(3)%select(z_max = cube(3)%z_min() )*3-1 + cube(1)%nn()*3 + cube(2)%nn()*3
    call solver%fix(DomainID=3,IDs=FixBoundary,FixValue=0.0d0)
    FixBoundary = cube(3)%select(z_max = cube(3)%z_min() )*3-0 + cube(1)%nn()*3 + cube(2)%nn()*3
    call solver%fix(DomainID=3,IDs=FixBoundary,FixValue=0.0d0)
    !
    print *, "b.c. imported."
    
    !
    !! solve
    solver%debug = .true.
    !
    all_disp = solver%solve()
    all_disp(:) = all_disp(:)*10.0d0
    call cube(1)%deform(disp=all_disp(                           1:solver%CRS_ID_Starts_From(2)-1  ) )
    call cube(2)%deform(disp=all_disp(solver%CRS_ID_Starts_From(2):solver%CRS_ID_Starts_From(3)-1  ) )
    call cube(3)%deform(disp=all_disp(solver%CRS_ID_Starts_From(3):solver%CRS_ID_Starts_From(4)-1  ) )
    call cube(4)%deform(disp=all_disp(solver%CRS_ID_Starts_From(4):solver%CRS_ID_Starts_From(5)-1  ) )
    call cube(5)%deform(disp=all_disp(solver%CRS_ID_Starts_From(5):                                ) )
    !
    ! option for stress tensors
    ! (i,j),I1, I2, I3, J1, J2, J3
    call cube(1)%vtk("cube_1_I1",&
        scalar=cube(1)%getElementCauchyStress(option="I1",&
        displacement=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ),&
        E=[1000000.0d0],v=[0.30d0]) )
    call cube(2)%vtk("cube_2_I1",&
        scalar=cube(2)%getElementCauchyStress(option="I1",&
        displacement=all_disp(solver%CRS_ID_Starts_From(2):solver%CRS_ID_Starts_From(3)-1  ),&
        E=[1000000.0d0],v=[0.10d0])  )
    call cube(3)%vtk("cube_3_I1",&
        scalar=cube(3)%getElementCauchyStress(option="I1",&
        displacement=all_disp(solver%CRS_ID_Starts_From(3):solver%CRS_ID_Starts_From(4)-1  ),&
        E=[1000000.0d0],v=[0.10d0])  )
    call cube(4)%vtk("cube_4_I1",&
        scalar=cube(4)%getElementCauchyStress(option="I1",&
        displacement=all_disp(solver%CRS_ID_Starts_From(4):solver%CRS_ID_Starts_From(5)-1  ),&
        E=[1000000.0d0],v=[0.10d0])  )
    call cube(5)%vtk("cube_5_I1",&
        scalar=cube(5)%getElementCauchyStress(option="I1",&
        displacement=all_disp(solver%CRS_ID_Starts_From(5):  ),&
        E=[1000000.0d0],v=[0.10d0])  )
!    call cube(1)%vtk("cube_1_J2_1",&
!        scalar=cube(1)%getElementCauchyStress(option="J2",&
!        displacement=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ),&
!        E=[1000000.0d0],v=[0.30d0]) )
!    call cube(2)%vtk("cube_2_J2_1",&
!        scalar=cube(2)%getElementCauchyStress(option="J2",&
!        displacement=all_disp(solver%CRS_ID_Starts_From(2): solver%CRS_ID_Starts_From(3)-1  ),&
!        E=[1000000.0d0],v=[0.10d0])  )
!    !
!    call cube(3)%vtk("cube_3_J2_1",&
!        scalar=cube(3)%getElementCauchyStress(option="J2",&
!        displacement=all_disp(solver%CRS_ID_Starts_From(3):  ),&
!        E=[1000000.0d0],v=[0.10d0])  )
!    !
!    
!    call cube(1)%vtk("cube_1_xx_1",&
!        scalar=cube(1)%getElementCauchyStress(option="(1,1)",&
!        displacement=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ),&
!        E=[1000000.0d0],v=[0.30d0]) )
!    call cube(2)%vtk("cube_2_xx_1",&
!        scalar=cube(2)%getElementCauchyStress(option="(1,1)",&
!        displacement=all_disp(solver%CRS_ID_Starts_From(2): solver%CRS_ID_Starts_From(3)-1  ),&
!        E=[1000000.0d0],v=[0.10d0])  )
!    !
!    call cube(3)%vtk("cube_3_xx_1",&
!        scalar=cube(3)%getElementCauchyStress(option="(1,1)",&
!        displacement=all_disp(solver%CRS_ID_Starts_From(3):  ),&
!        E=[1000000.0d0],v=[0.10d0])  )
!    !
!    
!    call cube(1)%vtk("cube_1_yy_1",&
!        scalar=cube(1)%getElementCauchyStress(option="(2,2)",&
!        displacement=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ),&
!        E=[1000000.0d0],v=[0.30d0]) )
!    call cube(2)%vtk("cube_2_yy_1",&
!        scalar=cube(2)%getElementCauchyStress(option="(2,2)",&
!        displacement=all_disp(solver%CRS_ID_Starts_From(2): solver%CRS_ID_Starts_From(3)-1  ),&
!        E=[1000000.0d0],v=[0.10d0])  )
!    !
!    call cube(3)%vtk("cube_3_yy_1",&
!        scalar=cube(3)%getElementCauchyStress(option="(2,2)",&
!        displacement=all_disp(solver%CRS_ID_Starts_From(3):  ),&
!        E=[1000000.0d0],v=[0.10d0])  )
!    !
!    call cube(1)%vtk("cube_1_zz_1",&
!        scalar=cube(1)%getElementCauchyStress(option="(3,3)",&
!        displacement=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ),&
!        E=[1000000.0d0],v=[0.30d0]) )
!
!    call cube(2)%vtk("cube_2_zz_1",&
!        scalar=cube(2)%getElementCauchyStress(option="(3,3)",&
!        displacement=all_disp(solver%CRS_ID_Starts_From(2): solver%CRS_ID_Starts_From(3)-1  ),&
!        E=[1000000.0d0],v=[0.10d0])  )
!    !
!    call cube(3)%vtk("cube_3_zz_1",&
!        scalar=cube(3)%getElementCauchyStress(option="(3,3)",&
!        displacement=all_disp(solver%CRS_ID_Starts_From(3):  ),&
!        E=[1000000.0d0],v=[0.10d0])  )
!    call mpid%end()
    !
    end program
    