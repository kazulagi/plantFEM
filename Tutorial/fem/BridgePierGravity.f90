program main
    use COOClass
    use FEMDomainClass
    use FEMSolverClass
    use CivilItemClass
    implicit none
    
    type(FEMDomain_) :: cube(7)
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
    logical :: connected(7,7) = .false.
    real(real64) :: height
    integer(int32) :: nn = 1000000
    
    call mpid%start()
    
    height=10.0d0 ! m
    do i_i=1,4
        cube(i_i) = ci%BridgePier(&
            Bottom=[3.0d0,2.0d0],&
            Top   =[5.0d0,2.0d0],&
            Divisions=[20,15,30],&
            Transition=[height-2.0d0,height-1.0d0],&
            height= height  )
        call cube(i_i)%move(y= dble(i_i-1)*30.0d0 )    
    enddo
    
    do i_i=5,7
        cube(i_i) = ci%BridgeGirder(&
            From=cube(i_i-4),&
            To  =cube(i_i-3),&
            Thickness=1.0d0,Width=5.0d0,&
            Divisions=[20,50,5], &
            fitPiers=[.false.,.false.]&
        )
        call cube(i_i)%move(z=-0.50d0)
    enddo
    
    do i_i=1,size(cube)
        call cube(i_i)%vtk("cube_no"+str(i_i) )
    enddo
    
    ! overset
    connected(1,5) = .true.
    connected(2,5) = .true.
    connected(2,6) = .true.
    connected(3,6) = .true.
    connected(3,7) = .true.
    connected(4,7) = .true.
    do i_i=1,size(connected,1)
        do j_j=1,size(connected,2)
            if( connected(i_i,j_j) )then
                call cube(i_i)%overset(cube(j_j),DomainID=j_j, &
                    algorithm=FEMDomain_Overset_P2P,MyDomainID=i_i) ! or "P2P"
            endif
        enddo
    enddo

    ! setup solver
    call solver%init(NumDomain=size(cube) )
    call solver%setDomain(FEMDomains=cube(:),DomainIDs=[1,2,3,4,5,6,7])
    call solver%setCRS(DOF=3)

    !$OMP parallel 
    !$OMP do
    do DomainID=1,size(cube)
        do ElementID = 1, cube(DomainID)%ne()
            call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
               Matrix=cube(DomainID)%StiffnessMatrix(ElementID=ElementID,E=21700000.0d0, v=0.20d0) )
            call solver%setVector(DomainID=DomainID,ElementID=ElementID,DOF=3,&
                Vector=cube(DomainID)%MassVector(&
                    ElementID=ElementID,&
                    DOF=cube(DomainID)%nd() ,&
                    Density=2.400d0,&
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

    FixBoundary = cube(4)%select(z_max = cube(4)%z_min() )*3-2 + cube(1)%nn()*3 + cube(2)%nn()*3 + cube(3)%nn()*3
    call solver%fix(DomainID=4,IDs=FixBoundary,FixValue=0.0d0)
    FixBoundary = cube(4)%select(z_max = cube(4)%z_min() )*3-1 + cube(1)%nn()*3 + cube(2)%nn()*3 + cube(3)%nn()*3
    call solver%fix(DomainID=4,IDs=FixBoundary,FixValue=0.0d0)
    FixBoundary = cube(4)%select(z_max = cube(4)%z_min() )*3-0 + cube(1)%nn()*3 + cube(2)%nn()*3 + cube(3)%nn()*3
    call solver%fix(DomainID=4,IDs=FixBoundary,FixValue=0.0d0)
    
    print *, "b.c. imported."
    
    !! solve
    solver%debug = .true.
    !
    all_disp = solver%solve()

    all_disp(:) = all_disp(:)!*10.0d0

    call cube(1)%deform(disp=all_disp(                           1:solver%CRS_ID_Starts_From(2)-1  ) )
    call cube(2)%deform(disp=all_disp(solver%CRS_ID_Starts_From(2):solver%CRS_ID_Starts_From(3)-1  ) )
    call cube(3)%deform(disp=all_disp(solver%CRS_ID_Starts_From(3):solver%CRS_ID_Starts_From(4)-1  ) )
    call cube(4)%deform(disp=all_disp(solver%CRS_ID_Starts_From(4):solver%CRS_ID_Starts_From(5)-1  ) )
    call cube(5)%deform(disp=all_disp(solver%CRS_ID_Starts_From(5):solver%CRS_ID_Starts_From(6)-1  ) )
    call cube(6)%deform(disp=all_disp(solver%CRS_ID_Starts_From(6):solver%CRS_ID_Starts_From(7)-1  ) )
    call cube(7)%deform(disp=all_disp(solver%CRS_ID_Starts_From(7):                                ) )
    !
    ! option for stress tensors
    ! (i,j),I1, I2, I3, J1, J2, J3
    call cube(1)%vtk("cube_1_I1",&
        scalar=cube(1)%getElementCauchyStress(i=3,j=3,&
        displacement=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ),&
        E=[21700000.0d0],v=[0.20d0]))
    call cube(2)%vtk("cube_2_I1",&
        scalar=cube(2)%getElementCauchyStress(i=3,j=3,&
        displacement=all_disp(solver%CRS_ID_Starts_From(2):solver%CRS_ID_Starts_From(3)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )
    call cube(3)%vtk("cube_3_I1",&
        scalar=cube(3)%getElementCauchyStress(i=3,j=3,&
        displacement=all_disp(solver%CRS_ID_Starts_From(3):solver%CRS_ID_Starts_From(4)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )
    call cube(4)%vtk("cube_4_I1",&
        scalar=cube(4)%getElementCauchyStress(i=3,j=3,&
        displacement=all_disp(solver%CRS_ID_Starts_From(4):solver%CRS_ID_Starts_From(5)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )
    call cube(5)%vtk("cube_5_I1",&
        scalar=cube(5)%getElementCauchyStress(i=3,j=3,&
        displacement=all_disp(solver%CRS_ID_Starts_From(5):solver%CRS_ID_Starts_From(6)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )
    call cube(6)%vtk("cube_6_I1",&
        scalar=cube(6)%getElementCauchyStress(i=3,j=3,&
        displacement=all_disp(solver%CRS_ID_Starts_From(6):solver%CRS_ID_Starts_From(7)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )
    call cube(7)%vtk("cube_7_I1",&
        scalar=cube(7)%getElementCauchyStress(i=3,j=3,&
        displacement=all_disp(solver%CRS_ID_Starts_From(7):  ),&
        E=[21700000.0d0],v=[0.20d0])  )
    
    call cube(1)%vtk("cube_1_J2",&
        scalar=(abs(cube(1)%getElementCauchyStress(option="J2",&
        displacement=all_disp(1:solver%CRS_ID_Starts_From(2)-1  ),&
        E=[1000000.0d0],v=[0.20d0]) )**(0.50)))
    call cube(2)%vtk("cube_2_J2",&
        scalar=(abs(cube(2)%getElementCauchyStress(option="J2",&
        displacement=all_disp(solver%CRS_ID_Starts_From(2):solver%CRS_ID_Starts_From(3)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )**(0.50)))
    call cube(3)%vtk("cube_3_J2",&
        scalar=(abs(cube(3)%getElementCauchyStress(option="J2",&
        displacement=all_disp(solver%CRS_ID_Starts_From(3):solver%CRS_ID_Starts_From(4)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )**(0.50)))
    call cube(4)%vtk("cube_4_J2",&
        scalar=(abs(cube(4)%getElementCauchyStress(option="J2",&
        displacement=all_disp(solver%CRS_ID_Starts_From(4):solver%CRS_ID_Starts_From(5)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )**(0.50)))
    call cube(5)%vtk("cube_5_J2",&
        scalar=(abs(cube(5)%getElementCauchyStress(option="J2",&
        displacement=all_disp(solver%CRS_ID_Starts_From(5):solver%CRS_ID_Starts_From(6)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )**(0.50)))
    call cube(6)%vtk("cube_6_J2",&
        scalar=(abs(cube(6)%getElementCauchyStress(option="J2",&
        displacement=all_disp(solver%CRS_ID_Starts_From(6):solver%CRS_ID_Starts_From(7)-1  ),&
        E=[21700000.0d0],v=[0.20d0])  )**(0.50)))
    call cube(7)%vtk("cube_7_J2",&
        scalar=(abs(cube(7)%getElementCauchyStress(option="J2",&
        displacement=all_disp(solver%CRS_ID_Starts_From(7):  ),&
        E=[21700000.0d0],v=[0.20d0])  )**(0.50)))
        
    call mpid%end()
end program
    