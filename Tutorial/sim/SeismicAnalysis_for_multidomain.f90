program main
    use SeismicAnalysisClass
    use LoggerClass
    implicit none

    type(SeismicAnalysis_)  :: siml 

    type(FEMDomain_),target :: domains(3)
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:)
    logical :: connected(1,1) = .false. 

    real(real64) ::  &
        Length, Width, Thickness
    real(real64),allocatable ::YoungModulus_1(:),PoissonRatio_1(:),rho_1(:)
    real(real64),allocatable ::YoungModulus_2(:),PoissonRatio_2(:),rho_2(:)
    real(real64),allocatable ::YoungModulus_3(:),PoissonRatio_3(:),rho_3(:)
    real(real64),allocatable :: v(:,:)
    
    real(real64) :: dt,t,angle
    type(MPI_) :: mpid
    
    real(real64) :: Density(1:4),Vs(1:4),Vp(1:4),H(1:4,1:2),E,vv
    integer(int32) :: layer
    type(Random_) :: random
    type(Logger_),allocatable :: loggers(:)

    integer(int32),allocatable :: impact_point_candidate(:),current_impact_point(:)

    call mpid%start()
    ! 常時微動アレイ観測の再現シミュレーション

    ! 地盤データ
    ! rho (t/m^3)    Vp (m/s)    Vs (m/s)     Thickness (m)
    ! 1.4   1000    170   8.0
    ! 1.5   1100    250   4.0
    ! 1.65  1400    350   3.0
    ! 2.00  1900    600  \infty

    ! 速度構造の入力
    density(1) =1.40d0; Vp(1) = 1000.0d0 ; Vs(1) = 170.0d0 ; H(1,1:2) = [      -8.1d0,  0.0d0 ];
    density(2) =1.50d0; Vp(2) = 1100.0d0 ; Vs(2) = 250.0d0 ; H(2,1:2) = [H(1,1)-4.00d0,  H(1,1)];
    density(3) =1.65d0; Vp(3) = 1400.0d0 ; Vs(3) = 350.0d0 ; H(3,1:2) = [H(2,1)-3.00d0,  H(2,1)];
    density(4) =2.00d0; Vp(4) = 1900.0d0 ; Vs(4) = 600.0d0 ; H(4,1:2) = [H(3,1)-3.00d0,  H(3,1)];

    Length    = 200.00d0 ! 1 km
    Width     = 200.00d0 ! 1 km
    Thickness = 17.00d0!  300 m
    !call domains(1)%create("Cube3D",x_num=300,y_num=300,z_num=30)
    call domains(1)%create("Cube3D",x_num=60,y_num=60,z_num=9)
    call domains(1)%resize(x=Length,y=width,z=9.0d0)
    call domains(1)%move(z = - domains(1)%z_max() )

    call domains(2)%create("Cube3D",x_num=40,y_num=40,z_num=8)
    call domains(2)%resize(x=Length,y=width,z=8.0d0)
    call domains(2)%move(z = - domains(2)%z_max() )
    call domains(2)%move(z = - 8.0d0 )

    call domains(3)%create("Cube3D",x_num=30,y_num=30,z_num=15)
    call domains(3)%resize(x=Length,y=width,z=Thickness*3.0d0)
    call domains(3)%move(z = - domains(3)%z_max() )
    call domains(3)%move(z = -15.0d0 )

    call domains(1)%vtk("mesh_1")
    call domains(2)%vtk("mesh_2")
    call domains(3)%vtk("mesh_3")

    print *, domains(1)%nn()
    print *, domains(2)%nn()
    print *, domains(3)%nn()
    
    ! ヤング率およびポワソン比の計算
    !<1st domain>
    YoungModulus_1 = zeros(domains(1)%ne())
    PoissonRatio_1 = zeros(domains(1)%ne())
    Rho_1          = zeros(domains(1)%ne())

    do layer=1,4
        vv = (((Vp(layer)/Vs(layer))**2) - 2.0d0)/2.0d0/((((Vp(layer)/Vs(layer))**2) - 1.0d0))
        E = (Vs(layer)*Vs(layer)*density(layer) )*2.0d0*(1.0d0+vv)
        print *, "LAYER " + str(layer) + " E(GPa) :",(E/1000.0d0/1000.0d0), " v :",vv,"Height: ",H(layer,2)
        YoungModulus_1(domains(1)%getElementList(zmax=H(layer,2)) ) = E 
        PoissonRatio_1(domains(1)%getElementList(zmax=H(layer,2)) ) = vv
        Rho_1(domains(1)%getElementList(zmax=H(layer,2)) ) = density(layer)
    enddo
    call domains(1)%vtk("YoungModulus_1",scalar=YoungModulus_1)
    call domains(1)%vtk("PoissonRatio_1",scalar=PoissonRatio_1)
    call domains(1)%vtk("Density_1",scalar=Rho_1)
    
    !<2nd domain>
    YoungModulus_2 = zeros(domains(2)%ne())
    PoissonRatio_2 = zeros(domains(2)%ne())
    Rho_2          = zeros(domains(2)%ne())

    do layer=1,4
        vv = (((Vp(layer)/Vs(layer))**2) - 2.0d0)/2.0d0/((((Vp(layer)/Vs(layer))**2) - 1.0d0))
        E = (Vs(layer)*Vs(layer)*density(layer) )*2.0d0*(1.0d0+vv)
        print *, "LAYER " + str(layer) + " E(GPa) :",(E/1000.0d0/1000.0d0), " v :",vv,"Height: ",H(layer,2)
        YoungModulus_2(domains(2)%getElementList(zmax=H(layer,2)) ) = E 
        PoissonRatio_2(domains(2)%getElementList(zmax=H(layer,2)) ) = vv
        Rho_2(domains(2)%getElementList(zmax=H(layer,2)) ) = density(layer)
    enddo
    call domains(2)%vtk("YoungModulus_2",scalar=YoungModulus_2)
    call domains(2)%vtk("PoissonRatio_2",scalar=PoissonRatio_2)
    call domains(2)%vtk("Density_2",scalar=Rho_2)

    
    !<3rd domain>
    YoungModulus_3 = zeros(domains(3)%ne())
    PoissonRatio_3 = zeros(domains(3)%ne())
    Rho_3          = zeros(domains(3)%ne())

    do layer=1,4
        vv = (((Vp(layer)/Vs(layer))**2) - 2.0d0)/2.0d0/((((Vp(layer)/Vs(layer))**2) - 1.0d0))
        E = (Vs(layer)*Vs(layer)*density(layer) )*2.0d0*(1.0d0+vv)
        print *, "LAYER " + str(layer) + " E(GPa) :",(E/1000.0d0/1000.0d0), " v :",vv,"Height: ",H(layer,2)
        YoungModulus_3(domains(3)%getElementList(zmax=H(layer,2)) ) = E 
        PoissonRatio_3(domains(3)%getElementList(zmax=H(layer,2)) ) = vv
        Rho_3(domains(3)%getElementList(zmax=H(layer,2)) ) = density(layer)
    enddo
    call domains(3)%vtk("YoungModulus_3",scalar=YoungModulus_3)
    call domains(3)%vtk("PoissonRatio_3",scalar=PoissonRatio_3)
    call domains(3)%vtk("Density_3",scalar=Rho_3)

    
    ! mesh creation done!

    ! overlap mesh
    call domains(1)%overset(domains,2,"GPP",debug=.true.) !
    call domains(2)%overset(domains,1,"GPP",debug=.true.) !
    call domains(2)%overset(domains,3,"GPP",debug=.true.) !
    call domains(3)%overset(domains,2,"GPP",debug=.true.) !

    
    ! >> Run solver
    print *, "Initialize >> "
    call siml%init(domains)


    print *, "Material >> "
    call siml%setMaterial(DomainID=1,density=Rho_1,YoungModulus=YoungModulus_1,PoissonRatio=PoissonRatio_1)
    call siml%setMaterial(DomainID=2,density=Rho_2,YoungModulus=YoungModulus_2,PoissonRatio=PoissonRatio_2)
    call siml%setMaterial(DomainID=3,density=Rho_3,YoungModulus=YoungModulus_3,PoissonRatio=PoissonRatio_3)
    
    print *, "Boundary >> "
    
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        xmax = domains(1)%xmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        ymax = domains(1)%ymin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        xmin = domains(1)%xmax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        ymin = domains(1)%ymax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])


    call siml%setBoundary(DomainID=2,NodeList=domains(2)%getNodeList(&
        xmax = domains(2)%xmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=2,NodeList=domains(2)%getNodeList(&
        ymax = domains(2)%ymin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=2,NodeList=domains(2)%getNodeList(&
        xmin = domains(2)%xmax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=2,NodeList=domains(2)%getNodeList(&
        ymin = domains(2)%ymax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])

    call siml%setBoundary(DomainID=3,NodeList=domains(3)%getNodeList(&
        zmax = domains(3)%zmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=3,NodeList=domains(3)%getNodeList(&
        xmax = domains(3)%xmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=3,NodeList=domains(3)%getNodeList(&
        ymax = domains(3)%ymin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=3,NodeList=domains(3)%getNodeList(&
        xmin = domains(3)%xmax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=3,NodeList=domains(3)%getNodeList(&
        ymin = domains(3)%ymax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])

    print *, "Solve >> "

    ! 1 kHz sampling
    dt = 1.0d0/50.0d0

    allocate(loggers(37) )
    v = zeros(domains(1)%nn(),domains(1)%nd() )

    do j_j=1,36
        call Loggers(j_j)%set(&
            femdomains=domains,&
            position=[50.0d0+3.0d0*cos(radian(10.0d0*j_j) ),50.0d0+3.00d0*sin(radian(10.0d0*j_j)),0.0d0 ],&
            dataset=siml%A,&
            DOF = 3,&
            name="Array_A_"+zfill(j_j,4) )
        call loggers(j_j)%vtk("Array_A_"+zfill(j_j,4) )
        
    enddo
    j_j = 37
    call Loggers(j_j)%set(&
        femdomains=domains,&
        position=[ 50.0d0,50.0d0,0.0d0 ],&
        dataset=siml%A,&
        DOF = 3,&
        name="Array_A_"+zfill(j_j,4) )
    call loggers(j_j)%vtk("Array_A_"+zfill(j_j,4) )

    do j_j=1,37
        call loggers(j_j)%start()
    enddo
    ! damping :: default
    !siml%alpha = 0.0d0
    !siml%beta = 0.0d0
    
    impact_point_candidate = domains(1)%select(&
        center=[Length/2.0d0,Width/2.0d0],&
        radius_range=[Length/2-6.d0,Length/2+6.d0],&
        z_min=-0.10d0)
    current_impact_point = int(zeros(100))
    do i_i=1,size(current_impact_point)
        current_impact_point(i_i) = random%choiceInt(vector=impact_point_candidate)
    enddo

    call siml%setSolverParameters(error=dble(1.0e-6),debug=.true.)
    t = 0.0d0
    do i_i=1,20000
        
        print *, t

        ! m/s
        ! 加速度，ホワイトノイズで遠方(0,0,0),(300,0,0),(0,300,0),(300,3000,0)を上下に揺らす.
        ! σ=1,平均0,倍率50.0d0/1000.0d0 (m/s/s)
        
        call siml%setBoundary(DomainID=1,NodeList=[current_impact_point(i_i) ]&
            ,condition="A",boundaryValue=[0.0d0,0.0d0,500.0d0/1000.0d0*random%gauss(mu=0.0d0,sigma=1.0d0) ])
        
        t = t + dt

        ! ILU is too slow!
        !call siml%solve(dt=dt,timeIntegral="Nemwark-beta",use_same_matrix=.true.,&
        !    preconditioning="PointJacobi")
        siml%overset_penalty=YoungModulus_1(1)*1.00d0
        call siml%solve(dt=dt,timeIntegral="Nemwark-beta",use_same_matrix=.true.)
        
        v = reshape(siml%v,domains(1)%nn(),domains(1)%nd() )
        !if(mod(i_i-1,100)==0 )then
            call domains(1)%vtk("v_"+"x_d1_step_"+zfill(i_i,4), siml%velocity(domainID=1,direction=1) )
            call domains(1)%vtk("v_"+"y_d1_step_"+zfill(i_i,4), siml%velocity(domainID=1,direction=2) )
            call domains(1)%vtk("v_"+"z_d1_step_"+zfill(i_i,4), siml%velocity(domainID=1,direction=3) )

            call domains(2)%vtk("v_"+"x_d2_step_"+zfill(i_i,4), siml%velocity(domainID=2,direction=1) )
            call domains(2)%vtk("v_"+"y_d2_step_"+zfill(i_i,4), siml%velocity(domainID=2,direction=2) )
            call domains(2)%vtk("v_"+"z_d2_step_"+zfill(i_i,4), siml%velocity(domainID=2,direction=3) )


            call domains(3)%vtk("v_"+"x_d3_step_"+zfill(i_i,4), siml%velocity(domainID=3,direction=1) )
            call domains(3)%vtk("v_"+"y_d3_step_"+zfill(i_i,4), siml%velocity(domainID=3,direction=2) )
            call domains(3)%vtk("v_"+"z_d3_step_"+zfill(i_i,4), siml%velocity(domainID=3,direction=3) )
        !endif
        do j_j=1,37
            call loggers(j_j)%save(t=t)
        enddo
        
    enddo
    ! destroy
    call siml%remove()
    call mpid%end()

    
end program main


