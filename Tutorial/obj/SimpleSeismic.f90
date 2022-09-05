
program main
    use SeismicAnalysisClass
    use LoggerClass
    implicit none

    type(SeismicAnalysis_)  :: siml 

    type(FEMDomain_),target :: domains(1)
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:)
    logical :: connected(1,1) = .false. 

    real(real64) ::  &
        Length, Width, Thickness
    real(real64),allocatable ::YoungModulus(:),PoissonRatio(:),rho(:)
    real(real64),allocatable :: v(:,:)
    
    real(real64) :: dt,t,angle
    type(MPI_) :: mpid
    
    real(real64) :: Density(1:4),Vs(1:4),Vp(1:4),H(1:4,1:2),E,vv
    integer(int32) :: layer
    type(Random_) :: random
    type(Logger_),allocatable :: loggers(:)
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

    Length    = 100.00d0 ! 1 km
    Width     = 100.00d0 ! 1 km
    Thickness = 30.00d0!  300 m
    !call domains(1)%create("Cube3D",x_num=300,y_num=300,z_num=30)
    call domains(1)%create("Cube3D",x_num=50,y_num=50,z_num=30)
    call domains(1)%resize(x=Length,y=width,z=Thickness)
    call domains(1)%move(z = - domains(1)%z_max() )

    call domains(1)%vtk("mesh")

    print *, domains(1)%nn()
    
    ! ヤング率およびポワソン比の計算
    YoungModulus = zeros(domains(1)%ne())
    PoissonRatio = zeros(domains(1)%ne())
    Rho          = zeros(domains(1)%ne())

    do layer=1,4
        vv = (((Vp(layer)/Vs(layer))**2) - 2.0d0)/2.0d0/((((Vp(layer)/Vs(layer))**2) - 1.0d0))
        E = (Vs(layer)*Vs(layer)*density(layer) )*2.0d0*(1.0d0+vv)
        print *, "LAYER " + str(layer) + " E(GPa) :",(E/1000.0d0/1000.0d0), " v :",vv,"Height: ",H(layer,2)
        YoungModulus(domains(1)%getElementList(zmax=H(layer,2)) ) = E 
        PoissonRatio(domains(1)%getElementList(zmax=H(layer,2)) ) = vv
        Rho(domains(1)%getElementList(zmax=H(layer,2)) ) = density(layer)
    enddo
    call domains(1)%vtk("YoungModulus",scalar=YoungModulus)
    call domains(1)%vtk("PoissonRatio",scalar=PoissonRatio)
    call domains(1)%vtk("Density",scalar=Rho)
    
    
    ! mesh creation done!
    
    
    ! >> Run solver
    print *, "Initialize >> "
    call siml%init(domains)

    print *, "Material >> "
    call siml%setMaterial(DomainID=1,density=Rho,YoungModulus=YoungModulus,PoissonRatio=PoissonRatio)
    
    print *, "Boundary >> "
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        zmax = domains(1)%zmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        xmax = domains(1)%xmin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        ymax = domains(1)%ymin() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        xmin = domains(1)%xmax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])
    call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
        ymin = domains(1)%ymax() ),condition="Absorbing Boundary", boundaryValue=[100.0d0, 1.00d0])

    print *, "Solve >> "

    siml%modal%solver%debug= .true.
    siml%modal%solver%er0  = dble(1.0e-14)
    siml%modal%solver%relative_er  = dble(1.0e-14)
    ! 1 kHz sampling
    dt = 1.0d0/100.0d0

    allocate(loggers(37) )
    v = zeros(domains(1)%nn(),domains(1)%nd() )
    do j_j=1,36
        call Loggers(j_j)%set(&
            femdomain=domains(1),&
            position=[50.0d0+3.0d0*cos(radian(10.0d0*j_j) ),50.0d0+3.00d0*sin(radian(10.0d0*j_j)),0.0d0 ],&
            dataset=siml%A,&
            name="Array_A_"+zfill(j_j,4) )
        call loggers(j_j)%vtk("Array_A_"+zfill(j_j,4) )
        
    enddo
    j_j = 37
    call Loggers(j_j)%set(&
        femdomain=domains(1),&
        position=[ 50.0d0,50.0d0,0.0d0 ],&
        dataset=siml%A,&
        name="Array_A_"+zfill(j_j,4) )
    call loggers(j_j)%vtk("Array_A_"+zfill(j_j,4) )

    do j_j=1,37
        call loggers(j_j)%start()
    enddo
    ! damping :: default
    !siml%alpha = 0.0d0
    !siml%beta = 0.0d0
    siml%femsolver%er0 = dble(1.0e-7)
    siml%femsolver%relative_er = dble(1.0e-7)    
    t = 0.0d0
    do i_i=1,200000
        
        print *, t

        ! m/s
        ! 加速度，ホワイトノイズで遠方(0,0,0),(300,0,0),(0,300,0),(300,3000,0)を上下に揺らす.
        ! σ=1,平均0,倍率50.0d0/1000.0d0 (m/s/s)
        call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
            xmax=1.0d0,ymax=1.0d0,zmin=-0.0010d0)&
            ,condition="A",boundaryValue=[0.0d0,0.0d0,500.0d0/1000.0d0*random%gauss(mu=0.0d0,sigma=1.0d0) ])
        
        call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
            xmin=99.0d0,ymax=1.0d0,zmin=-0.0010d0)&
            ,condition="A",boundaryValue=[0.0d0,0.0d0,500.0d0/1000.0d0*random%gauss(mu=0.0d0,sigma=1.0d0) ])
        
        call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
            xmax=1.0d0,ymin=99.0d0,zmin=-0.0010d0)&
            ,condition="A",boundaryValue=[0.0d0,0.0d0,500.0d0/1000.0d0*random%gauss(mu=0.0d0,sigma=1.0d0) ])
        
        call siml%setBoundary(DomainID=1,NodeList=domains(1)%getNodeList(&
            xmin=99.0d0,ymin=99.0d0,zmin=-0.0010d0)&
            ,condition="A",boundaryValue=[0.0d0,0.0d0,500.0d0/1000.0d0*random%gauss(mu=0.0d0,sigma=1.0d0) ])
        t = t + dt
        call siml%solve(dt=dt,timeIntegral="Nemwark-beta",use_same_matrix=.true.,preconditioning="PointJacobi")
        
        v = reshape(siml%v,domains(1)%nn(),domains(1)%nd() )
        !if(mod(i_i,100)==0 )then
        call domains(1)%vtk("v_"+"x_step_"+zfill(i_i/100,5), v(:,1) )
        call domains(1)%vtk("v_"+"y_step_"+zfill(i_i/100,5), v(:,2) )
        call domains(1)%vtk("v_"+"z_step_"+zfill(i_i/100,5), v(:,3) )
        !endif
        do j_j=1,37
            call loggers(j_j)%save(t=t)
        enddo
        exit
    enddo
    ! destroy
    call siml%remove()
    call mpid%end()

    ! [EXIT] BiCGSTAB >> [246] half


end program main


