program main
    use SoybeanClass
    use ArrayClass
    use IOClass
    implicit none
    
    type(Soybean_) :: soy
    type(Environment_) :: env
    type(FEMDomain_),allocatable :: domains(:)
    integer(int32) :: hour
    real(real64),allocatable :: photosynthesis(:),carbon_concentration(:),&
        respiration(:),Photosynthate(:),internal_pressure(:),struct_vs_nonstruct(:),&
        void_ratio(:),DiffusionCoeff(:),volume(:),RHS(:),FixValue(:)
    integer(int32),allocatable :: target_element_list(:),FixBoundary(:)
    real(real64) :: dt
    type(CRS_) :: crs
    type(IO_)  :: f
    

    ! initialize environment
    ! senario.csv: table data and time series
    ! senario.json: meta data
    ! all data should be contained.
    call env%init(json="Tutorial/obj/env.json")
    
    ! initialize soybean
    ! morphological data
    !soy%stem_division(1:3) = [2, 2, 20]
    !soy%peti_division(1:3) = [2, 2, 20]
    call soy%init(config="Tutorial/obj/soy.json")

    call soy%vtk("day"+zfill(0,4) )


    !call soy%setPropertiesCarbonDiffusionCoefficient(default_value=0.00010d0)
    ! grow soybean
    dt = 1.d0 ! 1 sec
    do hour = 1, 100
        ! (1-1) compute photosynthesis ratio (g/s)
        photosynthesis     = soy%getPhotoSynthesis(env=env,dt=dt) ! unit: micro-gram
        call soy%vtk("photosyn_"+zfill(hour,4),scalar_field=photosynthesis,single_file=.true. )
        
        volume     = soy%getVolumePerElement() ! unit: micro-gram
        call soy%vtk("vol_"+zfill(hour,4),scalar_field=volume,single_file=.true. )

        call soy%vtk("photosyn_vol_"+zfill(hour,4),scalar_field=photosynthesis/volume,single_file=.true. )
        ! (1-2) search carbon requirement(sink capacity) (g/s)
        ! Fix carbo concentration (micro-g/m^3) at sink
        carbon_concentration = soy%getCarbon_concentration(env=env,&
            FixBoundary=FixBoundary,FixValue=FixValue) ! micro-gram/m^3/s
        call soy%vtk("carb_conc_"+zfill(hour,4),scalar_field=carbon_concentration,single_file=.true. )
        
        ! get respiration (micro-g/m^3)
        respiration = soy%getRespiration(env=env) ! micro-gram/m^3/s
        call soy%vtk("carb_req_"+zfill(hour,4),scalar_field=respiration,single_file=.true. )
        
        DiffusionCoeff = soy%getDiffusionCoefficient()
        call soy%vtk("diff_coef_"+zfill(hour,4),scalar_field=DiffusionCoeff,single_file=.true. )
        ! (2) reaction-diffusion (g/s for each point)
        ! dC/dt = - D d^2/dx^2 c + P - R 
        ! ソースの拡散係数(μg/m^3/m/s)
        DiffusionCoeff = dble(1.0e-15) ! 
        Photosynthate = soy%getCarbonFlow( &
            dt = dt,&
            DiffusionCoeff = DiffusionCoeff,&
            photosynthesis=photosynthesis/volume,& ! reaction term (P)
            respiration = respiration,&     ! reaction term (R)
            FixBoundary=[(i_i,i_i=1,10)],&
            FixValue=ones(10),&
            Photosynthate_n=soy%Photosynthate_n,&
            delta_Photosynthate_dt_n=soy%delta_Photosynthate_dt_n, &
            penalty = maxval(DiffusionCoeff)*(1.0e+20),&
            RHS = RHS, &
            Matrix = CRS, &
            tol = dble(1.0e-1), &
            debug=.true. &
            ) ! initial condition
        soy%Photosynthate_n = Photosynthate
        call soy%vtk("biomass_"+zfill(hour,4),scalar_field=Photosynthate,single_file=.true. )
        print *, size(RHS),maxval(RHS),minval(RHS)
        
        print *, soy%nn(),soy%ne()
        call soy%vtk("RHS_"+zfill(hour,4),scalar_field=RHS,single_file=.true. )


        
        ! (3-1) decide ratio of structural carbon : non-structural carbon
        !struct_vs_nonstruct = soy%struct_vs_nonstruct( &
        !    Photosynthate=Photosynthate)


        ! (3-2) increment of biomass to internal pressure
        !internal_pressure = soy%internal_pressure( &
        !    Photosynthatet=Photosynthate,&
        !    struct_vs_nonstruct=struct_vs_nonstruct)
        
        ! (3-3) increment of biomass to void ratio
        !void_ratio = soy%internal_pressure( &
        !    Photosynthate=Photosynthate,&
        !    struct_vs_nonstruct=struct_vs_nonstruct)


        ! (4) increment of Young modulus and yield parameter to void ratio
        !call soy%update_Texture( &
        !    internal_pressure=internal_pressure,&
        !    struct_vs_nonstruct=struct_vs_nonstruct)
        
        ! (5) pressure-induced deformation
        !displacement = soy%static_deformation( &
        !    ground_level=0.0d0,&
        !    internal_pressure=internal_pressure)
        
        ! (6) deform soybean
        !call soy%deform(displacement=displacement)
        

        ! (7) remesh (create new object in apical)
        !call soy%remesh()
        

        ! (8) export morphology
        !call soy%vtk("day"+zfill(days,4) )
    enddo

end program
    
