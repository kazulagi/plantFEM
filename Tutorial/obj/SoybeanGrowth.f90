program main
    use SoybeanClass
    implicit none
    
    type(Soybean_) :: soy
    type(Environment_) :: env
    integer(int32) :: hour
    real(real64),allocatable :: photosynthesis(:),carbon_concentration(:),&
        respiration(:),delta_biomass(:),internal_pressure(:),struct_vs_nonstruct(:),&
        void_ratio(:)
    integer(int32),allocatable :: target_element_list(:)
    real(real64) :: dt
    
    
    ! initialize environment
    ! senario.csv: table data and time series
    ! senario.json: meta data
    ! all data should be contained.
    call env%init(json="env.json")
    
    ! initialize soybean
    ! morphological data
    
    call soy%init(config="Tutorial/obj/soy.json")
    call soy%vtk("day"+zfill(0,4) )

    !call soy%setPropertiesCarbonDiffusionCoefficient(default_value=0.00010d0)
    ! grow soybean
    dt = 60.d0*60.0d0 ! 1 hour
    do hour = 1, 24*1000
        ! (1-1) compute photosynthesis ratio (g/s)
        photosynthesis     = soy%getPhotoSynthesis(env=env,dt=dt) ! unit: micro-gram
        call soy%vtk("photosyn_"+zfill(hour,4),scalar_field=photosynthesis,single_file=.true. )
        ! (1-2) search carbon requirement(sink capacity) (g/s)
        ! Fix carbo concentration (micro-g/m^3) at sink
        carbon_concentration = soy%getCarbon_concentration(env=env) ! micro-gram/m^3/s
        call soy%vtk("carb_conc_"+zfill(hour,4),scalar_field=carbon_concentration,single_file=.true. )
        ! get respiration (micro-g/m^3)
        respiration = soy%getRespiration(env=env) ! micro-gram/m^3/s
        call soy%vtk("carb_req_"+zfill(hour,4),scalar_field=respiration,single_file=.true. )
        
        ! (2) reaction-diffusion (g/s for each point)
        ! dC/dt = - D d^2/dx^2 c + P - R 
        delta_biomass = soy%getCarbonFlow( &
            dt = dt,&
            photosynthesis=photosynthesis,& ! reaction term (P)
            respiration = respiration,&     ! reaction term (R)
            carbon_concentration=carbon_concentration,& ! dirichlet boundary 
            Photosynthate_n=soy%Photosynthate_n,&
            delta_Photosynthate_dt_n=soy%delta_Photosynthate_dt_n &
            ) ! initial condition
        call soy%vtk("biomass_"+zfill(hour,4),scalar_field=respiration,single_file=.true. )
        
        stop
        ! (3-1) decide ratio of structural carbon : non-structural carbon
        !struct_vs_nonstruct = soy%struct_vs_nonstruct( &
        !    delta_biomass=delta_biomass)

        ! (3-2) increment of biomass to internal pressure
        !internal_pressure = soy%internal_pressure( &
        !    delta_biomasst=delta_biomass,&
        !    struct_vs_nonstruct=struct_vs_nonstruct)
        
        ! (3-3) increment of biomass to void ratio
        !void_ratio = soy%internal_pressure( &
        !    delta_biomass=delta_biomass,&
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
    
