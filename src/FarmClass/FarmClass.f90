module FarmClass
    use SoilClass
    use SoybeanClass
    implicit none

    type :: Farm_
        type(Soybean_),allocatable :: Soybean(:,:)
        type(Soil_) ::  Soil
        integer :: num_of_ridge
        integer :: num_of_plant_per_ridge
        real(8) :: width_of_ridge
        real(8) :: width_of_plant_per_ridge
        real(8) :: length_of_farm, width_of_farm
        real(8) :: soil_depth
        real(8) :: seed_depth
        integer :: total_num_of_plant
        real(8) :: plant_density,plant_density_m,total_weight_of_seed,total_area
        real(8) :: g_per_100seed
        real(8) :: locale(2)
        
        real(8) :: Water_kg

        ! ================
        ! Nutorient
        !------------
        real(8) :: N_kg
        real(8) :: P_kg
        real(8) :: K_kg
        real(8) :: Ca_kg
        real(8) :: Mg_kg
        real(8) :: S_kg
        !------------
        real(8) :: Fe_kg
        real(8) :: Mn_kg
        real(8) :: B_kg
        real(8) :: Zn_kg
        real(8) :: Mo_kg
        real(8) :: Cu_kg
        real(8) :: Cl_kg
        ! ================

        
        ! ================
        ! Soil phyisical parameters
        real(8) :: C_N_ratio
        real(8) :: EC
        ! ================

    contains
        procedure :: init => initFarm
        procedure :: sowing => initFarm
        procedure :: export => exportFarm
    end type

contains

! ############################################
subroutine initFarm(obj,crop_name,num_of_ridge, num_of_plant_per_ridge,width_of_ridge,&
    width_of_plant_per_ridge,length_of_farm, width_of_farm,soil_depth,seed_depth,g_per_100seed,&
    meter,single)

    class(Farm_),intent(inout)  :: obj
    integer,optional,intent(in) :: num_of_ridge, num_of_plant_per_ridge
    real(8),optional,intent(in) :: width_of_ridge, width_of_plant_per_ridge,soil_depth
    real(8),optional,intent(in) :: length_of_farm, width_of_farm,seed_depth,g_per_100seed
    character(*),intent(in) :: crop_name
    logical,optional,intent(in)      :: meter,single
    integer :: i,j,k,l,n,m

    ! input default value

    obj%num_of_ridge                = 10      ! 10 ridge
    obj%num_of_plant_per_ridge      = 10      ! 10 plant per a ridge
    obj%width_of_ridge              = 10.0d0  ! 10.0 cm 
    obj%width_of_plant_per_ridge    = 10.0d0  ! 10.0 cm          
    obj%length_of_farm              = 100.0d0 ! 1.0 m
    obj%width_of_farm               = 100.0d0 ! 1.0 m  

    obj%soil_depth               = input(default=-100.0d0, option=soil_depth)
    obj%seed_depth               = input(default=-3.0d0, option=seed_depth)
    obj%g_per_100seed           = input(default=30.0d0, option = g_per_100seed )

    

    
    if(present(single) )then
        if(single .eqv. .true.)then
            obj%num_of_ridge                = 1      ! 1 ridge
            obj%num_of_plant_per_ridge      = 1      ! 1 plant per a ridge
            obj%width_of_ridge              = 1.0d0  ! 1.0 cm 
            obj%width_of_plant_per_ridge    = 1.0d0  ! 1.0 cm          
            obj%length_of_farm              = 1.0d0 ! 1.0 m
            obj%width_of_farm               = 1.0d0 ! 1.0 m  
        endif
    endif
    
    ! determine area default = 1 m^2 = (100 cm)^2
    if(present(length_of_farm) .and. present(width_of_farm)  )then
        obj%length_of_farm = length_of_farm
        obj%width_of_farm  = width_of_farm
        
        ! if num_of_ridge and num_of_plant_per_ridge are imported
        if(present(num_of_ridge) .and. present(num_of_plant_per_ridge) )then
            obj%num_of_ridge             = num_of_ridge            
            obj%num_of_plant_per_ridge   = num_of_plant_per_ridge  

            obj%width_of_ridge           = dble(  int( obj%width_of_farm    /dble(obj%num_of_ridge             ) ) )           
            obj%width_of_plant_per_ridge = dble(  int( obj%length_of_farm   /dble(obj%num_of_plant_per_ridge   ) ) ) 
        elseif(present(width_of_ridge) .and. present(width_of_plant_per_ridge) )then
            obj%width_of_ridge           = width_of_ridge           
            obj%width_of_plant_per_ridge = width_of_plant_per_ridge 

            obj%num_of_ridge             = int(   dble(  obj%width_of_farm    /dble(obj%width_of_ridge          )   ) )
            obj%num_of_plant_per_ridge   = int(   dble(  obj%length_of_farm   /dble(obj%width_of_plant_per_ridge)   ) )

        else
            print *, "initFarm #1 >> please give enough information about sowing condition >> default value is set."
        endif
        
    else
        print *, "initFarm #2 >> please give enough information about sowing condition >> default value is set."
    endif

    ! compute total number of plant
    obj%total_area          = obj%length_of_farm * obj%width_of_farm
    obj%total_num_of_plant  = obj%num_of_plant_per_ridge*obj%num_of_ridge
    obj%plant_density       = dble(obj%total_num_of_plant)/dble(obj%total_area) ! plant per cm^2 
    obj%plant_density_m     = obj%plant_density*100.0d0*100.0d0 ! plant per m^2 
    obj%total_weight_of_seed= obj%g_per_100seed* dble(obj%total_num_of_plant)/100.0d0

    ! compute soil profile
    call obj%soil%init(depth=obj%soil_depth,length=obj%length_of_farm,width=obj%width_of_farm)

    if(allocated(obj%soybean) )then
        deallocate(obj%soybean)
    endif
    allocate(obj%soybean(obj%num_of_ridge, obj%num_of_plant_per_ridge) )

    if( trim(crop_name) == "soybean" .or. trim(crop_name) == "Soybean" )then
        do i=1,obj%num_of_ridge
            do j=1, obj%num_of_plant_per_ridge
                call obj%soybean(i,j)%sowing(x=obj%width_of_ridge*dble(i-1)  ,&
                    y=obj%width_of_plant_per_ridge*dble(j-1) ,z=obj%seed_depth )
            enddo
        enddo        
    else
        print *, "Sorry, crop_name :: ",crop_name ,"is not implemented yet."
    endif

end subroutine
! ############################################

! ############################################
subroutine exportFarm(obj,FilePath)
    class(Farm_),intent(inout)::obj
    integer :: obj_id,plant_id,i,j
    character(*),intent(in) :: FilePath
    character(200) :: id
    
    ! Visualize soybean-field

    ! initialize
    obj_id = 1
    plant_id  = 0

    ! export soybeans
    do i=1,obj%num_of_ridge
        do j=1,obj%num_of_plant_per_ridge
            plant_id = plant_id + 1
            id=trim(  adjustl(fstring( plant_id ) ))
            print *, id
            call obj%soybean(i,j)%export(FileName=FilePath//trim(id)//".geo",SeedID=obj_id )
        enddo
    enddo

    ! export soil
    call obj%soil%export(FileName=FilePath//trim(id)//".geo.soil.geo",format=".geo",objID=obj_id)

end subroutine
! ############################################

end module 