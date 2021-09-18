module FarmClass
    use, intrinsic :: iso_fortran_env
    use SoilClass
    use SoybeanClass
    implicit none

    type :: Farm_
        type(Soybean_),allocatable :: Soybean(:,:)
        type(Soil_) ::  Soil
        integer(int32) :: num_of_ridge
        integer(int32) :: num_of_plant_per_ridge
        real(real64)   :: width_of_ridge
        real(real64)   :: width_of_plant_per_ridge
        real(real64)   :: length_of_farm, width_of_farm
        real(real64)   :: soil_depth
        real(real64)   :: seed_depth
        integer(int32) :: total_num_of_plant
        real(real64) :: plant_density
        real(real64) :: plant_density_m
        real(real64) :: total_weight_of_seed
        real(real64) :: total_area
        real(real64) :: g_per_100seed
        real(real64) :: locale(2)
        
        real(real64) :: Water_kg
    contains
        procedure :: init => initFarm
        procedure :: sowing => initFarm
        procedure :: fertilize => fertilizeFarm
        procedure :: diagnosis => diagnosisFarm
        procedure :: grow   => growFarm
        procedure :: export => exportFarm
    end type

contains

! ############################################
subroutine initFarm(obj,crop_name,num_of_ridge, num_of_plant_per_ridge,width_of_ridge,&
    width_of_plant_per_ridge,length_of_farm, width_of_farm,soil_depth,seed_depth,g_per_100seed,&
    meter,single,Variety)

    class(Farm_),intent(inout)  :: obj
    integer(int32),optional,intent(in) :: num_of_ridge, num_of_plant_per_ridge
    real(real64),optional,intent(in) :: width_of_ridge, width_of_plant_per_ridge,soil_depth
    real(real64),optional,intent(in) :: length_of_farm, width_of_farm,seed_depth,g_per_100seed
    character(*),intent(in) :: crop_name
    character(*),optional,intent(in) :: Variety
    logical,optional,intent(in)      :: meter,single
    integer(int32) :: i,j,k,l,n,m

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
    call obj%soil%init()

    if(allocated(obj%soybean) )then
        deallocate(obj%soybean)
    endif
    allocate(obj%soybean(obj%num_of_ridge, obj%num_of_plant_per_ridge) )

    if( trim(crop_name) == "soybean" .or. trim(crop_name) == "Soybean" )then
        do i=1,obj%num_of_ridge
            do j=1, obj%num_of_plant_per_ridge
                call obj%soybean(i,j)%sowing(x=obj%width_of_ridge*dble(i-1)  ,&
                    y=obj%width_of_plant_per_ridge*dble(j-1) ,z=obj%seed_depth,Variety=Variety )
            enddo
        enddo        
    else
        print *, "Sorry, crop_name :: ",crop_name ,"is not implemented yet."
    endif

end subroutine
! ############################################


! ############################################
subroutine fertilizeFarm(obj,N_kg,P_kg,K_kg,Ca_kg,Mg_kg,S_kg,Fe_kg,&
    Mn_kg,B_kg,Zn_kg,Mo_kg,Cu_kg,Cl_kg)
    class(Farm_),intent(inout) :: obj
    ! ================
    real(real64),optional,intent(in) :: N_kg
    real(real64),optional,intent(in) :: P_kg
    real(real64),optional,intent(in) :: K_kg
    real(real64),optional,intent(in) :: Ca_kg
    real(real64),optional,intent(in) :: Mg_kg
    real(real64),optional,intent(in) :: S_kg
    ! ================
    real(real64),optional,intent(in) :: Fe_kg
    real(real64),optional,intent(in) :: Mn_kg
    real(real64),optional,intent(in) :: B_kg
    real(real64),optional,intent(in) :: Zn_kg
    real(real64),optional,intent(in) :: Mo_kg
    real(real64),optional,intent(in) :: Cu_kg
    real(real64),optional,intent(in) :: Cl_kg
    ! ================

    call obj%Soil%fertilize(N_kg=N_kg,P_kg=P_kg,K_kg=K_kg,Ca_kg=Ca_kg,Mg_kg=Mg_kg,S_kg=S_kg,Fe_kg=Fe_kg,&
    Mn_kg=Mn_kg,B_kg=B_kg,Zn_kg=Zn_kg,Mo_kg=Mo_kg,Cu_kg=Cu_kg,Cl_kg=Cl_kg)

end subroutine
! ############################################

! ############################################
subroutine exportFarm(obj,FileName,withSTL,withMesh,TimeStep)
    class(Farm_),intent(inout)::obj
    integer(int32) :: obj_id,plant_id,i,j,tstep
    integer(int32),optional,intent(in) :: TimeStep
    character(*),intent(in) :: FileName
    logical,optional,intent(in) :: withSTL,withMesh
    character(200) :: id
    
    ! Visualize soybean-field
    tstep=input(default=0,option=TimeStep)
    ! initialize
    obj_id = 1 + tstep
    plant_id  = 0

    ! export soybeans
    do i=1,obj%num_of_ridge
        do j=1,obj%num_of_plant_per_ridge
            plant_id = plant_id + 1
            id=trim(  adjustl(fstring( plant_id ) ))
            if( present(withSTL) )then
                if(withSTL .eqv. .true. )then
                    call obj%soybean(i,j)%export(FileName=FileName//trim(id)//".geo",SeedID=obj_id,withSTL=withSTL)   
                endif
            endif
            if( present(withMesh) )then
                if(withMesh .eqv. .true. )then
                    call obj%soybean(i,j)%export(FileName=FileName//trim(id)//".geo",SeedID=obj_id,withMesh=withMesh)   
                endif
            endif
            call obj%soybean(i,j)%export(FileName=FileName//trim(id)//".geo",SeedID=obj_id )
        enddo
    enddo
    print *, "Total "//trim(id)//" plants are exported."

    ! export soil
    call obj%soil%export(FileName=FileName//trim(id)//"soil",format=".geo",objID=obj_id)

end subroutine
! ############################################

! ########################################
subroutine diagnosisFarm(obj,FileName)
    class(Farm_),intent(inout) :: obj
    character(*),optional,intent(in)::FileName

    call obj%Soil%diagnosis(FileName=FileName)

end subroutine
! ########################################

! ########################################
subroutine growFarm(obj,dt,temp,crop_name)
    class(Farm_),intent(inout) :: obj
    character(*),intent(in) :: crop_name
    real(real64),intent(in) :: dt,temp
    integer(int32) :: i,j

    if(crop_name=="Soybean" .or. crop_name=="soybean")then
        do i=1,size(obj%Soybean,1)
            do j=1,size(obj%Soybean,2)
                call obj%Soybean(i,j)%grow(dt=dt,Temp=temp)
            enddo
        enddo
    endif

end subroutine
! ########################################

end module 