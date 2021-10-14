module WaterAbsorptionClass
    use fem
    use DiffusionEquationClass
    use FiniteDeformationClass
    implicit none

    type :: WaterAbsorption_
        type(FEMDomain_),pointer:: Water, Tissue
        type(MaterialProp_),pointer:: a_Psi
        type(MaterialProp_),pointer:: a_P
        type(MaterialProp_),pointer:: theta_eq
        type(MaterialProp_),pointer:: Psi_eq
        type(MaterialProp_),pointer:: a_E
        type(MaterialProp_),pointer:: a_v
        type(MaterialProp_),pointer:: E_eq
        type(MaterialProp_),pointer:: v_eq

        type(DiffusionEq_)::DiffusionEq
        type(FiniteDeform_)::FiniteDeform

        real(real64),allocatable :: WaterAbsorbingPower(:)
        real(real64),allocatable :: WaterPotential(:)
        real(real64),allocatable :: TurgorPressure(:)
        real(real64),allocatable :: WaterContent(:)
        real(real64),allocatable :: WaterMass(:)
        real(real64),allocatable :: volume(:)
        real(real64),allocatable :: Conductivity(:)
        real(real64),allocatable :: YoungsModulus(:)
        real(real64),allocatable :: PoissonsRatio(:)
        real(real64),allocatable :: Permiability(:)
        real(real64),allocatable :: PorePressure(:)
        ! Parameters(A,B)
        ! A : Element ID 

        real(real64),allocatable :: a_Psi_val(:)
        real(real64),allocatable :: a_P_val(:)
        real(real64),allocatable :: theta_eq_val(:)
        real(real64),allocatable :: Psi_eq_val(:)
        real(real64),allocatable :: a_E_val(:)
        real(real64),allocatable :: a_v_val(:)
        real(real64),allocatable :: E_eq_val(:)
        real(real64),allocatable :: v_eq_val(:)

        real(real64),allocatable :: theta_ps_val(:) ! computed from other variables

        character(200) :: Name= " "
        integer(int32) :: timestep=0
        integer(int32) :: flushstep=0
        integer(int32) :: ID=0
        real(real64) :: dt
    contains
        procedure, public :: import=> importWaterAbsorption
        procedure, public :: assign=> importWaterAbsorption
        procedure, public :: init=> initWaterAbsorption
        procedure, public :: run => runWaterAbsorption
        procedure, public :: update=> updateWaterAbsorption
        procedure, public :: export=> exportWaterAbsorption
        procedure, public :: display => displayWaterAbsorption
        procedure, public :: bake => bakeWaterAbsorption
        procedure, public :: gnuplot => gnuplotWaterAbsorption
        procedure, public :: updatePermiability => updatePermiabilityWA
        procedure, public :: updateStiffness => updateStiffnessWA
        procedure, public :: UpdateVolume => UpdateVolumeWA

        ! <<<<<<<< IO >>>>>>>>>>
        procedure, public :: save =>saveWaterAbsorption
        procedure, public :: open => openWaterAbsorption
        procedure, public :: link => linkWaterAbsorption
        procedure, public :: result => resultWaterAbsorption
        procedure, public :: remove => removeWaterAbsorption
        ! <<<<<<<< IO >>>>>>>>>>

    end type
contains


!#####################################
subroutine removeWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj

    if(associated(obj%Water)) nullify(obj%Water)
    if(associated(obj%Tissue)) nullify(obj%Tissue)

    if(associated(obj%a_Psi))  nullify(obj%a_Psi)
    if(associated(obj%a_P))  nullify(obj%a_P)
    if(associated(obj%theta_eq))  nullify(obj%theta_eq)
    if(associated(obj%Psi_eq))  nullify(obj%Psi_eq)
    if(associated(obj%a_E))  nullify(obj%a_E)
    if(associated(obj%a_v))  nullify(obj%a_v)
    if(associated(obj%E_eq))  nullify(obj%E_eq)
    if(associated(obj%v_eq))  nullify(obj%v_eq)

    call obj%DiffusionEq%remove()
    call obj%FiniteDeform%remove()

    if(allocated(obj%WaterAbsorbingPower)) deallocate(obj%WaterAbsorbingPower)
    if(allocated(obj%WaterPotential)) deallocate(obj%WaterPotential)
    if(allocated(obj%TurgorPressure)) deallocate(obj%TurgorPressure)
    if(allocated(obj%WaterContent)) deallocate(obj%WaterContent)
    if(allocated(obj%WaterMass)) deallocate(obj%WaterMass)
    if(allocated(obj%Conductivity)) deallocate(obj%Conductivity)
    if(allocated(obj%YoungsModulus)) deallocate(obj%YoungsModulus)
    if(allocated(obj%PoissonsRatio)) deallocate(obj%PoissonsRatio)
    if(allocated(obj%Permiability)) deallocate(obj%Permiability)
    if(allocated(obj%PorePressure)) deallocate(obj%PorePressure)
    ! Parameters(A,B)
    ! A : Element ID 

    if(allocated(obj%a_Psi_val)) deallocate(obj%a_Psi_val)
    if(allocated(obj%a_P_val)) deallocate(obj%a_P_val)
    if(allocated(obj%theta_eq_val)) deallocate(obj%theta_eq_val)
    if(allocated(obj%Psi_eq_val)) deallocate(obj%Psi_eq_val)
    if(allocated(obj%a_E_val)) deallocate(obj%a_E_val)
    if(allocated(obj%a_v_val)) deallocate(obj%a_v_val)
    if(allocated(obj%E_eq_val)) deallocate(obj%E_eq_val)
    if(allocated(obj%v_eq_val)) deallocate(obj%v_eq_val)
    if(allocated(obj%theta_ps_val)) deallocate(obj%theta_ps_val)

    obj%Name=" "
    obj%timestep=0
    obj%flushstep=0
    obj%dt=1.0d0

end subroutine
!#####################################


!#####################################
subroutine resultWaterAbsorption(obj,path,name,step)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),intent(in) :: path, name
    integer(int32),intent(in) :: step
    
    call obj%DiffusionEq%display(OptionalStep=step,Name=trim(path)//"/output/"//trim(adjustl(name)))
    call obj%FiniteDeform%display(OptionalStep=step,Name=trim(path)//"/output/"//trim(adjustl(name)))

end subroutine
!#####################################

!#####################################
subroutine linkWaterAbsorption(obj,Water, Tissue,a_Psi,a_P,theta_eq,Psi_eq,a_E,a_v,E_eq,v_eq)
    class(WaterAbsorption_),intent(inout) :: obj
    type(FEMDomain_),optional,target :: Water, Tissue
    type(MaterialProp_),optional,target :: a_Psi,a_P,theta_eq,Psi_eq,a_E,a_v,E_eq,v_eq


    if(present(Water) )then
        if(associated(obj%Water) ) nullify(obj%Water)
        obj%Water => Water
    endif
    if(present(Tissue) )then
        if(associated(obj%Tissue) ) nullify(obj%Tissue)
        obj%Tissue => Tissue
    endif
    if(present(a_Psi) )then
        if(associated(obj%a_Psi) ) nullify(obj%a_Psi)
        obj%a_Psi => a_Psi
    endif
    if(present(a_P) )then
        if(associated(obj%a_P) ) nullify(obj%a_P)
        obj%a_P => a_P
    endif
    if(present(theta_eq) )then
        if(associated(obj%theta_eq) ) nullify(obj%theta_eq)
        obj%theta_eq => theta_eq
    endif
    if(present(Psi_eq) )then
        if(associated(obj%Psi_eq) ) nullify(obj%Psi_eq)
        obj%Psi_eq => Psi_eq
    endif
    if(present(a_E) )then
        if(associated(obj%a_E) ) nullify(obj%a_E)
        obj%a_E => a_E
    endif
    if(present(a_v) )then
        if(associated(obj%a_v) ) nullify(obj%a_v)
        obj%a_v => a_v
    endif
    if(present(E_eq) )then
        if(associated(obj%E_eq) ) nullify(obj%E_eq)
        obj%E_eq => E_eq
    endif
    if(present(v_eq) )then
        if(associated(obj%v_eq) ) nullify(obj%v_eq)
        obj%v_eq => v_eq
    endif

    if(present(Water) )then
        if(associated(obj%DiffusionEq%FEMDomain) ) nullify(obj%DiffusionEq%FEMDomain)
        obj%DiffusionEq%FEMDomain => Water
    endif
    
    if(present(Tissue) )then
        if(associated(obj%FiniteDeform%FEMDomain) ) nullify(obj%FiniteDeform%FEMDomain)
        obj%FiniteDeform%FEMDomain => Tissue
    endif
end subroutine
!#####################################


!#####################################
subroutine openWaterAbsorption(obj,path,name)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),optional,intent(in)::path
    character(*),optional,intent(in) :: Name
    character(200) ::fname
    integer(int32) :: fh
    logical :: restart=.true.
    type(IO_) :: f
    
    if(.not. present(path) )then
        print *, " exportWaterAbsorption ERROR >> .not. present(path)"
        stop
    endif

    if(present(name) )then
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)) )
	    fname=trim(path)//"/"//trim(adjustl(name)) 
        call f%open("./",trim(path)//"/"//trim(adjustl(name)) ,"/WaterAbsorption.prop")
        call obj%FiniteDeform%open(path=trim(path)//"/"//trim(adjustl(name)),name="FiniteDeform")
        call obj%DiffusionEq%open(path=trim(path)//"/"//trim(adjustl(name)),name="DiffusionEq")
    else
        call execute_command_line("mkdir -p "//trim(path)//"/WaterAbsorption")
        call f%open("./",trim(path)//"/WaterAbsorption","/WaterAbsorption.prop")
        call obj%FiniteDeform%open(path=trim(path)//"/WaterAbsorption",name="FiniteDeform")
        call obj%DiffusionEq%open(path=trim(path)//"/WaterAbsorption",name="DiffusionEq")
        fname=trim(path)//"/WaterAbsorption"
    endif

    
    call openArray(f%fh, obj%WaterAbsorbingPower)
    call openArray(f%fh, obj%WaterPotential)
    call openArray(f%fh, obj%TurgorPressure)
    call openArray(f%fh, obj%WaterContent)
    call openArray(f%fh, obj%WaterMass)
    call openArray(f%fh, obj%Conductivity)
    call openArray(f%fh, obj%YoungsModulus)
    call openArray(f%fh, obj%PoissonsRatio)
    call openArray(f%fh, obj%Permiability)
    call openArray(f%fh, obj%PorePressure)
    call openArray(f%fh, obj%a_Psi_val)
    call openArray(f%fh, obj%a_P_val)
    call openArray(f%fh, obj%theta_eq_val)
    call openArray(f%fh, obj%Psi_eq_val)
    call openArray(f%fh, obj%a_E_val)
    call openArray(f%fh, obj%a_v_val)
    call openArray(f%fh, obj%E_eq_val)
    call openArray(f%fh, obj%v_eq_val)
    call openArray(f%fh, obj%theta_ps_val) ! computed from other )
    read(f%fh, '(A)' ) obj%Name
    read(f%fh,*) obj%timestep
    read(f%fh,*) obj%dt
    call f%close()
		
    
end subroutine
! ################################################


!#####################################
subroutine flushWaterAbsorption(obj,path,name)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),optional,intent(in)::path
    character(*),optional,intent(in) :: Name
    character(200) ::fname
    character(200) ::fs
    integer(int32) :: fh
    logical :: restart=.true.
    type(IO_) :: f
    
    fs=trim(str(obj%flushstep) )
    if(.not. present(path) )then
        print *, " exportWaterAbsorption ERROR >> .not. present(path)"
        stop
    endif

    if(present(name) )then
        fname=trim(path)//"/"//trim(adjustl(name)) 
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)) )
        
        
        if(associated(obj%water) )then
            call obj%Water%save(path = "./"//trim(fname)//"/water", name="water")
        endif

        if(associated(obj%tissue) )then
            call obj%Tissue%save(path = "./"//trim(fname)//"/", name="tissue")
        endif

        call obj%DiffusionEq%export(path="./"//trim(fname)//"/DiffusionEq",restart=restart)
        call obj%FiniteDeform%export(path="./"//trim(fname)//"/FiniteDeform",restart=restart)

        if(associated(obj%a_Psi) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_Psi")
            call obj%a_Psi%export(      path=trim(fname)//"/a_Psi"  ,restart=restart)
        endif
        
        if(associated(obj%a_P) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_P")
            call obj%a_P%export(        path=trim(fname)//"/a_P"    ,restart=restart)
        endif
        
        if(associated(obj%theta_eq) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/theta_eq")
            call obj%theta_eq%export(   path=trim(fname)//"/theta_eq"   ,restart=restart)
        endif
        
        if(associated(obj%Psi_eq) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/Psi_eq")
            call obj%Psi_eq%export(     path=trim(fname)//"/Psi_eq"     ,restart=restart)
        endif
        
        if(associated(obj%a_E) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_E")
            call obj%a_E%export(        path=trim(fname)//"/a_E"    ,restart=restart)
        endif
        
        if(associated(obj%a_v) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_v")
            call obj%a_v%export(        path=trim(fname)//"/a_v"    ,restart=restart)
        endif
        
        if(associated(obj%E_eq) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/E_eq")
            call obj%E_eq%export(       path=trim(fname)//"/E_eq"   ,restart=restart)
        endif
        
        if(associated(obj%v_eq) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/v_eq")
            call obj%v_eq%export(       path=trim(fname)//"/v_eq"   ,restart=restart)
        endif

        call f%open("./",trim(path)//"/"//trim(adjustl(name)) ,"/WaterAbsorption.prop")
        
        write(f%fh,*) obj%WaterAbsorbingPower(:)
        write(f%fh,*) obj%WaterPotential(:)
        write(f%fh,*) obj%TurgorPressure(:)
        write(f%fh,*) obj%WaterContent(:)
        write(f%fh,*) obj%WaterMass(:)
        write(f%fh,*) obj%Conductivity(:)
        write(f%fh,*) obj%YoungsModulus(:)
        write(f%fh,*) obj%PoissonsRatio(:)
        write(f%fh,*) obj%Permiability(:)
        write(f%fh,*) obj%PorePressure(:)
        write(f%fh,*) obj%a_Psi_val(:)
        write(f%fh,*) obj%a_P_val(:)
        write(f%fh,*) obj%theta_eq_val(:)
        write(f%fh,*) obj%Psi_eq_val(:)
        write(f%fh,*) obj%a_E_val(:)
        write(f%fh,*) obj%a_v_val(:)
        write(f%fh,*) obj%E_eq_val(:)
        write(f%fh,*) obj%v_eq_val(:)
        write(f%fh,*) obj%theta_ps_val(:) ! computed from other variables
        write(f%fh,*) obj%Name
        write(f%fh,*) obj%timestep
        write(f%fh,*) obj%dt
        call f%close()
    else
        call execute_command_line("mkdir -p "//trim(path)//"/WaterAbsorption")
        call f%open("./",trim(path)//"/WaterAbsorption","/WaterAbsorption.prop")
        fname=trim(path)//"/WaterAbsorption"
        write(f%fh,*) obj%WaterAbsorbingPower(:)
        write(f%fh,*) obj%WaterPotential(:)
        write(f%fh,*) obj%TurgorPressure(:)
        write(f%fh,*) obj%WaterContent(:)
        write(f%fh,*) obj%WaterMass(:)
        write(f%fh,*) obj%Conductivity(:)
        write(f%fh,*) obj%YoungsModulus(:)
        write(f%fh,*) obj%PoissonsRatio(:)
        write(f%fh,*) obj%Permiability(:)
        write(f%fh,*) obj%PorePressure(:)
        write(f%fh,*) obj%a_Psi_val(:)
        write(f%fh,*) obj%a_P_val(:)
        write(f%fh,*) obj%theta_eq_val(:)
        write(f%fh,*) obj%Psi_eq_val(:)
        write(f%fh,*) obj%a_E_val(:)
        write(f%fh,*) obj%a_v_val(:)
        write(f%fh,*) obj%E_eq_val(:)
        write(f%fh,*) obj%v_eq_val(:)
        write(f%fh,*) obj%theta_ps_val(:) ! computed from other variables
        write(f%fh,*) obj%Name
        write(f%fh,*) obj%timestep
        write(f%fh,*) obj%dt
        call f%close()
        
        if(associated(obj%water) )then
            call obj%Water%save(path = "./"//trim(fname)//"/water", name="water")
        endif

        if(associated(obj%tissue) )then
            call obj%Tissue%save(path = "./"//trim(fname)//"/", name="tissue")
        endif

        call obj%DiffusionEq%export(path="./"//trim(fname)//"/DiffusionEq",restart=restart)
        call obj%FiniteDeform%export(path="./"//trim(fname)//"/FiniteDeform",restart=restart)

        if(associated(obj%a_Psi) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_Psi")
            call obj%a_Psi%export(      path=trim(fname)//"/a_Psi"  ,restart=restart)
        endif
        
        if(associated(obj%a_P) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_P")
            call obj%a_P%export(        path=trim(fname)//"/a_P"    ,restart=restart)
        endif
        
        if(associated(obj%theta_eq) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/theta_eq")
            call obj%theta_eq%export(   path=trim(fname)//"/theta_eq"   ,restart=restart)
        endif
        
        if(associated(obj%Psi_eq) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/Psi_eq")
            call obj%Psi_eq%export(     path=trim(fname)//"/Psi_eq"     ,restart=restart)
        endif
        
        if(associated(obj%a_E) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_E")
            call obj%a_E%export(        path=trim(fname)//"/a_E"    ,restart=restart)
        endif
        
        if(associated(obj%a_v) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_v")
            call obj%a_v%export(        path=trim(fname)//"/a_v"    ,restart=restart)
        endif
        
        if(associated(obj%E_eq) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/E_eq")
            call obj%E_eq%export(       path=trim(fname)//"/E_eq"   ,restart=restart)
        endif
        
        if(associated(obj%v_eq) )then
            call execute_command_line("mkdir -p ./"//trim(fname)//"/v_eq")
            call obj%v_eq%export(       path=trim(fname)//"/v_eq"   ,restart=restart)
        endif

    endif
		
    
end subroutine
! ################################################



!#####################################
subroutine saveWaterAbsorption(obj,path,name)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),optional,intent(in)::path
    character(*),optional,intent(in) :: Name
    character(200) ::fname
    integer(int32) :: fh
    logical :: restart=.true.
    type(IO_) :: f
    
    if(.not. present(path) )then
        print *, " exportWaterAbsorption ERROR >> .not. present(path)"
        stop
    endif

    if(present(name) )then
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)) )
	    fname=trim(path)//"/"//trim(adjustl(name)) 
        call f%open("./",trim(path)//"/"//trim(adjustl(name)) ,"/WaterAbsorption.prop")
        call obj%FiniteDeform%save(path=trim(path)//"/"//trim(adjustl(name)),name="FiniteDeform")
        call obj%DiffusionEq%save(path=trim(path)//"/"//trim(adjustl(name)),name="DiffusionEq")
        
    else
        call execute_command_line("mkdir -p "//trim(path)//"/WaterAbsorption")
        call f%open("./",trim(path)//"/WaterAbsorption","/WaterAbsorption.prop")
        call obj%FiniteDeform%save(path=trim(path)//"/WaterAbsorption",name="FiniteDeform")
        call obj%DiffusionEq%save(path=trim(path)//"/WaterAbsorption",name="DiffusionEq")
        fname=trim(path)//"/WaterAbsorption"
    endif

    
    call WriteArray(f%fh, obj%WaterAbsorbingPower)
    call WriteArray(f%fh, obj%WaterPotential)
    call WriteArray(f%fh, obj%TurgorPressure)
    call WriteArray(f%fh, obj%WaterContent)
    call WriteArray(f%fh, obj%WaterMass)
    call WriteArray(f%fh, obj%Conductivity)
    call WriteArray(f%fh, obj%YoungsModulus)
    call WriteArray(f%fh, obj%PoissonsRatio)
    call WriteArray(f%fh, obj%Permiability)
    call WriteArray(f%fh, obj%PorePressure)
    call WriteArray(f%fh, obj%a_Psi_val)
    call WriteArray(f%fh, obj%a_P_val)
    call WriteArray(f%fh, obj%theta_eq_val)
    call WriteArray(f%fh, obj%Psi_eq_val)
    call WriteArray(f%fh, obj%a_E_val)
    call WriteArray(f%fh, obj%a_v_val)
    call WriteArray(f%fh, obj%E_eq_val)
    call WriteArray(f%fh, obj%v_eq_val)
    call WriteArray(f%fh, obj%theta_ps_val) ! computed from other )
    write(f%fh,*) obj%Name
    write(f%fh,*) obj%timestep
    write(f%fh,*) obj%dt
    call f%close()
		
    
end subroutine
! ################################################



! ################################################
subroutine importWaterAbsorption(obj,Water,Tissue,a_Psi,a_P,theta_eq,theta_ps,&
    Psi_eq,a_E,a_v,E_eq,v_eq)
    class(WaterAbsorption_),intent(inout) :: obj
    type(FEMDomain_),target,optional,intent(inout) :: Water,Tissue

    type(MaterialProp_),target,optional,intent(in):: a_Psi
    type(MaterialProp_),target,optional,intent(in):: a_P
    type(MaterialProp_),target,optional,intent(in):: theta_eq
    type(MaterialProp_),target,optional,intent(in):: theta_ps
    type(MaterialProp_),target,optional,intent(in):: Psi_eq
    type(MaterialProp_),target,optional,intent(in):: a_E
    type(MaterialProp_),target,optional,intent(in):: a_v
    type(MaterialProp_),target,optional,intent(in):: E_eq
    type(MaterialProp_),target,optional,intent(in):: v_eq

    if(associated(obj%DiffusionEq%FEMDomain) )then
        nullify(obj%DiffusionEq%FEMDomain)
    endif

    if(associated(obj%FiniteDeform%FEMDomain) )then
        nullify(obj%FiniteDeform%FEMDomain)
    endif
    if(present(water) )then
        obj%DiffusionEq%FEMDomain => Water
        obj%water => water
        Water%SolverType  = "DiffusionEq_"
    endif
    if(present(tissue))then
        obj%FiniteDeform%FEMDomain => Tissue
        Tissue%SolverType = "FiniteDeform_" 
        obj%Tissue => Tissue
    endif

    
    print *, "[ImportFile]>"

    if(present(a_Psi) )then
        if(associated(obj%a_Psi) )then
            nullify(obj%a_Psi)
        endif
        obj%a_Psi => a_Psi
    endif
    
    if(present(a_P) )then
        if(associated(obj%a_P) )then
            nullify(obj%a_P)
        endif
        obj%a_P => a_P
    endif

    if(present(theta_eq) )then
        if(associated(obj%theta_eq) )then
            nullify(obj%theta_eq)
        endif
        obj%theta_eq => theta_eq
    endif
    if(present(Psi_eq) )then
        if(associated(obj%Psi_eq) )then
            nullify(obj%Psi_eq)
        endif
        obj%Psi_eq => Psi_eq
    endif
    if(present(a_E) )then
        if(associated(obj%a_E) )then
            nullify(obj%a_E)
        endif
        obj%a_E => a_E
    endif
    if(present(a_v) )then
        if(associated(obj%a_v) )then
            nullify(obj%a_v)
        endif
        obj%a_v => a_v
    endif

    if(present(E_eq) )then
        if(associated(obj%E_eq) )then
            nullify(obj%E_eq)
        endif
        obj%E_eq => E_eq
    endif

    if(present(v_eq) )then
        if(associated(obj%v_eq) )then
            nullify(obj%v_eq)
        endif
        obj%v_eq => v_eq
    endif


end subroutine importWaterAbsorption
!#####################################

!#####################################
subroutine runWaterAbsorption(obj,timestep,dt,SolverType,onlyInit,Only1st,Display,nr_tol,ReducedIntegration,&
    infinitesimal,interval,Name,restart,ID)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),intent(in) :: SolverType
    character(*),optional,intent(in) :: Name
    integer(int32) :: i,n,interv,coun
    logical,optional,intent(in) :: onlyInit,Only1st,Display,&
    ReducedIntegration,infinitesimal,restart
    real(real64) ,optional,intent(in) :: nr_tol
    integer(int32),intent(in) :: timestep
    integer(int32),optional,intent(in) :: interval,ID
    real(real64),optional,intent(in) :: dt

    if( present(restart) )then
        if(restart .eqv. .true. )then
            do i=1,timestep
                call obj%update(SolverType,Display=.true.,step=i,nr_tol=nr_tol,restart=.true.)    
                print *, "Timestep :: ",i,"/",timestep   
            enddo
            return
        endif
    endif
    if(present(Name) )then
        obj%water%FileName=Name
        obj%water%Name=Name
        obj%tissue%FileName=Name
        obj%tissue%Name=Name
    endif
    interv=input(default=0,option=interval)
    if(present(ReducedIntegration) )then
        obj%FiniteDeform%ReducedIntegration = ReducedIntegration
    endif
        
    obj%dt=input(default=1.0d0,option=dt)
    !obj%timestep=timestep
    
    obj%DiffusionEq%dt  = obj%dt
    obj%FiniteDeform%dt = obj%dt

    print *, "[ImportFile]>>>[Initialize]>"
    
    call obj%init(SolverType,Display=Display,nr_tol=nr_tol,infinitesimal=infinitesimal)
    print *, "[ImportFile]>>>[Initialize]>>>[1st Step]>"
    if(present(onlyInit) )then
        return
    endif
    ! Repeat over time
    coun=0
    do i=2,timestep
        
        if(coun==interv)then
            coun=0
            call obj%update(SolverType,Display=Display,step=i,nr_tol=nr_tol)
        else
            coun=coun+1
            call obj%update(SolverType,Display=.false.,step=i,nr_tol=nr_tol)
        endif    
        print *, "Timestep :: ",i,"/",timestep   
        
        !n=1
        !call showArray(obj%FiniteDeform%DeformVecGloTot,&
        !Name="test"//trim(adjustl(fstring(n))//".txt")  ) 
        
        if(present(only1st) )then
            return
        endif
    enddo


end subroutine runWaterAbsorption
!#####################################

!#####################################
subroutine initWaterAbsorption(obj,SolverType,Display,nr_tol,infinitesimal)
    class(WaterAbsorption_),intent(inout) :: obj
    type(Term_)             :: term
    character(*),intent(in) :: SolverType
    logical,optional,intent(in) :: Display,infinitesimal
    real(real64) ,optional,intent(in) :: nr_tol
    integer(int32) :: n

    call term%init()
    obj%volume = obj%FiniteDeform%getVolume()

    !open(113,file=trim(obj%tissue%name)//"x_length.txt",status="replace")
    !open(114,file=trim(obj%tissue%name)//"y_length.txt",status="replace")
    !open(115,file=trim(obj%tissue%name)//"z_length.txt",status="replace")
    call obj%updatePermiability()
    ! ###### Diffusion Part ###################
    call obj%DiffusionEq%Setup()
    call obj%DiffusionEq%Solve(SolverType=SolverType)
    if(present(Display) )then
        if(Display .eqv. .true.)then
            call DisplayDiffusionEq(obj%DiffusionEq,&
            DisplayMode=term%Gmsh,OptionalStep=1)
        endif
    endif

    ! ###### Diffusion Part ###################   
    print *, "[ImportFile]>>>[Initialize]>>>[1st Step]>> water >>"
    
    ! debug
    ! only diffusion


    ! ###### Finite deformation part #############################   
    !call obj%updateStiffness()
    !call obj%updateStiffness()
    !call obj%FiniteDeform%import(YoungsModulus=obj%YoungsModulus)
    !call obj%FiniteDeform%import(PoissonsRatio=obj%PoissonsRatio)
    !call obj%FiniteDeform%import(PorePressure=obj%PorePressure)
    ! ###### Finite deformation part #############################   
    if(present(infinitesimal) )then
        obj%FiniteDeform%infinitesimal = infinitesimal
    endif
    call obj%FiniteDeform%DivideBC()
    call obj%FiniteDeform%Solve(SolverType=SolverType,nr_tol=nr_tol)  
    call DisplayReactionForce(obj%FiniteDeform,obj%ID)
    if(present(Display) )then
        if(Display .eqv. .true.)then
            call DisplayDeformStress(obj%FiniteDeform,&
                DisplayMode="gmsh",OptionalStep=1)  
            n=obj%FiniteDeform%step
            !call showArray(obj%FiniteDeform%DeformVecGloTot,&
            !Name="test"//trim(adjustl(fstring(n))//".txt")  ) 
            
        endif
    endif
    call obj%UpdateVolume()

    ! update mesh of Diffusion analysis added at 20200827
    obj%DiffusionEq%FEMDomain%Mesh%NodCoord = obj%FiniteDeform%FEMDomain%Mesh%NodCoord
    ! ###### Finite deformation part #############################
    print *, "[ImportFile]>>>[Initialize]>>>[1st Step]>>>>["
    
end subroutine initWaterAbsorption
!#####################################



!#####################################
subroutine updateWaterAbsorption(obj,SolverType,Display,step,nr_tol,restart)
    class(WaterAbsorption_),intent(inout) :: obj
    character(*),intent(in) :: SolverType
    logical,optional,intent(in) :: Display,restart
    integer(int32),optional,intent(in) :: step
    type(Term_)             :: term
    real(real64),optional,intent(in) :: nr_tol
    integer(int32) :: i,n,m
    
    call term%init()
    obj%volume = obj%FiniteDeform%getVolume()



    obj%DiffusionEq%FEMDomain%name="DiffusionEq"
    obj%FiniteDeform%FEMDomain%name="FiniteDeform"
    obj%DiffusionEq%FEMDomain%filename="DiffusionEq"
    obj%FiniteDeform%FEMDomain%filename="FiniteDeform"
    ! ###### update DIffusion parameters ############
    call obj%updatePermiability()
    call obj%DiffusionEq%import(Permiability=obj%Permiability)
    !do i=1,size(obj%water%mesh%nodCoord,1)
    !    write(1234,*) obj%water%mesh%nodCoord(i,1),obj%DiffusionEq%dt*dble(step), obj%DiffusionEq%UnknownVec(i)
    !enddo
    !write(1234,*) "  "
    ! ###### Update Diffusion Field over timesteps ###################
    call obj%DiffusionEq%Update()
    call obj%DiffusionEq%Solve(SolverType=SolverType,restart=restart)
    obj%DiffusionEq%step=step
    if(present(Display) )then
        if(Display .eqv. .true.)then

            call DisplayDiffusionEq(obj%DiffusionEq,&
            DisplayMode=term%Gmsh,OptionalStep=step,name="water")
        endif
    endif
    ! ###### Update Diffusion Field over timesteps ###################

    ! only diffusion debug
    call obj%updateStiffness()
    ! ###### update DIffusion parameters ############
    call obj%FiniteDeform%import(YoungsModulus=obj%YoungsModulus)
    call obj%FiniteDeform%import(PoissonsRatio=obj%PoissonsRatio)
    call obj%FiniteDeform%import(PorePressure=obj%PorePressure)
    ! ###### Update Finite Deformation over timesteps ###################
    obj%FiniteDeform%step=step
    
    call obj%FiniteDeform%UpdateInitConfig()
    call obj%FiniteDeform%UpdateBC()
    call obj%FiniteDeform%Solve(SolverType=SolverType,nr_tol=nr_tol,restart=restart) 
    

    ! update mesh of Diffusion analysis added at 20200827
    obj%DiffusionEq%FEMDomain%Mesh%NodCoord = obj%FiniteDeform%FEMDomain%Mesh%NodCoord

    call DisplayReactionForce(obj%FiniteDeform,obj%ID)
    print *, "Timestep ",step
    if(present(Display) )then
        if(Display.eqv. .true.)then
            call DisplayDeformStress(obj%FiniteDeform,&
                DisplayMode="gmsh",OptionalStep=step,name="tissue")   
            
            call obj%export(displacement=.true.)
        endif
    endif

    ! ###### Update Finite Deformation over timesteps ###################

end subroutine updateWaterAbsorption
!#####################################


subroutine updatePermiabilityWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    integer(int32) :: i,j,n,m
    real(real64) :: k_0, a_hat, val(2),x
    n=size(obj%a_Psi_val)

    if(.not. allocated(obj%Permiability) )then
        allocate(obj%Permiability(n) )
    endif

    if(.not. allocated(obj%WaterContent) )then
        allocate(obj%WaterContent(n) )
        allocate(obj%WaterMass(n) )
        obj%WaterContent(:) = 0.0d0
        obj%WaterMass(:) = 0.0d0
    endif


    if(allocated(obj%DiffusionEq%UnknownValue) )then
        if(size(obj%DiffusionEq%UnknownValue,1)==n)then
            m=size(obj%DiffusionEq%UnknownValue,2)
            do i=1,n
                obj%WaterContent(i) = 0.0d0
                do j=1,m
                    ! get avarage value of wate content theta
                    obj%WaterContent(i) =obj%WaterContent(i)+ obj%DiffusionEq%UnknownValue(i,j)/dble(m)
                enddo
            enddo
        endif
    endif

    ! determine permiability for each element
    do i=1,n
        k_0=obj%Water%MaterialProp%matPara(obj%Water%Mesh%ElemMat(i),1 )
        val(1)=obj%a_Psi_val(i)
        val(2)=obj%a_Psi_val(i) - abs(obj%a_P_val(i))
        ! Hebiside step function
        !if(obj%WaterContent(i) <=obj%theta_ps_val(i) )then
        !    a_hat = val(1)
        !else
        !    a_hat=val(2)
        !endif
        ! use normalization
        x = 2.0d0 * (obj%WaterContent(i) - 0.50d0 )*100.0d0
        a_hat = obj%a_Psi_val(i) - 1.0d0 / (1.0d0 + exp(- x ) ) * abs(obj%a_P_val(i))
        !write(220,*) obj%WaterContent(i), a_hat*k_0
        obj%Permiability(i) = k_0 * a_hat
    enddo

    !write(100,*) obj%Permiability(:)
end subroutine updatePermiabilityWA
!#####################################


!#####################################
subroutine updateStiffnessWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    integer(int32) :: i,j,n
    real(real64) :: a_E, E, E_eq, theta,theta_eq,a_P,theta_ps
    real(real64) :: a_v, v, v_eq,p,val(2)
    n=size(obj%water%mesh%elemNod,1)
    if(.not. allocated(obj%YoungsModulus) )then
        allocate(obj%YoungsModulus(n) )
    endif

    if(.not. allocated(obj%PoissonsRatio) )then
        allocate(obj%PoissonsRatio(n) )
    endif

    if(.not. allocated(obj%PorePressure) )then
        allocate(obj%PorePressure(n) )
        obj%PorePressure=0.0d0
    endif

    ! update these parameters considering volumetric water content
    do i=1,n
        ! import data for each element
        a_E = obj%a_E_val(i)
        a_v = obj%a_v_val(i)
        E_eq = obj%E_eq_val(i)
        v_eq = obj%v_eq_val(i)
        theta = obj%WaterContent(i)
        theta_eq = obj%theta_eq_val(i)
        theta_ps = obj%theta_ps_val(i)
        a_P = obj%a_P_val(i)

        ! compute parameters
        if(a_E > 0)then
            a_E= - abs(a_E)  
        endif

        if( theta > 1.0d0)then
            theta = 1.0d0
        endif
        E = a_E * (theta - theta_eq) + E_eq
        v = a_v * (theta - theta_eq) + v_eq
        if(theta > theta_ps)then
            p=a_P*(theta - theta_ps)
        else
            p=0.0d0
        endif

        ! export parameters
        obj%YoungsModulus(i)= E
        obj%PoissonsRatio(i)= v
        obj%PorePressure(i) = p !- obj%PorePressure(i)
    enddo
    print *,"maxval(obj%a_P_val(:))",maxval(obj%a_P_val(:)),minval(obj%a_P_val(:))
    print *,"maxval(obj%WaterContent(:))",maxval(obj%WaterContent(:)),minval(obj%WaterContent(:))
    print *,"maxval(obj%theta_ps_val(:))",maxval(obj%theta_ps_val(:)),minval(obj%theta_ps_val(:))
    print *, "maxval(obj%PorePressure(:))",maxval(obj%PorePressure(:)),minval(obj%PorePressure(:))
    call obj%FiniteDeform%import( obj%YoungsModulus, obj%PoissonsRatio, obj%PorePressure )

end subroutine updateStiffnessWA
!#####################################


!#####################################
subroutine exportWaterAbsorption(obj,OptionalFileFormat,OptionalProjectName,FileHandle,&
    SolverType,MeshDimension,FileName,Name,regacy,with, displacement,restart,path)
    class(WaterAbsorption_),intent(inout) :: obj
    class(FEMDomain_),optional,intent(inout)::with
    character(*),optional,intent(in)::OptionalFileFormat,path
    character(*),optional,intent(in)::OptionalProjectName,SolverType,FileName
	character*4::FileFormat
	character(*),optional,intent(in) :: Name
	logical,optional,intent(in) :: regacy,displacement,restart
    character*200::ProjectName
	character*200 ::iFileName,fname
    integer(int32),allocatable::IntMat(:,:)
    real(real64),allocatable::RealMat(:,:)
    integer(int32),optional,intent(in)::FileHandle,MeshDimension
    integer(int32) :: fh,i,j,k,NumOfDomain,n,m,DimNum,GpNum,nn,fh_
    character*70 Msg
    type(IO_) :: f
    
    if(present(restart) )then
        if(.not. present(path) )then
            print *, " exportWaterAbsorption ERROR >> .not. present(path)"
            stop
        endif

        if(present(name) )then
            call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)) )
		    call f%open("./",trim(path)//"/"//trim(adjustl(name)) ,"/WaterAbsorption.prop")
            fname=trim(path)//"/"//trim(adjustl(name)) 
            write(f%fh,*) obj%WaterAbsorbingPower(:)
            write(f%fh,*) obj%WaterPotential(:)
            write(f%fh,*) obj%TurgorPressure(:)
            write(f%fh,*) obj%WaterContent(:)
            write(f%fh,*) obj%Conductivity(:)
            write(f%fh,*) obj%YoungsModulus(:)
            write(f%fh,*) obj%PoissonsRatio(:)
            write(f%fh,*) obj%Permiability(:)
            write(f%fh,*) obj%PorePressure(:)
            write(f%fh,*) obj%a_Psi_val(:)
            write(f%fh,*) obj%a_P_val(:)
            write(f%fh,*) obj%theta_eq_val(:)
            write(f%fh,*) obj%Psi_eq_val(:)
            write(f%fh,*) obj%a_E_val(:)
            write(f%fh,*) obj%a_v_val(:)
            write(f%fh,*) obj%E_eq_val(:)
            write(f%fh,*) obj%v_eq_val(:)
            write(f%fh,*) obj%theta_ps_val(:) ! computed from other variables
            write(f%fh,*) obj%Name
            write(f%fh,*) obj%timestep
            write(f%fh,*) obj%dt
            call f%close()


            call execute_command_line("mkdir -p ./"//trim(fname)//"/FEMDomain")
            call f%open("./",trim(fname),"/FEMDomain/Water.prop")
            call obj%Water%export(path = "./"//trim(fname)//"/FEMDomain", restart=restart)
            call f%close()
            
            call f%open("./",trim(fname),"/FEMDomain/Tissue.prop")
            call obj%Tissue%export(path = "./"//trim(fname)//"/FEMDomain", restart=restart)
            call f%close()

            call execute_command_line("mkdir -p ./"//trim(fname)//"/DiffusionEq")
            call f%open("./",trim(fname),"/DiffusionEq/DiffusionEq.prop")
            call obj%DiffusionEq%export(path="./"//trim(fname)//"/DiffusionEq",restart=restart)
            call f%close()
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/FiniteDeform")
            call f%open("./",trim(fname),"/FiniteDeform/FiniteDeform.prop")
            call obj%FiniteDeform%export(path="./"//trim(fname)//"/FiniteDeform",restart=restart)
            call f%close()

            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_Psi")
            call obj%a_Psi%export(      path=trim(fname)//"/a_Psi"  ,restart=restart)
            

            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_P")
            call obj%a_P%export(        path=trim(fname)//"/a_P"    ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/theta_eq")
            call obj%theta_eq%export(   path=trim(fname)//"/theta_eq"   ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/Psi_eq")
            call obj%Psi_eq%export(     path=trim(fname)//"/Psi_eq"     ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_E")
            call obj%a_E%export(        path=trim(fname)//"/a_E"    ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_v")
            call obj%a_v%export(        path=trim(fname)//"/a_v"    ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/E_eq")
            call obj%E_eq%export(       path=trim(fname)//"/E_eq"   ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/v_eq")
            call obj%v_eq%export(       path=trim(fname)//"/v_eq"   ,restart=restart)
        
        else
            call execute_command_line("mkdir -p "//trim(path)//"/WaterAbsorption")
            call f%open("./",trim(path)//"/WaterAbsorption","/WaterAbsorption.prop")
            fname=trim(path)//"/WaterAbsorption"
            write(f%fh,*) obj%WaterAbsorbingPower(:)
            write(f%fh,*) obj%WaterPotential(:)
            write(f%fh,*) obj%TurgorPressure(:)
            write(f%fh,*) obj%WaterContent(:)
            write(f%fh,*) obj%Conductivity(:)
            write(f%fh,*) obj%YoungsModulus(:)
            write(f%fh,*) obj%PoissonsRatio(:)
            write(f%fh,*) obj%Permiability(:)
            write(f%fh,*) obj%PorePressure(:)
            write(f%fh,*) obj%a_Psi_val(:)
            write(f%fh,*) obj%a_P_val(:)
            write(f%fh,*) obj%theta_eq_val(:)
            write(f%fh,*) obj%Psi_eq_val(:)
            write(f%fh,*) obj%a_E_val(:)
            write(f%fh,*) obj%a_v_val(:)
            write(f%fh,*) obj%E_eq_val(:)
            write(f%fh,*) obj%v_eq_val(:)
            write(f%fh,*) obj%theta_ps_val(:) ! computed from other variables
            write(f%fh,*) obj%Name
            write(f%fh,*) obj%timestep
            write(f%fh,*) obj%dt
            call f%close()
    
    
            call execute_command_line("mkdir -p ./"//trim(fname)//"/FEMDomain")
            call f%open("./",trim(fname),"/FEMDomain/Water.prop")
            call obj%Water%export(path = "./"//trim(fname)//"/FEMDomain", restart=restart)
            call f%close()
            
            call f%open("./",trim(fname),"/FEMDomain/Tissue.prop")
            call obj%Tissue%export(path = "./"//trim(fname)//"/FEMDomain", restart=restart)
            call f%close()
    
            call execute_command_line("mkdir -p ./"//trim(fname)//"/DiffusionEq")
            call f%open("./",trim(fname),"/DiffusionEq/DiffusionEq.prop")
            call obj%DiffusionEq%export(path="./"//trim(fname)//"/DiffusionEq",restart=restart)
            call f%close()
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/FiniteDeform")
            call f%open("./",trim(fname),"/FiniteDeform/FiniteDeform.prop")
            call obj%FiniteDeform%export(path="./"//trim(fname)//"/FiniteDeform",restart=restart)
            call f%close()
    
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_Psi")
            call obj%a_Psi%export(      path=trim(fname)//"/a_Psi"  ,restart=restart)
            
    
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_P")
            call obj%a_P%export(        path=trim(fname)//"/a_P"    ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/theta_eq")
            call obj%theta_eq%export(   path=trim(fname)//"/theta_eq"   ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/Psi_eq")
            call obj%Psi_eq%export(     path=trim(fname)//"/Psi_eq"     ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_E")
            call obj%a_E%export(        path=trim(fname)//"/a_E"    ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/a_v")
            call obj%a_v%export(        path=trim(fname)//"/a_v"    ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/E_eq")
            call obj%E_eq%export(       path=trim(fname)//"/E_eq"   ,restart=restart)
            
            call execute_command_line("mkdir -p ./"//trim(fname)//"/v_eq")
            call obj%v_eq%export(       path=trim(fname)//"/v_eq"   ,restart=restart)
            
        endif
		
        return
    endif

    if(present(displacement) )then
        if(displacement .eqv. .true.)then
            if(present(Name) )then
                open(12,file="x"//trim(adjustl(name)))
                open(13,file="y"//trim(adjustl(name)))
                open(14,file="z"//trim(adjustl(name)))
            else
                open(12,file="x_Disp.txt")
                open(13,file="y_Disp.txt")
                open(14,file="z_Disp.txt")
            endif
            n=size(obj%water%mesh%nodCoord,1)
            do i=1,n
                write(12,*) obj%water%mesh%nodCoord(i,1), obj%tissue%mesh%nodCoord(i,1)-obj%water%mesh%nodCoord(i,1)
                write(13,*) obj%water%mesh%nodCoord(i,2), obj%tissue%mesh%nodCoord(i,2)-obj%water%mesh%nodCoord(i,2)
                write(14,*) obj%water%mesh%nodCoord(i,3), obj%tissue%mesh%nodCoord(i,3)-obj%water%mesh%nodCoord(i,3)
            enddo
            close(12)
            close(13)
            close(14)
        endif
    endif
    ! bug exsits
    !call obj%Tissue%export(SolverType="FiniteDeform",Name=Name)
    !call obj%Water%export(SolverType="DiffusionEq",Name=Name)

end subroutine exportWaterAbsorption
!#####################################

!#####################################
subroutine displayWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj

    ! implement how to display the results.

end subroutine displayWaterAbsorption
!#####################################




!#####################################
subroutine bakeWaterAbsorption(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    integer(int32) :: i,j,k,l,n,m,NumOfMaterial,layer,in_num,NumOfLayer
    real(real64),allocatable :: matPara(:,:),info(:,:)
    integer(int32),allocatable :: key(:)
    type(Rectangle_) :: rect,mrect
    logical :: in_case
    real(real64) :: matparaval,coord(3),x_max(3),x_min(3)

    !call obj%tissue%bake(template="FiniteDeform_")
    !call obj%water%bake(template="DiffusionEq_") 
    
    n=size(obj%water%mesh%ElemNod,1)
    
    ! initialize all parameters
    if(allocated(obj%WaterAbsorbingPower) )then
        deallocate(obj%WaterAbsorbingPower)
    endif
    allocate(obj%WaterAbsorbingPower(n))
    if(allocated(obj%WaterPotential) )then
        deallocate(obj%WaterPotential)
    endif
    allocate(obj%WaterPotential(n))
    if(allocated(obj%TurgorPressure) )then
        deallocate(obj%TurgorPressure)
    endif
    allocate(obj%TurgorPressure(n))
    if(allocated(obj%WaterContent) )then
        deallocate(obj%WaterContent)
    endif
    allocate(obj%WaterContent(n))
    if(allocated(obj%Conductivity) )then
        deallocate(obj%Conductivity)
    endif
    allocate(obj%Conductivity(n))
    if(allocated(obj%YoungsModulus) )then
        deallocate(obj%YoungsModulus)
    endif
    allocate(obj%YoungsModulus(n))
    if(allocated(obj%PoissonsRatio) )then
        deallocate(obj%PoissonsRatio)
    endif
    allocate(obj%PoissonsRatio(n))

    ! initialize parameters
    obj%WaterAbsorbingPower(:) = 0.0d0 ! kPa
    obj%WaterPotential(:) = 0.0d0 ! water
    obj%TurgorPressure(:) = 0.0d0 ! changes with theta
    obj%WaterContent(:) = 1.0d0 ! theta
    obj%Conductivity(:) = 1.0d0 ! changes with theta
    obj%YoungsModulus(:) = 1.0d0 ! E
    obj%PoissonsRatio(:) = 0.30d0 ! v
    if(.not. associated(obj%a_Psi) )then
        print *, "a_Psi is not associated."
        stop 
    endif
    if(.not. associated(obj%a_P) )then
        print *, "a_P is not associated."
        stop 
    endif
    if(.not. associated(obj%theta_eq) )then
        print *, "theta_eq is not associated."
        stop 
    endif
    if(.not. associated(obj%Psi_eq) )then
        print *, "Psi_eq is not associated."
        stop 
    endif
    if(.not. associated(obj%a_E) )then
        print *, "a_E is not associated."
        stop 
    endif
    if(.not. associated(obj%a_v) )then
        print *, "a_v is not associated."
        stop 
    endif
    if(.not. associated(obj%E_eq) )then
        print *, "E_eq is not associated."
        stop 
    endif
    if(.not. associated(obj%v_eq) )then
        print *, "v_eq is not associated."
        stop 
    endif
    

    ! a_Psi
    call obj%a_Psi%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_Psi_val)
    call obj%a_P%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_P_val)
    call obj%theta_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%theta_eq_val)
    call obj%Psi_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%Psi_eq_val)
    call obj%a_E%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_E_val)
    call obj%a_v%getValues(mesh=obj%Tissue%Mesh,Values=obj%a_v_val)
    call obj%E_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%E_eq_val)
    call obj%v_eq%getValues(mesh=obj%Tissue%Mesh,Values=obj%v_eq_val)

    ! a_Psi should be negative values
    n=size(obj%theta_eq_val)
    do i=1,n
        obj%a_Psi_val(i) = - abs(obj%a_Psi_val(i)) 
    enddo


    ! a_Psi should be negative values
    n=size(obj%theta_eq_val)
    do i=1,n
        obj%a_E_val(i) = - abs(obj%a_E_val(i)) 
    enddo


    ! a_Psi should be negative values
    n=size(obj%theta_eq_val)
    do i=1,n
        obj%a_v_val(i) = abs(obj%a_v_val(i)) 
    enddo

    n=size(obj%theta_eq_val)
    if(.not.allocated(obj%theta_ps_val) )then
        allocate(obj%theta_ps_val(n) )
    endif
    
    do i=1,n
        obj%theta_ps_val(i)  =  (-  obj%Psi_eq_val(i) + obj%a_P_val(i) *  obj%theta_eq_val(i) )/obj%a_P_val(i) 
    enddo

    
    !call showarray(mat=obj%WaterAbsorbingPower, Name="test.txt")
     
end subroutine bakeWaterAbsorption
!#####################################

!#####################################
subroutine gnuplotWaterAbsorption(obj,mode,ElemID)
    class(WaterAbsorption_),intent(in) :: obj
    character(*),intent(in) :: mode
    integer(int32),optional,intent(in) :: ElemID
    integer(int32) :: i,j,n,m
    real(real64) :: theta,psi(2)

    n=input(default=1,option=ElemID)
    if(mode=="OWCC" .or. mode=="all")then
        open(20,file="OWCC.txt")
        do i=0,100
            theta=dble(i)*obj%theta_eq_val(n)/100.0d0
            write(20,*) theta, (theta-obj%theta_eq_val(n))*obj%a_Psi_val(n) +obj%Psi_eq_val(n)
        enddo
        write(20,*) " "
        do i=0,100
            theta=dble(i)*obj%theta_eq_val(n)/100.0d0
            psi(1)=0.0d0
            psi(2)=obj%a_P_val(n)*(theta-obj%theta_ps_val(n))
            write(20,*) theta, maxval(psi)
        enddo
        write(20,*) " "
        close(20)
    endif

    if(mode=="YoungsModulus" .or. mode=="all")then
        open(20,file="YoungsModulus.txt")
        do i=0,100
            theta=dble(i)*obj%theta_eq_val(n)/100.0d0
            write(20,*) theta, (theta-obj%theta_eq_val(n))*obj%a_E_val(n) +obj%E_eq_val(n)
        enddo
        close(20)
    endif

    if(mode=="PoissonsRatio" .or. mode=="all")then
        open(20,file="PoissonsRatio.txt")
        do i=0,100
            theta=dble(i)*obj%theta_eq_val(n)/100.0d0
            write(20,*) theta, (theta-obj%theta_eq_val(n))*obj%a_v_val(n) +obj%v_eq_val(n)
        enddo
        close(20)
    endif
end subroutine gnuplotWaterAbsorption
!#####################################


!#####################################
subroutine UpdateVolumeWA(obj)
    class(WaterAbsorption_),intent(inout) :: obj
    real(real64),allocatable :: volume(:)
    real(real64) :: theta_old, theta_new,mw,vol_old, vol_new
    integer(int32) :: i,j,n,numelem

    numelem = size(obj%FiniteDeform%FEMDomain%Mesh%ElemNod,1)
    volume = obj%FiniteDeform%getVolume()
    if(.not. allocated(obj%WaterMass) )then
        allocate(obj%WaterMass(numelem))
    endif

    do i=1,numelem
        theta_old = obj%WaterContent(i)
        vol_old = obj%volume(i)
        mw = theta_old*vol_old
        theta_new = mw/volume(i)
        obj%WaterMass(i) = mw
        obj%WaterContent(i) = theta_new
    enddo
    obj%volume = obj%FiniteDeform%getVolume()
    
end subroutine
!#####################################


end module 
