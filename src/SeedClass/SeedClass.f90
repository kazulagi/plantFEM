module SeedClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    use RandomClass
    use LsystemClass
    use FEMDomainClass
    use PreprocessingClass
    use sim
    implicit none

    type :: Seed_
        type(FEMDomain_) :: FEMDomain
        type(WaterAbsorption_) :: seedDomain
        type(FEMDomain_) :: water
        type(FEMDomain_) :: tissue
        type(MaterialProp_):: Permiability, YoungsModulus,PoissonRatio
        type(MaterialProp_):: density,Cohesion,phi,psi
        type(MaterialProp_):: a_Psi
        type(MaterialProp_):: a_P
        type(MaterialProp_):: theta_eq
        type(MaterialProp_):: Psi_eq
        type(MaterialProp_):: a_E
        type(MaterialProp_):: a_v
        type(MaterialProp_):: E_eq
        type(MaterialProp_):: v_eq
        type(Boundary_) :: disp_x,disp_y,disp_z
        type(Boundary_) :: traction_x,traction_y,traction_z
        type(Boundary_) :: flux, const 
        
        integer(int32) :: num_of_seed ! num of seed
        real(real64) :: mass ! seed mass g/cm^3
        real(real64) :: water_content ! seed water_content %
        real(real64) :: radius ! seed radius (cm)
        real(real64) :: width1,width2,width3 
        real(real64) :: width1_origin,width2_origin,width3_origin 
        real(real64) :: location(3) ! seed location (x,y,z)
    contains
        procedure :: create  => createSeed
        procedure :: env     => envSeed
        procedure :: material=> materialSeed
        procedure :: grow    => growSeed

        procedure :: init => initSeed 
        procedure :: import => importSeed
        procedure :: createMesh => createMeshSeed
        procedure :: export => exportSeed
        procedure :: show => showSeed

        procedure :: convertMeshType => convertMeshTypeSeed
    end type
contains

!########################################################
subroutine createSeed(obj,x_num,y_num,z_num,x_len,y_len,z_len,YoungsModulus,PoissonRatio,Permiability,&
    a_Psi, a_P, theta_eq, Psi_eq, a_E, a_v, E_eq, v_eq)
    
    
    class(Seed_),intent(inout) :: obj

    real(real64),optional,intent(in) :: Permiability, YoungsModulus, PoissonRatio

    real(real64),optional,intent(in) :: a_Psi, a_P, theta_eq, Psi_eq, &
        a_E, a_v, E_eq, v_eq

    integer(int32),optional,intent(in) :: x_num,y_num,z_num
    integer(int32) :: xnum,ynum,znum
    real(real64),optional,intent(in) :: x_len,y_len,z_len
    real(real64) :: xlen,ylen,zlen
    real(real64) :: val
    xnum = input(default=10,option=x_num)
    ynum = input(default=10,option=y_num)
    znum = input(default=10,option=z_num)

    xlen = input(default= 90.0d0,option=x_len)
    ylen = input(default= 80.0d0,option=y_len)
    zlen = input(default= 70.0d0,option=z_len)

    !  create mesh
    call obj%water%create(Name="water",MeshType="Sphere3D",x_num=xnum,y_num=ynum,x_len=xlen, y_len=ylen,&
        thickness=zlen,division=znum)
    call obj%tissue%copy(obj%water,onlyMesh=.true.)
    call obj%tissue%rename("tissue")


    ! create material
    ! for deformation analysis
    val=input(default = 100.0d0, option=YoungsModulus)
    call obj%YoungsModulus%create(Name="YoungsModulus",ParaValue=val,Layer=1)
    !call YoungsModulus%create(Name="YoungsModulus",x_max=1500.0d0,x_min=0.0d0,y_max=70.0d0,y_min=0.0d0,&
    !z_max=70.0d0,z_min=0.0d0,ParaValue=6000.0d0,Layer=1)
    val=input(default = 0.30d0, option=PoissonRatio)
    call obj%PoissonRatio%create(Name="PoissonRatio",ParaValue=val,Layer=2)
    !call PoissonRatio%create(Name="PoissonRatio",x_max=100.0d0,x_min=0.0d0,y_max=50.0d0,y_min=0.0d0,&
    !z_max=50.0d0,z_min=0.0d0,ParaValue=0.10d0,Layer=2)
    call obj%Density%create(Name="Density",ParaValue=0.00d0,Layer=3)
    call obj%Cohesion%create(Name="Cohesion",ParaValue=100000000.0d0,Layer=4)
    call obj%Phi%create(Name="Psi",ParaValue=0.0d0,Layer=5)
    call obj%psi%create(Name="psi",ParaValue=0.0d0,Layer=6)

    ! for diffusion analysis
    val=input(default = 1.0d0, option=Permiability)
    call obj%Permiability%create(Name="Permiability",ParaValue=val,Layer=1)

    
    val=input(default=20.0d0, option=a_Psi)
    call obj%a_Psi%create(   Name="a_Psi",   Paravalue=val,Layer=1)
    val=input(default=40.0d0, option=a_P)
    call obj%a_P%create(     Name="a_P",     Paravalue=val,Layer=1)
    val=input(default=1.0d0, option=theta_eq)
    call obj%theta_eq%create(Name="theta_eq",Paravalue=val,Layer=1)
    val=input(default=20.0d0, option=Psi_eq)
    call obj%Psi_eq%create(  Name="Psi_eq",  Paravalue=val,Layer=1)
    
    val=input(default=0.0d0, option=a_E)
    call obj%a_E%create(     Name="a_E",     Paravalue=val,Layer=1)
    val=input(default=0.0d0, option=a_v)
    call obj%a_v%create(     Name="a_v",Paravalue=val,Layer=1)
    val=input(default=100.0d0, option=E_eq)
    call obj%E_eq%create(    Name="E_eq",Paravalue=val,Layer=1)
    val=input(default=0.30d0, option=v_eq)
    call obj%v_eq%create(    Name="v_eq",Paravalue=val,Layer=1)


    ! import Materials onto FEMDomains
    call obj%tissue%import(Materials=.true., Material=obj%YoungsModulus)
    call obj%tissue%import(Materials=.true., Material=obj%PoissonRatio)
    call obj%tissue%import(Materials=.true., Material=obj%Density)
    call obj%tissue%import(Materials=.true., Material=obj%Cohesion)
    call obj%tissue%import(Materials=.true., Material=obj%Phi)
    call obj%tissue%import(Materials=.true., Material=obj%Psi)
    
    call obj%water%import(Materials=.true., Material=obj%Permiability)

    call obj%seedDomain%import(a_Psi    = obj%a_Psi)
    call obj%seedDomain%import(a_P      = obj%a_P)
    call obj%seedDomain%import(theta_eq = obj%theta_eq)
    call obj%seedDomain%import(Psi_eq   = obj%Psi_eq)
    call obj%seedDomain%import(a_E      = obj%a_E)
    call obj%seedDomain%import(a_v      = obj%a_v)
    call obj%seedDomain%import(E_eq     = obj%E_eq)
    call obj%seedDomain%import(v_eq     = obj%v_eq)

    ! visualize material
    call obj%YoungsModulus%gmsh(Name="YoungsModulus",Tag="M : YoungsModulus (kPa)")
    call obj%PoissonRatio%gmsh(Name="PoissonRatio",Tag="M : PoissonRatio")
    call obj%Permiability%gmsh(Name="Permiability",Tag="M : Permiability (cm/s)")
    
    call    obj%a_Psi%gmsh(Name="a_Psi   ", Tag="a_Psi   ")  
    call      obj%a_P%gmsh(Name="a_P     ", Tag="a_P     ")
    call obj%theta_eq%gmsh(Name="theta_eq", Tag="theta_eq")     
    call   obj%Psi_eq%gmsh(Name="Psi_eq  ", Tag="Psi_eq  ")   
    call      obj%a_E%gmsh(Name="a_E     ", Tag="a_E     ")
    call      obj%a_v%gmsh(Name="a_v     ", Tag="a_v     ")
    call     obj%E_eq%gmsh(Name="E_eq    ", Tag="E_eq    ") 
    call     obj%v_eq%gmsh(Name="v_eq    ", Tag="v_eq    ") 

    call obj%tissue%show()
    call obj%water%show()
    
end subroutine
!########################################################



!########################################################
subroutine materialSeed(obj,YoungsModulus,PoissonRatio,Permiability,&
    a_Psi, a_P, theta_eq, Psi_eq, a_E, a_v, E_eq, v_eq,x_max,x_min,y_max,y_min,&
    z_max,z_min)

    class(Seed_),intent(inout) :: obj
    real(real64),optional,intent(in) :: Permiability, YoungsModulus, PoissonRatio

    real(real64),optional,intent(in) :: a_Psi, a_P, theta_eq, Psi_eq, &
        a_E, a_v, E_eq, v_eq

    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,&
        z_max,z_min

    ! create material
    ! for deformation analysis
    if(present(YoungsModulus) )then
        call obj%YoungsModulus%create(Name="YoungsModulus",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=YoungsModulus,Layer=1)
    endif
    if(present(PoissonRatio ) )then
        call obj%PoissonRatio%create(Name="PoissonRatio ",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=PoissonRatio ,Layer=2)
    endif
    if(present(Permiability) )then
        call obj%Permiability%create(Name="Permiability",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=Permiability,Layer=1)
    endif


    if(present(a_Psi) )then
        call obj%a_Psi%create(Name="a_Psi",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=a_Psi,Layer=1)
    endif
    
    if(present(a_P) )then
        call obj%a_P%create(Name="a_P",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=a_P,Layer=1)
    endif

    if(present(theta_eq) )then
        call obj%theta_eq%create(Name="theta_eq",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=theta_eq,Layer=1)
    endif

    if(present(Psi_eq) )then
        call obj%Psi_eq%create(Name="Psi_eq",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=Psi_eq,Layer=1)
    endif

    if(present(a_E) )then
        call obj%a_E%create(Name="a_E",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=a_E,Layer=1)
    endif

    if(present(a_v) )then
        call obj%a_v%create(Name="a_v",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=a_v,Layer=1)
    endif

    if(present(E_eq) )then
        call obj%E_eq%create(Name="E_eq",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=E_eq,Layer=1)
    endif

    if(present(v_eq) )then
        call obj%v_eq%create(Name="v_eq",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,ParaValue=v_eq,Layer=1)
    endif
    
    !call obj%Density%create(Name="Density",ParaValue=0.00d0,Layer=3)
    !call obj%Cohesion%create(Name="Cohesion",ParaValue=100000000.0d0,Layer=4)
    !call obj%Phi%create(Name="Psi",ParaValue=0.0d0,Layer=5)
    !call obj%psi%create(Name="psi",ParaValue=0.0d0,Layer=6)

end subroutine



!########################################################
subroutine envSeed(obj,disp_x,disp_y,disp_z,WaterContent,x_max,x_min,y_max,y_min,&
    z_max,z_min)
    class(Seed_),intent(inout) :: obj
    real(real64),optional,intent(in)::disp_x,disp_y,disp_z,WaterContent
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,&
    z_max,z_min



    ! please use Layer (1, 2, 3...).
    if(present(disp_x) )then
        call obj%disp_x%create(Category="Dirichlet",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,BoundValue=disp_x,Layer=1,Name="disp_x")
        call obj%tissue%import(Boundaries=.true., Boundary=obj%disp_x)
        
    endif

    if(present(disp_y) )then
        call obj%disp_y%create(Category="Dirichlet",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,BoundValue=disp_y,Layer=2,Name="disp_y")
        call obj%tissue%import(Boundaries=.true., Boundary=obj%disp_y)
    endif

    if(present(disp_z) )then
        call obj%disp_z%create(Category="Dirichlet",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,BoundValue=disp_z,Layer=3,Name="disp_z")
        call obj%tissue%import(Boundaries=.true., Boundary=obj%disp_z)
        
    endif

    if(present(WaterContent) )then
        call obj%const%create(Category="Dirichlet",x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,&
        z_max=z_max,z_min=z_min,BoundValue=WaterContent,Layer=1,Name="const")
        call obj%water%import(Boundaries=.true., Boundary=obj%const)
    
    endif


end subroutine

!########################################################



!########################################################
subroutine growSeed(obj,timestep,dt,Display,nr_tol,interval)
    
    class(Seed_) ,intent(inout) :: obj
    integer(int32),optional, intent(in) :: timestep,interval
    real(real64),optional, intent(in) :: dt,nr_tol
    logical,optional, intent(in) ::Display
    ! bake data by using templates
    call obj%tissue%bake(template="FiniteDeform_")
    call obj%water%bake(template="DiffusionEq_")    
    
    ! visualize Domains
    call obj%tissue%gmsh(Name="tissue",Tag="Tissue Domain")
    call obj%water%gmsh(Name="water",Tag="Water Domain")
    
    call obj%tissue%export(Name="tissue")
    call obj%water%export(Name="water")

    ! mount data into a seed (An instance of Water-Absorption Solver)
    
    call obj%seedDomain%import(Water=obj%water,Tissue=obj%tissue)
    call obj%seedDomain%bake()
    ! run simula

    call obj%seedDomain%run(timestep=timestep,dt=dt,SolverType="BiCGSTAB",&
        Display=.true.,nr_tol=nr_tol,infinitesimal=.true.,interval=interval)

end subroutine
!########################################################



!########################################################
subroutine initSeed(obj,mass,water_content,radius,location,x,y,z,width1,width2,width3)
    class(Seed_),intent(inout) :: obj
    real(real64),optional,intent(in) :: mass,water_content,radius,location(3),x,y,z
    real(real64),optional,intent(in) :: width1,width2,width3
    real(real64) :: loc(3)
    loc(:)=0.0d0
    if(present(location) )then
        loc(:) = location(:)
    endif 
    obj%num_of_seed = 1
    obj%mass = input(default=1.0d0,option=mass)
    obj%water_content = input(default=12.0d0,option=water_content)
    

    obj%width1 = input(default=obj%radius,option=width1 )
    obj%width2 = input(default=obj%radius,option=width2 )
    obj%width3 = input(default=obj%radius,option=width3 )
    obj%radius = input(default=0.330d0*obj%width1+0.330d0*obj%width2+0.330d0*obj%width3,option=radius)

    obj%location(:) = loc(:)
    obj%location(1) = obj%location(1)+input(default=0.0d0,option=x)
    obj%location(2) = obj%location(2)+input(default=0.0d0,option=y)
    obj%location(3) = obj%location(3)+input(default=0.0d0+obj%radius,option=z)

    call obj%FEMDomain%init(simple=.true.)
    obj%width1_origin = obj%width1
    obj%width2_origin = obj%width2
    obj%width3_origin = obj%width3

end subroutine
!########################################################


!########################################################
subroutine importSeed(obj,FileName)
    class(Seed_),intent(inout) :: obj
    character(*),intent(in) :: FileName
 
end subroutine
!########################################################

!########################################################
subroutine createMeshSeed(obj,FileName,withSTL,ObjType,ElemType)

    class(Seed_),intent(inout):: obj
    type(Mesh_) :: mesh
    character(*),intent(in)    :: FileName,ElemType
    character(*),optional,intent(in) :: ObjType
    character*200   :: meshFileName,meshFileName_m
    character*200   :: command
    character*200   :: strings
    integer(int32) :: intval
    real(real64) :: x_rate,y_rate,z_rate
    logical,optional,intent(in) :: withSTL

    
    meshFileName=FileName//"createMesh_seed.geo"
    meshFileName_m=FileName//"createMesh_seed.mesh"
    call obj%export(FileName=meshFileName)
    command="gmsh "//trim(meshFileName)//" -3 -format mesh" 
    writE(*,'(A)') trim(command)
    call system(command)

    ! modification
    call mesh%import(FileName=trim(meshFileName_m),extention=".mesh",ElemType=ElemType)
    call obj%FEMDomain%importMesh(mesh)
    x_rate=obj%width1/(maxval(obj%FEMDomain%Mesh%NodCoord(:,1) ) - minval(obj%FEMDomain%Mesh%NodCoord(:,1) )  )
    y_rate=obj%width2/(maxval(obj%FEMDomain%Mesh%NodCoord(:,2) ) - minval(obj%FEMDomain%Mesh%NodCoord(:,2) )  )
    z_rate=obj%width3/(maxval(obj%FEMDomain%Mesh%NodCoord(:,3) ) - minval(obj%FEMDomain%Mesh%NodCoord(:,3) )  )
    call obj%FEMDomain%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate)
    
    if(present(withSTL) )then
        if(withSTL .eqv. .true.)then
            call obj%export(FileName=FileName,extention=".stl")
        endif
    endif
end subroutine
!########################################################


!########################################################
subroutine exportSeed(obj,FileName,SeedID,extention)
    class(Seed_),intent(inout) :: obj
    character(*),intent(in) :: FileName
    character(*),optional,intent(in) ::extention
    character(10) :: ex_format 
    integer(int32),optional,intent(in) :: SeedID


    if(.not.present(extention) )then
        ex_format=".geo"
    else
        ex_format=extention
    endif

    if(ex_format==".stl" .or. ex_format=="stl")then
        
        call obj%FEMDomain%export(OptionalFileFormat="stl",&
            FileName=FileName//"seed"// trim(adjustl(fstring(input(default=1,option=SeedID))))//".stl"  ,&
            MeshDimension=3 )
        
        
    elseif(ex_format==".pos" .or. ex_format=="pos")then 
        call obj%FEMDomain%GmshPlotMesh(Name=FileName//"seed"// trim(adjustl(fstring(input(default=1,option=SeedID))))//".pos")
    elseif(ex_format==".geo" .or. ex_format=="geo")then
        open(10,file=FileName)
        write(10,'(A)') "//+"
        write(10,'(A)') 'SetFactory("OpenCASCADE");'
        write(10,*) "Sphere(",input(default=1,option=SeedID),") = {",&
        obj%location(1),",", obj%location(2),",", obj%location(3),",",&
        obj%radius,", -Pi/2, Pi/2, 2*Pi};"
        close(10)
    else
        print *, "ERROR :: export formats .pos, .stl, .geo are available."
    endif

    


    

end subroutine
!########################################################

subroutine convertMeshTypeSeed(obj,Option)
    class(Seed_),intent(inout)::obj
    character(*),intent(in) :: Option

    call obj%FEMDomain%convertMeshType(Option=Option)
end subroutine

!########################################################
subroutine showSeed(obj)
    class(Seed_),intent(in) :: obj

    
    

end subroutine
!########################################################

end module SeedClass