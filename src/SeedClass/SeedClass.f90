module SeedClass
    use MathClass
    use RandomClass
    use LsystemClass
    use FEMDomainClass
    use PreprocessingClass
    implicit none

    type :: Seed_
        type(FEMDomain_) :: FEMDomain
        integer :: num_of_seed ! num of seed
        real(8) :: mass ! seed mass g/cm^3
        real(8) :: water_content ! seed water_content %
        real(8) :: radius ! seed radius (cm)
        real(8) :: width1,width2,width3 
        real(8) :: location(3) ! seed location (x,y,z)
    contains
        procedure :: init => initSeed 
        procedure :: import => importSeed
        procedure :: createMesh => createMeshSeed
        procedure :: export => exportSeed
        procedure :: show => showSeed
    end type
contains
!########################################################
subroutine initSeed(obj,mass,water_content,radius,location,x,y,z,width1,width2,width3)
    class(Seed_),intent(inout) :: obj
    real(8),optional,intent(in) :: mass,water_content,radius,location(3),x,y,z
    real(8),optional,intent(in) :: width1,width2,width3
    real(8) :: loc(3)
    loc(:)=0.0d0
    if(present(location) )then
        loc(:) = location(:)
    endif 
    obj%num_of_seed = 1
    obj%mass = input(default=1.0d0,option=mass)
    obj%water_content = input(default=12.0d0,option=water_content)
    obj%radius = input(default=0.30d0,option=radius)

    obj%width1 = input(default=obj%radius,option=width1 )
    obj%width2 = input(default=obj%radius,option=width2 )
    obj%width3 = input(default=obj%radius,option=width3 )
    
    obj%location(:) = loc(:)
    obj%location(1) = obj%location(1)+input(default=0.0d0,option=x)
    obj%location(2) = obj%location(2)+input(default=0.0d0,option=y)
    obj%location(3) = obj%location(3)+input(default=0.0d0+obj%radius,option=z)

    call obj%FEMDomain%init(simple=.true.)
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
    integer :: intval
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
    call obj%FEMDomain%resize(x_rate=obj%width1,y_rate=obj%width2,z_rate=obj%width3)
    
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
    integer,optional,intent(in) :: SeedID

    if(.not.present(extention) )then
        ex_format=".geo"
    else
        ex_format=extention
    endif

    if(ex_format==".stl" .or. ex_format=="stl")then
        call obj%FEMDomain%export(OptionalFileFormat="stl",&
            FileName=FileName//"seed"// trim(adjustl(fstring(input(default=1,option=SeedID))))//".stl"  ,&
            MeshDimension=3 )
        return
    endif


    if(ex_format==".geo" .or. ex_format=="geo")then
        open(10,file=FileName)
        write(10,'(A)') "//+"
        write(10,'(A)') 'SetFactory("OpenCASCADE");'
        write(10,*) "Sphere(",input(default=1,option=SeedID),") = {",&
        obj%location(1),",", obj%location(2),",", obj%location(3),",",&
        obj%radius,", -Pi/2, Pi/2, 2*Pi};"
        close(10)
    endif

end subroutine
!########################################################


!########################################################
subroutine showSeed(obj)
    class(Seed_),intent(in) :: obj

    
    

end subroutine
!########################################################

end module SeedClass