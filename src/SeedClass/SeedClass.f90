module SeedClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    use RandomClass
    use LsystemClass
    use FEMDomainClass
    use PreprocessingClass
    implicit none

    type :: Seed_
        type(FEMDomain_) :: FEMDomain
        integer(int32) :: num_of_seed ! num of seed
        real(real64) :: mass ! seed mass g/cm^3
        real(real64) :: water_content ! seed water_content %
        real(real64) :: radius ! seed radius (cm)
        real(real64) :: width1,width2,width3 
        real(real64) :: width1_origin,width2_origin,width3_origin 
        real(real64) :: location(3) ! seed location (x,y,z)
    contains
        procedure :: init => initSeed 
        procedure :: import => importSeed
        procedure :: createMesh => createMeshSeed
        procedure :: export => exportSeed
        procedure :: show => showSeed

        procedure :: convertMeshType => convertMeshTypeSeed
    end type
contains
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