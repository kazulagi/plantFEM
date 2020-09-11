module FEMDomainClass
    use, intrinsic :: iso_fortran_env
	use MathClass
    use ArrayClass
    use ShapeFunctionClass
    use MeshClass
    use MaterialPropClass
    use BoundaryConditionClass
	use ControlParaeterClass
	use std
	
	implicit none

	type::Meshp_
		type(Mesh_),pointer :: Meshp
	end type


	type::Materialp_
		type(MaterialProp_),pointer :: Materialp
	end type

	type::Boundaryp_
		type(Boundary_),pointer :: Boundaryp
	end type

    type::FEMDomain_
        type(Mesh_)             :: Mesh
        type(MaterialProp_)     :: MaterialProp
        type(Boundary_)         :: Boundary
		type(ControlParameter_) :: ControlPara

		type(Meshp_),allocatable :: Meshes(:)
		type(Materialp_),allocatable :: Materials(:)
		type(Boundaryp_),allocatable :: Boundaries(:)
		type(FEMDomainp_),allocatable :: FEMDomains(:)

        type(ShapeFunction_)    :: ShapeFunction
		real(real64),allocatable :: scalar(:)
		real(real64),allocatable :: vector(:,:)
		real(real64),allocatable :: tensor(:,:,:)
		real(real64) :: RealTime=1.0d0
		integer(int32) :: NumOfDomain=1
        character*200 :: FilePath="None"
        character*200 :: FileName="None"
        character*200 :: Name="None"
        character*9 :: Dtype="None"
		character*200 :: SolverType="None"
		character*200 :: Category1 ="None"
		character*200 :: Category2="None"
		character*200 :: Category3="None"
		integer(int32) :: DomainID=1
		integer(int32) :: timestep=1
		integer(int32) :: NumberOfBoundaries=0
		integer(int32) ::  NumberOfMaterials=0

    contains
		procedure,public :: addNBC => AddNBCFEMDomain 
        procedure,public :: addDBoundCondition => AddDBoundCondition
        procedure,public :: addNBoundCondition => AddNBoundCondition
        procedure,public :: addTBoundCondition => AddTBoundCondition
        procedure,public :: addMaterialID => AddMaterialID
		procedure,public :: assign => ImportFEMDomain
		
		procedure,public :: bake => bakeFEMDomain
		procedure,public :: bakeMaterials => bakeMaterialsFEMDomain
		procedure,public :: bakeDBoundaries => bakeDBoundariesFEMDomain
		procedure,public :: bakeNBoundaries => bakeNBoundariesFEMDomain
		procedure,public :: bakeTBoundaries => bakeTBoundariesFEMDomain
		
		procedure,public :: checkConnectivity => CheckConnedctivityFEMDomain
		procedure,public :: copy => copyFEMDomain
		procedure,public :: convertMeshType => convertMeshTypeFEMDomain
		procedure,public :: contactdetect => contactdetectFEMDomain
		procedure,public :: create => createFEMDomain

        procedure,public :: delete => DeallocateFEMDomain
		procedure,public :: display => displayFEMDomain
		procedure,public :: divide => divideFEMDomain
		procedure,public :: distribute => distributeFEMDomain
		
		procedure,public :: export => ExportFEMDomain
		
		procedure,public :: field => fieldFEMDomain
		
		procedure,public :: gmshPlotMesh => GmshPlotMesh
		procedure,public :: gmsh => GmshPlotMesh
		procedure,public :: gmshPlotContour => GmshPlotContour
		procedure,public :: gmshPlotVector => GmshPlotVector 
        procedure,public :: gmshPlotContour2D => GmshPlotContour2D
        procedure,public :: gnuplotPlotContour  => GnuplotPlotContour   
		procedure,public :: gnuplotExportStress => GnuplotExportStress  
		procedure,public :: getDBCVector => getDBCVectorFEMDomain
		
        procedure,public :: init   => InitializeFEMDomain
		procedure,public :: import => ImportFEMDomain
		procedure,public :: importMesh => ImportMeshFEMDomain
		procedure,public :: importMaterials => ImportMaterialsFEMDomain
		procedure,public :: importBoundaries => ImportBoundariesFEMDomain
        procedure,public :: initDBC => InitDBC
        procedure,public :: initNBC => InitNBC
		procedure,public :: initTBC => InitTBC

		procedure,public :: length => lengthFEMDomain

        procedure,public :: meltingSkelton => MeltingSkeltonFEMDomain
		procedure,public :: move => moveFEMDomain
		procedure,public :: meshing => meshingFEMDomain
		procedure,public :: merge  => MergeFEMDomain

		procedure,public :: open => openFEMDomain

		procedure,public :: removeMaterials => removeMaterialsFEMDomain
		procedure,public :: rotate => rotateFEMDomain
		procedure,public :: removeBoundaries => removeBoundariesFEMDomain
		procedure,public :: rename => renameFEMDomain
		procedure,public :: resize => resizeFEMDomain
		procedure,public :: remove => removeFEMDomain

		procedure,public :: save => saveFEMDomain

        procedure,public :: setDataType => SetDataType
        procedure,public :: setSolver => SetSolver 
        procedure,public :: setName => SetName 
        procedure,public :: setUp      => SetUpFEMDomain
		procedure,public :: setBoundary => setBoundaryFEMDomain
        procedure,public :: setControlPara =>  SetControlParaFEMDomain
		procedure,public :: show => showFEMDomain
		procedure,public :: showRange => showRangeFEMDomain
		procedure,public :: showMaterials => showMaterialsFEMDomain
		procedure,public :: showBoundaries => showBoundariesFEMDomain

		
    end type FEMDomain_

	type:: FEMDomainp_
		type(FEMDomain_),pointer :: FEMDomain
	end type
	
	type,extends(FEMDomain_) :: STFEMDomain_
        type(ShapeFunction_)    :: TimeShapeFunction
        type(Mesh_)             :: TimeMesh
    end type

contains

function lengthFEMDomain(obj) result(length)
	class(FEMDomain_),intent(in) :: obj
	real(real64) :: length(3)

	length(:)=obj%Mesh%length()
end function

! ####################################################################
subroutine openFEMDomain(obj,path,name)

	class(FEMDomain_),intent(inout) :: obj
	character(*),intent(in) :: path
	character(*),optional,intent(in) :: name
	character(200) :: pathi
	type(IO_) :: f
	integer(int32) :: n

	! remove and initialze
	call obj%remove()

	if(present(name) )then
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call system("mkdir -p "//trim(pathi))
		call system("mkdir -p "//trim(pathi)//"/"//trim(adjustl(name)) )
		call obj%Mesh%open(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="Mesh") !implement!
		call obj%MaterialProp%open(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="MaterialProp")!implement!
		call obj%Boundary%open(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="Boundary")!implement!
		call obj%ControlPara%open(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="ControlPara")!implement!
		call obj%ShapeFunction%open(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="ShapeFunction")!implement!

		call f%open(trim(pathi)//"/"//trim(adjustl(name)) ,"/"//"FEMDomain",".prop" )
		write(f%fh,*) obj%RealTime
		write(f%fh,*) obj%NumOfDomain
		write(f%fh, '(A)' ) trim(obj%FilePath)
		write(f%fh, '(A)' ) trim(obj%FileName)
		write(f%fh, '(A)' ) trim(obj%Name)
		write(f%fh, '(A)' ) trim(obj%Dtype)
		write(f%fh, '(A)' ) trim(obj%SolverType)
		write(f%fh, '(A)' ) trim(obj%Category1)
		write(f%fh, '(A)' ) trim(obj%Category2)
		write(f%fh, '(A)' ) trim(obj%Category3)
		write(f%fh,*) obj%timestep, obj%NumberOfBoundaries, obj%NumberOfMaterials
		call f%close()
	else
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call system("mkdir -p "//trim(pathi))
		call system("mkdir -p "//trim(pathi)//"/FEMDomain")
		call obj%Mesh%open(path=trim(pathi)//"/"//"FEMDomain",name="Mesh")
		call obj%MaterialProp%open(path=trim(pathi)//"/"//"FEMDomain",name="MaterialProp")
		call obj%Boundary%open(path=trim(pathi)//"/"//"FEMDomain",name="Boundary")
		call obj%ControlPara%open(path=trim(pathi)//"/"//"FEMDomain",name="ControlPara")
		call obj%ShapeFunction%open(path=trim(pathi)//"/"//"FEMDomain",name="ShapeFunction")

		call f%open(trim(pathi)//"/FEMDomain","/FEMDomain",".prop" )
		write(f%fh,*) obj%RealTime
		write(f%fh,*) obj%NumOfDomain
		write(f%fh, '(A)' ) trim(obj%FilePath)
		write(f%fh, '(A)' ) trim(obj%FileName)
		write(f%fh, '(A)' ) trim(obj%Name)
		write(f%fh, '(A)' ) trim(obj%Dtype)
		write(f%fh, '(A)' ) trim(obj%SolverType)
		write(f%fh, '(A)' ) trim(obj%Category1)
		write(f%fh, '(A)' ) trim(obj%Category2)
		write(f%fh, '(A)' ) trim(obj%Category3)
		write(f%fh,*) obj%timestep, obj%NumberOfBoundaries, obj%NumberOfMaterials
		call f%close()
	endif

end subroutine
! ####################################################################


! ####################################################################
subroutine removeFEMDomain(obj)
	class(FEMDomain_),intent(inout) :: obj

	! remove all objects

	call obj%Mesh%remove()
	call obj%MaterialProp%remove()
	call obj%Boundary%remove()
	call obj%ControlPara%remove()
	call obj%ShapeFunction%remove()

	if(allocated(obj%Meshes))then
		deallocate(obj%Meshes)
	endif
	if(allocated(obj%Materials))then
		deallocate(obj%Materials)
	endif
	if(allocated(obj%Boundaries))then
		deallocate(obj%Boundaries)
	endif
	if(allocated(obj%FEMDomains))then
		deallocate(obj%FEMDomains)
	endif


	if(allocated(obj%scalar) )then
		deallocate(obj%scalar)
	endif
	if(allocated(obj%vector) )then
		deallocate(obj%vector)
	endif
	if(allocated(obj%tensor) )then
		deallocate(obj%tensor)
	endif

	obj%RealTime=1.0d0
	obj%NumOfDomain=1
	obj%FilePath="None"
	obj%FileName="None"
	obj%Name="None"
	obj%Dtype="None"
	obj%SolverType="None"
	obj%Category1 ="None"
	obj%Category2="None"
	obj%Category3="None"
	obj%timestep=1
	obj%NumberOfBoundaries=0
	obj% NumberOfMaterials=0

end subroutine
! ####################################################################


! ####################################################################
subroutine saveFEMDomain(obj,path,name)
	class(FEMDomain_),intent(inout) :: obj
	character(*),intent(in) :: path
	character(*),optional,intent(in) :: name
	character(200) :: pathi
	type(IO_) :: f
	integer(int32) :: n

	if(present(name) )then
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call system("mkdir -p "//trim(pathi))
		call system("mkdir -p "//trim(pathi)//"/"//trim(adjustl(name)) )
		call obj%Mesh%save(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="Mesh")
		call obj%MaterialProp%save(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="MaterialProp")
		call obj%Boundary%save(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="Boundary")
		call obj%ControlPara%save(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="ControlPara")
		call obj%ShapeFunction%save(path=trim(pathi)//"/"//trim(adjustl(name)) ,name="ShapeFunction")

		call f%open(trim(pathi)//"/"//trim(adjustl(name)) ,"/"//"FEMDomain",".prop" )
		write(f%fh,*) obj%RealTime
		write(f%fh,*) obj%NumOfDomain
		write(f%fh, '(A)' ) trim(obj%FilePath)
		write(f%fh, '(A)' ) trim(obj%FileName)
		write(f%fh, '(A)' ) trim(obj%Name)
		write(f%fh, '(A)' ) trim(obj%Dtype)
		write(f%fh, '(A)' ) trim(obj%SolverType)
		write(f%fh, '(A)' ) trim(obj%Category1)
		write(f%fh, '(A)' ) trim(obj%Category2)
		write(f%fh, '(A)' ) trim(obj%Category3)
		write(f%fh,*) obj%timestep, obj%NumberOfBoundaries, obj%NumberOfMaterials
		call f%close()
	else
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call system("mkdir -p "//trim(pathi))
		call system("mkdir -p "//trim(pathi)//"/FEMDomain")
		call obj%Mesh%save(path=trim(pathi)//"/"//"FEMDomain",name="Mesh")
		call obj%MaterialProp%save(path=trim(pathi)//"/"//"FEMDomain",name="MaterialProp")
		call obj%Boundary%save(path=trim(pathi)//"/"//"FEMDomain",name="Boundary")
		call obj%ControlPara%save(path=trim(pathi)//"/"//"FEMDomain",name="ControlPara")
		call obj%ShapeFunction%save(path=trim(pathi)//"/"//"FEMDomain",name="ShapeFunction")

		call f%open(trim(pathi)//"/FEMDomain","/FEMDomain",".prop" )
		write(f%fh,*) obj%RealTime
		write(f%fh,*) obj%NumOfDomain
		write(f%fh, '(A)' ) trim(obj%FilePath)
		write(f%fh, '(A)' ) trim(obj%FileName)
		write(f%fh, '(A)' ) trim(obj%Name)
		write(f%fh, '(A)' ) trim(obj%Dtype)
		write(f%fh, '(A)' ) trim(obj%SolverType)
		write(f%fh, '(A)' ) trim(obj%Category1)
		write(f%fh, '(A)' ) trim(obj%Category2)
		write(f%fh, '(A)' ) trim(obj%Category3)
		write(f%fh,*) obj%timestep, obj%NumberOfBoundaries, obj%NumberOfMaterials
		call f%close()
	endif
end subroutine 

!##################################################
subroutine divideFEMDomain(obj,n) 
	class(FEMDomain_),intent(inout)::obj
    type(Mesh_),allocatable :: meshes(:)
	integer(int32),intent(in) :: n
	integer(int32) :: i
	
	! split obj into n objects
	if(allocated(obj%FEMDomains) )then
		deallocate(obj%FEMDomains)
	endif
	allocate(obj%FEMDomains(n))

	! Greedy algorithm
	if(obj%Mesh%empty() .eqv. .true. )then
		print *, "divideFEMDomain >> ERROR >> No mesh is imported."
		stop
	endif
	
	meshes = obj%mesh%divide(n)

	! import mesh
	do i=1,n
		call obj%FEMDomains(i)%FEMDomain%import(Mesh=meshes(i))
	enddo

end subroutine divideFEMDomain
!##################################################

!##################################################
subroutine distributeFEMDomain(obj,mpid) 
	class(FEMDomain_),intent(inout)::obj
    type(Mesh_),allocatable :: meshes(:)
	type(MPI_),intent(inout) :: mpid
	integer(int32) :: n
	
	n=mpid%petot

	! split obj into n objects
	if(allocated(obj%FEMDomains) )then
		deallocate(obj%FEMDomains)
	endif
	allocate(obj%FEMDomains(n))

	! Greedy algorithm
	if(obj%Mesh%empty() .eqv. .true. )then
		print *, "distributeFEMDomain >> ERROR >> No mesh is imported."
		stop
	endif
	
	meshes = obj%mesh%divide(n)

	! import mesh
	call obj%import(Mesh=meshes(mpid%myrank+1))


end subroutine distributeFEMDomain
!##################################################

!##################################################
subroutine displayFEMDomain(obj,path,name,extention)
	class(FEMDomain_),intent(inout) :: obj
	character(*),intent(in) :: path,name,extention
	integer(int32) :: i,j,n
	open(10,file=trim(path)//trim(adjustl(name))//trim(extention) )
	if( trim(extention) == ".vtk" )then
		write(10,'(A)' ) "# vtk DataFile Version 2.0"
		write(10,'(A)' ) "Cube example"
		write(10,'(A)' ) "ASCII"
		write(10,'(A)' ) "DATASET POLYDATA"
		write(10,'(A)' ,advance="no") "POINTS "
		write(10,'(i10)' ,advance="no")size(obj%mesh%NodCoord,1)
		write(10,'(A)')" float"
		do i=1,size(obj%mesh%NodCoord,1)
			do j=1,size(obj%mesh%NodCoord,2)
				if(j==size(obj%mesh%NodCoord,2))then
					write(10,'(f20.8)' ) obj%mesh%NodCoord(i,j)
				else
					write(10,'(f20.8)', advance="no" ) obj%mesh%NodCoord(i,j)
					write(10,'(A)', advance="no" ) " "
				endif
			enddo
		enddo
		write(10,'(A)',advance="no")" POLYGONS "
		write(10,'(i10)',advance="no") 6*size(obj%mesh%ElemNod,1)
		write(10,'(A)',advance="no") " "
		write(10,'(i10)') size(obj%mesh%ElemNod,1)*5*6
		do i=1,size(obj%mesh%ElemNod,1)
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,4)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,5)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,6)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,7)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,8)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,6)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,5)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,4)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,8)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,7)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,5)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,8)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,4)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,7)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,6)-1
			write(10,'(A)') " "
		enddo
		write(10,'(A)') "CELL_DATA 6"
	elseif(trim(extention) == ".ply")then
		write(10,'(A)')"ply"
		write(10,'(A)')"format ascii 1.0"
		write(10,'(A)',advance="no")"element vertex "
		write(10,'(i10)') size(obj%mesh%NodCoord,1)
		write(10,'(A)')"property float32 x"
		write(10,'(A)')"property float32 y"
		write(10,'(A)')"property float32 z"
		write(10,'(A)')"property uchar red"
		write(10,'(A)')"property uchar green"
		write(10,'(A)')"property uchar blue"
		write(10,'(A)',advance="no")"element face "
		write(10,'(i10)') size(obj%mesh%ElemNod,1)*6
		write(10,'(A)')"property list uint8 int32 vertex_indices"
		write(10,'(A)') "end_header"
		do i=1,size(obj%mesh%NodCoord,1)
			do j=1,size(obj%mesh%NodCoord,2)
				if(j==size(obj%mesh%NodCoord,2))then
					write(10,'(f20.8)', advance="no"  ) obj%mesh%NodCoord(i,j)
					write(10,'(A)', advance="no" ) " "
				else
					write(10,'(f20.8)', advance="no" ) obj%mesh%NodCoord(i,j)
					write(10,'(A)', advance="no" ) " "
				endif
			enddo
			write(10,'(A)', advance="no" ) " "
			write(10,'(i3)',advance="no") int(obj%mesh%NodCoord(i,1)*255.0d0/maxval(obj%mesh%NodCoord(:,1) ))
			write(10,'(A)', advance="no" ) " "
			write(10,'(i3)',advance="no") int(obj%mesh%NodCoord(i,2)*255.0d0/maxval(obj%mesh%NodCoord(:,2) ))
			write(10,'(A)', advance="no" ) " "
			write(10,'(i3)') int(obj%mesh%NodCoord(i,3)*255.0d0/maxval(obj%mesh%NodCoord(:,3) ))
		enddo
		do i=1,size(obj%mesh%ElemNod,1)
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,4)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,5)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,6)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,7)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,8)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,6)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,5)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,4)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,8)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,7)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,5)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,8)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,4)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,7)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") obj%mesh%ElemNod(i,6)-1
			write(10,'(A)') " "
		enddo

	else
		print *, "Invalid extention :: ",trim(extention)
		stop
	endif
	close(10)

end subroutine displayFEMDomain
!##################################################


!##################################################
subroutine fieldFEMDomain(obj,scalar,vector,tensor)
	class(FEMDomain_),intent(inout) :: obj
	real(real64),optional,intent(in) :: scalar(:),vector(:,:),tensor(:,:,:)
	integer(int32) :: i,j,k,n

	! import data >> to obj
	if(present(scalar) )then
		if(size(scalar,1)==0 )then
			print *, "displayFEMDomain :: ERROR :: scalar is not allocated."
			stop 
		endif
		if(allocated(obj%scalar) )then
			deallocate(obj%scalar)
		endif
		i=size(scalar)
		if(obj%mesh%empty() .eqv. .true.)then
			print *, "displayFEMDomain :: ERROR :: element is not imported."
			stop
		endif
		if(i/=size(obj%mesh%ElemNod,1))then
			print *, "displayFEMDomain :: ERROR :: size(scalar/=size(obj%mesh%ElemNod,1)"
			stop 
		endif
		allocate(obj%scalar(i) )
		obj%scalar(:) = scalar(:)
	endif
	
	! import data >> to obj
	if(present(vector) )then
		if(size(vector,1)==0 )then
			print *, "displayFEMDomain :: ERROR :: vector is not allocated."
			stop 
		endif
		if(allocated(obj%vector) )then
			deallocate(obj%vector)
		endif
		i=size(vector,1)
		j=size(vector,2)
		if(obj%mesh%empty() .eqv. .true.)then
			print *, "displayFEMDomain :: ERROR :: element is not imported."
			stop
		endif
		if(i/=size(obj%mesh%ElemNod,1))then
			print *, "displayFEMDomain :: ERROR :: size(vector/=size(obj%mesh%ElemNod,1)"
			stop 
		endif
		allocate(obj%vector(i,j) )
		obj%vector(:,:) = vector(:,:)
	endif
	
	! import data >> to obj
	if(present(tensor) )then
		if(size(tensor,1)==0 )then
			print *, "displayFEMDomain :: ERROR :: tensor is not allocated."
			stop 
		endif
		if(allocated(obj%tensor) )then
			deallocate(obj%tensor)
		endif
		i=size(tensor,1)
		j=size(tensor,2)
		k=size(tensor,3)
		if(obj%mesh%empty() .eqv. .true.)then
			print *, "displayFEMDomain :: ERROR :: element is not imported."
			stop
		endif
		if(i/=size(obj%mesh%ElemNod,1))then
			print *, "displayFEMDomain :: ERROR :: size(tensor/=size(obj%mesh%ElemNod,1)"
			stop 
		endif
		allocate(obj%tensor(i,j,k) )
		obj%tensor(:,:,:) = tensor(:,:,:)
	endif

end subroutine fieldFEMDomain
!##################################################


!##################################################
subroutine DeallocateFEMDomain(obj)
    class(FEMDomain_),intent(inout)::obj

    call DeallocateMesh(obj%Mesh)
    call DeallocateMaterialProp(obj%MaterialProp)
    call DeallocateBoundary(obj%Boundary)
    call DeallocateShapeFunction(obj%ShapeFunction)    

end subroutine DeallocateFEMDomain
!##################################################



! ################################################
subroutine renameFEMDomain(obj,Name)
	class(FEMDomain_),intent(inout) :: obj
	character(*),intent(in) :: Name

	obj%Name = ""
	obj%Name = trim(adjustl(name))

end subroutine renameFEMDomain


!##################################################
subroutine InitializeFEMDomain(obj,Default,FileName,simple)
	class(FEMDomain_),intent(inout)::obj
	character(*),optional,intent(in) :: FileName
    logical,optional,intent(in)::Default,simple

	if(.not. present(FileName) )then
		obj%FileName="noName"
	else
		obj%FileName=FileName
	endif

	if(present(simple) )then
		if(simple .eqv. .true.)then
			return
		endif

	endif

    if(Default .eqv. .true.)then
        obj%Dtype="FEMDomain"
    endif
    call InitializeMesh(obj%Mesh)
    call InitializeMaterial(obj%MaterialProp)
	call obj%Boundary%Init(Default)
	obj%timestep=0
    
    
    

end subroutine InitializeFEMDomain
!##################################################


!##################################################
subroutine showFEMDomain(obj)
	class(FEMDomain_),intent(in)::obj
	integer(int32)::i

	print *, "=========================="
	print *, "Name :: ",trim(obj%Name)
	print *, "Materials :: "
	if(.not.allocated(obj%Materials) )then
		print *, "No material is imported"
	else
		do i=1,obj%NumberOfMaterials
			if(associated(obj%Materials(i)%materialp ) )then
				call obj%Materials(i)%materialp%show()
			else
				cycle
			endif
		enddo
	endif
	print *, "Boundaries :: "
	if(.not.allocated(obj%boundaries) )then
		print *, "No Boundary is imported"
	else
		do i=1,obj%NumberOfBoundaries
			if(associated(obj%Boundaries(i)%Boundaryp ) )then
				call obj%Boundaries(i)%Boundaryp%show()
			else
				cycle
			endif
		enddo
	endif


end subroutine showFEMDomain
!##################################################

!##################################################
subroutine ImportFEMDomain(obj,OptionalFileFormat,OptionalProjectName,FileHandle,Mesh,Boundaries&
		,Boundary,Materials, Material,NumberOfBoundaries,BoundaryID,NumberOfMaterials,MaterialID,&
		node,element,materialinfo,dirichlet,neumann,file)
	class(FEMDomain_),intent(inout)::obj
	type(Mesh_),optional,intent(in)::Mesh
	type(Mesh_)::mobj
	type(Boundary_),optional,intent(in)::Boundary
	type(MaterialProp_),optional,intent(in)::Material
    character*4,optional,intent(in)::OptionalFileFormat
    character(*),optional,intent(in)::OptionalProjectName
	logical,optional,intent(in) :: node,element,materialinfo,dirichlet,neumann
	type(IO_) :: f

	character(*),optional,intent(in) :: file
	character*4::FileFormat
	character*70::ProjectName
	character*74 ::FileName
	character*9  :: DataType
	integer,allocatable::IntMat(:,:)
	real(8),allocatable::RealMat(:,:)
	integer,optional,intent(in)::FileHandle,NumberOfBoundaries,BoundaryID,MaterialID,NumberOfMaterials
	integer :: fh,i,j,k,NumOfDomain,n,m,DimNum,GpNum,nodenum,matnum, paranum
	character*70 Msg,name,ch
	logical,optional,intent(in) :: Boundaries,Materials

	if( trim(getext(trim(file)) )=="mesh" )then
		
		call f%open("./",trim(file))
		read(f%fh,*) ch
		read(f%fh,*) ch
		read(f%fh,*) n
		read(f%fh,*) ch
		read(f%fh,*) m
		allocate(mobj%NodCoord(m,n) )
		do i=1, m
			read(f%fh,*) mobj%NodCoord(i,:)
		enddo
		do
			read(f%fh,*) ch
			if(trim(adjustl(ch)) == "Tetrahedra" )then
				read(f%fh,*)n
				allocate(mobj%ElemNod(n,4),mobj%ElemMat(n) )
				mobj%ElemMat(:) = 1
				do i=1,n
					read(f%fh,*) mobj%ElemNod(i,1:4)
				enddo
				exit
			elseif(trim(adjustl(ch)) == "End" )then
				exit
			else
				read(f%fh,*)n
				do i=1,n
					read(f%fh,*) ch
				enddo
			endif
		enddo
		call f%close()
		call mobj%convertTetraToHexa()
		call obj%Mesh%copy(mobj)
		return
	endif

if(present(node) )then
	if(node .eqv. .true. )then
		if(.not. present(file) )then
			print *, "Please iput filename"
			stop
		endif
		call f%open("./",trim(file))
		read(f%fh,*) nodenum, dimnum
		if(allocated(obj%Mesh%NodCoord ) )then
			deallocate(obj%Mesh%NodCoord)
		endif
		allocate(obj%Mesh%NodCoord(nodenum, dimnum) )
		do i=1,nodenum
			read(f%fh,*) obj%Mesh%NodCoord(i,:)
		enddo
		call f%close()
		return
	endif
endif

if(present(Element) )then
	if(Element .eqv. .true. )then
		if(.not. present(file) )then
			print *, "Please iput filename"
			stop
		endif
		call f%open("./",trim(file))
		read(f%fh,*) nodenum, dimnum
		if(allocated(obj%Mesh%ElemNod ) )then
			deallocate(obj%Mesh%ElemNod)
		endif
		allocate(obj%Mesh%ElemNod(nodenum, dimnum) )
		do i=1,nodenum
			read(f%fh,*) obj%Mesh%ElemNod(i,:)
		enddo
		call f%close()
		return
	endif
endif

if(present(materialinfo) )then
	if(materialinfo .eqv. .true. )then
		if(.not. present(file) )then
			print *, "Please iput filename"
			stop
		endif
		call f%open("./",trim(file))
		read(f%fh,*) nodenum
		if(allocated(obj%Mesh%ElemMat ) )then
			deallocate(obj%Mesh%ElemMat)
		endif
		allocate(obj%Mesh%ElemMat(nodenum) )
		do i=1,nodenum
			read(f%fh,*) obj%Mesh%ElemMat(i)
		enddo
		
		read(f%fh,*) matnum, paranum
		if(allocated(obj%MaterialProp%MatPara ) )then
			deallocate(obj%MaterialProp%MatPara)
		endif
		allocate(obj%MaterialProp%MatPara(matnum, paranum) )
		do i=1,matnum
			read(f%fh,*) obj%MaterialProp%MatPara(i,:)
		enddo
		call f%close()
		return
	endif
endif

if(present(dirichlet) )then
	if(dirichlet .eqv. .true. )then
		if(.not. present(file) )then
			print *, "Please iput filename"
			stop
		endif
		call f%open("./",trim(file))
		dimnum=size(obj%mesh%NodCoord,2)
		if(allocated(obj%Boundary%DboundNum ) )then
			deallocate(obj%Boundary%DboundNum)
		endif
		allocate(obj%Boundary%DboundNum(dimnum) )
		read(f%fh,*) obj%Boundary%DboundNum(:)
		if(allocated(obj%Boundary%DboundNodID ) )then
			deallocate(obj%Boundary%DboundNodID)
		endif
		allocate(obj%Boundary%DboundNodID(maxval(obj%Boundary%DboundNum),dimnum ) )
		if(allocated(obj%Boundary%DBoundVal ) )then
			deallocate(obj%Boundary%DBoundVal)
		endif
		allocate(obj%Boundary%DBoundVal(maxval(obj%Boundary%DboundNum),dimnum ) )
		
		do i=1,size(obj%Boundary%DboundNodID,1)
			read(f%fh,*) obj%Boundary%DboundNodID(i,:)
		enddo
		do i=1,size(obj%Boundary%DboundVal,1)
			read(f%fh,*) obj%Boundary%DboundVal(i,:)
		enddo
		call f%close()
		return
	endif
endif


if(present(neumann) )then
	if(neumann .eqv. .true. )then
		if(.not. present(file) )then
			print *, "Please iput filename"
			stop
		endif
		call f%open("./",trim(file))
		dimnum=size(obj%mesh%NodCoord,2)
		if(allocated(obj%Boundary%NboundNum ) )then
			deallocate(obj%Boundary%NboundNum)
		endif
		allocate(obj%Boundary%NboundNum(dimnum) )
		read(f%fh,*) obj%Boundary%NboundNum(:)
		if(allocated(obj%Boundary%NboundNodID ) )then
			deallocate(obj%Boundary%NboundNodID)
		endif
		allocate(obj%Boundary%NboundNodID(maxval(obj%Boundary%NboundNum),dimnum ) )
		if(allocated(obj%Boundary%NBoundVal ) )then
			deallocate(obj%Boundary%NBoundVal)
		endif
		allocate(obj%Boundary%NBoundVal(maxval(obj%Boundary%NboundNum),dimnum ) )
		
		do i=1,size(obj%Boundary%NboundNodID,1)
			read(f%fh,*) obj%Boundary%NboundNodID(i,:)
		enddo
		do i=1,size(obj%Boundary%NboundVal,1)
			read(f%fh,*) obj%Boundary%NboundVal(i,:)
		enddo
		call f%close()
		return
	endif
endif



if(present(Boundaries) )then
	if(Boundaries .eqv. .true.)then
		call obj%ImportBoundaries(Boundary,NumberOfBoundaries,BoundaryID)
		return
	endif
endif

if(present(Materials) )then
	if(materials .eqv. .true.)then
		call obj%ImportMaterials(Material,NumberOfMaterials,MaterialID)
		return
	endif
endif
if(present(Mesh) )then
	call obj%Mesh%import(Mesh=Mesh)
	return
endif

!call DeallocateFEMDomain(obj)
name="untitled"
obj%FileName=input(default=name,option=trim(OptionalProjectName))

if(present(FileHandle) )then
    fh=FileHandle
else
    fh =104
endif

if(present(OptionalFileFormat) )then
    FileFormat=trim(OptionalFileFormat)
else
    FileFormat=".scf"
endif


if(present(OptionalProjectName) )then
    ProjectName=trim(OptionalProjectName)
else
    ProjectName="untitled"
endif

FileName = trim(ProjectName)//trim(FileFormat)

!!print *, "Project : ",ProjectName
!!print *, "is Exported as : ",FileFormat," format"
!!print *, "File Name is : ",FileName

open(fh,file=FileName,status="old")


if(trim(FileFormat)==".scf" )then

    read(fh,*) DataType
    if(DataType/="domain")then
        print *, "ERROR :: Datatype ",DataType," is not valid."
        return
    endif
    obj%Dtype=DataType
    read(fh,*) obj%SolverType
    read(fh,*) obj%NumOfDomain
    allocate(IntMat(obj%NumOfDomain,2))
    allocate(obj%Mesh%SubMeshNodFromTo(obj%NumOfDomain,3) )
    allocate(obj%Mesh%SubMeshElemFromTo(obj%NumOfDomain,3) )
    
    do i=1,obj%NumOfDomain
        obj%Mesh%SubMeshNodFromTo(i,1)=i
        read(fh,*) obj%Mesh%SubMeshNodFromTo(i,2),obj%Mesh%SubMeshNodFromTo(i,3)
    enddo

    do i=1,obj%NumOfDomain
        obj%Mesh%SubMeshElemFromTo(i,1)=i
        read(fh,*) obj%Mesh%SubMeshElemFromTo(i,3)
        if(i==1)then
            obj%Mesh%SubMeshElemFromTo(i,2)=1    
        else
            obj%Mesh%SubMeshElemFromTo(i,2)=obj%Mesh%SubMeshElemFromTo(i-1,3)+1
        endif
    enddo


    read(fh,*) n,m
    DimNum=m

    allocate(obj%Mesh%NodCoord(n,m) )
    call ImportArray(obj%Mesh%NodCoord,OptionalFileHandle=fh)
	call CopyArray(obj%Mesh%NodCoord,obj%Mesh%NodCoordInit )

    read(fh,*) n,m
    

    read(fh,*)obj%Mesh%ElemType

    obj%ShapeFunction%ElemType=obj%Mesh%ElemType
    allocate(obj%Mesh%ElemNod(n,m) )
    allocate(obj%Mesh%ElemMat(n  ) )
    call ImportArray(obj%Mesh%ElemNod,OptionalFileHandle=fh)
    do i=1,n
        read(fh,*) obj%Mesh%ElemMat(i)
    enddo
    read(fh,*) n,m

    allocate(obj%MaterialProp%MatPara(n,m) )
    call ImportArray(obj%MaterialProp%MatPara,OptionalFileHandle=fh)

    !DirichletBoundary
    read(fh,*) n !DirichletBoundaryDimension
    
    if(n<=0)then
        print *, "ImportFEMDomain >> Caution :: no Dirichlet Boundary Condition is loaded. "
    else
        allocate(obj%Boundary%DBoundNum(n ))
        read(fh,*) obj%Boundary%DBoundNum(:)


        allocate(obj%Boundary%DBoundNodID( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )
        allocate(obj%Boundary%DBoundVal( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )

        obj%Boundary%DBoundNodID(:,:)=-1
        obj%Boundary%DBoundVal(:,:)  =0.0d0

        do i=1,size(obj%Boundary%DBoundNum,1)
            do j=1,obj%Boundary%DBoundNum(i)
                read(fh,*) obj%Boundary%DBoundNodID(j,i)
                !!print *,obj%Boundary%DBoundNodID(j,i)
            enddo
            do j=1,obj%Boundary%DBoundNum(i)
                read(fh,*) obj%Boundary%DBoundVal(j,i)
                !!print *,obj%Boundary%DBoundVal(j,i)
            enddo
        enddo

    endif


    read(fh,*) DimNum

    if(DimNum<=0)then

        print *, "ImportFEMDomain >> Caution :: no Neumann Boundary Condition is loaded. "
    else
        read(fh,*) n
        allocate( obj%Boundary%NBoundNum(DimNum))
        allocate(obj%Boundary%NBoundNodID(n, size(obj%Boundary%NBoundNum)  )  )
        allocate(obj%Boundary%NBoundVal( n, size(obj%Boundary%NBoundNum)  )  )
        obj%Boundary%NBoundNodID(:,:)=-1
        obj%Boundary%NBoundVal(:,:)  =0.0d0

        obj%Boundary%NBoundNum(:)=n
        do i=1,n
            read(fh,*) m
            obj%Boundary%NBoundNodID(i,:)=m
        enddo

        do i=1,n
            read(fh,*) obj%Boundary%NBoundVal(i,:)
        enddo
    
    endif
        

    !######### Initial conditions #################
    ! For node-wize
    read(fh,*) DimNum
    if(DimNum<=0)then
        print *, "Caution :: no Initial Condition (Node-wise) Condition is loaded. "
    else
        read(fh,*) n
        allocate(obj%Boundary%TBoundNodID(n,DimNum) )
        allocate(obj%Boundary%TBoundVal(  n,DimNum) )
        allocate(obj%Boundary%TBoundNum(  DimNum) )

        obj%Boundary%TBoundNum(:)=n
        
        if(n/=0)then
            if(n<0)then
                print *, "ERROR :: number of initial conditions are to be zero"
            else
                do i=1,n
                    read(fh,*) obj%Boundary%TBoundNodID(i,:)
                enddo
                do i=1,n
                    read(fh,*) obj%Boundary%TBoundVal(i,:)
                enddo
            endif
        endif
    endif
    !######### Initial conditions #################




    !######### Initial conditions #################
    ! For ElementGP-wize
    read(fh,*) DimNum 
    if(DimNum<=0)then
        print *, "Caution :: no Initial Condition (Gp) is loaded. "
    else
        read(fh,*) GpNum
        read(fh,*) n
        allocate(obj%Boundary%TBoundElemID(n) )
        allocate(obj%Boundary%TBoundElemGpVal(n,GpNum,DimNum) )
        
        if(n/=0)then
            if(n<0)then
                print *, "ERROR :: number of initial conditions are to be zero"
            else
                do i=1,n
                    read(fh,*) obj%Boundary%TBoundElemID(i)
                enddo
                do i=1,n
                    do j=1,GpNum
                        do k=1,DimNum
                            read(fh,*) obj%Boundary%TBoundElemGpVal(i,j,k)
                        enddo
                    enddo
                enddo
            endif
        endif

    endif
    !######### Initial conditions #################

    read(fh,*) obj%ControlPara%SimMode ,obj%ControlPara%ItrTol,obj%ControlPara%Timestep
     
    close(fh)
else
    !!print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
endif





end subroutine ImportFEMDomain
!##################################################


!##################################################
subroutine ImportMeshFEMDomain(obj,Mesh)
	class(FEMDomain_),intent(inout)::obj
	class(Mesh_),intent(inout)::Mesh

	call obj%Mesh%copy(Mesh)
end subroutine
!##################################################

subroutine resizeFEMDomain(obj,x_rate,y_rate,z_rate,x_len,y_len,z_len)
	class(FEMDomain_),intent(inout) :: obj
	real(real64),optional,intent(in) :: x_rate,y_rate,z_rate,x_len,y_len,z_len

	call obj%Mesh%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate,x_len=x_len,y_len=y_len,z_len=z_len)

end subroutine


!##################################################
subroutine MergeFEMDomain(inobj1,inobj2,outobj)
    class(FEMDomain_),intent(in) ::inobj1,inobj2
    class(FEMDomain_),intent(out)::outobj
    
    include "./MergeFEMDomain.f90"

end subroutine MergeFEMDomain
!##################################################


!##################################################
subroutine ExportFEMDomain(obj,OptionalFileFormat,OptionalProjectName,FileHandle,SolverType,MeshDimension,&
	FileName,Name,regacy,with,path,extention,step,FieldValue,restart)
    class(FEMDomain_),intent(inout)::obj
    class(FEMDomain_),optional,intent(inout)::with
    character(*),optional,intent(in)::OptionalFileFormat,path,extention
    character(*),optional,intent(in)::OptionalProjectName,SolverType,FileName
	character*4::FileFormat
	character(*),optional,intent(in) :: Name
	logical,optional,intent(in) :: regacy,restart
    character*200::ProjectName
	character*200 ::iFileName
	real(real64),optional,intent(in) :: FieldValue(:,:)
    integer(int32),allocatable::IntMat(:,:)
    real(real64),allocatable::RealMat(:,:)
    integer(int32),optional,intent(in)::FileHandle,MeshDimension,step
    integer(int32) :: fh,i,j,k,n,m,DimNum,GpNum,nn
	character*70 Msg
	type(IO_) :: f

	if(present(restart) )then
		if(.not.present(path) )then
			print *, "FEMDomain ERROR :: .not.present(path)"
			stop 
		endif

		call system("mkdir -p "//trim(path))
		call system("mkdir -p "//trim(path)//"/FEMDomain")
		call obj%Mesh%export(path=trim(path)//"/FEMDomain",restart=.true.)
		call obj%MaterialProp%export(path=trim(path)//"/FEMDomain",restart=.true.)
		call obj%Boundary%export(path=trim(path)//"/FEMDomain",restart=.true.)
		call obj%ControlPara%export(path=trim(path)//"/FEMDomain",restart=.true.)
		call obj%ShapeFunction%export(path=trim(path)//"/FEMDomain",restart=.true.)

		call f%open(trim(path)//"/FEMDomain","/FEMDomain",".prop" )
		write(f%fh,*) obj%RealTime
		write(f%fh,*) obj%NumOfDomain
		write(f%fh, '(A)' ) trim(obj%FilePath)
		write(f%fh, '(A)' ) trim(obj%FileName)
		write(f%fh, '(A)' ) trim(obj%Name)
		write(f%fh, '(A)' ) trim(obj%Dtype)
		write(f%fh, '(A)' ) trim(obj%SolverType)
		write(f%fh, '(A)' ) trim(obj%Category1)
		write(f%fh, '(A)' ) trim(obj%Category2)
		write(f%fh, '(A)' ) trim(obj%Category3)
		write(f%fh,*) obj%timestep, obj%NumberOfBoundaries, obj%NumberOfMaterials
		call f%close()
		return
	endif

	if(present(regacy) )then
		if(regacy .eqv. .true.)then
			! export as regacy mode
			! request Name
			if(.not. present(Name) )then
				print *, "ExportFEMDomain :: please import Name"
				stop 
			endif

			open(100,file=trim(adjustl(name)) )
				print *, "Exporting .scf file >>> ",trim(adjustl(name))
				if(present(with) )then
					print *, "Mode :: contact problem"
					write(100, '(A)' ) "2"
					write(100, '(A)' ) "  "
					n=size(obj%Mesh%NodCoord,1)
					m=size(with%Mesh%NodCoord,1)
					write(100, '(A)' ) "1  "//trim(adjustl(fstring(n) ) )
					write(100, '(A)' ) trim(adjustl(fstring(n+1) ) )//"  "//trim(adjustl(fstring(n+m) ) )
					write(100, '(A)' ) "  "
					n=size(obj%Mesh%ElemNod,1)
					m=size(with%Mesh%ElemNod,1)
					write(100, '(A)' ) trim(adjustl(fstring(n) ) )
					write(100, '(A)' ) trim(adjustl(fstring(n+m) ) )
					write(100, '(A)' ) "  "
					n=size(obj%Mesh%NodCoord,1)
					m=size(obj%Mesh%NodCoord,2)
					write(100, * ) size(obj%Mesh%NodCoord,1)+size(with%Mesh%NodCoord,1)
					write(100, '(A)' ) "  "
					do i=1,n
						write(100,*) obj%Mesh%NodCoord(i,:)	
					enddo
					n=size(with%Mesh%NodCoord,1)
					m=size(with%Mesh%NodCoord,2)
					do i=1,n
						write(100,*) with%Mesh%NodCoord(i,:)
					enddo
					write(100, '(A)' ) "  "
					n=size(with%Mesh%ElemNod,1)+size(obj%Mesh%ElemNod,1)
					m=size(obj%Mesh%ElemNod,2)
					write(100, * ) trim(adjustl(fstring(n) ) ),"  ",trim(adjustl(fstring(m) ) )
					n=size(obj%Mesh%ElemNod,1)
					m=size(obj%Mesh%ElemNod,2)
					write(100, '(A)' ) "  "
					do i=1,n
						write(100,*) obj%Mesh%ElemNod(i,:)
					enddo
					n=size(with%Mesh%ElemNod,1)
					m=size(with%Mesh%ElemNod,2)
					nn=size(obj%Mesh%NodCoord,1)
					do i=1,n
						write(100,*) with%Mesh%ElemNod(i,:)+nn  
					enddo
					print *, "Elem-mat"
					write(100, '(A)' ) "  "
					n=size(obj%Mesh%ElemNod,1)
					if(.not.allocated(obj%Mesh%ElemMat) )then
						allocate(obj%Mesh%ElemMat(n) )
						obj%Mesh%ElemMat(:)=1
					endif
					write(100, '(A)' ) "  "

					do i=1,n
						write(100, *)  obj%Mesh%ElemMat(i)  
					enddo
					write(100, '(A)' ) "  "
					n=size(with%Mesh%ElemNod,1)
					if(.not.allocated(with%Mesh%ElemMat) )then
						allocate(with%Mesh%ElemMat(n) )
						with%Mesh%ElemMat(:)=2
					endif
					write(100, '(A)' ) "  "
					do i=1,n
						write(100, *) with%Mesh%ElemMat(i)  
					enddo
					write(100, '(A)' ) "  "
					
					print *, "Material parameters will be put in here." 
					write(100,*) size(obj%MaterialProp%MatPara,1)
					write(100, '(A)' ) "  "
					do i=1,size(obj%MaterialProp%MatPara,1)
						write(100,*) obj%MaterialProp%MatPara(i,:)
					enddo
					write(100, '(A)' ) "  "
					print *, "Dboundary will be put in here." 
					
					! count number of dirichlet condition for x
					n=0
					do i=1,size(obj%Boundary%DBoundNodID,1)
						if(obj%Boundary%DBoundNodID(i,1)>=1 )then
							n=n+1
						else
							cycle
						endif
					enddo
					do i=1,size(with%Boundary%DBoundNodID,1)
						if(with%Boundary%DBoundNodID(i,1)>=1 )then
							n=n+1
						else
							cycle
						endif
					enddo
					! count number of dirichlet condition for y
					m=0
					do i=1,size(obj%Boundary%DBoundNodID,1)
						if(obj%Boundary%DBoundNodID(i,2)>=1 )then
							m=m+1
						else
							cycle
						endif
					enddo
					do i=1,size(with%Boundary%DBoundNodID,1)
						if(with%Boundary%DBoundNodID(i,2)>=1 )then
							m=m+1
						else
							cycle
						endif
					enddo
					! write number of dirichlet condition for x and y
					write(100,*) n,m

					! write out dirichlet boundary for x
					do i=1,size(obj%Boundary%DBoundNodID,1)
						if(obj%Boundary%DBoundNodID(i,1)>=1 )then
							write(100,*) obj%Boundary%DBoundNodID(i,1)
						else
							cycle
						endif
					enddo
					do i=1,size(with%Boundary%DBoundNodID,1)
						if(with%Boundary%DBoundNodID(i,1)>=1 )then
							write(100,*) with%Boundary%DBoundNodID(i,1)+nn
						else
							cycle
						endif
					enddo
					write(100, '(A)' ) "  "

					! write out value of dirichlet boundary for x
					do i=1,size(obj%Boundary%DBoundNodID,1)
						if(obj%Boundary%DBoundNodID(i,1)>=1 )then
							write(100,*) obj%Boundary%DBoundVal(i,1)
						else
							cycle
						endif
					enddo
					do i=1,size(with%Boundary%DBoundNodID,1)
						if(with%Boundary%DBoundNodID(i,1)>=1 )then
							write(100,*) with%Boundary%DBoundVal(i,1)
						else
							cycle
						endif
					enddo
					write(100, '(A)' ) "  "

					! write out dirichlet boundary for y
					do i=1,size(obj%Boundary%DBoundNodID,1)
						if(obj%Boundary%DBoundNodID(i,2)>=1 )then
							write(100,*) obj%Boundary%DBoundNodID(i,2)
						else
							cycle
						endif
					enddo
					do i=1,size(with%Boundary%DBoundNodID,1)
						if(with%Boundary%DBoundNodID(i,2)>=1 )then
							write(100,*) with%Boundary%DBoundNodID(i,2)+nn
						else
							cycle
						endif
					enddo
					write(100, '(A)' ) "  "
					! write outvalue of  dirichlet boundary for y
					do i=1,size(obj%Boundary%DBoundNodID,1)
						if(obj%Boundary%DBoundNodID(i,2)>=1 )then
							write(100,*) obj%Boundary%DBoundVal(i,2)
						else
							cycle
						endif
					enddo
					do i=1,size(with%Boundary%DBoundNodID,1)
						if(with%Boundary%DBoundNodID(i,2)>=1 )then
							write(100,*) with%Boundary%DBoundVal(i,2)
						else
							cycle
						endif
					enddo
					write(100, '(A)' ) "  "

					if(.not. allocated(obj%Boundary%NBoundNodID)  )then
						write(100,*) 0
					else
						if(size(obj%Boundary%NBoundNodID,1)==0 )then
							write(100,*) 0
						else
							print *, "ERROR :: ExportFEMDOmain :: Neumann boundary will be implemented."
							stop
						endif
					endif
					write(100, '(A)' ) "  "

					! surface nodes
					! count surface nodes
					n=0
					n=size(obj%Mesh%SurfaceLine2D)+size(with%Mesh%SurfaceLine2D)
					write(100,*) n
					write(100, '(A)' ) "  "

					do i=1,size(obj%Mesh%SurfaceLine2D)
						write(100,*) obj%Mesh%SurfaceLine2D(i)
					enddo
					do i=1,size(with%Mesh%SurfaceLine2D)
						write(100,*) with%Mesh%SurfaceLine2D(i)+nn
					enddo
					write(100, '(A)' ) "  "
					write(100,*) 1, size(obj%Mesh%SurfaceLine2D)
					write(100,*) size(obj%Mesh%SurfaceLine2D)+1,size(obj%Mesh%SurfaceLine2D)+size(with%Mesh%SurfaceLine2D)

					write(100,*) 0.010d0, 0.010d0
					write(100,*) 1,1
					write(100,*) 1,n,1
					write(100,*) 1
					write(100,*) 0.5000000000000E+05,   0.5000000000000E+05,   0.2402100000000E+01 ,  0.5404000000000E+00
					write(100,*) 1,200,1
					


				endif
			close(100)
			return

		endif
	endif
	
	if(present(OptionalFileFormat) )then
		if(OptionalFileFormat=="stl" .or. OptionalFileFormat==".stl")then
			if(present(Name) )then
				call ExportFEMDomainAsSTL(obj=obj,&
			FileHandle=FileHandle,MeshDimension=MeshDimension,FileName=trim(adjustl(name)))
			else
				call ExportFEMDomainAsSTL(obj=obj,&
			FileHandle=FileHandle,MeshDimension=MeshDimension,FileName=FileName)
			endif
			
			return
		endif
	endif

	ProjectName = ""
	iFileName=""
	Msg=""


    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh =104
    endif



    if(present(OptionalFileFormat) )then
        FileFormat=trim(OptionalFileFormat)
    else
        FileFormat=".scf"
    endif

    if(present(OptionalProjectName) )then
        ProjectName=trim(OptionalProjectName)
    else
        ProjectName="untitled"
    endif
    iFileName = trim(ProjectName)//trim(FileFormat)

    !!print *, "Project : ",ProjectName
    !!print *, "is Exported as : ",FileFormat," format"
    !!print *, "File Name is : ",iFileName

	if(present(Name) )then
		open(fh,file=trim(adjustl(name))//".scf",status="replace")
	else
		open(fh,file=trim(iFileName),status="replace")
	endif

    if(trim(FileFormat)==".scf" )then
		
		if(allocated(obj%Mesh%SubMeshNodFromTo) )then
			obj%NumOfDomain=size(obj%Mesh%SubMeshNodFromTo,1)
		else
			obj%NumOfDomain=1
		endif

		obj%Dtype="domain"
        write(fh,'(A)') obj%Dtype
        write(*,'(A)') obj%Dtype,trim(iFileName)
        write(fh,*) "  "
        write(fh,'(A)') obj%SolverType
        write(fh,*) "  "
        write(fh,*) obj%NumOfDomain
        write(fh,*) "  "


        print *, "########### Meta Info ###########"
        print *, obj%Dtype
        print *, obj%SolverType
        print *, obj%NumOfDomain
        print *, "########### Meta Info ###########"

		if(.not. allocated(obj%Mesh%SubMeshNodFromTo) )then
			print *, "obj%Mesh%SubMeshNodFromTo is not allocated"
			stop 
		endif

        do i=1,obj%NumOfDomain
            write(fh,*) obj%Mesh%SubMeshNodFromTo(i,2),obj%Mesh%SubMeshNodFromTo(i,3)
        enddo
        write(fh,*) "  "
        do i=1,obj%NumOfDomain
            write(fh,*) obj%Mesh%SubMeshElemFromTo(i,3)
        enddo
        write(fh,*) "  "


		
        print *, "########### Domain info ###########"
        do i=1,obj%NumOfDomain
            !write(*,*) obj%Mesh%SubMeshNodFromTo(i,2),obj%Mesh%SubMeshNodFromTo(i,3)
        enddo
        do i=1,obj%NumOfDomain
            !write(*,*) obj%Mesh%SubMeshElemFromTo(i,3)
        enddo
        
        print *, "########### Domain info ###########"

        n=size(obj%Mesh%NodCoord,1)
        m=size(obj%Mesh%NodCoord,2)
        if(present(MeshDimension) )then
            m=MeshDimension
        endif
        write(fh,*) n,m
        DimNum=m

        write(fh,*) "  "
        do i=1,n
            write(fh,*) obj%Mesh%NodCoord(i,1:m)
        enddo
        flush(fh)

        print *, " "
        print *, "########### Node info ###########"
        print *, "Number of node : ",n, "Dimension : ",m
        print *, "########### Node info ###########"
        print *, " "


        n=size(obj%Mesh%ElemNod,1)
        m=size(obj%Mesh%ElemNod,2)
        write(fh,*) n,m
		write(fh,*) "  "
		
        write(fh,'(A)') trim(obj%Mesh%getElemType() )
        write(fh,*) "  "
        do i=1,n
            write(fh,*) obj%Mesh%ElemNod(i,:)
            if(obj%Mesh%ElemNod(i,1)==0 )then
                exit
            endif
        enddo
        write(fh,*) "  "
		flush(fh)


        print *, " "
        print *, "########### Element info ###########"
        print *, "Element Type : ",trim(obj%Mesh%GetElemType() )
        print *, "Number of Element : ",n, "Number of node per element : ",m
        print *, "Successfully Exported"
        print *, "########### Element info ###########"
        print *, " "

        n=size(obj%Mesh%ElemNod,1)
        do i=1,n
            write(fh,*) obj%Mesh%ElemMat(i)
        enddo
        write(fh,*) "  "
		
        n=size(obj%MaterialProp%MatPara,1)
        m=size(obj%MaterialProp%MatPara,2)
        write(fh,*) n,m
        do i=1,n
            write(fh,*) obj%MaterialProp%MatPara(i,:)
        enddo
        write(fh,*) "  "
        flush(fh)



        print *, "########### Material info ###########"
        n=size(obj%Mesh%ElemNod,1)
        !write(*,*) size(obj%Mesh%ElemMat,1)
        n=size(obj%MaterialProp%MatPara,1)
        m=size(obj%MaterialProp%MatPara,2)
        !write(*,*) n,m
        do i=1,n
            write(*,*) obj%MaterialProp%MatPara(i,:)
        enddo
        print *, "Successfully Exported"
        print *, "########### Material info ###########"
		
        !DirichletBoundary

        if(.not.allocated(obj%Boundary%DBoundNodID))then
            
            write(fh,*) "0" !DirichletBoundaryDimension
            write(fh,*) "  "
			print *, "ImportFEMDomain >> Caution :: no Dirichlet Boundary Condition is loaded. "
			stop 
        else
			! update obj%Boundary%DBoundNum
			if(allocated(obj%Boundary%DBoundNum) )then
				deallocate(obj%Boundary%DBoundNum)
			endif
			n=size(obj%Boundary%DBoundNodID,2)
			allocate(obj%Boundary%DBoundNum(n) )
			m=size(obj%Boundary%DBoundNodID,1)
			do i=1,n
				obj%Boundary%DBoundNum(i)=m-countif(Array=obj%Boundary%DBoundNodID(:,i),Equal=.true.,Value=-1 )
			enddo


            n=size(obj%Boundary%DBoundNum)
            write(fh,*) n !DirichletBoundaryDimension
            write(fh,*) "  "

            !allocate(obj%Boundary%DBoundNum(n ))
            write(fh,*) obj%Boundary%DBoundNum(:)
            write(fh,*) "  "
            !allocate(obj%Boundary%DBoundNodID( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )
            !allocate(obj%Boundary%DBoundVal( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )

            !obj%Boundary%DBoundNodID(:,:)=-1
            !obj%Boundary%DBoundVal(:,:)  =0.0d0

            do i=1,size(obj%Boundary%DBoundNum,1)
                do j=1,obj%Boundary%DBoundNum(i)
					write(fh,*) obj%Boundary%DBoundNodID(j,i)
                    !!print *,obj%Boundary%DBoundNodID(j,i)
                enddo
                write(fh,*) "  "
                do j=1,obj%Boundary%DBoundNum(i)
                    write(fh,*) obj%Boundary%DBoundVal(j,i)
                    !!print *,obj%Boundary%DBoundVal(j,i)
                enddo
                write(fh,*) "  "
            enddo

        endif




        print *, "########### Dirichlet Boundary info ###########"
        if(.not.allocated(obj%Boundary%DBoundNum))then
            
            write(*,*) "0" !DirichletBoundaryDimension
            write(*,*) "  "
            stop "ERROR :: FEMDomainClass :: no Dirichlet boundary is found"
            !print *, "ImportFEMDomain >> Caution :: no Dirichlet Boundary Condition is loaded. "
        else

            n=size(obj%Boundary%DBoundNum)
            !write(*,*) n !DirichletBoundaryDimension
            !write(*,*) "  "

            !allocate(obj%Boundary%DBoundNum(n ))
            !write(*,*) obj%Boundary%DBoundNum(:)
            !write(*,*) "  "
            !allocate(obj%Boundary%DBoundNodID( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )
            !allocate(obj%Boundary%DBoundVal( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )

            !obj%Boundary%DBoundNodID(:,:)=-1
            !obj%Boundary%DBoundVal(:,:)  =0.0d0

            do i=1,size(obj%Boundary%DBoundNum,1)
                do j=1,obj%Boundary%DBoundNum(i)
                    !write(*,*) obj%Boundary%DBoundNodID(j,i)
                    !!print *,obj%Boundary%DBoundNodID(j,i)
                enddo
                !write(*,*) "  "
                do j=1,obj%Boundary%DBoundNum(i)
                    !write(*,*) obj%Boundary%DBoundVal(j,i)
                    !!print *,obj%Boundary%DBoundVal(j,i)
                enddo
                !write(*,*) "  "
            enddo

        endif

        print *, "Successfully Exported"
        print *, "########### Dirichlet Boundary info ###########"
        
        



        if(.not.allocated(obj%Boundary%NBoundNum) )then
            DimNum=0
        else
            DimNum=size(obj%Boundary%NBoundNum,1)
        endif


        write(fh,*) DimNum
        write(fh,*) "  "
        if(DimNum<=0)then
            !print *, "ImportFEMDomain >> Caution :: no Neumann Boundary Condition is loaded. "
        else
            n=size(obj%Boundary%NBoundNodID,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate( obj%Boundary%NBoundNum(DimNum))
            !allocate(obj%Boundary%NBoundNodID(n, size(obj%Boundary%NBoundNum)  )  )
            !allocate(obj%Boundary%NBoundVal( n, size(obj%Boundary%NBoundNum)  )  )
            !obj%Boundary%NBoundNodID(:,:)=-1
            !obj%Boundary%NBoundVal(:,:)  =0.0d0

            !obj%Boundary%NBoundNum(:)=n
            do i=1,n
                write(fh,*) obj%Boundary%NBoundNodID(i,:)
                !obj%Boundary%NBoundNodID(i,:)=m
            enddo
            write(fh,*) "  "

            do i=1,n
                write(fh,*) obj%Boundary%NBoundVal(i,:)
            enddo
            write(fh,*) "  "
        
        endif
            


        print *, "########### Neumann Boundary info ###########"

        if(.not.allocated(obj%Boundary%NBoundNum) )then
            DimNum=0
        else
            DimNum=size(obj%Boundary%NBoundNum,1)
        endif
        !write(*,*) DimNum
        !write(*,*) "  "
        if(DimNum<=0)then
            !print *, "ImportFEMDomain >> Caution :: no Neumann Boundary Condition is loaded. "
        else
            n=size(obj%Boundary%NBoundNodID,1)
            !write(*,*) n
            !write(*,*) "  "
            !allocate( obj%Boundary%NBoundNum(DimNum))
            !allocate(obj%Boundary%NBoundNodID(n, size(obj%Boundary%NBoundNum)  )  )
            !allocate(obj%Boundary%NBoundVal( n, size(obj%Boundary%NBoundNum)  )  )
            !obj%Boundary%NBoundNodID(:,:)=-1
            !obj%Boundary%NBoundVal(:,:)  =0.0d0

            !obj%Boundary%NBoundNum(:)=n
            do i=1,n
                !write(*,*) obj%Boundary%NBoundNodID(i,:)
                !obj%Boundary%NBoundNodID(i,:)=m
            enddo
            !write(*,*) "  "

            do i=1,n
                !write(*,*) obj%Boundary%NBoundVal(i,:)
            enddo
            !write(*,*) "  "
        
        endif
        print *, "Successfully Exported"
        print *, "########### Neumann Boundary info ###########"
        
        



        print *, "########### Initial Condition info ###########"
        
        !######### Initial conditions #################
        ! For node-wize
        
        if(.not.allocated(obj%Boundary%TBoundVal) )then
            DimNum=0
        else
            DimNum=size(obj%Boundary%TBoundVal,2)
        endif
        write(fh,*) DimNum
        write(fh,*) "  "
        


        if(DimNum<=0)then
            !print *, "Caution :: no Initial Condition (Node-wise) Condition is loaded. "
        else
            n=size(obj%Boundary%TBoundVal,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate(obj%Boundary%TBoundNodID(n,DimNum) )
            !allocate(obj%Boundary%TBoundVal(  n,DimNum) )
            !allocate(obj%Boundary%TBoundNum(  DimNum) )

            !obj%Boundary%TBoundNum(:)=n
            
            if(n/=0)then
                if(n<0)then
                    print *, "ERROR :: number of initial conditions are to be zero"
                else
                    do i=1,n
                        write(fh,*) obj%Boundary%TBoundNodID(i,:)
                    enddo
                    write(fh,*) "  "
                    do i=1,n
                        write(fh,*) obj%Boundary%TBoundVal(i,:)
                    enddo
                    write(fh,*) "  "
                endif
            endif
        endif
        !######### Initial conditions #################

        print *, "Successfully Exported"
        print *, "########### Initial Condition info ###########"
        



        print *, "########### Initial Condition (Element-wize) info ###########"
        !######### Initial conditions #################
        ! For ElementGP-wize
        if(.not.allocated(obj%Boundary%TBoundElemGpVal) )then
            DimNum=0
        else
            DimNum=size(obj%Boundary%TBoundElemGpVal,3)
        endif
        
        write(fh,*) DimNum 
        write(fh,*) "  "
        if(DimNum<=0)then
            !print *, "Caution :: no Initial Condition (Gp) is loaded. "
        else
            !write(fh,*) 
            GpNum=size(obj%Boundary%TBoundElemGpVal,2)
            write(fh,*) GpNum
            write(fh,*) "  "
            !write(fh,*) 
            n=size(obj%Boundary%TBoundElemGpVal,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate(obj%Boundary%TBoundElemID(n) )
            !allocate(obj%Boundary%TBoundElemGpVal(n,GpNum,DimNum) )
            
            if(n/=0)then
                if(n<0)then
                    print *, "ERROR :: number of initial conditions are to be zero"
                else
                    do i=1,n
                        write(fh,*) obj%Boundary%TBoundElemID(i)
                    enddo
                    write(fh,*) "  "
                    do i=1,n
                        do j=1,GpNum
                            do k=1,DimNum
                                write(fh,*) obj%Boundary%TBoundElemGpVal(i,j,k)
                            enddo
                        enddo
                    enddo
                    write(fh,*) "  "
                endif
            endif

        endif
        !######### Initial conditions #################

        print *, "Successfully Exported"
        print *, "########### Initial Condition (Element-wize) info ###########"
        
        write(fh,*) obj%ControlPara%SimMode ,obj%ControlPara%ItrTol,obj%ControlPara%Timestep
        flush(fh)
        close(fh)
    else
        print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
    endif





end subroutine ExportFEMDomain
!##################################################

!##################################################
subroutine InitDBC(obj,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::obj
    integer(int32),intent(in) :: NumOfValPerNod
    
    integer(int32) :: n,m
    !if the facet is not created, create facets (surface elements)
    call GetSurface(obj%Mesh)        
    n=size(obj%Mesh%FacetElemNod,1)
    m=size(obj%Mesh%FacetElemNod,2)

    if(allocated(obj%Boundary%DBoundNum))then
        deallocate(obj%Boundary%DBoundNum)
    endif

    if(allocated(obj%Boundary%DBoundNodID))then
        deallocate(obj%Boundary%DBoundNodID)
    endif

    if(allocated(obj%Boundary%DBoundVal) )then
        deallocate(obj%Boundary%DBoundVal)
    endif

    allocate(obj%Boundary%DBoundNum(NumOfValPerNod) )
    obj%Boundary%DBoundNum(:)=0
    allocate(obj%Boundary%DBoundNodID(n*m,NumOfValPerNod) )
    obj%Boundary%DBoundNodID(:,:)=-1
    allocate(obj%Boundary%DBoundVal(n*m,NumOfValPerNod) )
    obj%Boundary%DBoundVal(:,:)=0.0d0
            
end subroutine
!##################################################


!##################################################
subroutine AddDBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::obj
    real(real64),optional,intent(in)::xmin,xmax
    real(real64),optional,intent(in)::ymin,ymax
    real(real64),optional,intent(in)::zmin,zmax
    real(real64),optional,intent(in)::tmin,tmax
    real(real64),optional,intent(in)::val
    integer(int32),optional,intent(in)::val_id,NumOfValPerNod
    real(real64)::x_min,x_max
    real(real64)::y_min,y_max
    real(real64)::z_min,z_max
    real(real64)::t_min,t_max
    
    real(real64),optional,intent(in)::valx,valy,valz


    logical,optional,intent(in) :: Mode2D
    logical :: InOut
    real(real64) :: minline,maxline,SetDBCound(3)
    integer(int32),allocatable::DBoundNodIDBuf(:,:),CopiedArrayInt(:,:)
    real(real64),allocatable::DBoundValBuf(:,:),CopiedArrayReal(:,:),x(:),rmin(:),rmax(:)
    integer(int32) :: countnum,i,j,k,node_id,n,m,NumVN,newboundnum,ValID,count_n,dim_num


    if(present(val_id) )then
        ValID=val_id
    else
        ValID=1
    endif

    if( present(NumOfValPerNod) )then
        NumVN=NumOfValPerNod
    else
        NumVN=3
    endif

	n=size(obj%Mesh%NodCoord,2)
	dim_num=n
	
    
    if( present(Mode2D) )then
        if(Mode2D .eqv. .true.)then
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        else
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        endif
    elseif(n==2)then
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    else
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    endif
    if(.not.present(xmin) ) then
        x_min = -1.0e+14
        
    else
        x_min=xmin
    endif
    if(.not.present(xmax) ) then
        x_max = 1.0e+14
    else
        x_max=xmax
    endif

    if(.not.present(ymin) ) then
        y_min = -1.0e+14
    else
        y_min=ymin
    endif
    if(.not.present(ymax) ) then
        y_max = 1.0e+14
    else
        y_max=ymax
    endif

    if(.not.present(zmin) ) then
        z_min = -1.0e+14
    else
        z_min = zmin
    
    endif
    if(.not.present(zmax) ) then
        z_max = 1.0e+14
    else
        z_max=zmax
    endif
    
    if(.not.present(tmin) ) then
        t_min = -1.0e+14
    else
        t_min = tmin
    endif
    if(.not.present(tmax) ) then
        t_max = 1.0e+14
    else
        t_max = tmax
    endif

	!print *, "Range is : ",x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    
    ! get node ID and value
    
    !if the facet is not created, create facets (surface elements)
    
    
    if( .not. allocated(obj%Mesh%FacetElemNod))then
		call obj%InitDBC(NumOfValPerNod)
		print *, "add dbc :: initialized"
	endif
	
	if(.not.allocated(obj%Boundary%DBoundNodID) )then
		call obj%InitDBC(NumOfValPerNod)
	endif
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
	rmax(3)=z_max
	
    n=size(obj%Mesh%FacetElemNod,1)
	m=size(obj%Mesh%FacetElemNod,2)
	count_n=0

	if(.not. allocated(obj%Boundary%DBoundNum) )then
		i=size(obj%Boundary%DBoundNodID,2)
		allocate(obj%Boundary%DBoundNum(i))
	endif

    do i=1,size(obj%Mesh%FacetElemNod,1)
		do j=1,size(obj%Mesh%FacetElemNod,2)
			if(obj%Mesh%FacetElemNod(i,j) > size(obj%Mesh%NodCoord,1) )then
				print *, "ERROR :: obj%Mesh%FacetElemNod is out of range"
				print *, "Number of nodes: ",size(obj%Mesh%NodCoord,1),&
					"obj%Mesh%FacetElemNod(i,j) is ",obj%Mesh%FacetElemNod(i,j)
				stop 
			endif
			x(:)=0.0d0
            x(1:dim_num)=obj%Mesh%NodCoord( obj%Mesh%FacetElemNod(i,j),1:dim_num )    
			InOut = InOrOut(x,rmax,rmin)
            if(InOut .eqv. .true.)then
                if( (i-1)*m+j > n*m )then
                    stop "sgdssdfssssssssssssss"
				endif
				count_n=count_n+1
				if(size(obj%Boundary%DBoundNodID,1) < (i-1)*m+j  )then
					print *, "ERROR :: obj%Boundary%DBoundNodID is out of range"
					print *, size(obj%Boundary%DBoundNodID,1),size(obj%Boundary%DBoundNodID,2),size(obj%Mesh%NodCoord,1),&
						ValID,obj%Mesh%FacetElemNod(i,j)
					stop 
				endif
                obj%Boundary%DBoundNum(ValID)=obj%Boundary%DBoundNum(ValID)+1
                obj%Boundary%DBoundNodID( (i-1)*m+j ,ValID)=obj%Mesh%FacetElemNod(i,j)
                obj%Boundary%DBoundVal( (i-1)*m+j ,ValID)=val
            endif
            
        enddo
            
    enddo
		
	print *, "Total ",count_n,"boundary conditions are set"

end subroutine AddDBoundCondition
!##################################################



!##################################################
subroutine InitNBC(obj,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::obj
    integer(int32),intent(in) :: NumOfValPerNod
    
    integer(int32) :: n,m
    !if the facet is not created, create facets (surface elements)
    if( .not. allocated(obj%Mesh%FacetElemNod) )then
        call GetSurface(obj%Mesh)        
	endif
	
    n=size(obj%Mesh%FacetElemNod,1)
    m=size(obj%Mesh%FacetElemNod,2)


    if(allocated(obj%Boundary%NBoundNum))then
        deallocate(obj%Boundary%NBoundNum)
    endif

    if(allocated(obj%Boundary%NBoundNodID))then
        deallocate(obj%Boundary%NBoundNodID)
    endif

    if(allocated(obj%Boundary%NBoundVal) )then
        deallocate(obj%Boundary%NBoundVal)
    endif

    allocate(obj%Boundary%NBoundNum(NumOfValPerNod) )
    obj%Boundary%NBoundNum(:)=0
    allocate(obj%Boundary%NBoundNodID(n*m,NumOfValPerNod) )
    obj%Boundary%NBoundNodID(:,:)=-1
    allocate(obj%Boundary%NBoundVal(n*m,NumOfValPerNod) )
    obj%Boundary%NBoundVal(:,:)=0.0d0
    
    return
        
end subroutine
!##################################################





!##################################################
subroutine AddNBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::obj
    real(real64),optional,intent(in)::xmin,xmax
    real(real64),optional,intent(in)::ymin,ymax
    real(real64),optional,intent(in)::zmin,zmax
    real(real64),optional,intent(in)::tmin,tmax
    real(real64),optional,intent(in)::val
    integer(int32),optional,intent(in)::val_id,NumOfValPerNod
    real(real64)::x_min,x_max
    real(real64)::y_min,y_max
    real(real64)::z_min,z_max
	real(real64)::t_min,t_max,area
	type(Triangle_) :: tobj
    
    real(real64),optional,intent(in)::valx,valy,valz


    logical,optional,intent(in) :: Mode2D
    logical :: InOut
    real(real64) :: minline,maxline,SetDBCound(3)
    integer(int32),allocatable::NBoundNodINBuf(:,:),CopiedArrayInt(:,:)
    real(real64),allocatable::NBoundValBuf(:,:),CopiedArrayReal(:,:),x(:),rmin(:),rmax(:)
    integer(int32) :: countnum,i,j,k,node_id,n,m,NumVN,newboundnum,ValID,dim,nodenum

    if(present(val_id) )then
        ValID=val_id
    else
        ValID=1
    endif

    if( present(NumOfValPerNod) )then
        NumVN=NumOfValPerNod
    else
        NumVN=3
    endif



    n=size(obj%Mesh%NodCoord,2)
    
    if( present(Mode2D) )then
        if(Mode2D .eqv. .true.)then
            allocate(x(2) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        else
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        endif
    elseif(n==2)then
        allocate(x(2) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    else
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    endif

    if(.not.present(xmin) ) then
        x_min = -1.0e+14
        
    else
        x_min=xmin
    endif
    if(.not.present(xmax) ) then
        x_max = 1.0e+14
    else
        x_max=xmax
    endif

    if(.not.present(ymin) ) then
        y_min = -1.0e+14
    else
        y_min=ymin
    endif
    if(.not.present(ymax) ) then
        y_max = 1.0e+14
    else
        y_max=ymax
    endif

    if(.not.present(zmin) ) then
        z_min = -1.0e+14
    else
        z_min = zmin
    
    endif
		
	if(.not.present(zmax) ) then
        z_max = 1.0e+14
    else
        z_max=zmax
    endif

	
    if(.not.present(tmin) ) then
        t_min = -1.0e+14
    else
        t_min = tmin
    endif
    if(.not.present(tmax) ) then
        t_max = 1.0e+14
    else
        t_max = tmax
    endif
    
    

    ! get node ID and value
    
    !if the facet is not created, create facets (surface elements)
    
    
    if( .not. allocated(obj%Mesh%FacetElemNod))then
        call obj%InitNBC(NumOfValPerNod)
        
    endif
    
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(obj%Mesh%FacetElemNod,1)
    m=size(obj%Mesh%FacetElemNod,2)
        
    do i=1,size(obj%Mesh%FacetElemNod,1)
        do j=1,size(obj%Mesh%FacetElemNod,2)
            x(:)=obj%Mesh%NodCoord( obj%Mesh%FacetElemNod(i,j),: )
            InOut = InOrOut(x,rmax,rmin)
            if(InOut .eqv. .true.)then
                if( (i-1)*m+j > n*m )then
                    stop "sgdssdfssssssssssssss"
                endif
                obj%Boundary%NBoundNum(ValID)=obj%Boundary%NBoundNum(ValID)+1
				obj%Boundary%NBoundNodID( (i-1)*m+j ,ValID)=obj%Mesh%FacetElemNod(i,j)
				nodenum=size(obj%Mesh%ElemNod,2)
				if(nodenum==3)then
					call tobj%init(dim=3)
					tobj%NodCoord(1,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),1:3)
					tobj%NodCoord(2,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),1:3)
					tobj%NodCoord(3,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),1:3)
					area=tobj%getArea()
				elseif(nodenum>=4)then
					nodenum=4
					call tobj%init(dim=3)
					tobj%NodCoord(1,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),1:3)
					tobj%NodCoord(2,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,2),1:3)
					tobj%NodCoord(3,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),1:3)
					area=tobj%getArea()
					call tobj%init(dim=3)
					tobj%NodCoord(1,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,2),1:3)
					tobj%NodCoord(2,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),1:3)
					tobj%NodCoord(3,1:3)=&
					obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,4),1:3)
					area=area+tobj%getArea()
				else
					print *, "ERROR :: Node num = ",nodenum,"is not implemented."
					stop 
				endif
				if(area==0.0d0 .or. area/=area)then
					print *, "area==0.0d0 .or. area/=area"
					stop
				endif
                obj%Boundary%NBoundVal( (i-1)*m+j ,ValID)=val*area/dble(nodenum)
            endif
        enddo
    enddo
    return

    

!
!    if(.not.present(valx) ) then
!        SetNBCound(1)=0.0d0
!    else
!        SetNBCound(1)=valx
!    endif
!    if(.not.present(valy) ) then
!        SetNBCound(2)=0.0d0
!    else
!        SetNBCound(2)=valy
!    endif    
!    if(.not.present(valz) ) then
!        SetNBCound(3)=0.0d0
!    else
!        SetNBCound(3)=valz
!    endif
!
!    
!    allocate(NBoundNodINBuf(size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%NBoundNodID,2) ) )
!    allocate(NBoundValBuf  (size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%NBoundNodID,2) ) )
!    
!    NBoundNodINBuf(:,:) = -1
!    NBoundValBuf(:,:)   = -1.0d0
!
!
!    k=0
!    do i=1,size(obj%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=obj%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(obj%Mesh%NodCoord,2)
!            if(j==1)then
!                minline=x_min
!                maxline=x_max
!            elseif(j==2)then
!                minline=y_min
!                maxline=y_max
!            elseif(j==3)then
!                minline=z_min
!                maxline=z_max
!            elseif(j==4)then
!                minline=t_min
!                maxline=t_max
!            else
!                !print *, "ERROR :: EditClass >> AddNBoundCondition >> dimension should 0 < d < 5"
!            endif
!            if(minline <= obj%Mesh%NodCoord(node_id,j) .and. obj%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(obj%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(obj%Mesh%NodCoord,2)
!                if(j==1)then
!                    if(.not.present(valx) ) then
!                        NBoundNodINBuf(k,1)=-1
!                    else
!                        NBoundNodINBuf(k,1)=node_id
!                        NBoundValBuf(k,1)=valx
!                    endif
!                elseif(j==2)then
!                    if(.not.present(valy) ) then
!                        NBoundNodINBuf(k,2)=-1
!                    else
!                        NBoundNodINBuf(k,2)=node_id
!                        NBoundValBuf(k,2)=valy
!                    endif    
!                elseif(j==3)then
!                    if(.not.present(valz) ) then
!                        NBoundNodINBuf(k,3)=-1
!                    else
!                        NBoundNodINBuf(k,3)=node_id
!                        NBoundValBuf(k,3)=valz
!                    endif
!                else
!                    stop "EditClass >Time domain is not implemented "
!                endif
!            enddo   
!        endif
!    enddo
!
!
!    
!    ! MergeArray
! 
!    call TrimArray(DBoundNodIDBuf,k)
!    call TrimArray(DBoundValBuf,k)
!    call CopyArray(obj%Boundary%DBoundNodID,CopiedArrayInt)
!    call CopyArray(obj%Boundary%DBoundVal,CopiedArrayReal)
!    call MergeArray(CopiedArrayInt,DBoundNodIDBuf,obj%Boundary%DBoundNodID)
!    call MergeArray(CopiedArrayReal,DBoundValBuf,obj%Boundary%DBoundVal)
!
!    call DeleteOverlapBoundary(obj%Boundary)

end subroutine AddNBoundCondition
!##################################################

!##################################################
subroutine InitTBC(obj,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::obj
    integer(int32),intent(in) :: NumOfValPerNod
    
    integer(int32) :: n,m
    !if the facet is not created, create facets (surface elements)
    if( .not. allocated(obj%Mesh%FacetElemNod) )then
        call GetSurface(obj%Mesh)        
    endif
    n=size(obj%Mesh%NodCoord,1)
    m=size(obj%Mesh%NodCoord,2)

    if(allocated(obj%Boundary%TBoundNum))then
        deallocate(obj%Boundary%TBoundNum)
    endif

    if(allocated(obj%Boundary%TBoundNodID))then
        deallocate(obj%Boundary%TBoundNodID)
    endif

    if(allocated(obj%Boundary%TBoundVal) )then
        deallocate(obj%Boundary%TBoundVal)
    endif

    allocate(obj%Boundary%TBoundNum(NumOfValPerNod) )
    obj%Boundary%TBoundNum(:)=0
    allocate(obj%Boundary%TBoundNodID(n,NumOfValPerNod) )
    obj%Boundary%TBoundNodID(:,:)=-1
    allocate(obj%Boundary%TBoundVal(n,NumOfValPerNod) )
    obj%Boundary%TBoundVal(:,:)=0.0d0
    
    return
        
end subroutine
!##################################################





!##################################################
subroutine AddTBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::obj
    real(real64),optional,intent(in)::xmin,xmax
    real(real64),optional,intent(in)::ymin,ymax
    real(real64),optional,intent(in)::zmin,zmax
    real(real64),optional,intent(in)::tmin,tmax
    real(real64),optional,intent(in)::val
    integer(int32),optional,intent(in)::val_id,NumOfValPerNod
    real(real64)::x_min,x_max
    real(real64)::y_min,y_max
    real(real64)::z_min,z_max
    real(real64)::t_min,t_max
    
    real(real64),optional,intent(in)::valx,valy,valz


    logical,optional,intent(in) :: Mode2D
    logical :: InOut
    real(real64) :: minline,maxline,SetDBCound(3)
    integer(int32),allocatable::TBoundNodITBuf(:,:),CopiedArrayInt(:,:)
    real(real64),allocatable::TBoundValBuf(:,:),CopiedArrayReal(:,:),x(:),rmin(:),rmax(:)
    integer(int32) :: countnum,i,j,k,node_id,n,m,NumVN,newboundnum,ValID,count_n

    if(present(val_id) )then
        ValID=val_id
    else
        ValID=1
    endif

    if( present(NumOfValPerNod) )then
        NumVN=NumOfValPerNod
    else
        NumVN=3
    endif

    n=size(obj%Mesh%NodCoord,2)
    
    if( present(Mode2D) )then
        if(Mode2D .eqv. .true.)then
            allocate(x(2) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        else
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        endif
    elseif(n==2)then
        allocate(x(2) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    else
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    endif

    if(.not.present(xmin) ) then
        x_min = -1.0e+14
        
    else
        x_min=xmin
    endif
    if(.not.present(xmax) ) then
        x_max = 1.0e+14
    else
        x_max=xmax
    endif

    if(.not.present(ymin) ) then
        y_min = -1.0e+14
    else
        y_min=ymin
    endif
    if(.not.present(ymax) ) then
        y_max = 1.0e+14
    else
        y_max=ymax
    endif

    if(.not.present(zmin) ) then
        z_min = -1.0e+14
    else
        z_min = zmin
    
    endif
    if(.not.present(zmax) ) then
        z_max = 1.0e+14
    else
        z_max=zmax
    endif
    
    if(.not.present(tmin) ) then
        t_min = -1.0e+14
    else
        t_min = tmin
    endif
    if(.not.present(tmax) ) then
        t_max = 1.0e+14
    else
        t_max = tmax
    endif
    
    

    ! get node ID and value
    
    !if the facet is not created, create facets (surface elements)
    
    
    if( size(obj%Mesh%NodCoord,1)/=size(obj%Boundary%TBoundNodID,1)  )then
        call obj%InitTBC(NumOfValPerNod)
        print *, "sifdh"
    endif
    
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(obj%Mesh%NodCoord,1)

	count_n=0
    do i=1,n
            x(:)=obj%Mesh%NodCoord( i,: )
            InOut = InOrOut(x,rmax,rmin)
			if(InOut .eqv. .true.)then
				count_n=count_n+1
                obj%Boundary%TBoundNum(ValID)=obj%Boundary%TBoundNum(ValID)+1
                obj%Boundary%TBoundNodID( i ,ValID)=i
                obj%Boundary%TBoundVal( i,ValID)=val
            endif
        
	enddo
	print *, "Initial value is in : ",count_n,"value is : ",val
    return

end subroutine AddTBoundCondition
!##################################################



!!##################################################
!subroutine AddNBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
!    tmin,tmax,valx,valy,valz)
!    class(FEMDomain_),intent(inout)::obj
!    real(real64),optional,intent(in)::xmin,xmax
!    real(real64),optional,intent(in)::ymin,ymax
!    real(real64),optional,intent(in)::zmin,zmax
!    real(real64),optional,intent(in)::tmin,tmax
!    real(real64)::x_min,x_max
!    real(real64)::y_min,y_max
!    real(real64)::z_min,z_max
!    real(real64)::t_min,t_max
!    
!    real(real64),optional,intent(in)::valx,valy,valz
!
!    real(real64) :: minline,maxline,SetNBCound(3)
!    integer(int32),allocatable::NBoundNodIDBuf(:,:),CopiedArrayInt(:,:)
!    real(real64),allocatable::NBoundValBuf(:,:),CopiedArrayReal(:,:)
!    integer(int32) :: countnum,i,j,k,node_id
!
!    
!
!
!
!
!    if(.not.present(xmin) ) then
!        x_min = -1.0e+14
!    else
!        x_min=xmin
!    endif
!    if(.not.present(xmax) ) then
!        x_max = 1.0e+14
!    else
!        x_max=xmax
!    endif
!
!    if(.not.present(ymin) ) then
!        y_min = -1.0e+14
!    else
!        y_min=ymin
!    endif
!    if(.not.present(ymax) ) then
!        y_max = 1.0e+14
!    else
!        y_max=ymax
!    endif
!
!    if(.not.present(zmin) ) then
!        z_min = -1.0e+14
!    else
!        z_min = zmin
!    
!    endif
!    if(.not.present(zmax) ) then
!        z_max = 1.0e+14
!    else
!        z_max=zmin
!    endif
!    
!    if(.not.present(tmin) ) then
!        t_min = -1.0e+14
!    else
!        t_min = tmin
!    endif
!    if(.not.present(tmax) ) then
!        t_max = 1.0e+14
!    else
!        t_max = tmax
!    endif
!    
!    if(.not.present(valx) ) then
!        SetNBCound(1)=0.0d0
!    else
!        SetNBCound(1)=valx
!    endif
!    if(.not.present(valy) ) then
!        SetNBCound(2)=0.0d0
!    else
!        SetNBCound(2)=valy
!    endif    
!    if(.not.present(valz) ) then
!        SetNBCound(3)=0.0d0
!    else
!        SetNBCound(3)=valz
!    endif
!
!
!    ! get node ID and value
!    allocate(NBoundNodIDBuf(size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%NBoundNodID,2) ) )
!    allocate(NBoundValBuf  (size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%NBoundNodID,2) ) )
!    NBoundNodIDBuf(:,:) = -1
!    NBoundValBuf(:,:)   = -1.0d0
!
!
!
!    k=0
!    do i=1,size(obj%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=obj%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(obj%Mesh%NodCoord,2)
!            if(j==1)then
!                minline=x_min
!                maxline=x_max
!            elseif(j==2)then
!                minline=y_min
!                maxline=y_max
!            elseif(j==3)then
!                minline=z_min
!                maxline=z_max
!            elseif(j==4)then
!                minline=t_min
!                maxline=t_max
!            else
!                !print *, "ERROR :: EditClass >> AddNBoundCondition >> dimension should 0 < d < 5"
!            endif
!            if(minline <= obj%Mesh%NodCoord(node_id,j) .and. obj%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(obj%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(obj%Mesh%NodCoord,2)
!                if(j==1)then
!                    if(.not.present(valx) ) then
!                        NBoundNodIDBuf(k,1)=-1
!                    else
!                        NBoundNodIDBuf(k,1)=node_id
!                        NBoundValBuf(k,1)=valx
!                    endif
!                elseif(j==2)then
!                    if(.not.present(valy) ) then
!                        NBoundNodIDBuf(k,2)=-1
!                    else
!                        NBoundNodIDBuf(k,2)=node_id
!                        NBoundValBuf(k,2)=valy
!                    endif    
!                elseif(j==3)then
!                    if(.not.present(valz) ) then
!                        NBoundNodIDBuf(k,3)=-1
!                    else
!                        NBoundNodIDBuf(k,3)=node_id
!                        NBoundValBuf(k,3)=valz
!                    endif
!                else
!                    stop "EditClass >Time domain is not implemented "
!                endif
!            enddo   
!        endif
!    enddo
!
!    
!
!    ! MergeArray
! 
!    call TrimArray(NBoundNodIDBuf,k)
!    call TrimArray(NBoundValBuf,k)
!    call CopyArray(obj%Boundary%NBoundNodID,CopiedArrayInt)
!    call CopyArray(obj%Boundary%NBoundVal,CopiedArrayReal)
!!    call MergeArray(CopiedArrayInt,NBoundNodIDBuf,obj%Boundary%NBoundNodID)
!!    call MergeArray(CopiedArrayReal,NBoundValBuf,obj%Boundary%NBoundVal)
!!    call DeleteOverlapBoundary(obj%Boundary)
!!    call InitializeBoundary(obj%Boundary)
!!    
!!
!!
!!end subroutine 
!!##################################################
!
!
!
!
!
!
!
!
!!##################################################
!subroutine AddTBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
!    tmin,tmax,valx,valy,valz)
!    class(FEMDomain_),intent(inout)::obj
!    real(real64),optional,intent(in)::xmin,xmax
!    real(real64),optional,intent(in)::ymin,ymax
!    real(real64),optional,intent(in)::zmin,zmax
!    real(real64),optional,intent(in)::tmin,tmax
!    real(real64)::x_min,x_max
!    real(real64)::y_min,y_max
!    real(real64)::z_min,z_max
!    real(real64)::t_min,t_max
!    
!    real(real64),optional,intent(in)::valx,valy,valz
!
!    real(real64) :: minline,maxline,SetTBCound(3)
!    integer(int32),allocatable::TBoundNodIDBuf(:,:),CopiedArrayInt(:,:)
!    real(real64),allocatable::TBoundValBuf(:,:),CopiedArrayReal(:,:)
!    integer(int32) :: countnum,i,j,k,node_id
!
!    
!
!
!
!
!    if(.not.present(xmin) ) then
!        x_min = -1.0e+14
!    else
!        x_min=xmin
!    endif
!    if(.not.present(xmax) ) then
!        x_max = 1.0e+14
!    else
!        x_max=xmax
!    endif
!
!    if(.not.present(ymin) ) then
!        y_min = -1.0e+14
!    else
!        y_min=ymin
!    endif
!    if(.not.present(ymax) ) then
!        y_max = 1.0e+14
!    else
!        y_max=ymax
!    endif
!
!    if(.not.present(zmin) ) then
!        z_min = -1.0e+14
!    else
!        z_min = zmin
!    
!    endif
!    if(.not.present(zmax) ) then
!        z_max = 1.0e+14
!    else
!        z_max=zmin
!    endif
!    
!    if(.not.present(tmin) ) then
!        t_min = -1.0e+14
!    else
!        t_min = tmin
!    endif
!    if(.not.present(tmax) ) then
!        t_max = 1.0e+14
!    else
!        t_max = tmax
!    endif
!    
!    if(.not.present(valx) ) then
!        SetTBCound(1)=0.0d0
!    else
!        SetTBCound(1)=valx
!    endif
!    if(.not.present(valy) ) then
!        SetTBCound(2)=0.0d0
!    else
!        SetTBCound(2)=valy
!    endif    
!    if(.not.present(valz) ) then
!        SetTBCound(3)=0.0d0
!    else
!        SetTBCound(3)=valz
!    endif
!
!
!    ! get node ID and value
!    allocate(TBoundNodIDBuf(size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%TBoundNodID,2) ) )
!    allocate(TBoundValBuf  (size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%TBoundNodID,2) ) )
!    TBoundNodIDBuf(:,:) = -1
!    TBoundValBuf(:,:)   = -1.0d0
!
!
!
!    k=0
!    do i=1,size(obj%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=obj%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(obj%Mesh%NodCoord,2)
!            if(j==1)then
!                minline=x_min
!                maxline=x_max
!            elseif(j==2)then
!                minline=y_min
!                maxline=y_max
!            elseif(j==3)then
!                minline=z_min
!                maxline=z_max
!            elseif(j==4)then
!                minline=t_min
!                maxline=t_max
!            else
!                !print *, "ERROR :: EditClass >> AddTBoundCondition >> dimension should 0 < d < 5"
!            endif
!            if(minline <= obj%Mesh%NodCoord(node_id,j) .and. obj%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(obj%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(obj%Mesh%NodCoord,2)
!                if(j==1)then
!                    if(.not.present(valx) ) then
!                        TBoundNodIDBuf(k,1)=-1
!                    else
!                        TBoundNodIDBuf(k,1)=node_id
!                        TBoundValBuf(k,1)=valx
!                    endif
!                elseif(j==2)then
!                    if(.not.present(valy) ) then
!                        TBoundNodIDBuf(k,2)=-1
!                    else
!                        TBoundNodIDBuf(k,2)=node_id
!                        TBoundValBuf(k,2)=valy
!                    endif    
!                elseif(j==3)then
!                    if(.not.present(valz) ) then
!                        TBoundNodIDBuf(k,3)=-1
!                    else
!                        TBoundNodIDBuf(k,3)=node_id
!                        TBoundValBuf(k,3)=valz
!                    endif
!                else
!                    stop "EditClass >Time domain is not implemented "
!                endif
!            enddo   
!        endif
!    enddo
!
!    
!    ! MergeArray
! 
!    call TrimArray(TBoundNodIDBuf,k)
!    call TrimArray(TBoundValBuf,k)
!    call CopyArray(obj%Boundary%TBoundNodID,CopiedArrayInt)
!    call CopyArray(obj%Boundary%TBoundVal,CopiedArrayReal)
!    call MergeArray(CopiedArrayInt,TBoundNodIDBuf,obj%Boundary%TBoundNodID)
!    call MergeArray(CopiedArrayReal,TBoundValBuf,obj%Boundary%TBoundVal)
!    call DeleteOverlapBoundary(obj%Boundary)
!    call InitializeBoundary(obj%Boundary)
!    
!
!
!end subroutine 
!!##################################################




!##################################################
subroutine SetSolver(obj,inSolverType)
    class(FEMDomain_),intent(inout)::obj
    character*200,intent(in) :: inSolverType

    obj%SolverType=inSolverType

end subroutine
!##################################################

!##################################################
subroutine SetName(obj,Name)
    class(FEMDomain_),intent(inout)::obj
    character(*),intent(in) :: Name

    obj%FileName=Name

end subroutine
!##################################################


!##################################################
subroutine SetDataType(obj,inDType)
    class(FEMDomain_),intent(inout)::obj
    character*200,intent(in) :: inDType

    obj%DType = inDType

end subroutine

!##################################################


!##################################################
subroutine SetUpFEMDomain(obj)
    class(FEMDomain_),intent(inout)::obj

    logical :: NodeExist
    logical :: ElementExist

    if(allocated(obj%Mesh%NodCoord)  )then
        NodeExist = .true.
    else
        NodeExist = .false.
    endif

    if(allocated(obj%Mesh%ElemNod)  )then
        ElementExist = .true.
    else
        ElementExist = .false.
    endif

    if( NodeExist .eqv. .false. )then
        print *, "ERROR :: SetUp FEMDomain_ >> No Nodes are imported"
        return
    endif
    if( ElementExist .eqv. .false. )then
        print *, "ERROR :: SetUp FEMDomain_ >> No Elements are imported"
        return
    endif





end subroutine
!##################################################


!##################################################
subroutine SetControlParaFEMDomain(obj,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
    class(FEMDomain_),intent(inout)::obj
    real(real64),optional,intent(in)::OptionalTol
    integer(int32),optional,intent(in)::OptionalSimMode,OptionalItrTol,OptionalTimestep
    
    call SetControlPara(obj%ControlPara,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
end subroutine
!##################################################


!##################################################
subroutine AddMaterialID(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,MaterialID ,mode2D)
    class(FEMDomain_),intent(inout)::obj
    real(real64),optional,intent(in)::xmin,xmax
    real(real64),optional,intent(in)::ymin,ymax
    real(real64),optional,intent(in)::zmin,zmax
    real(real64),optional,intent(in)::tmin,tmax
    
    integer(int32),optional,intent(in)::MaterialID
    real(real64)::x_min,x_max
    real(real64)::y_min,y_max
    real(real64)::z_min,z_max
    real(real64)::t_min,t_max
    
    real(real64),optional,intent(in)::valx,valy,valz


    logical,optional,intent(in) :: Mode2D
    logical :: InOut
    real(real64) :: minline,maxline,SetDBCound(3)
    integer(int32),allocatable::TBoundNodITBuf(:,:),CopiedArrayInt(:,:)
    real(real64),allocatable::TBoundValBuf(:,:),CopiedArrayReal(:,:),x(:),rmin(:),rmax(:)
    integer(int32) :: countnum,i,j,k,node_id,n,m,NumVN,newboundnum,ValID,md

    if(present(MaterialID) )then
        md=MaterialID
    else
        md=1
    endif


    n=size(obj%Mesh%NodCoord,2)
    
    if( present(Mode2D) )then
        if(Mode2D .eqv. .true.)then
            allocate(x(2) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        else
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        endif
    elseif(n==2)then
        allocate(x(2) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    else
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    endif

    if(.not.present(xmin) ) then
        x_min = -1.0e+14
        
    else
        x_min=xmin
    endif
    if(.not.present(xmax) ) then
        x_max = 1.0e+14
    else
        x_max=xmax
    endif

    if(.not.present(ymin) ) then
        y_min = -1.0e+14
    else
        y_min=ymin
    endif
    if(.not.present(ymax) ) then
        y_max = 1.0e+14
    else
        y_max=ymax
    endif

    if(.not.present(zmin) ) then
        z_min = -1.0e+14
    else
        z_min = zmin
    
    endif
    if(.not.present(zmax) ) then
        z_max = 1.0e+14
    else
        z_max=zmax
    endif
    
    if(.not.present(tmin) ) then
        t_min = -1.0e+14
    else
        t_min = tmin
    endif
    if(.not.present(tmax) ) then
        t_max = 1.0e+14
    else
        t_max = tmax
    endif
    
    

    ! get node ID and value
    
    !if the facet is not created, create facets (surface elements)
    
    
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(obj%Mesh%ElemMat,1)

    do i=1,n
        x(:)=0.0d0
        do j=1,size(obj%Mesh%ElemNod,2)
            x(:)=x(:)+obj%Mesh%NodCoord( obj%Mesh%ElemNod(i,j),: )
        enddo

        x(:)=1.0d0/dble(size(obj%Mesh%ElemNod,2))*x(:)
        
        InOut = InOrOut(x,rmax,rmin)
        if(InOut .eqv. .true.)then
            
            obj%Mesh%ElemMat(i)=md
        endif
        
    enddo


end subroutine
!##################################################


!##################################################
subroutine MeltingSkeltonFEMDomain(obj)
    class(FEMDomain_),intent(inout)::obj

    call obj%Mesh%MeltingSkelton()
    
end subroutine
!##################################################



! #########################################################################################
subroutine GmshPlotMesh(obj,OptionalContorName,OptionalAbb,OptionalStep,Name,withNeumannBC,withDirichletBC&
	,onlyNeumannBC,onlyDirichletBC,asMsh,withMaterial,Tag,timestep)
	class(FEMDomain_),intent(inout)::obj
	real(real64),allocatable::gp_value(:,:)
	integer(int32),optional,intent(in)::OptionalStep,timestep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	character(*),optional,intent(in)::Name,Tag
	logical,optional,intent(in)::withNeumannBC,withDirichletBC,onlyNeumannBC,onlyDirichletBC,asMsh,withMaterial
	real(real64),allocatable::x_double(:,:)
	real(real64),allocatable::x(:,:)
	integer(int32) i,j,k,l,step,fh,nodeid1,nodeid2
	character filename0*11
	character filename*200
	character filetitle*6
	character command*200
	character:: mapname*30,abbmap*6
	


	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	elseif(present(Tag) )then
		mapname=trim(Tag)
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif


	if(present(OptionalStep) )then
		step=OptionalStep
    elseif(present(timeStep) )then
        step=timestep
    else
		step=1
	endif

	fh=123

	filetitle(1:6)=abbmap(1:6)
    
    if(.not.allocated(obj%Mesh%ElemMat) )then
        allocate(obj%Mesh%ElemMat(size(obj%Mesh%ElemNod,1) ) )
        obj%Mesh%ElemMat(:)=1
    endif

	!---------------------
	write (filename0, '("_", i6.6, ".pos")') step ! ここでファイル名を生成している
	if(present(Name) )then
		filename=filename0
		
		!call system(  "touch "//trim(adjustl(name))//trim(obj%FileName)//trim(filename) )
		open(fh,file=trim(adjustl(name))//trim(filetitle)//trim(filename) )
		print *, "writing ",trim(adjustl(name))//trim(filetitle)//trim(filename)," step>>",step
	else
		filename=filename0
		!call system(  "touch "//trim(obj%FileName)//trim(filename) )
		!print *, trim(obj%FileName)//trim(filetitle)//trim(filename)
		open(fh,file=trim(obj%FileName)//trim(filetitle)//trim(filename) )
		print *, "writing ",trim(obj%FileName)//trim(filetitle)//trim(filename)," step>>",step
	endif
	
	
	!---------------------
	if( size(obj%Mesh%ElemNod,2)==4 .and. size(obj%Mesh%NodCoord,2)==2 ) then
		allocate(x(4,3) )
		allocate(x_double(4,3) )
		x(:,:)=0.0d0
		x_double(:,:)=0.0d0
	elseif( size(obj%Mesh%ElemNod,2)==8 .and. size(obj%Mesh%NodCoord,2)==3 ) then
		allocate(x(8,3) )
		allocate(x_double(8,3) )
		x(:,:)=0.0d0
		x_double(:,:)=0.0d0
		
	endif

	allocate(gp_value( size(obj%Mesh%ElemNod,1),size(obj%Mesh%ElemNod,2) ))
	if(allocated(obj%Mesh%ElemMat) )then
		do i=1,size(obj%Mesh%ElemMat,1)
			gp_value(i,:)=dble(obj%Mesh%ElemMat(i))
		enddo
	else
		gp_value(i,:)=0.0d0
	endif

	if(present(withDirichletBC) )then
		if(withDirichletBC .eqv. .true. )then
			! search Dirichlet BC and change color
			if(.not. allocated(obj%Boundary%DBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> withDirichletBC >> no NBC is found."
				return
			else
				print *, "[ok] GmshPlotMesh",trim(filename)," is exported withDirichletBC. The value is:",maxval(obj%Mesh%ElemMat(:))+40
				
			endif
			do i=1,size(obj%Boundary%DBoundNodID,1 )
				do j=1,size(obj%Boundary%DBoundNodID,2)
					
					if(obj%Boundary%DBoundNodID(i,j)>0 )then
						nodeid1=obj%Boundary%DBoundNodID(i,j)
					else
						cycle
					endif

					do k=1,size(obj%Mesh%ElemNod,1)
						do l=1,size(obj%Mesh%ElemNod,2)
							nodeid2=obj%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								gp_value(k,:)=dble(maxval(obj%Mesh%ElemMat(:)))+40.0d0 ! Dirichlet is +20
							endif
						enddo
					enddo
				enddo
			enddo

		endif
	endif

	if(present(withNeumannBC) )then
		if(withNeumannBC .eqv. .true. )then
			! search Neumann BC and change color
			if(.not. allocated(obj%Boundary%NBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> withNeumannBC >> no NBC is found."
				return
			else
				print *, "[ok] GmshPlotMesh",trim(filename)," is exported withNeumannBC. The value is:",maxval(obj%Mesh%ElemMat(:))+20
				
			endif
			do i=1,size(obj%Boundary%NBoundNodID,1 )
				do j=1,size(obj%Boundary%NBoundNodID,2)
					
					if(obj%Boundary%NBoundNodID(i,j)>0 .and. obj%Boundary%NBoundVal(i,j)/=0.0d0)then
						nodeid1=obj%Boundary%NBoundNodID(i,j)
					else
						cycle
					endif

					do k=1,size(obj%Mesh%ElemNod,1)
						do l=1,size(obj%Mesh%ElemNod,2)
							nodeid2=obj%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								gp_value(k,:)=dble(maxval(obj%Mesh%ElemMat(:)))+20.0d0 ! neumann is +20
							endif
						enddo
					enddo
				enddo
			enddo

		endif
	endif


	if(present(onlyDirichletBC) )then
		if(onlyDirichletBC .eqv. .true. )then
			! search Dirichlet BC and change color
			if(.not. allocated(obj%Boundary%DBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> onlyDirichletBC >> no NBC is found."
				return
			else
				print *, "[ok] GmshPlotMesh",trim(filename)," is exported onlyDirichletBC. The value is:",maxval(obj%Mesh%ElemMat(:))+40
				
			endif
			do i=1,size(obj%Boundary%DBoundNodID,1 )
				do j=1,size(obj%Boundary%DBoundNodID,2)
					
					if(obj%Boundary%DBoundNodID(i,j)>0 )then
						nodeid1=obj%Boundary%DBoundNodID(i,j)
					else
						cycle
					endif

					
					do k=1,size(obj%Mesh%ElemNod,1)
						do l=1,size(obj%Mesh%ElemNod,2)
							nodeid2=obj%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								if(l>size(gp_value,2) )then
									exit
								endif
								gp_value(k,l)=obj%Boundary%DBoundVal(i,j)
							endif
						enddo
					enddo
				enddo
			enddo

		endif
	endif

			

	x(:,:)=0.0d0
	write(fh,*) 'View "',mapname,'" {'
	do i=1,size(gp_value,1)
		if( size(obj%Mesh%ElemNod,2)==4 .and. size(obj%Mesh%NodCoord,2)==2 ) then
			
			! 2-D, 4 noded, isoparametric elements with four gauss points 
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )

			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
				

			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )

			
			x(:,:)=x_double(:,:) 

			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
				
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			
			x(:,:)=x_double(:,:) 
			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			
		elseif(size(obj%Mesh%ElemNod,2)==8 .and. size(obj%Mesh%NodCoord,2)==3  ) then
			
			! 3-D, 8 noded, isoparametric elements with 8 gauss points
			! 1/8

			x_double(1,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			x_double(3,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )
			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			x_double(5,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(6,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			
			x_double(7,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			
			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			
			! 2/8

			x_double(1,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			
			x_double(2,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			
			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(5,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			
			x_double(7,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			
			
			! 3/8

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(6,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			
			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				

			! 4/8

			x_double(6,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )
			
			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(8,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			



			! 5/8

			x_double(7,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(5,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(1,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			
			! 6/8

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(6,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			
			x_double(5,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(3,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )

			
			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(4,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			

			
			! 7/8

			x_double(5,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(2,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(1,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			

			

			
			! 8/8

			x_double(5,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(4,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(1,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			




        else
            print *, " size(obj%Mesh%ElemNod,2)==",size(obj%Mesh%ElemNod,2)
            print *, ".and. size(obj%Mesh%NodCoord,2)==",size(obj%Mesh%NodCoord,2)
			stop "plot_contour >> now constructing"
		endif
	enddo
	write(fh,*) '};'
	close(fh)
 end subroutine
 !===========================================================================================



! ########################################################################################
subroutine GmshPlotContour(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep,Name)
	class(FEMDomain_),intent(in)::obj
	real(real64),intent(in)::gp_value(:,:)
	integer(int32),optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	character(*),optional,intent(in)::Name
	real(real64),allocatable::x_double(:,:)
	real(real64),allocatable::x(:,:)
	integer(int32) i,j,k,step,fh
	character filename0*11
	character filename*25
	character filetitle*6
	character command*31
	character:: mapname*30,abbmap*6
	
	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif

	if(present(OptionalStep) )then
		step=OptionalStep
	else
		step=1
	endif
	fh=40

	filetitle(1:6)=abbmap(1:6)
	!---------------------
	write (filename0, '("_", i6.6, ".pos")') step ! ここでファイル名を生成している
	filename=filename0
	!command="touch "//trim(obj%FileName)//trim(filename)
	!call system("touch "//trim(obj%FileName)//trim(filename))

	open(fh,file=trim(obj%FileName)//trim(filetitle)//trim(filename))
	print *, "writing ",trim(obj%FileName)//trim(filetitle)//trim(filename)," step>>",step
	
	!---------------------
	if( size(obj%Mesh%ElemNod,2)==4 .and. size(obj%Mesh%NodCoord,2)==2 ) then
		allocate(x(4,3) )
		allocate(x_double(4,3) )
		
	elseif( size(obj%Mesh%ElemNod,2)==8 .and. size(obj%Mesh%NodCoord,2)==3 ) then
		allocate(x(8,3) )
		allocate(x_double(8,3) )
		
	endif


	x(:,:)=0.0d0
	write(fh,*) 'View "',mapname,'" {'
	do i=1,size(gp_value,1)
		if( size(obj%Mesh%ElemNod,2)==4 .and. size(obj%Mesh%NodCoord,2)==2 ) then
			
			! 2-D, 4 noded, isoparametric elements with four gauss points 
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )

			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
				

			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )

			
			x(:,:)=x_double(:,:) 

			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
				
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			
			x(:,:)=x_double(:,:) 
			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
		elseif(size(obj%Mesh%ElemNod,2)==8 .and. size(obj%Mesh%NodCoord,2)==3  ) then
			
			
			! 3-D, 8 noded, isoparametric elements with 8 gauss points
			! 1/8

			x_double(1,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			x_double(3,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )
			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )
			
			x_double(5,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(6,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			
			x_double(7,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			
			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			
			! 2/8

			x_double(1,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			
			x_double(2,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			
			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(5,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			
			x_double(7,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			
			
			! 3/8

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(6,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			
			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				

			! 4/8

			x_double(6,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )
			
			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(8,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			



			! 5/8

			x_double(7,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(5,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(1,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			
			! 6/8

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(6,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			
			x_double(5,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(3,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )

			
			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(4,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			

			
			! 7/8

			x_double(5,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(2,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(1,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			

			

			
			! 8/8

			x_double(5,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(4,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(1,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			




		else
			stop "plot_contour >> now constructing"
		endif
	enddo
	write(fh,*) '};'
	close(fh)
 end subroutine
!===========================================================================================

!===========================================================================================
subroutine GmshPlotVector(obj,Vector,Name,FieldName,Step,fh,withMsh,ElementWize,NodeWize,onlyDirichlet)
	class(FEMDomain_),intent(in)::obj
	real(real64),optional,intent(in)::Vector(:,:)
	character(*),intent(in)::FieldName
	character(*),optional,intent(in)::Name
	integer(int32),intent(in)::Step
	real(real64),allocatable ::DBCVector(:,:) 
	integer(int32),optional,intent(in)::fh
	logical,optional,intent(in)::withMsh,ElementWize,NodeWize,onlyDirichlet

	character :: filename0*11, filename1*11,center*15
	integer(int32) :: FileHandle,i,j,k,n,m
	FileHandle=input(default=1000,option=fh)


	if(present(onlyDirichlet) )then
		if(onlyDirichlet .eqv. .true.)then
			
			call obj%getDBCVector(DBCVector)
			do i=1,size(DBCVector,1)
				write(10,*) DBCVector(i,:)
			enddo

			center="$NodeData"
		
			! only for 3D
		
			write (filename0, '("_", i6.6, "_vec")') step 
			if(present(Name) )then
				open(FileHandle,file=Name//filename0//".msh")
				print *, Name//filename0//".msh"//" is exported!"
			else
				open(FileHandle,file="DBCVector"//filename0//".msh")
				print *, "DBCVector"//filename0//".msh"//" is exported!"
			endif
			write(FileHandle,'(A)') "$MeshFormat"
		
			write(FileHandle,'(A)')  "2.2 0 8"
			write(FileHandle,'(A)')  "$EndMeshFormat"
			write(FileHandle,'(A)')  trim(center)
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  '"'//FieldName//'"'
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  "0.0"
			write(FileHandle,'(A)')  "3"
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  "3"
			write(FileHandle,*) 	size(obj%Mesh%NodCoord,1)  
			do i=1,size(obj%Mesh%NodCoord,1)
				write(FileHandle,*) i,DBCVector(i,:)
			enddo

			close(FileHandle)
		
			if(present(withMsh) )then
				if(withMsh .eqv. .true.)then

				
				
					write (filename1, '("_", i6.6, "_msh")') step
					if(present(Name) )then
						open(FileHandle,file=Name//filename1//".msh")
						print *, Name//filename1//".msh"//" is exported!"
					else
						open(FileHandle,file="DBCVector"//filename1//".msh")
						print *, "DBCVector"//filename1//".msh"//" is exported!"
					endif
					write(FileHandle,'(A)') "$MeshFormat"
				
					write(FileHandle,'(A)')  "2.2 0 8"
					write(FileHandle,'(A)')  "$EndMeshFormat"
					write(FileHandle,'(A)')  "$Nodes"
					write(FileHandle,*) 	size(obj%Mesh%NodCoord,1)  
					do i=1,size(obj%Mesh%NodCoord,1)
						write(FileHandle,*) i,obj%Mesh%NodCoord(i,:)
					enddo
					write(FileHandle,'(A)')  "$EndNodes"
					write(FileHandle,'(A)')  "$Elements"
					write(FileHandle,*) 	size(obj%Mesh%ElemNod,1)
					do i=1,size(obj%Mesh%ElemNod,1)
						write(FileHandle,*) i,"5 2 0 1 ",obj%Mesh%ElemNod(i,:)
					enddo  
					write(FileHandle,'(A)')  "$EndElements"

					close(FileHandle)
				endif
			endif
			return
		
			
		endif
	endif
	
	if(present(NodeWize) )then
		if(NodeWize .eqv. .true.)then
			center="$NodeData"
		else
			center="$ElementData"
		endif
	elseif(present(ElementWize))then
		if(ElementWize .eqv. .true.)then
			center="$ElementData"
		else
			center="$NodeData"
		endif
	else
		center="$NodeData"
	endif

	! only for 3D

	write (filename0, '("_", i6.6, "_vec")') step 
	if(present(Name) )then
		open(FileHandle,file=Name//filename0//".msh")
		print *, Name//filename0//".msh"//" is exported!"
	else
		open(FileHandle,file="Vector"//filename0//".msh")
		print *, "Vector"//filename0//".msh"//" is exported!"
	endif
	write(FileHandle,'(A)') "$MeshFormat"

	write(FileHandle,'(A)')  "2.2 0 8"
	write(FileHandle,'(A)')  "$EndMeshFormat"
	write(FileHandle,'(A)')  trim(center)
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  '"'//FieldName//'"'
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  "0.0"
	write(FileHandle,'(A)')  "3"
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  "3"
	write(FileHandle,*) 	size(obj%Mesh%NodCoord,1)  
	do i=1,size(obj%Mesh%NodCoord,1)
		write(FileHandle,*) i,Vector(i,:)
	enddo
	
	close(FileHandle)

	if(present(withMsh) )then
		if(withMsh .eqv. .true.)then
			
		
		
			write (filename1, '("_", i6.6, "_msh")') step
			if(present(Name) )then
				open(FileHandle,file=Name//filename1//".msh")
				print *, Name//filename1//".msh"//" is exported!"
			else
				open(FileHandle,file="Vector"//filename1//".msh")
				print *, "Vector"//filename1//".msh"//" is exported!"
			endif
			write(FileHandle,'(A)') "$MeshFormat"
		
			write(FileHandle,'(A)')  "2.2 0 8"
			write(FileHandle,'(A)')  "$EndMeshFormat"
			write(FileHandle,'(A)')  "$Nodes"
			write(FileHandle,*) 	size(obj%Mesh%NodCoord,1)  
			do i=1,size(obj%Mesh%NodCoord,1)
				write(FileHandle,*) i,obj%Mesh%NodCoord(i,:)
			enddo
			write(FileHandle,'(A)')  "$EndNodes"
			write(FileHandle,'(A)')  "$Elements"
			write(FileHandle,*) 	size(obj%Mesh%ElemNod,1)
			do i=1,size(obj%Mesh%ElemNod,1)
				write(FileHandle,*) i,"5 2 0 1 ",obj%Mesh%ElemNod(i,:)
			enddo  
			write(FileHandle,'(A)')  "$EndElements"
			
			close(FileHandle)
		endif
	endif

	
	
end subroutine
!===========================================================================================


subroutine GmshPlotContour2D(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep,Name)
	class(FEMDomain_),intent(in)::obj
	real(real64),intent(in)::gp_value(:,:)
	integer(int32),optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	character(*),optional,intent(in)::Name
	real(real64),allocatable::x(:,:)
	integer(int32) i,j,k,step
	character filename0*11
	character filename*17
	character filetitle*6
	character:: mapname*30,abbmap*6
	
	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif

	if(present(OptionalStep) )then
		step=OptionalStep
	else
		step=1
	endif


	filetitle(1:6)=abbmap(1:6)
	!---------------------
	write (filename0, '("_", i6.6, ".pos")') step ! ここでファイル名を生成している
	filename=filename0


	open(40,file=trim(obj%FileName)//trim(filetitle)//filename0)
	print *, "writing ",trim(obj%FileName)//trim(filetitle)//filename0," step>>",step	

	!---------------------
	allocate(x(4,3) )
	x(:,:)=0.0d0
	write(40,*) 'View "',mapname,'" {'
	do i=1,size(obj%Mesh%ElemNod,1)
		if( size(obj%Mesh%ElemNod,2)/=4)  stop  "GmshPlotContour >> now constructing"
		
		
		x(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
		x(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
		x(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
			gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			
		x(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
		x(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
		x(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
			gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			
		x(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
		x(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
			gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			
		x(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
		x(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
			gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
	enddo
	write(40,*) '};'
 end subroutine GmshPlotContour2D
 !===========================================================================================
subroutine GmshExportStress(obj,uvec,sigma,strain_measure,step,Name )
	class(FEMDomain_),intent(in)::obj
	real(real64),intent(in)::uvec(:),sigma(:,:,:),strain_measure(:,:,:)
	integer(int32),intent(in)::step
	character p_stress_field*30
	real(real64),allocatable::c_nod_coord(:,:),gp_value(:,:),F_iJ(:,:),b_ij(:,:)
	real(real64) tr_sigma,tr_C,tr_b
	character q_stress_field*30
	character p_strain_field*30
	character q_strain_field*30
	character mapname*30,abbrivation*6
	character(*),optional,intent(in)::Name
	integer(int32) i,j,n,gp_number,dim_num
	
	gp_number=size(strain_measure,2)
	dim_num=size(obj%Mesh%NodCoord,2)
	
	p_stress_field="Hydrostatic stress (kPa)"
	q_stress_field="Deviatoric stress (kPa)"
	p_strain_field="Hydrostatic strain "
	q_strain_field="Deviatoric strain "
	
	allocate(F_iJ(3,3),b_ij(3,3) )
	
	allocate( c_nod_coord(size(obj%Mesh%NodCoord,1),size(obj%Mesh%NodCoord,2))) 
	allocate(gp_value(size(obj%Mesh%ElemNod,1),gp_number  ))
	
	do i=1,size(obj%Mesh%NodCoord,1)
		c_nod_coord(i,:)=obj%Mesh%NodCoord(i,:)+uvec(dim_num*(i-1)+1:dim_num*i )
	enddo
	
	!!"Hydrostatic stress (kPa)"
	do i=1,size(sigma,1)
		do j=1,size(sigma,2)
			if(dim_num==2)then
				gp_value(i,j)=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,4)
				gp_value(i,j)=gp_value(i,j)/3.0d0
			elseif(dim_num==3)then
				gp_value(i,j)=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,3)
				gp_value(i,j)=gp_value(i,j)/3.0d0
			endif
		enddo
	enddo
	mapname=p_stress_field
	abbrivation="Hysigm"
	call GmshPlotContour(obj,gp_value,mapname,abbrivation,step,Name=Name)
	
	!!""Deviatoric stress (kPa)"
	do i=1,size(sigma,1)
		do j=1,size(sigma,2)
			if(dim_num==2)then
				tr_sigma=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,4)

				gp_value(i,j)=( 1.50d0*( sigma(i,j,1)*sigma(i,j,1) +&
				sigma(i,j,2)*sigma(i,j,2) +sigma(i,j,4)*sigma(i,j,4) + sigma(i,j,3)*sigma(i,j,3)*2.0d0 -&
				tr_sigma*tr_sigma/3.0d0)   )**(0.50d0)
			elseif(dim_num==3)then
				
				gp_value(i,j)=( 1.50d0*( sigma(i,j,1)*sigma(i,j,1) +&
				sigma(i,j,2)*sigma(i,j,2) +sigma(i,j,3)*sigma(i,j,3) + sigma(i,j,4)*sigma(i,j,4)*3.0d0 &
				+ sigma(i,j,5)*sigma(i,j,5)*3.0d0 + sigma(i,j,6)*sigma(i,j,6)*3.0d0 &
				- sigma(i,j,1)*sigma(i,j,2)- sigma(i,j,2)*sigma(i,j,3)- sigma(i,j,3)*sigma(i,j,1)  )   )**(0.50d0)
			
			else
				stop "dim_num should be 2 or 3, GmshExportStress"
			endif
		enddo
	enddo
	mapname=q_stress_field
	abbrivation="Dvsigm"
	call GmshPlotContour(obj,gp_value,mapname,abbrivation,step,Name=Name)
	
	!!"Hydrostatic strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			F_iJ(:,:)=0.0d0
			if(dim_num==2)then
				F_iJ(1,1)=strain_measure(i,j,11)
				F_iJ(2,1)=strain_measure(i,j,14)
				F_iJ(1,2)=strain_measure(i,j,13)
				F_iJ(2,2)=strain_measure(i,j,12)
				F_iJ(3,3)=1.0d0
			elseif(dim_num==3)then
				F_iJ(1,1)=strain_measure(i,j,11)
				F_iJ(2,1)=strain_measure(i,j,14)
				F_iJ(1,2)=strain_measure(i,j,13)
				F_iJ(2,2)=strain_measure(i,j,12)
				F_iJ(3,3)=strain_measure(i,j,27)
				F_iJ(1,3)=strain_measure(i,j,28)
				F_iJ(2,3)=strain_measure(i,j,29)
				F_iJ(3,1)=strain_measure(i,j,30)
				F_iJ(3,2)=strain_measure(i,j,31)
			else
				stop "dim_num should be 2 or 3"
			endif
			b_ij(:,:)=matmul(F_iJ,transpose(F_iJ) )
			gp_value(i,j)=b_iJ(1,1)+b_iJ(2,2)+b_iJ(3,3)
			gp_value(i,j)=gp_value(i,j)/3.0d0
		enddo
	enddo
	mapname=p_strain_field
	abbrivation="Hyepsi"
	call GmshPlotContour(obj,gp_value,mapname,abbrivation,step,Name=Name)
	
	!!"Deviatoric strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			F_iJ(:,:)=0.0d0
			if(dim_num==2)then
				F_iJ(1,1)=strain_measure(i,j,11)
				F_iJ(2,1)=strain_measure(i,j,14)
				F_iJ(1,2)=strain_measure(i,j,13)
				F_iJ(2,2)=strain_measure(i,j,12)
				F_iJ(3,3)=1.0d0
			elseif(dim_num==3)then
				F_iJ(1,1)=strain_measure(i,j,11)
				F_iJ(2,1)=strain_measure(i,j,14)
				F_iJ(1,2)=strain_measure(i,j,13)
				F_iJ(2,2)=strain_measure(i,j,12)
				F_iJ(3,3)=strain_measure(i,j,27)
				F_iJ(1,3)=strain_measure(i,j,28)
				F_iJ(2,3)=strain_measure(i,j,29)
				F_iJ(3,1)=strain_measure(i,j,30)
				F_iJ(3,2)=strain_measure(i,j,31)
			else
				stop "dim_num should be 2 or 3"
			endif
			
			b_ij(:,:)=matmul(F_iJ,transpose(F_iJ) )
			gp_value(i,j)=( 1.50d0*( b_ij(1,1)*b_ij(1,1) +&
				b_ij(2,2)*b_ij(2,2) +b_ij(3,3)*b_ij(3,3) + b_ij(1,2)*b_ij(1,2)*3.0d0 &
				+ b_ij(2,3)*b_ij(2,3)*3.0d0 + b_ij(3,1)*b_ij(3,1)*3.0d0 &
				- b_ij(1,1)*b_ij(2,2)- b_ij(2,2)*b_ij(3,3)- b_ij(3,3)*b_ij(1,1)  )   )**(0.50d0)
			
		enddo
	enddo
	mapname=p_stress_field
	abbrivation="Dvepsi"
	call GmshPlotContour(obj,gp_value,mapname,abbrivation,step,Name=Name)
	
 end subroutine
 !=======================================================================================
subroutine GnuplotPlotContour(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep)
	class(FEMDomain_),intent(in)::obj
	real(real64),intent(in)::gp_value(:,:)
	integer(int32),optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	real(real64),allocatable::x(:,:)
	integer(int32) i,j,k,step,n
	character filename0*11
	character filename*17
	character filetitle*6
	character:: mapname*30,abbmap*6
	
	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif

	if(present(OptionalStep) )then
		step=OptionalStep
	else
		step=1
	endif


	filetitle(1:6)=abbmap(1:6)
	!---------------------
	write (filename0, '("_", i6.6, ".txt")') step ! ここでファイル名を生成している
	filename=filename0
	open(40,file="touch "//trim(obj%FileName)//filename)
	print *, "writing .gnuplot-txt file... step>>",step
	!---------------------

    do i=1,size(gp_value,1)
        do j=1,size(gp_value,2)
            n=obj%Mesh%ElemNod(i,j)
            write(40,*) obj%Mesh%NodCoord(n,:),&
                gp_value(i,j)
        enddo
    enddo
    close(40)
 end subroutine GnuplotPlotContour
 !===========================================================================================
 
 !===========================================================================================
subroutine GnuplotExportStress(obj,uvec,sigma,strain_measure,step )
	class(FEMDomain_),intent(in)::obj
	real(real64),intent(in)::uvec(:),sigma(:,:,:),strain_measure(:,:,:)
	integer(int32),intent(in)::step
	character p_stress_field*30
	
	real(real64),allocatable::c_nod_coord(:,:),gp_value(:,:)
	real(real64) tr_sigma,tr_C
	character q_stress_field*30
	character p_strain_field*30
	character q_strain_field*30
	character mapname*30,abbrivation*6
	integer(int32) i,j,n,gp_number,dim_num
	
	
	gp_number=size(strain_measure,2)
	dim_num=size(obj%Mesh%NodCoord,2)
	
	p_stress_field="Hydrostatic stress (kPa)"
	q_stress_field="Deviatoric stress (kPa)"
	p_strain_field="Hydrostatic strain "
	q_strain_field="Deviatoric strain "
	
	allocate( c_nod_coord(size(obj%Mesh%NodCoord,1),size(obj%Mesh%NodCoord,2))) 
	allocate(gp_value(size(obj%Mesh%ElemNod,1),gp_number  ))
	
	do i=1,size(obj%Mesh%NodCoord,1)
		c_nod_coord(i,:)=obj%Mesh%NodCoord(i,:)+uvec(dim_num*(i-1)+1:dim_num*i )
	enddo
	
	!!"Hydrostatic stress (kPa)"
	do i=1,size(sigma,1)
		do j=1,size(sigma,2)
			gp_value(i,j)=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,4)
			gp_value(i,j)=gp_value(i,j)/3.0d0
		enddo
	enddo
	mapname=p_stress_field
	abbrivation="Hysigm"
	call GnuplotPlotContour(obj,gp_value,mapname,abbrivation,step)
	
	!!""Deviatoric stress (kPa)"
	do i=1,size(sigma,1)
		do j=1,size(sigma,2)
			tr_sigma=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,4)
			gp_value(i,j)=( 1.50d0*( sigma(i,j,1)*sigma(i,j,1) +&
			sigma(i,j,2)*sigma(i,j,2) +sigma(i,j,4)*sigma(i,j,4) + sigma(i,j,3)*sigma(i,j,3)*2.0d0 -&
			tr_sigma*tr_sigma/3.0d0)   )**(0.50d0)
		enddo
	enddo
	mapname=q_stress_field
	abbrivation="Dvsigm"
	call GnuplotPlotContour(obj,gp_value,mapname,abbrivation,step)
	
	!!"Hydrostatic strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			gp_value(i,j)=strain_measure(i,j,4)+strain_measure(i,j,5)+1.0d0
			gp_value(i,j)=gp_value(i,j)/3.0d0
		enddo
	enddo
	mapname=p_strain_field
	abbrivation="Hyepsi"
	call GnuplotPlotContour(obj,gp_value,mapname,abbrivation,step)
	
	!!"Deviatoric strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			tr_C=strain_measure(i,j,4)+strain_measure(i,j,5)+1.0d0
			gp_value(i,j)=( 1.50d0*( strain_measure(i,j,4)*strain_measure(i,j,4) +&
			strain_measure(i,j,5)*strain_measure(i,j,5) +1.0d0&
			+ strain_measure(i,j,6)*strain_measure(i,j,6)*2.0d0 -&
			tr_C*tr_C/3.0d0)   )**(0.50d0)
		enddo
	enddo
	mapname=p_stress_field
	abbrivation="Dvepsi"
	call GnuplotPlotContour(obj,gp_value,mapname,abbrivation,step)
	
 end subroutine
 !=======================================================================================

! ################################################
subroutine moveFEMDomain(obj,x,y,z)
	class(FEMDomain_),intent(inout)::obj
	real(real64),optional,intent(in)::x,y,z
	
	
	if(present(x) )then
		obj%Mesh%NodCoord(:,1)=obj%Mesh%NodCoord(:,1)+x
	endif


	if(present(y) )then
		obj%Mesh%NodCoord(:,2)=obj%Mesh%NodCoord(:,2)+y
	endif


	if(size(obj%Mesh%NodCoord,2) <3 .and. present(z))then
		print *, "ERROR :: moveFEMDomain >> z cannot be imported"
		return
	endif

	if(present(z) )then
		obj%Mesh%NodCoord(:,3)=obj%Mesh%NodCoord(:,3)+z
	endif
end subroutine
! ################################################
 

! ################################################
subroutine rotateFEMDomain(obj,x,y,z,deg)
	class(FEMDomain_),intent(inout)::obj
	real(real64),optional,intent(in)::x,y,z
	real(real64) ::xd,yd,zd
	real(real64),allocatable :: midpoint(:),rotmat(:,:),rotation(:),coord(:)
	integer(int32) :: i,j,n,m
	logical,optional,intent(in) :: deg


	n=size(obj%Mesh%NodCoord,2)
	m=size(obj%Mesh%NodCoord,1)
	allocate(midpoint(n) )
	allocate(rotmat(n,n) )
	allocate(coord(n) )
	allocate(rotation(n) )
	
	midpoint(:)=0.0d0

	do i=1,m
		midpoint(:)=midpoint(:)+1.0d0/dble(m)*obj%Mesh%NodCoord(i,:)
	enddo

	if(present(x) )then
		do i=1,m

			coord(:)=obj%Mesh%NodCoord(i,:)-midpoint(:)

			rotmat(:,:)=0.0d0
			if(n==2)then
				rotmat(1,1)=cos(x)  ;rotmat(1,2) =-sin(x)    
				rotmat(2,1)=sin(x)   ;rotmat(2,2)= cos(x)  
			elseif(n==3)then
				rotmat(1,1)=1.0d0	;rotmat(1,2)=0.0d0		;rotmat(1,3)=0.0d0			;
				rotmat(2,1)=0.0d0	;rotmat(2,2)=cos(x)		;rotmat(2,3)=-sin(x)		;
				rotmat(3,1)=0.0d0	;rotmat(3,2)=sin(x)		;rotmat(3,3)= cos(x)		;
			else
				print *,"Error :: rotateFEMDomain:: size(obj%Mesh%NodCoord,2)=",n
				stop 
			endif

			rotation(:)=matmul(rotmat,coord)

			obj%Mesh%NodCoord(i,:)=midpoint(:)+rotation(:)
			
		enddo
		
	endif


	if(present(y) )then
		do i=1,m
			coord(:)=obj%Mesh%NodCoord(i,:)-midpoint(:)

			rotmat(:,:)=0.0d0
			if(n==2)then
				rotmat(1,1)=cos(y)  ;rotmat(1,2) =-sin(y)    
				rotmat(2,1)=sin(y)   ;rotmat(2,2)= cos(y)  
			elseif(n==3)then
				rotmat(1,1)=cos(y)	;rotmat(1,2)=0.0d0		;rotmat(1,3)=sin(y)			;
				rotmat(2,1)=0.0d0	;rotmat(2,2)=1.0d0		;rotmat(2,3)=0.0d0		;
				rotmat(3,1)=-sin(y)	;rotmat(3,2)=0.0d0		;rotmat(3,3)= cos(y)		;
			else
				print *,"Error :: rotateFEMDomain:: size(obj%Mesh%NodCoord,2)=",n
				stop 
			endif

			rotation(:)=matmul(rotmat,coord)

			obj%Mesh%NodCoord(i,:)=midpoint(:)+rotation(:)
			
		enddo
		
	endif


	if(size(obj%Mesh%NodCoord,2) <3 .and. present(z))then
		print *, "ERROR :: moveFEMDomain >> z cannot be imported"
		return
	endif

	if(present(z) )then
		do i=1,m
			coord(:)=obj%Mesh%NodCoord(i,:)-midpoint(:)

			rotmat(:,:)=0.0d0
			if(n==2)then
				rotmat(1,1)=cos(z)  ;rotmat(1,2) =-sin(z)    
				rotmat(2,1)=sin(z)   ;rotmat(2,2)= cos(z)  
			elseif(n==3)then
				rotmat(1,1)=cos(z)	;rotmat(1,2)=-sin(z)	;rotmat(1,3)=0.0d0		;
				rotmat(2,1)=sin(z)	;rotmat(2,2)=cos(z)		;rotmat(2,3)=0.0d0		;
				rotmat(3,1)=0.0d0	;rotmat(3,2)=0.0d0		;rotmat(3,3)=1.0d0 		;
			else
				print *,"Error :: rotateFEMDomain:: size(obj%Mesh%NodCoord,2)=",n
				stop 
			endif

			rotation(:)=matmul(rotmat,coord)

			obj%Mesh%NodCoord(i,:)=midpoint(:)+rotation(:)
			
		enddo
	endif

end subroutine
! ################################################



! ################################################
subroutine AddNBCFEMDomain(obj,NodID,DimID,Val,FastMode)
	class(FEMDomain_),intent(inout)::obj
	integer(int32),intent(in)::NodID,DimID
	real(real64),intent(in)::Val
	logical,optional,intent(in)::FastMode
	integer(int32) :: installed,i,j,n
	logical :: fmode

	if(present(FastMode) )then
		fmode=FastMode
	else
		fmode=.false.
	endif
	fmode = input(default=.false.,option=FastMode)

	if(.not.allocated(obj%Boundary%NBoundNodID))then
		print *, "ERROR  :: AddNBC >> obj%Boundary%NBoundNodID should be allocated."
		print *, "Initializing NBC..."
		call obj%InitNBC(NumOfValPerNod=3)
		return
	endif

	! check wheather NodID exisits or not
	! if obj%Boundary%NBoundNodID(NodID) is found, add the current Val to the last value and return.
	do i=1,size(obj%Boundary%NBoundNodID,1)
		if(obj%Boundary%NBoundNodID(i,DimID)==NodID)then
			obj%Boundary%NBoundVal(i,DimID)=obj%Boundary%NBoundVal(i,DimID)+Val
			return
		endif 
	enddo
	
	if(fmode .eqv. .false.)then
		installed=0
		do i=1,size(obj%Boundary%NBoundNodID,1)
			if(obj%Boundary%NBoundNodID(i,DimID)==-1  )then
				obj%Boundary%NBoundNodID(i,DimID)=NodID
				obj%Boundary%NBoundVal(i,DimID)=Val
				obj%Boundary%NBoundNum(DimID)=obj%Boundary%NBoundNum(DimID)+1
				installed=1
				exit
			else
				cycle
			endif
		enddo
	endif

	if(installed==1)then
		return
	else
		n=size(obj%Boundary%NBoundNodID,1)
		call insertArray(obj%Boundary%NBoundNodID ,insert1stColumn=.true.,DefaultValue=-1 ,NextOf=n)
		call insertArray(obj%Boundary%NBoundVal ,insert1stColumn=.true.,DefaultValue=0.0d0,NextOf=n)
		i=n+1
		obj%Boundary%NBoundNodID(i,DimID)=NodID
		obj%Boundary%NBoundVal(i,DimID)=Val
		obj%Boundary%NBoundNum(DimID)=obj%Boundary%NBoundNum(DimID)+1
		
	endif

end subroutine
! ################################################

subroutine ExportFEMDomainAsSTL(obj,FileHandle,MeshDimension,FileName)
	class(FEMDomain_),intent(inout)::obj
	integer(int32),optional,intent(in)::FileHandle,MeshDimension
	character(*),optional,intent(in)::FileName
	real(real64) :: x1(3),x2(3),x3(3)
	character*11  :: filename0
	integer(int32) :: fh,i,dim_num

	if(present(FileName) )then
	
		dim_num=input(default=3,option=MeshDimension)

		if(present(FileHandle) )then
			fh=FileHandle
		else
			fh =104
		endif
	
		write (filename0, '("_", i6.6, ".stl")') obj%Timestep ! ここでファイル名を生成している
		call system(  "touch "//trim(FileName)//trim(filename0) )
		print *, trim(FileName)//trim(filename0)
	
		open(fh,file=trim(FileName)//trim(filename0) )
	
		call obj%Mesh%GetSurface()
		
		if(dim_num/=3)then
			print *, "Sorry, Export stl is supported only for 3-D mesh"
			close(fh)
			return
		endif
		write(fh,'(A)') "solid "//trim(FileName)
		print *, "Number of facet is",size(obj%Mesh%FacetElemNod,1)
		do i=1,size(obj%Mesh%FacetElemNod,1)
			if(size(obj%Mesh%FacetElemNod,2)==4  )then
				! rectangular
				! describe two triangular
				
				x1(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,2),: )
				x3(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),: )
				write(fh,'(A)') "facet normal 0.0 0.0 1.0"
				write(fh,'(A)') "outer loop"
				write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
				write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
				write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
				write(fh,'(A)') "endloop"
				write(fh,'(A)') "endfacet"
				x1(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),: )
				x3(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,4),: )
				write(fh,'(A)') "facet normal 0.0 0.0 1.0"
				write(fh,'(A)') "outer loop"
				write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
				write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
				write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
				write(fh,'(A)') "endloop"
				write(fh,'(A)') "endfacet"
			elseif(size(obj%Mesh%FacetElemNod,2)==3  )then
				! rectangular
				! describe two triangular
				x1(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,2),: )
				x3(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),: )
				write(fh,'(A)') "facet normal 0.0 0.0 1.0"
				write(fh,'(A)') "outer loop"
				write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
				write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
				write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
				write(fh,'(A)') "endloop"
				write(fh,'(A)') "endfacet"
				
			else
				! other
				print *, "Sorry, Export stl is supported only for rectangular mesh"
				return
				close(fh)
			endif
		enddo
		write(fh,'(A)') "endsolid "//trim(FileName)
	
		print *, "writing ",trim(FileName)//trim(filename0)," step>>",obj%Timestep
		flush(fh)
		close(fh)
		return
		
	endif


	dim_num=input(default=3,option=MeshDimension)

    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh =104
    endif

	write (filename0, '("_", i6.6, ".stl")') obj%Timestep ! ここでファイル名を生成している
	call system(  "touch "//trim(obj%FileName)//trim(filename0) )
	print *, trim(obj%FileName)//trim(filename0)

	open(fh,file=trim(obj%FileName)//trim(filename0) )

	call obj%Mesh%GetSurface()
	
	if(dim_num/=3)then
		print *, "Sorry, Export stl is supported only for 3-D mesh"
		close(fh)
		return
	endif
	write(fh,'(A)') "solid "//trim(obj%FileName)
	print *, "Number of facet is",size(obj%Mesh%FacetElemNod,1)
	do i=1,size(obj%Mesh%FacetElemNod,1)
		if(size(obj%Mesh%FacetElemNod,2)==4  )then
			! rectangular
			! describe two triangular
			x1(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),: ) 
			x2(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,2),: )
			x3(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),: )
			write(fh,'(A)') "facet normal 0.0 0.0 1.0"
			write(fh,'(A)') "outer loop"
			write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
			write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
			write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
			write(fh,'(A)') "endloop"
			write(fh,'(A)') "endfacet"
			x1(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),: ) 
			x2(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),: )
			x3(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,4),: )
			write(fh,'(A)') "facet normal 0.0 0.0 1.0"
			write(fh,'(A)') "outer loop"
			write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
			write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
			write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
			write(fh,'(A)') "endloop"
			write(fh,'(A)') "endfacet"
		else
			! other
			print *, "Sorry, Export stl is supported only for rectangular mesh"
			return
			close(fh)
		endif
	enddo
	write(fh,'(A)') "endsolid "//trim(obj%FileName)

	print *, "writing ",trim(obj%FileName)//trim(filename0)," step>>",obj%Timestep
	flush(fh)
	close(fh)

end subroutine


!#######################################
subroutine meshingFEMDomain(obj)
	class(FEMDomain_),intent(inout)::obj

	call obj%Mesh%meshing()
end subroutine
!#######################################


!#######################################
subroutine removeDBoundCondition(obj)
	class(FEMDomain_),intent(inout)::obj

	call obj%Boundary%removeDBC()
end subroutine
!#######################################

!#######################################
subroutine removeNBoundCondition(obj)
	class(FEMDomain_),intent(inout)::obj
	call obj%Boundary%removeNBC()
end subroutine
!#######################################


!#######################################
subroutine removeTBoundCondition(obj)
	class(FEMDomain_),intent(inout)::obj
	call obj%Boundary%removeTBC()
end subroutine
!#######################################

!#######################################
subroutine CheckConnedctivityFEMDomain(obj,fix)
	class(FEMDomain_),intent(inout)::obj
	integer(int32),allocatable:: checklist(:,:),new_node_id(:)
	logical,optional,intent(in)::fix
	integer(int32) :: i,n,m,j

	n=size(obj%Mesh%NodCoord,1)
	allocate(checklist(n,1),new_node_id(n) )
	checklist(:,1)=0

	do i=1,n
		new_node_id(i)=i
	enddo

	do i=1,size(obj%Mesh%ElemNod,1)
		do j=1,size(obj%Mesh%ElemNod,2)
			checklist( obj%Mesh%ElemNod(i,j),1 )=1
		enddo
	enddo

	do i=1,n
		if(checklist(i,1) ==0)then
			! update node id
			do j=i+1,n
				new_node_id(j)=new_node_id(j)-1	
			enddo
			new_node_id(i)=0
		else
			cycle
		endif
	enddo
	

	if(minval(checklist)==0 )then
		print *, "[ERROR] Non-connected nodes exist"
	else
		print *, "[OK] All nodes are connected."
	endif

	if(present(fix) )then
		if( fix .eqv. .true. )then
			! update connectivity
			do i=1,size(obj%Mesh%ElemNod,1)
				do j=1,size(obj%Mesh%ElemNod,2)
					if(new_node_id(obj%Mesh%ElemNod(i,j))==0)then
						print *, "ERROR :: CheckConnedctivityFEMDomain"
					endif
					obj%Mesh%ElemNod(i,j)=new_node_id(obj%Mesh%ElemNod(i,j))
				enddo
			enddo
			
			! remove astray node
			i=1
			do 
				if(checklist(i,1)==0 )then
					call removeArray(obj%Mesh%NodCoord,remove1stColumn=.true.,NextOf=i-1)
					call removeArray(checklist        ,remove1stColumn=.true.,NextOf=i-1)
				else
					i=i+1
					cycle
				endif
				if(minval(checklist)==1 )then
					exit
				else
					cycle
				endif
			enddo

			

		endif	
	endif

end subroutine
!#######################################

subroutine getDBCVectorFEMDomain(obj,DBCvec)
	class(FEMDomain_),intent(in)::obj
	real(real64),allocatable,intent(inout)::DBCvec(:,:)
	integer(int32) :: i,j,n,m,k,l
	n=size(obj%Mesh%NodCoord,1)
	m=size(obj%Mesh%NodCoord,2)
	if(.not. allocated(DBCvec ) )then
		allocate(DBCvec(n,m) )
		DBCvec(:,:)=0.0d0
	endif

	! check number of DBC
	do i=1,size(obj%Boundary%DBoundNum)
		k=countif(Array=obj%Boundary%DBoundNodID(:,i),Value=-1,notEqual=.true.)
		l=obj%Boundary%DBoundNum(i)
		if(k /= l)then
			print *, "Caution :: FiniteDeformationClass::getDBCVector :: check number of DBC :: k /= l"
		endif
	enddo

	do i=1,size(obj%Boundary%DBoundNodID,1)
		do j=1,size(obj%Boundary%DBoundNodID,2)
			if(obj%Boundary%DBoundNodID(i,j) <=0)then
				cycle
			endif
			DBCvec(obj%Boundary%DBoundNodID(i,j),j )=obj%Boundary%DBoundVal(i,j)
		enddo
	enddo


end subroutine
! ##################################################

! ##################################################
subroutine convertMeshTypeFEMDomain(obj,Option)
	class(FEMDomain_),intent(inout) :: obj
	character(*),intent(in) :: Option

	call obj%Mesh%convertMeshType(Option=Option)

end subroutine
! ##################################################

! ##################################################
subroutine createFEMDomain(obj,Name,meshtype,x_num,y_num,z_num,x_len,y_len,z_len,Le,Lh,Dr,thickness,division,&
	top,margin,inclineRate)
	class(FEMDomain_),intent(inout) :: obj
	character(*),intent(in) :: meshtype
	character(*),optional,intent(in) ::Name
    integer(int32),optional,intent(in) :: x_num,y_num,z_num ! number of division
    integer(int32),optional,intent(in) :: division ! for 3D rectangular
    real(real64),optional,intent(in) :: x_len,y_len,z_len,Le,Lh,Dr ! length
    real(real64),optional,intent(in) :: thickness ! for 3D rectangular
    real(real64),optional,intent(in) :: top,margin,inclineRate ! for 3D Ridge and dam

	if(present(Name) )then
		obj%Name=Name
		obj%FileName=Name
	else
		obj%Name="NoName"
		obj%FileName="NoName"
	endif
	if(present(z_num) .or. present(z_len) )then
		call obj%Mesh%create(meshtype,x_num,y_num,x_len,y_len,Le,&
			Lh,Dr,z_len,z_num,top=top,margin=margin)
	else
		call obj%Mesh%create(meshtype,x_num,y_num,x_len,y_len,Le,&
			Lh,Dr,thickness,division,top=top,margin=margin)
	endif
end subroutine createFEMDomain
! ##################################################

! ##################################################
subroutine setBoundaryFEMDomain(obj,new,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,value,values)
	class(FEMDomain_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
	real(real64),optional,intent(in) :: value,values(4)
	logical,optional,intent(in) :: new

	!call obj%Boundary%setDB(new,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,value,values)

end subroutine setBoundaryFEMDomain
! ##################################################

! ##################################################
subroutine showRangeFEMDomain(obj)
	class(FEMDomain_)::obj

	call obj%Mesh%showRange()
end subroutine
! ##################################################

! ##################################################
subroutine ImportBoundariesFEMDomain(obj,Boundary,NumberOfBoundaries,BoundaryID)
	class(FEMDomain_),intent(inout) :: obj
	type(Boundary_),target,intent(in) :: Boundary
	integer(int32),optional,intent(in) :: NumberOfBoundaries,BoundaryID
	integer(int32) :: n,i


	if(.not.allocated(obj%Boundaries) )then
		n=input(default=30,option=NumberOfBoundaries)
		allocate(obj%Boundaries(n))
		do i=1,n
			nullify(obj%Boundaries(i)%boundaryp)
		enddo
		obj%NumberOfBoundaries = 0
	endif

	if(present(BoundaryID) )then
		if(BoundaryID > size(obj%Boundaries) )then
			print *, "ERROR :: ImportBoundariesFEMDomain >> requested BoundaryID is grater than the size of stack"
			print *, "Stack size is ",size(obj%Boundaries), " , and your request is ",BoundaryID
			return
		endif
		if(BoundaryID > obj%NumberOfBoundaries)then
			print *, "ERROR :: ImportBoundariesFEMDomain >> requested BoundaryID is grater than the Last ID"
			print *, "The last ID is ",obj%NumberOfBoundaries+1, " , and your request is ",BoundaryID
			print *, "Hence, your request ",BoundaryID, " is accepted as the ID of ",obj%NumberOfBoundaries+1
			obj%NumberOfBoundaries=obj%NumberOfBoundaries+1
			obj%Boundaries(obj%NumberOfBoundaries)%Boundaryp => Boundary
			print *, "Now, number of boundary conditions is ",obj%NumberOfBoundaries
			return
		endif
		if( associated(obj%Boundaries(BoundaryID)%Boundaryp) )then
			print *, "Boundary ID :: ", BoundaryID, " is overwritten."
			nullify(obj%Boundaries(BoundaryID)%Boundaryp )
		endif
		obj%Boundaries(BoundaryID)%Boundaryp => Boundary
		return
	endif

	obj%NumberOfBoundaries=obj%NumberOfBoundaries+1

	obj%Boundaries(obj%NumberOfBoundaries)%Boundaryp => Boundary

	print *, "Now, number of boundary conditions is ",obj%NumberOfBoundaries

end subroutine ImportBoundariesFEMDomain
! ##################################################


! ##################################################
subroutine showBoundariesFEMDomain(obj,Name)
	class(FEMDomain_),intent(inout) :: obj
	character(*),optional,intent(in)::Name
	integer(int32) :: i

	if(present(Name) )then
		print *, "Domain Name is :: ", trim(adjustl(name))
	endif

	if(.not. allocated(obj%Boundaries) )then
		print *, "No boundary is set."
	else
		do i=1,obj%NumberOfBoundaries
			print *, "Layer :: ",obj%Boundaries(i)%Boundaryp%Layer,"B.C. ::",i," => ",&
				associated(obj%Boundaries(i)%Boundaryp)
		enddo
	endif
end subroutine showBoundariesFEMDomain
! ##################################################


! ##################################################
subroutine removeBoundariesFEMDomain(obj,Name,BoundaryID)
	class(FEMDomain_),intent(inout) :: obj
	character(*),optional,intent(in)::Name
	integer(int32) :: i
	integer(int32),optional,intent(in) ::BoundaryID

	if(present(Name) )then
		print *, "Domain Name is :: ", trim(adjustl(name))
	endif

	if(.not. allocated(obj%Boundaries) )then
		print *, "No boundary is set."
	else
		if(present(BoundaryID))then
			nullify(obj%Boundaries(BoundaryID)%Boundaryp)
		else
			do i=1,obj%NumberOfBoundaries
				nullify(obj%Boundaries(i)%Boundaryp)
			enddo
		endif
	endif
	call obj%showBoundaries(Name)

end subroutine removeBoundariesFEMDomain
! ##################################################

! ##################################################
subroutine copyFEMDomain(obj,OriginalObj,onlyMesh)
	class(FEMDomain_),intent(inout) :: obj
	class(FEMDomain_),intent(in) :: OriginalObj
	logical,optional,intent(in) :: onlyMesh


	call obj%Mesh%copy(OriginalObj%Mesh)
	obj%FileName=Originalobj%FileName
	obj%Name=Originalobj%Name
	if(present(onlyMesh) )then
		if(onlyMesh .eqv. .true.)then
			print *, "Only mesh is copied."
			return
		endif
	endif

end subroutine copyFEMDomain
! ##################################################

! ##################################################
subroutine bakeFEMDomain(obj, template, templateFile,Tol,SimMode,ItrTol,Timestep)
	class(FEMDomain_),intent(inout) :: obj
	character(*),intent(in) :: template
	character(*),optional,intent(in) :: templateFile 
	integer(int32) :: SpaceDim, ElemNodNum, NumOfMatPara, NumOfMaterial, NodeDOF,NodeTDOF
	integer(int32),optional,intent(in)  :: SimMode,ItrTol,Timestep
	real(real64),optional,intent(in) :: Tol
	! bake creates a complete input file for a FEM analysis.
	! You can use build-in templates or your original template.
	! We prepare following build-in templates.
	! - FiniteDeform_ :: For 3-D Finite Deformation Analysis
	! - DiffusionEq_  :: For 3-D Diffusion Analysis
	! If you want to use your original format, please import 
	! your template file.
	! (This is being implemented.)
	
	if(template=="Original")then
		print *, "Please add an argument as 'templateFile = [Your_Template_File]'"
		! text-based finding is not good. Upper/lower cases >> global module to relate ID and 
		! INTEGER, PARAMETER :: TEMP_FINITE_DEFORM = 1000; call tissue%bake(template=TEMP_FINITE_DEFORM)
		! SELECT CASE( template ); CASE( TEMP_FINITE_DEFORM)
		return
	elseif(template=="FiniteDeform_" .or. template=="FiniteDeform")then
		print *, "Build-in template :: FiniteDeform_ is utilized..."
		! Run bakeing process ...
		NumOfMatPara = 6
		NodeDOF =  3
		NodeTDOF = 1
	elseif(template=="DiffusionEq_" .or. template=="DiffusionEq")then
		print *, "Build-in template :: DiffusionEq_ is utilized..."
		! Run bakeing process ...
		NumOfMatPara = 1
		NodeDOF = 1
		NodeTDOF= 1 
	else
		print *, "In case that you want to use your template, please type template='original'."
		return
	endif

	! domain information
	obj%Dtype="domain"
	obj%SolverType=trim(template)
	obj%NumOfDomain=1
	
	if(allocated(obj%Mesh%SubMeshNodFromTo))then
		deallocate(obj%Mesh%SubMeshNodFromTo)
	endif
	if(allocated(obj%Mesh%SubMeshElemFromTo))then
		deallocate(obj%Mesh%SubMeshElemFromTo)
	endif
	allocate(obj%Mesh%SubMeshNodFromTo(obj%NumOfDomain,3) )
	allocate(obj%Mesh%SubMeshElemFromTo(obj%NumOfDomain,3) )
	if(obj%Mesh%empty() .eqv. .true. )then
		print *, "bakeFEMDomain :: Mesh is Empty!"
		return
	endif
	obj%Mesh%ElemType=obj%Mesh%GetElemType()
	! mesh information
	obj%Mesh%SubMeshNodFromTo(1,1) = 1
	obj%Mesh%SubMeshNodFromTo(1,2) = 1
	obj%Mesh%SubMeshNodFromTo(1,3) = size(obj%Mesh%NodCoord,1)
	obj%Mesh%SubMeshElemFromTo(1,1) = 1
	obj%Mesh%SubMeshElemFromTo(1,2) = 1
	obj%Mesh%SubMeshElemFromTo(1,3) = size(obj%Mesh%ElemNod,1)
	if(.not.allocated(obj%Mesh%ElemMat) )then
		allocate(obj%Mesh%ElemMat(size(obj%Mesh%ElemNod,1) ) )
		obj%Mesh%ElemMat(:)=1
	endif
	call showarraysize(obj%Mesh%SubMeshNodFromTo)
	call showarraysize(obj%Mesh%SubMeshElemFromTo)


	call obj%bakeMaterials(NumOfMatPara=NumOfMatPara)
	call obj%bakeDBoundaries(NodeDOF=NodeDOF)
	call obj%bakeNBoundaries(NodeDOF=NodeDOF)
	call obj%bakeTBoundaries(NodeDOF=NodeTDOF)

	call obj%ControlPara%set(OptionalTol=Tol,&
	OptionalItrTol=ItrTol,&
	OptionalTimestep=Timestep,&
	OptionalSimMode=SimMode)

end subroutine bakeFEMDomain
! ##################################################

! ##################################################
subroutine bakeMaterialsFEMDomain(obj,NumOfMatPara)
	class(FEMDomain_),intent(inout) :: obj
	integer(int32),optional,intent(in) :: NumOfMatPara
	integer(int32) :: i,j,k,l,n,m,NumOfMaterial,layer,in_num,NumOfLayer
	real(real64),allocatable :: matPara(:,:),info(:,:)
	integer(int32),allocatable :: key(:)
	type(Rectangle_) :: rect,mrect
	logical :: in_case
	real(real64) :: matparaval,coord(3),x_max(3),x_min(3)

	! get Num of Layer
	NumOfLayer=0
	if(.not. allocated(obj%Materials) )then
		print *, "no materials found"
		return
	endif


	do i=1,size(obj%Materials)
		if(associated(obj%Materials(i)%materialp ) )then
			NumOfLayer=NumOfLayer+1
		else
			cycle
		endif
	enddo



	if(.not. allocated(obj%Materials) )then
		print *, "No material is baked. All material IDs are 1 "
		if(.not.allocated(obj%Mesh%ElemMat) )then
			allocate(obj%Mesh%ElemMat(size(obj%Mesh%ElemNod,1) ) )
			obj%Mesh%ElemMat(:)=1
		endif
		stop "No material parameters are found."
		return
	else
		! total $NumOfLayer material parameters exist.
		! for all materials, resistrate material parameter and material IDs
		m=input(default=NumOfLayer,option=NumOfMatPara)
		allocate(rect%NodCoord(size(obj%Mesh%ElemNod,2),size(obj%Mesh%NodCoord,2)) )
		allocate(mrect%NodCoord(size(obj%Mesh%ElemNod,2),size(obj%Mesh%NodCoord,2)) )
		allocate(matPara(size(obj%Mesh%ElemNod,1),m) )
		matPara(:,:) = 0.0d0
		do i=1,size(obj%Mesh%ElemNod,1)
			! for each element
			
			! input rectangler
			do j=1,size(obj%Mesh%ElemNod,2)
				rect%NodCoord(j,:)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,j),:)
			enddo

			! for all materials, check material parameters
			do j=1,size(obj%Materials)
				if(associated(obj%Materials(j)%materialp) )then
					do k=1, size(obj%Materials(j)%materialp%Mesh%ElemNod,1)
						! for each zones, check in-out
						! import nodal coordinate
						do l=1,size(obj%Materials(j)%materialp%Mesh%ElemNod,2)
							n=obj%Materials(j)%materialp%Mesh%ElemNod(k,l)
							mrect%NodCoord(l,:)=obj%Materials(j)%materialp%Mesh%NodCoord(n,:)
						enddo
						layer=obj%Materials(j)%materialp%layer
						! check in-out
						if(rect%contact(mrect) .eqv. .true. )then
							! in
							matPara(i,layer)=obj%Materials(j)%materialp%meshPara(k,1)
						else
							cycle
						endif
					enddo
				else
					cycle
				endif
			enddo
		enddo

	endif

	call getKeyAndValue(Array=matPara,key=obj%Mesh%ElemMat, info=obj%MaterialProp%MatPara)
	!call showarray(obj%Mesh%ElemMat,Name="test1.txt")
	!call showarray(obj%MaterialProp%MatPara,Name="test2.txt")
end subroutine bakeMaterialsFEMDomain
! ##################################################

! ##################################################
subroutine bakeDBoundariesFEMDomain(obj,NodeDOF)
	class(FEMDomain_),intent(inout) :: obj
	integer(int32) ,optional,intent(in) :: NodeDOF 
	integer(int32) :: i,j,k,l,n,m,NumOfMaterial,layer,in_num,NumOfLayer,DBCnum,&
	val_id,NumOfValPerNod
	real(real64),allocatable :: matPara(:,:),info(:,:)
	integer(int32),allocatable :: key(:)
	type(Rectangle_) :: rect,mrect
	logical :: in_case
	real(real64) :: matparaval,coord(3),x_max(3),x_min(3),&
	xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax,valx,valy,valz,val


	! get Num of Layer
	NumOfLayer=0
	if(.not. allocated(obj%Boundaries) )then
		print *, "no Boundaries found"
		return
	endif

	DBCnum=NodeDOF

	if(.not. allocated(obj%Boundaries) )then
		print *, "No Dirichlet Boundaries are imported."
		return
	endif

	NumOfLayer=0
	do i=1, size(obj%Boundaries,1)
		if(associated(obj%Boundaries(i)%Boundaryp ) )then
			if(obj%Boundaries(i)%Boundaryp%Dbound%empty() .eqv. .false. )then
				NumOfLayer=NumOfLayer+1
			endif
		else
			cycle
		endif
	enddo
	print *, "Number of Layer for Dirichlet Boundary= ",NumOfLayer

	call obj%initDBC(NumOfValPerNod=input(default=NumOfLayer,option=NodeDOF) )


	if(.not. allocated(obj%Boundaries) )then
		print *, "No Dirichlet boundary is baked."
		return
	else
		! total $NumOfLayer Boundary Conditions exist.
		! for all Boundaries, resistrate material parameter and material IDs
		do i=1,size(obj%Boundaries,1)
			! for each Layer
			if(associated(obj%Boundaries(i)%Boundaryp ) )then
				if(obj%Boundaries(i)%Boundaryp%DBound%empty() .eqv. .false. )then
					do j=1,size(obj%Boundaries(i)%Boundaryp%DBound%ElemNod,1)
						! for each Zone
						xmin = minval( obj%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,1) ) 
						xmax = maxval( obj%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,1) ) 
						ymin = minval( obj%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,2) )
						ymax = maxval( obj%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,2) )
						zmin = minval( obj%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,3) )
						zmax = maxval( obj%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,3) )
						val = obj%Boundaries(i)%Boundaryp%DBoundPara(j,1)
						call obj%AddDBoundCondition(xmin=xmin,xmax=xmax,ymin=ymin,&
						ymax=ymax,zmin=zmin,zmax=zmax,val=val,&
						val_id=obj%Boundaries(i)%Boundaryp%layer)
					enddo
				endif
			endif
		enddo
	endif

end subroutine bakeDBoundariesFEMDomain
! ##################################################


! ##################################################
subroutine bakeNBoundariesFEMDomain(obj,NodeDOF)
	class(FEMDomain_),intent(inout) :: obj
	integer(int32) ,optional,intent(in) :: NodeDOF 
	integer(int32) :: i,j,k,l,n,m,NumOfMaterial,layer,in_num,NumOfLayer,DBCnum,&
	val_id,NumOfValPerNod,numofnode
	real(real64),allocatable :: matPara(:,:),info(:,:)
	integer(int32),allocatable :: key(:)
	type(Rectangle_) :: rect,mrect
	logical :: in_case
	real(real64) :: matparaval,coord(3),x_max(3),x_min(3),&
	xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax,valx,valy,valz,val,area


	! get Num of Layer
	NumOfLayer=0
	if(.not. allocated(obj%Boundaries) )then
		print *, "no Boundaries found"
		return
	endif

	DBCnum=NodeDOF

	if(.not. allocated(obj%Boundaries) )then
		print *, "No Neumann Boundaries are imported."
		return
	endif

	NumOfLayer=0
	do i=1, size(obj%Boundaries,1)
		if(associated(obj%Boundaries(i)%Boundaryp ) )then
			if(obj%Boundaries(i)%Boundaryp%Nbound%empty() .eqv. .false. )then
				NumOfLayer=NumOfLayer+1
			endif
		else
			cycle
		endif
	enddo
	print *, "Number of Layer for Neumann Boundary= ",NumOfLayer

	call obj%initNBC(NumOfValPerNod=input(default=NumOfLayer,option=NodeDOF) )
	

	if(.not. allocated(obj%Boundaries) )then
		print *, "No Neumann boundary is baked."
		return
	else
		! total $NumOfLayer Boundary Conditions exist.
		! for all Boundaries, resistrate material parameter and material IDs
		do i=1,size(obj%Boundaries,1)
			! for each Layer
			if(associated(obj%Boundaries(i)%Boundaryp ) )then
				if(obj%Boundaries(i)%Boundaryp%NBound%empty() .eqv. .false. )then
					do j=1,size(obj%Boundaries(i)%Boundaryp%NBound%ElemNod,1)
						! for each Zone
						xmin = minval( obj%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,1) ) 
						xmax = maxval( obj%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,1) ) 
						ymin = minval( obj%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,2) )
						ymax = maxval( obj%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,2) )
						zmin = minval( obj%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,3) )
						zmax = maxval( obj%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,3) )
						val = obj%Boundaries(i)%Boundaryp%NBoundPara(j,1)
						call obj%AddNBoundCondition(xmin=xmin,xmax=xmax,ymin=ymin,&
						ymax=ymax,zmin=zmin,zmax=zmax,val=val,&
						val_id=obj%Boundaries(i)%Boundaryp%layer)
					enddo
				endif
			endif
		enddo
	endif

end subroutine bakeNBoundariesFEMDomain
! ##################################################


! ##################################################
subroutine bakeTBoundariesFEMDomain(obj,NodeDOF)
	class(FEMDomain_),intent(inout) :: obj
	integer(int32) ,optional,intent(in) :: NodeDOF 
	integer(int32) :: i,j,k,l,n,m,NumOfMaterial,layer,in_num,NumOfLayer,DBCnum,&
	val_id,NumOfValPerNod,numofnode
	real(real64),allocatable :: matPara(:,:),info(:,:)
	integer(int32),allocatable :: key(:)
	type(Rectangle_) :: rect,mrect
	logical :: in_case
	real(real64) :: matparaval,coord(3),x_max(3),x_min(3),&
	xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax,valx,valy,valz,val,area


	! get Num of Layer
	NumOfLayer=0
	if(.not. allocated(obj%Boundaries) )then
		print *, "no Boundaries found"
		return
	endif

	DBCnum=NodeDOF

	if(.not. allocated(obj%Boundaries) )then
		print *, "No Time Boundaries are imported."
		return
	endif

	NumOfLayer=0
	do i=1, size(obj%Boundaries,1)
		if(associated(obj%Boundaries(i)%Boundaryp ) )then
			if(obj%Boundaries(i)%Boundaryp%Tbound%empty() .eqv. .false. )then
				NumOfLayer=NumOfLayer+1
			endif
		else
			cycle
		endif
	enddo
	print *, "Number of Layer for Time Boundary= ",NumOfLayer

	call obj%initTBC(NumOfValPerNod=input(default=NumOfLayer,option=NodeDOF) )


	if(.not. allocated(obj%Boundaries) )then
		print *, "No Time boundary is baked."
		return
	else
		! total $NumOfLayer Boundary Conditions exist.
		! for all Boundaries, resistrate material parameter and material IDs
		do i=1,size(obj%Boundaries,1)
			! for each Layer
			if(associated(obj%Boundaries(i)%Boundaryp ) )then
				if(obj%Boundaries(i)%Boundaryp%TBound%empty() .eqv. .false. )then
					do j=1,size(obj%Boundaries(i)%Boundaryp%TBound%ElemNod,1)
						! for each Zone
						xmin = minval( obj%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,1) ) 
						xmax = maxval( obj%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,1) ) 
						ymin = minval( obj%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,2) )
						ymax = maxval( obj%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,2) )
						zmin = minval( obj%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,3) )
						zmax = maxval( obj%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(obj%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,3) )
						val = obj%Boundaries(i)%Boundaryp%TBoundPara(j,1)
						call obj%AddTBoundCondition(xmin=xmin,xmax=xmax,ymin=ymin,&
						ymax=ymax,zmin=zmin,zmax=zmax,val=val,&
						val_id=obj%Boundaries(i)%Boundaryp%layer)
					enddo
				endif
			endif
		enddo
	endif

end subroutine bakeTBoundariesFEMDomain
! ##################################################





! ##################################################
subroutine ImportMaterialsFEMDomain(obj,Material,NumberOfMaterials,MaterialID)
	class(FEMDomain_),intent(inout) :: obj
	type(MaterialProp_),target,intent(in) :: Material
	integer(int32), optional,intent(in) :: NumberOfMaterials,MaterialID
	integer(int32) :: n,i


	if(.not.allocated(obj%Materials) )then
		n=input(default=30,option=NumberOfMaterials)
		allocate(obj%Materials(n))
		obj%NumberOfMaterials = 0
		do i=1,n
			nullify(obj%Materials(i)%materialp)
		enddo
	endif

	if(present(MaterialID) )then
		if(MaterialID > size(obj%Materials) )then
			print *, "ERROR :: ImportMaterialsFEMDomain >> requested MaterialID is grater than the size of stack"
			print *, "Stack size is ",size(obj%Materials), " , and your request is ",MaterialID
			return
		endif
		if(MaterialID > obj%NumberOfMaterials)then
			print *, "ERROR :: ImportMaterialsFEMDomain >> requested MaterialID is grater than the Last ID"
			print *, "The last ID is ",obj%NumberOfMaterials+1, " , and your request is ",MaterialID
			print *, "Hence, your request ",MaterialID, " is accepted as the ID of ",obj%NumberOfMaterials+1
			obj%NumberOfMaterials=obj%NumberOfMaterials+1
			obj%Materials(obj%NumberOfMaterials)%Materialp => Material
			print *, "Now, number of Material conditions is ",obj%NumberOfMaterials
			return
		endif
		if( associated(obj%Materials(MaterialID)%Materialp) )then
			print *, "Material ID :: ", MaterialID, " is overwritten."
			nullify(obj%Materials(MaterialID)%Materialp )
		endif
		obj%Materials(MaterialID)%Materialp => Material
		return
	endif

	obj%NumberOfMaterials=obj%NumberOfMaterials+1

	obj%Materials(obj%NumberOfMaterials)%Materialp => Material

	print *, "Now, number of Material conditions is ",obj%NumberOfMaterials

end subroutine ImportMaterialsFEMDomain
! ##################################################




! ##################################################
subroutine showMaterialsFEMDomain(obj,Name)
	class(FEMDomain_),intent(inout) :: obj
	character(*),optional,intent(in)::Name
	integer(int32) :: i

	if(present(Name) )then
		print *, "Domain Name is :: ", trim(adjustl(name))
	endif

	if(.not. allocated(obj%Materials) )then
		print *, "No boundary is set."
	else
		do i=1,obj%NumberOfMaterials
			print *, "Layer :: ",obj%Materials(i)%Materialp%Layer,"Material ::",i," => ",&
				associated(obj%Materials(i)%Materialp)
		enddo
	endif
end subroutine showMaterialsFEMDomain
! ##################################################


! ##################################################
subroutine removeMaterialsFEMDomain(obj,Name,BoundaryID)
	class(FEMDomain_),intent(inout) :: obj
	character(*),optional,intent(in)::Name
	integer(int32) :: i
	integer(int32),optional,intent(in) ::BoundaryID

	if(present(Name) )then
		print *, "Domain Name is :: ", trim(adjustl(name))
	endif

	if(.not. allocated(obj%Materials) )then
		print *, "No boundary is set."
	else
		if(present(BoundaryID))then
			nullify(obj%Materials(BoundaryID)%Materialp)
		else
			do i=1,obj%NumberOfMaterials
				nullify(obj%Materials(i)%Materialp)
			enddo
		endif
	endif
	call obj%showMaterials(Name)

end subroutine removeMaterialsFEMDomain
! ##################################################


! ##################################################
subroutine contactdetectFEMDomain(obj1, obj2, ContactModel)
	class(FEMDomain_),intent(inout) :: obj1, obj2
	character(*),optional,intent(in) :: ContactModel
	type(Mesh_) :: BoundBox
	type(Random_) :: random
	type(ContactName_),allocatable :: cnbuf(:)
	integer(int32),allocatable :: buffer(:)
	real(real64),allocatable :: x(:)

	integer(int32) :: i,domain_id,n,id,m,node_id,seg_nod_num

	! detect contact nodes and assemble contact elements
	
	! first, both domains should be named.
	! If these are not named, name by random name.
	m=size(obj1%Mesh%NodCoord,2)
	allocate(x(m) )
	
	if( trim(obj1%name) == "NoName" )then
		obj1%name=trim( random%name() )
		print *, "Caution !!! object #1 is not named. New name is "//trim(obj1%name)
	endif
	if( trim(obj2%name) == "NoName" )then
		obj2%name=trim( random%name() )
		print *, "Caution !!! object #2 is not named. New name is "//trim(obj2%name)
	endif

	! create Node-To-Node contact elements
	! First, let us detect a bounding box, in which contact interfaces are presented.
	call obj1%Mesh%GetInterSectBox(obj2%Mesh,BoundBox)
	! , where, obj1, obj2 are FEMDomain objects, and BoundBox is the bounding box.

	! if, the BoundingBox is not allocated, return
	if( BoundBox%empty() )then
		return
	endif

	! Hereby, two domains are in contact.
	! let us detect the contact nodes.
	if(.not. allocated(obj1%Boundary%ContactNameList) )then
		allocate(obj1%Boundary%ContactNameList(1) )
		obj1%Boundary%ContactNameList(1)%name=obj2%name
		domain_id=1
	else
		cnbuf = obj1%Boundary%ContactNameList
		n=size(obj1%Boundary%ContactNameList)
		deallocate(obj1%Boundary%ContactNameList)
		allocate(obj1%Boundary%ContactNameList(n+1) )
		obj1%Boundary%ContactNameList(1:n)%name=cnbuf(1:n)%name
		obj1%Boundary%ContactNameList(n+1)%name=trim(obj2%name)
		domain_id=n+1
	endif
	

	buffer = obj1%Mesh%getNodeList(BoundingBox=BoundBox)
	
	call obj1%Mesh%getSurface()
	call obj2%Mesh%getSurface()
	if(m==2)then
		seg_nod_num=4
	else
		seg_nod_num=16
	endif

	do i=1,size(buffer)
		if(.not. allocated(obj1%Boundary%MasterNodeID) )then
			allocate(obj1%Boundary%MasterNodeID(1,2) )
			allocate(obj1%Boundary%SlaveNodeID(1,2) )
			allocate(obj1%Boundary%MasterSegment(seg_nod_num,2) )
			allocate(obj1%Boundary%SlaveSegment( seg_nod_num,2) )
		else
			call extend(obj1%Boundary%MasterNodeID,extend1stColumn=.true.)
			call extend(obj1%Boundary%SlaveNodeID,extend1stColumn=.true.)
		endif

		n=size(obj1%Boundary%MasterNodeID,1)
		
		obj1%Boundary%MasterNodeID(n,1) = buffer(i)
		obj1%Boundary%MasterNodeID(n,2) = domain_id
		obj1%Boundary%SlaveNodeID(n,1) = 0
		obj1%Boundary%SlaveNodeID(n,2) = domain_id

		 
		obj1%Boundary%MasterSegment(n,:)=domain_id 
		obj1%Boundary%SlaveSegment( n,:)=domain_id 
		

	enddo

	! assemble Node-To-Node contact element
	do i=1,size(obj1%Boundary%MasterNodeID,1)
		node_id=obj1%Boundary%MasterNodeID(i,1)
		x(:)=obj1%Mesh%NodCoord( node_id,: )
		id = SearchNearestCoord(Array=obj2%Mesh%NodCoord,x=x)
		obj1%Boundary%SlaveNodeID(i,1)=id
	enddo

	! assemble Node-To-Segment contact element
	



end subroutine
! ##################################################

end module FEMDomainClass