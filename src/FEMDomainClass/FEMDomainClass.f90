module FEMDomainClass
	use, intrinsic :: iso_fortran_env
	use MathClass
    use ArrayClass
    use ShapeFunctionClass
    use MeshClass
    use MaterialPropClass
    use BoundaryConditionClass
	use ControlParameterClass
	use std
	
	implicit none

	! VTK-FORMAT
	integer(int32),parameter,public :: VTK_VERTEX		 = 1 !	Vertex
	integer(int32),parameter,public :: VTK_POLY_VERTEX = 2 !	Vertex
	integer(int32),parameter,public :: VTK_LINE		 = 3 !	Edge Lagrange P1
	integer(int32),parameter,public :: VTK_TRIANGLE	 = 5 !	Triangle Lagrange P1
	integer(int32),parameter,public :: VTK_PIXEL		 = 8 !	Quadrilateral Lagrange P1
	integer(int32),parameter,public :: VTK_QUAD		 = 9 !	Quadrilateral Lagrange P1
	integer(int32),parameter,public :: VTK_TETRA		 = 10 !	Tetrahedron Lagrange P1
	integer(int32),parameter,public :: VTK_VOXEL		 = 11 !	Hexahedron Lagrange P1
	integer(int32),parameter,public :: VTK_HEXAHEDRON  = 12 !	Hexahedron Lagrange P1
	integer(int32),parameter,public :: VTK_WEDGE		 = 13 !	Wedge Lagrange P1
	integer(int32),parameter,public :: VTK_QUADRATIC_EDGE 	 = 21 !	Edge Lagrange P2
	integer(int32),parameter,public :: VTK_QUADRATIC_TRIANGLE  = 22 !	Triangle Lagrange P2
	integer(int32),parameter,public :: VTK_QUADRATIC_QUAD		 = 23 !	Quadrilateral Lagrange P2
	integer(int32),parameter,public :: VTK_QUADRATIC_TETRA	 = 24 !	Tetrahedron Lagrange P2
	integer(int32),parameter,public :: VTK_QUADRATIC_HEXAHEDRON = 25 !	Hexahedron Lagrange P2	
	integer(int32),parameter,public :: VTK_QUADRATIC_LINEAR_WEDGE = 31

	integer(int32),parameter,public :: MSH_LINE		 = 1 !	Edge Lagrange P1
	integer(int32),parameter,public :: MSH_TRIANGLE	 = 2 !	Triangle Lagrange P1
	integer(int32),parameter,public :: MSH_QUAD		 = 3 !	Quadrilateral Lagrange P1
	integer(int32),parameter,public :: MSH_TETRA		 = 4 !	Tetrahedron Lagrange P1
	integer(int32),parameter,public :: MSH_HEXAHEDRON  = 5 !	Hexahedron Lagrange P1
	integer(int32),parameter,public :: MSH_PRISM 	 = 6 !	Edge Lagrange P2
	integer(int32),parameter,public :: MSH_PYRAMID  = 7 !	Triangle Lagrange P2

	

	integer(int32),parameter,public :: FEMDomain_Overset_GPP = 1
	integer(int32),parameter,public :: FEMDomain_Overset_P2P = 2

	
	
	!integer(int32),parameter,public :: INFO_NUMBER_OF_POINTS  = 1 !	Information id#1 number of node
	!integer(int32),parameter,public :: INFO_NUMBER_OF_ELEMENTS  = 1 !	Information id#1 number of node
	!integer(int32),parameter,public :: INFO_NUMBER_OF_ELEMENTS  = 1 !	Information id#1 number of node
	

	type::Meshp_
		type(Mesh_),pointer :: Meshp => null()
	end type


	type::Materialp_
		type(MaterialProp_),pointer :: Materialp => null()
	end type


	type::Boundaryp_
		type(Boundary_),pointer :: Boundaryp => null()
	end type

	type :: OversetConnect_
		logical :: active = .false.
		real(real64),allocatable :: position(:)
		integer(int32) :: ElementID, GaussPointID, projection,MyElementID
		integer(int32),allocatable :: InterConnect(:)
		integer(int32),allocatable :: DomainIDs12(:)
	end type

    type::FEMDomain_
        type(Mesh_)             :: Mesh
        type(MaterialProp_)     :: MaterialProp
        type(Boundary_)         :: Boundary
		type(ControlParameter_) :: ControlPara
		type(ShapeFunction_)    :: ShapeFunction
		
		type(PhysicalField_),allocatable :: PhysicalField(:)
		integer(int32) :: numoflayer=0
		character(len=36) :: uuid

		character(len=36) :: link(2)
		character(len=70) :: meshtype

		real(real64),allocatable :: scalar(:)
		real(real64),allocatable :: vector(:,:)
		real(real64),allocatable :: tensor(:,:,:)

		real(real64) :: RealTime=1.0d0
		integer(int32) :: NumOfDomain=1
        character(:),allocatable :: FilePath!="None"
        character(:),allocatable :: FileName!="None"
        character(:),allocatable :: Name!="None"
		character(:),allocatable :: SolverType!="None"
		character(:),allocatable :: Category1! ="None"
		character(:),allocatable :: Category2!="None"
		character(:),allocatable :: Category3!="None"
        character*9 :: Dtype="None"
		integer(int32) :: DomainID=1
		integer(int32) :: timestep=1
		integer(int32) :: NumberOfBoundaries=0
		integer(int32) ::  NumberOfMaterials=0

		! for MPI
		integer(int32),allocatable :: mpi_global_node_idx(:)
		integer(int32),allocatable :: mpi_shared_node_info(:,:)

		! for overset, optional.
		type(OversetConnect_),allocatable :: OversetConnect(:)
		integer(int32),allocatable :: OversetExists(:,:)
		integer(int32) :: num_oversetconnect = 0
		
		! それか，pairingだけを決める．
		
		! juncs

		type(Meshp_),allocatable :: Meshes(:)
		type(Materialp_),allocatable :: Materials(:)
		type(Boundaryp_),allocatable :: Boundaries(:)

		real(real64),allocatable :: ObjectPosition(:)

		real(real64) :: total_rotation(1:3) = 0.0d0
		!type(FEMDomainp_),allocatable :: FEMDomains(:)

		! pyhsical modifiers
		integer(int32),allocatable :: grub_NodeList(:)

		logical :: debug_mode = .false.

    contains
		procedure,public :: add => addFEMDomain
		procedure,public :: addNBC => AddNBCFEMDomain 
		procedure,public :: add_point => add_pointFEMDomain
		procedure,public :: importLayer => importLayerFEMDomain


		procedure,pass :: addLayerFEMDomain
		procedure,pass :: addLayerFEMDomainScalar
		procedure,pass :: addLayerFEMDomainVector
		procedure,pass :: addLayerFEMDomainTensor
		generic,public :: addLayer => addLayerFEMDomainScalar,addLayerFEMDomain,&
			addLayerFEMDomainVector,&
			addLayerFEMDomainTensor

		procedure,public :: mpi_matmul => mpi_matmulFEMDomain
		procedure,public :: showLayer => showLayerFEMDomain
		procedure,public :: searchLayer => searchLayerFEMDomain

        procedure,public :: addDBoundCondition => AddDBoundCondition
        procedure,public :: addNBoundCondition => AddNBoundCondition
        procedure,public :: addTBoundCondition => AddTBoundCondition
        procedure,public :: addMaterialID => AddMaterialID
		procedure,public :: assign => ImportFEMDomain
		procedure,public :: allconnectivity => allconnectivityFEMDomain
		
		procedure,public :: bake => bakeFEMDomain
		procedure,public :: bakeMaterials => bakeMaterialsFEMDomain
		procedure,public :: bakeDBoundaries => bakeDBoundariesFEMDomain
		procedure,public :: bakeNBoundaries => bakeNBoundariesFEMDomain
		procedure,public :: bakeTBoundaries => bakeTBoundariesFEMDomain
		procedure,public :: Boolean => BooleanFEMDomain

		
		procedure,public :: checkConnectivity => CheckConnedctivityFEMDomain
		procedure,public :: connectivity => connectivityFEMDomain 
		procedure,public :: copy => copyFEMDomain
		
		! >>>>>>>> un-recommended >>>>>>>>>
		procedure,public :: convertMeshType => convertMeshTypeFEMDomain
		! <<<<<<<< un-recommended <<<<<<<<<

		procedure,public :: changeElementType => changeElementTypeFEMDomain
		procedure,public :: clipVector => clipVectorFEMDomain
		
		procedure,public :: contactdetect => contactdetectFEMDomain
		procedure,public :: centerPosition => centerPositionFEMDomain
		procedure,public :: getCenter => centerPositionFEMDomain
		procedure,public :: create => createFEMDomain

		!>>> direct object generation
		procedure,public :: cube => cubeFEMDomain
		!<<< direct object generation

        procedure,public :: delete => DeallocateFEMDomain
		procedure,public :: display => displayFEMDomain

		procedure,pass :: divide_mpi_FEMDomain
		procedure,pass :: divide_nFEMDomain

		generic ::  divide => divide_mpi_FEMDomain,divide_nFEMDomain
		!procedure,public :: distribute => distributeFEMDomain
		procedure,public :: Delaunay3D => Delaunay3DFEMDomain
		procedure,public :: Delaunay2D => Delaunay2DFEMDomain
		procedure,public :: deform => deformFEMDomain
		procedure,public :: Deduplicate => DeduplicateFEMDomain

		procedure,public :: export => ExportFEMDomain

		procedure,public :: edit => editFEMDomain
		procedure,public :: empty => emptyFEMDomain
		
		
		procedure,public :: field => fieldFEMDomain
		procedure,public :: fixReversedElements => fixReversedElementsFEMDomain
		procedure,public :: fit =>  fitFEMDomain
		procedure,public :: full => fullFEMDomain
		
		procedure,public :: gmshPlotMesh => GmshPlotMesh
		procedure,public :: gmsh => GmshPlotMesh
		procedure,public :: gmshPlotContour => GmshPlotContour
		procedure,public :: gmshPlotVector => GmshPlotVector 
        procedure,public :: gmshPlotContour2D => GmshPlotContour2D
        procedure,public :: gnuplotPlotContour  => GnuplotPlotContour   
		procedure,public :: gnuplotExportStress => GnuplotExportStress  
		procedure,public :: getDBCVector => getDBCVectorFEMDomain
		procedure,public :: getVolume => getVolumeFEMDomain

		procedure,public :: getJacobiMatrix => getJacobiMatrixFEMDomain
		procedure,pass :: getLayer_scalarFEMDomain
		generic :: getLayer => getLayer_scalarFEMDomain

		procedure,public :: getLayerID => getLayerIDFEMDomain
		procedure,public :: getLayerAttribute => getLayerAttributeFEMDomain
		procedure,public :: getLayerDataStyle => getLayerDataStyleFEMDomain
		procedure,public :: getShapeFunction => getShapeFunctionFEMDomain
		procedure,public :: getNearestNodeID => getNearestNodeIDFEMDomain
		procedure,public :: getE2Econnectivity => getE2EconnectivityFEMDomain
		procedure,public :: getElementCauchyStress => getElementCauchyStressFEMDomain
		procedure,public :: getMyID => getMyIDFEMDomain
		procedure,public :: getValue => getValueFEMDomain
		procedure,public :: getStrainTensor => getStrainTensorFEMDomain
		procedure,public :: getNumberOfOversetForElement => getNumberOfOversetForElementFEMDomain
		

		procedure,public :: getSurface => getSurfaceFEMDomain
		procedure,public :: getVertices => getVerticesFEMDomain
		procedure,public ::	NodeID => NodeIDFEMDomain
		procedure,public ::	getElementID => getElementIDFEMDomain
		procedure,public ::	getNodeList =>getNodeListFEMDomain
		
		procedure,public :: has  => hasFEMDomain
		procedure,public :: have => hasFEMDomain
		! filters
		procedure,public :: MovingAverageFilter => MovingAverageFilterFEMDomain
		

		procedure,public :: getElement => getElementFEMDOmain

		procedure,public :: getNeighboringElementList =>getNeighboringElementListFEMDomain
		procedure,public :: to_ElementID => to_ElementIDFEMDomain

		procedure,pass   :: getElementListFEMDomain
		procedure,pass   :: getElementList_by_radiusFEMDomain

		generic,public :: getElementList => getElementList_by_radiusFEMDomain,getElementListFEMDomain

		procedure,public :: getScalarField => getScalarFieldFEMDomain
		procedure,public :: getSingleFacetNodeID => getSingleFacetNodeIDFEMDomain
		
		!procedure,public :: getNumberOfPoint => getNumberOfPointFEMDomain
		
		procedure,public :: getLocalCoordinate => getLocalCoordinateFEMDomain	
		procedure,public :: GlobalPositionOfGaussPoint => getGlobalPositionOfGaussPointFEMDomain	
		
		procedure,public :: getElevation => getElevationFEMDomain

        procedure,public :: init   => InitializeFEMDomain
		procedure,public :: import => ImportFEMDomain
		procedure,public :: importVTKFile => ImportVTKFileFEMDomain
		procedure,public :: importSTLFile => ImportSTLFileFEMDomain
		
		procedure,public :: importMesh => ImportMeshFEMDomain
		procedure,public :: importMaterials => ImportMaterialsFEMDomain
		procedure,public :: importBoundaries => ImportBoundariesFEMDomain
        procedure,public :: initDBC => InitDBC
        procedure,public :: initNBC => InitNBC
		procedure,public :: initTBC => InitTBC
		procedure,public :: inside_of_element => inside_of_elementFEMDomain

		procedure,public :: json => jsonFEMDomain

		procedure,public :: killElement => killElementFEMDomain
		procedure,public :: killNodes => killNodesFEMDomain

		procedure,public :: length => lengthFEMDomain

		procedure,public :: meltingSkelton => MeltingSkeltonFEMDomain
		procedure,public :: move => moveFEMDomain
		procedure,public :: meshing => meshingFEMDomain
		procedure,public :: merge  => MergeFEMDomain


		procedure,public :: msh => mshFEMDomain


		! number of points
		procedure,public :: nn => nnFEMDomain
		procedure,public :: np => nnFEMDomain
		! number of dimensions
		procedure,public :: nd => ndFEMDomain
		! number of elements
		procedure,public :: ne => neFEMDomain
		! number of points per element
		procedure,public ::	nne => nneFEMDomain
		! number of Gauss-points 
		procedure,public ::	ngp => ngpFEMDomain
		! number of overset elements
		procedure, public :: NumOversetElements => NumOversetElementsFEMDomain

		! getter
		procedure,public ::	x => xFEMDomain

		

		procedure,public ::	y => yFEMDomain
		procedure,public ::	z => zFEMDomain
		procedure,public ::	getPoint => getPointFEMDomain
		procedure,public ::	Point => getPointFEMDomain
		procedure,public ::	getPoint_x => getPoint_xFEMDomain
		procedure,public ::	getPoint_y => getPoint_yFEMDomain
		procedure,public ::	getPoint_z => getPoint_zFEMDomain

		procedure,public ::	setPoint_x => set_xFEMDomain
		procedure,public ::	setPoint_y => set_yFEMDomain
		procedure,public ::	setPoint_z => set_zFEMDomain

		procedure,public ::	xyz => xyzFEMDomain

		
		! converter
		procedure,public :: asGlobalVector=>asGlobalVectorFEMDomain
		procedure,public :: ElementID2NodeID => ElementID2NodeIDFEMDomain
		procedure,public :: open => openFEMDomain
		
		procedure, pass  :: oversetFEMDomain
		procedure, pass  :: oversetFEMDomains
		generic :: overset => oversetFEMDomain,oversetFEMDomains
		generic :: overlap => oversetFEMDomain,oversetFEMDomains
		generic :: chimera => oversetFEMDomain,oversetFEMDomains

		procedure,public :: PCAvector => PCAvectorFEMDomain
		procedure,public :: ply => plyFEMDomain
		procedure,public :: projection => projectionFEMDomain
        procedure,public :: position => positionFEMDomain
        procedure,public :: position_x => position_xFEMDomain
        procedure,public :: position_y => position_yFEMDomain
        procedure,public :: position_z => position_zFEMDomain
		procedure,public ::	points => xyzFEMDomain

		procedure,public :: span => spanFEMDomain

		! editor
		procedure,public :: to_HollowTube => to_HollowTube_FEMDomain
		procedure,public :: to_Tube => to_HollowTube_FEMDomain
		procedure,public :: to_culm => to_culm_FEMDomain
		procedure,public :: to_multi_culm => to_multi_culm_FEMDomain
		procedure,public :: to_cylinder => to_cylinder_FEMDomain
		procedure,public :: to_vertexData => to_vertexData_FEMDomain

		procedure,public :: xmin => xminFEMDomain
		procedure,public :: x_min => xminFEMDomain
		procedure,public :: xmax => xmaxFEMDomain
		procedure,public :: x_max => xmaxFEMDomain
		procedure,public :: ymin => yminFEMDomain
		procedure,public :: y_min => yminFEMDomain
		procedure,public :: ymax => ymaxFEMDomain
		procedure,public :: y_max => ymaxFEMDomain
		procedure,public :: zmin => zminFEMDomain
		procedure,public :: z_min => zminFEMDomain
		procedure,public :: zmax => zmaxFEMDomain
		procedure,public :: z_max => zmaxFEMDomain

		procedure,public :: xrange  => xrangeFEMDomain
		procedure,public :: x_range => xrangeFEMDomain
		procedure,public :: xr      => xrangeFEMDomain

		procedure,public :: yrange  => yrangeFEMDomain
		procedure,public :: y_range => yrangeFEMDomain
		procedure,public :: yr      => yrangeFEMDomain

		procedure,public :: zrange  => zrangeFEMDomain
		procedure,public :: z_range => zrangeFEMDomain
		procedure,public :: zr      => zrangeFEMDomain

		
		procedure,public ::	x_len => x_lenFEMDomain
		procedure,public ::	y_len => y_lenFEMDomain
		procedure,public ::	z_len => z_lenFEMDomain
		

		procedure,public :: removeMaterials => removeMaterialsFEMDomain
		procedure,public :: rotate => rotateFEMDomain
		procedure,public :: removeBoundaries => removeBoundariesFEMDomain
		procedure,public :: rename => renameFEMDomain
		procedure,public :: resize => resizeFEMDomain
		procedure,public :: fat => fatFEMDomain
		procedure,public :: removeElement => removeElementFEMDomain
		procedure,public :: removeElements=> removeElementFEMDomain
		procedure,public :: remove => removeFEMDomain
		procedure,public :: remove_duplication => remove_duplication_FEMDomain
		procedure,pass ::  refineFEMDomain
		procedure,pass ::  refine_elementsFEMDomain
		generic,public :: refine => refineFEMDomain,refine_elementsFEMDomain
		
		procedure,pass   :: readFEMDomain
		procedure,pass   :: read_vtk_domain_decomposed_FEMDOmain
		generic :: read => readFEMDomain

		procedure,pass   :: read_SCALAR_FEMDomain
		generic :: read_SCALAR => read_SCALAR_FEMDomain

		
		generic :: read_vtk => read_vtk_domain_decomposed_FEMDOmain

		procedure,public :: read_mpi_property =>read_mpi_propertyFEMDomain
		procedure,public :: remesh => remeshFEMDomain
		procedure,public :: randomDance => randomDanceFEMDomain

		procedure,public :: save => saveFEMDomain

        procedure,public :: setDataType => SetDataType
        procedure,public :: setSolver => SetSolver 
        procedure,public :: setName => SetName 
        procedure,public :: setUp      => SetUpFEMDomain
		procedure,public :: setBoundary => setBoundaryFEMDomain
        procedure,public :: setControlPara =>  SetControlParaFEMDomain
		
		procedure,pass :: selectFEMDomain ! select nodes
		!procedure,pass :: select_by_functionFEMDomain ! select nodes
		generic,public :: select => selectFEMDomain!,select_by_functionFEMDomain
		
		procedure,public :: show => showFEMDomain
		procedure,public :: showRange => showRangeFEMDomain
		procedure,public :: showMaterials => showMaterialsFEMDomain
		procedure,public :: showBoundaries => showBoundariesFEMDomain
		procedure,public :: stl => stlFEMDomain
		procedure,public :: obj => objFEMDomain

		procedure,pass :: vtk_MPI_FEMDOmain
		procedure,pass :: vtkFEMDOmain

		generic :: vtk => vtkFEMDomain, vtk_MPI_FEMDOmain
		procedure,public :: x3d => x3dFEMDomain
		procedure,public :: csv => csvFEMDomain

		! >>> revising for adopting quad mesh >>> (2024.05.13)
		! matrices
		procedure,public :: Bmatrix => BMatrixFEMDomain ! <<< now <<<
		procedure,public :: Dmatrix => DMatrixFEMDomain
		procedure,public :: MassVector => MassVectorFEMDomain
		procedure,public :: StrainMatrix => StrainMatrixFEMDomain
		procedure,public :: StrainVector => StrainVectorFEMDomain
		procedure,public :: StressMatrix => StressMatrixFEMDomain
		procedure,public :: StressVector => StressVectorFEMDomain
		procedure,public :: ViscousBoundaryForce => ViscousBoundaryForceFEMDomain
		! Element-wize matrix
		procedure,pass :: DiffusionMatrixFEMDomain 
		procedure,pass :: StiffnessMatrixFEMDomain 
        procedure,pass :: MassMatrixFEMDomain


		procedure,public :: ConnectMatrix => ConnectMatrixFEMDomain 
		procedure,public :: ConnectVector => ConnectVectorFEMDomain 
		procedure,public :: ElementVector => ElementVectorFEMDomain 
		procedure,public :: GlobalVector => GlobalVectorFEMDomain 
		procedure,public :: TractionVector => TractionVectorFEMDomain
		procedure,public :: PointForceVector => PointForceVectorFEMDomain
		procedure,public :: FlowVector => FlowVectorFEMDomain

		! Domain-wize matrix (as CRS-format)

		procedure,pass :: DiffusionMatrix_as_CRS_FEMDomain
		procedure,pass :: StiffnessMatrix_as_CRS_FEMDomain
		procedure,pass :: MassMatrix_as_CRS_FEMDomain
		procedure,pass :: M_inv_K_Matrix_CRS_FEMDomain
		procedure,pass :: ZeroMatrix_as_CRS_FEMDomain
		procedure,pass :: ZeroMatrix_as_COO_FEMDomain

		generic ::DiffusionMatrix => DiffusionMatrixFEMDomain,DiffusionMatrix_as_CRS_FEMDomain
		generic ::StiffnessMatrix => StiffnessMatrixFEMDomain,StiffnessMatrix_as_CRS_FEMDomain
		generic ::MassMatrix => MassMatrixFEMDomain,MassMatrix_as_CRS_FEMDomain
		generic ::M_inv_K_Matrix => M_inv_K_Matrix_CRS_FEMDomain
		generic ::ZeroMatrix => ZeroMatrix_as_CRS_FEMDomain
		generic ::ZeroMatrix_as_COO => ZeroMatrix_as_COO_FEMDomain
		
		
		! <<< revising for adopting quad mesh <<< (2024.05.13)



		procedure,public :: loadPoints => loadPointsFEMDomain
		procedure,public :: particles  => particlesFEMDomain

		procedure,public :: sync => syncFEMDomain

		!# physical modifiers
		! Is it necessary?
		!procedure :: grub    => grubFEMDomain ! grub some part of object in range
		!procedure :: hold    => holdFEMDomain ! hold some part of object in range
		!procedure :: release => releaseFEMDomain ! 
		
    end type FEMDomain_

	!type:: FEMDomainp_
	!	type(FEMDomain_),pointer :: FEMDomain
	!end type
	
	type,extends(FEMDomain_) :: STFEMDomain_
        type(ShapeFunction_)    :: TimeShapeFunction
        type(Mesh_)             :: TimeMesh
    end type

	type :: FEMDomainp_
		type(FEMDomain_),pointer :: femdomainp => null()
	contains
		procedure,public :: getMyID => getMyIDFEMDomainp
		procedure,public :: overset => overset_FEMDomainp
		procedure,public :: connect => overset_FEMDomainp
		procedure,public :: overlap => overset_FEMDomainp
	end type


	public :: operator(+)
	
	interface operator(+)
		module procedure appendfemdomain
	end interface

	interface ZeroMatrix_as_CRS
		module procedure ZeroMatrix_as_CRS_FEMDomains
	end interface ZeroMatrix_as_CRS

	interface to_composite_beam
		module procedure to_composite_beam_FEMDomain
	end interface


	! for FEMDomainPointers
	interface to_ptr
		module procedure to_ptr_femdomain,to_ptr_femdomains
	end interface

	interface num_node
		module procedure num_node_femdomain_pointers
	end interface 
	
	interface num_element
		module procedure num_element_femdomain_pointers
	end interface 

	interface get_element_idx
		module procedure get_element_idx_FEMDomainPointer
	end interface

	interface get_node_list
		module procedure get_node_list_by_range_FEMDP
	end interface

	interface export_vtk
		module procedure export_vtk_FEMDomainPointer
	end interface export_vtk

contains

! ####################################################################
subroutine addFEMDomain(this,mesh,from,length,rot_x,rot_y,rot_z,x,y,z,dx,dy,dz)
	class(FEMDomain_),intent(inout) :: this
    class(Mesh_),optional,intent(inout)    :: mesh
    integer(int32),optional,intent(in) :: from
    real(real64),optional,intent(in) :: length,rot_x,rot_y,rot_z,x,y,z,dx,dy,dz
	call this%mesh%add(mesh,from,length,rot_x,rot_y,rot_z,x=x,y=y,z=z,dx=dx,dy=dy,dz=dz)
end subroutine
! ####################################################################

! ####################################################################
function lengthFEMDomain(this) result(length)
	class(FEMDomain_),intent(in) :: this
	real(real64) :: length(3)

	length(:)=this%Mesh%length()
end function

! ####################################################################


! ####################################################################
function x_lenFEMDomain(this) result(length)
	class(FEMDomain_),intent(in) :: this
	real(real64) :: length

	length = this%xmax()-this%xmin()
end function

! ####################################################################


! ####################################################################
function y_lenFEMDomain(this) result(length)
	class(FEMDomain_),intent(in) :: this
	real(real64) :: length

	length = this%ymax()-this%ymin()
end function

! ####################################################################

! ####################################################################
function z_lenFEMDomain(this) result(length)
	class(FEMDomain_),intent(in) :: this
	real(real64) :: length

	length = this%zmax()-this%zmin()
end function

! ####################################################################
subroutine openFEMDomain(this,path,name)

	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: path
	character(*),optional,intent(in) :: name
	character(:),allocatable :: pathi
	type(IO_) :: f
	integer(int32) :: n


	if(index(path,".vtk")/=0 )then
		call this%ImportVTKFile(name=path)
		return
	endif


	if(index(path,".stl")/=0 )then
		call this%ImportSTLFile(name=path)
		return
	endif

	if(present(name) )then
		if(index(name,".vtk")/=0 )then
			call this%ImportVTKFile(name=path//"/"//name)
			return
		endif
	endif

	! remove and initialze
	call this%remove()

	if(present(name) )then
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call execute_command_line("mkdir -p "//pathi)
		call execute_command_line("mkdir -p "//pathi//"/"//name )
		call this%Mesh%open(path=pathi//"/"//name ,name="Mesh") !implement!
		call this%MaterialProp%open(path=pathi//"/"//name ,name="MaterialProp")!implement!
		call this%Boundary%open(path=pathi//"/"//name ,name="Boundary")!implement!
		call this%ControlPara%open(path=pathi//"/"//name ,name="ControlPara")!implement!
		call this%ShapeFunction%open(path=pathi//"/"//name ,name="ShapeFunction")!implement!

		call f%open(pathi//"/"//name //"/"//"FEMDomain"//".prop" )
		write(f%fh,*) this%RealTime
		write(f%fh,*) this%NumOfDomain
		write(f%fh, '(A)' ) this%FilePath
		write(f%fh, '(A)' ) this%FileName
		write(f%fh, '(A)' ) this%name
		write(f%fh, '(A)' ) this%dtype
		write(f%fh, '(A)' ) this%SolverType
		write(f%fh, '(A)' ) this%Category1
		write(f%fh, '(A)' ) this%Category2
		write(f%fh, '(A)' ) this%Category3
		write(f%fh,*) this%timestep, this%NumberOfBoundaries, this%NumberOfMaterials
		call f%close()
	else
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call execute_command_line("mkdir -p "//pathi)
		call execute_command_line("mkdir -p "//pathi//"/FEMDomain")
		call this%Mesh%open(path=pathi//"/"//"FEMDomain",name="Mesh")
		call this%MaterialProp%open(path=pathi//"/"//"FEMDomain",name="MaterialProp")
		call this%Boundary%open(path=pathi//"/"//"FEMDomain",name="Boundary")
		call this%ControlPara%open(path=pathi//"/"//"FEMDomain",name="ControlPara")
		call this%ShapeFunction%open(path=pathi//"/"//"FEMDomain",name="ShapeFunction")

		call f%open(pathi//"/FEMDomain"//"/FEMDomain"//".prop" )
		write(f%fh,*) this%RealTime
		write(f%fh,*) this%NumOfDomain
		write(f%fh, '(A)' ) this%FilePath
		write(f%fh, '(A)' ) this%FileName
		write(f%fh, '(A)' ) this%name
		write(f%fh, '(A)' ) this%dtype
		write(f%fh, '(A)' ) this%SolverType
		write(f%fh, '(A)' ) this%Category1
		write(f%fh, '(A)' ) this%Category2
		write(f%fh, '(A)' ) this%Category3
		write(f%fh,*) this%timestep, this%NumberOfBoundaries, this%NumberOfMaterials
		call f%close()
	endif

end subroutine
! ####################################################################


recursive subroutine removeElementFEMDomain(this,x_min,x_max,y_min,y_max,z_min,z_max,&
	xr,yr,zr)

	class(FEMDomain_),intent(inout) :: this
	real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max,&
		xr(1:2),yr(1:2),zr(1:2)
	real(real64) :: xr_(1:2),yr_(1:2),zr_(1:2)
	real(real64) :: xr__(1:2),yr__(1:2),zr__(1:2)
	real(real64),allocatable :: x(:)
	integer(int32),allocatable :: killElemList(:)
	integer(int32) :: i

	killElemList = zeros(this%ne())
	xr_(1) = input(default=this%x_min(),option=x_min )
	xr_(2) = input(default=this%x_max(),option=x_max )
	yr_(1) = input(default=this%y_min(),option=y_min )
	yr_(2) = input(default=this%y_max(),option=y_max )
	zr_(1) = input(default=this%z_min(),option=z_min )
	zr_(2) = input(default=this%z_max(),option=z_max )

	if(present(xr) )then
		xr_ = xr
	endif
	

	if(present(yr) )then
		yr_ = yr
	endif


	if(present(zr) )then
		zr_ = zr
	endif

	do i=1,this%ne()
		x = this%centerPosition(ElementID=i)
		if(xr_(1) <= x(1) .and. x(1) <= xr_(2) )then
			if(yr_(1) <= x(2) .and. x(2) <= yr_(2) )then
				if(zr_(1) <= x(3) .and. x(3) <= zr_(2) )then
					killElemList(i) = 1
				endif
			endif
		endif
	enddo

	call this%killElement(blacklist=killElemList,flag=1)

end subroutine

! ####################################################################
subroutine removeFEMDomain(this)
	class(FEMDomain_),intent(inout) :: this
	

	! remove all objects

	call this%Mesh%remove()
	call this%MaterialProp%remove()
	call this%Boundary%remove()
	call this%ControlPara%remove()
	call this%ShapeFunction%remove()

	if(allocated(this%Meshes))then
		deallocate(this%Meshes)
	endif
	if(allocated(this%Materials))then
		deallocate(this%Materials)
	endif
	if(allocated(this%Boundaries))then
		deallocate(this%Boundaries)
	endif
	!if(allocated(this%FEMDomains))then
	!	deallocate(this%FEMDomains)
	!endif


	if(allocated(this%scalar) )then
		deallocate(this%scalar)
	endif
	if(allocated(this%vector) )then
		deallocate(this%vector)
	endif
	if(allocated(this%tensor) )then
		deallocate(this%tensor)
	endif

	this%RealTime=1.0d0
	this%NumOfDomain=1
	this%FilePath="None"
	this%FileName="None"
	this%Name="None"
	this%Dtype="None"
	this%SolverType="None"
	this%Category1 ="None"
	this%Category2="None"
	this%Category3="None"
	this%timestep=1
	this%NumberOfBoundaries=0
	this% NumberOfMaterials=0

	if(allocated(this%OversetConnect )) deallocate(this%OversetConnect)
	if(allocated(this%OversetExists) ) deallocate(this%OversetExists)
	this%num_oversetconnect = 0

	this%total_rotation = 0.0d0

end subroutine
! ####################################################################


! ####################################################################
subroutine saveFEMDomain(this,path,name)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: path
	character(*),optional,intent(in) :: name
	character(:),allocatable :: pathi
	type(IO_) :: f
	integer(int32) :: n

	if(present(name) )then
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call execute_command_line("mkdir -p "//pathi)
		call execute_command_line("mkdir -p "//pathi//"/"//name )
		call this%Mesh%save(path=pathi//"/"//name ,name="Mesh")
		call this%MaterialProp%save(path=pathi//"/"//name ,name="MaterialProp")
		call this%Boundary%save(path=pathi//"/"//name ,name="Boundary")
		call this%ControlPara%save(path=pathi//"/"//name ,name="ControlPara")
		call this%ShapeFunction%save(path=pathi//"/"//name ,name="ShapeFunction")

		call f%open(pathi//"/"//name ,"/"//"FEMDomain",".prop" )
		write(f%fh,*) this%RealTime
		write(f%fh,*) this%NumOfDomain
		write(f%fh, '(A)' ) this%FilePath
		write(f%fh, '(A)' ) this%FileName
		write(f%fh, '(A)' ) this%name
		write(f%fh, '(A)' ) this%dtype
		write(f%fh, '(A)' ) this%SolverType
		write(f%fh, '(A)' ) this%Category1
		write(f%fh, '(A)' ) this%Category2
		write(f%fh, '(A)' ) this%Category3
		write(f%fh,*) this%timestep, this%NumberOfBoundaries, this%NumberOfMaterials
		call f%close()
	else
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call execute_command_line("mkdir -p "//pathi)
		call execute_command_line("mkdir -p "//pathi//"/FEMDomain")
		call this%Mesh%save(path=pathi//"/"//"FEMDomain",name="Mesh")
		call this%MaterialProp%save(path=pathi//"/"//"FEMDomain",name="MaterialProp")
		call this%Boundary%save(path=pathi//"/"//"FEMDomain",name="Boundary")
		call this%ControlPara%save(path=pathi//"/"//"FEMDomain",name="ControlPara")
		call this%ShapeFunction%save(path=pathi//"/"//"FEMDomain",name="ShapeFunction")

		call f%open(pathi//"/FEMDomain"//"/FEMDomain"//".prop" )
		write(f%fh,*) this%RealTime
		write(f%fh,*) this%NumOfDomain
		write(f%fh, '(A)' ) this%FilePath
		write(f%fh, '(A)' ) this%FileName
		write(f%fh, '(A)' ) this%name
		write(f%fh, '(A)' ) this%dtype
		write(f%fh, '(A)' ) this%SolverType
		write(f%fh, '(A)' ) this%Category1
		write(f%fh, '(A)' ) this%Category2
		write(f%fh, '(A)' ) this%Category3
		write(f%fh,*) this%timestep, this%NumberOfBoundaries, this%NumberOfMaterials
		call f%close()
	endif
end subroutine 

!##################################################
function divide_mpi_FEMDomain(this,mpid) result(FEMDomain)
	class(FEMDomain_),intent(inout)::this
	type(FEMDomain_) :: FEMDomain
	
    type(Mesh_),allocatable :: meshes(:)
	type(MPI_),intent(inout) :: mpid
	integer(int32) :: n

	integer(int32),allocatable :: proc_id(:),next_elems(:),global_node_idx(:)

	! incremental method
	integer(int32),allocatable :: subdomain_idx(:),elem_idx(:)
	integer(int32),allocatable :: subdomain_center_elem_idx(:),node_idx(:),&
		sendbuf(:),recvbuf(:),new_combination(:),new_info(:),my_node_idx(:),your_node_idx(:)
	real(real64),allocatable :: subdomain_center_coord(:,:) 

	real(real64),allocatable :: norms(:),new_center(:),num_shared_elem(:)

	integer(int32) :: i,j,elem_id,last_elem_id,max_elem_per_subdomain,elem_counter,&
		subdomain_id,ne,nn,elemid,k,proc_idx

	type(Random_) :: random
	
	!>>> Passed the test "Tutorial/fem/divide_mesh.f90"

	! split obj into n objects
	! incremental method(逐次追加法)
	n = mpid%petot

	if(n==1)then
		femdomain = this
		return
	endif
	subdomain_idx = int(zeros(this%ne() ) )
	subdomain_center_coord = zeros(n,this%nd() )
	subdomain_center_elem_idx = int(zeros(n))
	elem_idx = [(i,i=1,this%ne() ) ]
	norms = zeros(n)
	! set kernel of each subdomain
	subdomain_center_elem_idx = random%draw(elem_idx,n)
	



	if(mpid%myrank==0)then

		!num_shared_elem = int(zeros(this%ne() ) )
		!!$OMP parallel do
		!do i=1,size(this%mesh%elemnod,1)
		!	num_shared_elem(this%mesh%elemnod(i,:) ) = num_shared_elem(this%mesh%elemnod(i,:) ) + 1
		!enddo
		!!$OMP end parallel do


		!$OMP parallel do
		do i=1,n
			subdomain_center_coord(i,:) =  this%centerPosition(ElementID=subdomain_center_elem_idx(i) )
		enddo
		!$OMP end parallel do

		!$OMP parallel do private(new_center,norms)
		do i=1,this%ne() 
			new_center =  this%centerPosition(ElementID=i)
			do j=1,n
				norms(j) = dot_product(new_center-subdomain_center_coord(j,:),new_center-subdomain_center_coord(j,:))
			enddo
			subdomain_idx(i) = minvalid(norms)
		enddo
		!$OMP end parallel do

	endif
	call mpid%bcast(from=0,val=subdomain_idx)

	! select ones
	i = mpid%myrank+1
	ne = countif(array=subdomain_idx,Equal=.true.,value=i)
	allocate(femdomain%mesh%elemnod(ne,this%nne() ) ) 
	elemid = 0
	do j=1,this%ne()
		if(subdomain_idx(j)== i)then
			elemid = elemid + 1
			! store global node id
			femdomain%mesh%elemnod(elemid,:) = this%mesh%elemnod(j,:)
		endif
	enddo
	femdomain%mpi_global_node_idx = RemoveOverlap(to_vector(femdomain%mesh%elemnod))
	femdomain%mesh%nodcoord = zeros( size(femdomain%mpi_global_node_idx),this%nd() )

	! Global index to local index
	do j=1,size(femdomain%mpi_global_node_idx)
		femdomain%mesh%nodcoord(j,:) = this%mesh%nodcoord( femdomain%mpi_global_node_idx(j),: )
	enddo
	do j=1,size(femdomain%mesh%elemnod,1)
		do k=1,size(femdomain%mesh%elemnod,2)
			femdomain%mesh%elemnod(j,k) = 1 .of. getIdx(vec=femdomain%mpi_global_node_idx,&
				equal_to=femdomain%mesh%elemnod(j,k)) 
		enddo
	enddo

	
	do proc_idx=0,mpid%petot-1
		if(mpid%myrank == proc_idx)then
			k = size(femdomain%mpi_global_node_idx)
			call mpid%bcast(from=proc_idx,val=k)
			call mpid%bcast(from=proc_idx,val=femdomain%mpi_global_node_idx)
		else
			call mpid%bcast(from=proc_idx,val=k)
			recvbuf = int(zeros(k) )
			call mpid%bcast(from=proc_idx,val=recvbuf)
			new_combination = femdomain%mpi_global_node_idx .cap. recvbuf
			if(size(new_combination)==0 )cycle
			my_node_idx   = getIdx(vec=femdomain%mpi_global_node_idx,equal_to=new_combination)
			your_node_idx = getIdx(vec=recvbuf,equal_to=new_combination)

			if(size(new_combination)==0 )then
				! do nothing
			else
				if(.not.allocated(femdomain%mpi_shared_node_info) )then
					allocate(femdomain%mpi_shared_node_info( size(new_combination),3 ))
					femdomain%mpi_shared_node_info(:,1) = my_node_idx
					femdomain%mpi_shared_node_info(:,2) = (proc_idx)*int(ones(size(new_combination) ) )
					femdomain%mpi_shared_node_info(:,3) = your_node_idx
				else
					femdomain%mpi_shared_node_info = &
						femdomain%mpi_shared_node_info .v. &
						(my_node_idx .h. &
						(proc_idx)*int(ones(size(new_combination) ) )&
							 .h. your_node_idx)
				endif
			endif
		endif
		call mpid%barrier()
	enddo
	

end function


!##################################################
function divide_nFEMDomain(this,n) result(FEMDomains)
	class(FEMDomain_),intent(inout)::this
	type(FEMDomain_),allocatable :: FEMDomains(:)
	!integer(int32),allocatable :: FEMDomains(:)
	
    type(Mesh_),allocatable :: meshes(:)
	integer(int32),intent(in) :: n

	integer(int32),allocatable :: proc_id(:),next_elems(:),global_node_idx(:)

	! incremental method
	integer(int32),allocatable :: subdomain_idx(:),elem_idx(:),global_to_local(:)
	integer(int32),allocatable :: subdomain_center_elem_idx(:),&
		node_idx(:),count_shared(:),mpi_shared_node_info(:,:)
	real(real64),allocatable :: subdomain_center_coord(:,:) 

	real(real64),allocatable :: norms(:),new_center(:)

	integer(int32) :: i,j,elem_id,last_elem_id,max_elem_per_subdomain,elem_counter,&
		subdomain_id,ne,nn,elemid,k
	type(Random_) :: random

	type :: global_to_local_
		integer(int32),allocatable :: global_to_local(:)
	end type
	type(global_to_local_),allocatable :: forall_g2l(:)

	if(n==1)then
		allocate(femdomains(1) )
		femdomains(1) = this
		return
	endif

	


	! split obj into n objects
	! incremental method(逐次追加法)
	subdomain_idx = int(zeros(this%ne() ) )
	subdomain_center_coord = zeros(n,this%nd() )
	subdomain_center_elem_idx = int(zeros(n))
	elem_idx = [(i,i=1,this%ne() ) ]
	norms = zeros(n)
	! set kernel of each subdomain
	subdomain_center_elem_idx = random%draw(elem_idx,n)

	!$OMP parallel do
	do i=1,n
		subdomain_center_coord(i,:) =  this%centerPosition(ElementID=subdomain_center_elem_idx(i) )
	enddo
	!$OMP end parallel do



	! select ones
	!$OMP parallel do private(new_center,norms)
	do i=1,this%ne() 
		new_center =  this%centerPosition(ElementID=i)
		do j=1,n
			norms(j) = dot_product(new_center-subdomain_center_coord(j,:),new_center-subdomain_center_coord(j,:))
		enddo
		subdomain_idx(i) = minvalid(norms)
	enddo
	!$OMP end parallel do

	
	allocate(femdomains(n) )

	!>> slow
	allocate(forall_g2l(n) )
	!$OMP parallel do private(ne,elemid,j,k)
	do i=1,n
		ne = countif(array=subdomain_idx,Equal=.true.,value=i)
		allocate(femdomains(i)%mesh%elemnod(ne,this%nne() ) ) 
		elemid = 0
		do j=1,this%ne()
			if(subdomain_idx(j)== i)then
				elemid = elemid + 1
				! store global node id
				femdomains(i)%mesh%elemnod(elemid,:) = this%mesh%elemnod(j,:)
			endif
		enddo

		femdomains(i)%mpi_global_node_idx = &
			RemoveOverlap(to_vector(femdomains(i)%mesh%elemnod))
		
		femdomains(i)%mesh%nodcoord = zeros( size(femdomains(i)%mpi_global_node_idx),this%nd() )
		
		do j=1,size(femdomains(i)%mpi_global_node_idx)
			femdomains(i)%mesh%nodcoord(j,:) = this%mesh%nodcoord( femdomains(i)%mpi_global_node_idx(j),: )
		enddo

		! change global node-idx to local node-idx
!		if(present(fast) )then
		if(.not.allocated(forall_g2l(i)%global_to_local) )then
			allocate(forall_g2l(i)%global_to_local( minval(femdomains(i)%mpi_global_node_idx):&
			maxval(femdomains(i)%mpi_global_node_idx)   ))
		endif
		
		do j=1,size(femdomains(i)%mpi_global_node_idx)
			forall_g2l(i)%global_to_local( femdomains(i)%mpi_global_node_idx(j) ) = j
		enddo
		do j=1,size(femdomains(i)%mesh%elemnod,1)
			do k=1,size(femdomains(i)%mesh%elemnod,2)
				femdomains(i)%mesh%elemnod(j,k) = forall_g2l(i)%global_to_local(femdomains(i)%mesh%elemnod(j,k) )
			enddo
		enddo
		
!		else
!			do j=1,size(femdomains(i)%mesh%elemnod,1)
!				do k=1,size(femdomains(i)%mesh%elemnod,2)
!					femdomains(i)%mesh%elemnod(j,k) = &
!					1 .of. getIdx(vec=femdomains(i)%mpi_global_node_idx,equal_to=femdomains(i)%mesh%elemnod(j,k)) 
!				enddo
!			enddo
!		endif

	enddo
	!$OMP end parallel do



	!$OMP parallel do private(j,count_shared,mpi_shared_node_info)
	do i=1,size(femdomains)
		do j=1,size(femdomains)
			if(i==j)cycle
			count_shared = femdomains(i)%mpi_global_node_idx &
				.cap. femdomains(j)%mpi_global_node_idx 
			
			if(size(count_shared)==0 )then
				cycle
			endif

!			if(present(fast) )then

			mpi_shared_node_info = int(zeros(size(count_shared),3 )  )
			
			mpi_shared_node_info(:,1) = forall_g2l(i)%global_to_local(count_shared)
			mpi_shared_node_info(:,2) = j-1
			mpi_shared_node_info(:,3) = forall_g2l(j)%global_to_local(count_shared)
			
			if(.not.allocated(femdomains(i)%mpi_shared_node_info))then
				femdomains(i)%mpi_shared_node_info = mpi_shared_node_info
			else
				femdomains(i)%mpi_shared_node_info = &
				femdomains(i)%mpi_shared_node_info .v. mpi_shared_node_info
			
			endif
!			else

!				mpi_shared_node_info = int(zeros(size(count_shared),3 )  )
!
!				mpi_shared_node_info(:,1) = &
!						getIdx(femdomains(i)%mpi_global_node_idx,&
!					equal_to=count_shared)
!				mpi_shared_node_info(:,2) = j-1
!				mpi_shared_node_info(:,3) = &
!						getIdx(femdomains(j)%mpi_global_node_idx,&
!					equal_to=count_shared)
!				
!				if(.not.allocated(femdomains(i)%mpi_shared_node_info))then
!					femdomains(i)%mpi_shared_node_info = mpi_shared_node_info
!				else
!					femdomains(i)%mpi_shared_node_info = &
!					femdomains(i)%mpi_shared_node_info .v. mpi_shared_node_info
!				
!				endif
!			endif
			
		enddo
	enddo
	!$OMP end parallel do
	


	return


!	return
!
!		! greedy method
!		allocate(FEMDomains(n))
!		proc_id = int(zeros(this%ne() ) )
!		proc_id(1) = 1
!		last_elem_id = 1
!		max_elem_per_subdomain = size(proc_id)/n + 1
!		! divide domain by Greedy method
!		do subdomain_id = 1,n
!			elem_counter = 0
!			do
!				next_elems = this%getNeighboringElementList(ElementID=last_elem_id)
!				do i=1,size(next_elems)
!					elem_counter = elem_counter + 1
!					proc_id( next_elems(i) ) = subdomain_id
!					if(elem_counter > max_elem_per_subdomain)then
!						exit
!					endif
!				enddo
!				if(elem_counter > max_elem_per_subdomain)then
!					exit
!				endif
!			enddo
!			if(elem_counter > max_elem_per_subdomain)then
!				do i=1,size(proc_id)
!					last_elem_id = 0
!					if(proc_id(i)==0 )then
!						last_elem_id = i
!					endif
!				enddo
!			endif
!		enddo
!		femdomains = proc_id
!	return
	! >>>> following has some bugs
!		! split obj into n objects
!		allocate(FEMDomains(n))
!
!		! Greedy algorithm
!		if(this%Mesh%empty() .eqv. .true. )then
!			print *, "divideFEMDomain >> ERROR >> No mesh is imported."
!			stop
!		endif
!
!		meshes = this%mesh%divide(n)
!
!		! import mesh
!		do i=1,n
!			call FEMDomains(i)%import(Mesh=meshes(i))
!		enddo

end function divide_nFEMDomain
!##################################################

!##################################################
subroutine displayFEMDomain(this,path,name,extention,field)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: path,name,extention
	integer(int32) :: i,j,n
	real(real64),optional,intent(in) :: field(:)
	real(real64) :: val

	open(10,file=path//name//extention )
	if( extention == ".vtk" )then
		write(10,'(A)' ) "# vtk DataFile Version 2.0"
		write(10,'(A)' ) "Cube example"
		write(10,'(A)' ) "ASCII"
		write(10,'(A)' ) "DATASET POLYDATA"
		write(10,'(A)' ,advance="no") "POINTS "
		write(10,'(i10)' ,advance="no")size(this%mesh%NodCoord,1)
		write(10,'(A)')" float"
		do i=1,size(this%mesh%NodCoord,1)
			do j=1,size(this%mesh%NodCoord,2)
				if(j==size(this%mesh%NodCoord,2))then
					write(10,'(f20.8)' ) this%mesh%NodCoord(i,j)
				else
					write(10,'(f20.8)', advance="no" ) this%mesh%NodCoord(i,j)
					write(10,'(A)', advance="no" ) " "
				endif
			enddo
		enddo
		write(10,'(A)',advance="no")" POLYGONS "
		write(10,'(i10)',advance="no") 6*size(this%mesh%ElemNod,1)
		write(10,'(A)',advance="no") " "
		write(10,'(i10)') size(this%mesh%ElemNod,1)*5*6
		do i=1,size(this%mesh%ElemNod,1)
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,4)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,5)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,6)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,7)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,8)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,6)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,5)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,4)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,8)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,7)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,1)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,5)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,8)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,4)-1
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,2)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,3)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,7)-1
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") this%mesh%ElemNod(i,6)-1
			write(10,'(A)') " "
		enddo
		write(10,'(A)') "CELL_DATA 6"
	elseif(extention == ".ply")then
		write(10,'(A)')"ply"
		write(10,'(A)')"format ascii 1.0"
		write(10,'(A)',advance="no")"element vertex "
		write(10,'(i10)') size(this%mesh%NodCoord,1)
		write(10,'(A)')"property float32 x"
		write(10,'(A)')"property float32 y"
		write(10,'(A)')"property float32 z"
		write(10,'(A)')"property uchar red"
		write(10,'(A)')"property uchar green"
		write(10,'(A)')"property uchar blue"
		write(10,'(A)',advance="no")"element face "
		write(10,'(i10)') size(this%mesh%ElemNod,1)*6
		write(10,'(A)')"property list uint8 int32 vertex_indices"
		write(10,'(A)') "end_header"
		do i=1,size(this%mesh%NodCoord,1)
			do j=1,size(this%mesh%NodCoord,2)
				if(j==size(this%mesh%NodCoord,2))then
					write(10,'(f20.8)', advance="no"  ) this%mesh%NodCoord(i,j)
					write(10,'(A)', advance="no" ) " "
				else
					write(10,'(f20.8)', advance="no" ) this%mesh%NodCoord(i,j)
					write(10,'(A)', advance="no" ) " "
				endif
			enddo
			write(10,'(A)', advance="no" ) " "
			write(10,'(i3)',advance="no") int(this%mesh%NodCoord(i,1)*255.0d0/maxval(this%mesh%NodCoord(:,1) ))
			write(10,'(A)', advance="no" ) " "
			write(10,'(i3)',advance="no") int(this%mesh%NodCoord(i,2)*255.0d0/maxval(this%mesh%NodCoord(:,2) ))
			write(10,'(A)', advance="no" ) " "
			write(10,'(i3)') int(this%mesh%NodCoord(i,3)*255.0d0/maxval(this%mesh%NodCoord(:,3) ))
		enddo
		do i=1,size(this%mesh%ElemNod,1)
			val = dble(this%mesh%ElemNod(i,1)-1)
			if(present(field) )then
				val=field(i)
			endif
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)') " "
			write(10,'(A)',advance="no") "4 "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)',advance="no") " "
			write(10,'(i10)',advance="no") int(val)
			write(10,'(A)') " "
		enddo

	else
		print *, "Invalid extention :: ",extention
		stop
	endif
	close(10)

end subroutine displayFEMDomain
!##################################################


!##################################################
subroutine fieldFEMDomain(this,scalar,vector,tensor)
	class(FEMDomain_),intent(inout) :: this
	real(real64),optional,intent(in) :: scalar(:),vector(:,:),tensor(:,:,:)
	integer(int32) :: i,j,k,n

	! import data >> to obj
	if(present(scalar) )then
		if(size(scalar,1)==0 )then
			print *, "displayFEMDomain :: ERROR :: scalar is not allocated."
			stop 
		endif
		if(allocated(this%scalar) )then
			deallocate(this%scalar)
		endif
		i=size(scalar)
		if(this%mesh%empty() .eqv. .true.)then
			print *, "displayFEMDomain :: ERROR :: element is not imported."
			stop
		endif
		if(i/=size(this%mesh%ElemNod,1))then
			print *, "displayFEMDomain :: ERROR :: size(scalar/=size(this%mesh%ElemNod,1)"
			stop 
		endif
		allocate(this%scalar(i) )
		this%scalar(:) = scalar(:)
	endif
	
	! import data >> to obj
	if(present(vector) )then
		if(size(vector,1)==0 )then
			print *, "displayFEMDomain :: ERROR :: vector is not allocated."
			stop 
		endif
		if(allocated(this%vector) )then
			deallocate(this%vector)
		endif
		i=size(vector,1)
		j=size(vector,2)
		if(this%mesh%empty() .eqv. .true.)then
			print *, "displayFEMDomain :: ERROR :: element is not imported."
			stop
		endif
		if(i/=size(this%mesh%ElemNod,1))then
			print *, "displayFEMDomain :: ERROR :: size(vector/=size(this%mesh%ElemNod,1)"
			stop 
		endif
		allocate(this%vector(i,j) )
		this%vector(:,:) = vector(:,:)
	endif
	
	! import data >> to obj
	if(present(tensor) )then
		if(size(tensor,1)==0 )then
			print *, "displayFEMDomain :: ERROR :: tensor is not allocated."
			stop 
		endif
		if(allocated(this%tensor) )then
			deallocate(this%tensor)
		endif
		i=size(tensor,1)
		j=size(tensor,2)
		k=size(tensor,3)
		if(this%mesh%empty() .eqv. .true.)then
			print *, "displayFEMDomain :: ERROR :: element is not imported."
			stop
		endif
		if(i/=size(this%mesh%ElemNod,1))then
			print *, "displayFEMDomain :: ERROR :: size(tensor/=size(this%mesh%ElemNod,1)"
			stop 
		endif
		allocate(this%tensor(i,j,k) )
		this%tensor(:,:,:) = tensor(:,:,:)
	endif

end subroutine fieldFEMDomain
!##################################################


!##################################################
subroutine DeallocateFEMDomain(this)
    class(FEMDomain_),intent(inout)::this

    call DeallocateMesh(this%Mesh)
    call DeallocateMaterialProp(this%MaterialProp)
    call DeallocateBoundary(this%Boundary)
    call DeallocateShapeFunction(this%ShapeFunction)    

end subroutine DeallocateFEMDomain
!##################################################



! ################################################
subroutine renameFEMDomain(this,Name)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: Name

	this%Name = ""
	this%Name = name

end subroutine renameFEMDomain


!##################################################
subroutine InitializeFEMDomain(this,Default,FileName,simple)
	class(FEMDomain_),intent(inout)::this
	character(*),optional,intent(in) :: FileName
    logical,optional,intent(in)::Default,simple


	this%FilePath="None"
	this%FileName="None"
	this%Name="None"
	this%SolverType="None"
	this%Category1 ="None"
	this%Category2="None"
	this%Category3="None"

	if(.not. present(FileName) )then
		this%FileName="noName"
	else
		this%FileName=FileName
	endif

	if(present(simple) )then
		if(simple .eqv. .true.)then
			return
		endif

	endif

    if(Default .eqv. .true.)then
        this%Dtype="FEMDomain"
    endif
    call InitializeMesh(this%Mesh)
    call InitializeMaterial(this%MaterialProp)
	call this%Boundary%Init(Default)
	this%timestep=0
    
    
    

end subroutine InitializeFEMDomain
!##################################################


!##################################################
subroutine showFEMDomain(this)
	class(FEMDomain_),intent(in)::this
	integer(int32)::i

	print *, "=========================="
	print *, "Name :: ",this%name
	print *, "Materials :: "
	if(.not.allocated(this%Materials) )then
		print *, "No material is imported"
	else
		do i=1,this%NumberOfMaterials
			if(associated(this%Materials(i)%materialp ) )then
				call this%Materials(i)%materialp%show()
			else
				cycle
			endif
		enddo
	endif
	print *, "Boundaries :: "
	if(.not.allocated(this%boundaries) )then
		print *, "No Boundary is imported"
	else
		do i=1,this%NumberOfBoundaries
			if(associated(this%Boundaries(i)%Boundaryp ) )then
				call this%Boundaries(i)%Boundaryp%show()
			else
				cycle
			endif
		enddo
	endif


end subroutine showFEMDomain
!##################################################

!##################################################
subroutine ImportFEMDomain(this,OptionalFileFormat,OptionalProjectName,FileHandle,Mesh,Boundaries&
		,Boundary,Materials, Material,NumberOfBoundaries,BoundaryID,NumberOfMaterials,MaterialID,&
		node,element,materialinfo,dirichlet,neumann,file)
	class(FEMDomain_),intent(inout)::this
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
	character(:),allocatable::ProjectName
	character(:),allocatable ::FileName
	character*9  :: DataType
	integer,allocatable::IntMat(:,:)
	real(8),allocatable::RealMat(:,:)
	integer,optional,intent(in)::FileHandle,NumberOfBoundaries,BoundaryID,MaterialID,NumberOfMaterials
	integer :: fh,i,j,k,NumOfDomain,n,m,DimNum,GpNum,nodenum,matnum, paranum
	character(:),allocatable ::  Msg,name,ch
	logical,optional,intent(in) :: Boundaries,Materials

	if(present(file) )then
		if(index(file,".vtk")/=0 )then
			call this%ImportVTKFile(name=file)
			print *, "imported ",file
			return
		endif
	endif

	if( getext(file) =="mesh" )then
		
		call f%open(file)
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
			if(ch == "Tetrahedra" )then
				read(f%fh,*)n
				allocate(mobj%ElemNod(n,4),mobj%ElemMat(n) )
				mobj%ElemMat(:) = 1
				do i=1,n
					read(f%fh,*) mobj%ElemNod(i,1:4)
				enddo
				exit
			elseif(ch == "End" )then
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
		call this%Mesh%copy(mobj)
		return
	endif

if(present(node) )then
	if(node .eqv. .true. )then
		if(.not. present(file) )then
			print *, "Please iput filename"
			stop
		endif
		call f%open(file)
		read(f%fh,*) nodenum, dimnum
		if(allocated(this%Mesh%NodCoord ) )then
			deallocate(this%Mesh%NodCoord)
		endif
		allocate(this%Mesh%NodCoord(nodenum, dimnum) )
		do i=1,nodenum
			read(f%fh,*) this%Mesh%NodCoord(i,:)
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
		call f%open(file)
		read(f%fh,*) nodenum, dimnum
		if(allocated(this%Mesh%ElemNod ) )then
			deallocate(this%Mesh%ElemNod)
		endif
		allocate(this%Mesh%ElemNod(nodenum, dimnum) )
		do i=1,nodenum
			read(f%fh,*) this%Mesh%ElemNod(i,:)
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
		call f%open(file)
		read(f%fh,*) nodenum
		if(allocated(this%Mesh%ElemMat ) )then
			deallocate(this%Mesh%ElemMat)
		endif
		allocate(this%Mesh%ElemMat(nodenum) )
		do i=1,nodenum
			read(f%fh,*) this%Mesh%ElemMat(i)
		enddo
		
		read(f%fh,*) matnum, paranum
		if(allocated(this%MaterialProp%MatPara ) )then
			deallocate(this%MaterialProp%MatPara)
		endif
		allocate(this%MaterialProp%MatPara(matnum, paranum) )
		do i=1,matnum
			read(f%fh,*) this%MaterialProp%MatPara(i,:)
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
		call f%open(file)
		dimnum=size(this%mesh%NodCoord,2)
		if(allocated(this%Boundary%DboundNum ) )then
			deallocate(this%Boundary%DboundNum)
		endif
		allocate(this%Boundary%DboundNum(dimnum) )
		read(f%fh,*) this%Boundary%DboundNum(:)
		if(allocated(this%Boundary%DboundNodID ) )then
			deallocate(this%Boundary%DboundNodID)
		endif
		allocate(this%Boundary%DboundNodID(maxval(this%Boundary%DboundNum),dimnum ) )
		if(allocated(this%Boundary%DBoundVal ) )then
			deallocate(this%Boundary%DBoundVal)
		endif
		allocate(this%Boundary%DBoundVal(maxval(this%Boundary%DboundNum),dimnum ) )
		
		do i=1,size(this%Boundary%DboundNodID,1)
			read(f%fh,*) this%Boundary%DboundNodID(i,:)
		enddo
		do i=1,size(this%Boundary%DboundVal,1)
			read(f%fh,*) this%Boundary%DboundVal(i,:)
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
		call f%open(file)
		dimnum=size(this%mesh%NodCoord,2)
		if(allocated(this%Boundary%NboundNum ) )then
			deallocate(this%Boundary%NboundNum)
		endif
		allocate(this%Boundary%NboundNum(dimnum) )
		read(f%fh,*) this%Boundary%NboundNum(:)
		if(allocated(this%Boundary%NboundNodID ) )then
			deallocate(this%Boundary%NboundNodID)
		endif
		allocate(this%Boundary%NboundNodID(maxval(this%Boundary%NboundNum),dimnum ) )
		if(allocated(this%Boundary%NBoundVal ) )then
			deallocate(this%Boundary%NBoundVal)
		endif
		allocate(this%Boundary%NBoundVal(maxval(this%Boundary%NboundNum),dimnum ) )
		
		do i=1,size(this%Boundary%NboundNodID,1)
			read(f%fh,*) this%Boundary%NboundNodID(i,:)
		enddo
		do i=1,size(this%Boundary%NboundVal,1)
			read(f%fh,*) this%Boundary%NboundVal(i,:)
		enddo
		call f%close()
		return
	endif
endif



if(present(Boundaries) )then
	if(Boundaries .eqv. .true.)then
		call this%ImportBoundaries(Boundary,NumberOfBoundaries,BoundaryID)
		return
	endif
endif

if(present(Materials) )then
	if(materials .eqv. .true.)then
		call this%ImportMaterials(Material,NumberOfMaterials,MaterialID)
		return
	endif
endif
if(present(Mesh) )then
	call this%Mesh%import(Mesh=Mesh)
	return
endif

!call DeallocateFEMDomain(this)
name="untitled"
this%FileName=input(default=name,option=OptionalProjectName)

if(present(FileHandle) )then
    fh=FileHandle
else
    fh =104
endif

if(present(OptionalFileFormat) )then
    FileFormat=OptionalFileFormat
else
    FileFormat=".scf"
endif


if(present(OptionalProjectName) )then
    ProjectName=OptionalProjectName
else
    ProjectName="untitled"
endif

FileName = ProjectName//FileFormat

!!print *, "Project : ",ProjectName
!!print *, "is Exported as : ",FileFormat," format"
!!print *, "File Name is : ",FileName

open(fh,file=FileName,status="old")


if(FileFormat==".scf" )then

    read(fh,*) DataType
    if(DataType/="domain")then
        print *, "ERROR :: Datatype ",DataType," is not valid."
        return
    endif
    this%Dtype=DataType
    read(fh,*) this%SolverType
    read(fh,*) this%NumOfDomain
    allocate(IntMat(this%NumOfDomain,2))
    allocate(this%Mesh%SubMeshNodFromTo(this%NumOfDomain,3) )
    allocate(this%Mesh%SubMeshElemFromTo(this%NumOfDomain,3) )
    
    do i=1,this%NumOfDomain
        this%Mesh%SubMeshNodFromTo(i,1)=i
        read(fh,*) this%Mesh%SubMeshNodFromTo(i,2),this%Mesh%SubMeshNodFromTo(i,3)
    enddo

    do i=1,this%NumOfDomain
        this%Mesh%SubMeshElemFromTo(i,1)=i
        read(fh,*) this%Mesh%SubMeshElemFromTo(i,3)
        if(i==1)then
            this%Mesh%SubMeshElemFromTo(i,2)=1    
        else
            this%Mesh%SubMeshElemFromTo(i,2)=this%Mesh%SubMeshElemFromTo(i-1,3)+1
        endif
    enddo


    read(fh,*) n,m
    DimNum=m

    allocate(this%Mesh%NodCoord(n,m) )
    call ImportArray(this%Mesh%NodCoord,OptionalFileHandle=fh)
	call CopyArray(this%Mesh%NodCoord,this%Mesh%NodCoordInit )

    read(fh,*) n,m
    

    read(fh,*)this%Mesh%ElemType

    !this%ShapeFunction%ElemType=this%Mesh%ElemType
    allocate(this%Mesh%ElemNod(n,m) )
    allocate(this%Mesh%ElemMat(n  ) )
    call ImportArray(this%Mesh%ElemNod,OptionalFileHandle=fh)
    do i=1,n
        read(fh,*) this%Mesh%ElemMat(i)
    enddo
    read(fh,*) n,m

    allocate(this%MaterialProp%MatPara(n,m) )
    call ImportArray(this%MaterialProp%MatPara,OptionalFileHandle=fh)

    !DirichletBoundary
    read(fh,*) n !DirichletBoundaryDimension
    
    if(n<=0)then
        print *, "ImportFEMDomain >> Caution :: no Dirichlet Boundary Condition is loaded. "
    else
        allocate(this%Boundary%DBoundNum(n ))
        read(fh,*) this%Boundary%DBoundNum(:)


        allocate(this%Boundary%DBoundNodID( maxval(this%Boundary%DBoundNum), size(this%Boundary%DBoundNum)  )  )
        allocate(this%Boundary%DBoundVal( maxval(this%Boundary%DBoundNum), size(this%Boundary%DBoundNum)  )  )

        this%Boundary%DBoundNodID(:,:)=-1
        this%Boundary%DBoundVal(:,:)  =0.0d0

        do i=1,size(this%Boundary%DBoundNum,1)
            do j=1,this%Boundary%DBoundNum(i)
                read(fh,*) this%Boundary%DBoundNodID(j,i)
                !!print *,this%Boundary%DBoundNodID(j,i)
            enddo
            do j=1,this%Boundary%DBoundNum(i)
                read(fh,*) this%Boundary%DBoundVal(j,i)
                !!print *,this%Boundary%DBoundVal(j,i)
            enddo
        enddo

    endif


    read(fh,*) DimNum

    if(DimNum<=0)then

        print *, "ImportFEMDomain >> Caution :: no Neumann Boundary Condition is loaded. "
    else
        read(fh,*) n
        allocate( this%Boundary%NBoundNum(DimNum))
        allocate(this%Boundary%NBoundNodID(n, size(this%Boundary%NBoundNum)  )  )
        allocate(this%Boundary%NBoundVal( n, size(this%Boundary%NBoundNum)  )  )
        this%Boundary%NBoundNodID(:,:)=-1
        this%Boundary%NBoundVal(:,:)  =0.0d0

        this%Boundary%NBoundNum(:)=n
        do i=1,n
            read(fh,*) m
            this%Boundary%NBoundNodID(i,:)=m
        enddo

        do i=1,n
            read(fh,*) this%Boundary%NBoundVal(i,:)
        enddo
    
    endif
        

    !######### Initial conditions #################
    ! For node-wize
    read(fh,*) DimNum
    if(DimNum<=0)then
        print *, "Caution :: no Initial Condition (Node-wise) Condition is loaded. "
    else
        read(fh,*) n
        allocate(this%Boundary%TBoundNodID(n,DimNum) )
        allocate(this%Boundary%TBoundVal(  n,DimNum) )
        allocate(this%Boundary%TBoundNum(  DimNum) )

        this%Boundary%TBoundNum(:)=n
        
        if(n/=0)then
            if(n<0)then
                print *, "ERROR :: number of initial conditions are to be zero"
            else
                do i=1,n
                    read(fh,*) this%Boundary%TBoundNodID(i,:)
                enddo
                do i=1,n
                    read(fh,*) this%Boundary%TBoundVal(i,:)
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
        allocate(this%Boundary%TBoundElemID(n) )
        allocate(this%Boundary%TBoundElemGpVal(n,GpNum,DimNum) )
        
        if(n/=0)then
            if(n<0)then
                print *, "ERROR :: number of initial conditions are to be zero"
            else
                do i=1,n
                    read(fh,*) this%Boundary%TBoundElemID(i)
                enddo
                do i=1,n
                    do j=1,GpNum
                        do k=1,DimNum
                            read(fh,*) this%Boundary%TBoundElemGpVal(i,j,k)
                        enddo
                    enddo
                enddo
            endif
        endif

    endif
    !######### Initial conditions #################

    read(fh,*) this%ControlPara%SimMode ,this%ControlPara%ItrTol,this%ControlPara%Timestep
     
    close(fh)
else
    !!print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
endif





end subroutine ImportFEMDomain
!##################################################


!##################################################
subroutine ImportMeshFEMDomain(this,Mesh)
	class(FEMDomain_),intent(inout)::this
	class(Mesh_),intent(inout)::Mesh

	call this%Mesh%copy(Mesh)
end subroutine
!##################################################

subroutine resizeFEMDomain(this,x_rate,y_rate,z_rate,x_len,y_len,z_len,&
	x,y,z)
	class(FEMDomain_),intent(inout) :: this
	real(real64),optional,intent(in) :: x_rate,y_rate,z_rate,x_len,y_len,z_len
	real(real64),optional,intent(in) :: x ,y ,z 
	
	call this%Mesh%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate,x_len=x_len,y_len=y_len,z_len=z_len)
	call this%Mesh%resize(x_len=x,y_len=y,z_len=z)

end subroutine


subroutine fatFEMDomain(this,ratio)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: ratio
	real(real64),allocatable :: center(:),dx(:)
	integer(int32) :: i

	if(ratio < 0.0d0)then
		print *, "[CAUTION] fatFEMDomain >> ratio should be >= 0"
	endif
	
	center = zeros(this%nd() )
	dx = zeros(this%nd() )
	do i=1,size(center)
		center(i) = average(this%mesh%nodcoord(:,i) )
	enddo

	do i=1,this%nn()
		dx = this%mesh%nodcoord(i,:) - center
		this%mesh%nodcoord(i,:) = center(:) + (1.0d0+ratio)*dx(:)
	enddo
end subroutine


!##################################################
subroutine MergeFEMDomain(inobj1,inobj2,outobj)
    class(FEMDomain_),intent(in) ::inobj1,inobj2
    class(FEMDomain_),intent(out)::outobj
    
	call MergeMesh(inobj1%Mesh,inobj2%Mesh,outobj%Mesh)
	call MergeMaterialProp(inobj1%MaterialProp,inobj2%MaterialProp,outobj%MaterialProp)
	call MergeDBound(inobj1%Boundary,inobj1%Mesh,inobj2%Boundary,inobj2%Mesh,outobj%Boundary)
	call MergeNBound(inobj1%Boundary,inobj1%Mesh,inobj2%Boundary,inobj2%Mesh,outobj%Boundary)
	
end subroutine MergeFEMDomain
!##################################################

subroutine DeduplicateFEMDomain(this,error,num_removed_node)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: error
	real(real64),allocatable :: nodcoord(:,:)
	integer(int32),optional,intent(inout) :: num_removed_node
	integer(int32) :: i,j,n
	logical,allocatable :: dup(:)
	integer(int32),allocatable :: num_dup
	integer(int32),allocatable :: old_id_to_new(:),same_as(:)
	! if dupulicate node exists, merge them
	allocate(dup(this%nn()) )
	allocate(same_as(this%nn()) )
	old_id_to_new = int(zeros(this%nn() ) )
	dup(:) = .false.
	num_dup = 0
	! O(n^2)
	do i=1,this%nn()-1
		if(dup(i)) cycle
		do j=i+1,this%nn()
			if(dup(j)) cycle
			if(norm(this%mesh%nodcoord(i,:)-this%mesh%nodcoord(j,:) ) < error )then
				dup(j) = .true.
				num_dup =num_dup + 1
				same_as(j)=i
			endif
		enddo
	enddo

	j = 0
	do i=1,this%nn()
		if(dup(i) )then
			old_id_to_new(i) = old_id_to_new(same_as(i))
		else
			j = j + 1
			old_id_to_new(i) = j
		endif
	enddo

	do i=1,this%ne()
		do j=1,this%nne()
			this%mesh%elemnod(i,j) = &
			old_id_to_new( this%mesh%elemnod(i,j) )
		enddo
	enddo



	nodcoord = zeros(this%nn()-num_dup,this%nd() )

	j = 0
	do i=1,this%nn()
		if( dup(i) )then
			cycle
		else
			j = j + 1
			nodcoord(j,:) = this%mesh%nodcoord(i,:)
		endif
	enddo
	if(present(num_removed_node) )then
		num_removed_node = num_dup
	endif
	this%mesh%nodcoord = nodcoord


end subroutine




!##################################################
subroutine ExportFEMDomain(this,OptionalFileFormat,OptionalProjectName,FileHandle,SolverType,MeshDimension,&
	FileName,Name,regacy,with,path,extention,step,FieldValue,restart)
    class(FEMDomain_),intent(inout)::this
    class(FEMDomain_),optional,intent(inout)::with
    character(*),optional,intent(in)::OptionalFileFormat,path,extention
    character(*),optional,intent(in)::OptionalProjectName,SolverType,FileName
	character*4::FileFormat
	character(*),optional,intent(in) :: Name
	logical,optional,intent(in) :: regacy,restart
    character(:),allocatable::ProjectName
	character(:),allocatable ::iFileName
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

		call execute_command_line("mkdir -p "//path)
		call execute_command_line("mkdir -p "//path//"/FEMDomain")
		call this%Mesh%export(path=path//"/FEMDomain",restart=.true.)
		call this%MaterialProp%export(path=path//"/FEMDomain",restart=.true.)
		call this%Boundary%export(path=path//"/FEMDomain",restart=.true.)
		call this%ControlPara%export(path=path//"/FEMDomain",restart=.true.)
		call this%ShapeFunction%export(path=path//"/FEMDomain",restart=.true.)

		call f%open(path//"/FEMDomain"//"/FEMDomain"//".prop" )
		write(f%fh,*) this%RealTime
		write(f%fh,*) this%NumOfDomain
		write(f%fh, '(A)' ) this%FilePath
		write(f%fh, '(A)' ) this%FileName
		write(f%fh, '(A)' ) this%name
		write(f%fh, '(A)' ) this%dtype
		write(f%fh, '(A)' ) this%SolverType
		write(f%fh, '(A)' ) this%Category1
		write(f%fh, '(A)' ) this%Category2
		write(f%fh, '(A)' ) this%Category3
		write(f%fh,*) this%timestep, this%NumberOfBoundaries, this%NumberOfMaterials
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

			open(100,file=name )
				print *, "Exporting .scf file >>> ",name
				if(present(with) )then
					print *, "Mode :: contact problem"
					write(100, '(A)' ) "2"
					write(100, '(A)' ) "  "
					n=size(this%Mesh%NodCoord,1)
					m=size(with%Mesh%NodCoord,1)
					write(100, '(A)' ) "1  "//fstring(n)
					write(100, '(A)' ) fstring(n+1)//"  "//fstring(n+m)
					write(100, '(A)' ) "  "
					n=size(this%Mesh%ElemNod,1)
					m=size(with%Mesh%ElemNod,1)
					write(100, '(A)' ) fstring(n)
					write(100, '(A)' ) fstring(n+m)
					write(100, '(A)' ) "  "
					n=size(this%Mesh%NodCoord,1)
					m=size(this%Mesh%NodCoord,2)
					write(100, * ) size(this%Mesh%NodCoord,1)+size(with%Mesh%NodCoord,1)
					write(100, '(A)' ) "  "
					do i=1,n
						write(100,*) this%Mesh%NodCoord(i,:)	
					enddo
					n=size(with%Mesh%NodCoord,1)
					m=size(with%Mesh%NodCoord,2)
					do i=1,n
						write(100,*) with%Mesh%NodCoord(i,:)
					enddo
					write(100, '(A)' ) "  "
					n=size(with%Mesh%ElemNod,1)+size(this%Mesh%ElemNod,1)
					m=size(this%Mesh%ElemNod,2)
					write(100, * ) fstring(n),"  ",fstring(m) 
					n=size(this%Mesh%ElemNod,1)
					m=size(this%Mesh%ElemNod,2)
					write(100, '(A)' ) "  "
					do i=1,n
						write(100,*) this%Mesh%ElemNod(i,:)
					enddo
					n=size(with%Mesh%ElemNod,1)
					m=size(with%Mesh%ElemNod,2)
					nn=size(this%Mesh%NodCoord,1)
					do i=1,n
						write(100,*) with%Mesh%ElemNod(i,:)+nn  
					enddo
					print *, "Elem-mat"
					write(100, '(A)' ) "  "
					n=size(this%Mesh%ElemNod,1)
					if(.not.allocated(this%Mesh%ElemMat) )then
						allocate(this%Mesh%ElemMat(n) )
						this%Mesh%ElemMat(:)=1
					endif
					write(100, '(A)' ) "  "

					do i=1,n
						write(100, *)  this%Mesh%ElemMat(i)  
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
					write(100,*) size(this%MaterialProp%MatPara,1)
					write(100, '(A)' ) "  "
					do i=1,size(this%MaterialProp%MatPara,1)
						write(100,*) this%MaterialProp%MatPara(i,:)
					enddo
					write(100, '(A)' ) "  "
					print *, "Dboundary will be put in here." 
					
					! count number of dirichlet condition for x
					n=0
					do i=1,size(this%Boundary%DBoundNodID,1)
						if(this%Boundary%DBoundNodID(i,1)>=1 )then
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
					do i=1,size(this%Boundary%DBoundNodID,1)
						if(this%Boundary%DBoundNodID(i,2)>=1 )then
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
					do i=1,size(this%Boundary%DBoundNodID,1)
						if(this%Boundary%DBoundNodID(i,1)>=1 )then
							write(100,*) this%Boundary%DBoundNodID(i,1)
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
					do i=1,size(this%Boundary%DBoundNodID,1)
						if(this%Boundary%DBoundNodID(i,1)>=1 )then
							write(100,*) this%Boundary%DBoundVal(i,1)
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
					do i=1,size(this%Boundary%DBoundNodID,1)
						if(this%Boundary%DBoundNodID(i,2)>=1 )then
							write(100,*) this%Boundary%DBoundNodID(i,2)
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
					do i=1,size(this%Boundary%DBoundNodID,1)
						if(this%Boundary%DBoundNodID(i,2)>=1 )then
							write(100,*) this%Boundary%DBoundVal(i,2)
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

					if(.not. allocated(this%Boundary%NBoundNodID)  )then
						write(100,*) 0
					else
						if(size(this%Boundary%NBoundNodID,1)==0 )then
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
					n=size(this%Mesh%SurfaceLine2D)+size(with%Mesh%SurfaceLine2D)
					write(100,*) n
					write(100, '(A)' ) "  "

					do i=1,size(this%Mesh%SurfaceLine2D)
						write(100,*) this%Mesh%SurfaceLine2D(i)
					enddo
					do i=1,size(with%Mesh%SurfaceLine2D)
						write(100,*) with%Mesh%SurfaceLine2D(i)+nn
					enddo
					write(100, '(A)' ) "  "
					write(100,*) 1, size(this%Mesh%SurfaceLine2D)
					write(100,*) size(this%Mesh%SurfaceLine2D)+1,size(this%Mesh%SurfaceLine2D)+size(with%Mesh%SurfaceLine2D)

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
				call ExportFEMDomainAsSTL(this=this,&
			FileHandle=FileHandle,MeshDimension=MeshDimension,FileName=name)
			else
				call ExportFEMDomainAsSTL(this=this,&
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
        FileFormat=OptionalFileFormat
    else
        FileFormat=".scf"
    endif

    if(present(OptionalProjectName) )then
        ProjectName=OptionalProjectName
    else
        ProjectName="untitled"
    endif
    iFileName = ProjectName//FileFormat

    !!print *, "Project : ",ProjectName
    !!print *, "is Exported as : ",FileFormat," format"
    !!print *, "File Name is : ",iFileName

	if(present(Name) )then
		open(fh,file=name//".scf",status="replace")
	else
		open(fh,file=iFileName,status="replace")
	endif

    if(FileFormat==".scf" )then
		
		if(allocated(this%Mesh%SubMeshNodFromTo) )then
			this%NumOfDomain=size(this%Mesh%SubMeshNodFromTo,1)
		else
			this%NumOfDomain=1
		endif

		this%Dtype="domain"
        write(fh,'(A)') this%Dtype
        write(*,'(A)') this%Dtype,iFileName
        write(fh,*) "  "
        write(fh,'(A)') this%SolverType
        write(fh,*) "  "
        write(fh,*) this%NumOfDomain
        write(fh,*) "  "


        print *, "########### Meta Info ###########"
        print *, this%Dtype
        print *, this%SolverType
        print *, this%NumOfDomain
        print *, "########### Meta Info ###########"

		if(.not. allocated(this%Mesh%SubMeshNodFromTo) )then
			print *, "this%Mesh%SubMeshNodFromTo is not allocated"
			stop 
		endif

        do i=1,this%NumOfDomain
            write(fh,*) this%Mesh%SubMeshNodFromTo(i,2),this%Mesh%SubMeshNodFromTo(i,3)
        enddo
        write(fh,*) "  "
        do i=1,this%NumOfDomain
            write(fh,*) this%Mesh%SubMeshElemFromTo(i,3)
        enddo
        write(fh,*) "  "


		
        print *, "########### Domain info ###########"
        do i=1,this%NumOfDomain
            !write(*,*) this%Mesh%SubMeshNodFromTo(i,2),this%Mesh%SubMeshNodFromTo(i,3)
        enddo
        do i=1,this%NumOfDomain
            !write(*,*) this%Mesh%SubMeshElemFromTo(i,3)
        enddo
        
        print *, "########### Domain info ###########"

        n=size(this%Mesh%NodCoord,1)
        m=size(this%Mesh%NodCoord,2)
        if(present(MeshDimension) )then
            m=MeshDimension
        endif
        write(fh,*) n,m
        DimNum=m

        write(fh,*) "  "
        do i=1,n
            write(fh,*) this%Mesh%NodCoord(i,1:m)
        enddo
        flush(fh)

        print *, " "
        print *, "########### Node info ###########"
        print *, "Number of node : ",n, "Dimension : ",m
        print *, "########### Node info ###########"
        print *, " "


        n=size(this%Mesh%ElemNod,1)
        m=size(this%Mesh%ElemNod,2)
        write(fh,*) n,m
		write(fh,*) "  "
		
        write(fh,'(A)') this%Mesh%getElemType()
        write(fh,*) "  "
        do i=1,n
            write(fh,*) this%Mesh%ElemNod(i,:)
            if(this%Mesh%ElemNod(i,1)==0 )then
                exit
            endif
        enddo
        write(fh,*) "  "
		flush(fh)


        print *, " "
        print *, "########### Element info ###########"
        print *, "Element Type : ",this%Mesh%getElemType()
        print *, "Number of Element : ",n, "Number of node per element : ",m
        print *, "Successfully Exported"
        print *, "########### Element info ###########"
        print *, " "

        n=size(this%Mesh%ElemNod,1)
        do i=1,n
            write(fh,*) this%Mesh%ElemMat(i)
        enddo
        write(fh,*) "  "
		
        n=size(this%MaterialProp%MatPara,1)
        m=size(this%MaterialProp%MatPara,2)
        write(fh,*) n,m
        do i=1,n
            write(fh,*) this%MaterialProp%MatPara(i,:)
        enddo
        write(fh,*) "  "
        flush(fh)



        print *, "########### Material info ###########"
        n=size(this%Mesh%ElemNod,1)
        !write(*,*) size(this%Mesh%ElemMat,1)
        n=size(this%MaterialProp%MatPara,1)
        m=size(this%MaterialProp%MatPara,2)
        !write(*,*) n,m
        do i=1,n
            write(*,*) this%MaterialProp%MatPara(i,:)
        enddo
        print *, "Successfully Exported"
        print *, "########### Material info ###########"
		
        !DirichletBoundary

        if(.not.allocated(this%Boundary%DBoundNodID))then
            
            write(fh,*) "0" !DirichletBoundaryDimension
            write(fh,*) "  "
			print *, "ImportFEMDomain >> Caution :: no Dirichlet Boundary Condition is loaded. "
			stop 
        else
			! update this%Boundary%DBoundNum
			if(allocated(this%Boundary%DBoundNum) )then
				deallocate(this%Boundary%DBoundNum)
			endif
			n=size(this%Boundary%DBoundNodID,2)
			allocate(this%Boundary%DBoundNum(n) )
			m=size(this%Boundary%DBoundNodID,1)
			do i=1,n
				this%Boundary%DBoundNum(i)=m-countif(Array=this%Boundary%DBoundNodID(:,i),Equal=.true.,Value=-1 )
			enddo


            n=size(this%Boundary%DBoundNum)
            write(fh,*) n !DirichletBoundaryDimension
            write(fh,*) "  "

            !allocate(this%Boundary%DBoundNum(n ))
            write(fh,*) this%Boundary%DBoundNum(:)
            write(fh,*) "  "
            !allocate(this%Boundary%DBoundNodID( maxval(this%Boundary%DBoundNum), size(this%Boundary%DBoundNum)  )  )
            !allocate(this%Boundary%DBoundVal( maxval(this%Boundary%DBoundNum), size(this%Boundary%DBoundNum)  )  )

            !this%Boundary%DBoundNodID(:,:)=-1
            !this%Boundary%DBoundVal(:,:)  =0.0d0

            do i=1,size(this%Boundary%DBoundNum,1)
                do j=1,this%Boundary%DBoundNum(i)
					write(fh,*) this%Boundary%DBoundNodID(j,i)
                    !!print *,this%Boundary%DBoundNodID(j,i)
                enddo
                write(fh,*) "  "
                do j=1,this%Boundary%DBoundNum(i)
                    write(fh,*) this%Boundary%DBoundVal(j,i)
                    !!print *,this%Boundary%DBoundVal(j,i)
                enddo
                write(fh,*) "  "
            enddo

        endif




        print *, "########### Dirichlet Boundary info ###########"
        if(.not.allocated(this%Boundary%DBoundNum))then
            
            write(*,*) "0" !DirichletBoundaryDimension
            write(*,*) "  "
            stop "ERROR :: FEMDomainClass :: no Dirichlet boundary is found"
            !print *, "ImportFEMDomain >> Caution :: no Dirichlet Boundary Condition is loaded. "
        else

            n=size(this%Boundary%DBoundNum)
            !write(*,*) n !DirichletBoundaryDimension
            !write(*,*) "  "

            !allocate(this%Boundary%DBoundNum(n ))
            !write(*,*) this%Boundary%DBoundNum(:)
            !write(*,*) "  "
            !allocate(this%Boundary%DBoundNodID( maxval(this%Boundary%DBoundNum), size(this%Boundary%DBoundNum)  )  )
            !allocate(this%Boundary%DBoundVal( maxval(this%Boundary%DBoundNum), size(this%Boundary%DBoundNum)  )  )

            !this%Boundary%DBoundNodID(:,:)=-1
            !this%Boundary%DBoundVal(:,:)  =0.0d0

            do i=1,size(this%Boundary%DBoundNum,1)
                do j=1,this%Boundary%DBoundNum(i)
                    !write(*,*) this%Boundary%DBoundNodID(j,i)
                    !!print *,this%Boundary%DBoundNodID(j,i)
                enddo
                !write(*,*) "  "
                do j=1,this%Boundary%DBoundNum(i)
                    !write(*,*) this%Boundary%DBoundVal(j,i)
                    !!print *,this%Boundary%DBoundVal(j,i)
                enddo
                !write(*,*) "  "
            enddo

        endif

        print *, "Successfully Exported"
        print *, "########### Dirichlet Boundary info ###########"
        
        



        if(.not.allocated(this%Boundary%NBoundNum) )then
            DimNum=0
        else
            DimNum=size(this%Boundary%NBoundNum,1)
        endif


        write(fh,*) DimNum
        write(fh,*) "  "
        if(DimNum<=0)then
            !print *, "ImportFEMDomain >> Caution :: no Neumann Boundary Condition is loaded. "
        else
            n=size(this%Boundary%NBoundNodID,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate( this%Boundary%NBoundNum(DimNum))
            !allocate(this%Boundary%NBoundNodID(n, size(this%Boundary%NBoundNum)  )  )
            !allocate(this%Boundary%NBoundVal( n, size(this%Boundary%NBoundNum)  )  )
            !this%Boundary%NBoundNodID(:,:)=-1
            !this%Boundary%NBoundVal(:,:)  =0.0d0

            !this%Boundary%NBoundNum(:)=n
            do i=1,n
                write(fh,*) this%Boundary%NBoundNodID(i,:)
                !this%Boundary%NBoundNodID(i,:)=m
            enddo
            write(fh,*) "  "

            do i=1,n
                write(fh,*) this%Boundary%NBoundVal(i,:)
            enddo
            write(fh,*) "  "
        
        endif
            


        print *, "########### Neumann Boundary info ###########"

        if(.not.allocated(this%Boundary%NBoundNum) )then
            DimNum=0
        else
            DimNum=size(this%Boundary%NBoundNum,1)
        endif
        !write(*,*) DimNum
        !write(*,*) "  "
        if(DimNum<=0)then
            !print *, "ImportFEMDomain >> Caution :: no Neumann Boundary Condition is loaded. "
        else
            n=size(this%Boundary%NBoundNodID,1)
            !write(*,*) n
            !write(*,*) "  "
            !allocate( this%Boundary%NBoundNum(DimNum))
            !allocate(this%Boundary%NBoundNodID(n, size(this%Boundary%NBoundNum)  )  )
            !allocate(this%Boundary%NBoundVal( n, size(this%Boundary%NBoundNum)  )  )
            !this%Boundary%NBoundNodID(:,:)=-1
            !this%Boundary%NBoundVal(:,:)  =0.0d0

            !this%Boundary%NBoundNum(:)=n
            do i=1,n
                !write(*,*) this%Boundary%NBoundNodID(i,:)
                !this%Boundary%NBoundNodID(i,:)=m
            enddo
            !write(*,*) "  "

            do i=1,n
                !write(*,*) this%Boundary%NBoundVal(i,:)
            enddo
            !write(*,*) "  "
        
        endif
        print *, "Successfully Exported"
        print *, "########### Neumann Boundary info ###########"
        
        



        print *, "########### Initial Condition info ###########"
        
        !######### Initial conditions #################
        ! For node-wize
        
        if(.not.allocated(this%Boundary%TBoundVal) )then
            DimNum=0
        else
            DimNum=size(this%Boundary%TBoundVal,2)
        endif
        write(fh,*) DimNum
        write(fh,*) "  "
        


        if(DimNum<=0)then
            !print *, "Caution :: no Initial Condition (Node-wise) Condition is loaded. "
        else
            n=size(this%Boundary%TBoundVal,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate(this%Boundary%TBoundNodID(n,DimNum) )
            !allocate(this%Boundary%TBoundVal(  n,DimNum) )
            !allocate(this%Boundary%TBoundNum(  DimNum) )

            !this%Boundary%TBoundNum(:)=n
            
            if(n/=0)then
                if(n<0)then
                    print *, "ERROR :: number of initial conditions are to be zero"
                else
                    do i=1,n
                        write(fh,*) this%Boundary%TBoundNodID(i,:)
                    enddo
                    write(fh,*) "  "
                    do i=1,n
                        write(fh,*) this%Boundary%TBoundVal(i,:)
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
        if(.not.allocated(this%Boundary%TBoundElemGpVal) )then
            DimNum=0
        else
            DimNum=size(this%Boundary%TBoundElemGpVal,3)
        endif
        
        write(fh,*) DimNum 
        write(fh,*) "  "
        if(DimNum<=0)then
            !print *, "Caution :: no Initial Condition (Gp) is loaded. "
        else
            !write(fh,*) 
            GpNum=size(this%Boundary%TBoundElemGpVal,2)
            write(fh,*) GpNum
            write(fh,*) "  "
            !write(fh,*) 
            n=size(this%Boundary%TBoundElemGpVal,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate(this%Boundary%TBoundElemID(n) )
            !allocate(this%Boundary%TBoundElemGpVal(n,GpNum,DimNum) )
            
            if(n/=0)then
                if(n<0)then
                    print *, "ERROR :: number of initial conditions are to be zero"
                else
                    do i=1,n
                        write(fh,*) this%Boundary%TBoundElemID(i)
                    enddo
                    write(fh,*) "  "
                    do i=1,n
                        do j=1,GpNum
                            do k=1,DimNum
                                write(fh,*) this%Boundary%TBoundElemGpVal(i,j,k)
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
        
        write(fh,*) this%ControlPara%SimMode ,this%ControlPara%ItrTol,this%ControlPara%Timestep
        flush(fh)
        close(fh)
    else
        print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
    endif





end subroutine ExportFEMDomain
!##################################################

!##################################################
subroutine InitDBC(this,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::this
    integer(int32),intent(in) :: NumOfValPerNod
    
    integer(int32) :: n,m
    !if the facet is not created, create facets (surface elements)
    call GetSurface(this%Mesh)        
    n=size(this%Mesh%FacetElemNod,1)
    m=size(this%Mesh%FacetElemNod,2)

    if(allocated(this%Boundary%DBoundNum))then
        deallocate(this%Boundary%DBoundNum)
    endif

    if(allocated(this%Boundary%DBoundNodID))then
        deallocate(this%Boundary%DBoundNodID)
    endif

    if(allocated(this%Boundary%DBoundVal) )then
        deallocate(this%Boundary%DBoundVal)
    endif

    allocate(this%Boundary%DBoundNum(NumOfValPerNod) )
    this%Boundary%DBoundNum(:)=0
    allocate(this%Boundary%DBoundNodID(n*m,NumOfValPerNod) )
    this%Boundary%DBoundNodID(:,:)=-1
    allocate(this%Boundary%DBoundVal(n*m,NumOfValPerNod) )
    this%Boundary%DBoundVal(:,:)=0.0d0
            
end subroutine
!##################################################


!##################################################
subroutine AddDBoundCondition(this,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::this
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

	n=size(this%Mesh%NodCoord,2)
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
    
    
    if( .not. allocated(this%Mesh%FacetElemNod))then
		call this%InitDBC(NumOfValPerNod)
		print *, "add dbc :: initialized"
	endif
	
	if(.not.allocated(this%Boundary%DBoundNodID) )then
		call this%InitDBC(NumOfValPerNod)
	endif
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
	rmax(3)=z_max
	
    n=size(this%Mesh%FacetElemNod,1)
	m=size(this%Mesh%FacetElemNod,2)
	count_n=0

	if(.not. allocated(this%Boundary%DBoundNum) )then
		i=size(this%Boundary%DBoundNodID,2)
		allocate(this%Boundary%DBoundNum(i))
	endif

    do i=1,size(this%Mesh%FacetElemNod,1)
		do j=1,size(this%Mesh%FacetElemNod,2)
			if(this%Mesh%FacetElemNod(i,j) > size(this%Mesh%NodCoord,1) )then
				print *, "ERROR :: this%Mesh%FacetElemNod is out of range"
				print *, "Number of nodes: ",size(this%Mesh%NodCoord,1),&
					"this%Mesh%FacetElemNod(i,j) is ",this%Mesh%FacetElemNod(i,j)
				stop 
			endif
			x(:)=0.0d0
            x(1:dim_num)=this%Mesh%NodCoord( this%Mesh%FacetElemNod(i,j),1:dim_num )    
			InOut = InOrOut(x,rmax,rmin)
            if(InOut .eqv. .true.)then
                if( (i-1)*m+j > n*m )then
                    stop "sgdssdfssssssssssssss"
				endif
				count_n=count_n+1
				if(size(this%Boundary%DBoundNodID,1) < (i-1)*m+j  )then
					print *, "ERROR :: this%Boundary%DBoundNodID is out of range"
					print *, size(this%Boundary%DBoundNodID,1),size(this%Boundary%DBoundNodID,2),size(this%Mesh%NodCoord,1),&
						ValID,this%Mesh%FacetElemNod(i,j)
					stop 
				endif
                this%Boundary%DBoundNum(ValID)=this%Boundary%DBoundNum(ValID)+1
                this%Boundary%DBoundNodID( (i-1)*m+j ,ValID)=this%Mesh%FacetElemNod(i,j)
                this%Boundary%DBoundVal( (i-1)*m+j ,ValID)=val
            endif
            
        enddo
            
    enddo
		
	print *, "Total ",count_n,"boundary conditions are set"

end subroutine AddDBoundCondition
!##################################################



!##################################################
subroutine InitNBC(this,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::this
    integer(int32),intent(in) :: NumOfValPerNod
    
    integer(int32) :: n,m
    !if the facet is not created, create facets (surface elements)
    if( .not. allocated(this%Mesh%FacetElemNod) )then
        call GetSurface(this%Mesh)        
	endif
	
    n=size(this%Mesh%FacetElemNod,1)
    m=size(this%Mesh%FacetElemNod,2)


    if(allocated(this%Boundary%NBoundNum))then
        deallocate(this%Boundary%NBoundNum)
    endif

    if(allocated(this%Boundary%NBoundNodID))then
        deallocate(this%Boundary%NBoundNodID)
    endif

    if(allocated(this%Boundary%NBoundVal) )then
        deallocate(this%Boundary%NBoundVal)
    endif

    allocate(this%Boundary%NBoundNum(NumOfValPerNod) )
    this%Boundary%NBoundNum(:)=0
    allocate(this%Boundary%NBoundNodID(n*m,NumOfValPerNod) )
    this%Boundary%NBoundNodID(:,:)=-1
    allocate(this%Boundary%NBoundVal(n*m,NumOfValPerNod) )
    this%Boundary%NBoundVal(:,:)=0.0d0
    
    return
        
end subroutine
!##################################################





!##################################################
subroutine AddNBoundCondition(this,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::this
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



    n=size(this%Mesh%NodCoord,2)
    
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
    
    
    if( .not. allocated(this%Mesh%FacetElemNod))then
        call this%InitNBC(NumOfValPerNod)
        
    endif
    
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(this%Mesh%FacetElemNod,1)
    m=size(this%Mesh%FacetElemNod,2)
        
    do i=1,size(this%Mesh%FacetElemNod,1)
        do j=1,size(this%Mesh%FacetElemNod,2)
            x(:)=this%Mesh%NodCoord( this%Mesh%FacetElemNod(i,j),: )
            InOut = InOrOut(x,rmax,rmin)
            if(InOut .eqv. .true.)then
                if( (i-1)*m+j > n*m )then
                    stop "sgdssdfssssssssssssss"
                endif
                this%Boundary%NBoundNum(ValID)=this%Boundary%NBoundNum(ValID)+1
				this%Boundary%NBoundNodID( (i-1)*m+j ,ValID)=this%Mesh%FacetElemNod(i,j)
				nodenum=size(this%Mesh%ElemNod,2)
				if(nodenum==3)then
					call tobj%init(dim=3)
					tobj%NodCoord(1,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),1:3)
					tobj%NodCoord(2,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),1:3)
					tobj%NodCoord(3,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),1:3)
					area=tobj%getArea()
				elseif(nodenum>=4)then
					nodenum=4
					call tobj%init(dim=3)
					tobj%NodCoord(1,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),1:3)
					tobj%NodCoord(2,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,2),1:3)
					tobj%NodCoord(3,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),1:3)
					area=tobj%getArea()
					call tobj%init(dim=3)
					tobj%NodCoord(1,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,2),1:3)
					tobj%NodCoord(2,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),1:3)
					tobj%NodCoord(3,1:3)=&
					this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,4),1:3)
					area=area+tobj%getArea()
				else
					print *, "ERROR :: Node num = ",nodenum,"is not implemented."
					stop 
				endif
				if(area==0.0d0 .or. area/=area)then
					print *, "area==0.0d0 .or. area/=area"
					stop
				endif
                this%Boundary%NBoundVal( (i-1)*m+j ,ValID)=val*area/dble(nodenum)
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
!    allocate(NBoundNodINBuf(size(this%Mesh%SurfaceLine2D),size(this%Boundary%NBoundNodID,2) ) )
!    allocate(NBoundValBuf  (size(this%Mesh%SurfaceLine2D),size(this%Boundary%NBoundNodID,2) ) )
!    
!    NBoundNodINBuf(:,:) = -1
!    NBoundValBuf(:,:)   = -1.0d0
!
!
!    k=0
!    do i=1,size(this%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=this%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(this%Mesh%NodCoord,2)
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
!            if(minline <= this%Mesh%NodCoord(node_id,j) .and. this%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(this%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(this%Mesh%NodCoord,2)
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
!    call CopyArray(this%Boundary%DBoundNodID,CopiedArrayInt)
!    call CopyArray(this%Boundary%DBoundVal,CopiedArrayReal)
!    call MergeArray(CopiedArrayInt,DBoundNodIDBuf,this%Boundary%DBoundNodID)
!    call MergeArray(CopiedArrayReal,DBoundValBuf,this%Boundary%DBoundVal)
!
!    call DeleteOverlapBoundary(this%Boundary)

end subroutine AddNBoundCondition
!##################################################

!##################################################
subroutine InitTBC(this,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::this
    integer(int32),intent(in) :: NumOfValPerNod
    
    integer(int32) :: n,m
    !if the facet is not created, create facets (surface elements)
    if( .not. allocated(this%Mesh%FacetElemNod) )then
        call GetSurface(this%Mesh)        
    endif
    n=size(this%Mesh%NodCoord,1)
    m=size(this%Mesh%NodCoord,2)

    if(allocated(this%Boundary%TBoundNum))then
        deallocate(this%Boundary%TBoundNum)
    endif

    if(allocated(this%Boundary%TBoundNodID))then
        deallocate(this%Boundary%TBoundNodID)
    endif

    if(allocated(this%Boundary%TBoundVal) )then
        deallocate(this%Boundary%TBoundVal)
    endif

    allocate(this%Boundary%TBoundNum(NumOfValPerNod) )
    this%Boundary%TBoundNum(:)=0
    allocate(this%Boundary%TBoundNodID(n,NumOfValPerNod) )
    this%Boundary%TBoundNodID(:,:)=-1
    allocate(this%Boundary%TBoundVal(n,NumOfValPerNod) )
    this%Boundary%TBoundVal(:,:)=0.0d0
    
    return
        
end subroutine
!##################################################





!##################################################
subroutine AddTBoundCondition(this,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::this
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

    n=size(this%Mesh%NodCoord,2)
    
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
    
    
    if( size(this%Mesh%NodCoord,1)/=size(this%Boundary%TBoundNodID,1)  )then
        call this%InitTBC(NumOfValPerNod)
        print *, "sifdh"
    endif
    
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(this%Mesh%NodCoord,1)

	count_n=0
    do i=1,n
            x(:)=this%Mesh%NodCoord( i,: )
            InOut = InOrOut(x,rmax,rmin)
			if(InOut .eqv. .true.)then
				count_n=count_n+1
                this%Boundary%TBoundNum(ValID)=this%Boundary%TBoundNum(ValID)+1
                this%Boundary%TBoundNodID( i ,ValID)=i
                this%Boundary%TBoundVal( i,ValID)=val
            endif
        
	enddo
	print *, "Initial value is in : ",count_n,"value is : ",val
    return

end subroutine AddTBoundCondition
!##################################################



!!##################################################
!subroutine AddNBoundCondition(this,xmin,xmax,ymin,ymax,zmin,zmax,&
!    tmin,tmax,valx,valy,valz)
!    class(FEMDomain_),intent(inout)::this
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
!    allocate(NBoundNodIDBuf(size(this%Mesh%SurfaceLine2D),size(this%Boundary%NBoundNodID,2) ) )
!    allocate(NBoundValBuf  (size(this%Mesh%SurfaceLine2D),size(this%Boundary%NBoundNodID,2) ) )
!    NBoundNodIDBuf(:,:) = -1
!    NBoundValBuf(:,:)   = -1.0d0
!
!
!
!    k=0
!    do i=1,size(this%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=this%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(this%Mesh%NodCoord,2)
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
!            if(minline <= this%Mesh%NodCoord(node_id,j) .and. this%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(this%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(this%Mesh%NodCoord,2)
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
!    call CopyArray(this%Boundary%NBoundNodID,CopiedArrayInt)
!    call CopyArray(this%Boundary%NBoundVal,CopiedArrayReal)
!!    call MergeArray(CopiedArrayInt,NBoundNodIDBuf,this%Boundary%NBoundNodID)
!!    call MergeArray(CopiedArrayReal,NBoundValBuf,this%Boundary%NBoundVal)
!!    call DeleteOverlapBoundary(this%Boundary)
!!    call InitializeBoundary(this%Boundary)
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
!subroutine AddTBoundCondition(this,xmin,xmax,ymin,ymax,zmin,zmax,&
!    tmin,tmax,valx,valy,valz)
!    class(FEMDomain_),intent(inout)::this
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
!    allocate(TBoundNodIDBuf(size(this%Mesh%SurfaceLine2D),size(this%Boundary%TBoundNodID,2) ) )
!    allocate(TBoundValBuf  (size(this%Mesh%SurfaceLine2D),size(this%Boundary%TBoundNodID,2) ) )
!    TBoundNodIDBuf(:,:) = -1
!    TBoundValBuf(:,:)   = -1.0d0
!
!
!
!    k=0
!    do i=1,size(this%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=this%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(this%Mesh%NodCoord,2)
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
!            if(minline <= this%Mesh%NodCoord(node_id,j) .and. this%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(this%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(this%Mesh%NodCoord,2)
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
!    call CopyArray(this%Boundary%TBoundNodID,CopiedArrayInt)
!    call CopyArray(this%Boundary%TBoundVal,CopiedArrayReal)
!    call MergeArray(CopiedArrayInt,TBoundNodIDBuf,this%Boundary%TBoundNodID)
!    call MergeArray(CopiedArrayReal,TBoundValBuf,this%Boundary%TBoundVal)
!    call DeleteOverlapBoundary(this%Boundary)
!    call InitializeBoundary(this%Boundary)
!    
!
!
!end subroutine 
!!##################################################




!##################################################
subroutine SetSolver(this,inSolverType)
    class(FEMDomain_),intent(inout)::this
    character(*),intent(in) :: inSolverType

    this%SolverType=inSolverType

end subroutine
!##################################################

!##################################################
subroutine SetName(this,Name)
    class(FEMDomain_),intent(inout)::this
    character(*),intent(in) :: Name

    this%FileName=Name

end subroutine
!##################################################


!##################################################
subroutine SetDataType(this,inDType)
    class(FEMDomain_),intent(inout)::this
    character(*),intent(in) :: inDType

    this%DType = inDType

end subroutine

!##################################################


!##################################################
subroutine SetUpFEMDomain(this)
    class(FEMDomain_),intent(inout)::this

    logical :: NodeExist
    logical :: ElementExist

    if(allocated(this%Mesh%NodCoord)  )then
        NodeExist = .true.
    else
        NodeExist = .false.
    endif

    if(allocated(this%Mesh%ElemNod)  )then
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
subroutine SetControlParaFEMDomain(this,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
    class(FEMDomain_),intent(inout)::this
    real(real64),optional,intent(in)::OptionalTol
    integer(int32),optional,intent(in)::OptionalSimMode,OptionalItrTol,OptionalTimestep
    
    call SetControlPara(this%ControlPara,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
end subroutine
!##################################################


!##################################################
subroutine AddMaterialID(this,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,MaterialID ,mode2D)
    class(FEMDomain_),intent(inout)::this
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


    n=size(this%Mesh%NodCoord,2)
    
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
    n=size(this%Mesh%ElemMat,1)

    do i=1,n
        x(:)=0.0d0
        do j=1,size(this%Mesh%ElemNod,2)
            x(:)=x(:)+this%Mesh%NodCoord( this%Mesh%ElemNod(i,j),: )
        enddo

        x(:)=1.0d0/dble(size(this%Mesh%ElemNod,2))*x(:)
        
        InOut = InOrOut(x,rmax,rmin)
        if(InOut .eqv. .true.)then
            
            this%Mesh%ElemMat(i)=md
        endif
        
    enddo


end subroutine
!##################################################


!##################################################
subroutine MeltingSkeltonFEMDomain(this)
    class(FEMDomain_),intent(inout)::this

    call this%Mesh%MeltingSkelton()
    
end subroutine
!##################################################


!##################################################
recursive subroutine mshFEMDomain(this,name,scalar,vector,tensor,step,fieldname,NodeList)
	! export as msh format
	class(FEMDomain_),intent(in)::this
	type(FEMDomain_)::mini_obj
	character(*),intent(in) :: name
	character(*),optional,intent(in) :: fieldname
	real(real64),optional,intent(in):: vector(:,:),scalar(:,:),tensor(:,:,:)
	real(real64),allocatable :: eigenvector(:,:),eigens(:),tens(:,:),vec1(:,:),vec2(:,:),scalar_(:,:)
	real(real64),allocatable :: vector_(:,:)
	integer(int32),optional,intent(in) :: step,NodeList(:)
	character(:),allocatable :: fname
	type(IO_) :: f
	integer(int32) :: i,j,typeid,n

	if(present(NodeList))then
		n = size(NodeList,1)
		mini_obj%mesh%nodcoord = zeros(n,this%nd())
		mini_obj%mesh%elemNod = zeros(n,this%nne())
		do i=1,n
			mini_obj%mesh%nodcoord(i,: ) = this%mesh%nodcoord( NodeList(i),: ) 
		enddo
		do i=1,n
			mini_obj%mesh%elemNod(i,:) = i
		enddo
		call mini_obj%msh(name=name)
		return
	endif


	if(present(tensor) )then
		
		if(size(tensor,2)==2)then

			allocate(tens(size(tensor,2),size(tensor,3)) )
			allocate(vec1(size(tensor,1),size(tensor,2)),vec2(size(tensor,1),size(tensor,2)))
			do i=1,size(tensor,1)
				tens(:,:) = tensor(i,:,:)
				call eigen_2d(tens, eigenvector)
				vec1(i,:) = eigenvector(1,:)
				vec2(i,:) = eigenvector(2,:)
			enddo
			call this%msh(vector=vec1,name="first_eigen_plus"//name)
			call this%msh(vector=vec2,name="second_eigen_plus"//name)
			do i=1,size(vec1,1)
				vec1(i,:) =  - vec1(i,:) 
				vec2(i,:) =  - vec2(i,:)
			enddo 
			call this%msh(vector=vec1,name="first_eigen_minus"//name)
			call this%msh(vector=vec2,name="second_eigen_minus"//name)
			return
		else
			! only rank-2 tensor is now implemented.
			! for arbitrary rank-size, please implement them in src/MathClass
			return
		endif
	endif

	if(present(Vector) )then
		n = input(default=1, option=step)
		if(present(fieldname) )then
			fname = fieldname
		else
			fname = "Vector Field"
		endif
		vector_ = array(size(vector,1),3 )
		vector_(:,1:size(vector,2) ) = vector(:,1:size(vector,2))
		call this%GmshPlotVector(Vector=vector_,name=name,FieldName=fname,step=n)
		return
	endif


	if(present(Scalar) )then
		n = input(default=1, option=step)
		if(present(fieldname) )then
			fname = fieldname
		else
			fname = "Scalar Field"
		endif
		call this%GmshPlotContour(gp_value=scalar,OptionalContorName=fname,OptionalStep=n,Name=name)
		return
	endif

	if(present(fieldname) )then
		! fieldname がどこかのレイヤーの名前と一致した場合
		do i=1,size(this%PhysicalField)
			if(this%PhysicalField(i)%name==fieldname )then
				
				if(allocated(this%PhysicalField(i)%scalar))then
					scalar_ = array(size(this%PhysicalField(i)%scalar) ,1)
					do j=1,size(scalar_)
						scalar_(j,:) = this%PhysicalField(i)%scalar(j)
					enddo
					call this%msh(name=name,scalar=scalar_,step=step,fieldname=fieldname)
					return
				endif
				if(allocated(this%PhysicalField(i)%vector))then
					call this%msh(name=name,vector=this%PhysicalField(i)%vector,step=step,fieldname=fieldname)
					return
				endif
				if(allocated(this%PhysicalField(i)%tensor))then
					call this%msh(name=name,tensor=this%PhysicalField(i)%tensor,step=step,fieldname=fieldname)
					return
				endif
			endif
		enddo
	endif

	call f%open(name//".msh",'w')
	write(f%fh, '(a)') "$MeshFormat"
	! version of gmsh, 0=ASCII, 8=real(8)
	write(f%fh, '(a)' ) "2.2 0 8"
	write(f%fh, '(a)' ) "$EndMeshFormat"
	
	write(f%fh, '(a)' ) "$Nodes"
	write(f%fh, '(a)' ) str(size(this%mesh%nodcoord,1) )
	do i=1,size(this%mesh%nodcoord,1)
		write(f%fh,'(a)',advance="no") str(i)//" "
		do j=1,size(this%mesh%nodcoord,2)-1
			write(f%fh,'(a)',advance="no") str(this%mesh%nodcoord(i,j))//" "
		enddo
		j=size(this%mesh%nodcoord,2)
		if(3-j == 0)then
			write(f%fh,'(a)',advance="yes") str(this%mesh%nodcoord(i,j))
		elseif(3-j==1)then
			write(f%fh,'(a)',advance="no") str(this%mesh%nodcoord(i,j))//" "
			write(f%fh,'(a)',advance="yes") "0.00000  "
		elseif(3-j==2)then
			write(f%fh,'(a)',advance="no") str(this%mesh%nodcoord(i,j))//" "
			write(f%fh,'(a)',advance="no") "0.00000  "
			write(f%fh,'(a)',advance="yes") "0.00000  "
		else
			print *, "ERROR :: mshFEMDomain >> invalid node dimension"
			stop 			
		endif
	enddo
	write(f%fh,'(a)' ) "$EndNodes"

	write(f%fh, '(a)' ) "$Elements"
	write(f%fh, '(a)' ) str(size(this%mesh%elemnod,1) )
	! id, type, tag
	! 1 : 2-node line
	! 2 : 3-node line
	! 3 : 4-node quadrangle
	! 4 : 4-node tetrahedron
	! 5 : 8-node hexahedron
	! ...etc.
	if(size(this%mesh%elemnod,2) == 8 .and. size(this%mesh%nodcoord,2)==3 ) then
		typeid=5
	elseif(size(this%mesh%elemnod,2) == 4 .and. size(this%mesh%nodcoord,2)==3 )then
		typeid=4
	elseif(size(this%mesh%elemnod,2) == 4 .and. size(this%mesh%nodcoord,2)==2 )then
		typeid=3
	elseif(size(this%mesh%elemnod,2) == 3 .and. size(this%mesh%nodcoord,2)==1 )then
		typeid=2
	elseif(size(this%mesh%elemnod,2) == 2 .and. size(this%mesh%nodcoord,2)==1 )then
		typeid=1
	else
		print *, "mshFEMDomain >> meshtype is not supported. (only 1-5 for elm-type)"
		stop 
	endif

	do i=1,size(this%mesh%elemnod,1)
		write(f%fh,'(a)',advance="no") str(i)//" "//str(typeid)//" 0 "
		do j=1,size(this%mesh%elemnod,2)-1
			write(f%fh,'(a)',advance="no") str(this%mesh%elemnod(i,j))//" "
		enddo
		j=size(this%mesh%elemnod,2)
		write(f%fh,'(a)',advance="yes") str(this%mesh%elemnod(i,j))
	enddo
	write(f%fh, '(a)' ) "$EndElements"
	call f%close()


end subroutine
!##################################################


! #########################################################################################
subroutine GmshPlotMesh(this,OptionalContorName,OptionalAbb,OptionalStep,Name,withNeumannBC,withDirichletBC&
	,onlyNeumannBC,onlyDirichletBC,asMsh,withMaterial,Tag,timestep,field)
	class(FEMDomain_),intent(inout)::this
	real(real64),allocatable::gp_value(:,:)
	real(real64),allocatable,optional,intent(in)::field(:)
	integer(int32),optional,intent(in)::OptionalStep,timestep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	character(*),optional,intent(in)::Name,Tag
	logical,optional,intent(in)::withNeumannBC,withDirichletBC,onlyNeumannBC,onlyDirichletBC,asMsh,withMaterial
	real(real64),allocatable::x_double(:,:)
	real(real64),allocatable::x(:,:)
	integer(int32) i,j,k,l,step,fh,nodeid1,nodeid2
	character filename0*11,filename0msh*11
	character(:),allocatable ::  filename
	character filetitle*6
	character(:),allocatable ::  command
	character:: mapname*30,abbmap*6
	


	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	elseif(present(Tag) )then
		mapname=Tag
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
    
    if(.not.allocated(this%Mesh%ElemMat) )then
        allocate(this%Mesh%ElemMat(size(this%Mesh%ElemNod,1) ) )
        this%Mesh%ElemMat(:)=1
    endif

	!---------------------
	write (filename0, '("_", i6.6, ".pos")') step ! ここでファイル名を生成している
	if(present(Name) )then
		filename=filename0
		
		!call execute_command_line(  "touch "//name//this%FileName//filename )
		open(fh,file=name//filetitle//filename )
		print *, "writing ",name//filetitle//filename," step>>",step
	else
		filename=filename0
		!call execute_command_line(  "touch "//this%FileName//filename )
		!print *, this%FileName//filetitle//filename
		open(fh,file=this%FileName//filetitle//filename )
		print *, "writing ",this%FileName//filetitle//filename," step>>",step
	endif
	
	
	!---------------------
	if( size(this%Mesh%ElemNod,2)==4 .and. size(this%Mesh%NodCoord,2)==2 ) then
		allocate(x(4,3) )
		allocate(x_double(4,3) )
		x(:,:)=0.0d0
		x_double(:,:)=0.0d0
	elseif( size(this%Mesh%ElemNod,2)==8 .and. size(this%Mesh%NodCoord,2)==3 ) then
		allocate(x(8,3) )
		allocate(x_double(8,3) )
		x(:,:)=0.0d0
		x_double(:,:)=0.0d0
		
	endif

	allocate(gp_value( size(this%Mesh%ElemNod,1),size(this%Mesh%ElemNod,2) ))
	if(allocated(this%Mesh%ElemMat) )then
		do i=1,size(this%Mesh%ElemMat,1)
			gp_value(i,:)=dble(this%Mesh%ElemMat(i))
		enddo
	else
		gp_value(i,:)=0.0d0
	endif
	if(present(Field) )then
		do i=1,size(gp_value,1)
			gp_value(i,:)=field(i)
		enddo
	endif

	if(present(withDirichletBC) )then
		if(withDirichletBC .eqv. .true. )then
			! search Dirichlet BC and change color
			if(.not. allocated(this%Boundary%DBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> withDirichletBC >> no NBC is found."
				return
			else
				if(this%debug_mode)then
					print *, "[ok] GmshPlotMesh",filename," is exported withDirichletBC. The value is:",maxval(this%Mesh%ElemMat(:))+40
				endif
			endif
			do i=1,size(this%Boundary%DBoundNodID,1 )
				do j=1,size(this%Boundary%DBoundNodID,2)
					
					if(this%Boundary%DBoundNodID(i,j)>0 )then
						nodeid1=this%Boundary%DBoundNodID(i,j)
					else
						cycle
					endif

					do k=1,size(this%Mesh%ElemNod,1)
						do l=1,size(this%Mesh%ElemNod,2)
							nodeid2=this%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								gp_value(k,:)=dble(maxval(this%Mesh%ElemMat(:)))+40.0d0 ! Dirichlet is +20
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
			if(.not. allocated(this%Boundary%NBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> withNeumannBC >> no NBC is found."
				return
			else
				if(this%debug_mode)then
					print *, "[ok] GmshPlotMesh",filename," is exported withNeumannBC. The value is:",maxval(this%Mesh%ElemMat(:))+20
				endif
			endif
			do i=1,size(this%Boundary%NBoundNodID,1 )
				do j=1,size(this%Boundary%NBoundNodID,2)
					
					if(this%Boundary%NBoundNodID(i,j)>0 .and. this%Boundary%NBoundVal(i,j)/=0.0d0)then
						nodeid1=this%Boundary%NBoundNodID(i,j)
					else
						cycle
					endif

					do k=1,size(this%Mesh%ElemNod,1)
						do l=1,size(this%Mesh%ElemNod,2)
							nodeid2=this%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								gp_value(k,:)=dble(maxval(this%Mesh%ElemMat(:)))+20.0d0 ! neumann is +20
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
			if(.not. allocated(this%Boundary%DBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> onlyDirichletBC >> no NBC is found."
				return
			else
				if(this%debug_mode)then
					print *, "[ok] GmshPlotMesh",filename," is exported onlyDirichletBC. The value is:",maxval(this%Mesh%ElemMat(:))+40
				endif
			endif
			do i=1,size(this%Boundary%DBoundNodID,1 )
				do j=1,size(this%Boundary%DBoundNodID,2)
					
					if(this%Boundary%DBoundNodID(i,j)>0 )then
						nodeid1=this%Boundary%DBoundNodID(i,j)
					else
						cycle
					endif

					
					do k=1,size(this%Mesh%ElemNod,1)
						do l=1,size(this%Mesh%ElemNod,2)
							nodeid2=this%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								if(l>size(gp_value,2) )then
									exit
								endif
								gp_value(k,l)=this%Boundary%DBoundVal(i,j)
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
		if( size(this%Mesh%ElemNod,2)==4 .and. size(this%Mesh%NodCoord,2)==2 ) then
			
			! 2-D, 4 noded, isoparametric elements with four gauss points 
			x_double(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )
			x_double(2,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )
			x_double(3,1:2)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )

			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
				

			x_double(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )
			x_double(2,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
			x_double(3,1:2)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )

			
			x(:,:)=x_double(:,:) 

			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
				
			x_double(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
			x_double(2,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(3,1:2)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				
			x_double(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(2,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )
			x_double(3,1:2)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			
			x(:,:)=x_double(:,:) 
			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			
		elseif(size(this%Mesh%ElemNod,2)==8 .and. size(this%Mesh%NodCoord,2)==3  ) then
			
			! 3-D, 8 noded, isoparametric elements with 8 gauss points
			! 1/8

			x_double(1,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )
			x_double(2,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )
			x_double(3,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2), 1:3  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  )
			x_double(4,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )

			x_double(5,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(6,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )
			
			x_double(7,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
					+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
					+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
					+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(8,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			
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

			x_double(1,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )
			
			x_double(2,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )
			
			x_double(3,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )
			

			x_double(4,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )

			x_double(5,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )

			x_double(6,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )

			
			x_double(7,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )


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

			x_double(8,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )
			
			x_double(2,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )
			

			x_double(1,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )

			x_double(6,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )

			x_double(7,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			
			x_double(4,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )


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

			x_double(6,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )
			
			x_double(7,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			

			x_double(1,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )

			x_double(8,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )

			
			x_double(2,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

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

			x_double(7,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(5,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )
			
			x_double(6,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )
			

			x_double(2,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(1,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )

			
			x_double(8,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

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

			x_double(8,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(6,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )
			
			x_double(5,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )
			

			x_double(1,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(3,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )

			x_double(2,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )

			
			x_double(7,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(4,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

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

			x_double(5,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			

			x_double(4,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(2,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			
			x_double(6,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(1,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

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

			x_double(5,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			
			x_double(6,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			

			x_double(2,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(4,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			
			x_double(8,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(1,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )


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
            print *, " size(this%Mesh%ElemNod,2)==",size(this%Mesh%ElemNod,2)
            print *, ".and. size(this%Mesh%NodCoord,2)==",size(this%Mesh%NodCoord,2)
			stop "plot_contour >> now constructing"
		endif
	enddo
	write(fh,*) '};'
	close(fh)
 end subroutine
 !===========================================================================================



! ########################################################################################
subroutine GmshPlotContour(this,gp_value,OptionalContorName,OptionalAbb,OptionalStep,Name)
	class(FEMDomain_),intent(in)::this
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
	!command="touch "//this%FileName//filename
	!call execute_command_line("touch "//this%FileName//filename)

	open(fh,file=this%FileName//filetitle//filename)
	print *, "writing ",this%FileName//filetitle//filename," step>>",step
	
	!---------------------
	if( size(this%Mesh%ElemNod,2)==4 .and. size(this%Mesh%NodCoord,2)==2 ) then
		allocate(x(4,3) )
		allocate(x_double(4,3) )
		
	elseif( size(this%Mesh%ElemNod,2)==8 .and. size(this%Mesh%NodCoord,2)==3 ) then
		allocate(x(8,3) )
		allocate(x_double(8,3) )
		
	endif


	x(:,:)=0.0d0
	write(fh,*) 'View "',mapname,'" {'
	do i=1,size(gp_value,1)
		if( size(this%Mesh%ElemNod,2)==4 .and. size(this%Mesh%NodCoord,2)==2 ) then
			
			! 2-D, 4 noded, isoparametric elements with four gauss points 
			x_double(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )
			x_double(2,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )
			x_double(3,1:2)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )

			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
				

			x_double(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )
			x_double(2,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
			x_double(3,1:2)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )

			
			x(:,:)=x_double(:,:) 

			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
				
			x_double(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
			x_double(2,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(3,1:2)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				
			x_double(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(2,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )
			x_double(3,1:2)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
			
			x(:,:)=x_double(:,:) 
			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
		elseif(size(this%Mesh%ElemNod,2)==8 .and. size(this%Mesh%NodCoord,2)==3  ) then
			
			
			! 3-D, 8 noded, isoparametric elements with 8 gauss points
			! 1/8

			x_double(1,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )
			x_double(2,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )
			x_double(3,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2), 1:3  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  )
			x_double(4,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )
			
			x_double(5,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(6,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )
			
			x_double(7,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
					+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
					+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
					+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(8,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
					+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			
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

			x_double(1,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )
			
			x_double(2,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )
			
			x_double(3,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )
			

			x_double(4,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )

			x_double(5,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )

			x_double(6,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )

			
			x_double(7,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )


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

			x_double(8,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )
			
			x_double(2,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )
			

			x_double(1,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )

			x_double(6,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )

			x_double(7,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			
			x_double(4,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )


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

			x_double(6,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )
			
			x_double(7,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			

			x_double(1,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )

			x_double(8,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )

			
			x_double(2,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

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

			x_double(7,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(5,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )
			
			x_double(6,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )
			

			x_double(2,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(1,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )

			
			x_double(8,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

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

			x_double(8,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(6,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )
			
			x_double(5,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )
			

			x_double(1,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )

			x_double(3,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )

			x_double(2,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )

			
			x_double(7,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(4,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

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

			x_double(5,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			

			x_double(4,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(2,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			
			x_double(6,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(1,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

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

			x_double(5,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			
			x_double(6,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )
			

			x_double(2,1:3)=&
				0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(4,1:3)=&
				 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4), 1:3  )+0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			
			x_double(8,1:3)=&
				0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )

			x_double(1,1:3)=&
				0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,5),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,7),1:3  )+0.1250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,8),1:3  )


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
subroutine GmshPlotVector(this,Vector,Name,FieldName,Step,fh,withMsh,ElementWize,NodeWize,onlyDirichlet)
	class(FEMDomain_),intent(in)::this
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
			
			call this%getDBCVector(DBCVector)
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
			write(FileHandle,'(A)')  center
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  '"'//FieldName//'"'
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  "0.0"
			write(FileHandle,'(A)')  "3"
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  "3"
			write(FileHandle,*) 	size(this%Mesh%NodCoord,1)  
			do i=1,size(this%Mesh%NodCoord,1)
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
					write(FileHandle,*) 	size(this%Mesh%NodCoord,1)  
					do i=1,size(this%Mesh%NodCoord,1)
						write(FileHandle,*) i,this%Mesh%NodCoord(i,:)
					enddo
					write(FileHandle,'(A)')  "$EndNodes"
					write(FileHandle,'(A)')  "$Elements"
					write(FileHandle,*) 	size(this%Mesh%ElemNod,1)
					do i=1,size(this%Mesh%ElemNod,1)
						write(FileHandle,*) i,"5 2 0 1 ",this%Mesh%ElemNod(i,:)
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
	write(FileHandle,'(A)')  center
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  '"'//FieldName//'"'
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  "0.0"
	write(FileHandle,'(A)')  "3"
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  "3"
	write(FileHandle,*) 	size(this%Mesh%NodCoord,1)  
	do i=1,size(this%Mesh%NodCoord,1)
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
			write(FileHandle,*) 	size(this%Mesh%NodCoord,1)  
			do i=1,size(this%Mesh%NodCoord,1)
				write(FileHandle,*) i,this%Mesh%NodCoord(i,:)
			enddo
			write(FileHandle,'(A)')  "$EndNodes"
			write(FileHandle,'(A)')  "$Elements"
			write(FileHandle,*) 	size(this%Mesh%ElemNod,1)
			do i=1,size(this%Mesh%ElemNod,1)
				write(FileHandle,*) i,"5 2 0 1 ",this%Mesh%ElemNod(i,:)
			enddo  
			write(FileHandle,'(A)')  "$EndElements"
			
			close(FileHandle)
		endif
	endif

	
	
end subroutine
!===========================================================================================


subroutine GmshPlotContour2D(this,gp_value,OptionalContorName,OptionalAbb,OptionalStep,Name)
	class(FEMDomain_),intent(in)::this
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


	open(40,file=this%FileName//filetitle//filename0)
	print *, "writing ",this%FileName//filetitle//filename0," step>>",step	

	!---------------------
	allocate(x(4,3) )
	x(:,:)=0.0d0
	write(40,*) 'View "',mapname,'" {'
	do i=1,size(this%Mesh%ElemNod,1)
		if( size(this%Mesh%ElemNod,2)/=4)  stop  "GmshPlotContour >> now constructing"
		
		
		x(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )
		x(2,1:2)=&
			0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )
		x(3,1:2)=&
			0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=&
			0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
			gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			
		x(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )
		x(2,1:2)=&
			0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
		x(3,1:2)=&
			0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=&
			0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
			gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			
		x(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
		x(2,1:2)=&
			0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
		x(3,1:2)=&
			0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=&
			0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
			gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			
		x(1,1:2)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
		x(2,1:2)=&
			0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )
		x(3,1:2)=&
			0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,1),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  )+0.250d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=&
			0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*this%Mesh%NodCoord(this%Mesh%ElemNod(i,4),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
			gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
	enddo
	write(40,*) '};'
 end subroutine GmshPlotContour2D
 !===========================================================================================
subroutine GmshExportStress(this,uvec,sigma,strain_measure,step,Name )
	class(FEMDomain_),intent(in)::this
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
	dim_num=size(this%Mesh%NodCoord,2)
	
	p_stress_field="Hydrostatic stress (kPa)"
	q_stress_field="Deviatoric stress (kPa)"
	p_strain_field="Hydrostatic strain "
	q_strain_field="Deviatoric strain "
	
	allocate(F_iJ(3,3),b_ij(3,3) )
	
	allocate( c_nod_coord(size(this%Mesh%NodCoord,1),size(this%Mesh%NodCoord,2))) 
	allocate(gp_value(size(this%Mesh%ElemNod,1),gp_number  ))
	
	do i=1,size(this%Mesh%NodCoord,1)
		c_nod_coord(i,:)=this%Mesh%NodCoord(i,:)+uvec(dim_num*(i-1)+1:dim_num*i )
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
	call GmshPlotContour(this,gp_value,mapname,abbrivation,step,Name=Name)
	
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
	call GmshPlotContour(this,gp_value,mapname,abbrivation,step,Name=Name)
	
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
	call GmshPlotContour(this,gp_value,mapname,abbrivation,step,Name=Name)
	
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
	call GmshPlotContour(this,gp_value,mapname,abbrivation,step,Name=Name)
	
 end subroutine
 !=======================================================================================
subroutine GnuplotPlotContour(this,gp_value,OptionalContorName,OptionalAbb,OptionalStep)
	class(FEMDomain_),intent(in)::this
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
	open(40,file="touch "//this%FileName//filename)
	print *, "writing .gnuplot-txt file... step>>",step
	!---------------------

    do i=1,size(gp_value,1)
        do j=1,size(gp_value,2)
            n=this%Mesh%ElemNod(i,j)
            write(40,*) this%Mesh%NodCoord(n,:),&
                gp_value(i,j)
        enddo
    enddo
    close(40)
 end subroutine GnuplotPlotContour
 !===========================================================================================
 
 !===========================================================================================
subroutine GnuplotExportStress(this,uvec,sigma,strain_measure,step )
	class(FEMDomain_),intent(in)::this
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
	dim_num=size(this%Mesh%NodCoord,2)
	
	p_stress_field="Hydrostatic stress (kPa)"
	q_stress_field="Deviatoric stress (kPa)"
	p_strain_field="Hydrostatic strain "
	q_strain_field="Deviatoric strain "
	
	allocate( c_nod_coord(size(this%Mesh%NodCoord,1),size(this%Mesh%NodCoord,2))) 
	allocate(gp_value(size(this%Mesh%ElemNod,1),gp_number  ))
	
	do i=1,size(this%Mesh%NodCoord,1)
		c_nod_coord(i,:)=this%Mesh%NodCoord(i,:)+uvec(dim_num*(i-1)+1:dim_num*i )
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
	call GnuplotPlotContour(this,gp_value,mapname,abbrivation,step)
	
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
	call GnuplotPlotContour(this,gp_value,mapname,abbrivation,step)
	
	!!"Hydrostatic strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			gp_value(i,j)=strain_measure(i,j,4)+strain_measure(i,j,5)+1.0d0
			gp_value(i,j)=gp_value(i,j)/3.0d0
		enddo
	enddo
	mapname=p_strain_field
	abbrivation="Hyepsi"
	call GnuplotPlotContour(this,gp_value,mapname,abbrivation,step)
	
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
	call GnuplotPlotContour(this,gp_value,mapname,abbrivation,step)
	
 end subroutine
 !=======================================================================================

pure function xrangeFEMDomain(this) result(xrange)
	class(FEMDomain_),intent(in) :: this
 	real(real64) :: xrange(1:2)

	if(.not.allocated(this%mesh%nodcoord) )then
		xrange = 0.0d0
	else
		xrange(1) = minval(this%mesh%nodcoord(:,1) )
		xrange(2) = maxval(this%mesh%nodcoord(:,1) )
	endif

end function

! ################################################

pure function yrangeFEMDomain(this) result(yrange)
	class(FEMDomain_),intent(in) :: this
 	real(real64) :: yrange(1:2)

	if(.not.allocated(this%mesh%nodcoord) )then
		yrange = 0.0d0
	else
		yrange(1) = minval(this%mesh%nodcoord(:,2) )
		yrange(2) = maxval(this%mesh%nodcoord(:,2) )
	endif

end function

! ################################################

pure function zrangeFEMDomain(this) result(zrange)
	class(FEMDomain_),intent(in) :: this
 	real(real64) :: zrange(1:2)

	if(.not.allocated(this%mesh%nodcoord) )then
		zrange = 0.0d0
	else
		zrange(1) = minval(this%mesh%nodcoord(:,3) )
		zrange(2) = maxval(this%mesh%nodcoord(:,3) )
	endif

end function

! ################################################
subroutine moveFEMDomain(this,x,y,z,NodeList,to)
	class(FEMDomain_),intent(inout)::this
	real(real64),optional,intent(in)::x,y,z
	real(real64),allocatable :: center(:)
	integer(int32),optional,intent(in) :: NodeList(:)
	character(*),optional,intent(in) :: to
	integer(int32) :: i, nid

	if(present(to) )then
		select case(to)
		case("center","CENTER","Center")
			center = this%centerPosition()
			call this%move(x=-center(1),y=-center(2),z=-center(3) )
		case("origin","ORIGIN","Origin")
			center = zeros(this%nd() )
			do i=1,this%nd()
				center(i) = minval(this%mesh%nodcoord(:,i))
			enddo
			call this%move(x=-center(1),y=-center(2),z=-center(3) )
		case default
			print *, "ERROR :: moveFEMDomain + arg :: to >> invalid keyword"
			print *, "select center or origin"
		end select
		return
	endif
	
	if(present(NodeList) )then

		if(present(x) )then
			do i=1,size(NodeList)
				nid = NodeList(i)
				this%Mesh%NodCoord(nid,1)=this%Mesh%NodCoord(nid,1)+x
			enddo
		endif


		if(present(y) )then
			do i=1,size(NodeList)
				nid = NodeList(i)
				this%Mesh%NodCoord(nid,2)=this%Mesh%NodCoord(nid,2)+y
			enddo
		endif


		if(size(this%Mesh%NodCoord,2) <3 .and. present(z))then
			print *, "ERROR :: moveFEMDomain >> z cannot be imported"
			return
		endif

		if(present(z) )then

			do i=1,size(NodeList)
				nid = NodeList(i)
				this%Mesh%NodCoord(nid,3)=this%Mesh%NodCoord(nid,3)+z
			enddo
		endif
	
	else
	
		if(present(x) )then
			this%Mesh%NodCoord(:,1)=this%Mesh%NodCoord(:,1)+x
		endif


		if(present(y) )then
			this%Mesh%NodCoord(:,2)=this%Mesh%NodCoord(:,2)+y
		endif


		if(size(this%Mesh%NodCoord,2) <3 .and. present(z))then
			print *, "ERROR :: moveFEMDomain >> z cannot be imported"
			return
		endif

		if(present(z) )then
			this%Mesh%NodCoord(:,3)=this%Mesh%NodCoord(:,3)+z
		endif
	endif

end subroutine
! ################################################
 

! ################################################
subroutine rotateFEMDomain(this,x,y,z,deg)
	class(FEMDomain_),intent(inout)::this
	real(real64),optional,intent(in)::x,y,z
	real(real64) ::xd,yd,zd,x_u,y_u,z_u
	real(real64),allocatable :: midpoint(:),&
	rotmat_x(:,:),rotmat_y(:,:),rotmat_z(:,:),&
	all_rotmat(:,:),rotation(:),coord(:),total_rot(:)

	integer(int32) :: i,j,n,m
	logical,optional,intent(in) :: deg
	logical :: xyz_nonzero(3)

	! Euler-XYZ
	xyz_nonzero(1:3) = .true.

	n=size(this%Mesh%NodCoord,2)
	m=size(this%Mesh%NodCoord,1)


	if(this%nd()==2 )then

		! unroll from Y to X

		midpoint = this%centerPosition()
		do i=1,this%nn()
			this%Mesh%NodCoord(i,:)=this%Mesh%NodCoord(i,:)-midpoint(:)
		enddo
		all_rotmat = eyes(this%nd(),this%nd() )

		rotmat_x = eyes(this%nd(),this%nd() )
		rotmat_y = eyes(this%nd(),this%nd() )
		
		all_rotmat = eyes(2,2)
		
		total_rot = this%total_rotation
		
		x_u = - total_rot(1)
		y_u = - total_rot(2)
		
		rotmat_x(1,1)=cos(x_u)  ;rotmat_x(1,2) =-sin(x_u)    
		rotmat_x(2,1)=sin(x_u)   ;rotmat_x(2,2)= cos(x_u)  
		rotmat_y(1,1)=cos(y_u)  ;rotmat_y(1,2) =-sin(y_u)    
		rotmat_y(2,1)=sin(y_u)   ;rotmat_y(2,2)= cos(y_u)  

		! roll back
		all_rotmat = matmul(rotmat_z,all_rotmat)
		all_rotmat = matmul(rotmat_y,all_rotmat)

		if(present(x) )then
			if(present(deg) )then
				if(deg)then
					this%total_rotation(1) = this%total_rotation(1) + radian(x)
				else
					this%total_rotation(1) = this%total_rotation(1) + x
				endif
			else
				this%total_rotation(1) = this%total_rotation(1) + x
			endif
		endif

		if(present(y) )then
			!this%total_rotation(2) = this%total_rotation(2)+ y
			if(present(deg) )then
				if(deg)then
					this%total_rotation(2) = this%total_rotation(2) + radian(y)
				else
					this%total_rotation(2) = this%total_rotation(2) + y
				endif
			else
				this%total_rotation(2) = this%total_rotation(2) + y
			endif
		endif

		total_rot = this%total_rotation
		
		x_u = total_rot(1)
		y_u = total_rot(2)

		rotmat_x(1,1)=cos(x_u)  ;rotmat_x(1,2) =-sin(x_u)    
		rotmat_x(2,1)=sin(x_u)   ;rotmat_x(2,2)= cos(x_u)  
		rotmat_y(1,1)=cos(y_u)  ;rotmat_y(1,2) =-sin(y_u)    
		rotmat_y(2,1)=sin(y_u)   ;rotmat_y(2,2)= cos(y_u) 

		all_rotmat = matmul(rotmat_x,all_rotmat)
		all_rotmat = matmul(rotmat_y,all_rotmat)

		!$OMP parallel do
		do i=1,this%nn()
			this%Mesh%NodCoord(i,:)=matmul(all_rotmat,this%Mesh%NodCoord(i,:))	
		enddo
		!$OMP end parallel do

		!$OMP parallel do
		do i=1,this%nn()
			this%Mesh%NodCoord(i,:)=this%Mesh%NodCoord(i,:)+midpoint(:)
		enddo
		!$OMP end parallel do

	elseif(this%nd()==3 )then
		! unroll from Z to X

		midpoint = this%centerPosition()
		do i=1,this%nn()
			this%Mesh%NodCoord(i,:)=this%Mesh%NodCoord(i,:)-midpoint(:)
		enddo
		all_rotmat = eyes(this%nd(),this%nd() )

		rotmat_x = eyes(this%nd(),this%nd() )
		rotmat_y = eyes(this%nd(),this%nd() )
		rotmat_z = eyes(this%nd(),this%nd() )
		
		all_rotmat = eyes(3,3)
		
		total_rot = this%total_rotation
		
		x_u = - total_rot(1)
		y_u = - total_rot(2)
		z_u = - total_rot(3)
		
		rotmat_x(1,1)=1.0d0	;rotmat_x(1,2)=0.0d0		;rotmat_x(1,3)=0.0d0			;
		rotmat_x(2,1)=0.0d0	;rotmat_x(2,2)=cos(x_u)		;rotmat_x(2,3)=-sin(x_u)		;
		rotmat_x(3,1)=0.0d0	;rotmat_x(3,2)=sin(x_u)		;rotmat_x(3,3)= cos(x_u)		;
		rotmat_y(1,1)=cos(y_u)	;rotmat_y(1,2)=0.0d0		;rotmat_y(1,3)=sin(y_u)			;
		rotmat_y(2,1)=0.0d0	;rotmat_y(2,2)=1.0d0		;rotmat_y(2,3)=0.0d0		;
		rotmat_y(3,1)=-sin(y_u)	;rotmat_y(3,2)=0.0d0		;rotmat_y(3,3)= cos(y_u)		;
		rotmat_z(1,1)=cos(z_u)	;rotmat_z(1,2)=-sin(z_u)	;rotmat_z(1,3)=0.0d0		;
		rotmat_z(2,1)=sin(z_u)	;rotmat_z(2,2)=cos(z_u)		;rotmat_z(2,3)=0.0d0		;
		rotmat_z(3,1)=0.0d0	;rotmat_z(3,2)=0.0d0		;rotmat_z(3,3)=1.0d0 		;	
		
		! roll back
		all_rotmat = matmul(rotmat_z,all_rotmat)
		all_rotmat = matmul(rotmat_y,all_rotmat)
		all_rotmat = matmul(rotmat_x,all_rotmat)


		if(present(x) )then
			if(present(deg) )then
				if(deg)then
					this%total_rotation(1) = this%total_rotation(1) + radian(x)
				else
					this%total_rotation(1) = this%total_rotation(1) + x
				endif
			else
				this%total_rotation(1) = this%total_rotation(1) + x
			endif
		endif

		if(present(y) )then
			!this%total_rotation(2) = this%total_rotation(2)+ y
			if(present(deg) )then
				if(deg)then
					this%total_rotation(2) = this%total_rotation(2) + radian(y)
				else
					this%total_rotation(2) = this%total_rotation(2) + y
				endif
			else
				this%total_rotation(2) = this%total_rotation(2) + y
			endif
		endif


		if(present(z) )then
			if(present(deg) )then
				if(deg)then
					this%total_rotation(3) = this%total_rotation(3) + radian(z)
				else
					this%total_rotation(3) = this%total_rotation(3) + z
				endif
			else
				this%total_rotation(3) = this%total_rotation(3) + z
			endif
		endif


		total_rot = this%total_rotation
		
		x_u = total_rot(1)
		y_u = total_rot(2)
		z_u = total_rot(3)

		rotmat_x(1,1)=1.0d0	;rotmat_x(1,2)=0.0d0		;rotmat_x(1,3)=0.0d0			;
		rotmat_x(2,1)=0.0d0	;rotmat_x(2,2)=cos(x_u)		;rotmat_x(2,3)=-sin(x_u)		;
		rotmat_x(3,1)=0.0d0	;rotmat_x(3,2)=sin(x_u)		;rotmat_x(3,3)= cos(x_u)		;
		
		rotmat_y(1,1)=cos(y_u)	;rotmat_y(1,2)=0.0d0		;rotmat_y(1,3)=sin(y_u)			;
		rotmat_y(2,1)=0.0d0	;rotmat_y(2,2)=1.0d0		;rotmat_y(2,3)=0.0d0		;
		rotmat_y(3,1)=-sin(y_u)	;rotmat_y(3,2)=0.0d0		;rotmat_y(3,3)= cos(y_u)		;
		
		rotmat_z(1,1)=cos(z_u)	;rotmat_z(1,2)=-sin(z_u)	;rotmat_z(1,3)=0.0d0		;
		rotmat_z(2,1)=sin(z_u)	;rotmat_z(2,2)=cos(z_u)		;rotmat_z(2,3)=0.0d0		;
		rotmat_z(3,1)=0.0d0	;    rotmat_z(3,2)=0.0d0		;rotmat_z(3,3)=1.0d0 	

		all_rotmat = matmul(rotmat_x,all_rotmat)
		all_rotmat = matmul(rotmat_y,all_rotmat)
		all_rotmat = matmul(rotmat_z,all_rotmat)

		!$OMP parallel do
		do i=1,this%nn()
			this%Mesh%NodCoord(i,:)=matmul(all_rotmat,this%Mesh%NodCoord(i,:))	
		enddo
		!$OMP end parallel do

		!$OMP parallel do
		do i=1,this%nn()
			this%Mesh%NodCoord(i,:)=this%Mesh%NodCoord(i,:)+midpoint(:)
		enddo
		!$OMP end parallel do

	endif
end subroutine
! ################################################



! ################################################
subroutine AddNBCFEMDomain(this,NodID,DimID,Val,FastMode)
	class(FEMDomain_),intent(inout)::this
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

	if(.not.allocated(this%Boundary%NBoundNodID))then
		print *, "ERROR  :: AddNBC >> this%Boundary%NBoundNodID should be allocated."
		print *, "Initializing NBC..."
		call this%InitNBC(NumOfValPerNod=3)
		return
	endif

	! check wheather NodID exisits or not
	! if this%Boundary%NBoundNodID(NodID) is found, add the current Val to the last value and return.
	do i=1,size(this%Boundary%NBoundNodID,1)
		if(this%Boundary%NBoundNodID(i,DimID)==NodID)then
			this%Boundary%NBoundVal(i,DimID)=this%Boundary%NBoundVal(i,DimID)+Val
			return
		endif 
	enddo
	
	if(fmode .eqv. .false.)then
		installed=0
		do i=1,size(this%Boundary%NBoundNodID,1)
			if(this%Boundary%NBoundNodID(i,DimID)==-1  )then
				this%Boundary%NBoundNodID(i,DimID)=NodID
				this%Boundary%NBoundVal(i,DimID)=Val
				this%Boundary%NBoundNum(DimID)=this%Boundary%NBoundNum(DimID)+1
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
		n=size(this%Boundary%NBoundNodID,1)
		call insertArray(this%Boundary%NBoundNodID ,insert1stColumn=.true.,DefaultValue=-1 ,NextOf=n)
		call insertArray(this%Boundary%NBoundVal ,insert1stColumn=.true.,DefaultValue=0.0d0,NextOf=n)
		i=n+1
		this%Boundary%NBoundNodID(i,DimID)=NodID
		this%Boundary%NBoundVal(i,DimID)=Val
		this%Boundary%NBoundNum(DimID)=this%Boundary%NBoundNum(DimID)+1
		
	endif

end subroutine
! ################################################
! too slow
subroutine ExportFEMDomainAsSTL(this,FileHandle,MeshDimension,FileName)
	class(FEMDomain_),intent(inout)::this
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
	
		write (filename0, '("_", i6.6, ".stl")') this%Timestep ! ここでファイル名を生成している
		call execute_command_line(  "touch "//filename//filename0 )
		print *, filename//filename0
	
		open(fh,file=filename//filename0 )
		! hot-splot
		call this%Mesh%GetSurface(sorting=.false.)
		
		if(dim_num/=3)then
			print *, "Sorry, Export stl is supported only for 3-D mesh"
			close(fh)
			return
		endif
		write(fh,'(A)') "solid "//filename
		print *, "Number of facet is",size(this%Mesh%FacetElemNod,1)
		do i=1,size(this%Mesh%FacetElemNod,1)
			if(size(this%Mesh%FacetElemNod,2)==4  )then
				! rectangular
				! describe two triangular
				
				x1(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,2),: )
				x3(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),: )
				write(fh,'(A)') "facet normal 0.0 0.0 1.0"
				write(fh,'(A)') "outer loop"
				write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
				write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
				write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
				write(fh,'(A)') "endloop"
				write(fh,'(A)') "endfacet"
				x1(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),: )
				x3(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,4),: )
				write(fh,'(A)') "facet normal 0.0 0.0 1.0"
				write(fh,'(A)') "outer loop"
				write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
				write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
				write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
				write(fh,'(A)') "endloop"
				write(fh,'(A)') "endfacet"
			elseif(size(this%Mesh%FacetElemNod,2)==3  )then
				! rectangular
				! describe two triangular
				x1(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,2),: )
				x3(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),: )
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
		write(fh,'(A)') "endsolid "//filename
	
		print *, "writing ",filename//filename0," step>>",this%Timestep
		flush(fh)
		close(fh)
		return
		
	endif

	if(present(FileHandle) )then
	
		fh=FileHandle
		
		call this%Mesh%GetSurface()
		dim_num=input(default=3,option=MeshDimension)
		if(dim_num/=3)then
			print *, "Sorry, Export stl is supported only for 3-D mesh"
			return
		endif
		write(fh,'(A)') "solid stl"
		print *, "Number of facet is",size(this%Mesh%FacetElemNod,1)
		do i=1,size(this%Mesh%FacetElemNod,1)
			if(size(this%Mesh%FacetElemNod,2)==4  )then
				! rectangular
				! describe two triangular
				
				x1(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,2),: )
				x3(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),: )
				write(fh,'(A)') "facet normal 0.0 0.0 1.0"
				write(fh,'(A)') "outer loop"
				write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
				write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
				write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
				write(fh,'(A)') "endloop"
				write(fh,'(A)') "endfacet"
				x1(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),: )
				x3(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,4),: )
				write(fh,'(A)') "facet normal 0.0 0.0 1.0"
				write(fh,'(A)') "outer loop"
				write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
				write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
				write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
				write(fh,'(A)') "endloop"
				write(fh,'(A)') "endfacet"
			elseif(size(this%Mesh%FacetElemNod,2)==3  )then
				! rectangular
				! describe two triangular
				x1(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),: ) 
				x2(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,2),: )
				x3(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),: )
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
		write(fh,'(A)') "endsolid "//filename
	
		print *, "writing ",filename//filename0," step>>",this%Timestep
		flush(fh)
		return
		
	endif


	dim_num=input(default=3,option=MeshDimension)

    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh =104
    endif

	write (filename0, '("_", i6.6, ".stl")') this%Timestep ! ここでファイル名を生成している
	call execute_command_line(  "touch "//this%FileName//filename0 )
	print *, this%FileName//filename0

	open(fh,file=this%FileName//filename0 )

	call this%Mesh%GetSurface()
	
	if(dim_num/=3)then
		print *, "Sorry, Export stl is supported only for 3-D mesh"
		close(fh)
		return
	endif
	write(fh,'(A)') "solid "//this%FileName
	print *, "Number of facet is",size(this%Mesh%FacetElemNod,1)
	do i=1,size(this%Mesh%FacetElemNod,1)
		if(size(this%Mesh%FacetElemNod,2)==4  )then
			! rectangular
			! describe two triangular
			x1(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),: ) 
			x2(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,2),: )
			x3(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),: )
			write(fh,'(A)') "facet normal 0.0 0.0 1.0"
			write(fh,'(A)') "outer loop"
			write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
			write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
			write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
			write(fh,'(A)') "endloop"
			write(fh,'(A)') "endfacet"
			x1(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,1),: ) 
			x2(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,3),: )
			x3(:)=this%Mesh%NodCoord(this%Mesh%FacetElemNod(i,4),: )
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
	write(fh,'(A)') "endsolid "//this%FileName

	print *, "writing ",this%FileName//filename0," step>>",this%Timestep
	flush(fh)
	close(fh)

end subroutine


!#######################################
subroutine meshingFEMDomain(this)
	class(FEMDomain_),intent(inout)::this

	call this%Mesh%meshing()
end subroutine
!#######################################


!#######################################
subroutine removeDBoundCondition(this)
	class(FEMDomain_),intent(inout)::this

	call this%Boundary%removeDBC()
end subroutine
!#######################################

!#######################################
subroutine removeNBoundCondition(this)
	class(FEMDomain_),intent(inout)::this
	call this%Boundary%removeNBC()
end subroutine
!#######################################


!#######################################
subroutine removeTBoundCondition(this)
	class(FEMDomain_),intent(inout)::this
	call this%Boundary%removeTBC()
end subroutine
!#######################################

!#######################################
subroutine CheckConnedctivityFEMDomain(this,fix)
	class(FEMDomain_),intent(inout)::this
	integer(int32),allocatable:: checklist(:,:),new_node_id(:)
	logical,optional,intent(in)::fix
	integer(int32) :: i,n,m,j

	n=size(this%Mesh%NodCoord,1)
	allocate(checklist(n,1),new_node_id(n) )
	checklist(:,1)=0

	do i=1,n
		new_node_id(i)=i
	enddo

	do i=1,size(this%Mesh%ElemNod,1)
		do j=1,size(this%Mesh%ElemNod,2)
			checklist( this%Mesh%ElemNod(i,j),1 )=1
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
		if(this%debug_mode)then
			print *, "[HIT!] Non-connected nodes exist"
		endif
	else
		if(this%debug_mode)then
			print *, "[OK] All nodes are connected."
		endif
	endif

	if(present(fix) )then
		if( fix .eqv. .true. )then
			! update connectivity
			do i=1,size(this%Mesh%ElemNod,1)
				do j=1,size(this%Mesh%ElemNod,2)
					if(new_node_id(this%Mesh%ElemNod(i,j))==0)then
						print *, "ERROR :: CheckConnedctivityFEMDomain"
					endif
					this%Mesh%ElemNod(i,j)=new_node_id(this%Mesh%ElemNod(i,j))
				enddo
			enddo
			
			! remove astray node
			i=1
			do 
				if(checklist(i,1)==0 )then
					call removeArray(this%Mesh%NodCoord,remove1stColumn=.true.,NextOf=i-1)
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
	if(this%debug_mode)then
		print *, "[OK] All nodes are connected."
	endif
end subroutine
!#######################################

subroutine getDBCVectorFEMDomain(this,DBCvec)
	class(FEMDomain_),intent(in)::this
	real(real64),allocatable,intent(inout)::DBCvec(:,:)
	integer(int32) :: i,j,n,m,k,l
	n=size(this%Mesh%NodCoord,1)
	m=size(this%Mesh%NodCoord,2)
	if(.not. allocated(DBCvec ) )then
		allocate(DBCvec(n,m) )
		DBCvec(:,:)=0.0d0
	endif

	! check number of DBC
	do i=1,size(this%Boundary%DBoundNum)
		k=countif(Array=this%Boundary%DBoundNodID(:,i),Value=-1,notEqual=.true.)
		l=this%Boundary%DBoundNum(i)
		if(k /= l)then
			print *, "Caution :: FiniteDeformationClass::getDBCVector :: check number of DBC :: k /= l"
		endif
	enddo

	do i=1,size(this%Boundary%DBoundNodID,1)
		do j=1,size(this%Boundary%DBoundNodID,2)
			if(this%Boundary%DBoundNodID(i,j) <=0)then
				cycle
			endif
			DBCvec(this%Boundary%DBoundNodID(i,j),j )=this%Boundary%DBoundVal(i,j)
		enddo
	enddo


end subroutine
! ##################################################

! ##################################################
subroutine convertMeshTypeFEMDomain(this,Option)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: Option

	print *, "[Caution!] >> convertMeshType  is not recommended!"
    print *, "              please use changeElementType()"
	call this%Mesh%convertMeshType(Option=Option)

end subroutine
! ##################################################


! ##################################################
subroutine changeElementTypeFEMDomain(this,elementtype)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: elementtype(:)
	
	call this%Mesh%changeElementType(elementtype=elementtype)

end subroutine
! ##################################################

subroutine remeshFEMDomain(this,meshtype,Name,x_num,y_num,z_num,x_len,y_len,z_len,Le,Lh,Dr,thickness,division,&
	top,margin,inclineRate,shaperatio,master,slave,x,y,z,dx,dy,dz,coordinate)
	class(FEMDomain_),intent(inout) :: this
	type(FEMDomain_),optional,intent(inout) :: master,slave
	character(*),optional,intent(in) :: meshtype
	character(*),optional,intent(in) ::Name
	integer(int32),optional,intent(in) :: x_num,y_num,z_num ! number of division
	integer(int32) :: xnum,ynum,znum ! number of division
    integer(int32),optional,intent(in) :: division ! for 3D rectangular
	real(real64),optional,intent(in) :: x_len,y_len,z_len,Le,Lh,Dr ! length
	real(real64) :: xlen,ylen,zlen ! length
	real(real64),optional,intent(in) :: thickness ! for 3D rectangular
	real(real64),optional,intent(in) :: shaperatio ! for 3D leaf
    real(real64),optional,intent(in) :: top,margin,inclineRate ! for 3D Ridge and dam
	real(real64),optional,intent(in) :: x,y,z,dx,dy,dz,coordinate(:,:)
	
	integer,dimension(3),parameter :: versions_to_test = [0,1,4]

!	! create uuid
!
!	this%meshtype = meshtype
!
!	this%uuid = generate_uuid(1)
!	this%mesh%uuid = this%uuid
!
	xnum=input(default=10,option=x_num)
	ynum=input(default=10,option=y_num)
	znum=input(default=10,option=z_num)

	xlen=input(default=1.0d0,option=x_len)
	ylen=input(default=1.0d0,option=y_len)
	zlen=input(default=1.0d0,option=z_len)

!	if(present(Name) )then
!		this%Name=Name
!		this%FileName=Name
!	else
!		this%Name="NoName"
!		this%FileName="NoName"
!	endif

!	! if create interface, set paired uuid in address
!	this%link(1) = "None"
!	this%link(2) = "None"
!	
!	if(present(master) )then
!		this%link(1) = master%uuid
!	endif
!
!	if(present(slave) )then
!		this%link(2) = slave%uuid
!	endif

	if(present(z_num) .or. present(z_len) )then
		call this%Mesh%remesh(meshtype=meshtype,x_num=xnum,y_num=ynum,x_len=xlen,y_len=ylen,Le=Le,&
			Lh=Lh,Dr=Dr,thickness=zlen,top=top,margin=margin,shaperatio=shaperatio,&
			master=master%mesh,slave=slave%mesh,x=x,y=y,z=z,dx=dx,dy=dy,dz=dz,&
			coordinate=coordinate,division=znum)
	elseif(present(thickness) )then
		call this%Mesh%remesh(meshtype=meshtype,x_num=xnum,y_num=ynum,x_len=xlen,y_len=ylen,Le=Le,&
			Lh=Lh,Dr=Dr,thickness=thickness,top=top,margin=margin,shaperatio=shaperatio,&
			master=master%mesh,slave=slave%mesh,x=x,y=y,z=z,dx=dx,dy=dy,dz=dz,&
			coordinate=coordinate,division=znum)
	else
		call this%Mesh%remesh(meshtype=meshtype,x_num=xnum,y_num=ynum,x_len=xlen,y_len=ylen,Le=Le,&
			Lh=Lh,Dr=Dr,top=top,margin=margin,shaperatio=shaperatio,&
			master=master%mesh,slave=slave%mesh,x=x,y=y,z=z,dx=dx,dy=dy,dz=dz,&
			coordinate=coordinate,division=znum)
	endif

!	if(this%nd()==2 .or. this%nd()==3)then
!		call this%getSurface()
!	endif

end subroutine

! ##################################################
subroutine createFEMDomain(this,meshtype,Name,x_num,y_num,z_num,x_len,y_len,z_len,Le,Lh,Dr,thickness,division,&
	top,margin,inclineRate,shaperatio,master,slave,x,y,z,dx,dy,dz,coordinate,species,SoyWidthRatio,&
	x_axis,y_axis,z_axis)
	class(FEMDomain_),intent(inout) :: this
	type(FEMDomain_),optional,intent(inout) :: master,slave
	character(*),intent(in) :: meshtype
	character(*),optional,intent(in) ::Name
	integer(int32),optional,intent(in) :: x_num,y_num,z_num ! number of division
	integer(int32) :: xnum,ynum,znum ! number of division
    integer(int32),optional,intent(in) :: division ! for 3D rectangular
	real(real64),optional,intent(in) :: x_len,y_len,z_len,Le,Lh,Dr ! length
	real(real64) :: xlen,ylen,zlen ! length
	real(real64),optional,intent(in) :: thickness ! for 3D rectangular
	real(real64),optional,intent(in) :: shaperatio ! for 3D leaf
    real(real64),optional,intent(in) :: top,margin,inclineRate ! for 3D Ridge and dam
	real(real64),optional,intent(in) :: x,y,z,dx,dy,dz,coordinate(:,:)
	real(real64),optional,intent(in) :: x_axis(:),y_axis(:),z_axis(:)
	integer(int32),optional,intent(in) :: species
	real(real64),optional,intent(in) :: SoyWidthRatio
	integer,dimension(3),parameter :: versions_to_test = [0,1,4]

	! create uuid

	this%meshtype = meshtype

	this%uuid = generate_uuid(1)
	this%mesh%uuid = this%uuid

	xnum=input(default=10,option=x_num)
	ynum=input(default=10,option=y_num)
	znum=input(default=10,option=z_num)

	xlen=input(default=1.0d0,option=x_len)
	ylen=input(default=1.0d0,option=y_len)
	zlen=input(default=1.0d0,option=z_len)


	if(present(Name) )then
		this%Name=Name
		this%FileName=Name
	else
		this%Name="NoName"
		this%FileName="NoName"
	endif

	! if create interface, set paired uuid in address
	this%link(1) = "None"
	this%link(2) = "None"
	
	if(present(master) )then
		this%link(1) = master%uuid
	endif

	if(present(slave) )then
		this%link(2) = slave%uuid
	endif


	select case(meshtype)
		case("Cube","Cube3D")
			if(present(x_axis) .and. present(y_axis) )then
				if(present(z_axis) )then
					call this%mesh%cube(x=x_axis,y=y_axis,z=z_axis)
				else
					call this%mesh%cube(x=x_axis,y=y_axis,z=[0.0d0,1.0d0])
				endif
				return
			endif
		case("Box","Box2D")
			call this%mesh%box(x=x_axis,y=y_axis)
		case ("Line","Line1D")
			call this%mesh%line(x_num=x_num,x_axis=x_axis)
			return

	end select

	if(present(z_num) .or. present(z_len) )then
		call this%Mesh%create(meshtype=meshtype,x_num=xnum,y_num=ynum,x_len=xlen,y_len=ylen,Le=Le,&
			Lh=Lh,Dr=Dr,thickness=zlen,top=top,margin=margin,shaperatio=shaperatio,&
			master=master%mesh,slave=slave%mesh,x=x,y=y,z=z,dx=dx,dy=dy,dz=dz,&
			coordinate=coordinate,division=znum,species=species,SoyWidthRatio=SoyWidthRatio)
	elseif(present(thickness) )then
		call this%Mesh%create(meshtype=meshtype,x_num=xnum,y_num=ynum,x_len=xlen,y_len=ylen,Le=Le,&
			Lh=Lh,Dr=Dr,thickness=thickness,top=top,margin=margin,shaperatio=shaperatio,&
			master=master%mesh,slave=slave%mesh,x=x,y=y,z=z,dx=dx,dy=dy,dz=dz,&
			coordinate=coordinate,division=znum,species=species,SoyWidthRatio=SoyWidthRatio)
	else
		call this%Mesh%create(meshtype=meshtype,x_num=xnum,y_num=ynum,x_len=xlen,y_len=ylen,Le=Le,&
			Lh=Lh,Dr=Dr,top=top,margin=margin,shaperatio=shaperatio,&
			master=master%mesh,slave=slave%mesh,x=x,y=y,z=z,dx=dx,dy=dy,dz=dz,&
			coordinate=coordinate,division=znum,species=species,SoyWidthRatio=SoyWidthRatio)
	endif

end subroutine createFEMDomain
! ##################################################

! ##################################################
subroutine setBoundaryFEMDomain(this,new,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,value,values)
	class(FEMDomain_),intent(inout) :: this
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
	real(real64),optional,intent(in) :: value,values(4)
	logical,optional,intent(in) :: new

	!call this%Boundary%setDB(new,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,value,values)

end subroutine setBoundaryFEMDomain
! ##################################################

! ##################################################
subroutine showRangeFEMDomain(this)
	class(FEMDomain_)::this

	call this%Mesh%showRange()
end subroutine
! ##################################################

! ##################################################
subroutine ImportBoundariesFEMDomain(this,Boundary,NumberOfBoundaries,BoundaryID)
	class(FEMDomain_),intent(inout) :: this
	type(Boundary_),target,intent(in) :: Boundary
	integer(int32),optional,intent(in) :: NumberOfBoundaries,BoundaryID
	integer(int32) :: n,i


	if(.not.allocated(this%Boundaries) )then
		n=input(default=30,option=NumberOfBoundaries)
		allocate(this%Boundaries(n))
		do i=1,n
			nullify(this%Boundaries(i)%boundaryp)
		enddo
		this%NumberOfBoundaries = 0
	endif

	if(present(BoundaryID) )then
		if(BoundaryID > size(this%Boundaries) )then
			print *, "ERROR :: ImportBoundariesFEMDomain >> requested BoundaryID is grater than the size of stack"
			print *, "Stack size is ",size(this%Boundaries), " , and your request is ",BoundaryID
			return
		endif
		if(BoundaryID > this%NumberOfBoundaries)then
			print *, "ERROR :: ImportBoundariesFEMDomain >> requested BoundaryID is grater than the Last ID"
			print *, "The last ID is ",this%NumberOfBoundaries+1, " , and your request is ",BoundaryID
			print *, "Hence, your request ",BoundaryID, " is accepted as the ID of ",this%NumberOfBoundaries+1
			this%NumberOfBoundaries=this%NumberOfBoundaries+1
			this%Boundaries(this%NumberOfBoundaries)%Boundaryp => Boundary
			print *, "Now, number of boundary conditions is ",this%NumberOfBoundaries
			return
		endif
		if( associated(this%Boundaries(BoundaryID)%Boundaryp) )then
			print *, "Boundary ID :: ", BoundaryID, " is overwritten."
			nullify(this%Boundaries(BoundaryID)%Boundaryp )
		endif
		this%Boundaries(BoundaryID)%Boundaryp => Boundary
		return
	endif

	this%NumberOfBoundaries=this%NumberOfBoundaries+1

	this%Boundaries(this%NumberOfBoundaries)%Boundaryp => Boundary

	print *, "Now, number of boundary conditions is ",this%NumberOfBoundaries

end subroutine ImportBoundariesFEMDomain
! ##################################################


! ##################################################
subroutine showBoundariesFEMDomain(this,Name)
	class(FEMDomain_),intent(inout) :: this
	character(*),optional,intent(in)::Name
	integer(int32) :: i

	if(present(Name) )then
		print *, "Domain Name is :: ", name
	endif

	if(.not. allocated(this%Boundaries) )then
		print *, "No boundary is set."
	else
		do i=1,this%NumberOfBoundaries
			print *, "Layer :: ",this%Boundaries(i)%Boundaryp%Layer,"B.C. ::",i," => ",&
				associated(this%Boundaries(i)%Boundaryp)
		enddo
	endif
end subroutine showBoundariesFEMDomain
! ##################################################


! ##################################################
subroutine removeBoundariesFEMDomain(this,Name,BoundaryID)
	class(FEMDomain_),intent(inout) :: this
	character(*),optional,intent(in)::Name
	integer(int32) :: i
	integer(int32),optional,intent(in) ::BoundaryID

	if(present(Name) )then
		print *, "Domain Name is :: ", name
	endif

	if(.not. allocated(this%Boundaries) )then
		print *, "No boundary is set."
	else
		if(present(BoundaryID))then
			nullify(this%Boundaries(BoundaryID)%Boundaryp)
		else
			do i=1,this%NumberOfBoundaries
				nullify(this%Boundaries(i)%Boundaryp)
			enddo
		endif
	endif
	call this%showBoundaries(Name)

end subroutine removeBoundariesFEMDomain
! ##################################################

! ##################################################
subroutine copyFEMDomain(this,OriginalObj,onlyMesh)
	class(FEMDomain_),intent(inout) :: this
	class(FEMDomain_),intent(in) :: OriginalObj
	logical,optional,intent(in) :: onlyMesh


	call this%Mesh%copy(OriginalObj%Mesh)
	this%FileName=OriginalObj%FileName
	this%Name=OriginalObj%Name
	if(present(onlyMesh) )then
		if(onlyMesh .eqv. .true.)then
			print *, "Only mesh is copied."
			return
		endif
	endif

end subroutine copyFEMDomain
! ##################################################

! ##################################################
recursive subroutine bakeFEMDomain(this, template, templateFile,&
	NodalDOF,NumOfMaterialPara,Tol,SimMode,ItrTol,Timestep)
	class(FEMDomain_),intent(inout) :: this
	character(*),optional,intent(in) :: template
	character(*),optional,intent(in) :: templateFile 
	integer(int32) :: SpaceDim, ElemNodNum, NumOfMatPara, NumOfMaterial, NodeDOF,NodeTDOF,i
	integer(int32),optional,intent(in)  :: SimMode,ItrTol,Timestep,NodalDOF,NumOfMaterialPara
	real(real64),optional,intent(in) :: Tol
	type(IO_) :: file
	type(String_) :: line
	! bake creates a complete input file for a FEM analysis.
	! You can use build-in templates or your original template.
	! We prepare following build-in templates.
	! - FiniteDeform_ :: For 3-D Finite Deformation Analysis
	! - DiffusionEq_  :: For 3-D Diffusion Analysis
	! If you want to use your original format, please import 
	! your template file.
	! (This is being implemented.)
	if(present(template) )then
		if(template=="Original" .or. template=="original")then
			print *, "Please add an argument as 'templateFile = [Your_Template_File]'"
			! text-based finding is not good. Upper/lower cases >> global module to relate ID and 
			! INTEGER, PARAMETER :: TEMP_FINITE_DEFORM = 1000; call tissue%bake(template=TEMP_FINITE_DEFORM)
			! SELECT CASE( template ); CASE( TEMP_FINITE_DEFORM)
			! read line by line
			NumOfMatPara = input(default=3, option=NumOfMaterialPara)
			NodeDOF = input(default=1, option=NodalDOF)
			NodeTDOF = 1

			if(present(templateFile) )then
				call file%open(templateFile)
			
				do 
					line = file%readline()

					i = index(line%all, "NumOfMatPara")
					if(i/=0)then
						line%all =line%all(i+1:) 
						read(line%all,* ) NumOfMatPara
					endif

					i = index(line%all, "NodeDOF")
					if(i/=0)then
						line%all =line%all(i+1:) 
						read(line%all,* ) NodeDOF
					endif

					i = index(line%all, "NodeTDOF")
					if(i/=0)then
						line%all =line%all(i+1:) 
						read(line%all,* ) NodeTDOF
					endif

					if(file%EOF .eqv. .true.)then
						exit
					endif
				enddo
				call file%close()
			endif
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
			print *, "BakeFEMDomain == default (="
			call this%bake(template="Original", templateFile= templateFile,NodalDOF=NodalDOF,&
				NumOfMaterialPara=NumOfMaterialPara,Tol=Tol,SimMode=SimMode,ItrTol=ItrTol,Timestep=Timestep)
			return
		endif
	else
		call this%bake(template="Original", templateFile= templateFile,NodalDOF=NodalDOF,&
		NumOfMaterialPara=NumOfMaterialPara,Tol=Tol,SimMode=SimMode,ItrTol=ItrTol,Timestep=Timestep)
	endif

	! domain information
	this%Dtype="domain"
	this%SolverType=template
	this%NumOfDomain=1
	
	if(allocated(this%Mesh%SubMeshNodFromTo))then
		deallocate(this%Mesh%SubMeshNodFromTo)
	endif
	if(allocated(this%Mesh%SubMeshElemFromTo))then
		deallocate(this%Mesh%SubMeshElemFromTo)
	endif
	allocate(this%Mesh%SubMeshNodFromTo(this%NumOfDomain,3) )
	allocate(this%Mesh%SubMeshElemFromTo(this%NumOfDomain,3) )
	if(this%Mesh%empty() .eqv. .true. )then
		print *, "bakeFEMDomain :: Mesh is Empty!"
		return
	endif
	this%Mesh%ElemType=this%Mesh%GetElemType()
	! mesh information
	this%Mesh%SubMeshNodFromTo(1,1) = 1
	this%Mesh%SubMeshNodFromTo(1,2) = 1
	this%Mesh%SubMeshNodFromTo(1,3) = size(this%Mesh%NodCoord,1)
	this%Mesh%SubMeshElemFromTo(1,1) = 1
	this%Mesh%SubMeshElemFromTo(1,2) = 1
	this%Mesh%SubMeshElemFromTo(1,3) = size(this%Mesh%ElemNod,1)
	if(.not.allocated(this%Mesh%ElemMat) )then
		allocate(this%Mesh%ElemMat(size(this%Mesh%ElemNod,1) ) )
		this%Mesh%ElemMat(:)=1
	endif
	call showarraysize(this%Mesh%SubMeshNodFromTo)
	call showarraysize(this%Mesh%SubMeshElemFromTo)

	call this%bakeMaterials(NumOfMatPara=NumOfMatPara)
	call this%bakeDBoundaries(NodeDOF=NodeDOF)
	call this%bakeNBoundaries(NodeDOF=NodeDOF)
	call this%bakeTBoundaries(NodeDOF=NodeTDOF)

	call this%ControlPara%set(OptionalTol=Tol,&
	OptionalItrTol=ItrTol,&
	OptionalTimestep=Timestep,&
	OptionalSimMode=SimMode)

end subroutine bakeFEMDomain
! ##################################################

! ##################################################
subroutine bakeMaterialsFEMDomain(this,NumOfMatPara)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),optional,intent(in) :: NumOfMatPara
	integer(int32) :: i,j,k,l,n,m,NumOfMaterial,layer,in_num,NumOfLayer
	real(real64),allocatable :: matPara(:,:),info(:,:)
	integer(int32),allocatable :: key(:)
	type(Rectangle_) :: rect,mrect
	logical :: in_case
	real(real64) :: matparaval,coord(3),x_max(3),x_min(3)

	! get Num of Layer
	NumOfLayer=0
	if(.not. allocated(this%Materials) )then
		print *, "no materials found"
		return
	endif


	do i=1,size(this%Materials)
		if(associated(this%Materials(i)%materialp ) )then
			NumOfLayer=NumOfLayer+1
		else
			cycle
		endif
	enddo



	if(.not. allocated(this%Materials) )then
		print *, "No material is baked. All material IDs are 1 "
		if(.not.allocated(this%Mesh%ElemMat) )then
			allocate(this%Mesh%ElemMat(size(this%Mesh%ElemNod,1) ) )
			this%Mesh%ElemMat(:)=1
		endif
		stop "No material parameters are found."
		return
	else
		! total $NumOfLayer material parameters exist.
		! for all materials, resistrate material parameter and material IDs
		m=input(default=NumOfLayer,option=NumOfMatPara)
		
		allocate(rect%NodCoord(size(this%Mesh%ElemNod,2),size(this%Mesh%NodCoord,2)) )
		allocate(mrect%NodCoord(size(this%Mesh%ElemNod,2),size(this%Mesh%NodCoord,2)) )
		allocate(matPara(size(this%Mesh%ElemNod,1),m) )
		matPara(:,:) = 0.0d0
		do i=1,size(this%Mesh%ElemNod,1)
			! for each element
			
			! input rectangler
			do j=1,size(this%Mesh%ElemNod,2)
				rect%NodCoord(j,:)=this%Mesh%NodCoord(this%Mesh%ElemNod(i,j),:)
			enddo

			! for all materials, check material parameters
			do j=1,size(this%Materials)
				if(associated(this%Materials(j)%materialp) )then
					do k=1, size(this%Materials(j)%materialp%Mesh%ElemNod,1)
						! for each zones, check in-out
						! import nodal coordinate
						do l=1,size(this%Materials(j)%materialp%Mesh%ElemNod,2)
							n=this%Materials(j)%materialp%Mesh%ElemNod(k,l)
							mrect%NodCoord(l,:)=this%Materials(j)%materialp%Mesh%NodCoord(n,:)
						enddo
						layer=this%Materials(j)%materialp%layer
						! check in-out
						if(rect%contact(mrect) .eqv. .true. )then
							! in
							matPara(i,layer)=this%Materials(j)%materialp%meshPara(k,1)
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

	call getKeyAndValue(Array=matPara,key=this%Mesh%ElemMat, info=this%MaterialProp%MatPara)
	!call showarray(this%Mesh%ElemMat,Name="test1.txt")
	!call showarray(this%MaterialProp%MatPara,Name="test2.txt")
end subroutine bakeMaterialsFEMDomain
! ##################################################

! ##################################################
subroutine bakeDBoundariesFEMDomain(this,NodeDOF)
	class(FEMDomain_),intent(inout) :: this
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
	if(.not. allocated(this%Boundaries) )then
		print *, "no Boundaries found"
		return
	endif

	DBCnum=NodeDOF

	if(.not. allocated(this%Boundaries) )then
		print *, "No Dirichlet Boundaries are imported."
		return
	endif

	NumOfLayer=0
	do i=1, size(this%Boundaries,1)
		if(associated(this%Boundaries(i)%Boundaryp ) )then
			if(this%Boundaries(i)%Boundaryp%Dbound%empty() .eqv. .false. )then
				NumOfLayer=NumOfLayer+1
			endif
		else
			cycle
		endif
	enddo
	print *, "Number of Layer for Dirichlet Boundary= ",NumOfLayer

	call this%initDBC(NumOfValPerNod=input(default=NumOfLayer,option=NodeDOF) )


	if(.not. allocated(this%Boundaries) )then
		print *, "No Dirichlet boundary is baked."
		return
	else
		! total $NumOfLayer Boundary Conditions exist.
		! for all Boundaries, resistrate material parameter and material IDs
		do i=1,size(this%Boundaries,1)
			! for each Layer
			if(associated(this%Boundaries(i)%Boundaryp ) )then
				if(this%Boundaries(i)%Boundaryp%DBound%empty() .eqv. .false. )then
					do j=1,size(this%Boundaries(i)%Boundaryp%DBound%ElemNod,1)
						! for each Zone
						xmin = minval( this%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,1) ) 
						xmax = maxval( this%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,1) ) 
						ymin = minval( this%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,2) )
						ymax = maxval( this%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,2) )
						zmin = minval( this%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,3) )
						zmax = maxval( this%Boundaries(i)%Boundaryp%DBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%DBound%ElemNod(j,:) ,3) )
						val = this%Boundaries(i)%Boundaryp%DBoundPara(j,1)
						call this%AddDBoundCondition(xmin=xmin,xmax=xmax,ymin=ymin,&
						ymax=ymax,zmin=zmin,zmax=zmax,val=val,&
						val_id=this%Boundaries(i)%Boundaryp%layer)
					enddo
				endif
			endif
		enddo
	endif

end subroutine bakeDBoundariesFEMDomain
! ##################################################


! ##################################################
subroutine bakeNBoundariesFEMDomain(this,NodeDOF)
	class(FEMDomain_),intent(inout) :: this
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
	if(.not. allocated(this%Boundaries) )then
		print *, "no Boundaries found"
		return
	endif

	DBCnum=NodeDOF

	if(.not. allocated(this%Boundaries) )then
		print *, "No Neumann Boundaries are imported."
		return
	endif

	NumOfLayer=0
	do i=1, size(this%Boundaries,1)
		if(associated(this%Boundaries(i)%Boundaryp ) )then
			if(this%Boundaries(i)%Boundaryp%Nbound%empty() .eqv. .false. )then
				NumOfLayer=NumOfLayer+1
			endif
		else
			cycle
		endif
	enddo
	print *, "Number of Layer for Neumann Boundary= ",NumOfLayer

	call this%initNBC(NumOfValPerNod=input(default=NumOfLayer,option=NodeDOF) )
	

	if(.not. allocated(this%Boundaries) )then
		print *, "No Neumann boundary is baked."
		return
	else
		! total $NumOfLayer Boundary Conditions exist.
		! for all Boundaries, resistrate material parameter and material IDs
		do i=1,size(this%Boundaries,1)
			! for each Layer
			if(associated(this%Boundaries(i)%Boundaryp ) )then
				if(this%Boundaries(i)%Boundaryp%NBound%empty() .eqv. .false. )then
					do j=1,size(this%Boundaries(i)%Boundaryp%NBound%ElemNod,1)
						! for each Zone
						xmin = minval( this%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,1) ) 
						xmax = maxval( this%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,1) ) 
						ymin = minval( this%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,2) )
						ymax = maxval( this%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,2) )
						zmin = minval( this%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,3) )
						zmax = maxval( this%Boundaries(i)%Boundaryp%NBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%NBound%ElemNod(j,:) ,3) )
						val = this%Boundaries(i)%Boundaryp%NBoundPara(j,1)
						call this%AddNBoundCondition(xmin=xmin,xmax=xmax,ymin=ymin,&
						ymax=ymax,zmin=zmin,zmax=zmax,val=val,&
						val_id=this%Boundaries(i)%Boundaryp%layer)
					enddo
				endif
			endif
		enddo
	endif

end subroutine bakeNBoundariesFEMDomain
! ##################################################


! ##################################################
subroutine bakeTBoundariesFEMDomain(this,NodeDOF)
	class(FEMDomain_),intent(inout) :: this
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
	if(.not. allocated(this%Boundaries) )then
		print *, "no Boundaries found"
		return
	endif

	DBCnum=NodeDOF

	if(.not. allocated(this%Boundaries) )then
		print *, "No Time Boundaries are imported."
		return
	endif

	NumOfLayer=0
	do i=1, size(this%Boundaries,1)
		if(associated(this%Boundaries(i)%Boundaryp ) )then
			if(this%Boundaries(i)%Boundaryp%Tbound%empty() .eqv. .false. )then
				NumOfLayer=NumOfLayer+1
			endif
		else
			cycle
		endif
	enddo
	print *, "Number of Layer for Time Boundary= ",NumOfLayer

	call this%initTBC(NumOfValPerNod=input(default=NumOfLayer,option=NodeDOF) )


	if(.not. allocated(this%Boundaries) )then
		print *, "No Time boundary is baked."
		return
	else
		! total $NumOfLayer Boundary Conditions exist.
		! for all Boundaries, resistrate material parameter and material IDs
		do i=1,size(this%Boundaries,1)
			! for each Layer
			if(associated(this%Boundaries(i)%Boundaryp ) )then
				if(this%Boundaries(i)%Boundaryp%TBound%empty() .eqv. .false. )then
					do j=1,size(this%Boundaries(i)%Boundaryp%TBound%ElemNod,1)
						! for each Zone
						xmin = minval( this%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,1) ) 
						xmax = maxval( this%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,1) ) 
						ymin = minval( this%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,2) )
						ymax = maxval( this%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,2) )
						zmin = minval( this%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,3) )
						zmax = maxval( this%Boundaries(i)%Boundaryp%TBound%NodCoord&
						(this%Boundaries(i)%Boundaryp%TBound%ElemNod(j,:) ,3) )
						val = this%Boundaries(i)%Boundaryp%TBoundPara(j,1)
						call this%AddTBoundCondition(xmin=xmin,xmax=xmax,ymin=ymin,&
						ymax=ymax,zmin=zmin,zmax=zmax,val=val,&
						val_id=this%Boundaries(i)%Boundaryp%layer)
					enddo
				endif
			endif
		enddo
	endif

end subroutine bakeTBoundariesFEMDomain
! ##################################################





! ##################################################
subroutine ImportMaterialsFEMDomain(this,Material,NumberOfMaterials,MaterialID)
	class(FEMDomain_),intent(inout) :: this
	type(MaterialProp_),target,intent(in) :: Material
	integer(int32), optional,intent(in) :: NumberOfMaterials,MaterialID
	integer(int32) :: n,i


	if(.not.allocated(this%Materials) )then
		n=input(default=30,option=NumberOfMaterials)
		allocate(this%Materials(n))
		this%NumberOfMaterials = 0
		do i=1,n
			nullify(this%Materials(i)%materialp)
		enddo
	endif

	if(present(MaterialID) )then
		if(MaterialID > size(this%Materials) )then
			print *, "ERROR :: ImportMaterialsFEMDomain >> requested MaterialID is grater than the size of stack"
			print *, "Stack size is ",size(this%Materials), " , and your request is ",MaterialID
			return
		endif
		if(MaterialID > this%NumberOfMaterials)then
			print *, "ERROR :: ImportMaterialsFEMDomain >> requested MaterialID is grater than the Last ID"
			print *, "The last ID is ",this%NumberOfMaterials+1, " , and your request is ",MaterialID
			print *, "Hence, your request ",MaterialID, " is accepted as the ID of ",this%NumberOfMaterials+1
			this%NumberOfMaterials=this%NumberOfMaterials+1
			this%Materials(this%NumberOfMaterials)%Materialp => Material
			print *, "Now, number of Material conditions is ",this%NumberOfMaterials
			return
		endif
		if( associated(this%Materials(MaterialID)%Materialp) )then
			print *, "Material ID :: ", MaterialID, " is overwritten."
			nullify(this%Materials(MaterialID)%Materialp )
		endif
		this%Materials(MaterialID)%Materialp => Material
		return
	endif

	this%NumberOfMaterials=this%NumberOfMaterials+1

	this%Materials(this%NumberOfMaterials)%Materialp => Material

	print *, "Now, number of Material conditions is ",this%NumberOfMaterials

end subroutine ImportMaterialsFEMDomain
! ##################################################




! ##################################################
subroutine showMaterialsFEMDomain(this,Name)
	class(FEMDomain_),intent(inout) :: this
	character(*),optional,intent(in)::Name
	integer(int32) :: i

	if(present(Name) )then
		print *, "Domain Name is :: ", name
	endif

	if(.not. allocated(this%Materials) )then
		print *, "No boundary is set."
	else
		do i=1,this%NumberOfMaterials
			print *, "Layer :: ",this%Materials(i)%Materialp%Layer,"Material ::",i," => ",&
				associated(this%Materials(i)%Materialp)
		enddo
	endif
end subroutine showMaterialsFEMDomain
! ##################################################


! ##################################################
subroutine removeMaterialsFEMDomain(this,Name,BoundaryID)
	class(FEMDomain_),intent(inout) :: this
	character(*),optional,intent(in)::Name
	integer(int32) :: i
	integer(int32),optional,intent(in) ::BoundaryID

	if(present(Name) )then
		print *, "Domain Name is :: ", name
	endif

	if(.not. allocated(this%Materials) )then
		print *, "No boundary is set."
	else
		if(present(BoundaryID))then
			nullify(this%Materials(BoundaryID)%Materialp)
		else
			do i=1,this%NumberOfMaterials
				nullify(this%Materials(i)%Materialp)
			enddo
		endif
	endif
	call this%showMaterials(Name)


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
	
	if( obj1%name == "NoName" )then
		obj1%name=random%name() 
		print *, "Caution !!! object #1 is not named. New name is "//obj1%name
	endif
	if( obj2%name == "NoName" )then
		obj2%name=random%name() 
		print *, "Caution !!! object #2 is not named. New name is "//obj2%name
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
		obj1%Boundary%ContactNameList(n+1)%name=obj2%name
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

	do i=1,size(buffer,1)
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

		 
		!obj1%Boundary%MasterSegment(n,:)=domain_id 
		!obj1%Boundary%SlaveSegment( n,:)=domain_id 
		

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

subroutine getSurfaceFEMDomain(this)
	class(FEMDomain_),intent(inout) :: this
	
	call this%mesh%getSurface()

end subroutine
! ##################################################


subroutine getVerticesFEMDomain(this,vertices,vertexIDs)
	class(FEMDomain_),intent(inout) :: this
	real(real64),allocatable,intent(inout) :: vertices(:)
	integer(int32),allocatable,intent(inout) :: vertexIDs(:)
	
	call this%mesh%getVertices(vertices,vertexIDs)

end subroutine
! ##################################################

! ##################################################
recursive function getVolumeFEMDomain(this,elem) result(ret)
	class(FEMDomain_),intent(in) :: this
	type(ShapeFunction_) :: sf
	integer(int32),optional,intent(in) :: elem
	real(real64) :: ret
	integer(int32) :: i,j,elemid

	if(present(elem) )then
		sf%ElemType=this%Mesh%GetElemType()
		call SetShapeFuncType(sf)
		i = elem
		ret = 0.0d0
		do j=1,sf%numOfGP
			call GetAllShapeFunc(sf,elem_id=i,nod_coord=this%Mesh%NodCoord,&
				elem_nod=this%Mesh%ElemNod,OptionalGpID=j)
			ret = ret + sf%detJ*((2.0d0)**this%nd())/dble(sf%numOfGP)
		enddo
	else
		! count all
		ret = 0.0d0
		do elemid=1,this%ne()
			ret = ret + this%getVolume(elem=elemid)	
		enddo
	endif
end function
! ##################################################

! ##################################################
function getJacobiMatrixFEMDomain(this,elem) result(ret)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: elem
	real(real64),allocatable :: ret(:,:)
	integer(int32) :: i,j


	this%ShapeFunction%ElemType=this%Mesh%GetElemType()
	call SetShapeFuncType(this%ShapeFunction)
	i = elem
	call GetAllShapeFunc(this%ShapeFunction,elem_id=i,nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=1)
	ret = this%ShapeFunction%Jmat

end function
! ##################################################


function getSingleFacetNodeIDFEMDomain(this,ElementID) result(facet)
	class(FEMDomain_),intent(in) :: this
	integer(int32),intent(in) :: ElementID
	integer(int32),allocatable :: facet(:,:)
	integer(int32) :: i, j

	if(this%nd()==3 .and. this%nne()==8 )then
		allocate(Facet(6,4) )
		Facet(1,1:4) = [4,3,2,1]
		Facet(2,1:4) = [1,2,6,5]
		Facet(3,1:4) = [2,3,7,6]
		Facet(4,1:4) = [3,4,8,7]
		Facet(5,1:4) = [4,1,5,8]
		Facet(6,1:4) = [5,6,7,8] 
	elseif(this%nd()==3 .and. this%nne()==4 )then
		allocate(Facet(4,3) )
		Facet(1,1:3) = [3,2,1]
		Facet(2,1:3) = [1,2,4]
		Facet(3,1:3) = [2,3,4]
		Facet(4,1:3) = [3,1,4]
	elseif(this%nd()==2 .and. this%nne()==4 )then
		allocate(Facet(4,2) )
		Facet(1,1:2) = [1,2]
		Facet(2,1:2) = [2,3]
		Facet(3,1:2) = [3,4]
		Facet(4,1:2) = [4,1]
	else
		print *, "ERROR :: getSingleFacetNodeIDFEMDomain >> "
		print *, "No implementation for such element type"
		print *, "Please send issue on the Github."
		stop
	endif
	

	do i=1,size(Facet,1)
		do j=1,size(Facet,2)
			Facet(i,j) = this%mesh%elemnod(ElementID, Facet(i,j) )
		enddo
	enddo

end function

subroutine x3dFEMDomain(this,name)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	type(IO_) :: f
	integer(int32),allocatable ::Facet(:,:)
	integer(int32) :: ElementID,FacetID,PointID

	! export as X3D
	call f%open(name + ".x3d","w")
	!write(f%fh,*) '<X3D version="3.0" profile="Immersive" xmlns:xsd="http://www.w3.org/2001/XMLSchema-instance" xsd:noNamespaceSchemaLocation="http://www.web3d.org/specifications/x3d-3.0.xsd">'
	write(f%fh,*) '<X3D version="3.0">'
	write(f%fh,*) '<Scene>'
	write(f%fh,*) '<Shape>'
	write(f%fh,*) '<IndexedFaceSet'
	write(f%fh,*) 'solid="false"'
	write(f%fh,*) 'coordIndex="'
	
	do ElementID = 1,this%ne()
		facet = this%getSingleFacetNodeID(ElementID=ElementID)
		facet(:,:) = facet(:,:) -1
		do FacetID = 1, size(Facet,1)
			write(f%fh,*) Facet(FacetID,:),"-1"
		enddo
	enddo
	
	write(f%fh,*) '">'

	write(f%fh,*) '<Coordinate DEF="coords_ME_Cube" point="'

	do PointID = 1,this%nn()
		write(f%fh,*) this%mesh%nodcoord(PointID,:)
	enddo

	write(f%fh,*) '"/>'
	write(f%fh,*) '</IndexedFaceSet>'



	write(f%fh,*) '</Shape>'
	write(f%fh,*) '</Scene>'
	write(f%fh,*) '</X3D>'


end subroutine
! ##################################################
subroutine vtk_MPI_FEMDOmain(this,name,num_division,remove)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: num_division
	character(*),intent(in) :: name
	logical,optional,intent(in) :: remove
	type(FEMDomain_),allocatable :: domains(:)
	integer(int32) :: i,j,nn
	type(IO_) :: f


	nn = this%nn()
	domains = this%divide(n=num_division)

	
	if(present(remove))then
		if(remove)then
			call this%remove()
		endif
	endif

	do i=1, num_division
		call domains(i)%vtk(name+"_"+zfill(i-1,6 ))
	enddo

	

	do i=1, num_division
		! domain connectivity
		call f%open(name+"_"+zfill(i-1,6 )+".csv","w")
		call f%write("# number of global node, number of local node, number of shared node,&
			number of division ")
		
		call f%write(str(nn ) + " ,"+ str( domains(i)%nn() )  + " ,"&
			+ str( size(domains(i)%mpi_shared_node_info,1) )  + " ," &
			+ str( num_division ) )
		call f%write("# local node Idx, global node Idx ")
		if(allocated(domains(i)%mpi_global_node_idx) )then
			do j=1,size(domains(i)%mpi_global_node_idx)
				call f%write( str(j) + " ,"+str(domains(i)%mpi_global_node_idx(j) )   )
			enddo
		endif
		call f%write("# local node Idx, domain Idx, shared local node Idx ")
		if( allocated(domains(i)%mpi_shared_node_info) )then
			do j=1,size(domains(i)%mpi_shared_node_info,1)
				call f%write( str(domains(i)%mpi_shared_node_info(j,1) ) &
					+ " ,"+str(domains(i)%mpi_shared_node_info(j,2) )  &
					+ " ,"+str(domains(i)%mpi_shared_node_info(j,3) )    )
			enddo
		endif
		call f%close()
	enddo

	return

end subroutine



! ##################################################
recursive subroutine vtkFEMDomain(this,name,scalar,vector,tensor,field,ElementType,NodeList,debug,displacement,only_field)
	class(FEMDomain_),intent(inout) :: this
	type(FEMDomain_) :: mini_obj
	character(*),intent(in) :: name
	character(*),optional,intent(in) :: field
	real(real64),optional,intent(in) :: scalar(:),vector(:,:),tensor(:,:,:),displacement(:)
	integer(int32),optional,intent(in) :: ElementType,Nodelist(:)
	character(len=:),allocatable :: point_scalars,point_vectors,point_tensors,cell_scalars,cell_vectors,cell_tensors
	logical,optional,intent(in) :: only_field
	type(IO_) :: f
	integer(int32) ::i,dim_num(3),j,VTK_CELL_TYPE,num_node,k,n
	logical,optional,intent(in) :: debug
	
	if(present(displacement) )then
		call this%deform(disp=displacement)
		call this%vtk(name=name,scalar=scalar,vector=vector,tensor=tensor,&
			field=field,ElementType=ElementType,NodeList=NodeList,debug=debug)
		call this%deform(disp=-displacement)
		return
	endif

	if(this%nd()==2 )then
		mini_obj = this
		call mini_obj%mesh%convert2Dto3D(division=1,thickness=dble(1.0e-8))
		call mini_obj%vtk(name=name,scalar=scalar,vector=vector,tensor=tensor,&
			field=field,ElementType=ElementType,NodeList=NodeList,debug=debug)
		return
	endif

	if(present(NodeList))then
		n = size(NodeList,1)
		mini_obj%mesh%nodcoord = zeros(n,this%nd())
		mini_obj%mesh%elemNod = zeros(n,this%nne())
		do i=1,n
			mini_obj%mesh%nodcoord(i,: ) = this%mesh%nodcoord( NodeList(i),: ) 
		enddo
		do i=1,n
			mini_obj%mesh%elemNod(i,:) = i
		enddo
		call mini_obj%vtk(name=name)
		return
	endif


	if(present(field) )then
		point_scalars = field
		point_vectors = field
		point_tensors = field
		cell_scalars = field
		cell_vectors = field
		cell_tensors = field
	else
		point_scalars = "point_scalars"
		point_vectors = "point_vectors"
		point_tensors = "point_tensors"
		cell_scalars  = "cell_scalars"
		cell_vectors  = "cell_vectors"
		cell_tensors  = "cell_tensors"
	endif

	if(this%mesh%empty() .eqv. .true.)then
		print *, "ERROR :: vtkFEMDomain >> this%mesh%empty() .eqv. .true., nothing exported"
		return
	endif

	
	if( .not.allocated(this%mesh%elemnod) )then
		VTK_CELL_TYPE=1 ! point
	elseif(this%nd()==3 .and. this%nne()==2 )then
		VTK_CELL_TYPE=1 ! point
	elseif(this%nd()==2 .and. this%nne()==3 )then
		VTK_CELL_TYPE=5 ! triangle
	elseif(this%nd()==2 .and. this%nne()==4 )then
		VTK_CELL_TYPE=9 ! square
	elseif(this%nd()==3 .and. this%nne()==4 )then
		VTK_CELL_TYPE=10 ! 4-node triangle
	elseif(this%nd()==3 .and. this%nne()==8 )then
		VTK_CELL_TYPE=12 ! 8-node box
	else
		VTK_CELL_TYPE = elementType2VTKCellType(this%mesh%getElementType())
		if(VTK_CELL_TYPE==-1)then
			print *, "VTKFEMDomain >> ERROR :: Nothing is exported."
			return
		endif
	endif
	
	

	if(present(ElementType) )then
		VTK_CELL_TYPE = ElementType
	endif

	!call displayFEMDomain(this,path="./",name=name,extention=".vtk")
	if(index(name,".vtk")/=0 .or. index(name,".VTK")/=0 )then
		call f%open(name,'w')
	else
		call f%open(name//".vtk",'w')
	endif
	
	call f%write("# vtk DataFile Version 2.0")
	call f%write(name)
	call f%write("ASCII")
	call f%write("DATASET UNSTRUCTURED_GRID")
	call f%write("POINTS "//str( this%nn() )//" float")
	do i=1,this%nn()
		do j=1, this%nd()-1
			write(f%fh,'(A)',advance="no") str(this%mesh%nodcoord(i,j))//" "
		enddo
		write(f%fh,'(A)',advance="yes") str(this%mesh%nodcoord(i,this%nd() ))
	enddo


	call f%write("CELLS "//str(this%ne())//" "//str(this%ne()* (this%nne()+1) ))
	do i=1, this%ne()
		num_node = this%nne() 
		if(present(ElementType) )then
			if(ElementType==1)then
				num_node = 1
			elseif(ElementType==5)then
				num_node = 3
			elseif(ElementType==9)then
				num_node = 4
			elseif(ElementType==10)then
				num_node = 4
			elseif(ElementType==12)then
				num_node = 8
			elseif(ElementType==13)then
				num_node = 6
			elseif(ElementType==14)then
				num_node = 4
			endif
		endif

		write(f%fh,'(A)',advance="no") str(num_node ) // " "
		
		do j=1, num_node-1
			write(f%fh,'(A)',advance="no") str(this%mesh%elemnod(i,j)-1)//" "
		enddo
		write(f%fh,'(A)',advance="yes") str(this%mesh%elemnod(i, num_node )-1)
	enddo
	
	call f%write("CELL_TYPES "//str(this%ne() ) )
	do i=1, this%ne()
		call f%write(str(VTK_CELL_TYPE) )
	enddo

	! if scalar or vector exists..
	if(present(scalar) )then
		if(size(scalar)==this%nn()  )then
			call f%write("POINT_DATA "//str(this%nn() ) )
			call f%write("SCALARS "//point_scalars//" float")
			call f%write("LOOKUP_TABLE default")
			do i=1,this%nn()
				write(f%fh,*) real(scalar(i))
			enddo
		elseif(size(scalar)==this%ne()  )then
			call f%write("CELL_DATA "//str(this%ne() ) )
			call f%write("SCALARS "//cell_scalars//" float")
			call f%write("LOOKUP_TABLE default")
			do i=1,this%ne()
				write(f%fh,*) real(scalar(i))
			enddo
		else
			call print("vtkFEMDOmain ERROR ::size(scalar) should be this%nn() or this%ne()  ")
			call print("size(scalar)="//str(size(scalar))//" <> this%nn() = "//str(this%nn() )//&
				" <> this%ne() = "//str(this%ne() ) )
			call f%close()
			return
		endif
	endif
	
	if(present(vector) )then
		if(size(vector,1)==this%nn()  )then
			call f%write("POINT_DATA "//str(this%nn() ) )
			call f%write("VECTORS "//point_vectors//" float")
			do i=1,this%nn()
				do j=1,size(vector,2)-1
					write(f%fh,'(A)',advance="no") str(vector(i,j) )//" "
				enddo
				write(f%fh,'(A)',advance="yes") str(vector(i, size(vector,2) ) )
			enddo
		elseif(size(vector,1)==this%ne()  )then
			call f%write("CELL_DATA "//str(this%ne() ) )
			call f%write("VECTORS "//cell_vectors//" float")
			do i=1,this%ne()
				do j=1,size(vector,2)-1
					write(f%fh,'(A)',advance="no") str(vector(i,j) )//" "
				enddo
				write(f%fh,'(A)',advance="yes") str(vector(i, size(vector,2) ) )
			enddo
		else
			call print("vtkFEMDOmain ERROR ::size(vector,1) sould be this%nn()   ")
			call print("size(vector,1)="//str(size(vector,1))//" and this%nn() = "//str(this%nn() ) )
			call f%close()
			return
		endif
	endif	



	if(present(tensor) )then
		if(size(tensor,1)==this%nn()  )then
			call f%write("POINT_DATA "//str(this%nn() ) )
			call f%write("TENSORS "//point_tensors//" float")
			do i=1,this%nn()
				do j=1,size(tensor,2)
					do k=1,size(tensor,3)-1
						write(f%fh,'(A)',advance="no") str(tensor(i,j,k) )//" "
					enddo
					write(f%fh,'(A)',advance="yes") str(tensor(i, j,size(tensor,3) ) )
				enddo
				
			enddo
		elseif(size(tensor,1)==this%ne()  )then
			call f%write("CELL_DATA "//str(this%ne() ) )
			call f%write("TENSORS "//cell_tensors//" float")
			do i=1,this%ne()
				do j=1,size(tensor,2)
					do k=1,size(tensor,3)-1
						write(f%fh,'(A)',advance="no") str(tensor(i,j,k) )//" "
					enddo
					write(f%fh,'(A)',advance="yes") str(tensor(i, j,size(tensor,3) ) )
				!do j=1,size(tensor,2)-1
				!	write(f%fh,'(A)',advance="no") str(tensor(i,j) )//" "
				!enddo
				!write(f%fh,'(A)',advance="yes") str(tensor(i, size(tensor,2) ) )
				enddo
			enddo
		else
			call print("vtkFEMDOmain ERROR ::size(tensor,1) sould be this%nn()   ")
			call print("size(tensor,1)="//str(size(tensor,1))//" and this%nn() = "//str(this%nn() ) )
			call f%close()
			return
		endif
	endif	

	if(present(debug) )then
		if(debug)then
			print *, name//".vtk is exported." 
		endif
	endif

	call f%close()

end subroutine
! ##################################################

! ##################################################
subroutine plyFEMDomain(this,name,NodeList)
	class(FEMDomain_),intent(inout) :: this
	type(FEMDomain_) :: mini_obj
	character(*),intent(in) :: name
	type(IO_) :: f
	integer(int32),optional,intent(in) :: NodeList(:)
	integer(int32) ::i,n

	if(this%mesh%empty() .eqv. .true.)then
		print *, "ERROR :: vtkFEMDomain >> this%mesh%empty() .eqv. .true., nothing exported"
		return
	endif

	if(present(NodeList))then
		n = size(NodeList,1)
		mini_obj%mesh%nodcoord = zeros(n,this%nd())
		mini_obj%mesh%elemNod = zeros(n,this%nne())
		do i=1,n
			mini_obj%mesh%nodcoord(i,: ) = this%mesh%nodcoord( NodeList(i),: ) 
		enddo
		do i=1,n
			mini_obj%mesh%elemNod(i,:) = i
		enddo
		call mini_obj%stl(name=name)
		return
	endif

	call displayFEMDomain(this,path="./",name=name,extention=".ply")
	return


end subroutine
! ##################################################

subroutine stlFEMDomain(this,name,NodeList)
	class(FEMDomain_),intent(inout) :: this
	type(IO_) :: f
	type(FEMDomain_) :: mini_obj
	integer(int32),optional,intent(in) :: NodeList(:)
	character(*),intent(in) :: name
	integer(int32) :: i,j,n

	if(present(NodeList))then
		n = size(NodeList,1)
		mini_obj%mesh%nodcoord = zeros(n,this%nd())
		mini_obj%mesh%elemNod = zeros(n,this%nne())
		do i=1,n
			mini_obj%mesh%nodcoord(i,: ) = this%mesh%nodcoord( NodeList(i),: ) 
		enddo
		do i=1,n
			mini_obj%mesh%elemNod(i,:) = i
		enddo
		call mini_obj%stl(name=name)
		return
	endif

	!call f%open(name//".stl")
	call ExportFEMDomainAsSTL(this,MeshDimension=size(this%mesh%Nodcoord,2),FileName=name)
	!call f%close()
end subroutine
! ##################################################

! ##################################################
subroutine objFEMDomain(this,name)
	class(FEMDomain_),intent(inout) :: this
	type(IO_) :: f
	character(*),intent(in) :: name
	integer(int32) :: i,j,k

	call f%open(name//".obj")
	do i=1,this%nn()
		write(f%fh,'(A)',advance="no") "v "
		do j=1,size(this%mesh%Nodcoord,2)-1
			write(f%fh,'(A)',advance="no") str(this%mesh%Nodcoord(i,j) )//" "
		enddo
		write(f%fh,'(A)',advance="yes") str(this%mesh%Nodcoord(i, size(this%mesh%Nodcoord,2) ) )
	enddo
	


	call f%close()
end subroutine
! ##################################################

! ##################################################
subroutine jsonFEMDomain(this,name,fh,endl)
	class(FEMDomain_),intent(in) :: this
	type(IO_) :: f
	integer(int32),optional,intent(in) :: fh
	character(*),optional,intent(in) :: name
	character(:),allocatable :: fname
	
	integer(int32) :: fileid
    logical,optional,intent(in) :: endl
	
	! export JSON file
	if(present(name)  )then
		if(present(fh) )then
			![ok] name
			![ok] file handle
			!append
			fileid=fh
			fname=name
		else
			call f%open(name)
			fileid=f%fh
			fname=name
			![ok] name
			![--] file handle
			! > create new file with Name=name
		endif
	else
		if(present(fh) )then
			fileid=fh	
			fname="untitled"
			![--] name
			![ok] file handle
			!append
		else
			![--] name
			![--] file handle
			!append
			call f%open(name="untitled.json")
			fileid=f%fh
			fname="untitled"
		endif
	endif


	write(fileid,'(A)') '{'
	if(present(name) )then
		write(fileid,*) '"name": "'//name//'",'
	endif
	write(fileid,*) '"type": "femdomain",'

	call this%mesh%json(fh=fileid)
	
	

	if(present(endl) )then
		if(endl .eqv. .false.)then
            write(fileid,*) '},'
        else
            write(fileid,*) '}'
        endif
    else
		write(fileid,*) '}'
    endif

	if(present(name)  )then
		if(present(fh) )then
			fileid=fh	
		else
			call f%close()
			fileid=f%fh
		endif
	else
		if(present(fh) )then
			fileid=fh	
		else
			call f%close()
			fileid=f%fh
		endif
	endif


end subroutine
! ##############################################
subroutine readFEMDomain(this,name,DimNum,ElementType)
	class(FEMDomain_) ,intent(inout) :: this
	character(*),intent(in) :: name
	character(:),allocatable :: line
	integeR(int32),allocatable :: elemnod(:,:),node_list(:),element_list(:),g_node_list(:),cell_types(:)
	integer(int32),optional,intent(in) :: DimNum,ElementType
	logical :: ret=.false.
	real(real64) :: x(3)
	real(real64),allocatable :: nodcoord(:,:)
	integer(int32) :: node_num,elem_num,i,j,id,itr,n,m,num_node_new,num_node,num_entity
	integer(int32) :: num_dim, num_c_node,nne,node_id
	type(IO_) :: f

	if(index(name,".vtk")/=0 )then
		call this%ImportVTKFile(name=name)
		return
	endif
	
	if(index(name,"json")/=0 )then
		call f%open(name )
		
		! json読み取ります

		call f%close()	
		ret = .true.
	endif

	
	if(index(name,"msh")/=0 )then
		call f%open(name,"r" )
		! get nodal coordinate
		! For MSH 4.1
		if(.not. present(DimNum) )then
			print *, "ERROR :: readFEMDomain >> DimNum should be 2 or 3"
			stop
		endif
		do 
			line = f%readline()
			if(f%EOF) exit
			if( index(line,"$Nodes")/=0 )then
				line = f%readline()
				read(line,*) num_entity, num_node, n,m
				allocate(g_node_list(num_node) )
				g_node_list(:) = 0
				allocate(node_list(num_node) )
				this%mesh%nodcoord = zeros(num_node, 3)
				node_id=0
				do
					line = f%readline()
					read(line,*) num_dim, num_c_node,n,m
					if(num_dim==DimNum)then
						! 2-D mesh"
						
						do i=1, m
							line = f%readline()
							read(line,*) node_list(i)
							print *, node_list(i)
							g_node_list( node_list(i) ) = node_list(i)
						enddo
						do i=1, m
							line = f%readline()
							node_id = node_id+1
							read(line,*) this%mesh%nodcoord(node_id,1:3)
						enddo
						exit
					else
						! ignore
						line = f%readline()
						read(line,*) n
						g_node_list( n ) = n
						line = f%readline()
						node_id = node_id+1
						read(line,*) this%mesh%nodcoord(n,1:3)
					endif
				enddo
			endif

			if(index(line,"$Elements")/=0 )then
				line = f%readline()
				read(line,*) num_entity, num_node, n,m
				do
					line = f%readline()
					read(line,*) num_dim, num_c_node,n,m
					if(num_dim==DimNum)then
						! 2-D mesh"
						allocate(element_list(m) )
						!defines the geometrical type of the n-th element:
						!
						!1
						!2-node line.
						!
						!2
						!3-node triangle.
						!
						!3
						!4-node quadrangle.
						!
						!4
						!4-node tetrahedron.
						!
						!5
						!8-node hexahedron.
						!
						!6
						!6-node prism.
						!
						!7
						!5-node pyramid.
						if(n==1)then
							nne=2
						elseif(n==2)then
							nne=3
						elseif(n==3)then
							nne=4
						elseif(n==4)then
							nne=4
						elseif(n==5)then
							nne=8
						elseif(n==6)then
							nne=6
						elseif(n==7)then
							nne=5
						else
							print *, "[CAUTION] ReadFEMDomain >> No such elemtype as",n
							exit	
						endif
						allocate(this%mesh%elemnod(m, nne))
						do i=1, m
							line = f%readline()
							print *, line
							read(line,*) element_list(i),this%mesh%elemnod(i,1:)
						enddo
						exit
					else
						! ignore
						do i=1,m
							line = f%readline()
						enddo
					endif
				enddo
				! got nodcoord & elemnod
				
				do i=1,size(this%mesh%elemnod,1)
					do j=1,size(this%mesh%elemnod,2)
						m = g_node_list( this%mesh%elemnod(i,j) )
						if(m==0)then
							print *, g_node_list(845:)
							print *, "[ERROR] ReadFEMDomain >> this%mesh%elemnod(i,j) = m",i,j,this%mesh%elemnod(i,j)
							stop 
						else
							this%mesh%elemnod(i,j) = m
						endif
					enddo
				enddo
				
			endif
		enddo
		call f%close()	
		ret = .true.
		print *, g_node_list
		return
	endif
	
	if(index(name,"vtk")/=0 )then
		itr=0
		call f%open(name,"r" )
		
		! msh読み取ります
		elem_num=0
		do
			line = f%readline()
			if(f%EOF) then
				! post processing
				if(present(ElementType) )then
					do i=1,size(cell_types)
						if(cell_types(i) /= ElementType)then
							cell_types(i) = -1
						endif
					enddo
					call this%killElement(blacklist=cell_types,flag=-1)		
				endif
				this%mesh%elemnod = this%mesh%elemnod + 1
				return
			endif

			if(index(line, "POINTS")/=0 )then

				n = index(line,"POINTS")
				read(line(n+6:),* ) node_num
				allocate(node_list(node_num) )
				node_list(:) = 0
				this%mesh%nodcoord = zeros(node_num,3)
				do i=1,node_num
					line = f%readline()
					read(line,*) this%mesh%nodcoord(i,:) 
				enddo
			endif

			if(index(line, "CELLS")/=0 )then
				n = index(line,"CELLS")
				read(line(n+5:),* ) elem_num
				if(allocated(this%mesh%elemnod)) deallocate(this%mesh%elemnod) 
				allocate(this%mesh%elemnod(elem_num,8))
				this%mesh%ElemNod(:,:) = 0
				j=0
				do i=1,elem_num
					line = f%readline()
					j = j + 1
					read(line,*) m,this%mesh%elemnod(j,1:m) 
				enddo
			
!				elemnod = this%mesh%elemnod
!				deallocate(this%mesh%elemnod)
!				allocate(this%mesh%elemnod(elem_num,4))
!				elem_num=0
!				do i=1,this%ne()
!					if(elemnod(i,1)/=0 )then
!						elem_num=elem_num+1
!						this%mesh%elemnod(elem_num,:) = elemnod(i,:)
!					endif
!				enddo
!				this%mesh%elemnod(:,:) = this%mesh%elemnod(:,:) + 1
!				! 要素の節点番号を振り直す。
!				do i=1,size(this%mesh%elemnod,1)
!					do j=1,size(this%mesh%elemnod,2)
!						node_list( this%mesh%elemnod(i,j) ) = 1
!					enddo
!				enddo
!				j=0
!				do i=1,size(node_list)
!					if(node_list(i)==1 )then
!						j=j+1
!						node_list(i) = j
!					endif	
!				enddo
!				num_node_new = j
!
!				! new node-id
!				do i=1,size(this%mesh%elemnod,1)
!					do j=1,size(this%mesh%elemnod,2)
!						this%mesh%elemnod(i,j) = node_list( this%mesh%elemnod(i,j) )
!					enddo
!				enddo

				! remove un-associated nodes
				!nodcoord = this%mesh%nodcoord
				!this%mesh%nodcoord = zeros(num_node_new,3) 
				!do i=1, size(node_list)
				!	j = node_list(i)
				!	if(j == 0)then
				!		cycle
				!	else
				!		this%mesh%nodcoord(node_list(i) ,:   ) = nodcoord(i,:)
				!	endif
				!enddo

			endif

			if(index(line, "CELL_TYPES")/=0 )then
				n = index(line, "CELL_TYPES")
				read(line(n+10:),* ) elem_num
				allocate(cell_types(elem_num) )
				do i=1,elem_num
					line = f%readline()
					read(line,*) cell_types(i)
				enddo
			endif
			
		enddo



		call f%close()	
		ret = .true.
		return
	endif

	if(ret .eqv. .false.)then
		print *, "ERROR >> readFEMDomain >> not such file as ",name
		return
	endif

end subroutine
! ##############################################



! ##############################################
subroutine addLayerFEMDomain(this,name,attribute,datastyle,vectorrank,tensorrank1,tensorrank2)
	class(FEMDomain_),intent(inout) :: this
	type(PhysicalField_),allocatable :: pfa(:)
	character(*),intent(in) :: attribute ! should be NODAL, ELEMENTAL, or GAUSSPOINT
	character(*),intent(in) :: datastyle ! should be SCALAR, VECTOR, or TENSOR
	character(*),intent(in) :: name
	integer,optional,intent(in) :: vectorrank,tensorrank1,tensorrank2
	integer(int32) :: datasize, datadimension,vector_rank,tensor_rank1,tensor_rank2,i


	vector_rank = input(default=3,option=vectorrank)
	tensor_rank1 = input(default=3,option=tensorrank1)
	tensor_rank2 = input(default=3,option=tensorrank2)

	if(.not.allocated(this % PhysicalField) ) then
		allocate(this % PhysicalField(100)) ! 100 layer as default
		this%numoflayer=0
	endif
	this%numoflayer=this%numoflayer+1
	
	if(this%numoflayer>size(this % PhysicalField) )then
		pfa = this%PhysicalField
		deallocate(this%PhysicalField)
		allocate(this%PhysicalField(size(pfa)*100 ) )
		do i=1,size(this%physicalfield)
		this%PhysicalField(i)%name = "untitled"
		enddo
		this%PhysicalField(1:size(pfa))=pfa(:)
	endif


	this % PhysicalField(this%numoflayer) % name   = name
	if(this%mesh%empty() .eqv. .true. )then
		print *, "ERROR >> addLayerFEMDomain >> mesh should be defined preliminary."
		return
	endif

	datasize=0
	select case( attribute)
		case ("Nodal","NODAL","node-wize","Node-Wize","NODEWIZE","Node","node")
			datasize=size(this%mesh%nodcoord,1)
			this%PhysicalField(this%numoflayer) %attribute = 1
		case ("Elemental","ELEMENTAL","element-wize","Element-Wize","ELEMENTWIZE","Element","element")
			datasize=size(this%mesh%elemnod,1)
			this%PhysicalField(this%numoflayer) %attribute = 2
		case ("Gausspoint","GAUSSPOINT","gausspoint-wize","GaussPoint-Wize","GAUSSPOINTWIZE")
			datasize=size(this%mesh%elemnod,1)
			this%PhysicalField(this%numoflayer) %attribute = 3
	end select

	select case( datastyle)
		case ("Scalar","SCALAR","scalar")
			allocate(this%PhysicalField(this%numoflayer) % scalar(datasize) )
			this%PhysicalField(this%numoflayer)%datastyle = 1
			this%PhysicalField(this%numoflayer) % scalar(:) = 0.0d0
		case ("Vector","VECTOR","vector")
			allocate(this%PhysicalField(this%numoflayer) % vector(datasize,vector_rank) )
			this%PhysicalField(this%numoflayer) % vector(:,:) = 0.0d0
			this%PhysicalField(this%numoflayer)%datastyle = 2
		case ("Tensor","TENSOR","tensor")
			allocate(this%PhysicalField(this%numoflayer) % tensor(datasize,tensor_rank1,tensor_rank2) )
			this%PhysicalField(this%numoflayer) % tensor(:,:,:) = 0.0d0
			this%PhysicalField(this%numoflayer)%datastyle = 3
	end select

	!if(present(scalar) )then
	!	this %PhysicalField(this%numoflayer) % scalar = scalar		
	!endif
    

end subroutine
! ######################################################################




! ######################################################################
subroutine addLayerFEMDomainScalar(this,name,scalar)
	class(FEMDomain_),intent(inout) :: this
	type(PhysicalField_),allocatable :: pfa(:)
	real(real64),intent(in) :: scalar(:)
	character(*),intent(in) :: name
	integer(int32) :: datasize,i


	if(.not.allocated(this % PhysicalField) ) then
		allocate(this % PhysicalField(100)) ! 100 layer as default
		this%numoflayer=0
	endif
	this%numoflayer=this%numoflayer+1
	
	if(this%numoflayer>size(this % PhysicalField) )then
		pfa = this%PhysicalField
		deallocate(this%PhysicalField)
		allocate(this%PhysicalField(size(pfa)*100 ) )
		do i=1,size(this%physicalfield)
		this%PhysicalField(i)%name = "untitled"
		enddo
		this%PhysicalField(1:size(pfa))=pfa(:)
	endif


	this %PhysicalField(this%numoflayer) % name   = name
	if(this%mesh%empty() .eqv. .true. )then
		print *, "ERROR >> addLayerFEMDomain >> mesh should be defined preliminary."
		return
	endif

	
	this%PhysicalField(this%numoflayer) % scalar =scalar
	
	! auto detection of the type of layer
	this%PhysicalField(this%numoflayer)%datastyle = 1
	if(size(scalar,1) == this%nn()  )then
		! Node-wise scalar field
		this%PhysicalField(this%numoflayer) %attribute = 1
	elseif(size(scalar,1) == this%ne())then
		! Element-wise scalar field
		this%PhysicalField(this%numoflayer) %attribute = 2
	elseif(size(scalar,1) == this%nne()*this%nn()  )then
		! GausPoint-wise field
		this%PhysicalField(this%numoflayer) %attribute = 3
	else
		this%PhysicalField(this%numoflayer) %attribute = 0
		print *, "addLaayerFEMDOmainScalar :: layer ",name,"is not node-wise, not element-wize nor GaussPoint-wise"
	endif


end subroutine
! ######################################################################



! ######################################################################
subroutine addLayerFEMDomainVector(this,name,vector)
	class(FEMDomain_),intent(inout) :: this
	type(PhysicalField_),allocatable :: pfa(:)
	real(real64),intent(in) :: vector(:,:)
	character(*),intent(in) :: name
	integer(int32) :: datasize,datadimension,i


	if(.not.allocated(this % PhysicalField) ) then
		allocate(this % PhysicalField(100)) ! 100 layer as default
		this%numoflayer=0
	endif
	this%numoflayer=this%numoflayer+1
	
	if(this%numoflayer>size(this % PhysicalField) )then
		pfa = this%PhysicalField
		deallocate(this%PhysicalField)
		allocate(this%PhysicalField(size(pfa)*100 ) )
		do i=1,size(this%physicalfield)
		this%PhysicalField(i)%name = "untitled"
		enddo
		this%PhysicalField(1:size(pfa))=pfa(:)
	endif


	this %PhysicalField(this%numoflayer) % name   = name
	if(this%mesh%empty() .eqv. .true. )then
		print *, "ERROR >> addLayerFEMDomain >> mesh should be defined preliminary."
		return
	endif
	this%PhysicalField(this%numoflayer) % vector =vector

		! auto detection of the type of layer
	this%PhysicalField(this%numoflayer)%datastyle = 2
	if(size(vector,1) == this%nn()  )then
		! Node-wise vector field
		this%PhysicalField(this%numoflayer) %attribute = 1
	elseif(size(vector,1) == this%ne())then
		! Element-wise vector field
		this%PhysicalField(this%numoflayer) %attribute = 2
	elseif(size(vector,1) == this%nne()*this%nn()  )then
		! GausPoint-wise field
		this%PhysicalField(this%numoflayer) %attribute = 3
	else
		this%PhysicalField(this%numoflayer) %attribute = 0
		print *, "addLaayerFEMDOmainvector :: layer ",name,"is not node-wise, not element-wize nor GaussPoint-wise"
	endif


end subroutine
! ######################################################################



! ######################################################################
subroutine addLayerFEMDomaintensor(this,name,tensor)
	class(FEMDomain_),intent(inout) :: this
	type(PhysicalField_),allocatable :: pfa(:)
	real(real64),intent(in) :: tensor(:,:,:)
	character(*),intent(in) :: name
	integer(int32) :: datasize,datadimension,i


	if(.not.allocated(this % PhysicalField) ) then
		allocate(this % PhysicalField(100)) ! 100 layer as default
		this%numoflayer=0
	endif
	this%numoflayer=this%numoflayer+1
	
	if(this%numoflayer>size(this % PhysicalField) )then
		pfa = this%PhysicalField
		deallocate(this%PhysicalField)
		allocate(this%PhysicalField(size(pfa)*100 ) )
		do i=1,size(this%physicalfield)
		this%PhysicalField(i)%name = "untitled"
		enddo
		this%PhysicalField(1:size(pfa))=pfa(:)
	endif


	this %PhysicalField(this%numoflayer) % name   = name
	if(this%mesh%empty() .eqv. .true. )then
		print *, "ERROR >> addLayerFEMDomain >> mesh should be defined preliminary."
		return
	endif

	this%PhysicalField(this%numoflayer) % tensor =tensor
    	! auto detection of the type of layer
	this%PhysicalField(this%numoflayer)%datastyle = 3
	if(size(tensor,1) == this%nn()  )then
		! Node-wise tensor field
		this%PhysicalField(this%numoflayer) %attribute = 1
	elseif(size(tensor,1) == this%ne())then
		! Element-wise tensor field
		this%PhysicalField(this%numoflayer) %attribute = 2
	elseif(size(tensor,1) == this%nne()*this%nn()  )then
		! GausPoint-wise field
		this%PhysicalField(this%numoflayer) %attribute = 3
	else
		this%PhysicalField(this%numoflayer) %attribute = 0
		print *, "addLaayerFEMDOmaintensor :: layer ",name,"is not node-wise, not element-wize nor GaussPoint-wise"
	endif

end subroutine
! ######################################################################

! ######################################################################
subroutine importLayerFEMDomain(this,name,id,scalar,vector,tensor)
	class(FEMDomain_),intent(inout) :: this
	character(*),optional,intent(in) :: name
	integer(int32),optional,intent(in)  :: id
	real(real64),optional,intent(in) :: scalar(:),vector(:,:),tensor(:,:,:)
	integer(int32) :: i,j,n
	
	if(present(name))then
		do i=1,this%numoflayer
			if( this%PhysicalField(i)%name==name )then
				if(present(scalar) )then
					this%PhysicalField(i)%scalar = scalar
				endif
				if(present(vector) )then
					this%PhysicalField(i)%vector = vector
				endif
				if(present(tensor) )then
					this%PhysicalField(i)%tensor = tensor
				endif
			endif
		enddo
	endif

	if(present(id) )then
		if(present(scalar) )then
			this%PhysicalField(id)%scalar = scalar
		endif
		if(present(vector) )then
			this%PhysicalField(id)%vector = vector
		endif
		if(present(tensor) )then
			this%PhysicalField(id)%tensor = tensor
		endif
	endif

end subroutine
! ######################################################################



! ######################################################################
subroutine showLayerFEMDomain(this)
	class(FEMDomain_),intent(inout) :: this
	integer(int32) :: i,j,n
	
	print *, "Number of layers : ",this%numoflayer
	do i=1,this%numoflayer
		print *, this%PhysicalField(i)%name//" : scalar >> "&
			//str(allocated(this%PhysicalField(i)%scalar))//" : vector >> "&
			//str(allocated(this%PhysicalField(i)%vector))//" : tensor >> "&
			//str(allocated(this%PhysicalField(i)%tensor))
	enddo

end subroutine
! ######################################################################


! ######################################################################
function searchLayerFEMDomain(this,name,id) result(ret)
	class(FEMDomain_),intent(inout) :: this
	character(*),optional,intent(in) :: name
	integer(int32),optional,intent(in) :: id
	integer(int32) :: i
	logical :: ret

	ret =.False.
	if(present(name) )then
		do i=1,this%numoflayer
			if(this%PhysicalField(i)%name==name )then
				ret=.true.
				return
			endif
		enddo
		return
	endif

	if(present(id) )then
		if(id <= this%numoflayer)then
			!print *, "Layer-ID : ",id," is : ",this%PhysicalField(id)%name
			ret = .true.
		else
			print *, "id ",id,"is greater than the number of layers",this%numoflayer
		endif
	endif

end function
! ######################################################################


! ######################################################################
function getLayerIDFEMDomain(this,name) result(id)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	integer(int32) :: id
	integer(int32)::i
	
	do i=1,this%numoflayer
		if(this%PhysicalField(i)%name==name )then
			id=i
			return
		endif
	enddo

end function
! ######################################################################



! ######################################################################
function getLayerAttributeFEMDomain(this,name) result(id)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	integer(int32):: id
	integer(int32)::i
	
	do i=1,this%numoflayer
		if(this%PhysicalField(i)%name==name )then
			id = this%PhysicalField(i)%attribute 
			return
		endif
	enddo

end function
! ######################################################################



! ######################################################################
function getLayerDataStyleFEMDomain(this,name) result(id)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	integer(int32) :: id
	integer(int32)::i
	
	do i=1,this%numoflayer
		if(this%PhysicalField(i)%name==name )then
			id = this%PhysicalField(i)%DataStyle
			return
		endif
	enddo

end function
! ######################################################################



! ######################################################################
subroutine projectionFEMDomain(this,direction,domain,PhysicalField,debug)
	class(FEMDomain_),intent(inout) :: this
	character(2),intent(in) :: direction ! "=>, <=, -> or <-"
	type(FEMDomain_),intent(inout) :: domain
	type(ShapeFunction_) :: shapefunc
	!type(MPI_),optional,intent(inout) :: mpid
	character(*),intent(in) :: PhysicalField
	logical,optional,intent(in) :: debug
	logical :: inside
	integer(int32) :: i,j,n,k,field_id,dim_num,start_id,end_id,from_rank
	integer(int32) :: num_node
	real(real64),allocatable :: Jmat(:,:),center(:),x(:),gzi(:),dx(:),dgzi(:),j_inv(:,:)
	real(real64),allocatable :: LocalCoord(:,:),nodvalue(:),original_scalar(:),xvec(:),x_max(:),x_min(:)
	integer(int32),allocatable :: ElemID(:)
	real(real64) :: scalar,val


	! pre-check list
	! PhysicalField exists for both domains?
	dim_num=size(this%mesh%nodcoord,2)
	if(dim_num/=3)then
		print *, "Caution :: femdomain%projection is ready for 3-D, not for other dimensions"
		return
	endif
	allocate(xvec(dim_num) )
	allocate(x_max(dim_num) )
	allocate(x_min(dim_num) )

	!(1) completed
	if(present(debug) )then
		if(debug .eqv. .true.)then
			print *, "[>>] projectionFEMDomain :: checklist starts."
		endif
	endif

	if(this%searchLayer(name=PhysicalField ) .eqv. .false. )then
		print *, "ERROR >> projectionFEMDomain >> no such physicalfield as '"//PhysicalField&
			//"' of domain#1"
		return 
	endif
	if(domain%searchLayer(name=PhysicalField ) .eqv. .false. )then
		print *, "ERROR >> projectionFEMDomain >> no such physicalfield as '"//PhysicalField&
			//"' of domain#1"
		return 
	endif
	if(present(debug) )then
		if(debug .eqv. .true.)then
			print *, "[OK] projectionFEMDomain :: checklist #1 fields exists."
		endif
	endif

	! check datastyle and attribute
	if(this%getLayerDataStyle(name=PhysicalField) /= &
		domain%getLayerDataStyle(name=PhysicalField) )then
		print *, "ERROR >> projectionFEMDomain >> INVALID DataStyle >> node=1, element=2, gauss point = 3"
		print *, "this%getLayerDataStyle(name=PhysicalField) :: ",this%getLayerDataStyle(name=PhysicalField) 
		print *, "domain%getLayerDataStyle(name=PhysicalField) :: ",domain%getLayerDataStyle(name=PhysicalField) 
		return
	endif
	if(this%getLayerAttribute(name=PhysicalField) /= &
		domain%getLayerAttribute(name=PhysicalField) )then
		print *, "ERROR >> projectionFEMDomain >> INVALID attribute >> node=1, element=2, gauss point = 3"
		print *, "this%getLayerAttribute(name=PhysicalField) :: ",this%getLayerAttribute(name=PhysicalField) 
		print *, "domain%getLayerAttribute(name=PhysicalField) :: ",domain%getLayerAttribute(name=PhysicalField) 
		return
	endif

	if(present(debug) )then
		if(debug .eqv. .true.)then
			print *, "[OK] projectionFEMDomain :: checklist #2 datastyles and attributes are valid."
		endif
	endif
	
	if(present(debug) )then
		if(debug .eqv. .true.)then
			print *, "[<<] projectionFEMDomain :: checklist completed."
		endif
	endif

	! projection starts
	! if this%getLayerAttribute(name=PhysicalField) == 1 (nodal values)
	if(this%getLayerAttribute(name=PhysicalField)==1)then
		if(present(debug) )then
			if(debug .eqv. .true.)then
				print *, "[>>] projectionFEMDomain :: projestion starts."
				print *, "[>>] projectionFEMDomain :: attribute #1 :: scalar."
			endif
		endif
		
		select case(direction)
			case ("=>", "->")
				! project obj-side field to => domain

				allocate(ElemID(size(domain%mesh%nodcoord,1)))
				ElemID(:) = -1
				k=size(domain%mesh%nodcoord,2)
				allocate(LocalCoord(size(domain%mesh%nodcoord,1),k))
				LocalCoord(:,:) = 0.0d0
				shapefunc%ElemType=this%Mesh%GetElemType()
				call SetShapeFuncType(shapefunc)

				!!call GetAllShapeFunc(shapefunc,elem_id=1,nod_coord=this%Mesh%NodCoord,&
				!elem_nod=this%Mesh%ElemNod,OptionalGpID=1)
				
				! for mpi acceralation
				start_id=1
				end_id=size(domain%mesh%nodcoord,1)
!				if(present(mpid) )then
!					call mpid%initItr(end_id)
!					start_id = mpid%start_id
!					end_id = mpid%end_id
!				endif

				do i=start_id, end_id ! for each node
					do j=1, size(this%mesh%elemnod,1) ! for each element
						
						! get Jacobian matrix (dx/dgzi)
						do k=1,shapefunc%NumOfGP
							call GetAllShapeFunc(shapefunc,elem_id=j,nod_coord=this%Mesh%NodCoord,&
							elem_nod=this%Mesh%ElemNod,OptionalGpID=k)
							if(k==1)then
								Jmat=shapefunc%Jmat
							else
								Jmat=Jmat+shapefunc%Jmat
							endif
						enddo
						! In-Or-out
						xvec(:)=domain%mesh%nodcoord(i,:)
						do k=1,dim_num
							x_max(k)=maxval(shapefunc%elemcoord(:,k) )
							x_min(k)=minval(shapefunc%elemcoord(:,k) )
						enddo
						inside = InOrOutReal(x=xvec(:),xmax=x_max(:),xmin=x_min(:),DimNum=size(xvec) )
						if(inside .eqv. .false.)then
							cycle
						endif
						! 
						if(.not. allocated(center) )then
							allocate(center(size(this%mesh%nodcoord,2) ) )
						endif
						if(.not. allocated(x) )then
							allocate(x(size(this%mesh%nodcoord,2) ) )
						endif
						if(.not. allocated(dx) )then
							allocate(dx(size(this%mesh%nodcoord,2) ) )
						endif
						if(.not. allocated(gzi) )then
							allocate(gzi(size(this%mesh%nodcoord,2) ) )
						endif
						if(.not. allocated(dgzi) )then
							allocate(dgzi(size(this%mesh%nodcoord,2) ) )
						endif
						center(:)=0.0d0
						do k=1,size(this%mesh%elemnod,2)
							center(:) = center(:) + this%mesh%nodcoord( this%mesh%elemnod(j,k),: )
						enddo
						center(:) = 1.0d0/dble(size(this%mesh%elemnod,2))*center(:)
						x(:) = domain%mesh%nodcoord(i,:)
						dx(:) = x(:) - center(:)
						call inverse_rank_2(Jmat,J_inv)
						dgzi = matmul(J_inv,dx)
						if( maxval(dgzi)<=1.0d0 .and. minval(dgzi)>=-1.0d0 )then
							ElemID(i) = j
							LocalCoord(i,:) = dgzi(:)
							exit
						else
							cycle
						endif
					enddo
					if(present(debug) )then
						if(debug .eqv. .true.)then
							if(i == int(dble(size(domain%mesh%nodcoord,1))/4.0d0) )then
								print *, "[--] projectionFEMDomain :: local coordinate 25 % done."
							endif
							if(i == int(dble(size(domain%mesh%nodcoord,1))/2.0d0) )then
								print *, "[--] projectionFEMDomain :: local coordinate 50 % done."
							endif
							if(i == int(3.0d0*dble(size(domain%mesh%nodcoord,1))/4.0d0) )then
								print *, "[--] projectionFEMDomain :: local coordinate 75 % done."
							endif
							if(i == size(domain%mesh%nodcoord,1))then
								print *, "[ok] projectionFEMDomain :: local coordinate 100 % done."
							endif
						endif
					endif
				enddo

				! for mpi acceralation
!				! merge data
!				if(present(mpid) )then
!					call mpid%Barrier()
!					do i=1,size(ElemID)
!						n =ElemID(i)
!						from_rank = mpid%start_end_id(i)-1
!						call mpid%Bcast(From=from_rank,val=n)
!						ElemID(i)=n
!
!
!						do j=1,size(LocalCoord,2)
!							val = LocalCoord(i,j)
!							call mpid%Bcast(From=from_rank,val=val)
!							LocalCoord(i,j)=val
!						enddo
!					enddo
!				endif
!
				
				! projection先の節点番号iに対応したprojection元の要素ID:ElemID(i)
				! projection先の節点番号iに対応したprojection元の要素局所座標:LocalCoord(i,1:3)@3D
				
				! projection
				field_id = domain%getLayerID(name=PhysicalField)
				if(domain%getLayerAttribute(name=PhysicalField)==1)then
					! scalar
					! for each element
					do i=1,size(this%mesh%nodcoord,1)
						! 節点ごとの値 node-by-node
						if(elemid(i)==-1 )then
							! 対応する要素なし
							cycle
						endif

						! local coordinate
						
						shapefunc%gzi(:) = localCoord(i,:)

						call GetShapeFunction(shapefunc)


						! 要素を構成する節点値sに乗っている値
						if(.not.allocated(nodvalue) )then
							allocate(nodvalue(size(shapefunc%Nmat,1)))
							nodvalue(:) = 0.0d0
						endif
						do k=1,size(this%mesh%elemnod,2)
							n = this%mesh%elemnod(elemid(i) ,k)
							nodvalue(k) = this%PhysicalField(field_id)%scalar(n)
						enddo
						! 節点値の計算
						scalar = dot_product(shapefunc%Nmat, nodvalue)
						domain%PhysicalField(field_id)%scalar(i)=scalar
						!if(.not.allocated(nodvalue) )then
						!	allocate(nodvalue(size(shapefunc%Nmat,1)))
						!	nodvalue(:) = scalar*shapefunc%Nmat(:)
						!	nodvalue(:) = scalar!*shapefunc%Nmat(:)
						!	! ここ、要注意、アルゴリズムに大幅な近似あり。
						!	! 単に一方の領域の節点値を他方の要素の節点値全体に適用している。
						!	! 局所座標gziは使っていない。
						!
						!	! obj => domainのプロジェクションの場合、
						!	! objの要素ごとに、domainの節点が入っているかを調査し、
						!	! objの要素に対するdomain節点の局所座標を確定し、
						!	! その後、objの接点値に形状関数をかけてdomainの節点値とすべき。
						!	! 要精査
						!endif
						!do k=1,size(domain%mesh%elemnod,2)
						!	n = domain%mesh%elemnod(elemid(i) ,k)
						!	domain%PhysicalField(field_id)%scalar(n)=nodvalue(k)
						!enddo

					enddo
				else
					print *, "ERROR now coding >> projectionFEMDomain"
					stop 
				endif
				
			case ("<=", "<-")
				! project domain-side field to => obj

				allocate(ElemID(size(this%mesh%nodcoord,1)))
				ElemID(:) = -1
				k=size(this%mesh%nodcoord,2)
				allocate(LocalCoord(size(this%mesh%nodcoord,1),k))
				LocalCoord(:,:) = 0.0d0
				shapefunc%ElemType=domain%Mesh%GetElemType()
				call SetShapeFuncType(shapefunc)

				!!call GetAllShapeFunc(shapefunc,elem_id=1,nod_coord=domain%Mesh%NodCoord,&
				!elem_nod=domain%Mesh%ElemNod,OptionalGpID=1)
				
				! for mpi acceralation
				start_id=1
				end_id=size(this%mesh%nodcoord,1)
!				if(present(mpid) )then
!					call mpid%initItr(end_id)
!					start_id = mpid%start_id
!					end_id = mpid%end_id
!				endif

				do i=start_id, end_id ! for each node
					do j=1, size(domain%mesh%elemnod,1) ! for each element
						
						! get Jacobian matrix (dx/dgzi)
						do k=1,shapefunc%NumOfGP
							call GetAllShapeFunc(shapefunc,elem_id=j,nod_coord=domain%Mesh%NodCoord,&
							elem_nod=domain%Mesh%ElemNod,OptionalGpID=k)
							if(k==1)then
								Jmat=shapefunc%Jmat
							else
								Jmat=Jmat+shapefunc%Jmat
							endif
						enddo
						! In-Or-out
						xvec(:)=this%mesh%nodcoord(i,:)
						do k=1,dim_num
							x_max(k)=maxval(shapefunc%elemcoord(:,k) )
							x_min(k)=minval(shapefunc%elemcoord(:,k) )
						enddo
						inside = InOrOutReal(x=xvec(:),xmax=x_max(:),xmin=x_min(:),DimNum=size(xvec) )
						if(inside .eqv. .false.)then
							cycle
						endif
						! 
						if(.not. allocated(center) )then
							allocate(center(size(domain%mesh%nodcoord,2) ) )
						endif
						if(.not. allocated(x) )then
							allocate(x(size(domain%mesh%nodcoord,2) ) )
						endif
						if(.not. allocated(dx) )then
							allocate(dx(size(domain%mesh%nodcoord,2) ) )
						endif
						if(.not. allocated(gzi) )then
							allocate(gzi(size(domain%mesh%nodcoord,2) ) )
						endif
						if(.not. allocated(dgzi) )then
							allocate(dgzi(size(domain%mesh%nodcoord,2) ) )
						endif
						center(:)=0.0d0
						do k=1,size(domain%mesh%elemnod,2)
							center(:) = center(:) + domain%mesh%nodcoord( domain%mesh%elemnod(j,k),: )
						enddo
						center(:) = 1.0d0/dble(size(domain%mesh%elemnod,2))*center(:)
						x(:) = this%mesh%nodcoord(i,:)
						dx(:) = x(:) - center(:)
						call inverse_rank_2(Jmat,J_inv)
						dgzi = matmul(J_inv,dx)
						if( maxval(dgzi)<=1.0d0 .and. minval(dgzi)>=-1.0d0 )then
							ElemID(i) = j
							LocalCoord(i,:) = dgzi(:)
							exit
						else
							cycle
						endif
					enddo
					if(present(debug) )then
						if(debug .eqv. .true.)then
							if(i == int(dble(size(this%mesh%nodcoord,1))/4.0d0) )then
								print *, "[--] projectionFEMDomain :: local coordinate 25 % done."
							endif
							if(i == int(dble(size(this%mesh%nodcoord,1))/2.0d0) )then
								print *, "[--] projectionFEMDomain :: local coordinate 50 % done."
							endif
							if(i == int(3.0d0*dble(size(this%mesh%nodcoord,1))/4.0d0) )then
								print *, "[--] projectionFEMDomain :: local coordinate 75 % done."
							endif
							if(i == size(this%mesh%nodcoord,1))then
								print *, "[ok] projectionFEMDomain :: local coordinate 100 % done."
							endif
						endif
					endif
				enddo

				! for mpi acceralation
				! merge data
!				if(present(mpid) )then
!					call mpid%Barrier()
!					do i=1,size(ElemID)
!						n =ElemID(i)
!						from_rank = mpid%start_end_id(i)-1
!						call mpid%Bcast(From=from_rank,val=n)
!						ElemID(i)=n
!
!
!						do j=1,size(LocalCoord,2)
!							val = LocalCoord(i,j)
!							call mpid%Bcast(From=from_rank,val=val)
!							LocalCoord(i,j)=val
!						enddo
!					enddo
!				endif
!
				
				! projection先の節点番号iに対応したprojection元の要素ID:ElemID(i)
				! projection先の節点番号iに対応したprojection元の要素局所座標:LocalCoord(i,1:3)@3D
				
				! projection
				field_id = domain%getLayerID(name=PhysicalField)
				if(domain%getLayerAttribute(name=PhysicalField)==1)then
					! scalar
					! for each element
					do i=1,size(this%mesh%nodcoord,1)
						! 節点ごとの値 node-by-node
						if(elemid(i)==-1 )then
							! 対応する要素なし
							cycle
						endif

						! local coordinate
						
						shapefunc%gzi(:) = localCoord(i,:)

						call GetShapeFunction(shapefunc)


						! 要素を構成する節点値sに乗っている値
						if(.not.allocated(nodvalue) )then
							allocate(nodvalue(size(shapefunc%Nmat,1)))
							nodvalue(:) = 0.0d0
						endif
						do k=1,size(this%mesh%elemnod,2)
							n = this%mesh%elemnod(elemid(i) ,k)
							nodvalue(k) = this%PhysicalField(field_id)%scalar(n)
						enddo
						! 節点値の計算
						scalar = dot_product(shapefunc%Nmat, nodvalue)
						domain%PhysicalField(field_id)%scalar(i)=scalar
						!if(.not.allocated(nodvalue) )then
						!	allocate(nodvalue(size(shapefunc%Nmat,1)))
						!	nodvalue(:) = scalar*shapefunc%Nmat(:)
						!	nodvalue(:) = scalar!*shapefunc%Nmat(:)
						!	! ここ、要注意、アルゴリズムに大幅な近似あり。
						!	! 単に一方の領域の節点値を他方の要素の節点値全体に適用している。
						!	! 局所座標gziは使っていない。
						!
						!	! obj => domainのプロジェクションの場合、
						!	! objの要素ごとに、domainの節点が入っているかを調査し、
						!	! objの要素に対するdomain節点の局所座標を確定し、
						!	! その後、objの接点値に形状関数をかけてdomainの節点値とすべき。
						!	! 要精査
						!endif
						!do k=1,size(domain%mesh%elemnod,2)
						!	n = domain%mesh%elemnod(elemid(i) ,k)
						!	domain%PhysicalField(field_id)%scalar(n)=nodvalue(k)
						!enddo

					enddo
				else
					print *, "ERROR now coding >> projectionFEMDomain"
					stop 
				endif
				
		end select
	endif

end subroutine
! ######################################################################


! ######################################################################
function centerPositionFEMDomain(this,ElementID,max,min) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32),optional,intent(in) :: ElementID
	logical,optional,intent(in) :: max, min
	real(real64),allocatable :: ret(:)
	integer(int32) :: i

	
	! get center coordinate of the element 
	ret = zeros(this%nd() )
	if(present(ElementID) )then

		if(present(max) )then
			if(max)then
				ret = zeros(this%nn() )
				do i=1,this%nn()
					ret(i) = maxval(this%mesh%nodcoord( this%mesh%elemnod(ElementID,:) ,i))
				enddo
				return
			endif
		elseif(present(min) )then
			if(min)then
				ret = zeros(this%nn() )
				do i=1,this%nn()
					ret(i) = minval(this%mesh%nodcoord( this%mesh%elemnod(ElementID,:) ,i))
				enddo
				return
			endif
		endif
		
		do i=1,this%nne()
			ret = ret + this%mesh%nodcoord( this%mesh%elemnod(ElementID,i) ,:)
		enddo
		ret = 1.0d0/dble( this%nne() )* ret
		
	else

		if(present(max) )then
			if(max)then
				ret = zeros(this%nn() )
				do i=1,this%nn()
					ret(i) = maxval(this%mesh%nodcoord( : ,i))
				enddo
				return
			endif
		elseif(present(min) )then
			if(min)then
				ret = zeros(this%nn() )
				do i=1,this%nn()
					ret(i) = minval(this%mesh%nodcoord( : ,i))
				enddo
				return
			endif
		endif

		do i=1,this%nd()
			ret(i) = sum(this%mesh%nodcoord(:,i) )/dble(this%nn() )
		enddo
	
	endif

end function
! ######################################################################


! ######################################################################
function getGlobalPositionOfGaussPointFEMDomain(this,ElementID,GaussPointID) result(ret)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: ElementID,GaussPointID
	real(real64),allocatable :: ret(:),center(:)
	integer(int32) :: i
	type(ShapeFunction_) :: sf
	! get center coordinate of the element 
	center = this%centerPosition(ElementID)

	sf = this%mesh%getShapeFunction(ElementID,GaussPointID)

	ret = zeros(size(center) )
	ret(:) = matmul( transpose(sf%elemcoord) , sf%nmat ) !+ center(:)
	
end function
! ######################################################################



! ######################################################################
recursive function getShapeFunctionFEMDomain(this, ElementID,GaussPointID,ReducedIntegration,Position) result(sobj)
	class(FEMDomain_),intent(inout)::this
    integer(int32),optional,intent(in) :: GaussPointID, ElementID
    logical,optional,intent(in) :: ReducedIntegration
	real(real64),optional,intent(in) :: position(:)
    type(ShapeFunction_) ::sobj
    character(:),allocatable :: ElemType
	integer(int32) :: i,j,n,m,gpid,elemID
	real(real64) :: x,y,z
	
	if(.not.present(position) )then
		sobj = this%mesh%getShapeFunction(ElementID,GaussPointID,ReducedIntegration)
	else
		! search nearest element
		
		! import coordinate
		x = 0.0d0
		y = 0.0d0
		z = 0.0d0
		if(size(Position)>=1 )then
			x =  Position(1)
		endif
		if(size(Position)>=2 )then
			y =  Position(2)
		endif

		if(size(Position)>=3 )then
			z =  Position(3)
		endif

		! get the nearest element's ID
		sobj%ElementID = -1
		sobj%ElementID      = this%mesh%getNearestElementID(x=x,y=y,z=z)
		if(sobj%ElementID==-1)then
			sobj%Empty = .true.
			print *, "[Caution]:: getShapeFunctionFEMDomain >> sobj%elementID = -1 , no such element"
			return
		endif

		! 4点セット
		sobj%NumOfNode = this%nne() !ok
		sobj%NumOfDim  = this%nd()  !ok
		sobj%gzi       = this%getLocalCoordinate(ElementID=sobj%ElementID,x=x,y=y,z=z)
		sobj%Nmat      = zeros(this%nne() )  !ok
		sobj%ElemCoord = zeros(this%nne(),this%nd() )
		
		call sobj%getOnlyNvec() !ok
		do i=1,this%nne()
			sobj%ElemCoord(i,1:this%nd() ) = this%mesh%nodcoord(this%mesh%elemnod(sobj%elementID,i),1:this%nd() )
		enddo
	endif
end function
! ######################################################################

! ######################################################################
function getLocalCoordinateFEMDomain(this,ElementID,x,y,z) result(xi)
	class(FEMDomain_),intent(inout) :: this
	type(ShapeFunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	real(real64),intent(in) ::  x,y,z
	real(real32),allocatable :: jmat32(:,:),j_inv32(:,:)
	real(real64),allocatable :: xcoord(:),jmat(:,:),j_inv(:,:),center(:)
	real(real64),allocatable :: xi(:)
	integer(int32) :: i,j,n

	Jmat = zeros(this%nd(),this%nd())
	allocate( xcoord(this%nd() ))
	allocate( xi(this%nd() ))
	allocate( center(this%nd() ))
	xcoord(:) = 0.0d0
	xi(:) = 0.0d0 
	center(:)  = 0.0d0
	! only for 2D 4-node/ 3D 8node- isoparametric elements
	if(this%nne()==4 .and. this%nd()==2 )then
		do i=1,4 ! 4-gauss points
			shapefunc = this%mesh%getShapeFunction(ElementID=ElementID,GaussPointID=i)
			jmat(:,:) = jmat(:,:) + shapefunc%jmat(:,:)
		enddo
		jmat(:,:) = 0.250d0 * jmat(:,:)
		xcoord(1) = x
		xcoord(2) = y
		do i=1,size(shapefunc%elemcoord,1)
			center(:) = center(:) + shapefunc%elemcoord(i,:)
		enddo
		center(:) = 0.250d0 *center(:)
	elseif(this%nne()==8 .and. this%nd()==3 )then
		do i=1,8 ! 8-gauss points
			shapefunc = this%mesh%getShapeFunction(ElementID=ElementID,GaussPointID=i)
			jmat(:,:) = jmat(:,:) + shapefunc%jmat(:,:)
		enddo
		jmat(:,:) = 0.1250d0 * jmat(:,:)
		xcoord(1) = x
		xcoord(2) = y
		xcoord(3) = z
		do i=1,size(shapefunc%elemcoord,1)
			center(:) = center(:) + shapefunc%elemcoord(i,:)
		enddo
		center(:) = 0.1250d0 *center(:)
	else
		print *, "ERROR :: getLocalCoordinateFEMDomain, only for 2D 4-node/ 3D 8node- isoparametric elements"
		stop
	endif

	! xcoord


	! xi(:) = J_inv x(:)
	xcoord(:) = xcoord(:) - center(:)
	!jmat32 = jmat
	!jmat = dble(jmat32)
	J_inv = inverse(jmat)
	!j_inv32 = J_inv
	!J_inv = dble(j_inv32)
	xi = matmul(J_inv,xcoord)


	! ok
	!allocate(xi( this%nd()*this%nne() ) )
	!n=0
	!do i=1,this%nne()
	!	do j=1,this%nd()
	!		n=n+1
	!		xi(n) = shapefunc%elemcoord(i,j)
	!	enddo
	!enddo

!	allocate(xi(12) )
!	xi(1) = Jmat(1,1)
!	xi(2) = Jmat(1,2)
!	xi(3) = Jmat(1,3)
!	xi(4) = Jmat(2,1)
!	xi(5) = Jmat(2,2)
!	xi(6) = Jmat(2,3)
!	xi(7) = Jmat(3,1)
!	xi(8) = Jmat(3,2)
!	xi(9) = Jmat(3,3)
!	xi(10)= center(1) 
!	xi(11)= center(2) 
!	xi(12)= center(3) 

end function
! ######################################################################

! ######################################################################
pure function nnFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32) :: ret

	if(this%empty() )then
		ret = 0
		return
	endif
	ret = size(this%mesh%nodcoord,1)

end function
! ######################################################################
! ######################################################################
pure function ndFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32) :: ret


	if(this%empty() )then
		ret = 0
		return
	endif
	ret = size(this%mesh%nodcoord,2)

end function
! ######################################################################
! ######################################################################
pure function neFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32) :: ret


	if(this%empty() )then
		ret = 0
		return
	endif

	if(.not.allocated(this%mesh%ElemNod) ) then
		ret = 0
		return
	endif
	ret = size(this%mesh%ElemNod,1)

end function
! ######################################################################

! ######################################################################
pure function nneFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32) :: ret


	if(this%empty() )then
		ret = 0
		return
	endif

	ret = size(this%mesh%ElemNod,2)

end function
! ######################################################################


! ######################################################################
function ngpFEMDomain(this) result(ret)
	class(FEMDomain_),intent(inout) :: this
	type(ShapeFunction_) :: sf
	integer(int32) :: ret


	if(this%empty() )then
		ret = 0
		return
	endif
	
	sf = this%mesh%getShapeFunction(ElementID=1, GaussPointID=1)
	ret = sf%NumOfGP

!	red = input(default=.false.,option=reduction)
!
!	if(this%nd()==1 )then
!		if(this%nne()==2 )then
!			! 1st order 1-D line element
!			if(reduction)then
!				ret = 1
!			else
!				ret = 2
!			endif
!		elseif(this%nne()==3 )then
!			! 2nd order 1-D line element
!			if(reduction)then
!				ret = 2
!			else
!				ret = 3
!			endif
!		else
!			print *, "ERROR :: ngpFEMDomain >> this%nne() should be 2 or 3 for 1D"
!			ret = -1
!		endif
!	elseif(this%nd()==2 )then
!		if(this%nne()==3 )then
!			! 1st order 2-D triangle element
!			if(reduction)then
!				ret = 1
!			else
!				ret = 3
!			endif
!		elseif(this%nne()==6 )then
!			! 2nd order 2-D triangle element
!			if(reduction)then
!				ret = 3
!			else
!				ret = 6
!			endif
!		elseif(this%nne()==4 )then
!			! 1st order 2-D rectangle element
!			if(reduction)then
!				ret = 1
!			else
!				ret = 4
!			endif
!
!		elseif(this%nne()==8 .or. this%nne()==9 )then
!			! 2nd order 2-D rectangle element
!			if(reduction)then
!				ret = 4
!			else
!				ret = 9
!			endif
!		else
!			print *, "ERROR :: ngpFEMDomain >> this%nne() should be 3, 4, or 9 for 2-D"
!			ret = -1
!		endif
!
!	elseif(this%nd()==3 )then
!
!		if(this%nne()==4 )then
!			! 1st order 3-D tetra element
!			if(reduction)then
!				ret = 1
!			else
!				ret = 4
!			endif
!		elseif(this%nne()==8 )then
!			! 1st order 2-D rectangle element
!			if(reduction)then
!				ret = 1
!			else
!				ret = 8
!			endif
!
!		else
!			print *, "ERROR :: ngpFEMDomain >> this%nne() should be 4, 8 for 3-D"
!			ret = -1
!		endif
!	else
!		print *, "ERROR :: ngpFEMDomain >> this%nd() should be 1, 2 or 3."
!		ret = -1
!	endif

end function
! ######################################################################


subroutine editFEMDomain(this,x,altitude)
    class(FEMDomain_),intent(inout) :: this
	real(real64),optional,intent(in) :: x(:),altitude(:)
	
	call this%mesh%edit(x,altitude)
end subroutine

! ######################################################################
function getNearestNodeIDFEMDomain(this,x,y,z,except,exceptlist) result(node_id)
	class(FEMDomain_),intent(inout) :: this
    real(real64),optional,intent(in) :: x,y,z ! coordinate
    integer(int32),optional,intent(in) :: except ! excepted node id
    integer(int32),optional,intent(in) :: exceptlist(:) ! excepted node id
	integer(int32) :: node_id,i

	node_id = this%mesh%getNearestNodeID(x=x,y=y,z=z,except=except,exceptlist=exceptlist)
end function
! ######################################################################

! ##########################################################################
function positionFEMDomain(this,id) result(x)
    class(FEMDomain_),intent(in) :: this
    integer(int32),optional,intent(in) :: id ! node_id
    real(real64) :: x(3)
    integer(int32) :: dim_num,i

	if(present(id) )then
    	dim_num = size(this%mesh%nodcoord,2)
    	do i=1,dim_num
    	    x(i) = this%mesh%nodcoord(id,i)
    	enddo
	else

		x = zeros(this%nd() )
		do i=1,this%nd()
			x(i) = sum(this%mesh%nodcoord(:,i) )/dble(this%nn() )
		enddo
	
	endif
end function
! ##########################################################################

! ##########################################################################
function position_xFEMDomain(this,id) result(x)
    class(FEMDomain_),intent(in) :: this
    integer(int32),optional,intent(in) :: id ! node_id
    real(real64) :: x
    
	if(present(id) )then
    	x = this%mesh%nodcoord(id,1)
	else
		x = sum(this%mesh%nodcoord(:,1))/dble(this%nn() )
	endif


end function
! ##########################################################################

! ##########################################################################
function position_yFEMDomain(this,id) result(x)
    class(FEMDomain_),intent(in) :: this
    integer(int32),optional,intent(in) :: id ! node_id
    real(real64) :: x
    
    
	if(present(id) )then
    	x = this%mesh%nodcoord(id,2)
	else
		x = sum(this%mesh%nodcoord(:,2))/dble(this%nn() )
	endif
	
end function
! ##########################################################################

! ##########################################################################
function position_zFEMDomain(this,id) result(x)
    class(FEMDomain_),intent(in) :: this
    integer(int32),optional,intent(in) :: id ! node_id
    real(real64) :: x
    
	if(present(id) )then
    	x = this%mesh%nodcoord(id,3)
	else
		x = sum(this%mesh%nodcoord(:,3))/dble(this%nn() )
	endif

end function
! ##########################################################################


! Basic matrices and vectors 
! ##########################################################################
function MassMatrix_as_CRS_FEMDomain(this,Density,DOF,omp) result(MassMatrix)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: Density(:)
	integer(int32),optional,intent(in) :: DOF
	logical,optional,intent(in) :: omp
	
	type(CRS_) :: MassMatrix
	type(COO_) :: COO
	integer(int32) :: ElementID,LE1,LE2,ni1,ni2,&
		pid_1,pid_2,DOF_1,DOF_2,loc_pid_1,loc_pid_2,i,col_id
	real(real64),allocatable :: EDM(:,:),val(:)
	real(real64) :: Length,entry_val

	

	! >>>>>>>> FOR 1-D case >>>>>>>> 
	if(this%nne()==2 )then
		! stiffness matrix for 1-D
		call coo%init(this%nn())
		do ElementID=1,this%ne() 
			Length = norm( this%mesh%nodcoord(this%mesh%elemnod(ElementID,1),:) &
				- this%mesh%nodcoord(this%mesh%elemnod(ElementID,2),:)  )
			entry_val = Density(ElementID)*Length
			call coo%add( this%mesh%elemnod(ElementID,1),this%mesh%elemnod(ElementID,1), entry_val/3.0d0 )
			call coo%add( this%mesh%elemnod(ElementID,1),this%mesh%elemnod(ElementID,2), entry_val/6.0d0 )
			call coo%add( this%mesh%elemnod(ElementID,2),this%mesh%elemnod(ElementID,1), entry_val/6.0d0 )
			call coo%add( this%mesh%elemnod(ElementID,2),this%mesh%elemnod(ElementID,2), entry_val/3.0d0 )
		enddo
		MassMatrix = coo%to_crs()
		return
	endif
	! <<<<<<<< FOR 1-D case <<<<<<<<


	if(.not.present(DOF) )then
		print *, "ERROR >> MassMatrix_as_CRS_FEMDomain should have arg [DOF]"
		stop
	endif

	if(present(omp) )then
		if(.not.omp)then
			MassMatrix = this%ZeroMatrix(DOF=DOF)
			do ElementID=1,this%ne()
				EDM = this%MassMatrix(&
					ElementID=ElementID,&
					Density=Density(ElementID) , &
					DOF=DOF  &
   				) 
				do LE1=1,this%nne()
					do LE2=1,this%nne()
						do DOF_1=1,DOF
							do DOF_2=1,DOF
								ni1 = this%mesh%elemnod(ElementID,LE1)
								ni2 = this%mesh%elemnod(ElementID,LE2)
								pid_1 = DOF*(ni1-1) + DOF_1
								pid_2 = DOF*(ni2-1) + DOF_2
								loc_pid_1 = DOF*(LE1-1) + DOF_1
								loc_pid_2 = DOF*(LE2-1) + DOF_2
								call MassMatrix%add(pid_1,pid_2,EDM(loc_pid_1,loc_pid_2)  )
							enddo
						enddo
					enddo
				enddo
			enddo
			return
		endif
	endif



	!call COO%init(this%nn()*DOF)

	MassMatrix = this%ZeroMatrix(DOF=DOF)
	val = MassMatrix%val
	!$OMP parallel do private(EDM,LE1,LE2,ni1,col_id,i,pid_1,pid_2,loc_pid_1,loc_pid_2,DOF_1,DOF_2) 
	do ElementID=1,this%ne()
		EDM = this%MassMatrix(&
			ElementID=ElementID,&
			Density=Density(ElementID) , &
			DOF=DOF  &
		   ) 
		do LE1=1,this%nne()
			do LE2=1,this%nne()
				do DOF_1=1,DOF
					do DOF_2=1,DOF
						ni1 = this%mesh%elemnod(ElementID,LE1)
						pid_1 = DOF*(ni1-1) + DOF_1
						ni1 = this%mesh%elemnod(ElementID,LE2)
						pid_2 = DOF*(ni1-1) + DOF_2
						loc_pid_1 = DOF*(LE1-1) + DOF_1
						loc_pid_2 = DOF*(LE2-1) + DOF_2
						!call COO%add(pid_1,pid_2,EDM(loc_pid_1,loc_pid_2)  )
						
						do i=MassMatrix%row_ptr(pid_1),MassMatrix%row_ptr(pid_1+1)-1
							if(MassMatrix%col_idx(i)==pid_2 )then
								val(i) = val(i) + EDM(loc_pid_1,loc_pid_2)  
								exit
							endif
						enddo

					enddo
				enddo
			enddo
		enddo
	enddo
	!$OMP end parallel do
	MassMatrix%val = val

end function

! ##########################################################################

! ##########################################################################
recursive function ZeroMatrix_as_CRS_FEMDomain(this,DOF,regacy) result(ZeroMatrix)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: DOF
	type(CRS_) :: ZeroMatrix
	type(COO_) :: COO
	integer(int32) :: ElementID,LocElemID_1,LocElemID_2,nodeid_1,nodeid_2,&
		pid_1,pid_2,DOF_1,DOF_2,loc_pid_1,loc_pid_2,DOF_as_1
	integer(int32) :: i,j,k,l,n,thread_num,m
	integer(int32),allocatable :: count_appear(:),ELL_col_idx(:,:),nonzero_idx(:),&
		ELL_elem(:,:),col_idx(:)
	integer(int64),allocatable ::row_ptr(:)
	
	logical,optional,intent(in) :: regacy

	! Segfo error


	if(present(regacy) )then
		if(regacy  )then
			
			DOF_as_1 = 1
			call COO%init(this%nn()*DOF_as_1)
			do ElementID=1,this%ne()
				do LocElemID_1=1,this%nne()
					do LocElemID_2=1,this%nne()
						do DOF_1=1,DOF_as_1
							do DOF_2=1,DOF_as_1
								nodeid_1 = this%mesh%elemnod(ElementID,LocElemID_1)
								nodeid_2 = this%mesh%elemnod(ElementID,LocElemID_2)
								pid_1 = DOF_as_1*(nodeid_1-1) + DOF_1
								pid_2 = DOF_as_1*(nodeid_2-1) + DOF_2
								loc_pid_1 = DOF_as_1*(LocElemID_1-1) + DOF_1
								loc_pid_2 = DOF_as_1*(LocElemID_2-1) + DOF_2
								call COO%add(pid_1,pid_2,0.0d0 )
							enddo
						enddo
					enddo
				enddo
			enddo
			ZeroMatrix = COO%to_CRS()
			call COO%remove()

			if(DOF>1)then
				! extend!
				row_ptr = ZeroMatrix%row_ptr
				col_idx = ZeroMatrix%col_idx
				ZeroMatrix%row_ptr = int(zeros( (size(row_ptr)-1)*DOF + 1 ) )


				ZeroMatrix%row_ptr(1) = 1 
				do i=1,size(row_ptr)-1
					do j=1,DOF
						ZeroMatrix%row_ptr( DOF*(i-1) + j + 1 ) = ZeroMatrix%row_ptr( DOF*(i-1) + j ) &
							+  DOF*( row_ptr(i+1) - row_ptr(i))
					enddo
				enddo
				ZeroMatrix%col_idx = int(zeros( ( ZeroMatrix%row_ptr(size(ZeroMatrix%row_ptr) ) )-1 ))


				m = 0
				do i=1,size(row_ptr)-1
					do k=1,DOF
						do j=row_ptr(i),row_ptr(i+1)-1
							n = row_ptr(i+1) - row_ptr(i)
							do l=1,DOF
								m = m + 1
								ZeroMatrix%col_idx( m) =  DOF*(col_idx(j)-1) + l
							enddo
						enddo
					enddo
				enddo
				ZeroMatrix%val = zeros(size(ZeroMatrix%col_idx) )
			endif


			return
		endif

	endif

	! bugs exist from here 


	!if(DOF/=1)then
	!	ZeroMatrix = this%ZeroMatrix(DOF=DOF,regacy=.true.)
	!	return
	!endif
	! DOF=3とかに対応できるよう，以下を変更

	
	!(i) count max-connect
	count_appear = int(zeros(this%nn()))
	!$OMP parallel do reduction(+:count_appear)
	do i=1,this%ne()
		count_appear(this%mesh%elemnod(i,:) ) = count_appear(this%mesh%elemnod(i,:) )  + 1
	enddo
	!$OMP end parallel do
	ELL_elem = int(zeros( this%nn(),maxval(count_appear) ) )
	count_appear = count_appear*this%nne()

	

	!(ii) Create ELL format
	! ここを並列化！！
	! NodeID vs ElementID
	ELL_col_idx = int(zeros( this%nn(),maxval(count_appear) ) )
	count_appear(:) = 0
	do i=1,this%ne()
		do j=1,this%nne()
			count_appear(this%mesh%elemnod(i,j) ) = count_appear(this%mesh%elemnod(i,j) )  + 1
			ELL_elem(this%mesh%elemnod(i,j),count_appear(this%mesh%elemnod(i,j)) ) &
				= i
		enddo
	enddo


	
	

	count_appear(:) = 0
	!$OMP parallel do private(j,k) reduction(+:count_appear)
	do i=1,size(ELL_elem,1)
		do j=1,size(ELL_elem,2)
			if(ELL_elem(i,j)==0 ) exit
			do k=1,this%nne()
				count_appear(i) = count_appear(i) + 1
				ELL_col_idx(i,count_appear(i)) = this%mesh%elemnod(ELL_elem(i,j),k )
			enddo
		enddo
	enddo
	!$OMP end parallel do

	!(iii) overlap should be 0
	nonzero_idx = int(zeros(size(ELL_col_idx,2)))
	!$OMP parallel do private(j,k)
	do i=1,size(ELL_col_idx,1)
		do j=1,count_appear(i)
			if(ELL_col_idx(i,j) == 0  )then
				cycle
			else
				do k=j+1,count_appear(i)
					if(ELL_col_idx(i,j)==ELL_col_idx(i,k) )then
						ELL_col_idx(i,k) = 0
					endif
				enddo
			endif
		enddo


	enddo
	!$OMP end parallel do



	!(iv) shift zero to right
	!count_appear(:) = 0
	!$OMP parallel do private(j,k,nonzero_idx)
	do i=1,size(ELL_col_idx,1)
		nonzero_idx(:) = 0
		k = 0
		do j=1,count_appear(i)
			if(ELL_col_idx(i,j)/=0 )then
				k = k +1
				nonzero_idx(k) = ELL_col_idx(i,j)
			endif
		enddo
		ELL_col_idx(i,:) = nonzero_idx(:)
	enddo
	!$OMP end parallel do

	

	!print *, "v"
	!(v) count non-zero
	count_appear(:) = 0
	!$OMP parallel do private(j) reduction(+:count_appear)
	do i=1,size(ELL_col_idx,1)
		do j=1,size(ELL_col_idx,2)
			if(ELL_col_idx(i,j)/=0 )then
				
				count_appear(i) = count_appear(i) + 1
			endif
		enddo
	enddo
	!$OMP end parallel do
	
	
	!ZeroMatrix%row_ptr = int(zeros(this%nn()+1))
	allocate( ZeroMatrix%row_ptr(this%nn()+1  ))
	ZeroMatrix%row_ptr(:) = 0
	
	!ZeroMatrix%col_idx = int(zeros(sum(count_appear) ))
	allocate( ZeroMatrix%col_idx(sum(count_appear)  ))
	ZeroMatrix%col_idx(:) = 0
	
	!print *, "vi"
	! (vi) create row_ptr
	ZeroMatrix%row_ptr(1) = 1 
	do i=1,size(ELL_col_idx,1)
		ZeroMatrix%row_ptr(i+1) = ZeroMatrix%row_ptr(i)  + count_appear(i)
	enddo

	

	!$OMP parallel do private(j,k)
	do i=1,size(ZeroMatrix%row_ptr)-1
		k=0
		do j=ZeroMatrix%row_ptr(i),ZeroMatrix%row_ptr(i+1)-1
			k = k + 1
			ZeroMatrix%col_idx(j) = ELL_col_idx(i,k)
		enddo
	enddo
	!$OMP end parallel do
	deallocate(ELL_col_idx,ELL_elem,count_appear,nonzero_idx)

	! ZeroMatrix%val = zeros(size(ZeroMatrix%col_idx) )
	allocate( ZeroMatrix%val(size(ZeroMatrix%col_idx)) )
	ZeroMatrix%val(:) = 0.0d0



	if(DOF>1)then
		
		! extend!
		row_ptr = ZeroMatrix%row_ptr
		col_idx = ZeroMatrix%col_idx
		!ZeroMatrix%row_ptr = int(zeros( (size(row_ptr)-1)*DOF + 1 ) )
		if(allocated(ZeroMatrix%row_ptr) ) deallocate(ZeroMatrix%row_ptr)
		allocate(ZeroMatrix%row_ptr( (size(row_ptr)-1)*DOF + 1 ) )
		ZeroMatrix%row_ptr(:) = 0
		
		! update row-pointers
		ZeroMatrix%row_ptr(1) = 1 
		do i=1,size(row_ptr)-1
			do j=1,DOF
				if(size(ZeroMatrix%row_ptr) < DOF*(i-1) + j + 1  ) then
					print *, "ERROR femdomain%zeromatrix >> if(size(ZeroMatrix%col_idx)>m ) then"
					stop
				endif
				ZeroMatrix%row_ptr( DOF*(i-1) + j + 1 ) = ZeroMatrix%row_ptr( DOF*(i-1) + j ) &
					+  DOF*( row_ptr(i+1) - row_ptr(i))
			enddo
		enddo
		!ZeroMatrix%col_idx = int(zeros( ( ZeroMatrix%row_ptr(size(ZeroMatrix%row_ptr) ) )-1 ))
		if(allocated(ZeroMatrix%col_idx) ) deallocate(ZeroMatrix%col_idx)
		allocate(ZeroMatrix%col_idx(  ( ZeroMatrix%row_ptr(size(ZeroMatrix%row_ptr) ) )-1  ) )
		ZeroMatrix%col_idx(:) = 0
		
		
		!return

		! update column-indices
		m = 0
		do i=1,size(row_ptr)-1
			do k=1,DOF
				do j=row_ptr(i),row_ptr(i+1)-1
					n = row_ptr(i+1) - row_ptr(i)
					do l=1,DOF
						m = m + 1
						if(size(ZeroMatrix%col_idx) < m ) then
							print *, "ERROR femdomain%zeromatrix >> if(size(ZeroMatrix%col_idx)>m ) then"
							stop
						endif
						if(size(col_idx) < j ) then
							print *, "ERROR femdomain%zeromatrix >> if(size(ZeroMatrix%col_idx)>m ) then"
							stop
						endif
						ZeroMatrix%col_idx(m) =  DOF*(col_idx(j)-1) + l
					enddo
				enddo
			enddo
		enddo

		m = size(ZeroMatrix%col_idx)
		ZeroMatrix%val = zeros( m )
		
		!return
		

		
	endif

	ZeroMatrix%val(:) = 0.0d0
	
end function

! ##########################################################################
function ZeroMatrix_as_COO_FEMDomain(this,DOF) result(ZeroMatrix)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: DOF
	type(COO_) :: ZeroMatrix
	integer(int32) :: ElementID,LocElemID_1,LocElemID_2,nodeid_1,nodeid_2,&
		pid_1,pid_2,DOF_1,DOF_2,loc_pid_1,loc_pid_2
	real(real64),allocatable :: eDiffMat(:,:)

	call ZeroMatrix%init(this%nn()*DOF)
	do ElementID=1,this%ne()
		do LocElemID_1=1,this%nne()
			do LocElemID_2=1,this%nne()
				do DOF_1=1,DOF
					do DOF_2=1,DOF
						nodeid_1 = this%mesh%elemnod(ElementID,LocElemID_1)
						nodeid_2 = this%mesh%elemnod(ElementID,LocElemID_2)
						pid_1 = DOF*(nodeid_1-1) + DOF_1
						pid_2 = DOF*(nodeid_2-1) + DOF_2
						loc_pid_1 = DOF*(LocElemID_1-1) + DOF_1
						loc_pid_2 = DOF*(LocElemID_2-1) + DOF_2
						call ZeroMatrix%add(pid_1,pid_2,0.0d0 )
					enddo
				enddo
			enddo
		enddo
	enddo

end function
! ##########################################################################


! ##########################################################################
function MassMatrixFEMDomain(this,ElementID,Density,DOF,Lumped) result(MassMatrix)
	class(FEMDomain_),intent(inout) :: this
	type(ShapeFunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	real(real64),optional,intent(in) :: Density
	real(real64),allocatable :: MassMatrix(:,:), Nmat(:,:)
	integer(int32),optional,intent(in) :: DOF
	real(real64) :: rho,center_mass
	integeR(int32) :: i,n,j,k,node_DOF
	logical,optional,intent(in) :: Lumped

	rho = input(default=1.0d0, option=Density)
	node_DOF = input(default=1, option=DOF)
	! For Element ID = ElementID, create Mass Matrix and return it
	! Number of Gauss Point = number of node per element, as default.
	! initialize shape-function object
    
	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne(),NumOfGp=this%mesh%getNumOfGp())
	

	do i=1, shapefunc%NumOfGp
		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
			nod_coord=this%Mesh%NodCoord,&
			elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
		
    	n=size(shapefunc%dNdgzi,2)*node_DOF

    	if(.not.allocated(MassMatrix) ) then
			allocate(MassMatrix(n,n) )
			MassMatrix(:,:)=0.0d0
		endif
    	if(size(MassMatrix,1)/=n .or.size(MassMatrix,2)/=n )then
    	    if(allocated(MassMatrix)) then
    	        deallocate(MassMatrix)
    	    endif
    	    allocate(MassMatrix(n,n) )
    	endif
		if(.not. allocated(Nmat ) )then
			allocate(Nmat(node_DOF*size(shapefunc%Nmat),node_DOF ) )
		endif


		Nmat (:,: )=0.0d0
		do j=1,size(shapefunc%Nmat)
			do k=1,node_DOF
				Nmat( (j-1)*node_DOF + k, k ) = shapefunc%Nmat(j)
				
				! in case node_DOF=3,
				! N_(1)    0     0
				! 0      N_(1)   0
				! 0        0    N_(1)
				! N_(2)    0     0
				! 0      N_(2)   0
				! 0        0    N_(2)
				! N_(3)    0     0
				! 0      N_(3)   0
				! 0        0    N_(3)
				! ...

		
			enddo
		enddo
		
    	
		MassMatrix(:,:)=MassMatrix(:,:)+&
			matmul( Nmat, transpose(Nmat) ) &
			*det_mat(shapefunc%Jmat,size(shapefunc%Jmat,1) )
		
	enddo


	MassMatrix = rho * MassMatrix

	if(present(Lumped) )then
		if(Lumped)then
			do i=1,size(MassMatrix,1)
				center_mass = sum(MassMatrix(i,:))
				MassMatrix(i,:) = 0.0d0
				MassMatrix(i,i) = center_mass
			enddo
		endif
	endif
	
end function
! ##########################################################################

!######################## Get Flow-vector ##########################
function FlowVectorFEMDomain(this,pressure,Permiability,ElementID) result(FlowVector)
    !class(DiffusionEq_),intent(inout)::obj
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: Pressure(:),Permiability
	integer(int32),intent(in) :: ElementID
	real(real64),allocatable :: Flowvector(:),p_elem_nodes(:),dNdx(:,:)
	type(ShapeFunction_) :: shapefunc
    
	integer(int32) ::  i,j,k,n,m
    
    
    
	Flowvector=zeros(this%nd() )    
	
	p_elem_nodes = this%ElementVector(ElementID=ElementID,GlobalVector=Pressure,DOF=1)
	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())

	do j=1, shapefunc%NumOfGp
		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=j)

        call GetAllShapeFunc(this%ShapeFunction,elem_id=ElementID,nod_coord=this%Mesh%NodCoord,&
            elem_nod=this%Mesh%ElemNod,OptionalGpID=j)

		dNdx = matmul(transpose(this%ShapeFunction%dNdgzi),this%ShapeFunction%JmatInv) * &
		this%ShapeFunction%detJ*this%ShapeFunction%GaussIntegWei(j) 
		

		FlowVector(:) = FlowVector(:) - Permiability* &
			matmul( &
				transpose(dNdx), &
				p_elem_nodes(:) )
		
    enddo


end function
!######################## Get Flow-vector ##########################





! ##########################################################################
function MassVectorFEMDomain(this,ElementID,Density,DOF,Accel) result(MassVector)
	class(FEMDomain_),intent(inout) :: this
	type(ShapeFunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	real(real64),optional,intent(in) :: Density,Accel(:)
	real(real64),allocatable :: MassVector(:),accel_vec(:)
	integer(int32),optional,intent(in) :: DOF
	real(real64) :: rho
	integer(int32) :: i,j,k,n,node_DOF,dim_num
	real(real64),allocatable :: Nmat(:,:)

	! density :: (unit: t/m^3)
	! accelerator :: m/s/s
	dim_num = size(this%mesh%nodcoord,2)
	rho = input(default=1.0d0, option=Density)
	node_DOF = input(default=1, option=DOF)
	if(present(accel) )then
		accel_vec = accel
	else
		allocate(accel_vec(dim_num) )
		accel_vec(:) = 0.0d0
	endif
	
	! For Element ID = ElementID, create Mass Matrix and return it
	! Number of Gauss Point = number of node per element, as default.

	! initialize shape-function object
    !this%ShapeFunction%ElemType=this%Mesh%ElemType
	
	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())

	do i=1, shapefunc%NumOfGp
		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
	
    	n=size(shapefunc%dNdgzi,2)*node_DOF
		
    	if(.not.allocated(MassVector) ) then
			allocate(MassVector(n) )
			MassVector(:)=0.0d0
		endif

    	if(size(MassVector,1)/=n)then
    	    if(allocated(MassVector)) then
    	        deallocate(MassVector)
    	    endif
    	    allocate(MassVector(n) )
    	endif
		
		if(.not. allocated(Nmat ) )then
			allocate(Nmat(node_DOF*size(shapefunc%Nmat),node_DOF ) )
		endif


		Nmat (:,: )=0.0d0
		do j=1,size(shapefunc%Nmat)
			do k=1,node_DOF
				Nmat( (j-1)*node_DOF + k, k ) = shapefunc%Nmat(j)
				! in case node_DOF=3,
				! N_(1)    0     0
				! 0      N_(1)   0
				! 0        0    N_(1)
				! N_(2)    0     0
				! 0      N_(2)   0
				! 0        0    N_(2)
				! N_(3)    0     0
				! 0      N_(3)   0
				! 0        0    N_(3)
				! ...

			enddo
		enddo



    	MassVector(:)=MassVector(:)+matmul(Nmat,accel_vec) &
			*det_mat(shapefunc%Jmat,size(shapefunc%Jmat,1) )

	enddo

	MassVector = rho * MassVector

end function
! ##########################################################################



! ##########################################################################


function StiffnessMatrix_as_CRS_FEMDomain(this,YoungModulus,PoissonRatio,omp) result(StiffnessMatrix)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: YoungModulus(:)
	real(real64),optional,intent(in) :: PoissonRatio(:)
	logical,optional,intent(in) :: omp
	type(CRS_) :: StiffnessMatrix
	type(COO_) :: COO
	integer(int32) :: ElementID,LocElemID_1,LocElemID_2,nodeid_1,nodeid_2,&
		pid_1,pid_2,DOF_1,DOF_2,DOF,loc_pid_1,loc_pid_2,i,col_id
	real(real64) :: Length,entry_val
	real(real64),allocatable :: val(:)
	real(real64),allocatable :: eDiffMat(:,:)

	! >>>>>>>> FOR 1-D case >>>>>>>> 
	if(this%nne()==2 )then
		! stiffness matrix for 1-D
		call coo%init(this%nn())
		do ElementID=1,this%ne() 
			Length = norm( this%mesh%nodcoord(this%mesh%elemnod(ElementID,1),:) &
				- this%mesh%nodcoord(this%mesh%elemnod(ElementID,2),:)  )
			entry_val = YoungModulus(ElementID)/Length
			call coo%add( this%mesh%elemnod(ElementID,1),this%mesh%elemnod(ElementID,1),entry_val )
			call coo%add( this%mesh%elemnod(ElementID,1),this%mesh%elemnod(ElementID,2), - entry_val )
			call coo%add( this%mesh%elemnod(ElementID,2),this%mesh%elemnod(ElementID,1), - entry_val )
			call coo%add( this%mesh%elemnod(ElementID,2),this%mesh%elemnod(ElementID,2),entry_val )
		enddo
		StiffnessMatrix = coo%to_crs()
		return
	endif
	! <<<<<<<< FOR 1-D case <<<<<<<<


	if(.not.present(PoissonRatio) )then
		print *, "ERROR >> MassMatrix_as_CRS_FEMDomain should have arg [DOF]"
		stop
	endif

	if(present(omp) )then
		if(.not.omp)then
			DOF = this%nd()
			call COO%init(this%nn()*DOF)
			do ElementID=1,this%ne()
				eDiffMat = this%StiffnessMatrix(&
					ElementID=ElementID,&
					E=YoungModulus(ElementID) , &
					v=PoissonRatio(ElementID)  &
		   		) 
				do LocElemID_1=1,this%nne()
					do LocElemID_2=1,this%nne()
						do DOF_1=1,DOF
							do DOF_2=1,DOF
								nodeid_1 = this%mesh%elemnod(ElementID,LocElemID_1)
								nodeid_2 = this%mesh%elemnod(ElementID,LocElemID_2)
								pid_1 = DOF*(nodeid_1-1) + DOF_1
								pid_2 = DOF*(nodeid_2-1) + DOF_2
								loc_pid_1 = DOF*(LocElemID_1-1) + DOF_1
								loc_pid_2 = DOF*(LocElemID_2-1) + DOF_2
								call COO%add(pid_1,pid_2,eDiffMat(loc_pid_1,loc_pid_2)  )
							enddo
						enddo
					enddo
				enddo
			enddo
			StiffnessMatrix = COO%to_CRS()
			return
		endif
	endif

	DOF = this%nd()
	!call COO%init(this%nn()*DOF)
	!COO = this%ZeroMatrix_as_COO(DOF=DOF)
	StiffnessMatrix = this%ZeroMatrix(DOF=DOF)
	val = StiffnessMatrix%val
	!$OMP parallel do &
	!$OMP private(eDiffMat,LocElemID_1,LocElemID_2,nodeid_1,nodeid_2,col_id,i,&
	!$OMP pid_1,pid_2,loc_pid_1,loc_pid_2,DOF_1,DOF_2)&
	!$OMP  reduction(+:val) 
	do ElementID=1,this%ne()
		eDiffMat = this%StiffnessMatrix(&
			ElementID=ElementID,&
			E=YoungModulus(ElementID) , &
			v=PoissonRatio(ElementID)  &
		) 
		do LocElemID_1=1,this%nne()
			do LocElemID_2=1,this%nne()
				do DOF_1=1,DOF
					do DOF_2=1,DOF
						nodeid_1 = this%mesh%elemnod(ElementID,LocElemID_1)
						nodeid_2 = this%mesh%elemnod(ElementID,LocElemID_2)
						pid_1 = DOF*(nodeid_1-1) + DOF_1
						pid_2 = DOF*(nodeid_2-1) + DOF_2
						loc_pid_1 = DOF*(LocElemID_1-1) + DOF_1
						loc_pid_2 = DOF*(LocElemID_2-1) + DOF_2
						!call COO%add(pid_1,pid_2,eDiffMat(loc_pid_1,loc_pid_2)  )

						do i=StiffnessMatrix%row_ptr(pid_1),StiffnessMatrix%row_ptr(pid_1+1)-1
							if(StiffnessMatrix%col_idx(i)==pid_2 )then
								val(i) = val(i) + eDiffMat(loc_pid_1,loc_pid_2)  
								exit
							endif
						enddo

					enddo
				enddo
			enddo
		enddo
	enddo
	!$OMP end parallel do
	StiffnessMatrix%val = val
	
end function

! ##########################################################################


! ##########################################################################
function StiffnessMatrixFEMDomain(this,ElementID,E,v) result(StiffnessMatrix)
	class(FEMDomain_),intent(inout) :: this
	type(Shapefunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	real(real64),intent(in) :: E, v ! Young's modulus and Poisson ratio
	real(real64),allocatable :: StiffnessMatrix(:,:),Bmat(:,:),Dmat(:,:)
	real(real64) :: rho
	integer(int32) :: node_DOF,i,j,n

	! 線形弾性微小ひずみにおける要素剛性マトリクス
	! For Element ID = ElementID, create Stiffness Matrix 
	! in terms of small-strain and return it
	! Number of Gauss Point = number of node per element, as default.
	
	node_DOF = this%nd() ! Degree of freedom/node = dimension of space

	! For Element ID = ElementID, create Mass Matrix and return it
	! Number of Gauss Point = number of node per element, as default.

	! initialize shape-function object
    
	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())
	

	do i=1, shapefunc%NumOfGp
		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
	
    	n=size(shapefunc%dNdgzi,2)*node_DOF

    	if(.not.allocated(StiffnessMatrix) ) then
			allocate(StiffnessMatrix(n,n) )
			StiffnessMatrix(:,:)=0.0d0
		endif
    	if(size(StiffnessMatrix,1)/=n .or.size(StiffnessMatrix,2)/=n )then
    	    if(allocated(StiffnessMatrix)) then
    	        deallocate(StiffnessMatrix)
    	    endif
    	    allocate(StiffnessMatrix(n,n) )
    	endif

		! get so-called B-matrix
		Bmat = this%Bmatrix(shapefunc)
		
		! get D-matrix
		Dmat = this%Dmatrix(E=E, v=v)

		if(i==1)then
			StiffnessMatrix = matmul(matmul(transpose(Bmat),Dmat),Bmat)
			StiffnessMatrix = StiffnessMatrix * det_mat(shapefunc%Jmat,size(shapefunc%Jmat,1) )
		else
			StiffnessMatrix = StiffnessMatrix + &
				matmul(matmul(transpose(Bmat),Dmat),Bmat)&
				*det_mat(shapefunc%Jmat,size(shapefunc%Jmat,1) )
		endif

		
	enddo

end function
! ##########################################################################


! ##########################################################################
function DMatrixFEMDomain(this,E,v) result(Dmat)
	class(FEMDomain_) ,intent(inout) :: this
	real(real64),intent(in) :: E, v
	real(real64),allocatable :: Dmat(:,:)
	real(real64) :: mu, lambda

	! Caution! this is for 
	! isotropic stiffness matrix
	mu = E/2.0d0/(1.0d0 + v)
	lambda = v*E/(1.0d0 + v)/(1.0d0-2.0d0*v)

	if(this%nd() == 1 )then
		Dmat = zeros(1,1)
		Dmat(:,:) = E
		return
	elseif(this%nd() == 2 )then
		! s_11, s_22, s_12
		Dmat = zeros(3,3)
		Dmat(1,1) = (1.0d0-v)*E/( (1.0d0+v)*(1.0d0-2.0d0*v))
		Dmat(2,2) = (1.0d0-v)*E/( (1.0d0+v)*(1.0d0-2.0d0*v))
		Dmat(3,3) = E/(2.0d0*(1.0d0+v) )
		Dmat(1,2) = v*E/( (1.0d0+v)*(1.0d0-2.0d0*v))
		Dmat(2,1) = v*E/( (1.0d0+v)*(1.0d0-2.0d0*v))
	elseif(this%nd() == 3 )then
		Dmat = zeros(6,6)
		Dmat(1,1)= 2.0d0*mu + lambda
		Dmat(1,2)= lambda
		Dmat(1,3)= lambda
		Dmat(2,1)= lambda
		Dmat(2,2)= 2.0d0*mu + lambda
		Dmat(2,3)= lambda
		Dmat(3,1)= lambda
		Dmat(3,2)= lambda
		Dmat(3,3)= 2.0d0*mu + lambda
		Dmat(4,4)= mu
		Dmat(5,5)= mu
		Dmat(6,6)= mu
	else
		print *, "Error :: DMatrixFEMDomain >> number of dimension should be 1-3. Now ",this%nd() 
		stop
	endif
end function
! ##########################################################################


! ##########################################################################
function StrainMatrixFEMDomain(this,ElementID,GaussPoint,disp) result(StrainMatrix)
	class(FEMDomain_),intent(inout) :: this
	type(Shapefunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	integer(int32),optional,intent(in) :: GaussPoint
	
	real(real64),intent(in) :: disp(:,:)
	real(real64),allocatable :: StrainMatrix(:,:),Bmat(:,:),Dmat(:,:),ElemDisp(:),Strainvec(:)
	real(real64) :: rho
	integer(int32) :: node_DOF,i,j,n,ns

	! 線形弾性微小ひずみにおける要素剛性マトリクス
	! For Element ID = ElementID, create Stiffness Matrix 
	! in terms of small-strain and return it
	! Number of Gauss Point = number of node per element, as default.
	
	node_DOF = this%nd() ! Degree of freedom/node = dimension of space

	! For Element ID = ElementID, create Mass Matrix and return it
	! Number of Gauss Point = number of node per element, as default.

	! initialize shape-function object
	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())
	
	ElemDisp = zeros(  size( this%mesh%elemnod,2 ) *node_DOF) 
	do i=1,this%nne()
		do j=1,node_DOF
			ElemDisp( node_DOF*(i-1) + j ) = Disp(i,j)
		enddo
	enddo

	if(present(gausspoint) )then
		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=gausspoint)
	
		n=size(shapefunc%dNdgzi,2)*node_DOF

		ns = node_DOF ! For 3D, 3-by-3 matrix.
		if(.not.allocated(StrainMatrix) ) then
			allocate(StrainMatrix(ns,ns) )
			StrainMatrix(:,:)=0.0d0
		endif
		if(size(StrainMatrix,1)/=ns .or.size(StrainMatrix,2)/=ns )then
			if(allocated(StrainMatrix)) then
				deallocate(StrainMatrix)
			endif
			allocate(StrainMatrix(ns,ns) )
		endif

		! get so-called B-matrix
		Bmat = this%Bmatrix(shapefunc)
		
		strainvec = matmul(Bmat,ElemDisp)

		if(node_DOF==3)then
			strainMatrix(1,1) = strainMatrix(1,1)+strainvec(1)
			strainMatrix(2,2) = strainMatrix(2,2)+strainvec(2)
			strainMatrix(3,3) = strainMatrix(3,3)+strainvec(3)
			strainMatrix(1,2) = strainMatrix(1,2)+strainvec(4)
			strainMatrix(2,3) = strainMatrix(2,3)+strainvec(5)
			strainMatrix(1,3) = strainMatrix(1,3)+strainvec(6)
			strainMatrix(2,1) = strainMatrix(2,1)+strainvec(4)
			strainMatrix(3,2) = strainMatrix(3,2)+strainvec(5)
			strainMatrix(3,1) = strainMatrix(3,1)+strainvec(6)
		elseif(node_DOF == 2)then
			strainMatrix(1,1) = strainMatrix(1,1) + strainvec(1)
			strainMatrix(2,2) = strainMatrix(2,2) + strainvec(2)
			strainMatrix(1,2) = strainMatrix(1,2) + strainvec(3)
			strainMatrix(2,1) = strainMatrix(2,1) + strainvec(3)
		else
			print *, "ERROR :: StrainMatrixFEMDomain >> invalid nodeal DOF",node_DOF
		endif
	else
		do i=1, shapefunc%NumOfGp
			call getAllShapeFunc(shapefunc,elem_id=ElementID,&
			nod_coord=this%Mesh%NodCoord,&
			elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
		
			n=size(shapefunc%dNdgzi,2)*node_DOF
	
			ns = node_DOF ! For 3D, 3-by-3 matrix.
			if(.not.allocated(StrainMatrix) ) then
				allocate(StrainMatrix(ns,ns) )
				StrainMatrix(:,:)=0.0d0
			endif
			if(size(StrainMatrix,1)/=ns .or.size(StrainMatrix,2)/=ns )then
				if(allocated(StrainMatrix)) then
					deallocate(StrainMatrix)
				endif
				allocate(StrainMatrix(ns,ns) )
			endif
	
			! get so-called B-matrix
			Bmat = this%Bmatrix(shapefunc)
			
			strainvec = matmul(Bmat,ElemDisp)
			if(node_DOF==3)then
				strainMatrix(1,1) = strainMatrix(1,1)+strainvec(1)
				strainMatrix(2,2) = strainMatrix(2,2)+strainvec(2)
				strainMatrix(3,3) = strainMatrix(3,3)+strainvec(3)
				strainMatrix(1,2) = strainMatrix(1,2)+strainvec(4)
				strainMatrix(2,3) = strainMatrix(2,3)+strainvec(5)
				strainMatrix(1,3) = strainMatrix(1,3)+strainvec(6)
				strainMatrix(2,1) = strainMatrix(2,1)+strainvec(4)
				strainMatrix(3,2) = strainMatrix(3,2)+strainvec(5)
				strainMatrix(3,1) = strainMatrix(3,1)+strainvec(6)
			elseif(node_DOF == 2)then
				strainMatrix(1,1) = strainMatrix(1,1) + strainvec(1)
				strainMatrix(2,2) = strainMatrix(2,2) + strainvec(2)
				strainMatrix(1,2) = strainMatrix(1,2) + strainvec(3)
				strainMatrix(2,1) = strainMatrix(2,1) + strainvec(3)
			else
				print *, "ERROR :: StrainMatrixFEMDomain >> invalid nodeal DOF",node_DOF
			endif
			
		enddo	
	endif
end function
! ##########################################################################


! ##########################################################################
function StrainVectorFEMDomain(this,ElementID,GaussPoint,disp) result(StrainVec)
	class(FEMDomain_),intent(inout) :: this
	type(Shapefunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	integer(int32),optional,intent(in) :: GaussPoint
	
	real(real64),intent(in) :: disp(:,:)
	real(real64),allocatable :: StrainMatrix(:,:),Bmat(:,:),Dmat(:,:),ElemDisp(:),Strainvec(:)
	real(real64) :: rho
	integer(int32) :: node_DOF,i,j,n,ns,vectorsize

	! 線形弾性微小ひずみにおける要素剛性マトリクス
	! For Element ID = ElementID, create Stiffness Matrix 
	! in terms of small-strain and return it
	! Number of Gauss Point = number of node per element, as default.
	
	node_DOF = this%nd() ! Degree of freedom/node = dimension of space

	! For Element ID = ElementID, create Mass Matrix and return it
	! Number of Gauss Point = number of node per element, as default.
	vectorsize = this%nd()
	do i=1,this%nd()-1
		do j=i+1, this%nd()
			vectorsize = vectorsize+1
		enddo
	enddo
	strainvec = zeros( vectorsize )
	! initialize shape-function object
	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())
	
	ElemDisp = zeros(  size( this%mesh%elemnod,2 ) *node_DOF) 
	do i=1,this%nne()
		do j=1,node_DOF
			ElemDisp( node_DOF*(i-1) + j ) = Disp(i,j)
		enddo
	enddo

	if(present(gausspoint) )then
		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=gausspoint)
	
		n=size(shapefunc%dNdgzi,2)*node_DOF

		ns = node_DOF ! For 3D, 3-by-3 matrix.
		if(.not.allocated(StrainMatrix) ) then
			allocate(StrainMatrix(ns,ns) )
			StrainMatrix(:,:)=0.0d0
		endif
		if(size(StrainMatrix,1)/=ns .or.size(StrainMatrix,2)/=ns )then
			if(allocated(StrainMatrix)) then
				deallocate(StrainMatrix)
			endif
			allocate(StrainMatrix(ns,ns) )
		endif

		! get so-called B-matrix
		Bmat = this%Bmatrix(shapefunc)
		
		strainvec = strainvec+matmul(Bmat,ElemDisp)
	else
		do i=1, shapefunc%NumOfGp
			call getAllShapeFunc(shapefunc,elem_id=ElementID,&
			nod_coord=this%Mesh%NodCoord,&
			elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
		
			n=size(shapefunc%dNdgzi,2)*node_DOF
	
			ns = node_DOF ! For 3D, 3-by-3 matrix.
			if(.not.allocated(StrainMatrix) ) then
				allocate(StrainMatrix(ns,ns) )
				StrainMatrix(:,:)=0.0d0
			endif
			if(size(StrainMatrix,1)/=ns .or.size(StrainMatrix,2)/=ns )then
				if(allocated(StrainMatrix)) then
					deallocate(StrainMatrix)
				endif
				allocate(StrainMatrix(ns,ns) )
			endif
	
			! get so-called B-matrix
			Bmat = this%Bmatrix(shapefunc)
			
			strainvec = strainvec + matmul(Bmat,ElemDisp)
			
		enddo	
	endif
end function
! ##########################################################################

! ##########################################################################
function StressMatrixFEMDomain(this,ElementID,GaussPoint,disp,E,v) result(StressMatrix)
	class(FEMDomain_),intent(inout) :: this
	type(Shapefunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	integer(int32),optional,intent(in) :: GaussPoint
	
	real(real64),intent(in) :: disp(:,:),E,v
	real(real64),allocatable :: StressMatrix(:,:),Bmat(:,:),Dmat(:,:),ElemDisp(:),Stressvec(:)
	real(real64) :: rho
	integer(int32) :: node_DOF,i,j,n,ns


	! 線形弾性微小ひずみにおける要素剛性マトリクス
	! For Element ID = ElementID, create Stiffness Matrix 
	! in terms of small-strain and return it
	! Number of Gauss Point = number of node per element, as default.
	
	node_DOF = this%nd() ! Degree of freedom/node = dimension of space

	! For Element ID = ElementID, create Mass Matrix and return it
	! Number of Gauss Point = number of node per element, as default.

	! initialize shape-function object
	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())
	
	ElemDisp = zeros(  size( this%mesh%elemnod,2 ) *node_DOF) 
	do i=1,this%nne()
		do j=1,node_DOF
			ElemDisp( node_DOF*(i-1) + j ) = Disp( this%mesh%elemnod(ElementID,i) ,j)
		enddo
	enddo

	if(present(gausspoint) )then
		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=gausspoint)
	
		n=size(shapefunc%dNdgzi,2)*node_DOF

		ns = node_DOF ! For 3D, 3-by-3 matrix.
		if(.not.allocated(StressMatrix) ) then
			allocate(StressMatrix(ns,ns) )
			StressMatrix(:,:)=0.0d0
		endif
		if(size(StressMatrix,1)/=ns .or.size(StressMatrix,2)/=ns )then
			if(allocated(StressMatrix)) then
				deallocate(StressMatrix)
			endif
			allocate(StressMatrix(ns,ns) )
		endif

		! get so-called B-matrix
		Dmat = this%Dmatrix(E,v)
		Bmat = this%Bmatrix(shapefunc)
		
		Stressvec = matmul(Dmat,matmul(Bmat,ElemDisp))
		
		if(node_DOF==3)then
			StressMatrix(1,1) = StressMatrix(1,1)+Stressvec(1)
			StressMatrix(2,2) = StressMatrix(2,2)+Stressvec(2)
			StressMatrix(3,3) = StressMatrix(3,3)+Stressvec(3)
			StressMatrix(1,2) = StressMatrix(1,2)+Stressvec(4)
			StressMatrix(2,3) = StressMatrix(2,3)+Stressvec(5)
			StressMatrix(1,3) = StressMatrix(1,3)+Stressvec(6)
			StressMatrix(2,1) = StressMatrix(2,1)+Stressvec(4)
			StressMatrix(3,2) = StressMatrix(3,2)+Stressvec(5)
			StressMatrix(3,1) = StressMatrix(3,1)+Stressvec(6)
		elseif(node_DOF == 2)then
			StressMatrix(1,1) = StressMatrix(1,1) + Stressvec(1)
			StressMatrix(2,2) = StressMatrix(2,2) + Stressvec(2)
			StressMatrix(1,2) = StressMatrix(1,2) + Stressvec(3)
			StressMatrix(2,1) = StressMatrix(2,1) + Stressvec(3)
		else
			print *, "ERROR :: StressMatrixFEMDomain >> invalid nodeal DOF",node_DOF
		endif
	else
		do i=1, shapefunc%NumOfGp
			call getAllShapeFunc(shapefunc,elem_id=ElementID,&
			nod_coord=this%Mesh%NodCoord,&
			elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
		
			n=size(shapefunc%dNdgzi,2)*node_DOF
	
			ns = node_DOF ! For 3D, 3-by-3 matrix.
			if(.not.allocated(StressMatrix) ) then
				allocate(StressMatrix(ns,ns) )
				StressMatrix(:,:)=0.0d0
			endif
			if(size(StressMatrix,1)/=ns .or.size(StressMatrix,2)/=ns )then
				if(allocated(StressMatrix)) then
					deallocate(StressMatrix)
				endif
				allocate(StressMatrix(ns,ns) )
			endif
	
			! get so-called B-matrix
			Bmat = this%Bmatrix(shapefunc)
			Dmat = this%Dmatrix(E,v)
			Stressvec = matmul(Dmat,matmul(Bmat,ElemDisp))
			if(node_DOF==3)then
				StressMatrix(1,1) = StressMatrix(1,1)+Stressvec(1)
				StressMatrix(2,2) = StressMatrix(2,2)+Stressvec(2)
				StressMatrix(3,3) = StressMatrix(3,3)+Stressvec(3)
				StressMatrix(1,2) = StressMatrix(1,2)+Stressvec(4)
				StressMatrix(2,3) = StressMatrix(2,3)+Stressvec(5)
				StressMatrix(1,3) = StressMatrix(1,3)+Stressvec(6)
				StressMatrix(2,1) = StressMatrix(2,1)+Stressvec(4)
				StressMatrix(3,2) = StressMatrix(3,2)+Stressvec(5)
				StressMatrix(3,1) = StressMatrix(3,1)+Stressvec(6)
			elseif(node_DOF == 2)then
				StressMatrix(1,1) = StressMatrix(1,1) + Stressvec(1)
				StressMatrix(2,2) = StressMatrix(2,2) + Stressvec(2)
				StressMatrix(1,2) = StressMatrix(1,2) + Stressvec(3)
				StressMatrix(2,1) = StressMatrix(2,1) + Stressvec(3)
			else
				print *, "ERROR :: StressMatrixFEMDomain >> invalid nodeal DOF",node_DOF
			endif
			
		enddo	
		! cell-averaged
		StressMatrix = StressMatrix/dble(shapefunc%NumOfGp)
	endif

end function
! ##########################################################################


! ##########################################################################
function StressVectorFEMDomain(this,ElementID,GaussPoint,disp,E,v) result(StressVec)
	class(FEMDomain_),intent(inout) :: this
	type(Shapefunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	integer(int32),optional,intent(in) :: GaussPoint
	
	real(real64),intent(in) :: disp(:,:),E,v
	real(real64),allocatable :: StressMatrix(:,:),Bmat(:,:),Dmat(:,:),ElemDisp_m(:,:),ElemDisp(:),Stressvec(:)
	real(real64) :: rho
	integer(int32) :: node_DOF,i,j,n,ns,vectorsize

	! [CAUTION]
	! disp is local displacement matrix (nne by nd )

	! 線形弾性微小ひずみにおける要素剛性マトリクス
	! For Element ID = ElementID, create Stiffness Matrix 
	! in terms of small-strain and return it
	! Number of Gauss Point = number of node per element, as default.
	
	node_DOF = this%nd() ! Degree of freedom/node = dimension of space
	! vector size
	! if nd == 3 => vectorsize = 6
	vectorsize = this%nd()
	do i=1,this%nd()-1
		do j=i+1, this%nd()
			vectorsize = vectorsize+1
		enddo
	enddo
	StressVec = zeros( vectorsize )
	! For Element ID = ElementID, create Mass Matrix and return it
	! Number of Gauss Point = number of node per element, as default.

	! initialize shape-function object
	!call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())
	
	!ElemDisp = zeros(  size( this%mesh%elemnod,2 ) *node_DOF) 
	if( size(disp,1)/=this%nne() )then
		print *, "[ERROR] StressVectorFEM :: Wrong Argument :: disp"
		print *, "[ERROR] >> size(disp,1) should be equal to this%nne()"
		stop
	endif
	ElemDisp_m = reshape(transpose(Disp),[ size(Disp,1)*size(Disp,2),1 ])
	ElemDisp = ElemDisp_m(:,1)
	!do i=1,this%nne()
	!	do j=1,node_DOF
	!		ElemDisp( node_DOF*(i-1) + j ) = Disp(i,j)
	!	enddo
	!enddo

	if(present(gausspoint) )then
		!call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		!nod_coord=this%Mesh%NodCoord,&
		!elem_nod=this%Mesh%ElemNod,OptionalGpID=gausspoint)
	
		shapefunc = this%getShapeFunction(&
		ElementID=ElementID,GaussPointID=GaussPoint)

		!n=size(shapefunc%dNdgzi,2)*node_DOF

		!ns = node_DOF ! For 3D, 3-by-3 matrix.

		! get so-called B-matrix
		Dmat = this%Dmatrix(E,v)
		Bmat = this%Bmatrix(shapefunc)
		
		Stressvec = matmul(Dmat,matmul(Bmat,ElemDisp))
	else
		do i=1, shapefunc%NumOfGp
			
			shapefunc = this%getShapeFunction(&
			ElementID=ElementID,GaussPointID=i)
		
			n=size(shapefunc%dNdgzi,2)*node_DOF
	
			ns = node_DOF ! For 3D, 3-by-3 matrix.
			
	
			! get so-called B-matrix
			!Bmat = this%Bmatrix(shapefunc)
			!Stressvec = Stressvec + matmul(Bmat,ElemDisp)

			Dmat = this%Dmatrix(E,v)
			Bmat = this%Bmatrix(shapefunc)
		
			Stressvec = Stressvec + matmul(Dmat,matmul(Bmat,ElemDisp))
		enddo
	endif

end function
! ##########################################################################

! ##########################################################################
function ViscousBoundaryForceFEMDomain(this,u,v,spring,damper,NodeList,Direction) result(ret)
	class(FEMDomain_),intent(in) :: this
	real(real64),intent(in) :: u(:),v(:),spring,damper
	integer(int32),intent(in) :: NodeList(:)
	character(*),intent(in) :: Direction
	integer(int32) :: NodeListIdx,Idx
	real(real64),allocatable :: ret(:)
	integer(int32) :: DOF

	DOF = size(this%mesh%nodcoord,2)
	
	ret = 0.0d0*u

	if( (index(Direction,"X"))+(index(Direction,"x"))/=0  )then
		do NodeListIdx=1,size(NodeList)
			idx = DOF*(NodeList(NodeListIdx)-1)+1
			ret(idx)  = - spring*u(idx) - damper*v(idx)
		enddo
	endif


	if( (index(Direction,"Y"))+(index(Direction,"y"))/=0  )then
		do NodeListIdx=1,size(NodeList)
			idx = DOF*(NodeList(NodeListIdx)-1)+2
			ret(idx)  = - spring*u(idx) - damper*v(idx)
		enddo
	endif


	if( (index(Direction,"Z"))+(index(Direction,"z"))/=0  )then
		do NodeListIdx=1,size(NodeList)
			idx = DOF*(NodeList(NodeListIdx)-1)+3
			ret(idx)  = - spring*u(idx) - damper*v(idx)
		enddo
	endif
end function
! ##########################################################################


! ##########################################################################
recursive function BMatrixFEMDomain(this,shapefunction,ElementID) result(Bmat)
	class(FEMDomain_) ,intent(inout) :: this
	integer(int32),optional,intent(in) :: ElementID
	type(ShapeFunction_),optional,intent(in) :: shapefunction
	real(real64), allocatable :: Psymat(:,:), Jmat(:,:), detJ 
	real(real64), allocatable :: Bmat(:,:)
	integer(int32)::dim_num
	real(real64), allocatable :: JPsy(:,:), Jin(:,:)
	integer(int32)  k, l,m, n, a, b, p,mm,i,j,q
	
	type(ShapeFunction_) :: sf

	if(present(shapefunction))then
		
	
		dim_num = this%nd()
		mm = this%nne() * 2
		Psymat = ShapeFunction%dNdgzi
		Jmat = ShapeFunction%Jmat
		detJ = det_mat(Jmat, dim_num)
		


		if(dim_num==2)then
		   k=3
		elseif(dim_num==3)then
		   k=6
		else
		   stop "B_mat >> dim_num = tobe 2 or 3 "
		endif
		

	! J:Psymat�̌v�Z
		if(this%nd()==2 .and. this%nne()==4)then

		   if(detJ==0.0d0)  stop "Bmat,detJ=0"
		   Jin = inverse(Jmat)
		   !Jin(1,1) = (1.0d0 / detJ) * Jmat(2,2)
		   !Jin(2,2) = (1.0d0 / detJ) * Jmat(1,1)
		   !Jin(1,2) = (-1.0d0 / detJ) * Jmat(1,2)
		   !Jin(2,1) = (-1.0d0 / detJ) * Jmat(2,1)
		   JPsy = matmul(Jin, Psymat)   
		   Bmat=zeros(3,8)
		   Bmat(1,1) =  JPsy(1,1)
		   Bmat(1,2) = 0.0d0
		   Bmat(1,3) =  JPsy(1,2)
		   Bmat(1,4) = 0.0d0
		   Bmat(1,5) =  JPsy(1,3)
		   Bmat(1,6) = 0.0d0
		   Bmat(1,7) =  JPsy(1,4)
		   Bmat(1,8) = 0.0d0
		   Bmat(2,1) = 0.0d0
		   Bmat(2,2) =  JPsy(2,1)
		   Bmat(2,3) = 0.0d0
		   Bmat(2,4) =  JPsy(2,2)
		   Bmat(2,5) = 0.0d0
		   Bmat(2,6) =  JPsy(2,3)
		   Bmat(2,7) = 0.0d0
		   Bmat(2,8) =  JPsy(2,4)
		   Bmat(3,1) = Bmat(2,2)
		   Bmat(3,2) = Bmat(1,1)
		   Bmat(3,3) = Bmat(2,4)
		   Bmat(3,4) = Bmat(1,3)
		   Bmat(3,5) = Bmat(2,6)
		   Bmat(3,6) = Bmat(1,5)
		   Bmat(3,7) = Bmat(2,8)
		   Bmat(3,8) = Bmat(1,7)
		
		elseif(this%nd()==2 .and. this%nne()==8 )then
			Jin = inverse(Jmat)
		   	JPsy(:,:) = matmul(Jin, Psymat)   

		   Bmat=zeros(3,16)
		   Bmat(1,1) =  -JPsy(1,1)
		   Bmat(1,2) = 0.0d0
		   Bmat(1,3) =  JPsy(1,2)
		   Bmat(1,4) = 0.0d0
		   Bmat(1,5) =  JPsy(1,3)
		   Bmat(1,6) = 0.0d0
		   Bmat(1,7) =  JPsy(1,4)
		   Bmat(1,8) = 0.0d0
		   Bmat(1,9) =  JPsy(1,5)
		   Bmat(1,10) = 0.0d0
		   Bmat(1,11) =  JPsy(1,6)
		   Bmat(1,12) = 0.0d0
		   Bmat(1,13) =  JPsy(1,7)
		   Bmat(1,14) = 0.0d0
		   Bmat(1,15) =  JPsy(1,8)
		   Bmat(1,16) = 0.0d0
		   Bmat(2,1) = 0.0d0
		   Bmat(2,2) =  JPsy(2,1)
		   Bmat(2,3) = 0.0d0
		   Bmat(2,4) =  JPsy(2,2)
		   Bmat(2,5) = 0.0d0
		   Bmat(2,6) =  JPsy(2,3)
		   Bmat(2,7) = 0.0d0
		   Bmat(2,8) =  JPsy(2,4)
		   Bmat(2,9) = 0.0d0
		   Bmat(2,10) =  JPsy(2,5)
		   Bmat(2,11) = 0.0d0
		   Bmat(2,12) =  JPsy(2,6)
		   Bmat(2,13) = 0.0d0
		   Bmat(2,14) =  JPsy(2,7)
		   Bmat(2,15) = 0.0d0
		   Bmat(2,16) =  JPsy(2,8)
		   Bmat(3,1) = Bmat(2,2)
		   Bmat(3,2) = Bmat(1,1)
		   Bmat(3,3) = Bmat(2,4)
		   Bmat(3,4) = Bmat(1,3)
		   Bmat(3,5) = Bmat(2,6)
		   Bmat(3,6) = Bmat(1,5)
		   Bmat(3,7) = Bmat(2,8)
		   Bmat(3,8) = Bmat(1,7)
		   Bmat(3,9) = Bmat(2,10)
		   Bmat(3,10) = Bmat(1,9)
		   Bmat(3,11) = Bmat(2,12)
		   Bmat(3,12) = Bmat(1,11)
		   Bmat(3,13) = Bmat(2,14)
		   Bmat(3,14) = Bmat(1,13)
		   Bmat(3,15) = Bmat(2,16)
		   Bmat(3,16) = Bmat(1,15)
		elseif(this%nd()==3  )then
		
		   if(detJ==0.0d0)  stop "Bmat,detJ=0"
		
		   call  inverse_rank_2(Jmat,Jin)
		
		   JPsy = transpose(matmul(transpose(Psymat),Jin)) !dNdgzi* dgzidx
		   Bmat=zeros(6,this%nne()*3)
		   do q=1,size(JPsy,2)
			   do p=1,dim_num
				   Bmat(p,dim_num*(q-1) + p )=JPsy(p,q)
			   enddo
			   Bmat(4,dim_num*(q-1) + 1 )=JPsy(2,q); Bmat(4, dim_num*(q-1) + 2 )=JPsy(1,q);Bmat(4, dim_num*(q-1) + 3 )=0.0d0    ; 
			   Bmat(5,dim_num*(q-1) + 1 )=0.0d0    ; Bmat(5, dim_num*(q-1) + 2 )=JPsy(3,q);Bmat(5, dim_num*(q-1) + 3 )=JPsy(2,q);
			   Bmat(6,dim_num*(q-1) + 1 )=JPsy(3,q); Bmat(6, dim_num*(q-1) + 2 )=0.0d0    ;Bmat(6, dim_num*(q-1) + 3 )=JPsy(1,q);
		   enddo

		   !Bmat(4:6,:)=0.50d0*Bmat(4:6,:)



		else
		   stop "Bmat >> The element is not supported."
	    endif

	else
		! take sum for all gauss-points
		if(.not. present(ElementID) )then
			print *, "BmatrixFEMDOmain >> ERROR >> at least, arg:ElementID or arg:shapefunction is necessary."
			stop
		endif
		call sf%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())
	

		do i=1, sf%NumOfGp
			call getAllShapeFunc(sf,elem_id=ElementID,&
			nod_coord=this%Mesh%NodCoord,&
			elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
			
			if(i==1)then
				Bmat = this%Bmatrix(sf,ElementID)
			else
				Bmat = Bmat + this%Bmatrix(sf,ElementID)
			endif
		enddo
		return
		
	endif
end function
! ##########################################################################


function DiffusionMatrix_as_CRS_FEMDomain(this,Coefficient,omp) result(DiffusionMatrix)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: Coefficient(:)
	type(CRS_) :: DiffusionMatrix
	type(COO_) :: COO
	logical,optional,intent(in) :: omp
	integer(int32) :: ElementID,LocElemID_1,LocElemID_2,nodeid_1,nodeid_2,i,col_id
	real(real64),allocatable :: val(:)
	real(real64),allocatable :: eDiffMat(:,:)

	if(present(omp) )then
		if(.not.omp)then
			call COO%init(this%nn())
			do ElementID=1,this%ne()
				eDiffMat = this%DiffusionMatrix(&
					ElementID=ElementID,&
					D=coefficient(ElementID) &
		   		) 
				do LocElemID_1=1,this%nne()
					do LocElemID_2=1,this%nne()
						nodeid_1 = this%mesh%elemnod(ElementID,LocElemID_1)
						nodeid_2 = this%mesh%elemnod(ElementID,LocElemID_2)
						call COO%add(nodeid_1,nodeid_2,eDiffMat(LocElemID_1,LocElemID_2)  )
					enddo
				enddo
			enddo
			DiffusionMatrix = COO%to_CRS()
			return
		endif
	endif

	! parallelized
	
	DiffusionMatrix = this%ZeroMatrix(DOF=1)
	val = DiffusionMatrix%val
	!$OMP parallel do private(eDiffMat,LocElemID_1,LocElemID_2,nodeid_1,nodeid_2,col_id,i) reduction(+:val) 
	do ElementID=1,this%ne()
		eDiffMat = this%DiffusionMatrix(&
			ElementID=ElementID,&
			D=coefficient(ElementID) &
   		) 
		do LocElemID_1=1,this%nne()
			do LocElemID_2=1,this%nne()
				nodeid_1 = this%mesh%elemnod(ElementID,LocElemID_1)
				nodeid_2 = this%mesh%elemnod(ElementID,LocElemID_2)
				! COO%add
				do i=DiffusionMatrix%row_ptr(nodeid_1),DiffusionMatrix%row_ptr(nodeid_1+1)-1
					if(DiffusionMatrix%col_idx(i)==nodeid_2 )then
						val(i) = val(i) + eDiffMat(LocElemID_1,LocElemID_2)
						exit
					endif
				enddo
			enddo
		enddo
	enddo
	!$OMP end parallel do
	DiffusionMatrix%val = val


end function

! ##########################################################################
function DiffusionMatrixFEMDomain(this,ElementID,D) result(DiffusionMatrix)
	! 拡散係数マトリクス
	! For Element ID = ElementID, create Diffusion Matrix 
	! in terms of small-strain and return it
	! Number of Gauss Point = number of node per element, as default.
	class(FEMDomain_),intent(inout) :: this
	type(ShapeFunction_) :: shapefunc
	integer(int32),intent(in) :: ElementID
	real(real64),optional,intent(in) :: D ! diffusion matrix
    real(real64)::diff_coeff
	real(real64)::	err = dble(1.0e-14)
	real(real64),allocatable :: DiffusionMatrix(:,:)
	integeR(int32) :: i,j,n


	diff_coeff = input(default=1.0d0, option=D)
	! For Element ID = ElementID, create Mass Matrix and return it
	! Number of Gauss Point = number of node per element, as default.

	! initialize shape-function object
    !this%ShapeFunction%ElemType=this%Mesh%ElemType
	
	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())

	do i=1, shapefunc%NumOfGp
		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
	
    	n=size(shapefunc%dNdgzi,2)
    	if(.not.allocated(DiffusionMatrix) ) then
			allocate(DiffusionMatrix(n,n) )
			DiffusionMatrix(:,:)=0.0d0
		endif

    	if(size(DiffusionMatrix,1)/=n .or.size(DiffusionMatrix,2)/=n )then
    	    if(allocated(DiffusionMatrix)) then
    	        deallocate(DiffusionMatrix)
    	    endif
    	    allocate(DiffusionMatrix(n,n) )
    	endif

		if(det_mat(shapefunc%Jmat,size(shapefunc%Jmat,1) )<0.0d0)then
			print *, "STOP DiffusionMatrixFEMDomain>>mesh is inversed!"
		endif
    	DiffusionMatrix(:,:)=DiffusionMatrix(:,:)+&
		matmul( transpose(matmul(shapefunc%JmatInv,shapefunc%dNdgzi)),&
    	matmul(shapefunc%JmatInv,shapefunc%dNdgzi))&
		*diff_coeff &
		*det_mat(shapefunc%Jmat,size(shapefunc%Jmat,1) )! it was JmatInv, but should be Jmat, revised @ 20221201

    	!DiffusionMatrix(:,:)=DiffusionMatrix(:,:)+&
		!matmul( transpose(shapefunc%dNdgzi),&
    	!shapefunc%dNdgzi)&
		!*diff_coeff &
		!*det_mat(shapefunc%Jmatinv,size(shapefunc%Jmatinv,1) )! it was JmatInv, but should be Jmat, revised @ 20221201
	enddo

	! if Rounding error >> fix 0 
	do i=1,size(DiffusionMatrix,1)
		do j=1,size(DiffusionMatrix,1)
			if(abs(DiffusionMatrix(i,j)) < err*abs(maxval(DiffusionMatrix)))then
				DiffusionMatrix(i,j) = 0.0d0
			endif
		enddo
	enddo
end function
! ##########################################################################



! ##########################################################################
!function GradMatrixFEMDomain(this,ElementID,DOF) result(GradMatrix)
!	! This matrix G_{A B}
!	
!	! \int_{\omega_e} N_A \frac{\partial N_B}{\partial x_i} d \Omega_e
!	! \int_{\omega_e} N_A  d N_B/d xi  (d xi/d x) det(d x/d Xi) d \Xi
!
!	class(FEMDomain_),intent(inout) :: this
!	type(ShapeFunction_) :: shapefunc
!	integer(int32),intent(in) :: ElementID,DOF
!
!	real(real64)::	err = dble(1.0e-14)
!	real(real64),allocatable :: GradMatrix(:,:)
!	integeR(int32) :: i,j,n
!
!	! For Element ID = ElementID, create Mass Matrix and return it
!	! Number of Gauss Point = number of node per element, as default.
!
!	! initialize shape-function object
!    !this%ShapeFunction%ElemType=this%Mesh%ElemType
!	
!	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())
!	n = this%nne()
!	GradMatrix = zeros(n,n*DOF)
!	do i=1, shapefunc%NumOfGp
!		call getAllShapeFunc(shapefunc,elem_id=ElementID,&
!		nod_coord=this%Mesh%NodCoord,&
!		elem_nod=this%Mesh%ElemNod,OptionalGpID=i)
!	
!    	n=size(shapefunc%dNdgzi,2)
!
!    	GradMatrix(:,:)=GradMatrix(:,:)+&
!		matmul( transpose(matmul(shapefunc%JmatInv,shapefunc%dNdgzi)),&
!		)&
!		*det_mat(shapefunc%Jmat,size(shapefunc%Jmat,1) )
!
!	enddo
!
!	! if Rounding error >> fix 0 
!	do i=1,size(GradMatrix,1)
!		do j=1,size(GradMatrix,1)
!			if(abs(GradMatrix(i,j)) < err*abs(maxval(GradMatrix)))then
!				GradMatrix(i,j) = 0.0d0
!			endif
!		enddo
!	enddo
!end function
! ##########################################################################


! ##########################################################################
function ElementVectorFEMDomain(this,ElementID,GlobalVector,DOF) result(ElementVector)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: ElementID
	real(real64),intent(in) :: GlobalVector(:) ! size = number_of_node
	real(real64),allocatable :: ElementVector(:)
	integer(int32),optional,intent(in) :: DOF
	integer(int32) :: i,j,num_node_per_elem, num_dim,nodal_DOF,node_id
	
	! For Element ID = ElementID, create ElementVector and return it
	! Number of Gauss Point = number of node per element, as default.
	num_node_per_elem = this%nne()
	nodal_DOF = input(default=1, option=DOF)
	allocate(ElementVector(num_node_per_elem*nodal_DOF) )
	ElementVector(:) = 0.0d0
	
	! (x1, y1, z1, x2, y2, z2 ...)
	do i=1,num_node_per_elem
		do j=1,nodal_DOF
			node_id = this%mesh%elemnod(ElementID,i)
			ElementVector( (i-1)*nodal_DOF + j) = &
				GlobalVector((node_id-1)*nodal_DOF + j )
		enddo
	enddo

end function
! ##########################################################################


! ##########################################################################
subroutine GlobalVectorFEMDomain(this,ElementID,ElementVector,DOF,Replace, Reset,GlobalVector)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: ElementID
	real(real64),intent(in) :: ElementVector(:) 
	real(real64),allocatable,intent(inout) :: GlobalVector(:)! size = number_of_node*DOF
	integer(int32),optional,intent(in) :: DOF
	logical,optional,intent(in) :: Replace, Reset
	integer(int32) :: i,j,k,num_node_per_elem, num_dim,nodal_DOF,node_id
	
	! For Element ID = ElementID, create ElementVector and return it
	! Number of Gauss Point = number of node per element, as default.

	num_node_per_elem = this%nne()
	nodal_DOF = input(default=1, option=DOF)

	if(.not. allocated(GlobalVector) )then
		GlobalVector = zeros(this%nn() * nodal_DOF )
	endif

	if(present(Replace) )then
		if(Replace)then
			GlobalVector = zeros(this%nn() * nodal_DOF )
		endif
	endif

	if(present(Reset) )then
		if(Reset)then
			GlobalVector = zeros(this%nn() * nodal_DOF )
		endif
	endif

	do j=1, this%nne() ! NNE : Number of Node per Element
		do k=1, nodal_DOF
			GlobalVector( (this%NodeID(ElementID,j)-1)*nodal_DOF + k ) = &
				ElementVector( (j-1)*nodal_DOF +  k )
		enddo
	enddo

end subroutine
! ##########################################################################

! ##########################################################################
function connectivityFEMDomain(this,ElementID) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32),intent(in) :: ElementID
	integer(int32),allocatable :: ret(:)

	allocate(ret(size(this%mesh%elemnod,2) ))
	ret(:) = this%mesh%elemnod(ElementID,:)

end function
! ##########################################################################

! ##########################################################################
function allconnectivityFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32),allocatable :: ret(:,:)

	ret = this%mesh%elemnod(:,:)

end function
! ##########################################################################

function selectFEMDomain(this,x_min,x_max,y_min,y_max,z_min,z_max,center,radius_range) result(NodeList)
	class(FEMDomain_),intent(in) :: this
	real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max,center(:),radius_range(1:2)
	real(real64) :: x(3),xmax(3),xmin(3),r
	integer(int32),allocatable :: NodeList(:),CheckList(:)
	logical :: InOut
	integer(int32) :: i,j,n

	CheckList = int(zeros(this%nn()) )
	xmin(1) = input(default=dble(-1.0e14),option=x_min )
	xmin(2) = input(default=dble(-1.0e14),option=y_min )
	xmin(3) = input(default=dble(-1.0e14),option=z_min )

	xmax(1) = input(default=dble(1.0e14),option=x_max )
	xmax(2) = input(default=dble(1.0e14),option=y_max )
	xmax(3) = input(default=dble(1.0e14),option=z_max )

	n = 0

	do i=1, this%nn()
		x(:)=this%mesh%nodcoord(i,:)


		if(present(center) .and. present(radius_range)  )then
			if(size(center)==2 )then
				!cylindrical
				r = norm( x(1:2) - center(1:2) )
				if( radius_range(1) <= r .and. r <= radius_range(2) )then
					if(xmin(3) <= x(3) .and. x(3) <= xmax(3) )then
						CheckList(i) = 1
					endif
				endif
			else
				!spherical
				r = norm(x-center)
				r = norm( x(1:2) - center(1:2) )
				if( radius_range(1) <= r .and. r <= radius_range(2) )then
					CheckList(i) = 1	
				endif
			endif
		else
			InOut = InOrOut(x=x,xmax=xmax,xmin=xmin,DimNum=this%nd() )
			if(InOut)then
				! inside
				CheckList(i) = 1
				!n=n+1
			endif
		endif
	enddo
	n = sum(CheckList)

	NodeList = int(zeros(n)  )
	
	if(n==0) return

	n=0
	do i=1,size(CheckList)
		if(CheckList(i)==1 )then
			n=n+1
			NodeList(n)=i
		endif
	enddo

end function
! ##########################################################################


! ##########################################################################
function NodeIDFEMDomain(this,ElementID,LocalNodeID) result(NodeID)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: ElementID,LocalNodeID
	integer(int32) :: NodeID

	NodeID = this%mesh%elemnod(ElementID,LocalNodeID)

end function
! ##########################################################################

subroutine killElementFEMDomain(this,blacklist,flag)
	class(FEMDomain_),intent(inout) :: this
	real(real64),allocatable :: new_nod_coord(:,:)
	integer(int32),allocatable :: elemnod_old(:,:),non_remove_node(:),new_node_id(:)
	integer(int32),optional,intent(in) :: blacklist(:),flag
	
	integer(int32) :: i,J,n,m,k
	logical :: survive

	! if(blacklist(i) == flag ) => kill ethe element
	elemnod_old = this%mesh%elemnod
	
	m = size(this%mesh%elemnod,2)
	k = size(this%mesh%elemnod,1)

	if(size(blacklist)/=k )then
		print *, "ERROR :: killElementFEMDomain >> should be size(blacklist)==k"
		return
	endif
	
	n=0
	do i=1,size(blacklist)
		if(blacklist(i)==flag )then
			n = n + 1
		endif
	enddo
	
	if(n==0)then
		return
	endif
	
	deallocate(this%mesh%elemnod)
	allocate(this%mesh%elemnod(k-n,m) )
	this%mesh%elemnod(:,:) = 0
	n=0
	do i=1, size(elemnod_old,1)
		
		if( blacklist(i)==flag )then
			cycle
		else
			n=n+1
			this%mesh%elemnod(n,:) = elemnod_old(i,:)
		endif
	enddo

	! if there are uncounted nodes, kill nodes
	non_remove_node = zeros(this%nn() )
	new_node_id = zeros(this%nn() )
	do i=1,this%ne()
		do j=1,this%nne()
			non_remove_node( this%mesh%elemnod(i,j) ) = 1
		enddo
	enddo

	if(non_remove_node(1)==1)then
		new_node_id(1) = 1
	else
		new_node_id(1) = 0
	endif

	do i=2,this%nn()
		new_node_id(i) = new_node_id(i-1) + non_remove_node(i) 
	enddo


	new_nod_coord = zeros( sum(non_remove_node),this%nd() )
	j=0
	do i=1,size(new_node_id)
		if(non_remove_node(i)==1 )then
			j = j + 1
			new_nod_coord( j,: ) = this%mesh%nodcoord(i,:)
		endif
	enddo

	do i=1,this%ne()
		do j=1,this%nne()
			this%mesh%elemnod(i,j) = new_node_id( this%mesh%elemnod(i,j) )
		enddo
	enddo
	this%mesh%nodcoord = new_nod_coord




end subroutine
! ###################################################################


! ###################################################################
function ConnectMatrixFEMDomain(this,position,DOF,shapefunction,strict) result(connectMatrix)
	class(FEMDomain_),intent(inout) :: this
	type(ShapeFunction_),optional,intent(in) :: shapefunction
	type(ShapeFunction_) :: sobj
	real(real64),intent(in) :: position(:)
	integer(int32),intent(in) :: DOF
	logical,optional,intent(in) :: strict
	real(real64),allocatable :: connectMatrix(:,:),cm_DOF1(:,:),Rcvec(:),Bc(:,:)
	integer(int32) :: i,j,n

	
	if(present(shapefunction) )then
		! Gauss-Point Projection
		! shapefunction=domain1: for 1 gauss point
		! obj = domain#2, nodes
		! sobj = domain#2, shape function
		! position : domain#1 gauss point
		
		! 

		! domain#2
		sobj = this%getShapeFunction(position=position)

		n = (this%nne()+size(shapefunction%nmat,1) ) * DOF
		
		if(sobj%elementid == -1)then
			! no contact
			connectMatrix = zeros(n,n)
			return
		endif
		
		Bc = zeros(DOF, n)

		!do i=1,DOF
		!	BC(i,i) = 1.0d0
		!enddo

		!allocate(Rcvec(n) )
		! <    Domain #1    > <    Domain #2    >
		! (N1 0  0 N2 0  0 ... -N1 0  0 -N2 0  0 ...   )
		! (0  N1 0 0  N2 0 ... 0  -N1 0 0  -N2 0 ...   )
		! (0  0 N1 0  0 N2 ... 0  0 -N1 0  0 -N2 ...   )
		if(present(strict) )then
			if(strict)then
				if(maxval(shapefunction%nmat(:))>1.0d0 .or. minval(shapefunction%nmat(:))<-1.0d0)then
					print *, "connectMatrix ERROR :::strict shape function is out of range"
					stop
				endif
			endif
		endif

		if(present(strict) )then
			if(strict)then
				if(maxval(sobj%nmat(:))>1.0d0 .or. minval(sobj%nmat(:))<-1.0d0)then
					print *, "connectMatrix ERROR :::strict shape function is out of range"
					stop
				endif
			endif
		endif

		! \epsilon \int_{x_e} Bc^T Bc detJ d x_e = 0
		do i=1,size(shapefunction%nmat)
			do j=1,DOF
				Bc(j, (i-1)*DOF + j ) =Bc(j, (i-1)*DOF + j )+ shapefunction%nmat(i)
			enddo
		enddo
		


		do i=1,size(sobj%nmat)
			do j=1,DOF
				Bc(j, size(shapefunction%nmat)*DOF + (i-1)*DOF + j ) =&
					Bc(j, size(shapefunction%nmat)*DOF + (i-1)*DOF + j )  - sobj%nmat(i)
			enddo
		enddo
		!print *, "position"
		!print *, position
		!print *, "shapefunction #1"
		!print *,shapefunction%nmat(:)
		!call print(shapefunction%ElemCoord)
		!call print(matmul(transpose(shapefunction%ElemCoord),shapefunction%nmat))
		
		!print *, "sobj #2"
		!print *,sobj%nmat(:)
		!call print(sobj%ElemCoord)
		!call print(matmul(transpose(sobj%ElemCoord),sobj%nmat))

		connectMatrix = matmul( transpose(Bc),Bc  )*shapefunction%detJ
		
		return
	
	else
		! P2P
		sobj = this%getShapeFunction(position=position)
		n = (this%nne()+1) * DOF
		
		if(sobj%elementid == -1)then
			! no contact
			connectMatrix = zeros(n,n)
			return
		endif
	
		n = (size(sobj%nmat)+1) * DOF
		Bc = zeros(DOF, n)
		do i=1,DOF
			BC(i,i) = 1.0d0
		enddo
		!allocate(Rcvec(n) )
		!Rcvec(1:DOF) = 1.0d0
		do i=1,size(sobj%nmat)
			do j=1,DOF
				!Rcvec(DOF+ (i-1)*DOF + j) = - sobj%nmat(i)
				Bc(j, i*DOF + j ) = - sobj%nmat(i)
			enddo
		enddo
		connectMatrix = matmul( transpose(Bc),Bc  )
		return
	endif
	
end function
! ##################################################################


! ###################################################################
function ConnectVectorFEMDomain(this,position,DOF,shapefunction,strict) result(Connectvector)
	class(FEMDomain_),intent(inout) :: this
	type(ShapeFunction_),optional,intent(in) :: shapefunction
	type(ShapeFunction_) :: sobj
	real(real64),intent(in) :: position(:)
	integer(int32),intent(in) :: DOF
	logical,optional,intent(in) :: strict
	real(real64),allocatable :: Connectvector(:),cm_DOF1(:,:),Rcvec(:),Bc(:,:)
	integer(int32) :: i,j,n

	
	if(present(shapefunction) )then
		! Gauss-Point Projection
		! shapefunction=domain1: for 1 gauss point
		! obj = domain#2, nodes
		! sobj = domain#2, shape function
		! position : domain#1 gauss point
		
		! 

		! domain#2
		sobj = this%getShapeFunction(position=position)

		n = (this%nne()+size(shapefunction%nmat,1) ) * DOF
		
		if(sobj%elementid == -1)then
			! no contact
			Connectvector = zeros(n)
			return
		endif
		
		Bc = zeros(DOF, n)

		!do i=1,DOF
		!	BC(i,i) = 1.0d0
		!enddo

		!allocate(Rcvec(n) )
		! <    Domain #1    > <    Domain #2    >
		! (N1 0  0 N2 0  0 ... -N1 0  0 -N2 0  0 ...   )
		! (0  N1 0 0  N2 0 ... 0  -N1 0 0  -N2 0 ...   )
		! (0  0 N1 0  0 N2 ... 0  0 -N1 0  0 -N2 ...   )
		if(present(strict) )then
			if(strict)then
				if(maxval(shapefunction%nmat(:))>1.0d0 .or. minval(shapefunction%nmat(:))<-1.0d0)then
					print *, "Connectvector ERROR :::strict shape function is out of range"
					stop
				endif
			endif
		endif

		if(present(strict) )then
			if(strict)then
				if(maxval(sobj%nmat(:))>1.0d0 .or. minval(sobj%nmat(:))<-1.0d0)then
					print *, "Connectvector ERROR :::strict shape function is out of range"
					stop
				endif
			endif
		endif

		! \epsilon \int_{x_e} Bc^T Bc detJ d x_e = 0
		do i=1,size(shapefunction%nmat)
			do j=1,DOF
				Bc(j, (i-1)*DOF + j ) =Bc(j, (i-1)*DOF + j )+ shapefunction%nmat(i)
			enddo
		enddo
		


		do i=1,size(sobj%nmat)
			do j=1,DOF
				Bc(j, size(shapefunction%nmat)*DOF + (i-1)*DOF + j ) =&
					Bc(j, size(shapefunction%nmat)*DOF + (i-1)*DOF + j )  - sobj%nmat(i)
			enddo
		enddo
		!print *, "position"
		!print *, position
		!print *, "shapefunction #1"
		!print *,shapefunction%nmat(:)
		!call print(shapefunction%ElemCoord)
		!call print(matmul(transpose(shapefunction%ElemCoord),shapefunction%nmat))
		
		!print *, "sobj #2"
		!print *,sobj%nmat(:)
		!call print(sobj%ElemCoord)
		!call print(matmul(transpose(sobj%ElemCoord),sobj%nmat))

		Connectvector = reshape(Bc,[size(Bc,1)*size(Bc,2) ] )*shapefunction%detJ
		
		return
	
	else
		! P2P
		sobj = this%getShapeFunction(position=position)
		n = (this%nne()+1) * DOF
		
		if(sobj%elementid == -1)then
			! no contact
			Connectvector = zeros(n)
			return
		endif
	
		n = (size(sobj%nmat)+1) * DOF
		Bc = zeros(DOF, n)
		do i=1,DOF
			BC(i,i) = 1.0d0
		enddo
		!allocate(Rcvec(n) )
		!Rcvec(1:DOF) = 1.0d0
		do i=1,size(sobj%nmat)
			do j=1,DOF
				!Rcvec(DOF+ (i-1)*DOF + j) = - sobj%nmat(i)
				Bc(j, i*DOF + j ) = - sobj%nmat(i)
			enddo
		enddo
		Connectvector = reshape(Bc,[size(Bc,1)*size(Bc,2) ] )
		return
	endif
	
end function
! ##################################################################

! ##################################################################
subroutine ImportVTKFileFEMDomain(this,name)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	type(IO_) :: f
	character(len=:),allocatable :: fullname, line,fieldname
	integer(int32) :: i,j,k,n,from,to,m,numnode,numline,POINT_DATA
	integer(int32),allocatable :: CELLS(:),CELL_TYPES(:)
	logical :: ASCII=.false.
	logical :: UNSTRUCTURED_GRID=.false.

	! Only for POINTS, CELLS, CELL_TYPES, VECTORS, TENSORS, SCALARS
	
	call this%remove()

	if( index(name,".vtk")==0 .and. index(name,".VTK")==0 )then
		fullname = name//".vtk"
	else
		fullname = name
	endif

	call f%open(fullname)
	
	! read settings
	do
		if(f%EOF) exit
		line = f%readline()
		line = adjustl(line)
		if(index( line(1:1),"#") /=0 )cycle
		
		if(index( line,"ASCII") /=0 )then
			ASCII = .true.
			cycle
		endif

		if(index( line,"DATASET") /=0 )then
			if( index( line,"UNSTRUCTURED_GRID") /=0 )then
				UNSTRUCTURED_GRID = .true.
			endif
			cycle
		endif
		if(index( line,"POINTS") /=0  )then
			exit
		endif

		if(index( line,"CELLS") /=0 .or. index( line,"cells") /=0  )then
			exit
		endif

		if(index( line,"VECTOR") /=0 .or. index( line,"vector") /=0  )then
			exit
		endif

		if(index( line,"TENSOR") /=0 .or. index( line,"tensor") /=0  )then
			exit
		endif

		if(index( line,"SCALAR") /=0 .or. index( line,"scalar") /=0  )then
			exit
		endif
	enddo

	! check vtk file
	if(ASCII)then
		if(this%debug_mode)then
			print *, "[ok] ASCII format."
		endif
	else
		print *, "ERROR :: importVTKFile >> here, vtk file should be ASCII format."
		stop
	endif
	
	if(UNSTRUCTURED_GRID)then
		if(this%debug_mode)then
			print *, "[ok] UNSTRUCTURED_GRID"
		endif
	else
		print *, "ERROR :: importVTKFile >> here, DATASET should be UNSTRUCTURED_GRID"
		stop
	endif
	
	if(f%EOF)then
		print *,"ERROR ;; importVTKFile >> no readable found in the file!"
		stop
	endif

	do
		if(f%EOF)exit
		if(index( line,"POINTS") /=0  )then
			from = index( line,"POINTS") + 6
			read( line(from:),* ) n
			allocate(this%mesh%nodcoord(n,3) )
			do i=1,n
				line = f%readline()
				read(line,*) this%mesh%nodcoord(i,:)
			enddo
		endif

		if(index( line,"CELLS") /=0  )then
			from = index( line,"CELLS") + 5
			read( line(from:),* ) n, m
			allocate(CELLS(m) )
			numline=0
			do i=1,n
				line = f%readline()
				read(line,*) numnode
				read(line,*) CELLS(numline+1:numline+numnode+1)
				CELLS(numline+2:numline+numnode+1) = CELLS(numline+2:numline+numnode+1)+1
				numline = numline + numnode + 1
			enddo
		endif

		if(index( line,"CELL_TYPES") /=0  )then
			from = index( line,"CELL_TYPES") + 10
			read( line(from:),* ) n
			if(.not.allocated(CELLS) )then
				print *, "ERROR :: importVTKFile >> no CELLS are found before CELL_TYPES."
				stop
			endif
			allocate(CELL_TYPES(n) )
			do i=1,n
				line = f%readline()
				read(line,*) CELL_TYPES(i)
			enddo

			! cannot use mixed mesh for PlantFEM

			if(maxval(CELL_TYPES)/=minval(CELL_TYPES) )then
				print *, "[Caution] :: importVTKFile >> cannot use mixed mesh for PlantFEM"
				print *, "Only CELL_TYPES = ",maxval(CELL_TYPES),"will be imported."
				n = 0
				do i=1,size(CELL_TYPES)
					if(CELL_TYPES(i)==maxval(CELL_TYPES) )then
						n=n+1
					endif
				enddo
			else
				n = size(CELL_TYPES)
			endif

			m = maxval(CELL_TYPES)
			select case(m)
			case(VTK_VERTEX)
				numnode=1
			case(VTK_POLY_VERTEX)
				numnode=1
			case(VTK_LINE)
				numnode=2
			case(VTK_TRIANGLE)
				numnode=3
			case(VTK_PIXEL)
				numnode=4
			case(VTK_QUAD)
				numnode=4
			case(VTK_TETRA)
				numnode=4
			case(VTK_VOXEL)
				numnode=8
			case(VTK_HEXAHEDRON)
				numnode=8
			case(VTK_WEDGE)
				numnode=6
			case(VTK_QUADRATIC_EDGE)
				numnode=3
			case(VTK_QUADRATIC_TRIANGLE)
				numnode=6
			case(VTK_QUADRATIC_QUAD)
				numnode=8
			case(VTK_QUADRATIC_TETRA)
				numnode=10
			case(VTK_QUADRATIC_HEXAHEDRON)
				numnode=16
			
			end select


			allocate(this%mesh%elemnod(n,numnode) )
			
			this%mesh%elemnod(:,:) = 0
			n=0
			do i=1,this%ne() 
				do
					if(n+1 > size(CELLS) ) exit
					if(CELLS(n+1)==numnode )then
						this%mesh%elemnod(i,1:numnode) = CELLS(n+2:n+numnode+1)
						n=n+1+numnode
						exit
					else
						n=n+1+numnode
						cycle
					endif
				enddo
			enddo

		endif

		if(index( line,"POINT_DATA") /=0  )then
			from = index( line,"POINT_DATA") + 10
			read( line(from:),* ) POINT_DATA
		endif

		if(index( line,"CELL_DATA") /=0  )then
			from = index( line,"CELL_DATA") + 10
			read( line(from:),* ) POINT_DATA
		endif


		if(index( line,"SCALARS") /=0  )then
			from = index( line,"SCALARS") + 7
			to = index( line(from+1:)," ")
			fieldname=line(from:to+7)
			if(.not.allocated(this%PhysicalField) )then
				allocate(this%PhysicalField(1) )
				do i=1,size(this%physicalfield)
				this%PhysicalField(i)%name = "untitled"
				enddo
			endif
			! read "LOOKUP_TABLE default"
			line = f%readline()
			
			do i=1,size(this%PhysicalField)
				if(allocated(this%PhysicalField(i)%scalar ) )then
					cycle
				elseif(allocated(this%PhysicalField(i)%vector ) )then
					cycle
				elseif(allocated(this%PhysicalField(i)%tensor ) )then
					cycle
				else
					allocate(this%PhysicalField(i)%scalar(POINT_DATA) )
					this%PhysicalField(i)%name = fieldname
					this%PhysicalField(i)%scalar(:) = 0.0d0
					do j=1,POINT_DATA
						line = f%readline()
						read(line,*)this%PhysicalField(i)%scalar(j) 
					enddo
					if(this%debug_mode)then
						print *, "[ok] Read SCALAR field"
					endif
				endif
			enddo
		endif
		

		
		if(index( line,"VECTORS") /=0  )then
			from = index( line,"VECTORS") + 7
			to = index( line(from+1:)," ")
			fieldname=line(from:to+7)
			if(.not.allocated(this%PhysicalField) )then
				allocate(this%PhysicalField(1) )
				do i=1,size(this%physicalfield)
				this%PhysicalField(i)%name = "untitled"
				enddo
			endif
			! read "LOOKUP_TABLE default"
			line = f%readline()
			
			do i=1,size(this%PhysicalField)
				if(allocated(this%PhysicalField(i)%scalar ) )then
					cycle
				elseif(allocated(this%PhysicalField(i)%vector ) )then
					cycle
				elseif(allocated(this%PhysicalField(i)%tensor ) )then
					cycle
				else
					allocate(this%PhysicalField(i)%vector(POINT_DATA,3) )
					this%PhysicalField(i)%name = fieldname
					this%PhysicalField(i)%vector(:,:) = 0.0d0
					do j=1,POINT_DATA
						line = f%readline()
						read(line,*)this%PhysicalField(i)%vector(j,:) 
						
					enddo
					exit
				endif
			enddo
		endif
		

		
		if(index( line,"TENSORS") /=0  )then
			from = index( line,"TENSORS") + 7
			to = index( line(from+1:)," ")
			fieldname=line(from:to+7)
			if(.not.allocated(this%PhysicalField) )then
				allocate(this%PhysicalField(100) )
				do i=1,size(this%physicalfield)
				this%PhysicalField(i)%name = "untitled"
				enddo
			endif
			! read "LOOKUP_TABLE default"
			line = f%readline()
			
			do i=1,size(this%PhysicalField)
				if(allocated(this%PhysicalField(i)%scalar ) )then
					cycle
				elseif(allocated(this%PhysicalField(i)%vector ) )then
					cycle
				elseif(allocated(this%PhysicalField(i)%tensor ) )then
					cycle
				else
					allocate(this%PhysicalField(i)%tensor(POINT_DATA,3,3) )
					this%PhysicalField(i)%name = fieldname
					this%PhysicalField(i)%tensor(:,:,:) = 0.0d0
					do j=1,POINT_DATA
						do k=1,3
							line = f%readline()
							read(line,*)this%PhysicalField(i)%tensor(j,k,:) 
						enddo
					enddo

				endif
				exit
			enddo
		endif
		
		
		

		line = f%readline()
	enddo
	call f%close()


end subroutine
! ##################################################################

function getElementFEMDOmain(this,ElementID) result(element)
	class(FEMDomain_),intent(in) :: this
	type(FEMDomain_) :: element
	integer(int32),intent(in) :: ElementID

	element%mesh = this%mesh%getelement(ElementID)

end function
! ##################################################################

! ##################################################################
subroutine Delaunay3DFEMDomain(this)
	class(FEMDomain_),intent(inout) :: this

	if(.not. allocated(this%mesh%nodcoord) )then
		print *, "ERROR :: Delauney3DFEMDomain >> no nodes are found in femdomain%mesh%nodcoord(:,:)"
	endif
	call this%mesh%meshing(mode=3)

end subroutine
! ##################################################################

! ##################################################################
subroutine Delaunay2DFEMDomain(this)
	class(FEMDomain_),intent(inout) :: this

	if(.not. allocated(this%mesh%nodcoord) )then
		print *, "ERROR :: Delauney3DFEMDomain >> no nodes are found in femdomain%mesh%nodcoord(:,:)"
	endif
	call this%mesh%meshing(delaunay2d=.true.)

end subroutine
! ##################################################################


! ##################################################################
function xFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	real(real64),allocatable :: ret(:)

	if(this%mesh%empty() )then
		ret = zeros(1)
	else
		allocate(ret(this%nn() ) )
		ret(:) = this%mesh%nodcoord(:,1) 
	endif

end function
! ##################################################################

! ##################################################################
function yFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	real(real64),allocatable :: ret(:)

	if(this%mesh%empty() )then
		ret = zeros(1)
	else
		allocate(ret(this%nn() ) )
		ret(:) = this%mesh%nodcoord(:,2) 
	endif

end function
! ##################################################################

! ##################################################################
function zFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	real(real64),allocatable :: ret(:)

	if(this%mesh%empty() )then
		ret = zeros(1)
	else
		allocate(ret(this%nn() ) )
		ret(:) = this%mesh%nodcoord(:,3) 
	endif

end function
! ##################################################################

function TractionVectorFEMDomain(this,displacement,YoungModulus,PoissonRatio,debug_elementID) result(Traction)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: displacement(:),YoungModulus(:),PoissonRatio(:)
	real(real64),allocatable :: Traction(:)
	real(real64),allocatable :: Dmat(:,:), Bmat(:,:),Te(:),Teg(:),ElemDisp(:,:),Tem(:,:),Disp_vec(:,:)
	real(real64),allocatable :: StressVector(:)
	integer(int32),optional,intent(in) :: debug_elementID
	type(ShapeFunction_) :: sf
	type(IO_) :: f
	integer(int32) :: i,j

	if(this%mesh%empty() )then
		return
	endif

	Traction = zeros(this%nn()*this%nd() ) 
	
	ElemDisp = zeros(this%nne(),this%nd() )

	! For each element
	do i=1, this%ne()
		if(present(debug_elementID) )then
			if(debug_elementID==i)then
				call f%open("TractionVector___debug_msg.txt")
			endif
		endif
		! For each integration point
		do j=1, this%ngp()
			! Compute traction vector
			! (1) get shape function
			sf = this%getShapeFunction(&
				ElementID=i,GaussPointID=j)
			! get B-matrix
			Bmat = this%BMatrix(&
				shapefunction=sf,ElementID=i)
			! get Element-wise displacement vector
			ElemDisp = selectRow(&
				Matrix=reshape(Displacement,this%nn(),this%nd()),  &
				RowIDs=this%connectivity(ElementID=i) )
			
			! get Stress vector
!  <<<<bug>>>>>
			StressVector = this%StressVector(&
				ElementID=i,GaussPoint=j,disp= ElemDisp,&
					E = YoungModulus(i),v=PoissonRatio(i) )
			! get elemental traction vector
!  <<<<bug>>>>>
			!Tem = matmul(transpose(Bmat),matmul(this%Dmatrix(E = YoungModulus(i),v=PoissonRatio(i)),&
			!matmul(Bmat,reshape(transpose(ElemDisp),[size(ElemDisp,1)*size(ElemDisp,2),1] )  )))*sf%detJ
			
			Te = matmul(transpose(Bmat),StressVector)*sf%detJ
			!Te = reshape(Tem,[size(Tem,1)*size(Tem,2)])
			
			! add to global vector
			Traction = Traction + this%asGlobalVector(LocalVector=Te,ElementID=i,DOF=this%nd() )
			if(present(debug_elementID) )then
				if(debug_elementID==i)then
				
					write(f%fh,*) "TractionVector >> debug mode:"
					write(f%fh,*) "ElementID, GaussPointID",i,j
					write(f%fh,*) "ElemDisp"
					call f%write(ElemDisp)
					write(f%fh,*) "ElemDisp(vector form)"
					Disp_vec =  reshape(transpose(ElemDisp),[size(ElemDisp,1)*size(ElemDisp,2),1 ])
					call f%write(Disp_vec)
					write(f%fh,*) "Bmat"
					call f%write(Bmat)
					write(f%fh,*) "StressVector"
					call f%write(StressVector)
					write(f%fh,*) "%StressVector"
					call f%write(this%StressVector(&
						ElementID=i,GaussPoint=j,disp= ElemDisp,&
							E = YoungModulus(i),v=PoissonRatio(i) ))
					write(f%fh,*) "Tem"
					call f%write(Tem)
					call f%write("Dmat")
					call f%write(this%Dmatrix(E = YoungModulus(i),v=PoissonRatio(i)) )
					write(f%fh,*) "Traction(element)"
					call f%write(Te)

					write(f%fh,*) "*matmul(Bmat,ElemDisp)"
					call f%write(matmul(Bmat,Disp_vec  ))					

					write(f%fh,*) "*matmul(Dmat,matmul(Bmat,ElemDisp))"
					call f%write(matmul(this%Dmatrix(E = YoungModulus(i),v=PoissonRatio(i)),&
						matmul(Bmat,Disp_vec  )))
					
					write(f%fh,*) "*sf%detJ*matmul(Dmat,matmul(Bmat,ElemDisp))"
					call f%write(sf%detJ*matmul(this%Dmatrix(E = YoungModulus(i),v=PoissonRatio(i)),&
						matmul(Bmat,Disp_vec  )))
				endif
			endif

		enddo	
		if(present(debug_elementID) )then
			if(debug_elementID==i)then
				call f%close()
			endif
		endif
	enddo

end function
! ##################################################################

!pure function SymmetryMatrixToVector(symmetryMatrix) result(vec)
!	real(real64),intent(in) :: SymmetryMatrix(:,:)
!	real(real64),allocatable :: vec(:)
!	integer(int32) :: dim_mat, dim_vec,k
!
!   [Caution] DO NOT USE THIS
!	! A11  A12  A13
!	! A12  A22  A23
!	! A13  A23  A33
!	! =>
!	! A11
!	! A22
!	! A33
!	! A12
!	! A13
!	! A23
!	dim_mat = size(SymmetryMatrix,1)
!	dim_vec = dim_mat
!	do i=dim_mat-1,1,-1
!		dim_vec = dim_vec+1
!	enddo
!
!	vec     = zeros(dim_vec)
!	do i=1,dim_mat
!		vec(i) = SymmetriMatrix(i,i)
!	enddo
!	k=0
!	do i=1,dim_mat-1
!		do j=i+1,dim_mat
!			k = k+1
!			vec(dim_mat+k) = SymmetriMatrix(i,i)
!		enddo
!	enddo
!	
!	
!
!end function
function asGlobalVectorFEMDomain(this,LocalVector,ElementID,DOF) result(globalvec)
	class(FEMDomain_),intent(in) :: this
	real(real64),intent(in):: LocalVector(:)
	integer(int32),intent(in) :: ElementID,DOF
	real(real64),allocatable :: globalvec(:)
	integer(int32) :: i,j,n, ng
	integer(int32), allocatable :: connectivity(:)
	

	n = this%nn()*DOF
	globalvec = zeros(n)

	! globalvec = (A1x, A1y, A1z, A2x, A2y, A2z, ...  ) 

	connectivity= this%connectivity(ElementID=ElementID)
	do i=1,this%nne()
		do j=1, DOF
			n = DOF*(i-1) + j
			ng= DOF*(connectivity(i)-1) + j
			globalvec(ng) = globalvec(ng) + LocalVector(n) 
		enddo
	enddo



end function
! #########################################################################



! #########################################################################
function getNodeListFEMDomain(this,BoundingBox,xmin,xmax,ymin,ymax,zmin,zmax) result(NodeList)
	class(FEMDomain_),intent(inout) :: this
	type(FEMDomain_),optional,intent(inout) :: BoundingBox
	real(real64),optional,intent(in) :: xmin,xmax,ymin,ymax,zmin,zmax
	integer(int32),allocatable :: NodeList(:)


	NodeList = this%mesh%getNodeList(BoundingBox=BoundingBox%mesh &
	,xmin=xmin &
	,xmax=xmax &
	,ymin=ymin &
	,ymax=ymax &
	,zmin=zmin &
	,zmax=zmax)

end function
! #########################################################################

! #########################################################################
function getFacetListFEMDomain(this,NodeID) result(FacetList)
    class(FEMDomain_),intent(inout) :: this
    integer(int32),intent(in) :: NodeID
    integer(int32),allocatable :: FacetList(:,:) ! Node-ID =  FacetList(FacetID, LocalNodeID ) 

	FacetList = this%mesh%getFacetList(NodeID=NodeID)

end function
! #########################################################################


function getElementListFEMDomain(this,BoundingBox,xmin,xmax,ymin,ymax,zmin,zmax,NodeID) result(ElementList)
    class(FEMDomain_),intent(inout) :: this
    type(FEMDomain_),optional,intent(inout) :: BoundingBox
    real(real64),optional,intent(in) :: xmin,xmax,ymin,ymax,zmin,zmax
    integer(int32),optional,intent(in) :: NodeID
    integer(int32),allocatable :: NodeList(:)
    integer(int32),allocatable :: ElementList(:)

	ElementList= this%mesh%getElementList(BoundingBox=BoundingBox%mesh &
		,xmin=xmin &
		,xmax=xmax &
		,ymin=ymin &
		,ymax=ymax &
		,zmin=zmin &
		,zmax=zmax &
		,NodeID=NodeID)

end function
! #########################################################################


function getElementList_by_radiusFEMDomain(this,center,radius,zmin,zmax) result(ElementList)
    class(FEMDomain_),intent(inout) :: this
    real(real64),intent(in) :: center(1:2),radius,zmin,zmax
    real(real64),allocatable :: elem_center(:)

    integer(int32),allocatable :: checklist(:)
    integer(int32),allocatable :: ElementList(:)
	integer(int32) :: i,j,k
	
	CheckList = int(zeros(this%ne() ))
	do i=1, this%ne() 
		elem_center = this%centerPosition(i)
		if(norm(elem_center(1:2)-center(1:2))<=radius ) then
			if(zmin <= elem_center(3) .and. elem_center(3) <=zmax )then
				CheckList(i) = 1
			endif
		endif
	enddo

	ElementList = int(zeros(sum(CheckList) ) )
	k=0
	do i=1,size(checklist)
		if(checklist(i)==1)then
			k = k + 1
			ElementList(k) = i
		else
			cycle
		endif
	enddo


end function

! #########################################################################

pure function selectRow(Matrix, RowIDs) result(SelectedRows)
	real(real64),intent(in) :: Matrix(:,:)
	integer(int32),intent(in) :: RowIDs(:)
	real(real64),allocatable :: SelectedRows(:,:)
	integer(int32) :: i
	
	! get rows from Matrix by rowIDs
	SelectedRows = zeros(size(RowIDs),size(Matrix,2) )

	do concurrent (i=1:size(RowIDs))
		SelectedRows(i,:) = Matrix(RowIDs(i), : )	
	enddo

end function

! ########################################



pure function emptyFEMDomain(this) result(FEMDomain_is_empty)
    class(FEMDomain_),intent(in) :: this
    logical :: FEMDomain_is_empty

    FEMDomain_is_empty = this%mesh%empty()

end function
! ########################################


! ########################################
function appendfemdomain(x,y)  result(z)
	class(FEMDomain_),intent(in) :: x, y
	type(FEMDomain_) :: z
	integer(int32) :: n,m

	if(x%empty() )then
		z = y
	elseif(y%empty() )then
		z = x
	else
		! add members
		! F**kin supid algorithm

		n = x%nn() + y%nn()
		m = maxval([x%nd(),y%nd()])
		z%mesh%nodcoord = zeros(n,m)
		z%mesh%nodcoord(       1:         x%nn(),:) = x%mesh%nodcoord(1:x%nn(),:)
		z%mesh%nodcoord(x%nn()+1: x%nn()+ y%nn(),:) = y%mesh%nodcoord(1:y%nn(),:)
		
		n = x%ne() + y%ne()
		m = maxval([x%nne(),y%nne()])
		z%mesh%elemnod  = zeros(n,m)
		z%mesh%elemnod(        1:          x%ne(),:) = x%mesh%elemnod(1:x%ne(),:)
		z%mesh%elemnod( x%ne()+1: x%ne() + y%ne(),:) = y%mesh%elemnod(1:y%ne(),:) + x%nn()

	endif

end function appendFEMDomain
! ########################################

subroutine fixReversedElementsFEMDomain(this)
	class(FEMDomain_),intent(inout) :: this
	real(real64) :: volume
    integer(int32) :: i,j
	integer(int32),allocatable :: elemnod(:)

	if(this%mesh%empty() )then
		return
	else
		! fix reversed elements
		!!$OMP parallel do default(shared) private(elemnod,volume)
		do i=1,this%mesh%ne()
			volume = this%getVolume(elem=i)
			if(volume < 0.0d0) then
				elemnod = this%mesh%elemnod(i,:) 
				
				if(this%nne()==8 .and. this%nd()==3 )then
					this%mesh%elemnod(i,1) = elemnod(4)
					this%mesh%elemnod(i,2) = elemnod(3)
					this%mesh%elemnod(i,3) = elemnod(2)
					this%mesh%elemnod(i,4) = elemnod(1)
					this%mesh%elemnod(i,5) = elemnod(8)
					this%mesh%elemnod(i,6) = elemnod(7)
					this%mesh%elemnod(i,7) = elemnod(6)
					this%mesh%elemnod(i,8) = elemnod(5)
				elseif(this%nne()==4 .and. this%nd()==3 )then
					this%mesh%elemnod(i,1) = elemnod(3)
					this%mesh%elemnod(i,2) = elemnod(2)
					this%mesh%elemnod(i,3) = elemnod(1)
					this%mesh%elemnod(i,4) = elemnod(4)
				elseif(this%nne()==4 .and. this%nd()==2 )then
					this%mesh%elemnod(i,1) = elemnod(4)
					this%mesh%elemnod(i,2) = elemnod(3)
					this%mesh%elemnod(i,3) = elemnod(2)
					this%mesh%elemnod(i,4) = elemnod(1)
				elseif(this%nne()==3 .and. this%nd()==2 )then
					this%mesh%elemnod(i,1) = elemnod(3)
					this%mesh%elemnod(i,2) = elemnod(2)
					this%mesh%elemnod(i,3) = elemnod(1)
				else
					print *, "[ERROR] >> fixReversedElementsFEMDomain"
					print *, "Element with ",this%nne(),"nne and",this%nd(),"this%nd()"
					print *, "is not impremented yet."
					stop
				endif
			endif
		enddo
		!!OMP end parallel do
	endif
end subroutine

!function getNumberOfPointFEMDomain(this,xmin,) result(ret)
!	class(FEMDomain_),intent(in) :: this
!end function

subroutine syncFEMDomain(this,from,mpid)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: from
	type(MPI_),intent(inout) :: mpid

	call this%mesh%sync(from=from, mpid=mpid)
end subroutine


subroutine syncFEMDomainVector(this,from,mpid)
	type(FEMDomain_),allocatable,intent(inout) :: this(:)
	integer(int32),intent(in) :: from
	type(MPI_),intent(inout) :: mpid
	integer(int32) :: vec_size, i

	vec_size=0
	if(mpid%myrank==from)then
		if(.not.allocated(this) )then
			vec_size = -1
		endif
	endif
	call mpid%bcast(from=from,val=vec_size)
	if(vec_size<1)then
		return
	endif

	if(from /= mpid%myrank)then
		if(allocated(this) )then
			deallocate(this)
		endif
		allocate(this(vec_size) )
	endif

	do i=1,vec_size
		call this(i)%mesh%sync(from=from, mpid=mpid)
	enddo

end subroutine

! ###################################################################
function getScalarFieldFEMDomain(this,xr,yr,zr,entryvalue,default) result(ScalarField)
	class(FEMDomain_),intent(in) :: this
	real(real64),intent(in) :: xr(2),yr(2),zr(2),default(:),entryvalue
	real(real64),allocatable:: ScalarField(:)
	real(real64) :: x(3)
	integer(int32) :: i,j,n
	logical :: empty_field
	
	n = size(default)
	if(n==this%nn() )then
		! node-wise
		ScalarField = default
		do i=1,this%nn()
			if( xr(1) <= this%position_x(i) .and. this%position_x(i) <= xr(2)  )then
				if( yr(1) <= this%position_y(i) .and. this%position_y(i) <= yr(2)  )then
					if( zr(1) <= this%position_z(i) .and. this%position_z(i) <= zr(2)  )then
						ScalarField(i) = entryvalue
					endif
				endif
			endif
		enddo
	elseif(n==this%ne())then
		! element-wise
		
		ScalarField = default
		do i=1,this%ne()
			x = this%centerPosition(i)
			if( xr(1) <= x(1) .and. x(1) <= xr(2)  )then
				if( yr(1) <= x(2) .and. x(2) <= yr(2)  )then
					if( zr(1) <= x(3) .and. x(3) <= zr(2)  )then
						ScalarField(i) = entryvalue
					endif
				endif
			endif
		enddo
	else
		! none of above
		print *, "ERROR :: getScalarFieldFEMDomain >>"
		print *, "size(default) should be femdomain%ne() or femdomain%nn()"
		return
	endif

	


end function
! ###################################################################

function getE2EconnectivityFEMDomain(this) result(E2Econnect)
	class(FEMDomain_),intent(in) :: this
	integer(int32),allocatable :: E2Econnect(:,:),elemnodid(:),GroupID(:,:),element_id_list(:)
	integer(int32) :: i,j,k,efacet_id(6,4),gfacet_id(6,4),l
	integer(int32) :: exists_count
	! Element-to-Element connectivity
	! only for 3-D cube elements

	integer(int32) :: group_id,num_elem,ii,jj

	if(this%mesh%empty()) then
		return
	endif

		
	! O(1025*1025*NlogN) algorithm
	allocate(E2Econnect(this%ne(),6) )
	E2Econnect(:,:) = -1
	elemnodid = zeros(this%nne())

	! only for 8-node isoparametric element
	efacet_id(1,1:4) = [1,2,6,5]
	efacet_id(2,1:4) = [2,3,7,6]
	efacet_id(3,1:4) = [3,4,8,7]
	efacet_id(4,1:4) = [4,1,5,8]
	efacet_id(5,1:4) = [1,2,3,4]
	efacet_id(6,1:4) = [5,6,7,8]
	
	GroupID = this%mesh%BinaryTreeSearch(old_GroupID=GroupID,min_elem_num=2000)
	! for each group IDs
	

	do group_id = 1, size(GroupID,1)
		num_elem=0
		do i=1,size(GroupID,2)
			if(GroupID(group_id,i)<1 )then
				exit
			else
				num_elem = num_elem + 1
			endif
		enddo
		if(num_elem<=1)then
			cycle
		endif
		element_id_list = int(zeros(num_elem) )
		element_id_list(1:num_elem) = GroupID(group_id,1:num_elem)
		
		do ii=1,size(element_id_list)
			i = element_id_list(ii)
			elemnodid = this%mesh%elemnod(i,:)

			do j=1,6
				do k=1,4
					gfacet_id(j,k) = this%mesh%elemnod(i,efacet_id(j,k) )
				enddo
			enddo

			do jj=1,size(element_id_list)
				j = element_id_list(jj)
				
				if(i==j) cycle
				
				
				if(minval(this%mesh%elemnod(j,:)) > maxval(gfacet_id) ) cycle
				if(maxval(this%mesh%elemnod(j,:)) < minval(gfacet_id) ) cycle
				
				do k=1,size(gfacet_id,1)
					exists_count = 0
					do l=1,size(gfacet_id,2)
						if( exists(vector=this%mesh%elemnod(j,:),val=gfacet_id(k,l) ) )then
							exists_count = exists_count+1
						endif
					enddo
					if(exists_count==4)then
						E2Econnect(i,k) = j
					else
						cycle
					endif
				enddo

			enddo

		enddo
	enddo
	

end function
! ###################################################################

! ###################################################################
function MovingAverageFilterFEMDomain(this,inScalarField,ignore_top_and_bottom) result(outScalarField)
	class(FEMDomain_),intent(in) :: this
	real(real64),intent(in) :: inScalarField(:)
	real(real64),allocatable:: outScalarField(:),neighborvalue(:) 
	logical,optional,intent(in) :: ignore_top_and_bottom
	integer(int32),allocatable :: E2Econnect(:,:),buf(:,:)
	integer(int32) :: i,j
	
	integer(int32) :: count_zero

	if(this%mesh%empty() )then
		return
	endif

	if(this%ne() /= size(inScalarField) )then
		!print *, "ERROR :: MovingAverageFilterFEMDomain >> only for element-wise scalar fields"
		return
	endif

	E2Econnect = this%getE2Econnectivity()
	!Element_Groups = this%mesh%BinaryTreeSearch(old_GroupID=GroupID,min_elem_num=10000)

	if(present(ignore_top_and_bottom) )then
		if(ignore_top_and_bottom)then
			buf = E2Econnect
			deallocate(E2Econnect)
			E2Econnect = buf(:,1:4)
		endif
	endif
	neighborvalue = zeros(size(E2Econnect,2) )
	outScalarField = inScalarField
	
	!移動平均フィルタ

	do i=1,this%ne()
		count_zero = 0
		neighborvalue = 0.0d0
		do j=1,size(E2Econnect,2)
			if(E2Econnect(i,j) <1 )then
				count_zero = count_zero+1
				cycle
			endif
			neighborvalue(j) = inScalarField( E2Econnect(i,j) )	
		enddo
		outScalarField(i) = (sum(neighborvalue)+inScalarField(i))&
			/dble(size(neighborvalue)+1-count_zero)
		
	enddo



end function
! ###################################################################


! #########################################################
pure function xminFEMDomain(this) result(ret)
    class(FEMDomain_),intent(in) :: this
    real(real64) :: ret

    ret = minval(this%mesh%nodcoord(:,1))

end function
! #########################################################


! #########################################################
pure function xmaxFEMDomain(this) result(ret)
    class(FEMDomain_),intent(in) :: this
    real(real64) :: ret

    ret = maxval(this%mesh%nodcoord(:,1))

end function
! #########################################################

! #########################################################
pure function yminFEMDomain(this) result(ret)
    class(FEMDomain_),intent(in) :: this
    real(real64) :: ret

    ret = minval(this%mesh%nodcoord(:,2))

end function
! #########################################################


! #########################################################
pure function ymaxFEMDomain(this) result(ret)
    class(FEMDomain_),intent(in) :: this
    real(real64) :: ret

    ret = maxval(this%mesh%nodcoord(:,2))

end function
! #########################################################

! #########################################################
pure function zminFEMDomain(this) result(ret)
    class(FEMDomain_),intent(in) :: this
    real(real64) :: ret

    ret = minval(this%mesh%nodcoord(:,3))

end function
! #########################################################


! #########################################################
function zmaxFEMDomain(this,x,y, debug) result(ret)
    class(FEMDomain_),intent(in) :: this
    real(real64),optional,intent(in) :: x, y
	real(real64) :: ret
	integer(int32) :: i
	real(real64),allocatable :: z_coord(:),xy_minmax(:,:)
	real(real64) :: z_min
	logical,allocatable :: inside(:)
	logical,optional,intent(in) :: debug
	logical :: debug_mode = .true.

	debug_mode = input(default=.false.,option=debug)
	if(present(x) .and. (present(y) ) )then
		xy_minmax = zeros(this%ne(),4 ) !xmin,ymin,xmax,ymax
		z_coord = zeros(this%ne() )
		allocate(inside(this%ne()) )
		inside(:) = .false.
		z_min = this%zmin()

		if(debug_mode) then
			print *, "[0] zmax >> started"
		endif
		!$OMP parallel default(shared) 
		!$OMP do
		do i=1,this%ne()
			xy_minmax(i,1) = minval(this%mesh%nodcoord( this%mesh%elemnod(i,:),1))!xmin
			xy_minmax(i,2) = minval(this%mesh%nodcoord( this%mesh%elemnod(i,:),2))!ymin
			xy_minmax(i,3) = maxval(this%mesh%nodcoord( this%mesh%elemnod(i,:),1))!xmax
			xy_minmax(i,4) = maxval(this%mesh%nodcoord( this%mesh%elemnod(i,:),2))!ymax
			z_coord(i)   = maxval(this%mesh%nodcoord( this%mesh%elemnod(i,:),3))!ymax
		enddo
		!$OMP end do
		!$OMP end parallel


		if(debug_mode) then
			print *, "[1] zmax >> xy_minmax solved."
		endif
		!$OMP parallel default(shared) 
		!$OMP do
		do i=1,this%ne()
			if( xy_minmax(i,1)  <= x .and. x <= xy_minmax(i,3)   )then
				if( xy_minmax(i,2)  <= y .and. y <= xy_minmax(i,4)   )then
					inside(i) = .true.
				endif
			endif
		enddo
		!$OMP end do
		!$OMP end parallel
		
		if(debug_mode) then
			print *, "[2] zmax >> inside solved."
		endif

		!$OMP parallel
		!$OMP do
		do i=1,this%ne()
			if(.not.inside(i) )then
				z_coord(i) = z_min
			endif
		enddo
		!$OMP end do
		!$OMP end parallel


		if(debug_mode) then
			print *, "[3] zmax >> z_coord solved."
		endif
		ret = maxval(z_coord)



	else
    	ret = maxval(this%mesh%nodcoord(:,3))
	endif

end function
! #########################################################

! #########################################################
function getElevationFEMDomain(this,x_num,y_num,x_len,y_len) result(ret)
    class(FEMDomain_),intent(in) :: this
    integer(int32),intent(in) :: x_num, y_num
	real(real64),intent(in) :: x_len,y_len
	real(real64) :: ret(x_num+1,y_num+1),dx,dy
	integer(int32) :: i
	
	ret(:,:) = this%zmin()
	dx = x_len/x_num
	dy = y_len/y_num
	do i=1,this%nn()
		ret( int(this%position_x(i)/dx+1),int(this%position_y(i)/dy+1) ) = &
		maxval([ ret( int(this%position_x(i)/dx+1),int(this%position_y(i)/dy+1) ) ,&
			this%position_z(i) ])
	enddo

end function
! #########################################################
subroutine deformFEMDomain(this,disp,velocity,accel,dt)
    class(FEMDomain_),intent(inout) :: this
    real(real64),optional,intent(in) :: disp(:),velocity(:),accel(:),dt

	if(this%mesh%empty() )then
		print *, "ERROR :: no mesh is imported."
		return
	else
		if(present(disp) )then
			this%mesh%nodcoord(:,:) = this%mesh%nodcoord(:,:) + reshape(disp,this%nn(),this%nd() )
		endif
		if(present(velocity) )then
			if(.not. present(dt) )then
				print *, "ERROR :: dt shuold be imported."
				stop
			endif
			this%mesh%nodcoord(:,:) = this%mesh%nodcoord(:,:) + reshape(velocity,this%nn(),this%nd() )*dt
		endif
		if(present(accel) )then
			if(.not. present(dt) )then
				print *, "ERROR :: dt shuold be imported."
				stop
			endif
			this%mesh%nodcoord(:,:) = this%mesh%nodcoord(:,:) + 0.50d0*reshape(accel,this%nn(),this%nd() )*dt*dt
		endif

	endif

	
end subroutine
! ####################################################################

function getMyIDFEMDomain(this,FEMDomains) result(id)
	class(FEMDOmain_),intent(in) :: this
	type(FEMDOmain_),intent(in) :: FEMDomains(:)
	integer(int32) :: id,i

	id = 0
	do i=1,size(FEMDomains)
		if(FEMDomains(i)%uuid == this%uuid )then
			if(id/=0)then
				id = -1 ! Crushed!
			else
				id = i
			endif
		endif
	enddo

end function

function getMyIDFEMDomainp(this,FEMDomainp) result(id)
	class(FEMDOmainp_),intent(in) :: this
	type(FEMDOmainp_),intent(in) :: femdomainp(:)
	integer(int32) :: id,i

	id = 0
	do i=1,size(femdomainp)
		if(femdomainp(i)%femdomainp%uuid == this%femdomainp%uuid )then
			if(id/=0)then
				id = -1 ! Crushed!
			else
				id = i
			endif
		endif
	enddo

end function



subroutine overset_FEMDomainp(this,FEMDomainp,to,by,debug)
	class(FEMDomainp_),intent(inout) :: this
	type(FEMDomainp_),intent(inout) :: femdomainp(:)
	integer(int32),intent(in) :: to
	character(*),intent(in) :: by
	integer(int32) :: from, MyID, algorithm
	logical,optional,intent(in) :: debug

	myID = this%getMyID(femdomainp)
	select case(by)
		case default
			algorithm = FEMDomain_Overset_GPP
		case("GPP","gpp")
			algorithm = FEMDomain_Overset_GPP
		case("P2P","PP","p2p","PointToPoint")
			algorithm = FEMDomain_Overset_P2P
	end select

	if(MyID == 0 )then
		print *, "[ERROR] oversetFEMDomain >> 404 Not Found."
		return
	endif


	if(MyID == -1 )then
		print *, "[ERROR] oversetFEMDomain >> uuids are crushed!"
		return
	endif

	call femdomainp(MyID)%femdomainp%overset(femdomainp(to)%femdomainp,DomainID=to, &
		algorithm=algorithm,MyDomainID=MyID,debug=debug)

end subroutine


subroutine oversetFEMDomains(this,FEMDomains,to,by,debug)
	class(FEMDomain_),intent(inout) :: this
	type(FEMDomain_),intent(inout) :: FEMDomains(:)
	integer(int32),intent(in) :: to
	character(*),intent(in) :: by
	integer(int32) :: from, MyID, algorithm
	logical,optional,intent(in) :: debug

	myID = this%getMyID(FEMDomains)
	select case(by)
		case default
			algorithm = FEMDomain_Overset_GPP
		case("GPP","gpp")
			algorithm = FEMDomain_Overset_GPP
		case("P2P","PP","p2p","PointToPoint")
			algorithm = FEMDomain_Overset_P2P
	end select

	if(MyID == 0 )then
		print *, "[ERROR] oversetFEMDomain >> 404 Not Found."
		return
	endif


	if(MyID == -1 )then
		print *, "[ERROR] oversetFEMDomain >> uuids are crushed!"
		return
	endif

	call FEMDomains(MyID)%overset(femdomains(to),DomainID=to, &
		algorithm=algorithm,MyDomainID=MyID,debug=debug)

end subroutine


subroutine oversetFEMDomain(this, FEMDomain, DomainID, algorithm, MyDomainID, debug)
	class(FEMDomain_),intent(inout) :: this
	type(FEMDomain_),intent(inout) :: FEMDomain
	integer(int32),intent(in) :: DomainID, algorithm
	integer(int32),optional,intent(in) :: MyDomainID

	integer(int32) :: ElementID, GaussPointID, NodeID, MyElementID
	real(real64),allocatable :: position(:)
	integer(int32) ,allocatable :: InterConnect(:),DomainIDs12(:) ,ElementIDList(:)
	logical,allocatable :: InsideElement(:)
	type(OversetConnect_),allocatable :: buf_oversetConnect(:)
	logical,optional,intent(in) :: debug
	integer(int32) :: kk
	

	if(.not. allocated(this%OversetConnect) )then
		allocate(this%OversetConnect(300) )
	endif
	position = zeros(this%nd() )


	if(algorithm == FEMDomain_Overset_GPP)then

		if(.not.allocated(this%OversetExists) )then
			this%OversetExists = int(zeros(this%ne(),this%ngp() ))
		endif

		InterConnect = int( zeros(this%nne()+ femdomain%nne()) )
						
		DomainIDs12 = int( zeros(this%nne()+ femdomain%nne()) ) 
		DomainIDs12(1:this%nne() ) = input(default=1, option=MyDomainID)
		DomainIDs12(this%nne()+1: ) = DomainID


		if(present(debug) )then
			if(debug)then
				print *, "FEMDomain%overset >> "
			endif
		endif

		! ElementIDList
		ElementIDList = this%getElementList(&
			xmin=FEMDomain%x_min(),&
			ymin=FEMDomain%y_min(),&
			zmin=FEMDomain%z_min(),&
			xmax=FEMDomain%x_max(),&
			ymax=FEMDomain%y_max(),&
			zmax=FEMDomain%z_max() )
		

		do kk=1, size(ElementIDList)
			ElementID = ElementIDList(kk)
			if(present(debug) )then
				if(debug)then
					print *, "Elem",kk,"/",size(ElementIDList)
				endif
			endif
			MyElementID = ElementIDList(kk)
			do GaussPointID = 1, this%ngp()
				! For 1st element, create stiffness matrix
		    	! set global coordinate
				position = this%GlobalPositionOfGaussPoint(ElementID,GaussPointID)
		    	if( femdomain%mesh%nearestElementID(x=position(1),y=position(2),z=position(3))<=0 )then
		    	    cycle
				else
					this%OversetExists(ElementID, GaussPointID) = this%OversetExists(ElementID, GaussPointID) +1
		    	endif

				if(this%num_oversetconnect + 1 > size(this%OversetConnect) )then
					buf_oversetConnect = this%OversetConnect
					deallocate(this%OversetConnect)
					allocate(this%OversetConnect(size(buf_oversetConnect)*2 ) )
					this%OversetConnect(1: size(buf_oversetConnect)) = buf_oversetConnect(1:size(buf_oversetConnect))
					deallocate(buf_oversetConnect)
				endif

				this%num_oversetconnect = this%num_oversetconnect + 1

				InterConnect(1:this%nne() ) = this%connectivity(ElementID)
		    	InterConnect(this%nne()+1:) &
					= femdomain%connectivity(femdomain%mesh%nearestElementID(x=position(1),y=position(2),z=position(3) ))
				
				this%OversetConnect(this%num_oversetconnect)%projection = FEMDomain_Overset_GPP
				this%OversetConnect(this%num_oversetconnect)%position   = position
				this%OversetConnect(this%num_oversetconnect)%ElementID  = ElementID
				this%OversetConnect(this%num_oversetconnect)%MyElementID  = MyElementID
				this%OversetConnect(this%num_oversetconnect)%GaussPointID = GaussPointID
				this%OversetConnect(this%num_oversetconnect)%InterConnect = InterConnect
				this%OversetConnect(this%num_oversetconnect)%DomainIDs12  = DomainIDs12
				this%OversetConnect(this%num_oversetconnect)%active = .true.
				! 何を記憶して，何はもう一度計算するか．
				
			enddo
		enddo


	elseif(algorithm == FEMDomain_Overset_P2P )then

		if(.not.allocated(this%OversetExists) )then
			this%OversetExists = int(zeros(this%nn(),1 ))
		endif

		allocate(DomainIDs12(femdomain%nne()+1 ) )
		allocate(InterConnect(femdomain%nne()+1 ) )

		do NodeID=1, this%nn()
			! For 1st element, create stiffness matrix
			! set global coordinate
			position(:) = this%mesh%nodcoord(NodeID,:)
			ElementID = femdomain%mesh%nearestElementID(x=position(1),y=position(2),z=position(3))
			MyElementID = this%mesh%nearestElementID(x=position(1),y=position(2),z=position(3))
			if( ElementID<=0 )then
				cycle
			else
				this%OversetExists(NodeID, 1) = this%OversetExists(NodeID, 1) +1
			endif

			if(this%num_oversetconnect + 1 > size(this%OversetConnect) )then
				buf_oversetConnect = this%OversetConnect
				deallocate(this%OversetConnect)
				allocate(this%OversetConnect(size(buf_oversetConnect)*2 ) )
				this%OversetConnect(1: size(buf_oversetConnect)) = buf_oversetConnect(1:size(buf_oversetConnect))
				deallocate(buf_oversetConnect)
			endif
			this%num_oversetconnect = this%num_oversetconnect + 1

			InterConnect(1) = NodeID
			InterConnect(2:) = femdomain%connectivity(femdomain%mesh%nearestElementID(x=position(1),y=position(2),z=position(3) ))
			DomainIDs12(1)   = input(default=1,option=myDomainID)
			DomainIDs12(2:)  = DomainID

			this%OversetConnect(this%num_oversetconnect)%projection = FEMDomain_Overset_P2P
			this%OversetConnect(this%num_oversetconnect)%position = position
			this%OversetConnect(this%num_oversetconnect)%ElementID =ElementID
			this%OversetConnect(this%num_oversetconnect)%MyElementID = MyElementID
			this%OversetConnect(this%num_oversetconnect)%GaussPointID =0 ! ignore
			this%OversetConnect(this%num_oversetconnect)%InterConnect = InterConnect
			this%OversetConnect(this%num_oversetconnect)%DomainIDs12 = DomainIDs12
			this%OversetConnect(this%num_oversetconnect)%active = .true.
			
			!A_ij = penalty*femdomain%ConnectMatrix(position,DOF=femdomain%nd() ) 
			!! assemble them 
			!call this%solver%assemble(&
			!	connectivity=InterConnect,&
			!	DOF=femdomain%nd() ,&
			!	eMatrix=A_ij,&
			!	DomainIDs=DomainIDs12)    
		enddo
	else
		! invalid
		print  *, "oversetFEMDomain :: invalid algroithm "
		stop
	endif


end subroutine

! #############################################################
pure function NumOversetElementsFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32) :: ret

	ret = this%num_oversetconnect

end function
! #############################################################


! #############################################################
subroutine refineFEMDomain(this,ElementID,success)  
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: ElementID
	real(real64),allocatable :: NodCoord_main(:,:)
	real(real64),allocatable :: NodCoord_buf(:,:)

	integer(int32),allocatable :: ElemNod_buf(:,:)
	integer(int32),allocatable :: cross_pairing(:,:),new_pairing(:,:),main_new_pairing(:,:)
	real(real64) :: theta
	integer(int32) :: i,j,nn
	logical,optional,intent(inout):: success

	if(present(success) )then
		success = .true.
	endif

	if(ElementID > this%ne() ) then
		if(present(success) )then
			success = .false.
		endif
		return
	endif
	! Only for 8-node isoparametric element
	if(this%nne()/=8 ) then
		print *, "[ERROR] refineFEMDomain is only for 8-node isoparametric elements."
		if(present(success) )then
			success = .false.
		endif
		stop
	else
		NodCoord_buf = zeros(this%nne(),3)
		ElemNod_buf  = int(zeros(7,this%nne()))
		NodCoord_buf = this%mesh%NodCoord(  this%mesh%ElemNod(ElementID,:)  ,:)

		nn = this%nn()
		
		![1,7] 
		![2,8] 
		![3,5] 
		![4,6] 
		![5,3] 
		![6,4] 
		![7,1] 
		![8,2] 
		cross_pairing = int(zeros(this%nne(),2))
		cross_pairing(1,1:2) = [1,7]
		cross_pairing(2,1:2) = [2,8]
		cross_pairing(3,1:2) = [3,5]
		cross_pairing(4,1:2) = [4,6]
		cross_pairing(5,1:2) = [5,3] 
		cross_pairing(6,1:2) = [6,4] 
		cross_pairing(7,1:2) = [7,1] 
		cross_pairing(8,1:2) = [8,2] 

		theta = 1.0d0/4.0d0

		do i=1,size(cross_pairing,1)
			do j=1,size(cross_pairing,2)
				cross_pairing(i,j) =  this%mesh%elemnod(ElementID,cross_pairing(i,j))
			enddo
		enddo
		
		! 1/3 method
		do i=1,size(cross_pairing,1)
			NodCoord_buf(i,1:3) = (1.0d0-theta)*this%mesh%nodcoord( &
					cross_pairing(i,1) ,1:3)  &
				+ (theta)*this%mesh%nodcoord( &
					cross_pairing(i,2) ,1:3)
		enddo


		! connectivity

		ElemNod_buf(1,1) = this%mesh%elemnod(ElementID,1) 
		ElemNod_buf(1,2) = this%mesh%elemnod(ElementID,2) 
		ElemNod_buf(1,3) = this%mesh%elemnod(ElementID,3) 
		ElemNod_buf(1,4) = this%mesh%elemnod(ElementID,4) 
		ElemNod_buf(1,5) = nn + 1
		ElemNod_buf(1,6) = nn + 2
		ElemNod_buf(1,7) = nn + 3
		ElemNod_buf(1,8) = nn + 4

		ElemNod_buf(2,1) = this%mesh%elemnod(ElementID,1) 
		ElemNod_buf(2,2) = this%mesh%elemnod(ElementID,5) 
		ElemNod_buf(2,3) = this%mesh%elemnod(ElementID,6) 
		ElemNod_buf(2,4) = this%mesh%elemnod(ElementID,2) 
		ElemNod_buf(2,5) = nn + 1
		ElemNod_buf(2,6) = nn + 5
		ElemNod_buf(2,7) = nn + 6
		ElemNod_buf(2,8) = nn + 2

		ElemNod_buf(3,1) = this%mesh%elemnod(ElementID,2) 
		ElemNod_buf(3,2) = this%mesh%elemnod(ElementID,6) 
		ElemNod_buf(3,3) = this%mesh%elemnod(ElementID,7) 
		ElemNod_buf(3,4) = this%mesh%elemnod(ElementID,3) 
		ElemNod_buf(3,5) = nn + 2
		ElemNod_buf(3,6) = nn + 6
		ElemNod_buf(3,7) = nn + 7
		ElemNod_buf(3,8) = nn + 3

		ElemNod_buf(4,1) = this%mesh%elemnod(ElementID,3) 
		ElemNod_buf(4,2) = this%mesh%elemnod(ElementID,7) 
		ElemNod_buf(4,3) = this%mesh%elemnod(ElementID,8) 
		ElemNod_buf(4,4) = this%mesh%elemnod(ElementID,4) 
		ElemNod_buf(4,5) = nn + 3
		ElemNod_buf(4,6) = nn + 7
		ElemNod_buf(4,7) = nn + 8
		ElemNod_buf(4,8) = nn + 4
		

		ElemNod_buf(5,1) = this%mesh%elemnod(ElementID,4) 
		ElemNod_buf(5,2) = this%mesh%elemnod(ElementID,8) 
		ElemNod_buf(5,3) = this%mesh%elemnod(ElementID,5) 
		ElemNod_buf(5,4) = this%mesh%elemnod(ElementID,1) 
		ElemNod_buf(5,5) = nn + 4
		ElemNod_buf(5,6) = nn + 8
		ElemNod_buf(5,7) = nn + 5
		ElemNod_buf(5,8) = nn + 1

		ElemNod_buf(6,1) = this%mesh%elemnod(ElementID,8) 
		ElemNod_buf(6,2) = this%mesh%elemnod(ElementID,7) 
		ElemNod_buf(6,3) = this%mesh%elemnod(ElementID,6) 
		ElemNod_buf(6,4) = this%mesh%elemnod(ElementID,5) 
		ElemNod_buf(6,5) = nn + 8
		ElemNod_buf(6,6) = nn + 7
		ElemNod_buf(6,7) = nn + 6
		ElemNod_buf(6,8) = nn + 5

		ElemNod_buf(7,1) = nn + 1
		ElemNod_buf(7,2) = nn + 2
		ElemNod_buf(7,3) = nn + 3
		ElemNod_buf(7,4) = nn + 4
		ElemNod_buf(7,5) = nn + 5
		ElemNod_buf(7,6) = nn + 6
		ElemNod_buf(7,7) = nn + 7
		ElemNod_buf(7,8) = nn + 8


		this%mesh%nodcoord = this%mesh%nodcoord .v. NodCoord_buf
		
		

		if(ElementID==1)then
			if(this%ne()==1 )then
				this%mesh%elemnod  =  ElemNod_buf 
			else
				this%mesh%elemnod  =  ElemNod_buf .v. this%mesh%elemnod(2:,:)
			endif
		elseif(ElementID==this%ne() )then
			this%mesh%elemnod  = this%mesh%elemnod(:this%ne()-1,: )  .v. ElemNod_buf
		else
			this%mesh%elemnod  = this%mesh%elemnod(:ElementID-1,:)  .v. ElemNod_buf &
			 .v. this%mesh%elemnod(ElementID+1:,:)
		endif
	endif




	
end subroutine
! #############################################################

! #############################################################
subroutine refine_elementsFEMDomain(this,ElementID)  
	class(FEMDomain_),intent(inout) :: this
	integer(int32),intent(in) :: ElementID(:)
	real(real64),allocatable :: NodCoord_main(:,:)
	real(real64),allocatable :: NodCoord_buf(:,:)

	integer(int32),allocatable :: ElemNod_buf(:,:)
	integer(int32),allocatable :: cross_pairing(:,:),new_pairing(:,:),main_new_pairing(:,:)
	real(real64) :: theta
	integer(int32) :: i,j,nn
	integer(int32),allocatable :: ElementList(:)
	logical :: success

	ElementList = ElementID
	call heapsort(size(ElementList), ElementList)

	do i=1,size(ElementList)
		call this%refine(ElementID=ElementList(i),success=success)
		if(success)then
			do j=i+1,size(ElementList)
				if(ElementList(j)>ElementList(i) )then
					ElementList(j) = ElementList(j) -1 + 7
				endif
			enddo
		endif
	enddo




	
end subroutine
! #############################################################



! #############################################################
subroutine csvFEMDomain(this,name)
	! export as point cloud
	class(FEMDomain_),intent(in) :: this
	character(*),intent(in) :: name
	integer(int32) :: i,j
	type(IO_) :: f

	if(this%empty() ) stop "ERROR :: csvFEMDomain >> no data"

	call f%open(name//".csv","w")
	do i=1,this%nn()
		write(f%fh,*) this%mesh%nodcoord(i,1),",",this%mesh%nodcoord(i,2),","	,this%mesh%nodcoord(i,3),","
	enddo
	call f%close()

end subroutine


subroutine fitFEMDomain(this,x,y,z,debug)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: x(:),y(:),z(:)
	logical,optional,intent(in) :: debug
	real(real64),allocatable :: center(:),min_xyz(:),max_xyz(:),this_center(:),&
	cov_mat(:,:),W(:),WORK(:),xyz(:,:),v1(:),v2(:),v3(:)
	character :: JOBZ, UPLO
	real(real64) :: v_per_v0
	integer(int32) :: i,j,n,itr,INFO,LDA,LWORK

	n = this%nd()
	
	if(n==3)then
		center  = zeros(n)

		center(1)  = sum(x)/size(x)
		center(2)  = sum(y)/size(y)
		center(3)  = sum(z)/size(z)

		this_center = this%centerPosition()

		call this%move(&
			x=- this_center(1) + center(1),&
			y=- this_center(2) + center(2),&
			z=- this_center(3) + center(3) &
		)
		
		xyz = zeros(3,size(x) )
		xyz(1,:) = x(:)
		xyz(2,:) = y(:)
		xyz(3,:) = z(:)
		! long-side finder
		! by PCA
		! 3 dimensions, 20 samples
		cov_mat = covarianceMatrix(xyz,xyz,n=3)
		
		WORK=zeros(3*3-1)
		W = zeros(3)
		JOBZ='V'
		UPLO='U'
		LWORK=3*3-1
		N = 3
		LDA = 3
		W = zeros(3)		
		call dsyev(JOBZ,&
			UPLO,&
			N,&
			cov_mat,&
			LDA,&
			W,&
			WORK,&
			LWORK,&
			INFO)
!
		! eigenvalues
		! w(1:3)

		! eigenvectors
		v1 = cov_mat(:,1)
		v2 = cov_mat(:,2)
		v3 = cov_mat(:,3)
		v1 = v1(:)/norm(v1)
		v2 = v2(:)/norm(v2)
		v3 = v3(:)/norm(v3)
		cov_mat(:,1) = v1 * sqrt(w(1))
		cov_mat(:,2) = v2 * sqrt(w(2))
		cov_mat(:,3) = v3 * sqrt(w(3))
		! x -> v1, times(w(1))
		! y -> v2
		! z -> v3
		!$OMP parallel do
		do i=1,this%nn()
			this%mesh%nodcoord(i,:)  = matmul(cov_mat,this%mesh%nodcoord(i,:) )
		enddo
		!$OMP end parallel do
		v_per_v0 = &
			( maxval(x) - minval(x) )/(this%xmax() - this%xmin())/3.0d0 &
		 +  ( maxval(y) - minval(y) )/(this%ymax() - this%ymin())/3.0d0 &
		 +  ( maxval(z) - minval(z) )/(this%zmax() - this%zmin())/3.0d0 

		this%mesh%nodcoord(:,1)  = this%mesh%nodcoord(:,1) * v_per_v0
		this%mesh%nodcoord(:,2)  = this%mesh%nodcoord(:,2) * v_per_v0
		this%mesh%nodcoord(:,3)  = this%mesh%nodcoord(:,3) * v_per_v0
		
		if(present(debug) )	then
			if(debug)then
				call print(">>> eigenvalue >>>")
				call print(w)
				call print(">>> eigenvector >>>")
				call print(cov_mat)
			endif
		endif
		
		

		center  = zeros(n)

		center(1)  = sum(x)/size(x)
		center(2)  = sum(y)/size(y)
		center(3)  = sum(z)/size(z)

		this_center = this%centerPosition()

		call this%move(&
			x=- this_center(1) + center(1),&
			y=- this_center(2) + center(2),&
			z=- this_center(3) + center(3) &
		)
		
	else
		print *, "fitFEMDomain >> only size(point_cloud,2)==3 is implemented. "
		stop
	endif



end subroutine

subroutine randomDanceFEMDomain(this,move,rotate,resize)
	class(FEMDomain_),intent(inout) :: this
	! 1st and 2nd moment
	real(real64),optional,intent(in) :: move(1:2),rotate(1:2),resize(1:2)
	type(Random_) :: random
	real(real64) :: rot_angle(1:3),center(1:3)
	if(present(resize) )then
	
		rot_angle = this%total_rotation
		center(1) = this%Position_x()
		center(2) = this%Position_y()
		center(3) = this%Position_z()

		call this%move(&
			x = -center(1)	, &
			y = -center(2)	, &
			z = -center(3)	  &
		)

		call this%rotate(&
			x = -rot_angle(1)	, &
			y = -rot_angle(2)	, &
			z = -rot_angle(3)	  &
		)


		call this%resize(&
			x_rate = random%gauss(mu=resize(1),sigma=resize(2) )	, &
			y_rate = random%gauss(mu=resize(1),sigma=resize(2) )	, &
			z_rate = random%gauss(mu=resize(1),sigma=resize(2) )	  &
		)

		call this%rotate(&
			x = rot_angle(1)	, &
			y = rot_angle(2)	, &
			z = rot_angle(3)	  &
		)
		call this%move(&
			x = center(1)	, &
			y = center(2)	, &
			z = center(3)	  &
		)
	endif


	if(present(rotate) )then
		center(1) = this%Position_x()
		center(2) = this%Position_y()
		center(3) = this%Position_z()
		call this%move(&
			x = -center(1)	, &
			y = -center(2)	, &
			z = -center(3)	  &
		)
		call this%rotate(&
			x = random%gauss(mu=rotate(1),sigma=rotate(2) )	, &
			y = random%gauss(mu=rotate(1),sigma=rotate(2) )	, &
			z = random%gauss(mu=rotate(1),sigma=rotate(2) )	  &
		)
		call this%move(&
			x = center(1)	, &
			y = center(2)	, &
			z = center(3)	  &
		)
	endif

	if(present(move) )then
		call this%move(&
			x = random%gauss(mu=move(1),sigma=move(2) )	, &
			y = random%gauss(mu=move(1),sigma=move(2) )	, &
			z = random%gauss(mu=move(1),sigma=move(2) )	  &
		)
	endif
	
	
end subroutine

function PCAvectorFEMDomain(this,eigen_values)  result(vectors)
	class(FEMDomain_),intent(inout) :: this
	real(real64),optional,allocatable,intent(inout) :: eigen_values(:)
	real(real64),allocatable :: vectors(:,:),center(:),A(:,:)

    !>>>>>>>>>>>>>> INPUT
    integer(int32) :: ITYPE = 1   ! A*x = (lambda)*B*x
    character(1) :: JOBZ  = 'V' ! Compute eigenvalues and eigenvectors.
    character(1) :: UPLO  = 'U' ! Upper triangles of A and B are stored;
    !<<<<<<<<<<<<<< INPUT

    integer(int32) :: N = 3 ! order of matrix
    real(real64),allocatable :: AP(:)
    real(real64),allocatable :: BP(:)
    real(real64),allocatable :: W(:)
    real(real64),allocatable :: Z(:,:),M(:)
    real(real64),allocatable :: WORK(:),ID(:)

	integer(int32),allocatable :: IWORK(:),IDS(:)
    integer(int32) :: LDZ
    integer(int32) :: LWORK
    integer(int32) :: LIWORK 
    integer(int32) :: INFO

    

	center = this%position()
	call this%move(&
		x= -center(1),&
		y= -center(2),&
		z= -center(3) )
		
	A = matmul( transpose(this%mesh%nodcoord ),this%mesh%nodcoord)
	A = A/dble(this%nn()-1 )


    !>>>>>>>>>>>>>> INPUT
    N      = 3
    LDZ    = 3
    LWORK  = 1 + 6*N + 2*N**2
    LIWORK = 3 + 5*N
    !<<<<<<<<<<<<<< INPUT

        !>>>>>>>>>>>>>>  INPUT/OUTPUT
	AP = zeros(N*(N+1)/2 )
	BP = zeros(N*(N+1)/2 )
	! Upper triangle matrix
	AP = [A(1,1),A(1,2),A(2,2),A(1,3),A(2,3),A(3,3)]
	BP = [1.0d0,0.0d0,1.0d0,0.0d0,0.0d0,1.0d0]
	!<<<<<<<<<<<<<< INPUT/OUTPUT
	
	!>>>>>>>>>>>>>>  OUTPUT
	W     = zeros(N )
	Z     = zeros(LDZ,N)
	WORK  = zeros(LWORK)
	IWORK = zeros(LIWORK)
	INFO  = 0
	!<<<<<<<<<<<<<< OUTPUT
	
	call DSPGVD (ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
        LWORK, IWORK, LIWORK, INFO)

	
	vectors = Z
	if(present(eigen_values) )then
		eigen_values = W	
	endif

	call this%move(&
		x=  center(1),&
		y=  center(2),&
		z=  center(3) )

end function

function getElementCauchyStressFEMDomain(this,displacement,E,v,i,j,option) result(sigma)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: displacement(:)
	real(real64),intent(in) :: E(:), v(:)
	real(real64),allocatable :: sigma(:),sigma_tensor(:,:)
	integer(int32),optional,intent(in) :: i, j
	character(*),optional,intent(in) :: option

	real(real64) :: YM, PR
	integer(int32) :: n,m

	if(present(i) .and. present(j) )then
	
		! get cell-avaraged Cauchy stress \sigma(i,j)
		sigma = zeros(this%ne() )
		do n=1,this%ne()
			if(size(E)==1 )then
				YM=E(1)
			elseif(size(E)==this%ne() )then
				YM=E(n)
			else
				print *, "ERROR :: getElementCauchyStressFEMDomain >> Invalid vector size of E(:)"
				print *, "size(E) should be 1 or number_of_element  "
				stop
			endif
			if(size(v)==1 )then
				PR=v(1)
			elseif(size(v)==this%ne() )then
				PR=v(n)
			else
				print *, "ERROR :: getElementCauchyStressFEMDomain >> Invalid vector size of v(:)"
				print *, "size(v) should be 1 or number_of_element  "
				stop
			endif

			sigma_tensor = this%StressMatrix(ElementID=n,&
				disp=reshape(displacement,this%nn(),this%nd() ),E=YM,v=PR)
			sigma(n) = sigma_tensor(i,j)

		enddo
	elseif(present(option) )then
		sigma = zeros(this%ne() )
		!$OMP parallel default(shared) private(YM,PR,sigma_tensor)
		!$OMP do
		do n=1,this%ne()
			if(size(E)==1 )then
				YM=E(1)
			elseif(size(E)==this%ne() )then
				YM=E(n)
			else
				print *, "ERROR :: getElementCauchyStressFEMDomain >> Invalid vector size of E(:)"
				print *, "size(E) should be 1 or number_of_element  "
				stop
			endif
			if(size(v)==1 )then
				PR=v(1)
			elseif(size(v)==this%ne() )then
				PR=v(n)
			else
				print *, "ERROR :: getElementCauchyStressFEMDomain >> Invalid vector size of v(:)"
				print *, "size(v) should be 1 or number_of_element  "
				stop
			endif

			sigma_tensor = this%StressMatrix(ElementID=n,&
				disp=reshape(displacement,this%nn(),this%nd() ),E=YM,v=PR)

			select case (option)

				case("p","P")
					sigma(n) = trace(sigma_tensor)/3.0d0
				
				case("I",'I1',"i1","trace","tr","TRACE","TR")
					sigma(n) = trace(sigma_tensor)
				
				case("II","I2","i2","ii")
					sigma(n) = ( trace(sigma_tensor)*trace(sigma_tensor) &
					- trace(matmul(sigma_tensor,sigma_tensor) ) )*0.50d0
				
				case("III","I3","i3","iii")
					sigma(n) = det_mat(sigma_tensor,n=size(sigma_tensor,1) )

				case("J",'J1',"j1","j")
					sigma_tensor = sigma_tensor &
						- trace(sigma_tensor)/3.0d0*eyes(size(sigma_tensor,1),size(sigma_tensor,2) )
					sigma(n) = trace(sigma_tensor)
				
				case("JJ","J2","j2","jj")
					sigma_tensor = sigma_tensor &
						- trace(sigma_tensor)/3.0d0*eyes(size(sigma_tensor,1),size(sigma_tensor,2) )
					sigma(n) = ( trace(sigma_tensor)*trace(sigma_tensor) &
					- trace(matmul(sigma_tensor,sigma_tensor) ) )*0.50d0
				
				case("JJJ","J3","j3","jjj")
					sigma_tensor = sigma_tensor &
						- trace(sigma_tensor)/3.0d0*eyes(size(sigma_tensor,1),size(sigma_tensor,2) )
					sigma(n) = det_mat(sigma_tensor,n=size(sigma_tensor,1) )

				case("1,1","(1,1)","_{1,1}")
					sigma(n) = sigma_tensor(1,1)
				
				case("2,2","(2,2)","_{2,2}")
					sigma(n) = sigma_tensor(2,2)
				
				case("3,3","(3,3)","_{3,3}")
					sigma(n) = sigma_tensor(3,3)

				case("1,2","(1,2)","_{1,2}","2,1","(2,1)","_{2,1}")
					sigma(n) = sigma_tensor(1,2)

				case("1,3","(1,3)","_{1,3}","3,1","(3,1)","_{3,1}")
					sigma(n) = sigma_tensor(1,3)
				
				case("3,2","(3,2)","_{3,2}","2,3","(2,3)","_{2,3}")
					sigma(n) = sigma_tensor(2,3)

			end select
			
			

		enddo
		!$OMP end do
		!$OMP end parallel

	endif

end function


subroutine loadPointsFEMDomain(this,x,y,z)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: x(:),y(:),z(:)

	if(.not.allocated(this%mesh%nodcoord))then
		this%mesh%nodcoord = zeros(size(x),3)
	endif
	this%mesh%nodcoord(:,1) = x(:)
	this%mesh%nodcoord(:,2) = y(:)
	this%mesh%nodcoord(:,3) = z(:)
	
end subroutine


subroutine particlesFEMDomain(this,name)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	type(IO_) :: f
	integer(int32) :: i

	if(.not.allocated(this%mesh%nodcoord))then
		print *, "[Warning] no point is loaded."
		return
	endif


	call f%open(name + ".particles","w")
	do i=1,size(this%mesh%nodcoord,1)
		write(f%fh,*) this%mesh%nodcoord(i,1),","&
			,this%mesh%nodcoord(i,2),",",this%mesh%nodcoord(i,3)
	enddo
	call f%close()
	
end subroutine


subroutine BooleanFEMDomain(this, object, difference) 
	class(FEMDomain_),intent(inout) :: this
	type(FEMDomain_),intent(in)  :: object
	logical,optional,intent(in) :: difference
	integer(int32),allocatable  :: removed_nodes(:), buf(:)
	integer(int32) :: i,j,k,num_zeros,ElementID,NodeID
	logical :: inside
	
	
	if(present(difference) )then
		if(difference)then
			! default = keep all nodes 
			removed_nodes = int(zeros(this%nn()) )
			
			!$OMP parallel do
			do i=1,this%nn()
				! detect in or out
				if(object%x_min() <=  this%position_x(i) &
								.and. this%position_x(i) <= object%x_max() )then
					if(object%y_min() <=  this%position_y(i) &
									.and. this%position_y(i) <= object%y_max() )then
						if(object%z_min() <=  this%position_z(i) &
										.and. this%position_z(i) <= object%z_max() )then
							removed_nodes(i)=1
						endif	
					endif	
				endif
			enddo
			!$OMP end parallel do
			
			buf = removed_nodes
			deallocate(removed_nodes)
			allocate(removed_nodes(sum(buf)))
			if(sum(buf)==0 )then
				return
			endif
			j=0
			do i=1,size(buf)
				if(buf(i) == 1 )then
					j=j+1
					removed_nodes(j) = i
				endif
			enddo
			deallocate(buf)
			


			num_zeros = 0
			do ElementID=1,object%ne()
				inside=.false.
				do NodeID =1,size(removed_nodes)
					if( object%inside_of_element(point=this%position(NodeID) ,ElementID=ElementID) )then			
						inside=.true.
						exit
					endif
				enddo 
				if(.not. inside)then
					removed_nodes(NodeID) = 0 ! remove
					num_zeros = num_zeros + 1
				endif
			enddo

			buf = removed_nodes
			deallocate(removed_nodes)
			allocate(removed_nodes(size(buf,1) - num_zeros ) )
			
			j=0
			do i=1,size(buf)
				if(buf(i)/=0)then
					j = j+1
					removed_nodes(j)=buf(i)
				endif
			enddo

			call this%killNodes(NodeList=removed_nodes)

		endif
	endif


end subroutine
! ##################################################################
function hasFEMDomain(this,position) result(inside)
	class(FEMDomain_),intent(in) :: this
	real(real64),intent(in)::position(:)
	real(real64) :: min_max_x(3,1:2)
	logical :: inside
	integer(int32) :: counter,i

	min_max_x(1,1:2) = [this%xmin(), this%xmax() ]
	min_max_x(2,1:2) = [this%ymin(), this%ymax() ]
	min_max_x(3,1:2) = [this%zmin(), this%zmax() ]

	counter = 0
	do i=1,size(position)
		if( (min_max_x(i,1) <= position(i) ).and.(position(i) <= min_max_x(i,2) ))then
			counter = counter + 1
		endif
	enddo
	inside = (counter == size(position) )

end function

! ##################################################################
function inside_of_elementFEMDomain(this,point,ElementID) result(inside)
	class(FEMDomain_),intent(in) :: this
	real(real64),intent(in)::point(:)
	real(real64)::a1(3),a2(3),n(3)
	integer(int32),intent(in)::ElementID
	integer(int32),allocatable :: facet(:,:)
	integer(int32) :: i
	logical :: inside

	! get facet
	facet = this%getSingleFacetNodeID(ElementID)
	do i=1, size(facet,1)
		a1(1:3) = this%position(facet(i,2) ) - this%position(facet(i,1) )
		a2(1:3) = this%position(facet(i,4) ) - this%position(facet(i,1) ) 
		n  = cross_product(a1,a2)
		! compute outer-nomal n
		if(dot_product(n,point) > 0.0d0 )then
			inside=.false.
			return
		else
			cycle
		endif
	enddo

	inside = .true.
	
	! for all facet,dot_product(n,point)<=0 >> inside
	! otherwise :: inside=.false.


end function

subroutine killNodesFEMDomain(this,NodeList) 
    class(FEMDomain_),intent(inout)::this
    integer(int32),intent(in) :: NodeList(:)
	integer(int32),allocatable:: Kill_or_not(:),new_Node_ID(:)
	real(real64),allocatable :: rebuf(:,:)
	integer(int32),allocatable :: intbuf(:,:)
	integer(int32) :: i,j,n_remove_elem
	
	!e.g. [1, 3, 6]
	Kill_or_not = int(zeros(this%nn() ) )

	![0,0,0,0,0,0]
	new_node_id = int(zeros(this%nn() ) )

	![1,0,1,0,0,1]
	do i=1,size(NodeList)
		Kill_or_not( NodeList(i) ) = 1
	enddo

	if(Kill_or_not(1)==1 )then
		new_node_id(1)=0
	else
		new_node_id(1)=1
	endif
	do i=1, size(new_node_id)-1
		new_node_id(i+1) = new_node_id(i) + 1 - Kill_or_not(i+1)
	enddo
	!>> [0,1,1,2,3,3]

	rebuf = this%mesh%nodcoord

	deallocate(this%mesh%nodcoord)

	this%mesh%nodcoord = zeros( size(rebuf,1)-sum(Kill_or_not),size(rebuf,2)  )

	j = 0
	do i =1,size(rebuf,1)
		if(Kill_or_not(i)==0 )then
			j = j + 1
			this%mesh%nodcoord(j,:) = rebuf(i,:)
		endif
	enddo

	deallocate(rebuf)

	n_remove_elem = 0
	do i=1, size(this%mesh%elemnod,1)
		do j=1, size(this%mesh%elemnod,2)
			if( kill_or_not(this%mesh%elemnod(i,j))==1)then
				this%mesh%elemnod(i,:) = 0
				n_remove_elem = n_remove_elem + 1
				exit
			endif
		enddo
	enddo

	intbuf = this%mesh%elemnod
	this%mesh%elemnod = int(zeros( size(intbuf,1)-n_remove_elem, size(intbuf,2) ) )
	
	j = 0
	do i=1,size(intbuf,1)
		if(intbuf(i,1)/=0 )then
			j = j + 1
			this%mesh%elemnod(j,:) = intbuf(i,:)	
		endif
	enddo

	do i=1,size(this%mesh%elemnod,1)
		do j=1,size(this%mesh%elemnod,2)
			this%mesh%elemnod(i,j) = new_node_id( this%mesh%elemnod(i,j) )
		enddo
	enddo


end subroutine

function clipVectorFEMDomain(this,vector,femdomains,DomainID) result(ret_vec)
	class(FEMDomain_),intent(in) :: this
	type(FEMDomain_),intent(in) :: femdomains(:)
	real(real64),intent(in) :: vector(:)
	real(real64),allocatable :: ret_vec(:)
	integer(int32),intent(in) :: DomainID
	integer(int32) :: i,j,k,DOF,n,total_nn
	
	! Assuming vector(:) is a set of
	! node-wise value
	total_nn = 0
	do i=1,size(femdomains)
		total_nn = total_nn + femdomains(i)%nn()
	enddo

	DOF = size(vector)/total_nn

	total_nn = 0
	do i=1,DomainID-1
		total_nn = total_nn + femdomains(i)%nn()
	enddo

	ret_vec = zeros(this%nn()*DOF )
	ret_vec(:) = vector(  total_nn*DOF+1 : total_nn*DOF+ this%nn()*DOF )

end function

function getElementIDFEMDomain(this,x,debug,info) result(ElementID)
	class(FEMDomain_),intent(in) :: this
    real(real64),intent(in) :: x(:)
	logical,optional,intent(in) :: debug
    integer(int32),optional,allocatable,intent(inout) :: info(:)
    integer(int32) :: ElementID

	ElementID = this%mesh%getElementID(x=x,debug=debug,info=info)


end function

! ##########################################################
function getValueFEMDomain(this,scalar_field,position) result(retval)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: scalar_field(:),position(1:3)
	real(real64) :: retval,localCoord(1:3)
	type(ShapeFunction_) :: sf
	integer(int32) :: ElementID,i
	
	ElementID = this%getElementID(x=position)

    if(ElementID <=0)then
		print *, "getValueFEMDomain >> invalid element position"
		print *, "ERROR code: ",ElementID
		stop
	endif
	
	sf = this%getShapeFunction(ElementID=ElementID,position=position)
	retval = 0.0d0
	do i=1,this%nne()
		retval = retval + sf%Nmat(i)*scalar_field(this%mesh%elemnod(ElementID,i) )
	enddo

  
end function


!## physical operator

!! ####################################################
!subroutine grubFEMDomain(this,x_min,x_max,y_min,y_max,z_min,z_max) 
!	class(FEMDomain_),intent(in) :: this
!	real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
!	
!
!	if(allocated(this%grub_NodeList) )then
!		this%grub_NodeList = this%select(x_min,x_max,y_min,y_max,z_min,z_max)
!	else
!		this%grub_NodeList = this%grub_NodeList // this%select(x_min,x_max,y_min,y_max,z_min,z_max)
!	endif
!
!end subroutine
!! ####################################################
!
!
!! ####################################################
!subroutine fixFEMDomain(this) 
!	class(FEMDomain_),intent(in) :: this
!	
!	if(allocated(this%grub_NodeList) )then
!		deallocate(this%grub_NodeList)
!	endif
!	
!end subroutine
!! ####################################################
!
!
!! ####################################################
!subroutine releaseFEMDomain(this) 
!	class(FEMDomain_),intent(in) :: this
!	
!	if(allocated(this%grub_NodeList) )then
!		deallocate(this%grub_NodeList)
!	endif
!	
!end subroutine
!! ####################################################
subroutine ImportSTLFileFEMDomain(this,name)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	type(STL_) :: stl
	integer(int32) :: num_facet,num_point,num_dim,i,j,k
	
	if( .not.this%empty() )then
		call this%remove()
	endif

	call stl%import(name)
	num_facet = size(stl%facet,1)
	num_point = size(stl%facet,2)
	num_dim   = size(stl%facet,3)

	this%mesh%nodcoord = zeros(num_facet*num_point,num_dim)
	this%mesh%elemnod  = zeros(num_facet,8)
	do i=1, num_facet
		do j=1, num_point
			this%mesh%nodcoord( (i-1)*num_point + j,1:num_dim ) = stl%facet(i,j,1:num_dim)		
		enddo
	enddo

	do i=1, num_facet
		do j=1, num_point
			this%mesh%elemnod( i,j ) =  (i-1)*num_point + j
		enddo
		do j=num_point+1,8
			this%mesh%elemnod( i,j ) =  (i-1)*num_point + num_point
		enddo
	enddo

end subroutine


! ###################################################################
function xyzFEMDomain(this) result(nodcoord)
	class(FEMDOmain_),intent(in) :: this
	real(real64),allocatable :: nodcoord(:,:)

	nodcoord = this%mesh%nodcoord
	
end function
! ###################################################################



! ###################################################################

function getStrainTensorFEMDomain(this,displacement,ElementID,GaussPointID,debug) result(StrainTensor)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in)   :: displacement(:,:)
	integer(int32),intent(in) :: ElementID, GaussPointID
	logical,optional,intent(in) :: debug
	real(real64),allocatable :: StrainTensor(:,:),Bmat(:,:),ElemDisp(:),StrainVector(:)
	type(ShapeFunction_) :: shapefunc
	integer(int32) :: i,j
	StrainTensor = zeros(3,3)

	call shapefunc%SetType(NumOfDim=this%nd(),NumOfNodePerElem=this%nne() ,NumOfGp=this%mesh%getNumOfGp())

	call getAllShapeFunc(shapefunc,elem_id=ElementID,&
		nod_coord=this%Mesh%NodCoord,&
		elem_nod=this%Mesh%ElemNod,OptionalGpID=GaussPointID)
	
	ElemDisp = zeros(  size( this%mesh%elemnod,2 ) *3) 
	do i=1,this%nne()
		do j=1,3
			ElemDisp( 3*(i-1) + j ) = &
				Displacement( this%mesh%elemnod(ElementID,i) ,j)
		enddo
	enddo
	Bmat = this%Bmatrix(shapefunc)
	

	StrainVector = matmul(Bmat,ElemDisp)
	StrainTensor(1,1) = StrainVector(1)
	StrainTensor(2,2) = StrainVector(2)
	StrainTensor(3,3) = StrainVector(3)
	StrainTensor(1,2) = StrainVector(4)
	StrainTensor(2,3) = StrainVector(5)
	StrainTensor(1,3) = StrainVector(6)
	StrainTensor(2,1) = StrainVector(4)
	StrainTensor(3,2) = StrainVector(5)
	StrainTensor(3,1) = StrainVector(6)


	if(present(debug) )then
		print *, "StrainVector"
		call print(StrainVector)
		print *, "Bmat"
		call print(Bmat)
		print *, "ElemDisp"
		call print(ElemDisp)
	endif

end function

function getNumberOfOversetForElementFEMDomain(this) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32),allocatable :: ret(:)
	integer(int32) :: i,n

	if(this%empty()) then
		allocate(ret(0) )
		return
	endif
	ret = int(zeros(this%ne() ) )
	
	if(.not.allocated(this%OversetConnect) ) then
		ret(:) = 0
		return
	endif
	
	
	do i=1,this%num_oversetconnect
		if(this%OversetConnect(i)%MyElementID < 0) cycle
		ret(this%OversetConnect(i)%MyElementID)=ret(this%OversetConnect(i)%MyElementID)+1
	enddo

end function


function ElementID2NodeIDFEMDomain(this,ElementID) result(NodeID)
	class(FEMDomain_),intent(in) :: this
	integeR(int32),intent(in) :: ElementID(:)
	integeR(int32),allocatable :: NodeID(:),nodecount(:)
	integeR(int32) :: i,j
	nodecount = int(zeros(this%nn() ))

	do i=1,size(ElementID)
		do j=1,	this%nne()
			nodecount(this%mesh%elemnod( ElementID(i),j ))  = 1
		enddo
	enddo

	NodeID = getIDx(nodecount,equal_to=1)

end function

! ###############################################################
function fullFEMDomain(this,func,params) result(scalar_field)
	class(FEMDomain_),intent(in) :: this
	interface 
		function func(x,params) result(scalar_value)
			use iso_fortran_env
			real(real64),intent(in) :: x(:)
			real(real64),optional,intent(in) :: params(:)
			real(real64) :: scalar_value
		end function
	end interface 
	real(real64),optional,intent(in) :: params(:)
	real(real64),allocatable :: scalar_field(:)
	real(real64) :: x(1:3)
	integer(int32) :: NodeID

	scalar_field = zeros(this%nn() )
	do NodeID=1,this%nn()
		x = [this%position_x(NodeID),this%position_y(NodeID),this%position_z(NodeID)]
		scalar_field(NodeID) = func(x=x,params=params)
	enddo

end function

!#################################################################
function ZeroMatrix_as_CRS_FEMDomains(femdomains,DOF) result(ret)
	type(FEMDomain_),intent(inout) :: femdomains(:)
	integer(int32),intent(in) :: DOF
	type(CRS_) :: ret, buf
	integer(int32) :: i,n,ncol

	!n = 0
	!do i=1,size(femdomains)
	!	n = n + femdomains(i)%nn()*DOF
	!enddo
	! ignore overlap
	do i=1,size(femdomains)
		buf = femdomains(i)%ZeroMatrix(DOF=DOF)
		if(i==1)then
			ret%row_ptr = buf%row_ptr
			ret%col_idx = buf%col_idx
			ret%val     = buf%val    
			n = size(ret%row_ptr)-1
			ncol = maxval(buf%row_ptr)-1
		else
			buf%row_ptr(:) = buf%row_ptr(:) + ncol
			buf%col_idx(:) = buf%col_idx(:) + n

			ret%row_ptr = ret%row_ptr(:) // buf%row_ptr(2:) 
			ret%col_idx = ret%col_idx(:) // buf%col_idx(:)
			ret%val     = ret%val     // buf%val    
			n = size(ret%row_ptr)-1
			ncol = maxval(buf%row_ptr)-1
		endif
	enddo

end function
! #####################################################
subroutine spanFEMDomain(this,p1,p2) 
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: p1(:),p2(:)
	real(real64) :: center(1:3),this_center(1:3),X(3,3),xbuf(1:3)
	real(real64) :: angle(1:3) = 0.0d0
	real(real64) :: z(1:3) = [0.0d0,0.0d0,1.0d0]
	real(real64) :: length,alpha,beta,gamma,n(1:3)
	integer(int32) :: i
	type(Random_) :: random
	

	center = 0.50d0*p1 + 0.50d0*p2
	this_center = this%centerPosition()
	length = norm(p1-p2)
	call this%move(&
		x=-this_center(1) ,&
		y=-this_center(2) ,&
		z=-this_center(3)  &
		)
	X(:,1) = p2-p1
	X(:,2) = random%randn(3)
	X(:,3) = random%randn(3)
	X(:,1) = X(:,1)/norm(X(:,1))
	X(:,2) = X(:,2)/norm(X(:,2))
	X(:,3) = X(:,3)/norm(X(:,3))
	call GramSchmidt(X,size(X,1),size(X,2),X )
	xbuf(:)=X(:,1)
	X(:,1) = X(:,3) 
	X(:,2) = X(:,2) 
	X(:,3) = xbuf(:)

	call this%resize(z=length)

	if(norm(p1-p2)==0.0d0 ) return
	
	do i=1,this%nn()
		this%mesh%nodcoord(i,:) = &
			  this%mesh%nodcoord(i,1)*X(:,1) &
			+ this%mesh%nodcoord(i,2)*X(:,2) &
			+ this%mesh%nodcoord(i,3)*X(:,3) 
	enddo
	call this%move(&
		x=center(1) ,&
		y=center(2) ,&
		z=center(3)  &
		)


	

end subroutine 

! #################################################
subroutine to_HollowTube_FEMDomain(this,r_num,theta_num,z_num,thickness,radius,length)
	class(FEMDomain_),intent(inout) :: this
    integer(int32),optional,intent(in) :: r_num,z_num,theta_num
    real(real64),optional,intent(in) :: thickness, radius, length

	call this%mesh%to_HollowTube(r_num=r_num,&
	theta_num=theta_num,&
	z_num=z_num,&
	thickness=thickness,&
	radius=radius,&
	length=length)
end subroutine
! #################################################


! #################################################
subroutine to_culm_FEMDomain(this,r_num,theta_num,z_num,thickness,radius,length,&
		node_thickness)
	class(FEMDomain_),intent(inout) :: this
    integer(int32),optional,intent(in) :: r_num,z_num,theta_num
    real(real64),optional,intent(in) :: thickness, radius, length, node_thickness

	call this%mesh%to_culm(r_num=r_num,&
		theta_num=theta_num,&
		z_num=z_num,&
		thickness=thickness,&
		radius=radius,&
		length=length,&
		node_thickness=node_thickness)
end subroutine
! #################################################


! #################################################
subroutine to_multi_culm_FEMDomain(this,r_num,theta_num,z_num,thickness,radius,length,&
	node_thickness,n)
class(FEMDomain_),intent(inout) :: this
type(FEMDomain_) :: single_culm
integer(int32),optional,intent(in) :: r_num,z_num,theta_num
real(real64),optional,intent(in) :: thickness, radius, length, node_thickness
integer(int32),intent(in) :: n
integer(int32) :: i
real(real64) :: L

	call single_culm%mesh%to_culm(r_num=r_num,&
		theta_num=theta_num,&
		z_num=z_num,&
		thickness=thickness,&
		radius=radius,&
		length=length,&
		node_thickness=node_thickness)

	! copy and joint
	L = single_culm%zmax() - single_culm%zmin()
	do i=1,n
		if(i==1)then
			this%mesh%nodcoord = single_culm%mesh%nodcoord
			this%mesh%elemnod = single_culm%mesh%elemnod
			this%mesh%elemmat = single_culm%mesh%elemmat
		else
			single_culm%mesh%nodcoord(:,3) = single_culm%mesh%nodcoord(:,3)+ L
			this%mesh%nodcoord = this%mesh%nodcoord .v. single_culm%mesh%nodcoord  
			this%mesh%elemnod = this%mesh%elemnod .v. (single_culm%mesh%elemnod + single_culm%nn()*(i-1) )
			this%mesh%elemmat = this%mesh%elemmat // single_culm%mesh%elemmat 
		endif
	enddo

	! kill overlap nodes
	call this%remove_duplication()

end subroutine
! #################################################

! #################################################
subroutine to_cylinder_FEMDomain(this,x_num,y_num,z_num,radius,length)
class(FEMDomain_),intent(inout) :: this
integer(int32),optional,intent(in) :: x_num,z_num,y_num
real(real64),optional,intent(in) :: radius(1:2), length
real(real64) :: r(1:2),l
integer(int32) :: xn,yn,zn

r = input(default=[1.0d0,1.0d0],option=radius)
l = input(default=1.0d0,option=length)
xn = input(default=20,option=x_num)
yn = input(default=20,option=y_num)
zn = input(default=20,option=z_num)
call this%create("Cylinder3D",x_num=x_num,y_num=y_num,division=z_num,&
	thickness=l,x_len=r(1),y_len=r(2) )

end subroutine
! #################################################

! #################################################
subroutine remove_duplication_FEMDomain(this)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),allocatable :: same_node_as(:),kill_node_list(:)
	integer(int32) :: i,j,n

	! remove duplicated points
	same_node_as = int(zeros(this%nn() ) )
	n=0
	!!$OMP parallel do private(j) reduction(+:n)
	do i=1,this%nn()
		if(same_node_as(i)>=1)cycle
		!!$OMP parallel do private(j) reduction(+:n)
		do j=i+1,this%nn()
			if(same_node_as(j)>=1)cycle
			if(norm(this%mesh%nodcoord(i,:)-this%mesh%nodcoord(j,:))<=1.0e-18 )then
				same_node_as(j)=i
				n = n + 1
				cycle
			endif
		enddo
		!!$OMP end parallel do
	enddo
	!!$OMP end parallel do

	if(n==0)then
		print *, "no duplicated nodes"
		return
	endif

	! remove nodes
	do i=1,this%ne()
		do j=1,this%nne()
			if(same_node_as(this%mesh%elemnod(i,j))==0 )then
				cycle
			else
				this%mesh%elemnod(i,j) = same_node_as(this%mesh%elemnod(i,j))
			endif
		enddo
	enddo

	allocate(kill_node_list(n) )
	kill_node_list(:) = 0
	n=0
	do i=1,size(same_node_as)
		if(same_node_as(i)/=0 )then
			n=n+1
			kill_node_list(n) = i
		endif
	enddo

	call this%killNodes(NodeList=kill_node_list)



end subroutine
! #################################################
subroutine cubeFEMDomain(this,x_num,y_num,z_num,&
		x_axis,y_axis,z_axis)
	class(FEMDomain_),intent(inout) :: this
	integer(int32),optional,intent(in) :: x_num,y_num,z_num
	real(real64),optional,intent(in) :: x_axis(:),y_axis(:),z_axis(:)

	call this%create("Cube3D",x_num=x_num,y_num=y_num,z_num=z_num,&
		x_axis=x_axis,y_axis=y_axis,z_axis=z_axis)
end subroutine

! #################################################


! #################################################
function getNeighboringElementListFEMDomain(this,ElementID) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32),intent(in) :: ElementID

	integer(int32),allocatable :: ret(:)
	integer(int32),allocatable :: node_list(:)
	integer(int32),allocatable :: element_list(:)

	node_list = this%mesh%elemnod(ElementID,:)
	ret = this%to_ElementID(node_list)


end function
! #################################################

function to_ElementIDFEMDomain(this,NodeList) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32),intent(in) :: NodeList(:)
	logical,allocatable :: ElementList(:)
	integer(int32),allocatable :: ret(:)
	integer(int32) :: elemid,enodid,i,n

	allocate(ElementList(this%ne()) )

	ElementList(:) = .false.

	!$OMP parallel do private(enodid,i) reduction(.or.:ElementList)
	do elemid=1,this%ne()
		do enodid=1,this%nne()
			do i=1,size(NodeList)
				if(this%mesh%elemnod(elemid,enodid)	== NodeList(i))then
					ElementList(elemid) =  ElementList(elemid) .or. .true.
					exit
				endif
			enddo
		enddo
	enddo
	!$OMP end parallel do

	n = 0
	!$OMP parallel do reduction(+:n)
	do i=1,size(ElementList)
		if(ElementList(i) )then
			n = n + 1
		endif
	enddo
	!$OMP end parallel do
	
	allocate( ret(n) )
	n = 0
	do i=1,size(ElementList)
		if(ElementList(i) )then
			n = n + 1
			ret(n) = i
		endif
	enddo
	
end function
! ######################################################

! ######################################################
function mpi_matmulFEMDomain(this,A,x,mpid) result(ret)
	class(FEMDomain_),intent(inout) :: this
	type(CRS_),optional,intent(in) :: A
	real(real64),intent(in)  :: x(:)
	type(MPI_),intent(inout) :: mpid
	real(real64),allocatable :: ret(:),val(:),val_buf(:)
	!integer(int32),allocatable :: send_req(:),recv_req(:)
	integer(int32) :: send_req,recv_req
	integer(int32) :: proc_idx,i,node_idx,DOF

	ret = A%matmul(x)
	DOF = size(ret)/this%nn()

	if(mpid%petot==1) return

	! sync
	val = zeros(DOF*size(this%mpi_shared_node_info,1))
	val_buf = zeros(DOF)
	send_req = 0!int(zeros(DOF))
	recv_req = 0!int(zeros(DOF))
	do i=1,size(this%mpi_shared_node_info,1)
		node_idx = this%mpi_shared_node_info(i,1)
		val_buf(:) = 0.0d0
		call mpid%irecv(&
			from=this%mpi_shared_node_info(i,2),&
			val=val_buf(1:DOF),&
			req=recv_req,&
			tag=this%mpi_shared_node_info(i,1) )
		
		call mpid%isend(&
			to=this%mpi_shared_node_info(i,2),&
			val=ret(DOF*(node_idx-1)+1:DOF*(node_idx-1)+DOF),&
			req=send_req,&
			tag=this%mpi_shared_node_info(i,3) )

		call mpid%WaitAll(send_req=send_req,recv_req=recv_req )
		
		val( DOF*(i-1)+1:DOF*(i-1)+DOF  ) = val( DOF*(i-1)+1:DOF*(i-1)+DOF  ) + val_buf(1:DOF)
		val_buf = 0.0d0
	enddo
	

	do i=1,size(this%mpi_shared_node_info,1)
		node_idx = this%mpi_shared_node_info(i,1)
		ret(DOF*(node_idx-1)+1:DOF*(node_idx-1)+DOF ) = ret(DOF*(node_idx-1)+1:DOF*(node_idx-1)+DOF ) &
			+ val( DOF*(i-1)+1:DOF*(i-1)+DOF )
	enddo
	

end function
! ######################################################

subroutine read_mpi_propertyFEMDomain(this,name,num_division)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	integer(int32),optional,intent(inout) :: num_division
	character(1) :: line
	integer(int32) :: buf,num_glo_node,num_loc_node,num_shared_node,&
		num_div,i,idx,val
	integer(int32) :: buf_vec(3)
	type(IO_) :: f

	call f%open(name,"r")
	read(f%fh,*) line
	read(f%fh,*) num_glo_node,num_loc_node,num_shared_node,num_div
	this%mpi_global_node_idx = int(zeros(num_loc_node) )
	this%mpi_shared_node_info = int(zeros(num_shared_node,3) )
	read(f%fh,*) line
	do i = 1, num_loc_node
		read(f%fh,*) idx,val
		this%mpi_global_node_idx(idx) = val
	enddo
	read(f%fh,*) line
	do i = 1, num_shared_node
		read(f%fh,*) buf_vec(1:3)
		this%mpi_shared_node_info(i,1:3) = buf_vec(1:3)
	enddo
	call f%close()

	if(present(num_division) )then
		num_division = num_div
	endif
end subroutine

! #######################################################
subroutine read_vtk_domain_decomposed_FEMDOmain(this,name,myrank)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	integer(int32),intent(in) :: myrank
	
    call this%read("Cube3D_"+zfill(myrank,6)+".vtk")
    call this%read_mpi_property("Cube3D_"+zfill(myrank,6)+".csv")

end subroutine
! #######################################################


! #######################################################
function M_inv_K_Matrix_CRS_FEMDomain(this,Density,YoungModulus,PoissonRatio) result(ret)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: Density(:),YoungModulus(:)
	real(real64),optional,intent(in) :: PoissonRatio(:)
	real(real64),allocatable :: v(:)
	type(CRS_) :: K,M,ret
	integer(int32) :: dof

	K = this%StiffnessMatrix( &
    	YoungModulus=YoungModulus, &
    	PoissonRatio=PoissonRatio )

	dof = K%size()/this%nn()

	M = this%MassMatrix(&
    	Density=Density ,&
    	DOF=dof)

	v = M%lumped()
	
	ret = K%divide_by(v)
	

end function
! #######################################################




! #######################################################
subroutine set_xFEMDomain(this,idx,coord)
	class(FEMDOmain_),intent(inout) :: this
	integer(int32),intent(in) :: idx
	real(real64),intent(in) :: coord

	this%mesh%nodcoord(idx,1) = coord

end subroutine
! #######################################################





! #######################################################
subroutine set_yFEMDomain(this,idx,coord)
	class(FEMDOmain_),intent(inout) :: this
	integer(int32),intent(in) :: idx
	real(real64),intent(in) :: coord

	this%mesh%nodcoord(idx,2) = coord

end subroutine
! #######################################################



! #######################################################
subroutine set_zFEMDomain(this,idx,coord)
	class(FEMDOmain_),intent(inout) :: this
	integer(int32),intent(in) :: idx
	real(real64),intent(in) :: coord

	this%mesh%nodcoord(idx,3) = coord

end subroutine
! #######################################################










! #######################################################
function getPoint_xFEMDomain(this,idx) result(coord)
	class(FEMDOmain_),intent(inout) :: this
	integer(int32),intent(in) :: idx
	real(real64) :: coord

	coord = this%mesh%nodcoord(idx,1) 

end function
! #######################################################





! #######################################################
function getPoint_yFEMDomain(this,idx) result(coord)
	class(FEMDOmain_),intent(inout) :: this
	integer(int32),intent(in) :: idx
	real(real64) :: coord

	coord = this%mesh%nodcoord(idx,2) 

end function
! #######################################################



! #######################################################
function getPoint_zFEMDomain(this,idx) result(coord)
	class(FEMDOmain_),intent(inout) :: this
	integer(int32),intent(in) :: idx
	real(real64) :: coord

	coord = this%mesh%nodcoord(idx,3) 

end function
! #######################################################










! #######################################################
function to_composite_beam_FEMDomain(length,width,angle_x,angle_z,division) result(this)
	type(FEMDomain_) :: this
	real(real64),intent(in) :: length(:),width(:)
	integer(int32),optional,intent(in) :: division(:)
	real(real64),optional,intent(in) :: angle_x(:),angle_z

	real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:)
	real(real64),allocatable :: x_axis_r(:),y_axis_r(:),z_axis_r(:)
	integer(int32) :: i, j, n, internode_idx(1:2),a,b,comp_mode
	integer(int32),allocatable :: nodelist(:)
	real(real64) :: r,x,y,z,w_a,w_b,theta,p(1:2),c(1:2),dc(1:2),psi_z,z_n
	real(real64) :: w,YY,rt,r0,eps
	type(Math_) :: math
	!type(IO_) :: f

	eps = 1.0e-8
	
	
	x_axis = [-1.0d0,0.0d0,1.0d0]
	y_axis = [-1.0d0,0.0d0,1.0d0]
	z_axis = prefix_sum(length)
	x_axis_r = x_axis
	y_axis_r = y_axis
	z_axis_r = z_axis
	
	if(present(division) )then
		
		if(size(division)>=1 )then
			
			call refine(x_axis_r,division(1) )
		endif
		
		if(size(division)>=2 )then
			call refine(y_axis_r,division(2) )
		endif

		if(size(division)>=3 )then
			call refine(z_axis_r,division(3) )
		endif

	endif
	
	call this%create("Cube3D",x_axis=x_axis_r,y_axis=y_axis_r,z_axis=z_axis_r)

	!call f%open("debug.txt","w")

	do i=1,this%nn()
		x = this%getPoint_x(i)
		y = this%getPoint_y(i)
		z = this%getPoint_z(i)
		internode_idx = find_section(z_axis,z)
		a = internode_idx(1)
		b = internode_idx(2)

		
		if(a ==0)then
			w_a = width(1)
		else
			w_a = width(a)
		endif
		
		if(a==size(width,1) )then
			w_b = width(size(width,1))
		else
			w_b = width(b)
		endif
		
		
		if(a==0)then
			theta = 0.0d0
		elseif(a==size(z_axis) )then
			theta = 0.0d0
		else
			theta         = (z - z_axis(internode_idx(1)))&
				/(z_axis(internode_idx(2)) - z_axis(internode_idx(1)))
		endif
		w = (1.0d0-theta)*w_a + theta*w_b  
		x = (x/1.0d0) * abs(w/2.0d0)
		y = (y/1.0d0) * abs(w/2.0d0)

		
		
		theta = atan2(y,x)
		r     = sqrt(x*x + y*y )
		theta = theta + math%pi

		if(x==0)then
			comp_mode=1
		elseif( abs(y/x) >= 1.0d0  )then
			comp_mode=1
		else
			comp_mode=2
		endif
		
		if( comp_mode == 1  )then
			if((1.0d0-(cos(theta))**2 )/=0.0d0)then
				rt = (0.50d0*w)/(sqrt(1.0d0-(cos(theta))**2 ))
			else
				rt = 0.50d0*w
			endif
		else
			if(sqrt(1.0d0-(sin(theta))**2 )/=0.0d0)then
				rt = (0.50d0*w)/(sqrt(1.0d0-(sin(theta))**2 ))
			else
				rt = 0.50d0*w
			endif
		endif
		theta = atan2(y,x)
		r     = sqrt(x*x + y*y )
		r = r*(0.50d0*w)/rt

		! debug
		!x = r*cos(theta)
		!y = r*sin(theta)


		!call f%write(rt*cos(theta),rt*sin(theta) )
		!YY  = (0.50d0*w)/x*y 
		!R   = sqrt( (0.50d0*w)**2 + YY**2 )
		
		
		!x   = (0.50d0*w)/(R) *x
		!y   = (0.50d0*w)/(R) *y 
		

		call this%setPoint_x(idx=i,coord=x)
		call this%setPoint_y(idx=i,coord=y)
		call this%setPoint_z(idx=i,coord=z)

	enddo


	if(present(angle_x) )then
		
		! x = x + z/tan(0.50d0*math%pi-radian(angle_x(1)) )
		! z = z - z*(1 - cos(0.50d0*math%pi-radian(angle_x(1)) ))
		!$OMP parallel do private(x,y,z)
		do i=1,this%nn()
			x = this%getPoint_x(i)
			y = this%getPoint_y(i)
			z = this%getPoint_z(i)

			x = x + z/tan(0.50d0*math%pi-radian(angle_x(1)) )
			z = z*cos(radian(angle_x(1)) )

			call this%setPoint_x(i,x)
			call this%setPoint_y(i,y)
			call this%setPoint_z(i,z)

		enddo
		!$OMP end parallel do

		do i=2,min(size(z_axis),size(angle_x))
			
			do j=1,this%nn()
				x = this%getPoint_x(j)
				y = this%getPoint_y(j)
				z = this%getPoint_z(j)
				
				if(z<=z_axis(i) )cycle

				x = x + (z-z_axis(i)) /tan(0.50d0*math%pi-radian(angle_x(i)) ) 
				z = (z-z_axis(i))*cos(radian(angle_x(i)) ) + z_axis(i)
				
				call this%setPoint_x(j,x)
				call this%setPoint_y(j,y)
				call this%setPoint_z(j,z)
			enddo
		enddo
	endif
	

	if(present(angle_z) )then
		call this%rotate(z= radian(angle_z))
	endif
	! IMPLEMENTATION FAILED>
!	if(present(angle_z) )then
!		
!		c(:) = 0.0d0
!		psi_z = 0.0d0
!		z_n = 0.0d0
!		do i=1,size(angle_z)
!			
!			do j=1,this%nn()
!				x = this%getPoint_x(j)
!				y = this%getPoint_y(j)
!				z = this%getPoint_z(j)
!				
!				if(z<=z_n )cycle
!
!				p(1:2) = [x,y]
!				p = p - c
!				p = matmul( rotationMatrix(radian(angle_z(i))),p )
!				p = p + c
!				x = p(1)
!				y = p(2)
!					
!				call this%setPoint_x(j,x)
!				call this%setPoint_y(j,y)
!				call this%setPoint_z(j,z)
!			enddo
!
!			! update rotation center
!			!psi_z = psi_z + angle_z(i)
!			z_n = z_n + length(i+1)*cos( radian(angle_x(i) ))
!			c(1) = average( this%mesh%nodcoord( this%select(z_min=z_n-eps,z_max=z_n+eps) ,1)  )
!			c(2) = average( this%mesh%nodcoord( this%select(z_min=z_n-eps,z_max=z_n+eps) ,2)  )
!			!
!		enddo
!	endif
!
	!call f%close()



end function
! #######################################################

! #######################################################
subroutine add_pointFEMDomain(this,coord)
	class(FEMDomain_),intent(inout) :: this
	real(real64),intent(in) :: coord



end subroutine
! #######################################################


!function select_by_functionFEMDomain(this,surface,params,sign) result(NodeList)
!	
!	interface 
!		function surface(x,params) result(ret)
!			use iso_fortran_env
!			real(real64),intent(in) :: x(:)
!			real(real64),intent(in) :: params(:)
!			real(real64) :: ret
!		end function
!	end interface
!
!	character(*),intent(in) :: sign
!	class(FEMDomain_),intent(in) :: this
!	real(real64),intent(in) :: params(:)
!	integer(int32),allocatable :: flags(:)
!	real(real64) :: ret
!	real(real64),allocatable :: x(:)
!	integer(int32) :: i
!	integer(int32),allocatable :: NodeList(:)
!
!	flags = int(zeros(this%nn() ) ) 
!	do i=1,this%nn()
!		x = this%mesh%nodcoord(i,:)
!		ret = surface(x ,params )
!		select case(sign)
!			case(">=")
!
!		end select
!		
!		
!	enddo
!	
!
!end function

function getPointFEMDomain(this,pointIdx) result(ret)
	class(FEMDOmain_),intent(in) :: this
	integer(int32),intent(in) :: pointIdx
	real(real64),allocatable :: ret(:)

	ret = this%mesh%nodcoord(pointIdx,:)

end function


function PointForceVectorFEMDomain(this, NodeList,Direction,force) result(ret)
	class(FEMDomain_),intent(in) :: this
	integer(int32),intent(in) :: NodeList(:)
	character(*),intent(in) :: Direction
	real(real64),intent(in) :: force
	real(real64),allocatable :: ret(:)
	integer(int32) :: DOF

	DOF = this%nd()
	ret = zeros(this%nn()*this%nd() )

	if( ("X" .in. Direction) .or. ("x" .in. Direction)  )then
		ret(DOF*( NodeList(:)-1 )+1 ) = force
	endif
	if( ("Y" .in. Direction) .or. ("y" .in. Direction)  )then
		ret(DOF*( NodeList(:)-1 )+2 ) = force
	endif
	if( ("Z" .in. Direction) .or. ("z" .in. Direction)  )then
		ret(DOF*( NodeList(:)-1 )+3 ) = force
	endif

end function 


subroutine read_SCALAR_FEMDomain(this,filename)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: filename
	type(IO_) :: f
	character(:),allocatable :: line,POINT_DATA
	integer(int32) :: i,n,num_pd

	if(allocated(this%mesh%nodcoord))then
		POINT_DATA = "POINT_DATA"
		call f%search_line(filename,n)
		call f%open(filename,"r")
		do i=1,n-1
			read (f%fh,'()') ! 読み飛ばし
		enddo
		read(f%fh,*) POINT_DATA,num_pd
		read (f%fh,'()') 
		read (f%fh,'()') 
		if(.not.allocated(this%physicalfield) )then
			allocate(this%physicalfield(1))
		endif
		
		this%physicalfield(1)%scalar = zeros(num_pd)
		do i=1,num_pd
			read(f%fh,*) this%physicalfield(1)%scalar(i)
		enddo
		call f%close()
	else
		call this%read(filename)
	endif

end subroutine

function elementType2VTKCellType(elementType) result(ret)
	integer(int32),intent(in) :: elementType(:)
	integer(int32) :: ret

	if(elementType(1)==1)then
		! 1-dimensional
		if(elementType(2)==1)then
			ret = VTK_VERTEX
		elseif(elementType(2)==2)then
			ret = VTK_LINE
		elseif(elementType(2)==3)then
			ret = VTK_QUADRATIC_EDGE
		else 
			ret = -1
		endif
	elseif(elementType(1)==2)then
		! 2-dimensional
		if(elementType(2)==3)then
			ret = VTK_TRIANGLE
		elseif(elementType(2)==4)then
			ret = VTK_QUAD
		elseif(elementType(2)==6)then
			ret = VTK_QUADRATIC_TRIANGLE
		elseif(elementType(2)==8)then
			ret = VTK_QUADRATIC_QUAD
		else 
			ret = -1
		endif
	elseif(elementType(1)==3)then
		! 3-dimensional
		if(elementType(2)==4)then
			ret = VTK_TETRA
		elseif(elementType(2)==6)then
			ret = VTK_WEDGE
		elseif(elementType(2)==8)then
			ret = VTK_HEXAHEDRON
		elseif(elementType(2)==10)then
			ret = VTK_QUADRATIC_TETRA
		elseif(elementType(2)==12)then
			ret = VTK_QUADRATIC_LINEAR_WEDGE
		elseif(elementType(2)==20)then
			ret = VTK_QUADRATIC_HEXAHEDRON
		else 
			ret = -1
		endif
	endif
end function


function to_vertexData_FEMDomain(this,vertexIDs,scalar) result(vertexData)
	class(FEMDomain_),intent(in) :: this
	integer(int32),intent(in) :: vertexIDs(:)
	real(real64),intent(in) :: scalar(:)
	real(real64),allocatable :: vertexData(:)

	allocate(vertexData(size(vertexIDs) ) )

	vertexData(:) = scalar(vertexIDs)

end function



subroutine vtk_file(name,vertices,vertexData)
	character(*),intent(in) :: name
	real(real64),intent(in) :: vertices(:),vertexData(:)
	character(:),allocatable :: vtk_filename
	type(IO_) :: f
	integer(int32) :: i

	if(".vtk" .in. name)then
		vtk_filename = name
	else
		vtk_filename = name + ".vtk"
	endif

	call f%open(vtk_filename,"w")
	call f%write("# vtk DataFile Version 3.0")
	call f%write(vtk_filename)
	call f%write("ASCII")
	call f%write("DATASET UNSTRUCTURED_GRID")
	call f%write("POINTS "+str(size(vertices)/3)+" float")
	do i=1,size(vertices)/3
		call f%write(vertices( (i-1)*3+1:(i-1)*3+3 ))
	enddo
	call f%write("CELL_TYPES "+str(size(vertices)/3))
	do i=1,size(vertices)/3
		call f%write("1")
	enddo
	call f%write("POINT_DATA "+str(size(vertexData)))
	call f%write("SCALARS radius float")
	call f%write("LOOKUP_TABLE default")
	do i=1,size(vertexData)
		write(f%fh,*) vertexData(i)
	enddo
	call f%close()
end subroutine


! ########################################################
function to_ptr_femdomain(femdomain) result(ret)
	type(femdomain_),target,intent(in) :: femdomain
	type(femdomainp_) :: ret
	
	ret%femdomainp => femdomain
	
end function
! ########################################################




! ########################################################
function to_ptr_femdomains(femdomains) result(ret)
	type(femdomain_),target,intent(in) :: femdomains(:)
	type(femdomainp_),allocatable :: ret(:)
	integer(int32) :: i
	
	allocate(ret(size(femdomains) ) )
	do i=1,size(femdomains)
		ret(i)%femdomainp => femdomains(i)
	enddo
	
end function
! ########################################################

! ########################################################
subroutine getLayer_scalarFEMDomain(this,name,ret)
	class(FEMDomain_),intent(inout) :: this
	character(*),intent(in) :: name
	real(real64),allocatable :: ret(:)

	ret = this%PhysicalField( this%getLayerID(name) )%scalar

end subroutine
! ########################################################

! ########################################################
function get_element_idx_FEMDomainPointer(FEMDomainPointer,DomainID,ElementID) result(ret)
	type(FEMDomainp_),intent(in) :: FEMDomainPointer(:)
	integer(int32),intent(in) :: DomainID,ElementID
	integer(int32) :: ret, i

	ret = 0
	do i=1,DomainID-1
		ret = ret + FEMDomainPointer(i)%femdomainp%ne()
	enddo
	ret = ret + ElementID
	
end function
! ########################################################


! ########################################################
function get_node_list_by_range_FEMDP(FEMDomainPointer,range) result(ret)
	type(FEMDomainp_),intent(in) :: FEMDomainPointer(:)
	type(Range_),intent(in) :: range
	integer(int32) :: i,offset
	integer(int32),allocatable :: ret(:),dret(:)
	
	offset = 0
	allocate(ret(0))
	do i=1,size(FEMDomainPointer)
		dret = FEMDomainPointer(i)%femdomainp%select(&
			x_min=range%x_range(1),&
			y_min=range%y_range(1),&
			z_min=range%z_range(1),&
			x_max=range%x_range(2),&
			y_max=range%y_range(2),&
			z_max=range%z_range(2)&
		)
		dret(:) = dret(:) + offset
		ret = ret // dret
		offset = offset + FEMDomainPointer(i)%femdomainp%nn()
	enddo
	
end function
! ########################################################


! ########################################################
function num_node_femdomain_pointers(FEMDomainPointer) result(ret)
	type(FEMDomainp_),intent(in) :: FEMDomainPointer(:)
	integer(int32) :: ret, i
	ret = 0
	do i=1,size(FEMDomainPointer)
		ret = ret + FEMDomainPointer(i)%femdomainp%nn()
	enddo

end function
! ########################################################


! ########################################################
function num_element_femdomain_pointers(FEMDomainPointer) result(ret)
	type(FEMDomainp_),intent(in) :: FEMDomainPointer(:)
	integer(int32) :: ret, i
	ret = 0
	do i=1,size(FEMDomainPointer)
		ret = ret + FEMDomainPointer(i)%femdomainp%ne()
	enddo

end function
! ########################################################


! ########################################################
subroutine export_vtk_FEMDomainPointer(FEMDomainPointer,name,field,displacement)
	type(FEMDomainp_),intent(in) :: FEMDomainPointer(:)
	character(*),intent(in) :: name
	real(real64),intent(in) :: field(:)
	real(real64),optional,intent(in) :: displacement(:)
	real(real64),allocatable :: vec(:),vf_array(:,:)
	integer(int32) :: DOF,offset,i,offset_nn

	
	if(mod(size(field),num_node(FEMDomainPointer))==0 )then
		DOF = size(field)/num_node(FEMDomainPointer)
		! node_wise value
		if(DOF==1)then
			! scalar field
			offset = 0
			do i=1,size(FEMDomainPointer,1)
				if(present(displacement) )then
					call FEMDomainPointer(i)%femdomainp%vtk(name="domain_"+zfill(i,5)+name,&
						scalar=field(offset+1:offset+DOF*FEMDomainPointer(i)%femdomainp%nn()),&
						displacement=field(offset+1:offset+FEMDomainPointer(i)%femdomainp%nd()&
							*FEMDomainPointer(i)%femdomainp%nn()))
				else
					call FEMDomainPointer(i)%femdomainp%vtk(name="domain_"+zfill(i,5)+name,&
						scalar=field(offset+1:offset+DOF*FEMDomainPointer(i)%femdomainp%nn()))
				endif
				offset = offset + DOF*FEMDomainPointer(i)%femdomainp%nn()
			enddo
		elseif(DOF>=2)then
			! vector field
			offset = 0
			do i=1,size(FEMDomainPointer,1)
				vec = field(offset+1:offset+DOF*FEMDomainPointer(i)%femdomainp%nn())
				vf_array = reshape(vec,[size(vec)/DOF,DOF ])
				if(present(displacement) )then
					call FEMDomainPointer(i)%femdomainp%vtk(name="domain_"+zfill(i,5)+name,&
						vector=vf_array,&
						displacement=field(offset+1:offset+FEMDomainPointer(i)%femdomainp%nd()&
							*FEMDomainPointer(i)%femdomainp%nn()))
				else
					call FEMDomainPointer(i)%femdomainp%vtk(name="domain_"+zfill(i,5)+name,&
						vector=vf_array)
				endif
				offset = offset + DOF*FEMDomainPointer(i)%femdomainp%nn()
			enddo
		endif
		return
	endif


	if(mod(size(field),num_element(FEMDomainPointer))==0 )then
		DOF = size(field)/num_element(FEMDomainPointer)
		! node_wise value
		if(DOF==1)then
			! scalar field
			offset = 0
			offset_nn = 0
			do i=1,size(FEMDomainPointer,1)
				if(present(displacement) )then
					call FEMDomainPointer(i)%femdomainp%vtk(name=name+"_domain_"+zfill(i,5),&
						scalar=field(offset+1:offset+DOF*FEMDomainPointer(i)%femdomainp%ne()),&
						displacement=field(offset_nn+1:offset_nn+FEMDomainPointer(i)%femdomainp%nd()&
							*FEMDomainPointer(i)%femdomainp%nn()))
				else
					call FEMDomainPointer(i)%femdomainp%vtk(name=name+"_domain_"+zfill(i,5),&
					scalar=field(offset+1:offset+DOF*FEMDomainPointer(i)%femdomainp%ne()))
				endif
				offset = offset + DOF*FEMDomainPointer(i)%femdomainp%ne()
				offset_nn = offset_nn + DOF*FEMDomainPointer(i)%femdomainp%nn()
			enddo
		elseif(DOF>=2)then
			! vector field
			offset = 0
			offset_nn = 0
			do i=1,size(FEMDomainPointer,1)
				vec = field(offset+1:offset+DOF*FEMDomainPointer(i)%femdomainp%ne())
				vf_array = reshape(vec,[size(vec)/DOF,DOF ])
				if(present(displacement) )then
					call FEMDomainPointer(i)%femdomainp%vtk(name=name+"_domain_"+zfill(i,5),&
						vector=vf_array,&
						displacement=field(offset_nn+1:offset_nn+FEMDomainPointer(i)%femdomainp%nd()&
							*FEMDomainPointer(i)%femdomainp%nn()))
				else
					call FEMDomainPointer(i)%femdomainp%vtk(name=name+"_domain_"+zfill(i,5),&
						vector=vf_array)
				endif
				offset = offset + DOF*FEMDomainPointer(i)%femdomainp%ne()
				offset_nn = offset_nn + DOF*FEMDomainPointer(i)%femdomainp%nn()
			enddo
		endif
		return
	endif
end subroutine

end module FEMDomainClass


