module SoybeanClass
   use, intrinsic :: iso_fortran_env
   use sim
   use SeedClass
   use LeafClass
   use RootClass
   use LightClass
   use PlantNodeClass
   use StemClass
   use FEMSolverClass
   use EnvironmentClass
   use MeristemClass
   implicit none

   integer(int32), parameter :: PF_SOY_OBJECT_WISE = 1

   type :: soybean_internode_info_
      real(real64), allocatable :: FinalInterNodeLength(:)
      real(real64), allocatable :: FinalPetioleLength(:)
      real(real64), allocatable :: FinalLeafLength(:)
      real(real64), allocatable :: FinalLeafWidth(:)
   end type

   type :: soybean_NodeID_Branch_
      integer(int32), allocatable :: ID(:)
   contains
      procedure, public :: sync => syncsoybean_NodeID_Branch
   end type

   integer(int32), parameter :: PF_DEFORMATION_ANALYSIS = 100
   integer(int32), parameter :: PF_DEFAULT_SOYBEAN_ASIZE = 300

   ! connectivity information
   ! s2s, l2s, ...etc.
   !(1-1) stem to stem
   integer(int32), private, parameter :: PF_SOYBEAN_MAINSTEM_TO_MAINSTEM = 1
   integer(int32), private, parameter :: PF_SOYBEAN_BRANCH_TO_MAINSTEM   = 2
   integer(int32), private, parameter :: PF_SOYBEAN_BRANCH_TO_BRANCH     = 3
   !(1-2) petiole to stem
   integer(int32), private, parameter :: PF_SOYBEAN_PETIOLE_TO_MAINSTEM  = 4
   integer(int32), private, parameter :: PF_SOYBEAN_PETIOLE_TO_BRANCH    = 5
   integer(int32), private, parameter :: PF_SOYBEAN_PETIOLE_TO_PETIOLE   = 6
   !(2) leaf to stem/petiole
   integer(int32), private, parameter :: PF_SOYBEAN_LEAF_TO_PETIOLE    = 11
   integer(int32), private, parameter :: PF_SOYBEAN_LEAF_TO_MAINSTEM   = 12
   integer(int32), private, parameter :: PF_SOYBEAN_LEAF_TO_BRANCH     = 13
   !(2) root to stem/petiole
   integer(int32), private, parameter :: PF_SOYBEAN_ROOT_TO_ROOT       = 21
   integer(int32), private, parameter :: PF_SOYBEAN_ROOT_TO_MAINSTEM   = 22
   integer(int32), private, parameter :: PF_SOYBEAN_ROOT_TO_BRANCH     = 23

   !(2) root to meristem to stem
   integer(int32), private, parameter :: PF_SOYBEAN_MERISTEM_TO_MAINSTEM = 31
   integer(int32), private, parameter :: PF_SOYBEAN_MERISTEM_TO_BRANCH   = 32

   type :: soybean_

      ! [new implementation with the meristem-class]
      type(Meristem_),allocatable :: meristem(:)

      ! setting
      integer(int32) :: stem_division(1:3) = [3, 3, 30]
      integer(int32) :: peti_division(1:3) = [3, 3, 30]
      integer(int32) :: leaf_division(1:3) = [10, 1, 20]
      integer(int32) :: root_division(1:3) = [2, 2, 20]

      ! growth_habit = determinate, indeterminate, semi-indeterminate, or vine
      character*20 :: growth_habit
      character*2  :: growth_stage
      integer(int32) :: Num_Of_Node
      integer(int32) :: num_leaf
      integer(int32) :: num_stem_node
      integer(int32) :: Num_Of_Root

      integer(int32) :: TYPE_STEM = 1
      integer(int32) :: TYPE_LEAF = 2
      integer(int32) :: TYPE_ROOT = 3

      integer(int32) :: MaxLeafNum = PF_DEFAULT_SOYBEAN_ASIZE
      integer(int32) :: MaxRootNum = PF_DEFAULT_SOYBEAN_ASIZE
      integer(int32) :: MaxStemNum = PF_DEFAULT_SOYBEAN_ASIZE

      logical :: determinate
      integer(int32) :: max_num_leaf_per_petiole = 3 ! as default

      integer(int32)  :: ms_node, br_node(PF_DEFAULT_SOYBEAN_ASIZE), br_from(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: ms_length, br_length(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: ms_width, br_width(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: ms_angle_ave, br_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: ms_angle_sig, br_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE)

      integer(int32)  :: mr_node, brr_node(PF_DEFAULT_SOYBEAN_ASIZE), brr_from(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: mr_length, brr_length(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: mr_width, brr_width(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: mr_angle_ave, brr_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: mr_angle_sig, brr_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE)

      real(real64)    :: peti_size_ave(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: peti_size_sig(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: peti_width_ave(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: peti_width_sig(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: peti_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE)
      real(real64)    :: peti_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE)

      real(real64)    :: leaf_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE*3)
      real(real64)    :: leaf_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE*3)
      real(real64)    :: leaf_length_ave(PF_DEFAULT_SOYBEAN_ASIZE*3)
      real(real64)    :: leaf_length_sig(PF_DEFAULT_SOYBEAN_ASIZE*3)
      real(real64)    :: leaf_width_ave(PF_DEFAULT_SOYBEAN_ASIZE*3)
      real(real64)    :: leaf_width_sig(PF_DEFAULT_SOYBEAN_ASIZE*3)
      real(real64)    :: leaf_thickness_ave(PF_DEFAULT_SOYBEAN_ASIZE*3)
      real(real64)    :: leaf_thickness_sig(PF_DEFAULT_SOYBEAN_ASIZE*3)

      character(3) :: Stage ! VE, CV, V1,V2, ..., R1, R2, ..., R8
      character(200) :: name
      integer(int32)::stage_id = 0
      real(real64) :: dt
      type(Seed_) :: Seed
      type(PlantNode_), allocatable :: NodeSystem(:)
      type(PlantRoot_), allocatable :: RootSystem(:)

      type(Stem_), allocatable :: Stem(:)
      type(Leaf_), allocatable :: Leaf(:)
      type(Root_), allocatable :: Root(:)

      ! material info
      real(real64), allocatable :: stemYoungModulus(:)
      real(real64), allocatable :: leafYoungModulus(:)
      real(real64), allocatable :: rootYoungModulus(:)

      real(real64), allocatable :: stemPoissonRatio(:)
      real(real64), allocatable :: leafPoissonRatio(:)
      real(real64), allocatable :: rootPoissonRatio(:)

      real(real64), allocatable :: stemDensity(:)
      real(real64), allocatable :: leafDensity(:)
      real(real64), allocatable :: rootDensity(:)

      ! 節-節点データ構造
      type(Mesh_) :: struct
      integer(int32), allocatable :: meristem2stem(:,:)
      integer(int32), allocatable :: leaf2stem(:, :)
      integer(int32), allocatable :: stem2stem(:, :)
      integer(int32), allocatable :: root2stem(:, :)
      integer(int32), allocatable :: root2root(:, :)


      ! 器官オブジェクト配列 (regacy)
      type(FEMDomain_), allocatable :: leaf_list(:)
      type(FEMDomain_), allocatable :: stem_list(:)
      type(FEMDomain_), allocatable :: root_list(:)

      ! シミュレータ
      type(ContactMechanics_) :: contact
      real(real64) :: time
      real(real64) :: seed_length
      real(real64) :: seed_width
      real(real64) :: seed_height
      real(real64), allocatable :: stem_angle(:, :)
      real(real64), allocatable :: root_angle(:, :)
      real(real64), allocatable :: leaf_angle(:, :)

      character(200) :: stemconfig = ""
      character(200) :: rootconfig = ""
      character(200) :: leafconfig = ""

      ! for deformation analysis
      logical :: property_deform_material_density = .false.
      logical :: property_deform_material_YoungModulus = .false.
      logical :: property_deform_material_PoissonRatio = .false.
      logical :: property_deform_material_CarbonDiffusionCoefficient = .false.
      logical :: property_deform_initial_Displacement = .false.
      logical :: property_deform_initial_Stress = .false.
      logical :: property_deform_boundary_TractionForce = .false.
      logical :: property_deform_boundary_Displacement = .false.
      logical :: property_deform_gravity = .false.

      real(real64) :: Gravity_acceralation = 9.810d0
      real(real64) :: PenaltyParameter = 100000.0d0
      logical :: GaussPointProjection = .false.
      integer(int32) :: overset_algorithm = FEMDomain_Overset_GPP !

      integer(int32), allocatable :: NodeID_MainStem(:)
      type(soybean_NodeID_Branch_), allocatable :: NodeID_Branch(:)

      logical ::  inLoop = .false.
      real(real64) :: hours = 0.0d0

      ! growth simulation
      real(real64) :: FullyExpanded_stem_threshold = 0.10d0
      integer(int32) :: MaxBranchNum = 20
      type(soybean_internode_info_), allocatable :: InterNodeInfo(:)
      real(real64) :: default_Leaf_growth_ratio = 1.0d0/3.0d0
      real(real64) :: default_Stem_growth_ratio = 1.0d0/3.0d0
      integer(int32), allocatable :: MainStem_num_branch(:)
      real(real64) :: apical_dominance_distance = 1.0d0

      ! create CV
      real(real64) :: CV_stem_length_ave = 0.03d0
      real(real64) :: CV_stem_length_sig = 0.001d0
      real(real64) :: CV_stem_width_ave = 0.003d0
      real(real64) :: CV_stem_width_sig = 0.00001d0
      real(real64) :: CV_leaf_length_ave = 0.03d0
      real(real64) :: CV_leaf_length_sig = 0.001d0
      real(real64) :: CV_leaf_width_ave = 0.02d0
      real(real64) :: CV_leaf_width_sig = 0.0005d0
      real(real64) :: CV_leaf_thickness_ave = 0.005d0
      real(real64) :: CV_leaf_thickness_sig = 0.0001d0

      real(real64) :: VC_stem_length_ave = 0.04d0
      real(real64) :: VC_stem_length_sig = 0.001d0
      real(real64) :: VC_stem_width_ave = 0.004d0
      real(real64) :: VC_stem_width_sig = 0.0001d0

      real(real64) :: VC_leaf_length_ave = 0.03d0
      real(real64) :: VC_leaf_length_sig = 0.001d0
      real(real64) :: VC_leaf_width_ave = 0.03d0
      real(real64) :: VC_leaf_width_sig = 0.0005d0
      real(real64) :: VC_leaf_thickness_ave = 0.001d0
      real(real64) :: VC_leaf_thickness_sig = 0.00001d0

      character(36) :: UUID

      ! carbon flow and photosynthesis
      ! carbon concentration (micro-gram/m^3) at apical
      real(real64) :: apical_carbon_concentration = 0.01d0
      real(real64), allocatable :: Photosynthate_n(:), reaction_n(:)


   contains
      !procedure,public :: addRoot => addRootSoybean
      !procedure,public :: addLeaf => addLeafSoybean

      ! creation
      procedure, pass :: initsoybean
      procedure, pass :: init_as_seed_soybean
      generic :: init => initsoybean,init_as_seed_soybean

      
      procedure, pass :: setMeristem_soybeanclass
      procedure, pass :: setMeristem_multi_soy
      generic, public :: setMeristem => setMeristem_soybeanclass,setMeristem_multi_soy
      

      procedure, public :: VC => VCSoybean

      procedure, public :: remove => removeSoybean
      procedure, public :: create => initsoybean
      procedure, public :: new => initsoybean
      procedure, public :: sowing => initsoybean
      procedure, public :: export => exportSoybean
      ! procedure, public :: expanition => expanitionSoybean
      ! procedure, public :: development => developmentSoybean


      !  Simulator
      procedure, public :: checkProperties => checkPropertiesSoybean
      procedure, public :: setPoints => setPointsSoybean
      procedure, public :: setProperties => setPropertiesSoybean
      procedure, public :: easy_grow => easy_grow_SoybeanClass

      
      ! editor
      procedure, public :: set_stem_length_by_list => set_stem_length_by_list_Soybean
      procedure, public :: set_stem_angle_by_list => set_stem_angle_by_list_Soybean

      ! simple setters
      procedure, public :: addStem => addStemSoybean
      procedure, public :: setPropertiesDensity => setPropertiesDensitySoybean
      procedure, public :: setPropertiesYoungModulus => setPropertiesYoungModulusSoybean
      procedure, public :: setPropertiesPoissonRatio => setPropertiesPoissonRatioSoybean
      procedure, public :: setPropertiesInitialDisplacement => setPropertiesInitialDisplacementSoybean
      procedure, public :: setPropertiesInitialStress => setPropertiesInitialStressSoybean
      procedure, public :: setPropertiesBoundaryTractionForce => setPropertiesBoundaryTractionForceSoybean
      procedure, public :: setPropertiesBoundaryDisplacement => setPropertiesBoundaryDisplacementSoybean
      procedure, public :: setPropertiesGravity => setPropertiesGravitySoybean
      procedure, public :: setFEMDomains => setFEMDomainsSoybean
      procedure, public :: setFEMDomain => setFEMDomainsSoybean


      ! alternative setters
      procedure, public :: setYoungModulus => setYoungModulusSoybean
      procedure, public :: setPoissonRatio => setPoissonRatioSoybean
      procedure, public :: setDensity => setDensitySoybean

      procedure, public :: runSimulation => runSimulationSoybean
      procedure, public :: runSimulator => runSimulationSoybean
      ! readyForSoybean
      procedure, public :: readyFor => readyForSoybean

      ! observation/info
      procedure, public :: stemlength => stemlengthSoybean
      procedure, public :: NumberOfBranch => NumberOfBranchSoybean
      procedure, public :: isMainStem => isMainStemSoybean
      procedure, public :: isBranchStem => isBranchStemSoybean

      procedure, public :: checkYoungModulus => checkYoungModulusSoybean
      procedure, public :: checkPoissonRatio => checkPoissonRatioSoybean
      procedure, public :: checkDensity => checkDensitySoybean

      procedure, public :: checkMemoryRequirement => checkMemoryRequirementSoybean

      procedure, public :: getYoungModulus => getYoungModulusSoybean
      procedure, public :: getPoissonRatio => getPoissonRatioSoybean
      procedure, public :: getDensity => getDensitySoybean
      procedure, public :: getVertices => getVerticesSoybean

      procedure, public :: getYoungModulusField => getYoungModulusFieldSoybean
      procedure, public :: getPoissonRatioField => getPoissonRatioFieldSoybean
      procedure, public :: getDensityField => getDensityFieldSoybean
      procedure, public :: getDiffusionCoefficient => getDiffusionCoefficientSoybean

      ! these two functions are different!!!
      ! get [obj_idx(:),obj_type(:),local element idx(:)]
      procedure, public :: getElementList => getElementListSoybean
      ! get global Element Idx
      procedure, public :: getGlobalElementIdx => getGlobalElementIdxSoybean

      ! stem length, stem angles
      procedure, public :: get_stem_length_list => get_stem_length_list_Soybean
      procedure, public :: get_stem_angle_list  => get_stem_angle_list_Soybean

      procedure, public :: MassMatrix => MassMatrixSoybean
      procedure, public :: StiffnessMatrix => StiffnessMatrixSoybean

      

      ! operation
      procedure, public :: findApical => findApicalSoybean

      procedure, public :: grow => growSoybean
      procedure, public :: getVolume => getVolumeSoybean
      procedure, public :: getVolumePerElement => getVolumePerElementSoybean
      procedure, public :: getBioMass => getBioMassSoybean
      procedure, public :: getElementBiomass => getElementBiomassSoybean
      procedure, public :: getTotalWeight => getTotalWeightSoybean
      procedure, public :: getSubDomain => getSubDomainSoybean
      procedure, public :: getSubDomainType => getSubDomainTypeSoybean
      procedure, public :: setSubDomain => setSubDomainSoybean
      procedure, public :: getPoints => getPointsSoybean
      procedure, public :: getRadius => getRadiusSoybean
      procedure, public :: getCenter => getCenterSoybean
      procedure, public :: getDistanceFromGround => getDistanceFromGroundSoybean
      procedure, public :: getNumberOfPoint => getNumberOfPointSoybean
      procedure, public :: getNumberOfElement => getNumberOfElementSoybean
      procedure, public :: getDistanceToGroundFromStemID &
         => getDistanceToGroundFromStemIDSoybean
      
      procedure, public :: getNumberOfBranch => getNumberOfBranch_Soy
      procedure, public :: getMainStemTipIdx => getMainStemTipIdx_Soy
      procedure, public :: getBranchStemTipIdx => getBranchStemTipIdx_Soy
      procedure, public :: getBranchBaseStemIdx => getBranchBaseStemIdx_Soy
      procedure, public :: getTipOfStem => getTipOfStem_SoybeanClass
      procedure, public :: getNumberOfInternode => getNumberOfInternode_Soy
      procedure, public :: getInternodes => getInternodes_Soy
      

      procedure, public :: getDistanceToGroundFromRootID &
         => getDistanceToGroundFromRootIDSoybean
      procedure, public :: getLeafCosValue => getLeafCosValueSoybean

      procedure, public :: getRangeOfNodeID => getRangeOfNodeIDSoybean
      procedure, public :: getFEMDomainPointers => getFEMDomainPointersSoybean
      procedure, public :: fall_leaf => fall_leafSoybean
      procedure, public :: getFEMDomains => to_FEMDomainsSoybean
      procedure, public :: to_FEMDomains => to_FEMDomainsSoybean

      ! >> simulation
      procedure, public :: getPPFD => getPPFDSoybean
      procedure, public :: getSpectrum => getSpectrumSoybean
      procedure, public :: to_R_FR => to_R_FRSoybean

      procedure, public :: getDisplacement => getDisplacementSoybean
      procedure, public :: getEigenMode => getEigenModeSoybean

      procedure, pass :: getPhotoSynthesisSoybean
      procedure, pass :: getPhotoSynthesis_by_env_soybean
      generic :: getPhotoSynthesis => getPhotoSynthesis_by_env_soybean, getPhotoSynthesisSoybean

      procedure, public :: getPhotoSynthesisSpeedPerVolume => getPhotoSynthesisSpeedPerVolumeSoybean
      procedure, public :: getLeafArea => getLeafAreaSoybean
      procedure, public :: getIntersectLeaf => getIntersectLeafSoybean
      procedure, public :: getOverwrapLeaf => getIntersectLeafSoybean

      procedure, public :: searchStem => searchStemSoybean
      procedure, public :: searchPetiole => searchPetioleSoybean
      procedure, public :: searchLeaf => searchLeafSoybean

      ! post-processing
      procedure, public :: export_eig => export_eigSoybean
      procedure, public :: getStressField => getStressFieldSoybean

      ! max *** ID
      procedure, public :: maxleafID => maxleafIDSoybean
      procedure, public :: maxInterNodeID => maxInterNodeIDSoybean
      procedure, public :: maxPetioleID => maxPetioleIDSoybean
      procedure, public :: maxStemID => maxStemIDSoybean

      ! data-format converter
      procedure, public :: convertDataFormat => convertDataFormatSoybean

      procedure, public :: fixReversedElements => fixReversedElementsSoybean

      procedure, public :: resize => resizeSoybean
      procedure, public :: deform => deformSoybean
      ! MPI
      procedure, public :: sync => syncSoybean

      ! visualization
      procedure, public :: show => showSoybean
      procedure, public :: gmsh => gmshSoybean
      procedure, public :: msh => mshSoybean
      procedure, public :: vtk => vtkSoybean
      procedure, public :: stl => stlSoybean
      procedure, public :: ply => plySoybean
      procedure, public :: json => jsonSoybean

      ! get info
      !procedure,public :: properties => propertiesSoybean
      ! number of subdomain
      procedure, public :: ns => nsSoybean
      ! number of element
      procedure, public :: ne => neSoybean
      ! number of points
      procedure, public :: nn => nnSoybean
      procedure, public :: np => nnSoybean
      procedure, public :: branchID => branchIDSoybean
      ! range of pointIDs for [Organ type, ID]
      procedure, public :: nn_range => nn_rangeSoybean

      ! observe
      procedure,public  :: height => height_Soybean

      procedure,public  :: xmin   => x_min_Soybean
      procedure,public  :: x_min  => x_min_Soybean
      procedure,public  :: ymin   => y_min_Soybean
      procedure,public  :: y_min  => y_min_Soybean
      procedure,public  :: zmin   => z_min_Soybean
      procedure,public  :: z_min  => z_min_Soybean

      procedure,public  :: xmax   => x_max_Soybean
      procedure,public  :: x_max  => x_max_Soybean
      procedure,public  :: ymax   => y_max_Soybean
      procedure,public  :: y_max  => y_max_Soybean
      procedure,public  :: zmax   => z_max_Soybean
      procedure,public  :: z_max  => z_max_Soybean

      ! regacy/experimental
      procedure, public :: WaterAbsorption => WaterAbsorptionSoybean
      procedure, public :: move => moveSoybean
      procedure, public :: rotate => rotateSoybean

      procedure, public :: numMeristem => numMeristemSoybean
      procedure, public :: numleaf => numleafsoybean
      procedure, public :: numstem => numstemsoybean
      procedure, public :: numroot => numrootsoybean

      procedure, public :: laytracing => laytracingsoybean
      procedure, public :: SinkSourceFlow => SinkSourceFlowSoybean

      procedure, public :: update => updateSoybean
      procedure, public :: updateFlowers => updateFlowersSoybean
      procedure, public :: updatePods => updatePodsSoybean
      procedure, public :: AddNode => AddNodeSoybean
      procedure, public :: AddPhytomere => AddNodeSoybean

      ! structure editor/analyzer
      procedure, pass ::  resizeStem => resizeStemSoybean
      procedure, pass ::  rotateStem => rotateStemSoybean
      procedure, pass ::  resizePetiole => resizePetioleSoybean
      procedure, pass ::  rotatePetiole => rotatePetioleSoybean
      procedure, pass ::  resizeLeaf => resizeLeafSoybean

      ! growth parameters
      procedure, pass :: setFinalInterNodeLength => setFinalInterNodeLengthSoybean
      procedure, pass :: setFinalPetioleLength => setFinalPetioleLengthSoybean
      procedure, pass :: setFinalLeafLength => setFinalLeafLengthSoybean
      procedure, pass :: setFinalLeafWidth => setFinalLeafWidthSoybean

      ! converter
      procedure, public :: ElementID2NodeID => ElementID2NodeIDSoybean

      ! essential routines for growth simulation
      procedure, pass :: getcarbon_concentration => getCarbon_concentrationSoybean
      procedure, pass :: getRespiration => getRespirationSoybean
      procedure, pass :: getCarbonFlow => getCarbonFlowSoybean
   end type

   type :: soybeanp_
      type(soybean_), pointer :: soybeanp => null()
   end type

   type :: SoybeanCanopy_
      real(real64) :: inter_row, intra_row
      type(soybean_), allocatable :: Canopy(:, :)
   end type

   interface to_soybean
      module procedure to_soybean_soybeanclass
   end interface

contains

!
   subroutine VCSoybean(this)
      class(Soybean_), intent(inout) :: this
      type(Random_) :: random
      integer(int32) :: i
      real(real64) :: y_val, z_val, x_val, leaf_z_angles(4)

      !initialize
      call print("[WARNING] soybean % VC() is deprecated,")
      
      call this%remove()

      ! set default parameters

      ! set default parameters
      ! stem
      this%br_node(:) = 0
      this%br_from(:) = 0
      this%br_length(:) = 0.0d0

      this%br_angle_ave(:) = 0.0d0
      this%br_angle_sig(:) = 10.0d0
      !this%br_angle_ave(1)=10.0d0
      !this%br_angle_sig(1)=2.0d0

      this%ms_angle_ave = 0.0d0
      this%ms_angle_sig = 2.0d0

      ! for roots
      this%brr_node(:) = 0
      this%brr_from(:) = 0
      this%brr_length(:) = 0.0d0

      this%brr_angle_ave(:) = 0.0d0
      this%brr_angle_sig(:) = 10.0d0
      this%brr_angle_ave(1) = 30.0d0
      this%brr_angle_sig(1) = 2.0d0

      this%mr_angle_ave = 0.0d0
      this%mr_angle_sig = 2.0d0
      ! peti
      ! is also stem

      this%peti_size_ave(:) = 0.20d0
      this%peti_size_sig(:) = 0.010d0

      this%peti_width_ave(:) = 0.0050d0
      this%peti_width_sig(:) = 0.00010d0

      this%peti_angle_ave(:) = 30.0d0
      this%peti_angle_sig(:) = 1.00d0

      ! leaf
      this%leaf_length_ave(:) = 0.20d0
      this%leaf_length_sig(:) = 0.01d0

      this%leaf_width_ave(:) = 0.050d0
      this%leaf_width_sig(:) = 0.010d0

      this%leaf_thickness_ave(:) = 0.00100d0
      this%leaf_thickness_sig(:) = 0.00050d0

      this%leaf_angle_ave(:) = 80.0d0
      this%leaf_angle_sig(:) = 10.0d0

      allocate (this%leaf(this%MaxLeafNum))
      allocate (this%root(this%MaxrootNum))
      allocate (this%stem(this%MaxstemNum))

      allocate (this%leafYoungModulus(this%MaxLeafNum))
      allocate (this%rootYoungModulus(this%MaxrootNum))
      allocate (this%stemYoungModulus(this%MaxstemNum))
      ! default value
      this%leafYoungModulus(:) = 1000.0d0
      this%rootYoungModulus(:) = 1000.0d0
      this%stemYoungModulus(:) = 1000.0d0

      allocate (this%leafPoissonRatio(this%MaxLeafNum))
      allocate (this%rootPoissonRatio(this%MaxrootNum))
      allocate (this%stemPoissonRatio(this%MaxstemNum))
      this%leafPoissonRatio(:) = 0.30d0
      this%rootPoissonRatio(:) = 0.30d0
      this%stemPoissonRatio(:) = 0.30d0

      allocate (this%leafDensity(this%MaxLeafNum))
      allocate (this%rootDensity(this%MaxrootNum))
      allocate (this%stemDensity(this%MaxstemNum))

      this%leafDensity(:) = 0.0d0
      this%rootDensity(:) = 0.0d0
      this%stemDensity(:) = 0.0d0

      allocate (this%stem2stem(this%MaxstemNum, this%MaxstemNum))
      allocate (this%leaf2stem(this%MaxstemNum, this%MaxLeafNum))
      allocate (this%root2stem(this%MaxrootNum, this%MaxstemNum))
      allocate (this%root2root(this%MaxrootNum, this%MaxrootNum))
      this%stem2stem(:, :) = 0
      this%leaf2stem(:, :) = 0
      this%root2stem(:, :) = 0
      this%root2root(:, :) = 0

      ! create VC plant
      ! create stage CV
      this%NodeID_MainStem = eyes(1)
      call this%stem(1)%init()

      this%stem(1)%stemID = 0
      this%stem(1)%InterNodeID = 1
      this%stem(1)%already_grown = .true.

      call this%stem(1)%resize( &
         x=random%gauss(mu=this%CV_stem_width_ave, sigma=this%CV_stem_width_sig), &
         y=random%gauss(mu=this%CV_stem_width_ave, sigma=this%CV_stem_width_sig), &
         z=random%gauss(mu=this%CV_stem_length_ave, sigma=this%CV_stem_length_sig) &
         )
      call this%stem(1)%move( &
         x=-this%stem(1)%femdomain%xmax()/2.0d0, &
         y=-this%stem(1)%femdomain%ymax()/2.0d0, &
         z=0.0d0 &
         )

      call this%stem(1)%rotate( &
         z=radian(360.0d0*random%random()) &
         )

      ! end of primary growth
      this%stem(1)%already_grown = .true.

      leaf_z_angles(1) = random%random()*360.0d0
      leaf_z_angles(2) = leaf_z_angles(1) + 180.0d0
      leaf_z_angles(3) = random%random()*360.0d0
      leaf_z_angles(4) = leaf_z_angles(3) + 180.0d0
      this%num_leaf = 0

      do i = 1, 2
         this%num_leaf = this%num_leaf + 1
         call this%leaf(i)%init(species=PF_SOYBEAN_CV) ! soybean

         y_val = random%gauss(mu=this%CV_leaf_thickness_ave, sigma=this%CV_leaf_thickness_sig)
         z_val = random%gauss(mu=this%CV_leaf_length_ave, sigma=this%CV_leaf_length_sig)
         x_val = random%gauss(mu=this%CV_leaf_width_ave, sigma=this%CV_leaf_width_sig)

         this%leaf(i)%already_grown = .true.

         call this%leaf(i)%resize( &
            y=y_val, &
            z=z_val, &
            x=x_val &
            )
         call this%leaf(i)%move( &
            y=-y_val/2.0d0, &
            z=-z_val/2.0d0, &
            x=-x_val/2.0d0 &
            )
         call this%leaf(i)%rotate( &
            x=radian(90.0d0), &
            y=0.0d0, &
            z=radian(leaf_z_angles(i)), reset=.true. &
            )
         call this%leaf(i)%connect("=>", this%stem(1))
         this%leaf2stem(i, 1) = 1
      end do
      call this%update()

      ! create stage VC
      if (allocated(this%NodeID_Branch)) then
         deallocate (this%NodeID_Branch)
      end if
      this%NodeID_MainStem = this%NodeID_MainStem//[2]

      call this%stem(2)%init()
      this%stem(2)%stemID = 0
      this%stem(2)%InterNodeID = 1
      this%stem(2)%already_grown = .true.

      call this%stem(2)%resize( &
         x=random%gauss(mu=this%VC_stem_width_ave, sigma=this%VC_stem_width_sig), &
         y=random%gauss(mu=this%VC_stem_width_ave, sigma=this%VC_stem_width_sig), &
         z=random%gauss(mu=this%VC_stem_length_ave, sigma=this%VC_stem_length_sig) &
         )
      call this%stem(2)%move( &
         x=-this%stem(2)%femdomain%xmax()/2.0d0, &
         y=-this%stem(2)%femdomain%ymax()/2.0d0, &
         z=0.0d0 &
         )

      call this%stem(2)%rotate( &
         z=radian(360.0d0*random%random()) &
         )

      ! end of primary growth
      this%stem(2)%already_grown = .true.

      this%stem2stem(2, 1) = PF_SOYBEAN_MAINSTEM_TO_MAINSTEM

      do i = 3, 4
         this%num_leaf = this%num_leaf + 1
         call this%leaf(i)%init(SoyWidthRatio=0.50d0)

         y_val = random%gauss(mu=this%VC_leaf_thickness_ave, sigma=this%VC_leaf_thickness_sig)
         z_val = random%gauss(mu=this%VC_leaf_length_ave, sigma=this%VC_leaf_length_sig)
         x_val = random%gauss(mu=this%VC_leaf_width_ave, sigma=this%VC_leaf_width_sig)

         this%leaf(i)%already_grown = .true.

         call this%leaf(i)%resize( &
            y=y_val, &
            z=z_val, &
            x=x_val &
            )
         call this%leaf(i)%move( &
            y=-y_val/2.0d0, &
            z=-z_val/2.0d0, &
            x=-x_val/2.0d0 &
            )

         call this%leaf(i)%rotate( &
            x=radian(90.0d0), &
            y=0.0d0, &
            z=radian(leaf_z_angles(i)), reset=.true. &
            )
         call this%leaf(i)%connect("=>", this%stem(2))
         this%leaf2stem(i, 2) = 1
      end do

      call this%update()
      ! no root

      this%stem2stem(2, 1) = PF_SOYBEAN_MAINSTEM_TO_MAINSTEM

   end subroutine

! ########################################
   recursive subroutine updateSoybean(this, stem_id, root_id, leaf_id, overset_margin, debug)
      class(Soybean_), intent(inout) :: this
      integer(int32), optional, intent(in) :: stem_id, root_id, leaf_id
      real(real64), optional, intent(in) :: overset_margin
      integer(int32) :: i, j, this_stem_id, next_stem_id, A_id, B_id, itr_tol, itr, k, kk,meristem_id
      integer(int32) :: this_leaf_id, next_leaf_id
      integer(int32) :: this_root_id, next_root_id, InterNodeID, PetioleID, StemID, LeafID
      real(real64) :: x_A(3), x_B(3), diff(3), error, last_error, mgn, overset_m, error_tol,original_position(1:3),disp(1:3)
      logical, optional, intent(in) :: debug

      original_position = this%stem(1)%femdomain%mesh%nodcoord(1,3)

      if (this%default_Leaf_growth_ratio > 0.0d0) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) cycle
            this%leaf(i)%length_growth_ratio = this%default_Leaf_growth_ratio
            this%leaf(i)%Width_growth_ratio = this%default_Leaf_growth_ratio
         end do
      end if

      if (this%default_stem_growth_ratio > 0.0d0) then
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) cycle
            this%stem(i)%length_growth_ratio = this%default_stem_growth_ratio
            this%stem(i)%Width_growth_ratio = this%default_stem_growth_ratio
         end do
      end if

      ! if soybean_internode_info_ is active
      ! update parameters
      if (allocated(this%InterNodeInfo)) then
         do i = 0, this%MaxBranchNum

            if (allocated(this%InterNodeInfo(i)%FinalInterNodeLength)) then
               do j = 1, this%maxInterNodeID(StemID=i)
                  InterNodeID = this%searchStem(StemID=i, InterNodeID=j)
                  if (size(this%InterNodeInfo(i)%FinalInterNodeLength) < j) then
                     print *, "ERROR :: updateSoybean >> "
                     print *, "size(this%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
                     stop
                  end if
                  if (InterNodeID < 1) then
                     cycle
                  end if
                  this%stem(InterNodeID)%final_length = this%InterNodeInfo(i)%FinalInterNodeLength(j)
               end do
            end if

            if (allocated(this%InterNodeInfo(i)%FinalPetioleLength)) then
               do j = 1, this%maxInterNodeID(StemID=i)
                  do k = 1, this%maxPetioleID(StemID=i, InterNodeID=j)
                     if (size(this%InterNodeInfo(i)%FinalPetioleLength) < j) then
                        print *, "ERROR :: updateSoybean >> "
                        print *, "size(this%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
                        stop
                     end if

                     PetioleID = this%searchPetiole(StemID=i, InterNodeID=j, PetioleID=k)

                     this%stem(PetioleID)%final_length = this%InterNodeInfo(i)%FinalPetioleLength(j)
                  end do
               end do
            end if

            if (allocated(this%InterNodeInfo(i)%FinalLeafLength)) then
               do j = 1, this%maxInterNodeID(StemID=i)
                  do k = 1, this%maxPetioleID(StemID=i, InterNodeID=j)
                     do kk = 1, this%maxleafID(StemID=i, InterNodeID=j, PetioleID=k)
                        if (size(this%InterNodeInfo(i)%FinalLeafLength) < j) then
                           print *, "ERROR :: updateSoybean >> "
                           print *, "size(this%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
                           stop
                        end if
                        LeafID = this%searchleaf(StemID=i, InterNodeID=j, PetioleID=k, LeafID=kk)
                        this%leaf(LeafID)%final_length = this%InterNodeInfo(i)%FinalLeafLength(j)
                     end do
                  end do
               end do
            end if

            if (allocated(this%InterNodeInfo(i)%FinalLeafWidth)) then
               do j = 1, this%maxInterNodeID(StemID=i)
                  do k = 1, this%maxPetioleID(StemID=i, InterNodeID=j)
                     do kk = 1, this%maxleafID(StemID=i, InterNodeID=j, PetioleID=k)
                        if (size(this%InterNodeInfo(i)%FinalLeafWidth) < j) then
                           print *, "ERROR :: updateSoybean >> "
                           print *, "size(this%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
                           stop
                        end if
                        LeafID = this%searchleaf(StemID=i, InterNodeID=j, PetioleID=k, LeafID=kk)
                        this%leaf(LeafID)%final_Width = this%InterNodeInfo(i)%FinalLeafWidth(j)
                     end do
                  end do
               end do
            end if

         end do
      end if

      ! update connectivity
      if (.not. allocated(this%stem2stem)) then
         print *, "updateSoybean >> ERROR :: .not. allocated(this%stem2stem )"
         return
      end if

      error_tol = dble(1.0e-14)

      ! margin between subdomains
      overset_m = input(default=0.03d0, option=overset_margin)

      itr_tol = 1000
      itr = 0

      ! if debug
      !if(present(debug) )then
      !    if(debug)then
      !        print *, "this%stem2stem"
      !        call print(this%stem2stem)
      !    endif
      !endif

      ! stem to stem
      last_error = 1.0d0
      if (maxval(this%stem2stem) >= 1) then

         do
            itr = itr + 1
            error = 0.0d0
            do i = 1, size(this%stem2stem, 1)
               do j = 1, size(this%stem2stem, 2)
                  this_stem_id = j
                  next_stem_id = i
                  if (this%stem2stem(i, j) >= 1 .and. i /= j) then
                     ! this_stem_id ===>>> next_stem_id, connected!

                     !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
                     !x_A(:) = this%stem(next_stem_id)%getCoordinate("A")
                     ! Overset分食い込ませる
                     x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("B") &
                              + overset_m*this%stem(this_stem_id)%getCoordinate("A")
                     ! Overset分食い込ませる
                     x_A(:) = (1.0d0 - overset_m)*this%stem(next_stem_id)%getCoordinate("A") &
                              + overset_m*this%stem(next_stem_id)%getCoordinate("B")

                     diff(:) = x_B(:) - x_A(:)

                     error = error + dot_product(diff, diff)
                     call this%stem(next_stem_id)%move(x=diff(1), y=diff(2), z=diff(3))

                  end if
               end do
            end do
            if (present(debug)) then
               if (debug) then
                  print *, "soybean % update s2s >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "soybean % update s2s >> ERROR :: not converged"
               stop
            end if

            if (abs(error) + abs(last_error) < error_tol) exit
            last_error = error
         end do
      end if

      ! root to stem
      if (allocated(this%root2stem)) then
         last_error = 1.0d0
         do
            itr = itr + 1
            error = 0.0d0
            do i = 1, size(this%root2stem, 1)
               do j = 1, size(this%root2stem, 2)
                  this_stem_id = j
                  next_root_id = i
                  if (this%root2stem(i, j) == 1) then
                     ! this_stem_id ===>>> next_root_id, connected!
                     !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
                     !x_A(:) = this%root(next_root_id)%getCoordinate("A")

                     ! Overset分食い込ませる
                     x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("A") &
                              + overset_m*this%stem(this_stem_id)%getCoordinate("B")
                     ! Overset分食い込ませる
                     x_A(:) = (1.0d0 - overset_m)*this%root(next_root_id)%getCoordinate("A") &
                              + overset_m*this%root(next_root_id)%getCoordinate("B")

                     diff(:) = x_B(:) - x_A(:)
                     error = error + dot_product(diff, diff)
                     call this%root(next_root_id)%move(x=diff(1), y=diff(2), z=diff(3))
                  end if
               end do
            end do
            if (present(debug)) then
               if (debug) then
                  print *, "soybean % update r2s >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "soybean % update r2s  >> ERROR :: not converged"
               stop
            end if

            if (abs(error) + abs(last_error) < error_tol) exit
            last_error = error
         end do
      end if

      if (allocated(this%root2root)) then
         ! root to root
         last_error = 1.0d0
         do
            itr = itr + 1
            error = 0.0d0
            do i = 1, size(this%root2root, 1)
               do j = 1, size(this%root2root, 2)
                  this_root_id = j
                  next_root_id = i
                  if (next_root_id == 1) then
                     cycle
                  end if
                  if (this%root2root(i, j) /= 0 .and. i /= j) then
                     ! this_root_id ===>>> next_root_id, connected!
                     !x_B(:) = this%root(this_root_id)%getCoordinate("B")
                     !x_A(:) = this%root(next_root_id)%getCoordinate("A")

                     ! Overset分食い込ませる
                     x_B(:) = (1.0d0 - overset_m)*this%root(this_root_id)%getCoordinate("B") &
                              + overset_m*this%root(this_root_id)%getCoordinate("A")
                     ! Overset分食い込ませる
                     x_A(:) = (1.0d0 - overset_m)*this%root(next_root_id)%getCoordinate("A") &
                              + overset_m*this%root(next_root_id)%getCoordinate("B")

                     diff(:) = x_B(:) - x_A(:)
                     error = error + dot_product(diff, diff)
                     call this%root(next_root_id)%move(x=diff(1), y=diff(2), z=diff(3))
                  end if
               end do
            end do
            if (present(debug)) then
               if (debug) then
                  print *, "soybean % update r2r >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "soybean % update r2r >> ERROR :: not converged"
               stop
            end if

            if (abs(error) + abs(last_error) < error_tol) exit
            last_error = error
         end do
      end if

      ! leaf to stem
      last_error = 1.0d0
      do
         itr = itr + 1
         error = 0.0d0
         do i = 1, size(this%leaf2stem, 1)
            do j = 1, size(this%leaf2stem, 2)
               this_stem_id = j
               next_leaf_id = i
               if (this%leaf2stem(i, j) == 1) then
                  ! this_stem_id ===>>> next_leaf_id, connected!
                  !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
                  !x_A(:) = this%leaf(next_leaf_id)%getCoordinate("A")

                  ! Overset分食い込ませる
                  x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("B") &
                           + overset_m*this%stem(this_stem_id)%getCoordinate("A")
                  ! Overset分食い込ませる
                  x_A(:) = this%leaf(next_leaf_id)%getCoordinate("A")

                  diff(:) = x_B(:) - x_A(:)
                  error = error + dot_product(diff, diff)
                  call this%leaf(next_leaf_id)%move(x=diff(1), y=diff(2), z=diff(3))
               end if
            end do
         end do
         if (present(debug)) then
            if (debug) then
               print *, "soybean % update l2s >> error :: ", error
            end if
         end if
         if (itr > itr_tol) then
            print *, "soybean % update l2s  >> ERROR :: not converged"
            stop
         end if

         if (abs(error) - abs(last_error) < error_tol) exit
         last_error = error
      end do

      
      !> connect meristem to stem
      if (allocated(this%meristem2stem) ) then
         do i = 1, size(this%meristem2stem, 1)
            do j = 1, size(this%meristem2stem, 2)
               if (this%meristem2stem(i, j) >= 1 ) then
                  ! this_stem_id ===>>> meristem_id, connected!
                  !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
                  !x_A(:) = this%stem(meristem_id)%getCoordinate("A")
                  ! Overset分食い込ませる
                  this_stem_id = j
                  meristem_id  = i

                  x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("B") &
                           + overset_m*this%stem(this_stem_id)%getCoordinate("A")
                  ! Overset分食い込ませる
                  x_A(:) = (1.0d0 - overset_m)*this%meristem(meristem_id)%stem(1)%getCoordinate("A") &
                           + overset_m*this%meristem(meristem_id)%stem(1)%getCoordinate("B")
                  diff(:) = x_B(:) - x_A(:)
                  error = error + dot_product(diff, diff)
                  call this%meristem(meristem_id)%move(x=diff(1), y=diff(2), z=diff(3))
                  call this%meristem(meristem_id)%update()
               end if
            end do
         end do
      end if


      ! offset displacement
      !if ( norm(this%stem(1)%femdomain%mesh%nodcoord(1,3) - original_position) > error_tol)then
      !   disp = this%stem(1)%femdomain%mesh%nodcoord(1,3) - original_position
      !   call this%move(x=-disp(1),y=-disp(2),z=-disp(3))
      !endif

   end subroutine
! ########################################

! ########################################
   subroutine initsoybean(this, config, &
                          regacy, mass, water_content, radius, location, x, y, z, &
                          PlantRoot_diameter_per_seed_radius, max_PlantNode_num, Variety, FileName, &
                          max_leaf_num, max_stem_num, max_root_num, profiler)
      class(Soybean_), intent(inout) :: this

      real(real64), optional, intent(in) :: mass, water_content, radius, location(3), x, y, z
      real(real64), optional, intent(in) :: PlantRoot_diameter_per_seed_radius
      character(*), optional, intent(in) :: Variety, FileName, config
      logical, optional, intent(in) :: regacy, profiler
      character(:), allocatable :: fn, conf, line
      integer(int32), optional, intent(in) :: max_PlantNode_num, max_leaf_num, max_stem_num, max_root_num
      real(real64) :: MaxThickness, Maxwidth, loc(3), vec(3), rot(3), zaxis(3), meshloc(3), meshvec(3), &
                      x_val, y_val, z_val
      integer(int32) :: i, j, k, blcount, id, rmc, n, node_id, node_id2, elemid, branch_id, num_stem_node

      real(real64)::readvalreal
      real(real64), allocatable :: leaf_z_angles(:)
      integer(int32) :: readvalint
      logical :: debug = .false.
      logical :: timeOpt = .false.
      type(IO_) :: soyconf
      type(Random_) :: random
      type(Time_) :: time
      type(Stem_) :: stem
      type(Leaf_) :: leaf
      type(Root_) :: root
      real(real64) :: seed_width,seed_length,seed_thickness
      integer(int32) :: seed_division

      this%UUID = generate_uuid(1)

      timeOpt = input(default=.false., option=profiler)


      ! IS THIS A SEED?
      if (present(config))then
         !if ("Seed" .in. keys(json_file=config)) then
         if (.not. ( "{'not found'}" .in. soyconf%parse_json(filename=config, &
            keys=to_list("Seed","Length")) ) ) then
            seed_length    = soyconf%parse_json(filename=config, keys=to_list("Seed","Length"))
            seed_width     = soyconf%parse_json(filename=config, keys=to_list("Seed","Width"))
            seed_thickness = soyconf%parse_json(filename=config, keys=to_list("Seed","Thickness"))
            seed_division  = soyconf%parse_json(filename=config, keys=to_list("Seed","Division"))
            call this%init(&
               radius=[seed_length,seed_width,seed_thickness],&
               division=[seed_division,seed_division,seed_division])
            return
         endif
      endif

      !if(.not.allocated(this%InterNodeInfo) )then
      !    allocate(this%InterNodeInfo(0:this%MaxBranchNum) )
      !    ! default value
      !    do i=0,size(this%InterNodeInfo)
      !        this%InterNodeInfo(i)%FinalInterNodeLength = linspace([0.030d0,0.060d0],30)
      !        this%InterNodeInfo(i)%FinalLeafLength      = linspace([0.050d0,0.20d0],30)
      !        this%InterNodeInfo(i)%FinalLeafWidth       = linspace([0.020d0,0.25d0],30)
      !        this%InterNodeInfo(i)%FinalPetioleLength   = linspace([0.050d0,0.25d0],30)
      !    enddo
      !endif

      if (timeOpt) then
         call time%start()
      end if

      call this%remove()
      ! set default parameters
      ! stem
      this%br_node(:) = 0
      this%br_from(:) = 0
      this%br_length(:) = 0.0d0

      this%br_angle_ave(:) = 0.0d0
      this%br_angle_sig(:) = 10.0d0
      !this%br_angle_ave(1)=10.0d0
      !this%br_angle_sig(1)=2.0d0

      this%ms_angle_ave = 0.0d0
      this%ms_angle_sig = 2.0d0

      ! for roots
      this%brr_node(:) = 0
      this%brr_from(:) = 0
      this%brr_length(:) = 0.0d0

      this%brr_angle_ave(:) = 0.0d0
      this%brr_angle_sig(:) = 10.0d0
      this%brr_angle_ave(1) = 30.0d0
      this%brr_angle_sig(1) = 2.0d0

      this%mr_angle_ave = 0.0d0
      this%mr_angle_sig = 2.0d0
      ! peti
      ! is also stem

      this%peti_size_ave(:) = 0.20d0
      this%peti_size_sig(:) = 0.010d0

      this%peti_width_ave(:) = 0.0050d0
      this%peti_width_sig(:) = 0.00010d0

      this%peti_angle_ave(:) = 30.0d0
      this%peti_angle_sig(:) = 1.00d0

      ! leaf
      this%leaf_length_ave(:) = 0.20d0
      this%leaf_length_sig(:) = 0.01d0

      this%leaf_width_ave(:) = 0.050d0
      this%leaf_width_sig(:) = 0.010d0

      this%leaf_thickness_ave(:) = 0.00100d0
      this%leaf_thickness_sig(:) = 0.00050d0

      this%leaf_angle_ave(:) = 80.0d0
      this%leaf_angle_sig(:) = 10.0d0

      if (timeOpt) then
         print *, "[1] set default values :: "
         call time%show()
      end if

      ! 子葉節、初生葉節、根の第1節まで種子の状態で存在

      ! 節を生成するためのスクリプトを開く
      if (.not. present(config) .or. index(config, ".json") == 0) then
         ! デフォルトの設定を生成
         print *, "New soybean-configuration >> soyconfig.json"
         call soyconf%open("soyconfig.json")
         write (soyconf%fh, *) '{'
         write (soyconf%fh, *) '   "type": "soybean",'
         write (soyconf%fh, *) '   "stage": 0,'
         write (soyconf%fh, *) '   "length": 0.0090,'
         write (soyconf%fh, *) '   "width" : 0.0081,'
         write (soyconf%fh, *) '   "height": 0.0072,'
         write (soyconf%fh, *) '   "MaxLeafNum": 50,'
         write (soyconf%fh, *) '   "MaxRootNum":200,'
         write (soyconf%fh, *) '   "MaxStemNum": 50,'

         ! stem
         write (soyconf%fh, *) '   "br_node" : 0,'
         write (soyconf%fh, *) '   "br_from" : 0,'
         write (soyconf%fh, *) '   "br_length" : 0.00,'
         write (soyconf%fh, *) '   "br_angle_ave" : 0.00,'
         write (soyconf%fh, *) '   "br_angle_sig" : 10.00,'
         write (soyconf%fh, *) '   "br_angle_ave(1)": 0.00,'
         write (soyconf%fh, *) '   "br_angle_sig(1)": 10.00,'
         write (soyconf%fh, *) '   "ms_angle_ave": 0.00,'
         write (soyconf%fh, *) '   "ms_angle_sig": 2.00,'

         ! root
         write (soyconf%fh, *) '   "brr_node" : 0,'
         write (soyconf%fh, *) '   "brr_from" : 0,'
         write (soyconf%fh, *) '   "brr_length" : 0.00,'
         write (soyconf%fh, *) '   "brr_angle_ave" : 0.00,'
         write (soyconf%fh, *) '   "brr_angle_sig" : 10.00,'
         write (soyconf%fh, *) '   "brr_angle_ave(1)": 360.00,'
         write (soyconf%fh, *) '   "brr_angle_sig(1)": 2.00,'
         write (soyconf%fh, *) '   "mr_angle_ave": 0.00,'
         write (soyconf%fh, *) '   "mr_angle_sig": 2.00,'
         ! peti
         ! is also stem
         write (soyconf%fh, *) '   "peti_size_ave"  :  0.200,'
         write (soyconf%fh, *) '   "peti_size_sig"  :  0.0100,'
         write (soyconf%fh, *) '   "peti_width_ave"  :  0.00500,'
         write (soyconf%fh, *) '   "peti_width_sig"  :  0.000100,'
         write (soyconf%fh, *) '   "peti_angle_ave"  :  30.00,'
         write (soyconf%fh, *) '   "peti_angle_sig"  :  1.000,'
         ! leaf
         write (soyconf%fh, *) '   "leaf_length_ave"  :  0.200,'
         write (soyconf%fh, *) '   "leaf_length_sig"  :  0.010,'
         write (soyconf%fh, *) '   "leaf_width_ave"  :  0.0500,'
         write (soyconf%fh, *) '   "leaf_width_sig"  :  0.0100,'
         write (soyconf%fh, *) '   "leaf_thickness_ave"  :  0.001000,'
         write (soyconf%fh, *) '   "leaf_thickness_sig"  :  0.000500,'
         write (soyconf%fh, *) '   "leaf_angle_ave"  :  80.00,'
         write (soyconf%fh, *) '   "leaf_angle_sig"  :  10.00'
         write (soyconf%fh, *) '}'
         conf = "soyconfig.json"
         call soyconf%close()
      else
         conf = config
      end if

      if (timeOpt) then
         print *, "[2] create default config "
         call time%show()
      end if

      line = soyconf%parse(conf, key1="Genotype", key2="Dt1")
      if (index(line, "Dt1") /= 0) then
         this%determinate = .False.
      else
         this%determinate = .True.
      end if

      call soyconf%open(conf)
      blcount = 0
      do
         read (soyconf%fh, '(a)') line
         if (debug) print *, line
         if (adjustl(line) == "{") then
            blcount = 1
            cycle
         end if
         if (adjustl(line) == "}") then
            exit
         end if

         if (blcount == 1) then

            if (index(line, "Name") /= 0) then
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%name
            end if

            if (index(line, "Mainstem") /= 0) then
               do
                  read (soyconf%fh, '(a)') line
                  if (debug) print *, line
                  if (index(line, "}") /= 0) then
                     exit
                  end if

                  if (index(line, "Length") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%ms_length
                  end if

                  if (index(line, "Width") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%ms_width
                  end if

                  if (index(line, "Node") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%ms_node
                  end if

               end do
            end if

            if (index(line, "Branch#") /= 0) then
               rmc = index(line, "{")
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               rmc = index(line, '"')
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               rmc = index(line, '"')
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               rmc = index(line, ':')
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, "#")
               if (debug) print *, line
               read (line(id + 1:), *) branch_id

               do
                  read (soyconf%fh, '(a)') line
                  if (debug) print *, line
                  if (index(line, "}") /= 0) then
                     exit
                  end if

                  if (index(line, "Length") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%br_length(branch_id)
                  end if

                  if (index(line, "Width") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%br_Width(branch_id)
                  end if

                  if (index(line, "Node") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%br_node(branch_id)
                  end if

                  if (index(line, "From") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%br_from(branch_id)
                  end if

               end do
            end if

            ! for roots

            if (index(line, "Mainroot") /= 0) then
               do
                  read (soyconf%fh, '(a)') line
                  if (debug) print *, line
                  if (index(line, "}") /= 0) then
                     exit
                  end if

                  if (index(line, "Length") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%mr_length
                  end if

                  if (index(line, "Width") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%mr_width
                  end if

                  if (index(line, "Node") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%mr_node
                  end if

               end do
            end if

            if (index(line, "Branchroot#") /= 0) then
               rmc = index(line, "{")
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               rmc = index(line, '"')
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               rmc = index(line, '"')
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               rmc = index(line, ':')
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, "#")
               if (debug) print *, line
               read (line(id + 1:), *) branch_id

               do
                  read (soyconf%fh, '(a)') line
                  if (debug) print *, line
                  if (index(line, "}") /= 0) then
                     exit
                  end if

                  if (index(line, "Length") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%brr_length(branch_id)
                  end if

                  if (index(line, "Width") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%brr_Width(branch_id)
                  end if

                  if (index(line, "Node") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%brr_node(branch_id)
                  end if

                  if (index(line, "From") /= 0) then
                     rmc = index(line, ",")
                     if (rmc /= 0) then
                        line(rmc:rmc) = " "
                     end if
                     id = index(line, ":")
                     read (line(id + 1:), *) this%brr_from(branch_id)
                  end if

               end do
            end if

            if (index(line, "rootconfig") /= 0) then
               ! 茎の設定ファイル
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%rootconfig
            end if

            if (index(line, "stemconfig") /= 0) then
               ! 茎の設定ファイル
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%stemconfig
            end if

            if (index(line, "leafconfig") /= 0) then
               ! 茎の設定ファイル
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%leafconfig
            end if

            if (index(line, "stage") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%stage_id
            end if

            if (index(line, "MaxLeafNum") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%MaxLeafNum
            end if

            if (index(line, "MaxStemNum") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%MaxStemNum
            end if

            if (index(line, "MaxRootNum") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%MaxRootNum
            end if

            if (index(line, "length") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%seed_length
            end if

            if (index(line, "width") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%seed_width
            end if

            if (index(line, "height") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) this%seed_height
            end if

            ! for version 2020.11.24

            ! stem
            if (index(line, "br_angle_ave") /= 0 .and. index(line, "br_angle_ave(") == 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%br_angle_ave(:) = readvalreal
            end if

            if (index(line, "br_angle_sig") /= 0 .and. index(line, "br_angle_sig(") == 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%br_angle_sig(:) = readvalreal
            end if

            if (index(line, "br_angle_ave(1)") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%br_angle_ave(1) = readvalreal
            end if
            if (index(line, "br_angle_sig(1)") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%br_angle_sig(1) = readvalreal
            end if

            if (index(line, "ms_angle_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%ms_angle_ave = readvalreal
            end if

            if (index(line, "ms_angle_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%ms_angle_sig = readvalreal
            end if
            ! peti
            ! is also stem

            if (index(line, "peti_size_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%peti_size_ave(:) = readvalreal
            end if

            if (index(line, "peti_size_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%peti_size_sig(:) = readvalreal
            end if

            if (index(line, "peti_width_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%peti_width_ave(:) = readvalreal
            end if

            if (index(line, "peti_width_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%peti_width_sig(:) = readvalreal
            end if

            if (index(line, "peti_angle_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%peti_angle_ave(:) = readvalreal
            end if

            if (index(line, "peti_angle_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%peti_angle_sig(:) = readvalreal
            end if
            ! leaf

            if (index(line, "leaf_length_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%leaf_length_ave(:) = readvalreal
            end if

            if (index(line, "leaf_length_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%leaf_length_sig(:) = readvalreal
            end if

            if (index(line, "leaf_width_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%leaf_width_ave(:) = readvalreal
            end if

            if (index(line, "leaf_width_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%leaf_width_sig(:) = readvalreal
            end if

            if (index(line, "leaf_thickness_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%leaf_thickness_ave(:) = readvalreal
            end if

            if (index(line, "leaf_thickness_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%leaf_thickness_sig(:) = readvalreal
            end if

            if (index(line, "leaf_angle_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%leaf_angle_ave(:) = readvalreal
            end if

            if (index(line, "leaf_angle_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%leaf_angle_sig(:) = readvalreal
            end if

            ! added in 2020/12/15
            ! for roots

            if (index(line, "brr_angle_ave") /= 0 .and. index(line, "brr_angle_ave(") == 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%brr_angle_ave(:) = readvalreal
            end if

            if (index(line, "brr_angle_sig") /= 0 .and. index(line, "brr_angle_sig(") == 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%brr_angle_sig(:) = readvalreal
            end if

            if (index(line, "brr_angle_ave(1)") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%brr_angle_ave(1) = readvalreal
            end if
            if (index(line, "brr_angle_sig(1)") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%brr_angle_sig(1) = readvalreal
            end if

            if (index(line, "mr_angle_ave") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%mr_angle_ave = readvalreal
            end if

            if (index(line, "mr_angle_sig") /= 0) then

               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) readvalreal
               this%mr_angle_sig = readvalreal
            end if

            cycle

         end if

      end do
      call soyconf%close()

      if (index(config, ".json") == 0) then
         this%stemconfig = " "
         this%rootconfig = " "
         this%leafconfig = " "
      end if

      if (timeOpt) then
         print *, "[3] read config "
         call time%show()
      end if

      if (this%ms_node /= 0) then
         ! loaded from Mainstem-Branches relation file format
         ! ex.
!       {
!           "Name":"soybean",
!           "Mainstem":{
!               "Length":1.2,
!               "Node":13
!           },
!           "Branch#1":{
!               "From":1,
!               "Length":0.6,
!               "Node":7
!           },
!           "Branch#2":{
!               "From":3,
!               "Length":0.2,
!               "Node":2
!           },
!           "Branch#3":{
!               "From":4,
!               "Length":0.2,
!               "Node":2
!           }
!       }
         ! count number of nodes
         !num_node = countif(this%ms_node,notEquai=.true.,0)
         !num_node = num_node + countif(this%br_node,notEquai=.true.,0)

         allocate (this%leaf(this%MaxLeafNum))
         allocate (this%root(this%MaxrootNum))
         allocate (this%stem(this%MaxstemNum))

         allocate (this%leafYoungModulus(this%MaxLeafNum))
         allocate (this%rootYoungModulus(this%MaxrootNum))
         allocate (this%stemYoungModulus(this%MaxstemNum))
         ! default value
         this%leafYoungModulus(:) = 1000.0d0
         this%rootYoungModulus(:) = 1000.0d0
         this%stemYoungModulus(:) = 1000.0d0

         allocate (this%leafPoissonRatio(this%MaxLeafNum))
         allocate (this%rootPoissonRatio(this%MaxrootNum))
         allocate (this%stemPoissonRatio(this%MaxstemNum))
         this%leafPoissonRatio(:) = 0.30d0
         this%rootPoissonRatio(:) = 0.30d0
         this%stemPoissonRatio(:) = 0.30d0

         allocate (this%leafDensity(this%MaxLeafNum))
         allocate (this%rootDensity(this%MaxrootNum))
         allocate (this%stemDensity(this%MaxstemNum))

         this%leafDensity(:) = 0.0d0
         this%rootDensity(:) = 0.0d0
         this%stemDensity(:) = 0.0d0

         allocate (this%stem2stem(this%MaxstemNum, this%MaxstemNum))
         allocate (this%leaf2stem(this%MaxstemNum, this%MaxLeafNum))
         allocate (this%root2stem(this%MaxrootNum, this%MaxstemNum))
         allocate (this%root2root(this%MaxrootNum, this%MaxrootNum))
         this%stem2stem(:, :) = 0
         this%leaf2stem(:, :) = 0
         this%root2stem(:, :) = 0
         this%root2root(:, :) = 0

         ! set mainstem

         allocate (this%NodeID_MainStem(this%ms_node))

         if (index(this%stemconfig, ".json") == 0) then
            call stem%init( &
               x_num=this%stem_division(1), &
               y_num=this%stem_division(2), &
               z_num=this%stem_division(3) &
               )
         else
            call stem%init(config=this%stemconfig)
         end if

         do i = 1, this%ms_node

            !call this%stem(i)%init(config=this%stemconfig)

            this%stem(i) = stem

            this%stem(i)%stemID = 0
            this%stem(i)%InterNodeID = i
            this%stem(i)%already_grown = .true.

            this%NodeID_MainStem(i) = i
            call this%stem(i)%resize( &
               x=this%ms_width, &
               y=this%ms_width, &
               z=this%ms_length/dble(this%ms_node) &
               )
            call this%stem(i)%move( &
               x=-this%ms_width/2.0d0, &
               y=-this%ms_width/2.0d0, &
               z=-this%ms_length/dble(this%ms_node)/2.0d0 &
               )

            call this%stem(i)%rotate( &
               x=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)), &
               y=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)), &
               z=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)) &
               )

         end do

         if (timeOpt) then
            print *, "[4] created Main stem."
            call time%show()
         end if

         do i = 1, this%ms_node - 1
            call this%stem(i + 1)%connect("=>", this%stem(i))
            this%stem2stem(i + 1, i) = PF_SOYBEAN_MAINSTEM_TO_MAINSTEM
         end do

         ! set branches
         k = this%ms_node
         allocate (this%NodeID_Branch(size(this%br_node)))
         do i = 1, size(this%br_node) ! num branch
            allocate (this%NodeID_Branch(i)%ID(this%br_node(i)))
            do j = 1, this%br_node(i)

               k = k + 1
               !call this%stem(k)%init(config=this%stemconfig)
               this%stem(k) = stem
               this%stem(k)%stemID = i
               this%stem(k)%InterNodeID = j
               this%stem(k)%already_grown = .true.

               this%NodeID_Branch(i)%ID(j) = k

               call this%stem(k)%resize( &
                  x=this%br_width(i), &
                  y=this%br_width(i), &
                  z=this%br_length(i)/dble(this%br_node(i)) &
                  )

               call this%stem(k)%move( &
                  x=-this%br_width(i)/2.0d0, &
                  y=-this%br_width(i)/2.0d0, &
                  z=-this%br_length(i)/dble(this%br_node(i))/2.0d0 &
                  )
               call this%stem(k)%rotate( &
                  x=radian(random%gauss(mu=this%br_angle_ave(j), sigma=this%br_angle_sig(j))), &
                  y=0.0d0, &
                  z=radian(360.0d0*random%random()) &
                  )

               if (j == 1) then
                  call this%stem(k)%connect("=>", this%stem(this%br_from(i)))
                  this%stem2stem(k, this%br_from(i)) = PF_SOYBEAN_BRANCH_TO_MAINSTEM
               else
                  call this%stem(k)%connect("=>", this%stem(k - 1))
                  this%stem2stem(k, k - 1) = PF_SOYBEAN_BRANCH_TO_BRANCH
               end if

            end do
         end do

         if (timeOpt) then
            print *, "[4] created Branches."
            call time%show()
         end if

         ! peti and leaf
         this%num_stem_node = k
         this%num_leaf = 0
         ! bugfix 2021/08/18
         !call leaf%init(config=this%leafconfig,species=PF_GLYCINE_SOJA)

         if (index(this%leafconfig, ".json") == 0) then
            call leaf%init(species=PF_GLYCINE_SOJA, &
                           x_num=this%leaf_division(1), &
                           y_num=this%leaf_division(2), &
                           z_num=this%leaf_division(3) &
                           )
         else
            call leaf%init(config=this%leafconfig, species=PF_GLYCINE_SOJA)
         end if

         if (.not. stem%empty()) then
            call stem%remove()
         end if

         if (index(this%stemconfig, ".json") == 0) then

            call stem%init( &
               x_num=this%peti_division(1), &
               y_num=this%peti_division(2), &
               z_num=this%peti_division(3) &
               )
         else
            call stem%init(config=this%stemconfig)
         end if

         do i = 1, k
            ! ３複葉
            ! add peti
            this%num_stem_node = this%num_stem_node + 1
            !call this%stem(this%num_stem_node)%init(config=this%stemconfig)
            this%stem(this%num_stem_node) = stem
            this%stem(this%num_stem_node)%already_grown = .true.

            call this%stem(this%num_stem_node)%resize( &
               x=random%gauss(mu=this%peti_width_ave(i), sigma=this%peti_width_sig(i)), &
               y=random%gauss(mu=this%peti_width_ave(i), sigma=this%peti_width_sig(i)), &
               z=random%gauss(mu=this%peti_size_ave(i), sigma=this%peti_size_sig(i)) &
               )
            call this%stem(this%num_stem_node)%rotate( &
               x=radian(random%gauss(mu=this%peti_angle_ave(i), sigma=this%peti_angle_sig(i))), &
               y=0.0d0, &
               z=radian(360.0d0*random%random()) &
               )
            call this%stem(this%num_stem_node)%connect("=>", this%stem(i))
            !this%leaf2stem(num_stem_node,i) = 1
            this%stem2stem(this%num_stem_node, i) = PF_SOYBEAN_PETIOLE_TO_MAINSTEM

            ! add leaves

            leaf_z_angles = linspace([0.0d0, 360.0d0], this%max_num_leaf_per_petiole + 1)
            do j = 1, this%max_num_leaf_per_petiole
               leaf_z_angles(j) = radian(leaf_z_angles(j))
            end do

            leaf_z_angles(:) = leaf_z_angles(:) + radian(random%random()*360.0d0)

            do j = 1, this%max_num_leaf_per_petiole
               this%num_leaf = this%num_leaf + 1
               !call this%leaf(this%num_leaf)%init(config=this%leafconfig,species=PF_GLYCINE_SOJA)
               this%leaf(this%num_leaf) = leaf
               this%leaf(this%num_leaf)%LeafID = j

               y_val = random%gauss(mu=this%leaf_thickness_ave(i), sigma=this%leaf_thickness_sig(i))
               z_val = random%gauss(mu=this%leaf_length_ave(i), sigma=this%leaf_length_sig(i))
               x_val = random%gauss(mu=this%leaf_width_ave(i), sigma=this%leaf_width_sig(i))

               this%leaf(this%num_leaf)%already_grown = .true.

               call this%leaf(this%num_leaf)%resize( &
                  y=y_val, &
                  z=z_val, &
                  x=x_val &
                  )
               call this%leaf(this%num_leaf)%move( &
                  y=-y_val/2.0d0, &
                  z=-z_val/2.0d0, &
                  x=-x_val/2.0d0 &
                  )

               call this%leaf(this%num_leaf)%rotate( &
                  x=radian(random%gauss(mu=this%leaf_angle_ave(i), sigma=this%leaf_angle_sig(i))), &
                  y=0.0d0, &
                  z=leaf_z_angles(j) &
                  )
               call this%leaf(this%num_leaf)%connect("=>", this%stem(this%num_stem_node))
               this%leaf2stem(this%num_leaf, this%num_stem_node) = 1
            end do

         end do

         if (timeOpt) then
            print *, "[4] created Peti and Leaves."
            call time%show()
         end if

         ! set mainroot
         !call root%init(this%rootconfig)

         if (index(this%rootconfig, ".json") == 0) then
            call root%init( &
               x_num=this%root_division(1), &
               y_num=this%root_division(2), &
               z_num=this%root_division(3) &
               )
         else
            call root%init(config=this%rootconfig)
         end if

         do i = 1, this%mr_node

            this%root(i) = root
            this%root(i)%already_grown = .true.

            call this%root(i)%resize( &
               x=this%mr_width, &
               y=this%mr_width, &
               z=this%mr_length/dble(this%mr_node) &
               )
            call this%root(i)%move( &
               x=-this%mr_width/2.0d0, &
               y=-this%mr_width/2.0d0, &
               z=-this%mr_length/dble(this%mr_node)/2.0d0 &
               )
            call this%root(i)%rotate( &
               x=radian(random%gauss(mu=this%mr_angle_ave, sigma=this%mr_angle_sig)), &
               y=radian(random%gauss(mu=this%mr_angle_ave, sigma=this%mr_angle_sig)), &
               z=radian(random%gauss(mu=this%mr_angle_ave, sigma=this%mr_angle_sig)) &
               )
         end do

         do i = 1, this%mr_node - 1
            if (i == 1) then
               call this%root(1)%connect("=>", this%stem(1))
               this%root2stem(1, 1) = 1
            end if
            call this%root(i + 1)%connect("=>", this%root(i))
            this%root2root(i + 1, i) = 1
         end do

         ! set branches
         k = this%mr_node
         do i = 1, size(this%brr_node)
            do j = 1, this%brr_node(i)
               k = k + 1
               !call this%root(k)%init(config=this%rootconfig)
               this%root(k) = root
               this%root(k)%already_grown = .true.

               call this%root(k)%resize( &
                  x=this%mr_width, &
                  y=this%mr_width, &
                  z=this%mr_length/dble(this%mr_node) &
                  )
               call this%root(k)%move( &
                  x=-this%mr_width/2.0d0, &
                  y=-this%mr_width/2.0d0, &
                  z=-this%mr_length/dble(this%mr_node)/2.0d0 &
                  )
               call this%root(k)%rotate( &
                  x=radian(random%gauss(mu=this%brr_angle_ave(j), sigma=this%brr_angle_sig(j))), &
                  y=0.0d0, &
                  z=radian(360.0d0*random%random()) &
                  )

               if (j == 1) then
                  call this%root(k)%connect("=>", this%root(this%brr_from(i)))
                  this%root2root(k, this%brr_from(i)) = 1
               else
                  call this%root(k)%connect("=>", this%root(k - 1))
                  this%root2root(k, k - 1) = 1
               end if

            end do
         end do

         this%stage = "V"//str(this%ms_node)

         call this%update()

         call this%fixReversedElements()

         if (timeOpt) then
            print *, "[4] create objects."
            call time%show()
         end if
         return
      else
         ! create leaf, root, stem
         allocate (this%leaf(this%MaxLeafNum))
         allocate (this%root(this%MaxrootNum))
         allocate (this%stem(this%MaxstemNum))

         allocate (this%leafYoungModulus(this%MaxLeafNum))
         allocate (this%rootYoungModulus(this%MaxrootNum))
         allocate (this%stemYoungModulus(this%MaxstemNum))
         ! default value
         this%leafYoungModulus(:) = 1000.0d0
         this%rootYoungModulus(:) = 1000.0d0
         this%stemYoungModulus(:) = 1000.0d0

         allocate (this%leafPoissonRatio(this%MaxLeafNum))
         allocate (this%rootPoissonRatio(this%MaxrootNum))
         allocate (this%stemPoissonRatio(this%MaxstemNum))
         this%leafPoissonRatio(:) = 0.30d0
         this%rootPoissonRatio(:) = 0.30d0
         this%stemPoissonRatio(:) = 0.30d0

         allocate (this%leafDensity(this%MaxLeafNum))
         allocate (this%rootDensity(this%MaxrootNum))
         allocate (this%stemDensity(this%MaxstemNum))

         this%leafDensity(:) = 0.0d0
         this%rootDensity(:) = 0.0d0
         this%stemDensity(:) = 0.0d0

         allocate (this%stem2stem(this%MaxstemNum, this%MaxstemNum))
         allocate (this%leaf2stem(this%MaxstemNum, this%MaxLeafNum))
         allocate (this%root2stem(this%MaxrootNum, this%MaxstemNum))
         allocate (this%root2root(this%MaxrootNum, this%MaxrootNum))

         !allocate(this%struct%NodCoord(4,3) )
         !allocate(this%struct%ElemNod(3,2) )
         !allocate(this%struct%ElemMat(3) )
         ! 子葉結節部=(0,0,0)
         !this%struct%NodCoord(1,1:3) = 0.0d0
         call this%leaf(1)%init(this%leafconfig, species=PF_GLYCINE_SOJA)
         call this%leaf(1)%rotate(x=radian(90.0d0), y=radian(90.0d0), z=radian(10.0d0))
         this%leaf(1)%already_grown = .true.

         call this%leaf(2)%init(this%leafconfig, species=PF_GLYCINE_SOJA)
         call this%leaf(2)%rotate(x=radian(90.0d0), y=radian(90.0d0), z=radian(-10.0d0))
         this%leaf(2)%already_grown = .true.

         call this%stem(1)%init(this%stemconfig)
         call this%stem(1)%rotate(x=radian(40.0d0))
         this%stem(1)%already_grown = .true.

         call this%stem(2)%init(this%stemconfig)
         call this%stem(2)%rotate(x=radian(80.0d0))
         this%stem(2)%already_grown = .true.

         call this%root(1)%init(this%rootconfig)
         call this%root(1)%fix(x=0.0d0, y=0.0d0, z=0.0d0)
         call this%root(1)%rotate(x=radian(-60.0d0))
         this%root(1)%already_grown = .true.

         call this%leaf(1)%connect("=>", this%stem(1))
         this%leaf2stem(1, 1) = 1

         call this%leaf(2)%connect("=>", this%stem(1))
         this%leaf2stem(2, 1) = 1

         call this%stem(2)%connect("=>", this%stem(1))
         this%stem2stem(2, 1) = PF_SOYBEAN_MAINSTEM_TO_MAINSTEM

         call this%root(1)%connect("=>", this%stem(1))
         this%root2stem(1, 1) = 1

         this%stage = "VE"
         ! 初生葉結節部
         !this%struct%NodCoord(2,1) = 0.0d0
         !this%struct%NodCoord(2,2) = 0.0d0
         !this%struct%NodCoord(2,3) = 1.0d0/20.0d0*this%seed_height
         ! 地際部
         !this%struct%NodCoord(3,1) = 1.0d0/4.0d0*this%seed_length
         !this%struct%NodCoord(3,2) = 0.0d0
         !this%struct%NodCoord(3,3) = -1.0d0/3.0d0*this%seed_height
         ! 根冠
         !this%struct%NodCoord(4,1) = 1.0d0/2.0d0*this%seed_length
         !this%struct%NodCoord(4,2) = 0.0d0
         !this%struct%NodCoord(4,3) = -1.0d0/2.0d0*this%seed_height

         ! 子葉-初生葉節
         !this%struct%ElemNod(1,1) = 1
         !this%struct%ElemNod(1,2) = 2
         ! 地際-子葉節
         !this%struct%ElemNod(2,1) = 3
         !this%struct%ElemNod(2,2) = 1
         ! 地際-根冠節
         !this%struct%ElemNod(3,1) = 3
         !this%struct%ElemNod(3,2) = 4

         ! 子葉-初生葉節 stem: 1
         !this%struct%ElemMat(1) = 1
         ! 地際-子葉節 stem: 1
         !this%struct%ElemMat(2) = 1
         ! 地際-根冠節 primary root: -1
         !this%struct%ElemMat(3) = -1

         ! FEメッシュを生成
         ! 領域を確保
         !    n = input(default=80,option=max_leaf_num)
         !    allocate(this%leaf_list(n) )
         !    n = input(default=80,option=max_stem_num)
         !    allocate(this%stem_list(n) )
         !    n = input(default=80,option=max_root_num)
         !    allocate(this%root_list(n) )
         !
         !    ! 子葉のメッシュを生成
         !    call this%leaf_list(1)%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
         !        x_len=this%seed_length,y_len=this%seed_width,z_len=this%seed_height)
         !    call this%leaf_list(1)%move(x=0.0d0,y=-0.50d0*this%seed_width,z=-0.50d0*this%seed_height)
         !
         !    call this%leaf_list(2)%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
         !        x_len=this%seed_length,y_len=this%seed_width,z_len=this%seed_height)
         !    call this%leaf_list(2)%rotate(x=radian(180.0d0) )
         !    call this%leaf_list(2)%move(x=0.0d0,y=-0.50d0*this%seed_width,z=-0.50d0*this%seed_height)
         !
         !
         !
         !    ! 子葉-初生葉節のメッシュを生成
         !    rot(:) = 0.0d0
         !    call this%stem_list(1)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
         !        x_len=this%seed_width/6.0d0,y_len=this%seed_width/6.0d0,z_len=this%seed_length/4.0d0)
         !    ! 節基部の節点ID
         !    node_id = this%struct%ElemNod(1,1)
         !    ! 節先端部の節点ID
         !    node_id2= this%struct%ElemNod(1,2)
         !    ! 節基部の位置ベクトル
         !    loc(:) = this%struct%NodCoord( node_id  ,:)
         !    ! 節先端部までの方向ベクトル
         !    vec(:) =  this%struct%NodCoord( node_id2 ,:) - this%struct%NodCoord( node_id  ,:)
         !
         !    ! structの構造データにメッシュデータを合わせる。
         !    print *, this%stem_list(1)%Mesh%BottomElemID
         !    print *, this%stem_list(1)%Mesh%TopElemID
         !
         !    elemid = this%stem_list(1)%Mesh%BottomElemID
         !    node_id = this%stem_list(1)%Mesh%ElemNod(elemID,1)
         !    meshloc(:) = this%stem_list(1)%Mesh%NodCoord(node_id,:)
         !
         !    elemid = this%stem_list(1)%Mesh%TopElemID
         !    node_id = this%stem_list(1)%Mesh%ElemNod(elemID,1)
         !    meshvec(:) = this%stem_list(1)%Mesh%NodCoord(node_id,:)-meshloc(:)

         !print *, "loc",loc
         !print *, "meshloc",meshloc
         !print *, "vec",vec
         !print *, "meshvec",meshvec

         !    ! 節中央を原点へ
         !    call this%stem_list(1)%move(x=-this%seed_width/12.0d0,y=-this%seed_width/12.0d0)
         !
         !    print *, "loc",loc
         !    print *, "vec",vec
         !    print *, "rot",rot
         !    zaxis(:)=0.0d0
         !    zaxis(3)=this%seed_length/5.0d0
         !    rot(:) = angles(zaxis,vec)
         !    call this%stem_list(1)%move(x=loc(1),y=loc(2),z=loc(3) )
         !    call this%stem_list(1)%rotate(x=0.0d0,y=0.0d0,z=0.0d0 )
    !!
         !
    !!
         !
         !
         !    ! 地際-子葉節のメッシュを生成
         !    rot(:) = 0.0d0
         !    call this%stem_list(2)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
         !        x_len=this%seed_width/6.0d0,y_len=this%seed_width/6.0d0,z_len=this%seed_length/4.0d0)
         !    ! 節基部の節点ID
         !    node_id = this%struct%ElemNod(2,1)
         !    ! 節先端部の節点ID
         !    node_id2= this%struct%ElemNod(2,2)
         !    ! 節基部の位置ベクトル
         !    loc(:) = this%struct%NodCoord( node_id  ,:)
         !    ! 節先端部までの方向ベクトル
         !    vec(:) =  this%struct%NodCoord( node_id2 ,:) - this%struct%NodCoord( node_id  ,:)
         !    ! 節中央を原点へ
         !    call this%stem_list(2)%move(x=-this%seed_width/12.0d0,y=-this%seed_width/12.0d0,&
         !        z=-this%seed_length/8.0d0)
         !    zaxis(:)=0.0d0
         !    zaxis(3)=this%seed_length/5.0d0
         !    rot(:) = angles(zaxis,vec)
         !    print *, "loc",loc
         !    print *, "vec",vec
         !    print *, "rot",rot
         !    !call this%stem_list(2)%rotate(x=rot(1),y=rot(2),z=rot(3) )
         !    call this%stem_list(2)%move(x=loc(1),y=loc(2),z=loc(3) )
         !
         !
         !
         !    ! 地際-根冠節のメッシュ生成
         !    rot(:) = 0.0d0
         !    call this%root_list(1)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
         !        x_len=this%seed_width/6.0d0,y_len=this%seed_width/6.0d0,z_len=this%seed_length/4.0d0)
         !    ! 節基部の節点ID
         !    node_id = this%struct%ElemNod(3,1)
         !    ! 節先端部の節点ID
         !    node_id2= this%struct%ElemNod(3,2)
         !    ! 節基部の位置ベクトル
         !    loc(:) = this%struct%NodCoord( node_id  ,:)
         !    ! 節先端部までの方向ベクトル
         !    vec(:) =  this%struct%NodCoord( node_id2 ,:) - this%struct%NodCoord( node_id  ,:)
         !    ! 節基部へ移動
         !    call this%root_list(1)%move(x=-this%seed_width/12.0d0,y=-this%seed_width/12.0d0,&
         !        z=-this%seed_length/8.0d0)
         !    call this%root_list(1)%move(x=loc(1),y=loc(2),z=loc(3) )
         !    zaxis(:)=0.0d0
         !    zaxis(3)=this%seed_length/5.0d0
         !    rot(:) = angles(zaxis,vec)
         !    !call this%root_list(1)%rotate(x=rot(1),y=rot(2),z=rot(3) )
         !    print *, "loc",loc
         !    print *, "vec",vec
         !    print *, "rot",rot
         call this%update()
         call this%fixReversedElements()

      end if

      ! ここからレガシーモード
      if (present(regacy)) then
         if (regacy .eqv. .true.) then
            this%Stage = "VE"
            if (present(FileName)) then
               fn = FileName
            else
               fn = "untitled"
            end if

            loc(:) = 0.0d0

            if (present(x)) then
               loc(1) = x
            end if

            if (present(y)) then
               loc(2) = y
            end if

            if (present(z)) then
               loc(3) = z
            end if

            if (present(location)) then
               loc(:) = location(:)
            end if

            ! initialize RootSystem and NodeSystem
            if (.not. allocated(this%RootSystem)) then
               allocate (this%RootSystem(input(default=1000, option=max_PlantNode_num)))
               this%num_of_root = 1
            end if
            if (.not. allocated(this%NodeSystem)) then
               allocate (this%NodeSystem(input(default=1000, option=max_PlantNode_num)))
               this%num_of_node = 1
            end if

            ! setup seed
            if (Variety == "Tachinagaha" .or. Variety == "tachinagaha") then
               call this%Seed%init(mass=mass, width1=9.70d0, width2=8.20d0, &
                                  width3=7.70d0, &
                                  water_content=water_content, radius=radius, location=loc)
               call this%Seed%createMesh(FileName=fn//".stl", &
                                        ElemType="Tetrahedra")

               call this%Seed%convertMeshType(Option="TetraToHexa")

            else
               print *, "Variety name :: is not implemented."
               stop
            end if

            ! setup primary node (plumule)
            call this%NodeSystem(1)%init(Stage=this%Stage, &
                                        Plantname="soybean", location=loc)

            ! setup primary node (radicle))
            MaxThickness = input(default=0.20d0, &
                                 option=PlantRoot_diameter_per_seed_radius)*this%Seed%radius
            Maxwidth = input(default=0.20d0, &
                             option=PlantRoot_diameter_per_seed_radius)*this%Seed%radius
            call this%RootSystem(1)%init(Plantname="soybean", &
                                        Stage=this%Stage, MaxThickness=MaxThickness, Maxwidth=Maxwidth, location=loc)

            this%time = 0.0d0
            call this%update()
            call this%fixReversedElements()

            return
         end if
      end if

   end subroutine
! ########################################

! ########################################
   subroutine growSoybean(this, dt, light, air, temp, simple, add_apical)
      class(Soybean_), intent(inout) :: this
      type(Light_), optional, intent(inout) :: light
      type(air_), optional, intent(in) :: air
      real(real64), optional, intent(in) :: temp
      real(real64), intent(in) :: dt! time-interval
      real(real64) :: ac_temp ! time-interval
      logical, optional, intent(in) :: add_apical
      integer(int32) :: i, j
      logical, optional, intent(in) :: simple
      integer(int32), allocatable :: apicals(:)
      integer(int32), allocatable :: last_apicals(:)
      integer(int32), allocatable :: last_last_apicals(:)
      integer(int32), allocatable :: has_branch(:)
      integer(int32) :: StemID, InterNodeID, PetioleID, LeafID, N_StemID
      logical :: add_node = .false.
      real(real64) :: count_dist

      this%dt = dt
      call this%update()

      if (present(simple)) then
         if (simple) then
            ! simple algorithmic growth
            ! growth by temp by time

            do i = 1, size(this%stem)
               call this%stem(i)%change_length_or_width(dt=dt)
            end do
            call this%update()
            do i = 1, size(this%leaf)
               call this%leaf(i)%change_length_or_width(dt=dt)
            end do
            call this%update()

            if (present(add_apical)) then
               if (add_apical) then
                  apicals = this%findApical()
                  do i = 1, size(apicals)
                     ! add stem&leaf
                     if (i == 1) then
                        ! main stem
                        StemID = apicals(i)
                        add_node = .false.

                        j = size(this%NodeID_MainStem)
                        if (j >= 1) then
                           N_StemID = this%NodeID_MainStem(j)

                           if (N_StemID >= 1) then

                              if (this%stem(N_StemID)%FullyExpanded(threshold=this%FullyExpanded_stem_threshold)) then

                                 add_node = .true.

                              end if
                           end if
                        end if
                     else
                        ! branch to
                        StemID = apicals(i)
                        add_node = .false.

                        N_StemID = maxval(this%NodeID_Branch(i - 1)%ID)
                        ! 1個前の節ID

                        if (this%stem(N_StemID)%FullyExpanded(threshold=this%FullyExpanded_stem_threshold)) then
                           add_node = .true.
                        end if

                     end if

                     if (add_node) then
                        call this%addNode(StemNodeID=apicals(i), mainstem_to_branch=.false.)
                     end if
                     call this%update()
                  end do

                  ! branch
                  has_branch = zeros(size(this%NodeID_MainStem))
                  if (allocated(this%MainStem_num_branch)) then
                     has_branch(1:size(this%MainStem_num_branch)) = this%MainStem_num_branch(:)
                  end if
                  this%MainStem_num_branch = has_branch

                  ! we introduced an apploximation of the apical dominance.
                  do i = 1, size(this%NodeID_MainStem) - 1
                     if (this%MainStem_num_branch(i) >= 1) then
                        cycle
                     else
                        count_dist = 0.0d0
                        do j = i + 1, size(this%NodeID_MainStem)
                           count_dist = count_dist + this%stem(this%NodeID_MainStem(j))%getLength()
                        end do
                        if (count_dist > this%apical_dominance_distance) then
                           ! add Node
                           if (this%stem(i)%StemID /= 0) cycle
                           !debug
                           call this%addNode(StemNodeID=i, mainstem_to_branch=.true.)
                           !has_branch(i) = has_branch(i) + 1
                           this%MainStem_num_branch(i) = this%MainStem_num_branch(i) + 1
                           call this%update()

                        end if

                     end if
                  end do

               end if
            end if

!            if(present(add_apical) )then
!                if(add_apical)then
!                    apicals = this%findApical()
!                    do i=1,size(apicals)-1
!                        ! add stem&leaf
!                        StemID = apicals(i+1)
!                        add_node = .false.
!
!                        do j=size(this%NodeID_Branch(i)%ID)-4,size(this%NodeID_Branch(i)%ID)-1
!
!                            if(j < 0 )then
!                                cycle
!                            else
!                                N_StemID = this%NodeID_Branch(i)%ID(j)
!                                LeafID = this%searchLeaf(StemID=i,InterNodeID=j,PetioleID=1,LeafID=1)
!
!                                if(LeafID < 1)then
!                                    cycle
!                                endif
!
!                                if( this%leaf(LeafID)%FullyExpanded(threshold=0.90d0 ))then
!
!                                    add_node = .true.
!                                    exit
!                                endif
!
!                            endif
!                        enddo
!                        if(add_node)then
!                            call this%addNode(StemNodeID=apicals(i))
!                        endif
!                    enddo
!                endif
!            endif

            call this%update()
            return
         end if
      end if

      ! 光量子量を計算
      call this%laytracing(light=light)

      ! 光合成量を計算
      do i = 1, size(this%Leaf)
         if (this%Leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%leaf(i)%photosynthesis(dt=dt, air=air)
         end if
      end do

      ! シンクソース輸送を計算
      !call this%SinkSourceFlow()

      ! ソースの消耗、拡散を計算
      !call this%source2sink()

      ! 伸長を計算
      !call this%extention()

      ! 分化を計算、構造の更新
      !call this%development()

      !限界日長以下>> 花成 & 子実成長
      !if( this%DayLengthIsShort() .eqv. .true. )then
      !call soybean%updateFlowers()
      !call soybean%updatePods()
      !endif

   end subroutine
! ########################################

! ########################################
   subroutine SinkSourceFlowSoybean(this, simple)
      class(Soybean_), intent(inout) :: this
      logical, optional, intent(in) :: simple
      !type(DiffusionEq_) :: DiffusionEq

      if (present(simple)) then
         if (simple) then
            ! simple flow
            ! for each stem,

            return
         end if
      end if
      !call this%lossEnergy()

      ! solve diffusion equation for multi-domains
      !call DiffusionEq%init(multiDomain=.true.,connectivity=this%domainConnectivity)
      !call DiffusionEq%add(domainlist=this%stem(:)%femdomain)
      !call DiffusionEq%add(domainlist=this%leaf(:)%femdomain)
      !call DiffusionEq%add(domainlist=this%root(:)%femdomain)
      !call DiffusionEq%FixValue(&
      !range=soil%femdonain, projection=true, values=soil%watercontent)

      !call DiffusionEq%run(dt=this%dt)
      !soybean%sourceContent = Diffusion%unknowns

   end subroutine
! ########################################

! ########################################
   subroutine expanitionSoybean(this)
      class(Soybean_), intent(inout) :: this
      !type(ContactMechanics_) :: contact

      !contact%init(connectivity=this%connectivity)
      !contact%add(domain=this%stem(:)%domain)
      !contact%add(domain=this%leaf(:)%domain)
      !contact%add(domain=this%root(:)%domain)
      !contact%add(domain=soil)
      !contact%Density = this%density()
      !contact%PoissonRatio = this%PoissonRatio()
      !contact%PenaltyParameter = this%PenaltyParameter()
      !contact%fix(bottom=.true., direction="xyz",displacement=[0.0d0,0.0d0,0.0d0] )
      !contact%fix(side=.true., direction="xyz",displacement=[0.0d0,0.0d0,0.0d0] )
      !contact%solve(dt=this%dt)
      !soybean%CauchyStress = contact%CauchyStress
      !soybean%displacement = contact%displacement

   end subroutine
! ########################################

! ########################################
   subroutine developmentSoybean(this)
      class(Soybean_), intent(inout) :: this
      integer(int32) :: i, new_stem_id, new_leaf_id, new_root_id

      !do i=1, this%numStemApical
      !   stemID=this%StemApical(i)
      !   if(this%stem(stemID)%source => this%minimalSource )then
      !       new_stem_id = this%newStem(from=StemID,how=this%stem(stemID)%properties)
      !       new_leaf_id = this%newLeaf(from=new_stem_id,how=this%stem(stemID)%properties)
      !   endif
      !enddo

      !do i=1, this%numleaf
      !   leafID=i
      !   call this%leaf(leafID)%change_length_or_width()
      !enddo

      !do i=1, this%numrootApical
      !   rootID=this%rootApical(i)
      !   if(this%root(rootID)%source => this%minimalSource )then
      !       new_root_id = this%newroot(from=rootID,how=this%root(rootID)%properties)
      !   endif
      !enddo

   end subroutine
! ########################################

! ########################################
   subroutine updateFlowersSoybean(this)
      class(Soybean_), intent(inout) :: this
      integer(int32) :: i

      !do i=1, this%numStem()
      !   call this%stem(i)%updateFlowerCapacity()
      !enddo

   end subroutine
! ########################################

! ########################################
   subroutine updatePodsSoybean(this)
      class(Soybean_), intent(inout) :: this
      integer(int32) :: i

      !do i=1, this%numStem()
      !   call this%stem(i)%updatePodCapacity()
      !   call this%stem(i)%growPod()
      !enddo

   end subroutine
! ########################################

! ########################################
   subroutine WaterAbsorptionSoybean(this, temp, dt)
      class(Soybean_), intent(inout) :: this
      real(real64), intent(in) :: temp, dt
      real(real64) :: a, b, c, d, AA, BB, w1max, w2max, w3max, time
      real(real64) :: x_rate, y_rate, z_rate, wx, wy, wz

      this%time = this%time + dt

      ! tested by tachinagaha, 2019
      a = 0.00910d0
      b = -1.76450d0
      c = 3.32E-04
      d = -0.0905180d0
      AA = a*temp + b
      !BB=c*exp(d*temp)
      BB = c*temp + d
      ! width1 becomes 1.7 times, width2 becomes 1.2, width3 becomes 1.1
      w1max = 1.70d0
      w2max = 1.20d0
      w3max = 1.10d0
      this%seed%width1 = this%seed%width1_origin*(w1max - AA*exp(-BB*this%time))
      this%seed%width2 = this%seed%width2_origin*(w2max - AA*exp(-BB*this%time))
      this%seed%width3 = this%seed%width3_origin*(w3max - AA*exp(-BB*this%time))

      ! linear model; it should be changed in near future.
      if (this%time > 60.0d0*6.0d0) then
         this%seed%width2 = this%seed%width2_origin*(w2max)
         this%seed%width3 = this%seed%width3_origin*(w3max)
      else
         this%seed%width2 = this%seed%width2_origin + this%seed%width2_origin*(w2max - 1.0d0)*(this%time)/(60.0d0*6.0d0)
         this%seed%width3 = this%seed%width3_origin + this%seed%width3_origin*(w3max - 1.0d0)*(this%time)/(60.0d0*6.0d0)
      end if

      wx = maxval(this%Seed%FEMDomain%Mesh%NodCoord(:, 1)) - minval(this%Seed%FEMDomain%Mesh%NodCoord(:, 1))
      wy = maxval(this%Seed%FEMDomain%Mesh%NodCoord(:, 2)) - minval(this%Seed%FEMDomain%Mesh%NodCoord(:, 2))
      wz = maxval(this%Seed%FEMDomain%Mesh%NodCoord(:, 3)) - minval(this%Seed%FEMDomain%Mesh%NodCoord(:, 3))
      !print *, wx,wy,wz
      x_rate = 1.0d0/wx
      y_rate = 1.0d0/wy
      z_rate = 1.0d0/wz
      call this%Seed%FEMDomain%resize(x_rate=x_rate, y_rate=y_rate, z_rate=z_rate)
      x_rate = this%seed%width1
      y_rate = this%seed%width2
      z_rate = this%seed%width3
      call this%Seed%FEMDomain%resize(x_rate=x_rate, y_rate=y_rate, z_rate=z_rate)

   end subroutine
! ########################################

! ########################################
   subroutine exportSoybean(this, FilePath, FileName, SeedID, withSTL, withMesh)
      class(Soybean_), intent(inout) :: this
      character(*), optional, intent(in) :: FilePath
      character(*), intent(in) :: FileName
      integer(int32), optional, intent(inout) :: SeedID
      logical, optional, intent(in) :: withSTL, withMesh
      integer(int32) :: i, itr

      itr = SeedID
      ! if seed exists => output
      if (this%Seed%num_of_seed >= 0) then
         if (present(withSTL)) then
            if (withSTL .eqv. .true.) then
               call this%Seed%export(FileName=FileName, SeedID=itr, extention=".stl")
            end if
         end if
         if (present(withMesh)) then
            if (withMesh .eqv. .true.) then
               call this%Seed%export(FileName=FileName, SeedID=itr, extention=".pos")
            end if
         end if

         if (present(FilePath)) then
            call this%Seed%export(FileName=FilePath//"/seed.geo", SeedID=itr)
         else
            call this%Seed%export(FileName=FileName, SeedID=itr)
         end if
      end if

      itr = itr + 1
      ! export NodeSystem
      do i = 1, size(this%NodeSystem)

         if (present(FilePath)) then
            call this%NodeSystem(i)%export(FileName=FilePath//"/Node.geo", objID=itr)
         else
            call this%NodeSystem(i)%export(FileName=FileName//"_Node.geo", objID=itr)
         end if
         if (i == this%num_of_node) then
            exit
         end if
      end do

      ! export RootSystem
      do i = 1, size(this%RootSystem)

         if (present(FilePath)) then
            call this%RootSystem(i)%export(FileName=FilePath//"/Root.geo", RootID=itr)
         else
            call this%RootSystem(i)%export(FileName=FileName//"_Root.geo", RootID=itr)
         end if
         if (i == this%num_of_root) then
            exit
         end if
      end do
      SeedID = itr

   end subroutine
! ########################################

! ########################################

! ########################################
!subroutine initsoybean(this,growth_habit,Max_Num_of_Node)
!    class(soybean_) :: this
!    character(*),optional,intent(in) :: growth_habit
!    integer(int32),optional,intent(in)::Max_Num_of_Node
!    integer(int32) ::n
!
!    if(present(growth_habit) )then
!        this%growth_habit=growth_habit
!    else
!        this%growth_habit="determinate"
!    endif
!
!    this%growth_stage="VE"
!
!    n=input(default=100,option=Max_Num_of_Node)
!
!    allocate(this%NodeSystem(n))
!    this%NumOfNode=0
!    this%NumOfRoot=0
!
!    ! set an initial node and root
!    ! two leaves, one root.
!
!    call this%AddNode()
!
!end subroutine
!! ########################################
!
!
!
!
!
!
!! ########################################
!subroutine AddNodeSoybean(this,SizeRatio)
!    class(soybean_),intent(inout):: this
!    real(real64),optional,intent(in)::SizeRatio
!    real(real64) :: magnif
!
!    magnif=input(default=1.0d0,option=SizeRatio)
!    this%NumOfNode=this%NumOfNode+1
!
!    ! add leaves
!    if(this%NumOfNode==1 .or. this%NumOfNode==2)then
!        allocate(this%NodeSystem(this%NumOfNode)%leaf(2) )
!        call this%NodeSystem(this%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!        call this%NodeSystem(this%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    else
!        allocate(this%NodeSystem(this%NumOfNode)%leaf(3) )
!        call this%NodeSystem(this%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!        call this%NodeSystem(this%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!        call this%NodeSystem(this%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!    ! add stem
!    if(this%NumOfNode==1 .or. this%NumOfNode==2)then
!        allocate(this%NodeSystem(this%NumOfNode)%Stem(1) )
!        call this%NodeSystem(this%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!    ! add Peti
!    if(this%NumOfNode==1 .or. this%NumOfNode==2)then
!        allocate(this%NodeSystem(this%NumOfNode)%Peti(1) )
!        call this%NodeSystem(this%NumOfNode)%Peti(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!end subroutine
!! ########################################
!

! ########################################
   subroutine showSoybean(this, name)
      class(Soybean_), intent(inout) :: this
      character(*), intent(in)::name

      if (this%struct%empty() .eqv. .true.) then
         print *, "Error :: showSoybean>> no structure is imported."
         return
      end if

      call this%struct%export(name=name)

   end subroutine
! ########################################

! ########################################
   function numleafsoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: ret, i

      ret = 0
      if (.not. allocated(this%leaf)) then
         return
      end if

      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%Mesh%empty() .eqv. .false.) then
            ret = ret + 1
         end if
      end do

   end function
! ########################################

! ########################################
   function numstemsoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: ret, i

      ret = 0
      if (.not. allocated(this%stem)) then
         return
      end if

      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%Mesh%empty() .eqv. .false.) then
            ret = ret + 1
         end if
      end do

   end function
! ########################################

! ########################################
   function numrootsoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: ret, i

      ret = 0
      if (.not. allocated(this%root)) then
         return
      end if

      do i = 1, size(this%root)
         if (this%root(i)%femdomain%Mesh%empty() .eqv. .false.) then
            ret = ret + 1
         end if
      end do

   end function
! ########################################

! ########################################
   subroutine gmshSoybean(this, name, num_threads, single_file)
      class(Soybean_), intent(inout) :: this
      character(*), intent(in) :: name
      type(FEMDomain_) :: femdomain
      integer(int32), optional, intent(in) :: num_threads
      logical, optional, intent(in) :: single_file
      integer(int32) :: i, n

      if (present(single_file)) then
         if (single_file) then
            ! export mesh for a single file
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (.not. this%stem(i)%femdomain%empty()) then
                     femdomain = femdomain + this%stem(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%leaf)) then
               do i = 1, size(this%leaf)
                  if (.not. this%leaf(i)%femdomain%empty()) then
                     femdomain = femdomain + this%leaf(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%root)) then
               do i = 1, size(this%root)
                  if (.not. this%root(i)%femdomain%empty()) then
                     femdomain = femdomain + this%root(i)%femdomain
                  end if
               end do
            end if
            call femdomain%gmsh(name=name)
            return
         end if
      end if

      n = input(default=1, option=num_threads)
    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do

      do i = 1, size(this%stem)
         !if(this%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
         call this%stem(i)%gmsh(name=name//"_stem"//str(i))
         !endif
      end do
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do

      do i = 1, size(this%root)
         !if(this%root(i)%femdomain%mesh%empty() .eqv. .false. )then
         call this%root(i)%gmsh(name=name//"_root"//str(i))
         !endif
      end do
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%leaf)
         !if(this%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
         call this%leaf(i)%gmsh(name=name//"_leaf"//str(i))
         !endif
      end do
    !!$OMP end do
    !!$OMP end parallel

   end subroutine
! ########################################

! ########################################
   subroutine mshSoybean(this, name, num_threads)
      class(Soybean_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32), optional, intent(in) :: num_threads
      integer(int32) :: i, n
      type(IO_) :: f
      ! index file
      call f%open(name//"_index.txt", "w")

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               call f%write(name//"_stem"//str(i)//".msh")
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               call f%write(name//"_root"//str(i)//".msh")
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               call f%write(name//"_leaf"//str(i)//".msh")
            end if
         end do
      end if
      call f%close()

      n = input(default=1, option=num_threads)
    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%stem)
         !if(this%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
         call this%stem(i)%msh(name=name//"_stem"//str(i))
         !endif
      end do
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%root)
         !if(this%root(i)%femdomain%mesh%empty() .eqv. .false. )then
         call this%root(i)%msh(name=name//"_root"//str(i))
         !endif
      end do
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%leaf)
         !if(this%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
         call this%leaf(i)%msh(name=name//"_leaf"//str(i))
         !endif
      end do
    !!$OMP end do
    !!$OMP end parallel

   end subroutine
! ########################################

! ########################################
   subroutine vtkSoybean(this, name, num_threads, single_file, &
                         scalar_field, vector_field, tensor_field, field_name)
      class(Soybean_), intent(inout) :: this
      character(*), intent(in) :: name
      character(*), optional, intent(in) :: field_name

      type(IO_) :: f
      type(FEMDomain_) :: femdomain
      integer(int32), optional, intent(in) :: num_threads
      real(real64), optional, intent(in) :: scalar_field(:)
      real(real64), optional, intent(in) :: vector_field(:, :)
      real(real64), optional, intent(in) :: tensor_field(:, :, :)
      integer(int32) :: i, n
      logical, optional, intent(in) :: single_file

      if (.not. allocated(this%stem)) then
         if (.not. allocated(this%leaf)) then
            if (.not. allocated(this%root)) then
               return
            end if
         end if
      end if

      if (present(single_file)) then
         if (single_file) then
            ! export mesh for a single file
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (.not. this%stem(i)%femdomain%empty()) then
                     femdomain = femdomain + this%stem(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%leaf)) then
               do i = 1, size(this%leaf)
                  if (.not. this%leaf(i)%femdomain%empty()) then
                     femdomain = femdomain + this%leaf(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%root)) then
               do i = 1, size(this%root)
                  if (.not. this%root(i)%femdomain%empty()) then
                     femdomain = femdomain + this%root(i)%femdomain
                  end if
               end do
            end if

            if(allocated(this%meristem))then
               do i=1,size(this%meristem)
                  if( .not. this%meristem(i)%empty())then
                     femdomain = femdomain + this%meristem(i)%get_all_femdomain()
                  endif
               enddo
            endif

            if (present(scalar_field)) then
               ! export scalar-valued field
               ! as a single file
               call femdomain%vtk(field=field_name, name=name, scalar=scalar_field)
            elseif (present(vector_field)) then
               ! export vector-valued field
               ! as a single file
               call femdomain%vtk(field=field_name, name=name, vector=vector_field)
            elseif (present(tensor_field)) then
               ! export tensor-valued field
               ! as a single file
               call femdomain%vtk(field=field_name, name=name, tensor=tensor_field)
            else
               call femdomain%vtk(field=field_name, name=name)
            end if
            return
         end if
      end if

      n = input(default=1, option=num_threads)

      ! index file
      call f%open(name//"_index.txt", "w")

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               call f%write(name//"_stem"//str(i)//".vtk")
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               call f%write(name//"_root"//str(i)//".vtk")
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               call f%write(name//"_leaf"//str(i)//".vtk")
            end if
         end do
      end if
      call f%close()

      if (allocated(this%stem)) then
        !!$OMP parallel num_threads(n) private(i)
        !!$OMP do
         do i = 1, size(this%stem)
            !if(this%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call this%stem(i)%vtk(field_name=field_name, name=name//"_stem"//str(i))
            !endif
         end do
        !!$OMP end do
        !!$OMP end parallel
      end if

      if (allocated(this%root)) then

        !!$OMP parallel num_threads(n) private(i)
        !!$OMP do
         do i = 1, size(this%root)
            !if(this%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call this%root(i)%vtk(field_name=field_name, name=name//"_root"//str(i))
            !endif
         end do

        !!$OMP end do
        !!$OMP end parallel
      end if

      if (allocated(this%leaf)) then

        !!$OMP parallel num_threads(n) private(i)
        !!$OMP do
         do i = 1, size(this%leaf)
            !if(this%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call this%leaf(i)%vtk(field_name=field_name, name=name//"_leaf"//str(i))
            !endif
         end do
        !!$OMP end do
        !!$OMP end parallel
      end if

   end subroutine
! ########################################

! ########################################
   subroutine jsonSoybean(this, name)
      class(Soybean_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32) :: i, countnum
      type(IO_) :: f

      call f%open(name//".json")
      call f%write("{")
      countnum = 0
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
            countnum = countnum + 1
            call f%write('"'//"stem"//str(i)//'":')
            call this%stem(i)%femdomain%json(name=name//"_stem"//str(i), fh=f%fh, endl=.false.)
         end if
      end do
      call f%write('"num_stem":'//str(countnum)//',')

      countnum = 0
      do i = 1, size(this%root)
         if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
            countnum = countnum + 1
            call f%write('"'//"root"//str(i)//'":')
            call this%root(i)%femdomain%json(name=name//"_root"//str(i), fh=f%fh, endl=.false.)
         end if
      end do
      call f%write('"num_root":'//str(countnum)//',')

      countnum = 0
      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            countnum = countnum + 1
            call f%write('"'//"leaf"//str(i)//'":')
            call this%leaf(i)%femdomain%json(name=name//"_leaf"//str(i), fh=f%fh, endl=.false.)
         end if
      end do
      call f%write('"this%num_leaf":'//str(countnum)//',')
      call f%write('"return_soybean":0')
      call f%write("}")
      call f%close()
   end subroutine
! ########################################

! ########################################
   subroutine stlSoybean(this, name, num_threads, single_file)
      class(Soybean_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32), optional, intent(in) :: num_threads
      type(FEMDomain_) :: femdomain
      logical, optional, intent(in) :: single_file
      integer(int32) :: i, n

      type(IO_) :: f

      if (present(single_file)) then
         if (single_file) then
            ! export mesh for a single file
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (.not. this%stem(i)%femdomain%empty()) then
                     femdomain = femdomain + this%stem(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%leaf)) then
               do i = 1, size(this%leaf)
                  if (.not. this%leaf(i)%femdomain%empty()) then
                     femdomain = femdomain + this%leaf(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%root)) then
               do i = 1, size(this%root)
                  if (.not. this%root(i)%femdomain%empty()) then
                     femdomain = femdomain + this%root(i)%femdomain
                  end if
               end do
            end if
            call femdomain%stl(name=name)
            return
         end if
      end if

      ! index file
      call f%open(name//"_index.txt", "w")

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               call f%write(name//"_stem"//str(i)//".stl")
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               call f%write(name//"_root"//str(i)//".stl")
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               call f%write(name//"_leaf"//str(i)//".stl")
            end if
         end do
      end if
      call f%close()

      n = input(default=1, option=num_threads)
      !call execute_command_line("echo ' ' > "//name//".stl")
    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%stem(i)%stl(name=name//"_stem"//str(i))
            !call execute_command_line("cat "//name//"_stem"//str(i)//"_000001.stl >> "//name//".stl")
         end if
      end do
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%root)
         if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%root(i)%stl(name=name//"_root"//str(i))
            !call execute_command_line("cat "//name//"_root"//str(i)//"_000001.stl >> "//name//".stl")
         end if
      end do
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%leaf(i)%stl(name=name//"_leaf"//str(i))
            !call execute_command_line("cat "//name//"_leaf"//str(i)//"_000001.stl >> "//name//".stl")
         end if
      end do
    !!$OMP end do
    !!$OMP end parallel

      call execute_command_line("cat "//name//"*_leaf*.stl > "//name//"_leaf.stl")
      call execute_command_line("cat "//name//"*_stem*.stl > "//name//"_stem.stl")
      call execute_command_line("cat "//name//"*_root*.stl > "//name//"_root.stl")
      call execute_command_line("cat "//name//"_leaf.stl "//name//"_stem.stl " &
                                //name//"_root.stl > "//name//".stl")

   end subroutine
! ########################################

! ########################################
   subroutine plySoybean(this, name, num_threads, single_file)
      class(Soybean_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32), optional, intent(in) :: num_threads
      type(FEMDomain_) :: femdomain
      logical, optional, intent(in) :: single_file
      integer(int32) :: i, n

      type(IO_) :: f

      if (present(single_file)) then
         if (single_file) then
            ! export mesh for a single file
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (.not. this%stem(i)%femdomain%empty()) then
                     femdomain = femdomain + this%stem(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%leaf)) then
               do i = 1, size(this%leaf)
                  if (.not. this%leaf(i)%femdomain%empty()) then
                     femdomain = femdomain + this%leaf(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%root)) then
               do i = 1, size(this%root)
                  if (.not. this%root(i)%femdomain%empty()) then
                     femdomain = femdomain + this%root(i)%femdomain
                  end if
               end do
            end if
            call femdomain%ply(name=name)
            return
         end if
      end if

      ! index file
      call f%open(name//"_index.txt", "w")

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               call f%write(name//"_stem"//str(i)//".ply")
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               call f%write(name//"_root"//str(i)//".ply")
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               call f%write(name//"_leaf"//str(i)//".ply")
            end if
         end do
      end if
      call f%close()

      n = input(default=1, option=num_threads)
      !call execute_command_line("echo ' ' > "//name//".ply")
    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%stem(i)%ply(name=name//"_stem"//str(i))
            !call execute_command_line("cat "//name//"_stem"//str(i)//"_000001.ply >> "//name//".ply")
         end if
      end do
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%root)
         if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%root(i)%ply(name=name//"_root"//str(i))
            !call execute_command_line("cat "//name//"_root"//str(i)//"_000001.ply >> "//name//".ply")
         end if
      end do
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do
      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%leaf(i)%ply(name=name//"_leaf"//str(i))
            !call execute_command_line("cat "//name//"_leaf"//str(i)//"_000001.ply >> "//name//".ply")
         end if
      end do
    !!$OMP end do
    !!$OMP end parallel

      call execute_command_line("cat "//name//"*_leaf*.ply > "//name//"_leaf.ply")
      call execute_command_line("cat "//name//"*_stem*.ply > "//name//"_stem.ply")
      call execute_command_line("cat "//name//"*_root*.ply > "//name//"_root.ply")
      call execute_command_line("cat "//name//"_leaf.ply "//name//"_stem.ply " &
                                //name//"_root.ply > "//name//".ply")

   end subroutine
! ########################################

! ########################################
   subroutine moveSoybean(this, x, y, z, reset)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      logical,optional,intent(in) :: reset
      integer(int32) :: i

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%stem(i)%move(x=x, y=y, z=z, reset=reset)
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%leaf(i)%move(x=x, y=y, z=z, reset=reset)
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%root(i)%move(x=x, y=y, z=z,reset=reset)
            end if
         end do
      end if

   end subroutine
! ########################################

! ########################################
   subroutine laytracingsoybean(this, light, Transparency, Resolution)
      class(Soybean_), intent(inout) :: this
      type(Light_), intent(in) :: light
      real(real64), optional, intent(in) :: Transparency, Resolution
      real(real64), allocatable :: ppfd(:)
      integer(int32), allocatable ::  NumberOfElement(:)
      integer(int32) :: from, elem_id

      ! >>> regacy
      real(real64), allocatable :: stemcenter(:, :), stemradius(:)
      real(real64), allocatable :: leafcenter(:, :), leafradius(:)
      real(real64), allocatable :: elemnodcoord(:, :), x(:), x2(:)
      real(real64) :: max_PPFD, r, rc, r0
      real(real64), parameter :: extinction_ratio = 100.0d0 ! ratio/m
      type(IO_) :: f
      integer(int32) :: i, j, n, num_particle, k, l, nodeid, m, totcount
      integer(int32) :: num_particle_leaf, tocount_leaf
      ! <<< regacy

      ppfd = this%getPPFD(Light=Light, Transparency=Transparency, Resolution=Resolution)

      NumberOfElement = this%getNumberOfElement()
      from = sum(NumberOfElement(1:this%numstem()))

      elem_id = from
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%empty()) then
            this%leaf(i)%ppfd = zeros(this%leaf(i)%femdomain%ne())
            do j = 1, this%leaf(i)%femdomain%ne()
               elem_id = elem_id + 1
               this%leaf(i)%ppfd(j) = ppfd(elem_id)
            end do
         end if
      end do
      return

      ! >>> Regacy

      ! 総当りで、総遮蔽長を割り出す
      ! 茎は光を通さない、葉は透過率あり、空間は透過率ゼロ
      ! 要素中心から頂点への平均長さを半径に持ち、要素中心を中心とする球
      ! を考え、Layとの公差判定を行う。
      num_particle = 0
      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            num_particle = num_particle + size(this%leaf(i)%femdomain%mesh%ElemNod, 1)
         end if
      end do
      allocate (leafcenter(num_particle, 3), leafradius(num_particle))
      leafcenter(:, :) = 0.0d0
      leafradius(:) = 0.0d0

      num_particle = 0
      do i = 1, size(this%leaf)
         if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
            num_particle = num_particle + size(this%stem(i)%femdomain%mesh%ElemNod, 1)
         end if
      end do
      allocate (stemcenter(num_particle, 3), stemradius(num_particle))
      stemcenter(:, :) = 0.0d0
      stemradius(:) = 0.0d0

      num_particle = 0

      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            n = size(this%leaf(i)%femdomain%mesh%Elemnod, 2)
            m = size(this%leaf(i)%femdomain%mesh%Nodcoord, 2)
            allocate (elemnodcoord(n, m))
            allocate (x(m))
            do j = 1, size(this%leaf(i)%femdomain%mesh%elemnod, 1)
               do k = 1, size(this%leaf(i)%femdomain%mesh%elemnod, 2)
                  nodeid = this%leaf(i)%femdomain%mesh%elemnod(j, k)
                  elemnodcoord(k, :) = this%leaf(i)%femdomain%mesh%Nodcoord(nodeid, :)
               end do
               num_particle = num_particle + 1
               do k = 1, size(elemnodcoord, 1)
                  do l = 1, size(elemnodcoord, 2)
                     leafcenter(num_particle, l) = &
                        +leafcenter(num_particle, l) &
                        + 1.0d0/dble(size(elemnodcoord, 1))*elemnodcoord(k, l)
                  end do
               end do
               do k = 1, size(elemnodcoord, 1)
                  x(:) = elemnodcoord(k, :)
                  x(:) = x(:) - leafcenter(num_particle, :)
                  if (k >= 2 .and. leafradius(num_particle) > sqrt(dot_product(x, x))) then
                     leafradius(num_particle) = sqrt(dot_product(x, x))
                  elseif (k == 1) then
                     leafradius(num_particle) = sqrt(dot_product(x, x))
                  else
                     cycle
                  end if
               end do
            end do
            deallocate (elemnodcoord)
            deallocate (x)
         end if
      end do

      num_particle = 0
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
            n = size(this%stem(i)%femdomain%mesh%Elemnod, 2)
            m = size(this%stem(i)%femdomain%mesh%Nodcoord, 2)
            allocate (elemnodcoord(n, m))
            allocate (x(m))
            do j = 1, size(this%stem(i)%femdomain%mesh%elemnod, 1)
               do k = 1, size(this%stem(i)%femdomain%mesh%elemnod, 2)
                  nodeid = this%stem(i)%femdomain%mesh%elemnod(j, k)
                  elemnodcoord(k, :) = this%stem(i)%femdomain%mesh%Nodcoord(nodeid, :)
               end do
               num_particle = num_particle + 1
               do k = 1, size(elemnodcoord, 1)
                  do l = 1, size(elemnodcoord, 2)
                     stemcenter(num_particle, l) = &
                        +stemcenter(num_particle, l) &
                        + 1.0d0/dble(size(elemnodcoord, 1))*elemnodcoord(k, l)
                  end do
               end do
               do k = 1, size(elemnodcoord, 1)
                  x(:) = elemnodcoord(k, :)
                  x(:) = x(:) - stemcenter(num_particle, :)
                  !最小半径で考える
                  if (k >= 2 .and. stemradius(num_particle) > sqrt(dot_product(x, x))) then
                     stemradius(num_particle) = sqrt(dot_product(x, x))
                  elseif (k == 1) then
                     stemradius(num_particle) = sqrt(dot_product(x, x))
                  else
                     cycle
                  end if
               end do
            end do
            deallocate (elemnodcoord)
            deallocate (x)
         end if
      end do

      ! DEBUG
      call f%open("leaf.txt")
      do i = 1, size(leafcenter, 1)
         write (f%fh, *) leafcenter(i, :)
      end do
      call f%close()

      call f%open("stem.txt")
      do i = 1, size(stemcenter, 1)
         write (f%fh, *) stemcenter(i, :)
      end do
      call f%close()

      allocate (x(3), x2(3))

      num_particle = 0
      totcount = 0
      tocount_leaf = 0
      num_particle_leaf = 0

      do i = 1, size(this%leaf)
         !print *, i,"/",this%numleaf()
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            ! 葉あり
            this%leaf(i)%PPFD(:) = max_PPFD

            !!$OMP parallel do private(j)
            do j = 1, size(this%leaf(i)%PPFD)

               totcount = tocount_leaf + j

               num_particle = num_particle_leaf + j
               ! それぞれの要素について、遮蔽particleを探索
               ! 茎：全減衰
               ! 葉：半減衰
               ! 簡単のため上からのみ
               ! x-yのみについて見て、上方かつx-y平面距離が半径以内で覆陰判定
               x(:) = leafcenter(num_particle, :)
               r0 = leafradius(num_particle)
               ! 枝による覆陰判定

               do k = 1, size(stemcenter, 1)
                  x2(:) = stemcenter(k, :)
                  r = stemradius(k)
                  rc = (x(1) - x2(1))**(2.0d0) + (x(2) - x2(2))**(2.0d0)
                  rc = sqrt(rc)
                  if (rc <= r0 + r .and. x(3) < x2(3)) then
                     ! 茎により覆陰されてる
                     this%leaf(i)%PPFD(j) = 0.0d0
                     exit
                  end if
               end do
               if (this%leaf(i)%PPFD(j) == 0.0d0) then
                  cycle
               end if

               do k = 1, size(leafcenter, 1)
                  ! もし自信だったら除外
                  if (totcount == k) then
                     cycle
                  end if

                  x2(:) = leafcenter(k, :)
                  r = leafradius(k)
                  rc = (x(1) - x2(1))**(2.0d0) + (x(2) - x2(2))**(2.0d0)
                  rc = sqrt(rc)
                  if (rc <= (r0 + r)/2.0d0 .and. x(3) < x2(3)) then
                     ! 茎により覆陰されてる
                     this%leaf(i)%PPFD(j) = &
                        this%leaf(i)%PPFD(j)*(1.0d0 - extinction_ratio*2.0d0*r)
                     if (this%leaf(i)%PPFD(j) <= 0.0d0) then
                        this%leaf(i)%PPFD(j) = 0.0d0
                     end if
                  end if
               end do

            end do
            !!$OMP end parallel do

            tocount_leaf = tocount_leaf + size(this%leaf(i)%PPFD)
            num_particle_leaf = num_particle_leaf + size(this%leaf(i)%PPFD)
         end if
      end do

      call f%open("PPFD.txt")
      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            ! 葉あり
            do j = 1, size(this%leaf(i)%PPFD, 1)
               write (f%fh, *) this%leaf(i)%PPFD(j), "leaf_id: ", str(i), "elem_id: ", str(j)
            end do
         end if
      end do
      call f%close()

   end subroutine
! ########################################

   subroutine addNodeSoybean(this, StemNodeID, RootNodeID, peti_width_ave, peti_width_sig, peti_size_ave &
                             , peti_size_sig, peti_angle_ave, peti_angle_sig, leaf_thickness_ave, leaf_thickness_sig &
                             , leaf_length_ave, leaf_length_sig, leaf_width_ave, leaf_width_sig, leaf_angle_sig &
                             , leaf_angle_ave, mainstem_to_branch)
      class(Soybean_), intent(inout) :: this
      integer(int32), optional, intent(in) :: StemNodeID, RootNodeID
      real(real64), optional, intent(in) :: peti_width_ave, peti_width_sig, peti_size_ave &
                                           , peti_size_sig, peti_angle_ave, peti_angle_sig, leaf_thickness_ave, leaf_thickness_sig &
                                            , leaf_length_ave, leaf_length_sig, leaf_width_ave, leaf_width_sig, leaf_angle_sig &
                                            , leaf_angle_ave
      logical, optional, intent(in) :: mainstem_to_branch
      logical :: mainstem_2_branch = .false.
      real(real64), allocatable :: leaf_z_angles(:)
      type(Random_) :: random
      type(soybean_NodeID_Branch_), allocatable :: old_NodeID_Branch(:)
      integer(int32) :: i, j, branch_id, My_StemID

      if (present(mainstem_to_branch)) then
         mainstem_2_branch = mainstem_to_branch
      end if

      call this%update()

      if (present(StemNodeID)) then
         i = StemNodeID

         this%leaf_thickness_ave(this%num_leaf) = input( &
                                                default=this%leaf_thickness_ave(this%num_leaf), &
                                                option=leaf_thickness_ave)
         this%leaf_thickness_sig(this%num_leaf) = input( &
                                                default=this%leaf_thickness_sig(this%num_leaf), &
                                                option=leaf_thickness_sig)

         this%leaf_length_ave(this%num_leaf) = input( &
                                             default=this%leaf_length_ave(this%num_leaf), &
                                             option=leaf_length_ave)
         this%leaf_length_sig(this%num_leaf) = input( &
                                             default=this%leaf_length_sig(this%num_leaf), &
                                             option=leaf_length_sig)
         this%leaf_width_ave(this%num_leaf) = input( &
                                            default=this%leaf_width_ave(this%num_leaf), &
                                            option=leaf_width_ave)
         this%leaf_width_sig(this%num_leaf) = input( &
                                            default=this%leaf_width_sig(this%num_leaf), &
                                            option=leaf_width_sig)
         this%leaf_angle_sig(this%num_leaf) = input( &
                                            default=this%leaf_angle_sig(this%num_leaf), &
                                            option=leaf_angle_sig)

         this%leaf_angle_ave(this%num_leaf) = input( &
                                            default=this%leaf_angle_ave(this%num_leaf), &
                                            option=leaf_angle_ave)

         ! main stem -> main stem
         if (this%isMainStem(StemNodeID) .and. .not. mainstem_2_branch) then
            print *, "Main -> Main", StemNodeID
            ! main stem
            i = StemNodeID
            call this%stem(this%numStem() + 1)%init(config=this%stemconfig)

            call extend(this%NodeID_MainStem)
            this%NodeID_MainStem(size(this%NodeID_MainStem)) = this%numStem()

            if (this%ms_node > 0.0d0) then
               call this%stem(i)%resize( &
                  x=this%ms_width, &
                  y=this%ms_width, &
                  z=this%ms_length/dble(this%ms_node) &
                  )
            end if

            call this%stem(i)%rotate( &
               x=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)), &
               y=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)), &
               z=this%stem(StemNodeID)%femdomain%total_rotation(3) + radian((random%random() - 0.50d0)*90.0d0) &
               )
            call this%stem(i)%change_length_or_width(dt=0.0d0)
            this%stem(i)%StemID = 0
            this%stem(i)%InterNodeID = size(this%NodeID_MainStem)

            ! branch -> branch
         elseif (.not. this%isMainStem(StemNodeID) .and. .not. mainstem_2_branch) then

            i = StemNodeID

            branch_id = this%BranchID(i)
            print *, "Branch -> Branch branch id", branch_id

            call this%stem(this%numStem() + 1)%init(config=this%stemconfig)

            if (.not. allocated(this%NodeID_Branch(branch_id)%ID)) then
               this%NodeID_Branch(branch_id)%ID = [this%numStem()]
            else
               this%NodeID_Branch(branch_id)%ID = this%NodeID_Branch(branch_id)%ID//[this%numStem()]
            end if

            My_StemID = this%numStem()
            call this%stem(My_StemID)%rotate( &
               x=radian(random%gauss(mu=this%br_angle_ave(branch_id), sigma=this%br_angle_sig(branch_id))), &
               y=radian(random%gauss(mu=this%br_angle_ave(branch_id), sigma=this%br_angle_sig(branch_id))), &
               z=this%stem(StemNodeID)%femdomain%total_rotation(3) + radian((random%random() - 0.50d0)*90.0d0) &
               )

            call this%stem(My_StemID)%change_length_or_width(dt=0.0d0)
            this%stem(My_StemID)%StemID = branch_id
            this%stem(My_StemID)%InterNodeID = size(this%NodeID_Branch(branch_id)%ID)
            ! main stem -> branch
         elseif (this%isMainStem(StemNodeID) .and. mainstem_2_branch) then
            ! branch
            i = StemNodeID ! 1 : stem ID of main stem
            My_StemID = i

            ! create new internode
            call this%stem(this%numStem() + 1)%init(config=this%stemconfig)

            ! if mainstem -> branch

            if (allocated(this%MainStem_num_branch)) then
               branch_id = 1
               do j = 1, size(this%NodeID_MainStem)
                  if (this%NodeID_MainStem(j) == StemNodeID) then
                     exit
                  elseif (this%MainStem_num_branch(j) /= 0) then
                     branch_id = branch_id + this%MainStem_num_branch(j)
                     cycle
                  else
                     cycle
                  end if
               end do
            else
               branch_id = 1
            end if

            print *, "Main -> Branch branch id", branch_id

            if (.not. allocated(this%NodeID_Branch)) then
               allocate (this%NodeID_Branch(this%MaxStemNum))
            end if

            if (.not. allocated(this%NodeID_Branch(branch_id)%ID)) then
               this%NodeID_Branch(branch_id)%ID = [this%numStem()]
            else
               this%NodeID_Branch(branch_id)%ID = this%NodeID_Branch(branch_id)%ID//[this%numStem()]
            end if

            My_StemID = this%numStem()

            this%stem(this%numStem())%StemID = branch_id

            call this%stem(My_StemID)%rotate( &
               x=radian(random%gauss(mu=this%br_angle_ave(branch_id), sigma=this%br_angle_sig(branch_id))), &
               y=radian(random%gauss(mu=this%br_angle_ave(branch_id), sigma=this%br_angle_sig(branch_id))), &
               z=radian(random%random()*360.0d0) &
               )

            call this%stem(My_StemID)%change_length_or_width(dt=0.0d0)
            this%stem(My_StemID)%StemID = branch_id
            this%stem(My_StemID)%InterNodeID = 1
         else
            print *, this%isMainStem(StemNodeID)
            print *, mainstem_2_branch
            print *, "[ERROR] addNode"
            stop
         end if

         call this%stem(this%numStem())%connect("=>", this%stem(StemNodeID))
         this%stem2stem(this%numStem(), StemNodeID) = PF_SOYBEAN_BRANCH_TO_MAINSTEM
         ! petiole

         call this%stem(this%numStem() + 1)%init(config=this%stemconfig)

         this%stem(this%numStem())%StemID = -1

         call this%stem(this%numStem())%resize( &
            x=random%gauss(mu=this%peti_width_ave(i), sigma=this%peti_width_sig(i)), &
            y=random%gauss(mu=this%peti_width_ave(i), sigma=this%peti_width_sig(i)), &
            z=random%gauss(mu=this%peti_size_ave(i), sigma=this%peti_size_sig(i)) &
            )
         call this%stem(this%numStem())%change_length_or_width(dt=0.0d0)

         call this%stem(this%numStem())%rotate( &
            x=radian(random%gauss(mu=this%peti_angle_ave(i), sigma=this%peti_angle_sig(i))), &
            y=0.0d0, &
            z=radian(360.0d0*random%random()) &
            )

         !call this%stem(this%numStem() )%connect("=>",this%stem(i))
         !this%stem2stem(this%numStem() ,i) = 1
         call this%stem(this%numStem())%connect("=>", this%stem(this%numStem() - 1))
         this%stem2stem(this%numStem(), this%numStem() - 1) = PF_SOYBEAN_PETIOLE_TO_BRANCH

         leaf_z_angles = linspace([0.0d0, 360.0d0], this%max_num_leaf_per_petiole + 1)
         do j = 1, this%max_num_leaf_per_petiole
            leaf_z_angles(j) = radian(leaf_z_angles(j))
         end do

         leaf_z_angles(:) = leaf_z_angles(:) + radian(random%random()*360.0d0)

         ! add leaves
         do j = 1, this%max_num_leaf_per_petiole
            this%num_leaf = this%num_leaf + 1

            call this%leaf(this%num_leaf)%init(config=this%leafconfig, species=PF_GLYCINE_SOJA)
            call this%leaf(this%num_leaf)%resize( &
               y=random%gauss(mu=this%leaf_thickness_ave(i), sigma=this%leaf_thickness_sig(i)), &
               z=random%gauss(mu=this%leaf_length_ave(i), sigma=this%leaf_length_sig(i)), &
               x=random%gauss(mu=this%leaf_width_ave(i), sigma=this%leaf_width_sig(i)) &
               )
            call this%leaf(this%num_leaf)%rotate( &
               x=radian(random%gauss(mu=this%leaf_angle_ave(i), sigma=this%leaf_angle_sig(i))), &
               y=0.0d0, &
               z=leaf_z_angles(j) &
               )
            call this%leaf(this%num_leaf)%connect("=>", this%stem(this%numStem()))
            this%leaf2stem(this%num_leaf, this%numStem()) = 1

            call this%leaf(this%num_leaf)%change_length_or_width(dt=0.0d0)

         end do
      elseif (present(RootNodeID)) then

         ! set mainroot
         call this%root(this%numRoot() + 1)%init(this%rootconfig)
         call this%root(i)%resize( &
            x=this%mr_width, &
            y=this%mr_width, &
            z=this%mr_length/dble(this%mr_node) &
            )
         call this%root(i)%rotate( &
            x=radian(random%gauss(mu=this%mr_angle_ave, sigma=this%mr_angle_sig)), &
            y=radian(random%gauss(mu=this%mr_angle_ave, sigma=this%mr_angle_sig)), &
            z=radian(random%gauss(mu=this%mr_angle_ave, sigma=this%mr_angle_sig)) &
            )

         i = RootNodeID
         call this%root(this%numRoot())%connect("=>", this%root(i))
         this%root2root(this%numRoot(), i) = 1

      else
         print *, "ERROR :: add Node ` soybean >> RootNodeID or StemNodeID should be identified."
         stop
      end if

      call this%update()
   end subroutine
! ########################################

! ########################################
   subroutine addStemSoybean(this, stemid, rotx, roty, rotz, json)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: stemid
      character(*), optional, intent(in) :: json
      real(real64), optional, intent(in) :: rotx, roty, rotz
      integer(int32) :: i

      ! add a stem after stem(stemid)
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%mesh%empty() .eqv. .true.) then
            if (present(json)) then
               call this%stem(i)%init(json)
               call this%stem(i)%rotate(x=rotx, y=roty, z=rotz)
               call this%stem(i)%connect("=>", this%stem(stemid))
               return
            else
               call this%stem(i)%init()
               call this%stem(i)%rotate(x=rotx, y=roty, z=rotz)
               call this%stem(i)%connect("=>", this%stem(stemid))
               return
            end if
         else
            cycle
         end if
      end do

   end subroutine
! #############################################################

   subroutine deformSoybean(this, displacement, penaltyparameter, groundLevel, disp, &
                            x_min, x_max, y_min, y_max, z_min, z_max)

      class(Soybean_), target, intent(inout) :: this

      ! deform soybean by displacement
      real(real64), optional, intent(in) :: displacement(:)

      ! >> regacy
      real(real64), optional, intent(in) :: groundLevel, disp(3)
      real(real64), optional, intent(in) :: penaltyparameter, x_min, x_max, y_min, y_max, z_min, z_max
      type(FEMDomainp_), allocatable :: domainsp(:)
      integer(int32), allocatable :: contactList(:, :)
      integer(int32) :: i, j, numDomain, stemDomain, leafDomain, rootDomain, from, to, nd, nn
      real(real64) :: penalty, GLevel

      if (present(displacement)) then
         if (size(displacement) /= this%nn()*3) then
            print *, "ERROR :: deformSoybean >> size(displacement) should be (this%numStem() + this%numLeaf() + this%numRoot())*3"
            return
         end if

         ! order :: stem -> leaf -> root
         from = 1
         to = 0
         if (allocated(this%stem)) then
            do i = 1, size(this%stem)
               if (.not. this%stem(i)%femdomain%Mesh%empty()) then
                  nn = this%stem(i)%femdomain%nn()
                  nd = this%stem(i)%femdomain%nd()

                  to = from + this%stem(i)%femdomain%nn()*this%stem(i)%femdomain%nd() - 1

                  this%stem(i)%femdomain%mesh%nodcoord(:, :) = &
                     this%stem(i)%femdomain%mesh%nodcoord(:, :) + &
                     reshape(displacement(from:to), nn, nd)

                  from = to + 1
               end if
            end do
         end if

         if (allocated(this%leaf)) then
            do i = 1, size(this%leaf)
               if (.not. this%leaf(i)%femdomain%Mesh%empty()) then
                  nn = this%leaf(i)%femdomain%nn()
                  nd = this%leaf(i)%femdomain%nd()

                  to = from + this%leaf(i)%femdomain%nn()*this%leaf(i)%femdomain%nd() - 1

                  this%leaf(i)%femdomain%mesh%nodcoord(:, :) = &
                     this%leaf(i)%femdomain%mesh%nodcoord(:, :) + &
                     reshape(displacement(from:to), nn, nd)

                  from = to + 1
               end if
            end do
         end if

         if (allocated(this%root)) then
            do i = 1, size(this%root)
               if (.not. this%root(i)%femdomain%Mesh%empty()) then
                  nn = this%root(i)%femdomain%nn()
                  nd = this%root(i)%femdomain%nd()

                  to = from + this%root(i)%femdomain%nn()*this%root(i)%femdomain%nd() - 1

                  this%root(i)%femdomain%mesh%nodcoord(:, :) = &
                     this%root(i)%femdomain%mesh%nodcoord(:, :) + &
                     reshape(displacement(from:to), nn, nd)

                  from = to + 1
               end if
            end do
         end if
         return
      end if

      ! >> regacy >>
      if (.not. allocated(this%Stem)) then
         print *, "ERROR :: deformSoybean >> no soybean is found!"
         return
      end if
      numDomain = 0

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
            end if
         end do
      end if
      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
            end if
         end do
      end if
      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
            end if
         end do
      end if

      allocate (domainsp(numDomain))
      numDomain = 0
      stemDomain = 0
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
               stemDomain = stemDomain + 1
               domainsp(numDomain)%femdomainp => this%stem(i)%femdomain
            end if
         end do
      end if

      leafDomain = 0
      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
               leafDomain = leafDomain + 1
               domainsp(numDomain)%femdomainp => this%leaf(i)%femdomain
            end if
         end do
      end if

      rootDomain = 0
      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
               rootDomain = rootDomain + 1
               domainsp(numDomain)%femdomainp => this%root(i)%femdomain
            end if
         end do
      end if

      ! (1) create contact-list for all domains

      contactlist = zeros(numDomain, numDomain)
      if (allocated(this%stem2stem)) then
         do i = 1, stemDomain
            do j = 1, stemDomain
               contactlist(i, j) = this%stem2stem(i, j)
            end do
         end do
      end if

      if (allocated(this%leaf2stem)) then
         do i = 1, leafDomain
            do j = 1, stemDomain
               contactlist(i + stemDomain, j) = this%leaf2stem(i, j)
            end do
         end do
      end if

      if (allocated(this%root2stem)) then
         do i = 1, rootDomain
            do j = 1, stemDomain
               contactlist(i + stemDomain + leafDomain, j) = this%root2stem(i, j)
            end do
         end do
      end if

      if (allocated(this%root2root)) then
         do i = 1, rootDomain
            do j = 1, rootDomain
               contactlist(i + stemDomain + leafDomain, j + stemDomain + leafDomain) = this%root2root(i, j)
            end do
         end do
      end if

      call this%contact%init(femdomainsp=domainsp, contactlist=contactlist)

      ! load material info
      numDomain = 0
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
               call this%contact%setYoungModulus(YoungModulus=this%stemYoungModulus(i), DomainID=numDomain)
               call this%contact%setPoissonRatio(PoissonRatio=this%stemPoissonRatio(i), DomainID=numDomain)
               call this%contact%setDensity(density=this%stemDensity(i), DomainID=numDomain)
            end if
         end do
      end if
      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
               call this%contact%setYoungModulus(YoungModulus=this%leafYoungModulus(i), DomainID=numDomain)
               call this%contact%setPoissonRatio(PoissonRatio=this%leafPoissonRatio(i), DomainID=numDomain)
               call this%contact%setDensity(density=this%leafDensity(i), DomainID=numDomain)
            end if
         end do
      end if
      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%femdomain%mesh%empty()) then
               cycle
            else
               numDomain = numDomain + 1
               call this%contact%setYoungModulus(YoungModulus=this%rootYoungModulus(i), DomainID=numDomain)
               call this%contact%setPoissonRatio(PoissonRatio=this%rootPoissonRatio(i), DomainID=numDomain)
               call this%contact%setDensity(density=this%rootDensity(i), DomainID=numDomain)
            end if
         end do
      end if
      !

      penalty = input(default=1000.0d0, option=penaltyparameter)

      call this%contact%setup(penaltyparameter=penalty)

      ! if displacement is set, load displacement
      if (present(disp)) then
         do i = 1, numDomain
            call this%contact%fix(direction="x", disp=disp(1), DomainID=i, &
                                 x_min=x_min, x_max=x_max, &
                                 y_min=y_min, y_max=y_max, &
                                 z_min=z_min, z_max=z_max)
            call this%contact%fix(direction="y", disp=disp(2), DomainID=i, &
                                 x_min=x_min, x_max=x_max, &
                                 y_min=y_min, y_max=y_max, &
                                 z_min=z_min, z_max=z_max)
            call this%contact%fix(direction="z", disp=disp(3), DomainID=i, &
                                 x_min=x_min, x_max=x_max, &
                                 y_min=y_min, y_max=y_max, &
                                 z_min=z_min, z_max=z_max)
         end do
      end if

      Glevel = input(default=0.0d0, option=groundLevel)
      ! under-ground parts are fixed.
      do i = 1, numDomain
         call this%contact%fix(direction="x", disp=0.0d0, DomainID=i, &
                              z_max=Glevel)
         call this%contact%fix(direction="y", disp=0.0d0, DomainID=i, &
                              z_max=Glevel)
         call this%contact%fix(direction="z", disp=0.0d0, DomainID=i, &
                              z_max=Glevel)
      end do

      ! solve > get displacement
      call this%contact%solver%solve("BiCGSTAB")
      ! update mesh
      call this%contact%updateMesh()

   end subroutine
! #####################################################################

! #####################################################################
   function getVolumeSoybean(this, stem, leaf, root) result(ret)
      class(Soybean_), intent(in) :: this
      logical, optional, intent(in) :: stem, leaf, root
      logical :: all
      integer(int32) :: i, j
      real(real64) :: ret

      all = .false.
      if (.not. present(stem) .and. .not. present(leaf)) then
         if (.not. present(root)) then
            all = .true.
         end if
      end if

      ret = 0.0d0
      if (all) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               do j = 1, this%stem(i)%femdomain%ne()
                  ret = ret + this%stem(i)%femdomain%getVolume(elem=j)
               end do
            end if
         end do
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               do j = 1, this%leaf(i)%femdomain%ne()
                  ret = ret + this%leaf(i)%femdomain%getVolume(elem=j)
               end do
            end if
         end do
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               do j = 1, this%root(i)%femdomain%ne()
                  ret = ret + this%root(i)%femdomain%getVolume(elem=j)
               end do
            end if
         end do
         return
      end if

      if (present(stem)) then
         if (stem .or. all) then
            do i = 1, size(this%stem)
               if (.not. this%stem(i)%femdomain%mesh%empty()) then
                  do j = 1, this%stem(i)%femdomain%ne()
                     ret = ret + this%stem(i)%femdomain%getVolume(elem=j)
                  end do
               end if
            end do
         end if
      end if
      if (present(leaf)) then
         if (leaf) then
            do i = 1, size(this%leaf)
               if (.not. this%leaf(i)%femdomain%mesh%empty()) then
                  do j = 1, this%leaf(i)%femdomain%ne()
                     ret = ret + this%leaf(i)%femdomain%getVolume(elem=j)
                  end do
               end if
            end do
         end if
      end if
      if (present(root)) then
         if (root) then
            do i = 1, size(this%root)
               if (.not. this%root(i)%femdomain%mesh%empty()) then
                  do j = 1, this%root(i)%femdomain%ne()
                     ret = ret + this%root(i)%femdomain%getVolume(elem=j)
                  end do
               end if
            end do
         end if
      end if

   end function
! ############################################################################

! #####################################################################
   function getVolumePerElementSoybean(this) result(volume)
      class(Soybean_), intent(in) :: this
      integer(int32) :: i, j, elem_id
      real(real64), allocatable :: volume(:)

      elem_id = 0
      volume = zeros(this%ne())

      do i = 1, size(this%stem)
         if (.not. this%stem(i)%femdomain%mesh%empty()) then
            do j = 1, this%stem(i)%femdomain%ne()
               elem_id = elem_id + 1
               volume(elem_id) = this%stem(i)%femdomain%getVolume(elem=j)
            end do
         end if
      end do
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%mesh%empty()) then
            do j = 1, this%leaf(i)%femdomain%ne()
               elem_id = elem_id + 1
               volume(elem_id) = this%leaf(i)%femdomain%getVolume(elem=j)
            end do
         end if
      end do
      do i = 1, size(this%root)
         if (.not. this%root(i)%femdomain%mesh%empty()) then
            do j = 1, this%root(i)%femdomain%ne()
               elem_id = elem_id + 1
               volume(elem_id) = this%root(i)%femdomain%getVolume(elem=j)
            end do
         end if
      end do

   end function
! ############################################################################

! ############################################################################
   function getBiomassSoybean(this, stem, leaf, root) result(ret)
      class(Soybean_), intent(in) :: this
      logical, optional, intent(in) :: stem, leaf, root
      logical :: all
      integer(int32) :: i, j
      real(real64) :: ret, volume

      all = .false.
      if (.not. present(stem) .and. .not. present(leaf)) then
         if (.not. present(root)) then
            all = .true.
         end if
      end if

      ret = 0.0d0
      if (all) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               do j = 1, this%stem(i)%femdomain%ne()
                  volume = this%stem(i)%femdomain%getVolume(elem=j)
                  ! total = total + solid(=drydensity * volume)
                  ret = ret + volume*this%stem(i)%drydensity(j)
               end do

            end if
         end do
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               do j = 1, this%leaf(i)%femdomain%ne()
                  volume = this%leaf(i)%femdomain%getVolume(elem=j)
                  ! total = total + solid(=drydensity * volume)
                  ret = ret + volume*this%leaf(i)%drydensity(j)
               end do

            end if
         end do
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               do j = 1, this%root(i)%femdomain%ne()
                  volume = this%root(i)%femdomain%getVolume(elem=j)
                  ! total = total + solid(=drydensity * volume)
                  ret = ret + volume*this%root(i)%drydensity(j)
               end do

            end if
         end do
         return
      end if

      if (present(stem)) then
         if (stem .or. all) then
            do i = 1, size(this%stem)
               if (.not. this%stem(i)%femdomain%mesh%empty()) then
                  do j = 1, this%stem(i)%femdomain%ne()
                     volume = this%stem(i)%femdomain%getVolume(elem=j)
                     ! total = total + solid(=drydensity * volume)
                     ret = ret + volume*this%stem(i)%drydensity(j)
                  end do
               end if
            end do
         end if
      end if
      if (present(leaf)) then
         if (leaf) then
            do i = 1, size(this%leaf)
               if (.not. this%leaf(i)%femdomain%mesh%empty()) then
                  do j = 1, this%leaf(i)%femdomain%ne()
                     volume = this%leaf(i)%femdomain%getVolume(elem=j)
                     ! total = total + solid(=drydensity * volume)
                     ret = ret + volume*this%leaf(i)%drydensity(j)
                  end do
               end if
            end do
         end if
      end if
      if (present(root)) then
         if (root) then
            do i = 1, size(this%root)
               if (.not. this%root(i)%femdomain%mesh%empty()) then
                  do j = 1, this%root(i)%femdomain%ne()
                     volume = this%root(i)%femdomain%getVolume(elem=j)
                     ! total = total + solid(=drydensity * volume)
                     ret = ret + volume*this%root(i)%drydensity(j)
                  end do
               end if
            end do
         end if
      end if

   end function

! ############################################################################
   function getElementBiomassSoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: i, j, itr
      real(real64), allocatable :: ret(:)
      real(real64) :: volume

      ret = zeros(this%ne())

      itr = 0
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%femdomain%mesh%empty()) then
            do j = 1, this%stem(i)%femdomain%ne()
               volume = this%stem(i)%femdomain%getVolume(elem=j)
               ! total = total + solid(=drydensity * volume)
               itr = itr + 1
               ret(itr) = volume*this%stem(i)%drydensity(j)
            end do

         end if
      end do
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%mesh%empty()) then
            do j = 1, this%leaf(i)%femdomain%ne()
               volume = this%leaf(i)%femdomain%getVolume(elem=j)
               ! total = total + solid(=drydensity * volume)
               itr = itr + 1
               ret(itr) = volume*this%leaf(i)%drydensity(j)
            end do

         end if
      end do
      do i = 1, size(this%root)
         if (.not. this%root(i)%femdomain%mesh%empty()) then
            do j = 1, this%root(i)%femdomain%ne()
               volume = this%root(i)%femdomain%getVolume(elem=j)
               ! total = total + solid(=drydensity * volume)
               itr = itr + 1
               ret(itr) = volume*this%root(i)%drydensity(j)
            end do

         end if
      end do

   end function

   function getTotalWeightSoybean(this, stem, leaf, root, waterDensity) result(ret)
      class(Soybean_), intent(in) :: this
      logical, optional, intent(in) :: stem, leaf, root
      real(real64), optional, intent(in) :: waterDensity
      logical :: all
      integer(int32) :: i, j
      real(real64) :: ret, volume, water_density

      ! kg, m
      water_density = input(default=1000.0d0, option=waterDensity)

      all = .false.
      if (.not. present(stem) .and. .not. present(leaf)) then
         if (.not. present(root)) then
            all = .true.
         end if
      end if

      ret = 0.0d0
      if (all) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               do j = 1, this%stem(i)%femdomain%ne()
                  volume = this%stem(i)%femdomain%getVolume(elem=j)
                  ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                  ret = ret + volume*this%stem(i)%drydensity(j) + volume*this%stem(i)%watercontent(j)*water_density
               end do

            end if
         end do
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               do j = 1, this%leaf(i)%femdomain%ne()
                  volume = this%leaf(i)%femdomain%getVolume(elem=j)
                  ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                  ret = ret + volume*this%leaf(i)%drydensity(j) + volume*this%leaf(i)%watercontent(j)*water_density
               end do

            end if
         end do
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               do j = 1, this%root(i)%femdomain%ne()
                  volume = this%root(i)%femdomain%getVolume(elem=j)
                  ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                  ret = ret + volume*this%root(i)%drydensity(j) + volume*this%root(i)%watercontent(j)*water_density
               end do

            end if
         end do
         return
      end if

      if (present(stem)) then
         if (stem .or. all) then
            do i = 1, size(this%stem)
               if (.not. this%stem(i)%femdomain%mesh%empty()) then
                  do j = 1, this%stem(i)%femdomain%ne()
                     volume = this%stem(i)%femdomain%getVolume(elem=j)
                     ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                     ret = ret + volume*this%stem(i)%drydensity(j) + volume*this%stem(i)%watercontent(j)*water_density
                  end do
               end if
            end do
         end if
      end if
      if (present(leaf)) then
         if (leaf) then
            do i = 1, size(this%leaf)
               if (.not. this%leaf(i)%femdomain%mesh%empty()) then
                  do j = 1, this%leaf(i)%femdomain%ne()
                     volume = this%leaf(i)%femdomain%getVolume(elem=j)
                     ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                     ret = ret + volume*this%leaf(i)%drydensity(j) + volume*this%leaf(i)%watercontent(j)*water_density
                  end do
               end if
            end do
         end if
      end if
      if (present(root)) then
         if (root) then
            do i = 1, size(this%root)
               if (.not. this%root(i)%femdomain%mesh%empty()) then
                  do j = 1, this%root(i)%femdomain%ne()
                     volume = this%root(i)%femdomain%getVolume(elem=j)
                     ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                     ret = ret + volume*this%root(i)%drydensity(j) + volume*this%root(i)%watercontent(j)*water_density
                  end do
               end if
            end do
         end if
      end if

   end function

!function getBioMassSoybean(this,stemDensity,leafDensity,rootDensity) result(ret)
!    class(Soybean_),intent(in) :: this
!    real(real64),optional,intent(in) :: stemDensity,leafDensity,rootDensity
!    logical :: all
!    integer(int32) :: i,j
!    real(real64) :: ret
!
!    ret = 0.0d0
!
!    if(present(stemDensity))then
!        ret = ret + this%getVolume(stem=.true.) * stemDensity
!    endif
!
!    if(present(leafDensity))then
!        ret = ret + this%getVolume(leaf=.true.) * leafDensity
!    endif
!
!    if(present(rootDensity))then
!        ret = ret + this%getVolume(root=.true.) * rootDensity
!    endif
!
!
!
!end function
   subroutine fall_leafSoybean(this, BranchID, InterNodeID, with_petiole)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: BranchID, InterNodeID
      logical, optional, intent(in) :: with_petiole
      integer(int32) :: i, j, stemID
      integer(int32), allocatable :: petioleIDs(:)

      ! fall leaves
      do i = 1, size(this%stem)
         if (this%stem(i)%empty()) cycle
         if (this%stem(i)%stemID == branchID .and. &
             this%stem(i)%InterNodeID == InterNodeID) then
            stemID = i
         end if
      end do

      allocate (petioleIDs(0))
      do i = 1, size(this%stem2stem, 1)
         ! stem id i -> stemID
         if (this%stem2stem(i, StemID) >= 1 .and. this%stem(i)%InterNodeID < 1) then
            ! petiole
            petioleIDs = petioleIDs//[i]
         end if
      end do

      print *, "petioleIDs", petioleIDs
      ! remove leaves
      do i = 1, size(petioleIDs)
         do j = 1, size(this%leaf2stem, 1)
            if (this%leaf2stem(j, petioleIDs(i)) /= 0) then
               this%leaf2stem(j, petioleIDs(i)) = 0
               call this%leaf(j)%remove()
            end if
         end do
      end do

      if (present(with_petiole)) then
         if (with_petiole) then
            do i = 1, size(petioleIDs)
               this%stem2stem(petioleIDs(i), :) = 0
               call this%stem(petioleIDs(i))%remove()
            end do
         end if
      end if

   end subroutine

   subroutine removeSoybean(this, root)
      class(Soybean_), intent(inout) :: this
      logical, optional, intent(in) :: root

      if (present(root)) then
         if (root) then

            this%mr_node = 0
            this%brr_node(:) = 0
            this%brr_from(:) = 0
            this%mr_length = 0.0d0
            this%brr_length(:) = 0.0d0
            this%mr_width = 0.0d0
            this%brr_width(:) = 0.0d0
            this%mr_angle_ave = 0.0d0
            this%brr_angle_ave(:) = 0.0d0
            this%mr_angle_sig = 0.0d0
            this%brr_angle_sig(:) = 0.0d0
            if (allocated(this%RootSystem)) deallocate (this%RootSystem)
            if (allocated(this%Root)) deallocate (this%Root)
            if (allocated(this%rootYoungModulus)) deallocate (this%rootYoungModulus)
            if (allocated(this%rootPoissonRatio)) deallocate (this%rootPoissonRatio)
            if (allocated(this%rootDensity)) deallocate (this%rootDensity)

            if (allocated(this%root2stem)) deallocate (this%root2stem)
            if (allocated(this%root2root)) deallocate (this%root2root)
            if (allocated(this%root_list)) deallocate (this%root_list)

            if (allocated(this%root_angle)) deallocate (this%root_angle)
            this%rootconfig = " "
            this%Num_Of_Root = 0

         end if
         return
      end if

      this%growth_habit = " "
      this%growth_stage = " "
      this%Num_Of_Node = 0
      this%Num_Of_Root = 0

      this%MaxLeafNum = 300
      this%MaxRootNum = 300
      this%MaxStemNum = 300

      this%ms_node = 0
      this%br_node(:) = 0
      this%br_from(:) = 0
      this%ms_length = 0.0d0
      this%br_length(:) = 0.0d0
      this%ms_width = 0.0d0
      this%br_width(:) = 0.0d0
      this%ms_angle_ave = 0.0d0
      this%br_angle_ave(:) = 0.0d0
      this%ms_angle_sig = 0.0d0
      this%br_angle_sig(:) = 0.0d0

      this%mr_node = 0
      this%brr_node(:) = 0
      this%brr_from(:) = 0
      this%mr_length = 0.0d0
      this%brr_length(:) = 0.0d0
      this%mr_width = 0.0d0
      this%brr_width(:) = 0.0d0
      this%mr_angle_ave = 0.0d0
      this%brr_angle_ave(:) = 0.0d0
      this%mr_angle_sig = 0.0d0
      this%brr_angle_sig(:) = 0.0d0

      this%peti_size_ave(:) = 0.0d0
      this%peti_size_sig(:) = 0.0d0
      this%peti_width_ave(:) = 0.0d0
      this%peti_width_sig(:) = 0.0d0
      this%peti_angle_ave(:) = 0.0d0
      this%peti_angle_sig(:) = 0.0d0

      this%leaf_angle_ave(:) = 0.0d0
      this%leaf_angle_sig(:) = 0.0d0
      this%leaf_length_ave(:) = 0.0d0
      this%leaf_length_sig(:) = 0.0d0
      this%leaf_width_ave(:) = 0.0d0
      this%leaf_width_sig(:) = 0.0d0
      this%leaf_thickness_ave(:) = 0.0d0
      this%leaf_thickness_sig(:) = 0.0d0

      this%Stage = "" ! VE, CV, V1,V2, ..., R1, R2, ..., R8
      this%name = ""
      this%stage_id = 0
      this%dt = 0.0d0
      call this%Seed%remove()
      if (allocated(this%NodeSystem)) deallocate (this%NodeSystem)
      if (allocated(this%RootSystem)) deallocate (this%RootSystem)

      if (allocated(this%Stem)) deallocate (this%Stem)
      if (allocated(this%Leaf)) deallocate (this%Leaf)
      if (allocated(this%Root)) deallocate (this%Root)

      ! material info
      if (allocated(this%stemYoungModulus)) deallocate (this%stemYoungModulus)
      if (allocated(this%leafYoungModulus)) deallocate (this%leafYoungModulus)
      if (allocated(this%rootYoungModulus)) deallocate (this%rootYoungModulus)

      if (allocated(this%stemPoissonRatio)) deallocate (this%stemPoissonRatio)
      if (allocated(this%leafPoissonRatio)) deallocate (this%leafPoissonRatio)
      if (allocated(this%rootPoissonRatio)) deallocate (this%rootPoissonRatio)

      if (allocated(this%stemDensity)) deallocate (this%stemDensity)
      if (allocated(this%leafDensity)) deallocate (this%leafDensity)
      if (allocated(this%rootDensity)) deallocate (this%rootDensity)

      if (allocated(this%NodeID_MainStem)) deallocate (this%NodeID_MainStem)
      if (allocated(this%NodeID_Branch)) deallocate (this%NodeID_Branch)
      ! 節-節点データ構造
      call this%struct%remove(all=.true.)
      if (allocated(this%leaf2stem)) deallocate (this%leaf2stem)
      if (allocated(this%stem2stem)) deallocate (this%stem2stem)
      if (allocated(this%root2stem)) deallocate (this%root2stem)
      if (allocated(this%root2root)) deallocate (this%root2root)

      ! 器官オブジェクト配列
      if (allocated(this%leaf_list)) deallocate (this%leaf_list)
      if (allocated(this%stem_list)) deallocate (this%stem_list)
      if (allocated(this%root_list)) deallocate (this%root_list)

      ! シミュレータ
      call this%contact%remove()
      this%time = 0.0d0
      this%seed_length = 0.0d0
      this%seed_width = 0.0d0
      this%seed_height = 0.0d0
      if (allocated(this%stem_angle)) deallocate (this%stem_angle)
      if (allocated(this%root_angle)) deallocate (this%root_angle)
      if (allocated(this%leaf_angle)) deallocate (this%leaf_angle)

      this%stemconfig = " "
      this%rootconfig = " "
      this%leafconfig = " "

   end subroutine

   function stemlengthSoybean(this, StemID) result(ret)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: StemID ! 0, 1, 2...
      integer(int32) :: num_snode, i
      real(real64), allocatable :: ret(:)

      if (StemID == 0) then
         ! main stem
         num_snode = size(this%NodeID_MainStem)
         allocate (ret(num_snode))
         do i = 1, num_snode
            ret(i) = this%stem(this%NodeID_MainStem(i))%getLength()
         end do
      else
         if (StemID >= size(this%NodeID_Branch)) then
            print *, "ERROR :: stemlengthSoybean >> StemID >=size(this%NodeID_Branch)"
            ret = zeros(1)
            return
         end if
         ! main stem
         num_snode = size(this%NodeID_Branch(StemID)%ID)
         allocate (ret(num_snode))
         do i = 1, num_snode
            ret(i) = this%stem(this%NodeID_Branch(StemID)%ID(i))%getLength()
         end do
      end if

   end function
! ###################################################################

! object editor

! rotateStem
! rotateRoot
! rotateLeaf

! resizeStem(MainStem)
! resizeRoot
! resizeLeaf

! ###################################################################
   subroutine resizeSoybean(this, StemID, StemLength)
      class(Soybean_), intent(inout) :: this
      integer(int32), optional, intent(in) :: StemID
      real(real64), optional, intent(in) :: StemLength(:)
      integer(int32) :: num_snode, i

      if (present(StemID)) then
         if (.not. present(StemLength)) then
            print *, "ERROR :: resizeSoybean >> needs StemLength(:) "
            stop
         end if

         if (StemID == 0) then
            ! main stem
            num_snode = size(this%NodeID_MainStem)

            do i = 1, num_snode
               call this%stem(this%NodeID_MainStem(i))%change_length_or_width(length=StemLength(i))
            end do
         else
            if (StemID >= size(this%NodeID_Branch)) then
               print *, "ERROR :: resizeSoybean >> StemID >=size(this%NodeID_Branch)"

               return
            end if
            ! main stem
            num_snode = size(this%NodeID_Branch(StemID)%ID)

            do i = 1, num_snode
               call this%stem(this%NodeID_Branch(StemID)%ID(i))%change_length_or_width(length=StemLength(i))
            end do
         end if
         call this%update()

      end if
   end subroutine
! ###################################################################

! ###################################################################
   function NumberOfBranchSoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: ret, i

      ret = 0
      if (allocated(this%NodeID_Branch)) then
         do i = 1, size(this%NodeID_Branch)
            if (allocated(this%NodeID_Branch(i)%ID)) then
               if (size(this%NodeID_Branch(i)%ID) >= 1) then
                  ret = ret + 1
               end if
            end if
         end do
      end if
   end function
! ###################################################################

! ###################################################################
   function findApicalSoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), allocatable :: ret(:)
      !integer(int32),optional,intent(in) :: StemID
      integer(int32), allocatable :: stem
      integer(int32) :: i, j, itr

      ret = zeros(this%NumberOfBranch() + 1)

      ret(1) = maxval(this%NodeID_MainStem(:))

      itr = 1
      do i = 1, this%NumberOfBranch()
         if (allocated(this%NodeID_Branch(i)%ID)) then
            itr = itr + 1
            ret(itr) = maxval(this%NodeID_Branch(i)%ID(:))
         end if
      end do

!    if(present(StemID) )then
!        if(StemID > size(this%br_node) )then
!            print *, "ERROR >> findApicalSoybean >> number of branch is ",size(this%br_node)
!            print *, "StemID=",StemID,"is larger than it."
!            return
!        endif
!
!
!        return
!    endif

   end function
! ###################################################################

!function propertiesSoybean(this) result(ret)
!    class(Soybean_) ,intent(in) :: this
!
!
!end function

! ##################################################################
   pure function nnSoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: ret, i

      ! get number of node (point)
      ret = 0

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               ret = ret + this%stem(i)%femdomain%nn()
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               ret = ret + this%leaf(i)%femdomain%nn()
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               ret = ret + this%root(i)%femdomain%nn()
            end if
         end do
      end if

   end function
! ##################################################################

! ##################################################################
   function neSoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: ret, i

      ! get number of element
      ret = 0
      if(allocated(this%stem))then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               ret = ret + this%stem(i)%femdomain%ne()
            end if
         end do
      end if
      if(allocated(this%leaf))then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               ret = ret + this%leaf(i)%femdomain%ne()
            end if
         end do
      end if
      if(allocated(this%root))then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               ret = ret + this%root(i)%femdomain%ne()
            end if
         end do
      end if

   end function
! ##################################################################

! ##################################################################
   function nsSoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: ret, i

      ! get number of subdomain
      ret = 0
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%femdomain%mesh%empty()) then
            ret = ret + 1
         end if
      end do
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%mesh%empty()) then
            ret = ret + 1
         end if
      end do
      do i = 1, size(this%root)
         if (.not. this%root(i)%femdomain%mesh%empty()) then
            ret = ret + 1
         end if
      end do

   end function

! ##################################################################

   function getSubDomainSoybean(this, id) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: id
      type(FEMDomain_) :: ret
      integer(int32) :: i, ret_id

      ! get number of subdomain
      ret_id = 0
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%femdomain%mesh%empty()) then
            ret_id = ret_id + 1
            if (id == ret_id) then
               ret = this%stem(i)%femdomain
               return
            end if
         end if
      end do
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%mesh%empty()) then
            ret_id = ret_id + 1
            if (id == ret_id) then
               ret = this%stem(i)%femdomain
               return
            end if
         end if
      end do
      do i = 1, size(this%root)
         if (.not. this%root(i)%femdomain%mesh%empty()) then
            ret_id = ret_id + 1
            if (id == ret_id) then
               ret = this%stem(i)%femdomain
               return
            end if
         end if
      end do

      print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains", ret_id
      return

   end function
! ##################################################################

! ##################################################################

   subroutine setSubDomainSoybean(this, domain, id)
      class(Soybean_), intent(inout) :: this
      type(FEMDomain_), intent(in) :: domain
      integer(int32), intent(in) :: id
      integer(int32) :: i, domain_id

      ! get number of subdomain
      domain_id = 0
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%femdomain%mesh%empty()) then
            domain_id = domain_id + 1
            if (id == domain_id) then
               this%stem(i)%femdomain = domain
               return
            end if
         end if
      end do
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%mesh%empty()) then
            domain_id = domain_id + 1
            if (id == domain_id) then
               this%stem(i)%femdomain = domain
               return
            end if
         end if
      end do
      do i = 1, size(this%root)
         if (.not. this%root(i)%femdomain%mesh%empty()) then
            domain_id = domain_id + 1
            if (id == domain_id) then
               this%stem(i)%femdomain = domain
               return
            end if
         end if
      end do

      print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains", domain_id
      return

   end subroutine
! ##################################################################

! ##################################################################

   function getSubDomainTypeSoybean(this, id) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: id
      character(:), allocatable :: ret
      integer(int32) :: i, ret_id

      ! get number of subdomain
      ret_id = 0
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%femdomain%mesh%empty()) then
            ret_id = ret_id + 1
            if (id == ret_id) then
               ret = "stem"
               return
            end if
         end if
      end do
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%mesh%empty()) then
            ret_id = ret_id + 1
            if (id == ret_id) then
               ret = "leaf"
               return
            end if
         end if
      end do
      do i = 1, size(this%root)
         if (.not. this%root(i)%femdomain%mesh%empty()) then
            ret_id = ret_id + 1
            if (id == ret_id) then
               ret = "root"
               return
            end if
         end if
      end do

      print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains", ret_id
      return

   end function
! ##################################################################

! ##################################################################
   pure function isMainStemSoybean(this, StemNodeID) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in)  :: StemNodeID
      logical :: ret

      ret = exists(vector=this%NodeID_MainStem, val=StemNodeID)

   end function
! ##################################################################

! ##################################################################
   pure function isBranchStemSoybean(this, StemNodeID) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in)  :: StemNodeID
      logical :: ret

      if (this%branchID(StemNodeID) == 0) then
         ret = .False.
      else
         ret = .True.
      end if

   end function
! ##################################################################

   pure function branchIDSoybean(this, StemNodeID) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in)  :: StemNodeID
      integer(int32), allocatable :: ret
      integer(int32) :: i, j, k, l, m, n, ret_id

      do i = 1, size(this%NodeID_Branch)
         if (exist(this%NodeID_Branch(i)%ID(:), StemNodeID)) then
            ret = i
            return
         end if
      end do
      ret = 0

   end function
! ##################################################################

   subroutine checkPropertiesSoybean(this, Simulator)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in)  ::  Simulator
      type(Time_) :: time
      type(IO_) :: f

      call f%open("__soybeanclass__checkPropertiesSoybean.log")
      if (Simulator == PF_DEFORMATION_ANALYSIS) then
         call print("---------------------------------------")
         call print("-- checkProperties @ SoybeanClass  ----")
         call print("---------------------------------------")
         call print(" Simulator mode :: Deformation analysis")
         call print("---------------------------------------")
         call print("Date and time: "//time%DateAndTime())
         call print("---------------------------------------")
         call print("Checking datasets for deformation analysis...")
         ! check if it ready or not.
         print *, "property_deform_material_density       |", &
            this%property_deform_material_density
         print *, "property_deform_material_YoungModulus  |", &
            this%property_deform_material_YoungModulus
         print *, "property_deform_material_CarbonDiffusionCoefficient|", &
            this%property_deform_material_CarbonDiffusionCoefficient
         print *, "property_deform_material_PoissonRatio  |", &
            this%property_deform_material_PoissonRatio
         print *, "property_deform_initial_Displacement   |", &
            this%property_deform_initial_Displacement
         print *, "property_deform_initial_Stress         |", &
            this%property_deform_initial_Stress
         print *, "property_deform_boundary_TractionForce |", &
            this%property_deform_boundary_TractionForce
         print *, "property_deform_boundary_Displacement  |", &
            this%property_deform_boundary_Displacement
         print *, "property_deform_gravity                |", &
            this%property_deform_gravity
         call print("---------------------------------------")
         ! >>>> export to log
         call f%write("---------------------------------------")
         call f%write("-- checkProperties @ SoybeanClass  ----")
         call f%write("---------------------------------------")
         call f%write(" Simulator mode :: Deformation analysis")
         call f%write("---------------------------------------")
         call f%write("Date and time: "//time%DateAndTime())
         call f%write("---------------------------------------")
         call f%write("Checking datasets for deformation analysis...")
         ! check if it ready or not.
         call f%write("property_deform_material_density       |"// &
                      str(this%property_deform_material_density))
         call f%write("property_deform_material_YoungModulus  |"// &
                      str(this%property_deform_material_YoungModulus))
         call f%write("property_deform_material_CarbonDiffusionCoefficient  |"// &
                      str(this%property_deform_material_CarbonDiffusionCoefficient))
         call f%write("property_deform_material_PoissonRatio  |"// &
                      str(this%property_deform_material_PoissonRatio))
         call f%write("property_deform_initial_Displacement   |"// &
                      str(this%property_deform_initial_Displacement))
         call f%write("property_deform_initial_Stress         |"// &
                      str(this%property_deform_initial_Stress))
         call f%write("property_deform_boundary_TractionForce |"// &
                      str(this%property_deform_boundary_TractionForce))
         call f%write("property_deform_boundary_Displacement  |"// &
                      str(this%property_deform_boundary_Displacement))
         call f%write("property_deform_gravity                |"// &
                      str(this%property_deform_gravity))
         call f%write("---------------------------------------")
      else
         call print("Invalid  Simulator ID :: "//str(Simulator))

      end if
      call f%close()

   end subroutine
! ##################################################################
! ##################################################################
   subroutine setPropertiesDensitySoybean(this)
      class(Soybean_), intent(inout) :: this
      integer(int32) :: i, j

      ! default == false
      this%property_deform_material_density = .false.

      ! check
      ! Does stem/leaf/root have Density for each element?
      if (allocated(this%leaf)) then
         print *, "[ok] setPropertiesSoybean >> leaf exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               ! exists
               ! check
               !  (1) allocation of density(:)
               !  (2) size of density(:)
               !  if invalid, compute from drydensity(:)and watercontent(:)
               !  if not both do not exists, create all as 0.0
               if (allocated(this%leaf(i)%density)) then
                  ! check size

                  !if(size(this%leaf(i)%density)/=this%leaf(i)%femdomain%ne() )then
                  !print *, "[Caution] setPropertiesSoybean >> stem("//str(i)//")%density >> "//&
                  !"size(this%leaf(i)%density)/=this%leaf(i)%femdomain%ne() >> reset by zero!!"
                  !deallocate(this%leaf(i)%density)
                  ! let's go to next
                  !else
                  print *, "[ok] setPropertiesSoybean &
&                        >> leaf("//str(i)//")%density >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_density = .true.

                  !endif
               else
                  ! density is not allocated.

                  this%leaf(i)%density = zeros(this%leaf(i)%femdomain%ne())

                  ! >> try to compute from drydensity(:) and watercontent(:)
                  ! >> check existatce of drydensity(:) and watercontent(:)
                  if (.not. allocated(this%leaf(i)%drydensity)) then
                     this%leaf(i)%drydensity = zeros(this%leaf(i)%femdomain%ne())
                  end if
                  if (.not. allocated(this%leaf(i)%watercontent)) then
                     this%leaf(i)%watercontent = zeros(this%leaf(i)%femdomain%ne())
                  end if

                  if (size(this%leaf(i)%drydensity) /= this%leaf(i)%femdomain%ne()) then
                     this%leaf(i)%drydensity = zeros(this%leaf(i)%femdomain%ne())
                  end if
                  if (size(this%leaf(i)%watercontent) /= this%leaf(i)%femdomain%ne()) then
                     this%leaf(i)%watercontent = zeros(this%leaf(i)%femdomain%ne())
                  end if

                  ! compute density from drydensity and water content
                  ! \rho_t = \rho_d * (1 - w )
                    !!$OMP parallel do private(j)
                  do j = 1, this%leaf(i)%femdomain%ne()
                     this%leaf(i)%density(j) = this%leaf(i)%drydensity(j)*(1.0d0 - this%leaf(i)%watercontent(j))
                  end do
                    !!$OMP end parallel do
               end if
            end if
         end do
        !!$OMP end parallel do
         this%property_deform_material_density = .true.
      else
         print *, "[Notice] setPropertiesSoybean >> no leaf"
      end if
    !! stem
      if (allocated(this%stem)) then
         print *, "[ok] setPropertiesSoybean >> stems exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               ! exists
               ! check
               !  (1) allocation of density(:)
               !  (2) size of density(:)
               !  if invalid, compute from drydensity(:)and watercontent(:)
               !  if not both do not exists, create all as 0.0
               if (allocated(this%stem(i)%density)) then
                  ! check size

                  !if(size(this%stem(i)%density)/=this%stem(i)%femdomain%ne() )then
                  !print *, "[Caution] setPropertiesSoybean >> stem("//str(i)//")%density >> "//&
                  !"size(this%stem(i)%density)/=this%stem(i)%femdomain%ne() >> reset by zero!!"
                  !deallocate(this%stem(i)%density)
                  ! let's go to next
                  !else
                  print *, "[ok] setPropertiesSoybean >> stem("//str(i)//")%density >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_density = .true.

                  !endif
               else
                  ! density is not allocated.

                  this%stem(i)%density = zeros(this%stem(i)%femdomain%ne())

                  ! >> try to compute from drydensity(:) and watercontent(:)
                  ! >> check existatce of drydensity(:) and watercontent(:)
                  if (.not. allocated(this%stem(i)%drydensity)) then
                     this%stem(i)%drydensity = zeros(this%stem(i)%femdomain%ne())
                  end if
                  if (.not. allocated(this%stem(i)%watercontent)) then
                     this%stem(i)%watercontent = zeros(this%stem(i)%femdomain%ne())
                  end if

                  if (size(this%stem(i)%drydensity) /= this%stem(i)%femdomain%ne()) then
                     this%stem(i)%drydensity = zeros(this%stem(i)%femdomain%ne())
                  end if
                  if (size(this%stem(i)%watercontent) /= this%stem(i)%femdomain%ne()) then
                     this%stem(i)%watercontent = zeros(this%stem(i)%femdomain%ne())
                  end if

                  ! compute density from drydensity and water content
                  ! \rho_t = \rho_d * (1 - w )
                    !!$OMP parallel do private(j)
                  do j = 1, this%stem(i)%femdomain%ne()
                     this%stem(i)%density(j) = this%stem(i)%drydensity(j)*(1.0d0 - this%stem(i)%watercontent(j))
                  end do
                    !!$OMP end parallel do
               end if
            end if
         end do
        !!$OMP end parallel do
         this%property_deform_material_density = .true.
      else
         print *, "[Notice] setPropertiesSoybean >> no stems"
      end if
    !! root
      if (allocated(this%root)) then
         print *, "[ok] setPropertiesSoybean >> roots exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               ! exists
               ! check
               !  (1) allocation of density(:)
               !  (2) size of density(:)
               !  if invalid, compute from drydensity(:)and watercontent(:)
               !  if not both do not exists, create all as 0.0
               if (allocated(this%root(i)%density)) then
                  ! check size

                  !if(size(this%root(i)%density)/=this%root(i)%femdomain%ne() )then
                  !print *, "[Caution] setPropertiesSoybean >> root("//str(i)//")%density >> "//&
                  !"size(this%root(i)%density)/=this%root(i)%femdomain%ne() >> reset by zero!!"
                  !deallocate(this%root(i)%density)
                  ! let's go to next
                  !else
                  print *, "[ok] setPropertiesSoybean >> root("//str(i)//")%density >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_density = .true.

                  !endif
               else
                  ! density is not allocated.

                  this%root(i)%density = zeros(this%root(i)%femdomain%ne())

                  ! >> try to compute from drydensity(:) and watercontent(:)
                  ! >> check existatce of drydensity(:) and watercontent(:)
                  if (.not. allocated(this%root(i)%drydensity)) then
                     this%root(i)%drydensity = zeros(this%root(i)%femdomain%ne())
                  end if
                  if (.not. allocated(this%root(i)%watercontent)) then
                     this%root(i)%watercontent = zeros(this%root(i)%femdomain%ne())
                  end if

                  if (size(this%root(i)%drydensity) /= this%root(i)%femdomain%ne()) then
                     this%root(i)%drydensity = zeros(this%root(i)%femdomain%ne())
                  end if
                  if (size(this%root(i)%watercontent) /= this%root(i)%femdomain%ne()) then
                     this%root(i)%watercontent = zeros(this%root(i)%femdomain%ne())
                  end if

                  ! compute density from drydensity and water content
                  ! \rho_t = \rho_d * (1 - w )
                    !!$OMP parallel do private(j)
                  do j = 1, this%root(i)%femdomain%ne()
                     this%root(i)%density(j) = this%root(i)%drydensity(j)*(1.0d0 - this%root(i)%watercontent(j))
                  end do
                    !!$OMP end parallel do
               end if
            end if
         end do
        !!$OMP end parallel do
         this%property_deform_material_density = .true.
      else
         print *, "[Notice] setPropertiesSoybean >> no roots"
      end if

   end subroutine
! ##################################################################
   subroutine setPropertiesYoungModulusSoybean(this, default_value)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: default_value
      real(real64) :: defval
      integer(int32) :: i, j

      defval = input(default=0.0d0, option=default_value)

      if (allocated(this%stem)) then
         print *, "[ok] setPropertiesYoungModulusSoybean >> stems exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if stem(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%stem(i)%youngmodulus)) then

                  print *, "[ok] setPropertiesYoungModulusSoybean >> stem("//str(i)//")%youngmodulus >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_youngmodulus = .true.

               else

                  ! youngmodulus is not allocated.

                  this%stem(i)%youngmodulus = zeros(this%stem(i)%femdomain%ne())
                  this%stem(i)%youngmodulus = defval
               end if
            end if
         end do
      end if

      ! same as this
      if (allocated(this%root)) then
         print *, "[ok] setPropertiesYoungModulusSoybean >> roots exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if root(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%root(i)%youngmodulus)) then

                  print *, "[ok] setPropertiesYoungModulusSoybean >> root("//str(i)//")%youngmodulus >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_youngmodulus = .true.

               else

                  ! youngmodulus is not allocated.

                  this%root(i)%youngmodulus = zeros(this%root(i)%femdomain%ne())
                  this%root(i)%youngmodulus = defval
               end if
            end if
         end do
      end if

      ! same for leaf
      if (allocated(this%leaf)) then
         print *, "[ok] setPropertiesYoungModulusSoybean >> leafs exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if leaf(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%leaf(i)%youngmodulus)) then

                  print *, "[ok] setPropertiesYoungModulusSoybean &
&                    >> leaf("//str(i)//")%youngmodulus >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_youngmodulus = .true.

               else
                  ! youngmodulus is not allocated.

                  this%leaf(i)%youngmodulus = zeros(this%leaf(i)%femdomain%ne())
                  this%leaf(i)%youngmodulus = defval
               end if
            end if
         end do
      end if
      this%property_deform_material_youngmodulus = .true.

   end subroutine
! ##################################################################

! ##################################################################
   subroutine setPropertiesCarbonDiffusionCoefficientSoybean(this, default_value)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: default_value
      real(real64) :: defval
      integer(int32) :: i, j

      defval = input(default=0.0d0, option=default_value)

      if (allocated(this%stem)) then
         print *, "[ok] setPropertiesCarbonDiffusionCoefficientSoybean >> stems exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if stem(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%stem(i)%CarbonDiffusionCoefficient)) then

                  print *, "[ok] setPropertiesCarbonDiffusionCoefficientSoybean >> &
&                    stem("//str(i)//")%CarbonDiffusionCoefficient >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_CarbonDiffusionCoefficient = .true.

               else

                  ! CarbonDiffusionCoefficient is not allocated.

                  this%stem(i)%CarbonDiffusionCoefficient = zeros(this%stem(i)%femdomain%ne())
                  this%stem(i)%CarbonDiffusionCoefficient = defval
               end if
            end if
         end do
      end if

      ! same as this
      if (allocated(this%root)) then
         print *, "[ok] setPropertiesCarbonDiffusionCoefficientSoybean >> roots exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if root(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%root(i)%CarbonDiffusionCoefficient)) then

                  print *, "[ok] setPropertiesCarbonDiffusionCoefficientSoybean &
&                    >> root("//str(i)//")%CarbonDiffusionCoefficient >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_CarbonDiffusionCoefficient = .true.

               else

                  ! CarbonDiffusionCoefficient is not allocated.

                  this%root(i)%CarbonDiffusionCoefficient = zeros(this%root(i)%femdomain%ne())
                  this%root(i)%CarbonDiffusionCoefficient = defval
               end if
            end if
         end do
      end if

      ! same for leaf
      if (allocated(this%leaf)) then
         print *, "[ok] setPropertiesCarbonDiffusionCoefficientSoybean >> leafs exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if leaf(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%leaf(i)%CarbonDiffusionCoefficient)) then

                  print *, "[ok] setPropertiesCarbonDiffusionCoefficientSoybean &
&                    >> leaf("//str(i)//")%CarbonDiffusionCoefficient >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_CarbonDiffusionCoefficient = .true.

               else
                  ! CarbonDiffusionCoefficient is not allocated.

                  this%leaf(i)%CarbonDiffusionCoefficient = zeros(this%leaf(i)%femdomain%ne())
                  this%leaf(i)%CarbonDiffusionCoefficient = defval
               end if
            end if
         end do
      end if
      this%property_deform_material_CarbonDiffusionCoefficient = .true.

   end subroutine
! ##################################################################

! ##################################################################
!same for poissonratio
   subroutine setPropertiesPoissonRatioSoybean(this, default_value)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: default_value
      real(real64) :: defval
      integer(int32) :: i, j

      defval = input(default=0.0d0, option=default_value)

      if (allocated(this%stem)) then
         print *, "[ok] setPropertiesPoissonRatioSoybean >> stems exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if stem(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%stem(i)%poissonratio)) then

                  print *, "[ok] setPropertiesPoissonRatioSoybean >> stem("//str(i)//")%poissonratio >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_poissonratio = .true.

               else

                  ! youngmodulus is not allocated.

                  this%stem(i)%poissonratio = zeros(this%stem(i)%femdomain%ne())
                  this%stem(i)%poissonratio = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if

      ! same for leaf
      if (allocated(this%leaf)) then
         print *, "[ok] setPropertiesPoissonRatioSoybean >> leafs exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if leaf(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%leaf(i)%poissonratio)) then

                  print *, "[ok] setPropertiesPoissonRatioSoybean >> leaf("//str(i)//")%poissonratio >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_poissonratio = .true.

               else
                  ! youngmodulus is not allocated.

                  this%leaf(i)%poissonratio = zeros(this%leaf(i)%femdomain%ne())
                  this%leaf(i)%poissonratio = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if

      ! same for root
      if (allocated(this%root)) then
         print *, "[ok] setPropertiesPoissonRatioSoybean >> roots exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if root(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%root(i)%poissonratio)) then

                  print *, "[ok] setPropertiesPoissonRatioSoybean >> root("//str(i)//")%poissonratio >> allocated"
                  ! then ok. let's return
                  this%property_deform_material_poissonratio = .true.

               else

                  ! youngmodulus is not allocated.

                  this%root(i)%poissonratio = zeros(this%root(i)%femdomain%ne())
                  this%root(i)%poissonratio = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if
      this%property_deform_material_poissonratio = .true.
   end subroutine

! ##################################################################
! similar subroutine for Initialdisplacement
   subroutine setPropertiesInitialDisplacementSoybean(this, default_value)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: default_value
      real(real64) :: defval
      integer(int32) :: i, j

      defval = input(default=0.0d0, option=default_value)

      if (allocated(this%stem)) then
         print *, "[ok] setPropertiesInitialDisplacementSoybean >> stems exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if stem(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%stem(i)%Displacement)) then

                  print *, "[ok] setPropertiesInitialDisplacementSoybean >> &
&                    stem("//str(i)//")%Displacement >> allocated"
                  ! then ok. let's return
                  this%property_deform_initial_displacement = .true.

               else

                  ! youngmodulus is not allocated.

                  this%stem(i)%Displacement = zeros(this%stem(i)%femdomain%nn(), 3)
                  this%stem(i)%Displacement = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if

      ! same for leaf
      if (allocated(this%leaf)) then
         print *, "[ok] setPropertiesInitialDisplacementSoybean >> leafs exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if leaf(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%leaf(i)%Displacement)) then

                  print *, "[ok] setPropertiesInitialDisplacementSoybean >> leaf("//str(i)//")%Displacement >> allocated"
                  ! then ok. let's return
                  this%property_deform_initial_displacement = .true.

               else
                  ! youngmodulus is not allocated.

                  this%leaf(i)%Displacement = zeros(this%leaf(i)%femdomain%nn(), 3)
                  this%leaf(i)%Displacement = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if

      ! same for root
      if (allocated(this%root)) then
         print *, "[ok] setPropertiesInitialDisplacementSoybean >> roots exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if root(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%root(i)%Displacement)) then

                  print *, "[ok] setPropertiesInitialDisplacementSoybean >> root("//str(i)//")%Displacement >> allocated"
                  ! then ok. let's return
                  this%property_deform_initial_displacement = .true.

               else

                  ! youngmodulus is not allocated.

                  this%root(i)%Displacement = zeros(this%root(i)%femdomain%nn(), 3)
                  this%root(i)%Displacement = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if
      this%property_deform_initial_displacement = .true.

   end subroutine

! same for initialstress but dimension = 3
   subroutine setPropertiesInitialStressSoybean(this, default_value)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: default_value
      real(real64) :: defval
      integer(int32) :: i, j

      defval = input(default=0.0d0, option=default_value)

      if (allocated(this%stem)) then
         print *, "[ok] setPropertiesInitialStressSoybean >> stems exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if stem(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%stem(i)%stress)) then

                  print *, "[ok] setPropertiesInitialStressSoybean >> &
&                    stem("//str(i)//")%stress >> allocated"
                  ! then ok. let's return
                  this%property_deform_initial_stress = .true.

               else

                  ! youngmodulus is not allocated.

                  this%stem(i)%stress = zeros(this%stem(i)%femdomain%ne(), this%stem(i)%femdomain%nne(), 6)
                  this%stem(i)%stress = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if
      ! same for leaf
      if (allocated(this%leaf)) then
         print *, "[ok] setPropertiesInitialStressSoybean >> leafs exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if leaf(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%leaf(i)%stress)) then

                  print *, "[ok] setPropertiesInitialStressSoybean >> leaf("//str(i)//")%stress >> allocated"
                  ! then ok. let's return
                  this%property_deform_initial_stress = .true.

               else
                  ! youngmodulus is not allocated.

                  this%leaf(i)%stress = zeros(this%leaf(i)%femdomain%ne(), this%leaf(i)%femdomain%nne(), 6)
                  this%leaf(i)%stress = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if

      ! same for root
      if (allocated(this%root)) then
         print *, "[ok] setPropertiesInitialStressSoybean >> roots exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if root(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%root(i)%stress)) then

                  print *, "[ok] setPropertiesInitialStressSoybean >> root("//str(i)//")%stress >> allocated"
                  ! then ok. let's return
                  this%property_deform_initial_stress = .true.

               else

                  ! youngmodulus is not allocated.

                  this%root(i)%stress = zeros(this%root(i)%femdomain%ne(), this%root(i)%femdomain%nne(), 6)
                  this%root(i)%stress = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if
      this%property_deform_initial_stress = .true.
   end subroutine

! ################################################################################

! same as initdisplacement for BoundaryTractionForce
   subroutine setPropertiesBoundaryTractionForceSoybean(this, default_value, xrange, yrange, zrange)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: default_value, xrange(2), yrange(2), zrange(2)
      real(real64) :: defval
      integer(int32) :: i, j

      defval = input(default=0.0d0, option=default_value)

      if (allocated(this%stem)) then
         print *, "[ok] setPropertiesBoundaryTractionForceSoybean >> stems exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if stem(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%stem(i)%BoundaryTractionForce)) then

                  print *, "[ok] setPropertiesBoundaryTractionForceSoybean >> &
&                    stem("//str(i)//")%BoundaryTractionForce >> allocated"
                  ! then ok. let's return
                  this%property_deform_boundary_tractionforce = .true.

               else

                  ! youngmodulus is not allocated.

                  this%stem(i)%BoundaryTractionForce = zeros(this%stem(i)%femdomain%nn(), 3)
                  this%stem(i)%BoundaryTractionForce = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if
      ! same for leaf
      if (allocated(this%leaf)) then
         print *, "[ok] setPropertiesBoundaryTractionForceSoybean >> leafs exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if leaf(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%leaf(i)%BoundaryTractionForce)) then

                  print *, "[ok] setPropertiesBoundaryTractionForceSoybean &
&                    >> leaf("//str(i)//")%BoundaryTractionForce >> allocated"
                  ! then ok. let's return
                  this%property_deform_boundary_tractionforce = .true.

               else
                  ! youngmodulus is not allocated.

                  this%leaf(i)%BoundaryTractionForce = zeros(this%leaf(i)%femdomain%nn(), 3)
                  this%leaf(i)%BoundaryTractionForce = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if

      ! same for root
      if (allocated(this%root)) then
         print *, "[ok] setPropertiesBoundaryTractionForceSoybean >> roots exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if root(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%root(i)%BoundaryTractionForce)) then

                  print *, "[ok] setPropertiesBoundaryTractionForceSoybean &
&                    >> root("//str(i)//")%BoundaryTractionForce >> allocated"
                  ! then ok. let's return
                  this%property_deform_boundary_tractionforce = .true.

               else

                  ! youngmodulus is not allocated.

                  this%root(i)%BoundaryTractionForce = zeros(this%root(i)%femdomain%nn(), 3)
                  this%root(i)%BoundaryTractionForce = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if
      this%property_deform_boundary_tractionforce = .true.
   end subroutine
! ##################################################################

! ################################################################################

! same as initdisplacement for BoundaryTractionForce
   subroutine setPropertiesBoundaryDisplacementSoybean(this, default_value, xrange, yrange, zrange)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: default_value, xrange(2), yrange(2), zrange(2)
      real(real64) :: defval
      integer(int32) :: i, j

      defval = input(default=0.0d0, option=default_value)

      if (allocated(this%stem)) then
         print *, "[ok] setPropertiesBoundaryDisplacementSoybean >> stems exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if stem(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%stem(i)%BoundaryDisplacement)) then

                  print *, "[ok] setPropertiesBoundaryDisplacementSoybean >> &
&                    stem("//str(i)//")%BoundaryDisplacement >> allocated"
                  ! then ok. let's return
                  this%property_deform_boundary_displacement = .true.

               else

                  ! youngmodulus is not allocated.

                  this%stem(i)%BoundaryDisplacement = zeros(this%stem(i)%femdomain%nn(), 3)
                  this%stem(i)%BoundaryDisplacement = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if
      ! same for leaf
      if (allocated(this%leaf)) then
         print *, "[ok] setPropertiesBoundaryDisplacementSoybean >> leafs exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if leaf(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%leaf(i)%BoundaryDisplacement)) then

                  print *, "[ok] setPropertiesBoundaryDisplacementSoybean &
&                    >> leaf("//str(i)//")%BoundaryDisplacement >> allocated"
                  ! then ok. let's return
                  this%property_deform_boundary_Displacement = .true.

               else
                  ! youngmodulus is not allocated.

                  this%leaf(i)%BoundaryDisplacement = zeros(this%leaf(i)%femdomain%nn(), 3)
                  this%leaf(i)%BoundaryDisplacement = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if

      ! same for root
      if (allocated(this%root)) then
         print *, "[ok] setPropertiesBoundaryDisplacementSoybean >> roots exist."
         ! leaf exists
        !!$OMP parallel do private(i)
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               ! allocate youngmoludus if root(i) exists and not allocated
               ! the default value is defval
               if (allocated(this%root(i)%BoundaryDisplacement)) then

                  print *, "[ok] setPropertiesBoundaryDisplacementSoybean &
&                    >> root("//str(i)//")%BoundaryDisplacement >> allocated"
                  ! then ok. let's return
                  this%property_deform_boundary_displacement = .true.

               else

                  ! youngmodulus is not allocated.

                  this%root(i)%BoundaryDisplacement = zeros(this%root(i)%femdomain%nn(), 3)
                  this%root(i)%BoundaryDisplacement = defval
               end if
            end if
         end do
        !!$OMP end parallel do
      end if
      this%property_deform_boundary_Displacement = .true.
   end subroutine
! ##################################################################

   subroutine setPropertiesGravitySoybean(this, default_value, xrange, yrange, zrange)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: default_value, xrange(2), yrange(2), zrange(2)
      real(real64) :: defval
      integer(int32) :: i, j

      defval = input(default=9.810d0, option=default_value)

      this%Gravity_acceralation = defval

      this%property_deform_gravity = .true.

   end subroutine

! ##################################################################
   subroutine setPropertiesSoybean(this, density, YoungModulus, PoissonRatio, &
                                   InitialStress, InitialDisplacement, &
                                   BoundaryTractionForce, BoundaryDisplacement, Gravity, xr, yr, zr, &
                                   default_value)
      class(Soybean_), intent(inout) :: this
      logical, optional, intent(in) :: density, YoungModulus, PoissonRatio, InitialStress, &
                                       InitialDisplacement, &
                                       BoundaryTractionForce, BoundaryDisplacement, Gravity
      real(real64), optional, intent(in) :: xr(2), yr(2), zr(2), default_value
      integer(int32) :: i, j

      ! set each conditions
      if (present(density)) then
         if (density) then
            call this%setPropertiesDensity()
         end if
      end if

      if (present(YoungModulus)) then
         if (YoungModulus) then
            call this%setPropertiesYoungModulus(default_value=default_value)
         end if
      end if

      if (present(PoissonRatio)) then
         if (PoissonRatio) then
            call this%setPropertiesPoissonRatio(default_value=default_value)
         end if
      end if

      if (present(InitialDisplacement)) then
         if (InitialDisplacement) then
            call this%setPropertiesInitialDisplacement(default_value=default_value)
         end if
      end if

      if (present(InitialStress)) then
         if (InitialStress) then
            call this%setPropertiesInitialStress(default_value=default_value)
         end if
      end if

      if (present(BoundaryTractionForce)) then
         if (BoundaryTractionForce) then
            call this%setPropertiesBoundaryTractionForce(default_value=default_value)
         end if
      end if

      if (present(BoundaryDisplacement)) then
         if (BoundaryDisplacement) then
            call this%setPropertiesBoundaryDisplacement(default_value=default_value)
         end if
      end if

      if (present(Gravity)) then
         if (Gravity) then
            call this%setPropertiesGravity(default_value=default_value)
         end if
      end if
   end subroutine
! ##################################################################

   function readyForSoybean(this, Simulator) result(ready)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) ::  Simulator
      logical :: ready
      ! default = ready!
      ! if all the properties are set, then ready = true
      if (Simulator == PF_DEFORMATION_ANALYSIS) then
         ready = .true.
         ready = ready .and. this%property_deform_material_density
         ready = ready .and. this%property_deform_material_YoungModulus
         ready = ready .and. this%property_deform_material_PoissonRatio
         ready = ready .and. this%property_deform_initial_Displacement
         ready = ready .and. this%property_deform_material_CarbonDiffusionCoefficient
         ready = ready .and. this%property_deform_initial_Stress
         ready = ready .and. this%property_deform_boundary_TractionForce
         ready = ready .and. this%property_deform_boundary_Displacement
         ready = ready .and. this%property_deform_gravity
      else
         print *, "[ERROR] readyForSoybean >> invalid  Simulator type.", Simulator
      end if
   end function
! ##################################################################

   subroutine runSimulationSoybean(this, Simulator, error_tolerance, debug, z_min)
      class(Soybean_), target, intent(inout) :: this
      type(ContactMechanics_) :: contact
      type(FEMDomainp_), allocatable :: femdomainp(:)
      type(FEMDomain_), allocatable :: femdomains(:)

      type(Dictionary_) :: YoungModulusList
      type(Dictionary_) :: PoissonRatioList
      type(Dictionary_) :: DensityList
      real(real64), optional, intent(in) :: error_tolerance
      real(real64), intent(in) :: z_min

      logical, optional, intent(in) :: debug

      integer(int32), allocatable :: contactlist(:, :)
      integer(int32), intent(in) ::  Simulator
      integer(int32) :: i, j, k, i_offset, j_offset
      type(IO_)  :: f

      if (.not. this%readyFor(Simulator)) then
         call this%checkProperties(Simulator=Simulator)
         print *, "[ERROR] :: runSimulationSoybean >> .not.this%readyFor(Simulator) "
         return
      end if

      if (Simulator == PF_DEFORMATION_ANALYSIS) then
         ! run
         print *, "[ok] Running PF_DEFORMATION_ANALYSIS..."
         ! 全てのdomainのpointer
         allocate (femdomainp(this%numleaf() + this%numStem() + this%numRoot()))
         allocate (femdomains(this%numleaf() + this%numStem() + this%numRoot()))
         k = 0
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               k = k + 1
               femdomainp(k)%femdomainp => this%stem(i)%femdomain
               femdomains(k) = this%stem(i)%femdomain
            end if
         end do
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               k = k + 1
               femdomainp(k)%femdomainp => this%leaf(i)%femdomain
               femdomains(k) = this%leaf(i)%femdomain
            end if
         end do
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               k = k + 1
               femdomainp(k)%femdomainp => this%root(i)%femdomain
               femdomains(k) = this%root(i)%femdomain
            end if
         end do

         ! >>>>>>>>>>>>>>>>>>>>>>>
         ! connectivitylist
         ! >>>>>>>>>>>>>>>>>>>>>>>

         k = this%numStem() + this%numleaf() + this%numRoot()
         contactlist = zeros(k, k)

         ! leaf to stem
         i_offset = this%numStem()
         j_offset = 0
        !!!$OMP parallel do private(i,j)
         do i = 1, this%numleaf()
            do j = 1, this%numstem()
               if (this%leaf2stem(i, j) /= 0) then
                  contactlist(i + i_offset, j + j_offset) = this%leaf2stem(i, j)
               end if
            end do
         end do
        !!!$OMP end parallel do

         ! stem to stem
         i_offset = 0
         j_offset = 0
        !!!$OMP parallel do private(i,j)
         do i = 1, this%numstem()
            do j = 1, this%numstem()
               if (this%stem2stem(i, j) >= 1) then
                  contactlist(i + i_offset, j + j_offset) = this%stem2stem(i, j)
               end if
            end do
         end do
        !!!$OMP end parallel do

         ! root to stem
         i_offset = this%numstem() + this%numleaf()
         j_offset = 0
        !!!$OMP parallel do private(i,j)
         do i = 1, this%numroot()
            do j = 1, this%numstem()
               if (this%root2stem(i, j) /= 0) then
                  contactlist(i + i_offset, j + j_offset) = this%root2stem(i, j)
               end if
            end do
         end do
        !!!$OMP end parallel do

         ! root to root
         i_offset = this%numstem() + this%numroot()
         j_offset = this%numstem() + this%numroot()
        !!!$OMP parallel do private(i,j)
         do i = 1, this%numroot()
            do j = 1, this%numroot()
               if (this%root2root(i, j) /= 0) then
                  contactlist(i + i_offset, j + j_offset) = this%root2root(i, j)
               end if
            end do
         end do
        !!!$OMP end parallel do

         ! YoungModulusListを作る
         allocate (YoungModulusList%pages(this%numleaf() + this%numStem() + this%numRoot()))
         k = 0
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               k = k + 1
               YoungModulusList%pages(k)%realist = this%stem(i)%YoungModulus
            end if
         end do
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               k = k + 1
               YoungModulusList%pages(k)%realist = this%leaf(i)%YoungModulus
            end if
         end do
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               k = k + 1
               YoungModulusList%pages(k)%realist = this%root(i)%YoungModulus
            end if
         end do

         ! PoissonRatioListを作る
         allocate (PoissonRatioList%pages(this%numstem() + this%numleaf() + this%numRoot()))
         k = 0
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               k = k + 1
               PoissonRatioList%pages(k)%realist = this%stem(i)%PoissonRatio
            end if
         end do
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               k = k + 1
               PoissonRatioList%pages(k)%realist = this%leaf(i)%PoissonRatio
            end if
         end do
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               k = k + 1
               PoissonRatioList%pages(k)%realist = this%root(i)%PoissonRatio
            end if
         end do

         ! DensityListを作る
         allocate (DensityList%pages(this%numStem() + this%numLeaf() + this%numRoot()))
         k = 0
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               k = k + 1
               DensityList%pages(k)%realist = this%stem(i)%Density
            end if
         end do
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               k = k + 1
               DensityList%pages(k)%realist = this%leaf(i)%Density
            end if
         end do
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               k = k + 1
               DensityList%pages(k)%realist = this%root(i)%Density
            end if
         end do

         ! ContactMechanicsClassを呼ぶ
         call contact%init(femdomains=femdomains, contactlist=contactlist)

         print *, "[ok] Initialized simulator"

         ! 要修正(1) 材料パラメータをElement-wiseに導入する．
         ! Import material parameters
         ! Element-wise にする．

         contact%YoungModulusList = YoungModulusList
         contact%PoissonRatioList = PoissonRatioList
         contact%DensityList = DensityList
         contact%gravity = this%Gravity_acceralation

         !
         call contact%setup(penaltyparameter=this%PenaltyParameter, &
                            GaussPointProjection=this%GaussPointProjection)

         ! 要修正(2) 境界条件を課す節点のリスト+値から境界条件を導入．
         ! Boundary conditions

         ! fix displacement
         ! Listから選択
         print *, "[ok] set up done."
         call contact%fix(direction="x", disp=0.0d0, DomainID=1, z_max=0.010d0)
         call contact%fix(direction="y", disp=0.0d0, DomainID=1, z_max=0.010d0)
         call contact%fix(direction="z", disp=0.0d0, DomainID=1, z_max=0.010d0)

         !do i=1,size(contact%femdomains)
         call contact%fix(direction="x", disp=-0.01d0, DomainID=5, z_min=0.30d0, z_max=0.410d0)
         !enddo

         ! traction forceを入れる．

         ! solve > get displacement
         !call f%open("debug.txt")
         !call f%write(contact%contactlist)
         !call f%close()
         !stop
         contact%solver%er0 = input(default=dble(1.0e-7), option=error_tolerance)
         if (present(debug)) then
            contact%solver%debug = debug
         end if
         call contact%solver%solve("BiCGSTAB")

         ! update mesh
         call contact%updateMesh()
         k = 0
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) then
               cycle
            else
               k = k + 1
               this%stem(i)%femdomain = femdomains(k)
            end if
         end do
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) then
               cycle
            else
               k = k + 1
               this%leaf(i)%femdomain = femdomains(k)
            end if
         end do
         do i = 1, size(this%root)
            if (this%root(i)%empty()) then
               cycle
            else
               k = k + 1
               this%root(i)%femdomain = femdomains(k)
            end if
         end do

         ! 要修正(3) 変位から応力，等価節点力を計算

      else
         print *, "[ERROR] readyForSoybean >> invalid  Simulator type.", Simulator
      end if
   end subroutine
! ##################################################################

   pure function getPointsSoybean(this, leaf, stem, root) result(points)
      class(Soybean_), intent(in) :: this
      logical, optional, intent(in) :: leaf, stem, root
      real(real64), allocatable :: points(:, :), buf(:, :)
      logical :: count_leaf, count_stem, count_root
      integer(int32) :: i, n, id

      if (present(leaf)) then
         count_leaf = Leaf
      end if

      if (present(stem)) then
         count_stem = Stem
      end if

      if (present(root)) then
         count_root = Root
      end if

      n = this%nn()
      points = zeros(n, 3)

      id = 1
      !if(count_stem)then
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%empty()) then
               points(id:id + this%stem(i)%femdomain%nn() - 1, 1:3) = &
                  this%stem(i)%femdomain%mesh%nodcoord(1:this%stem(i)%femdomain%nn(), 1:3)
               id = id + this%stem(i)%femdomain%nn()
            end if
         end do
      end if
      !endif

      !if(count_leaf)then
      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%empty()) then
               points(id:id + this%leaf(i)%femdomain%nn() - 1, 1:3) = &
                  this%leaf(i)%femdomain%mesh%nodcoord(:, :)
               id = id + this%leaf(i)%femdomain%nn()
            end if
         end do
      end if
      !endif

      !if(count_root)then
      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%empty()) then
               points(id:id + this%root(i)%femdomain%nn() - 1, 1:3) = &
                  this%root(i)%femdomain%mesh%nodcoord(:, :)
               id = id + this%root(i)%femdomain%nn()
            end if
         end do
      end if
      !endif

      !if(id /=n)then
      !    buf = points
      !    points = zeros(id,3)
      !    points(1:id,:) = buf(1:id,:)
      !endif

   end function
! ############################################################################

! ##################################################################

   subroutine setPointsSoybean(this, points)
      class(Soybean_), intent(inout) :: this
      real(real64), intent(in) :: points(:, :)
      integer(int32) :: i, n, id

      if (size(points, 1) /= this%nn()) then
         print *, "[ERROR] setPointsSoybean >> Invalid size of arg points"
         print *, "size(points,1)/=this%nn()", size(points, 1), this%nn()
         return
      end if

      id = 1
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%empty()) then
               this%stem(i)%femdomain%mesh%nodcoord(1:this%stem(i)%femdomain%nn(), 1:3) &
                  = points(id:id + this%stem(i)%femdomain%nn() - 1, 1:3)
               id = id + this%stem(i)%femdomain%nn()
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%empty()) then
               this%leaf(i)%femdomain%mesh%nodcoord(:, :) &
                  = points(id:id + this%leaf(i)%femdomain%nn() - 1, 1:3)
               id = id + this%leaf(i)%femdomain%nn()
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%empty()) then
               this%root(i)%femdomain%mesh%nodcoord(:, :) &
                  = points(id:id + this%root(i)%femdomain%nn() - 1, 1:3)

               id = id + this%root(i)%femdomain%nn()
            end if
         end do
      end if

   end subroutine
! ############################################################################

   function getDistanceFromGroundSoybean(this) result(distance_per_nodes)
      class(Soybean_), intent(inout) :: this
      real(real64), allocatable :: distance_per_nodes(:), xA(:), xB(:), dist_per_stem(:), &
                                   dist_per_root(:)
      integer(int32), allocatable :: num_of_point(:)
      real(real64)   :: dist_AB, dist_add, dist_parent
      integer(int32) :: i, j, k, node_id, num_node, id, from_id, to_id, stem_id

      ! get distance from the intersection between root and stem
      node_id = 1

      ! get the intersection
      num_node = this%nn()

      distance_per_nodes = zeros(this%nn())
      dist_per_stem = zeros(size(this%stem))
      dist_per_root = zeros(size(this%root))

      num_of_point = this%getNumberOfPoint()
      id = 0
      ! global search
      ! calculate distance from bottom to "A" node of each stem domains
      ! 1節目から順番に接いでいったと仮定する．

    !!$OMP parallel do private(i)
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%empty()) then
            dist_per_stem(i) = this%getDistanceToGroundFromStemID( &
                               dist_in=0.0d0, &
                               stem_id=i)
         end if
      end do
    !!$OMP end parallel do

      ! comupte node-wise data from stem-wise data

      do i = 1, size(num_of_point)
         if (i == 1) then
            from_id = 1
            to_id = num_of_point(1)
         else
            from_id = sum(num_of_point(1:i - 1)) + 1
            to_id = sum(num_of_point(1:i))
         end if

         distance_per_nodes(from_id:to_id) = &
            distance_per_nodes(from_id:to_id) &
            + dist_per_stem(i)

      end do

      do i = this%numStem() + 1, this%numStem() + this%numLeaf()

         if (i == 1) then
            from_id = 1
            to_id = num_of_point(1)
         else
            from_id = sum(num_of_point(1:i - 1)) + 1
            to_id = sum(num_of_point(1:i))
         end if

         do j = 1, size(this%leaf2stem, 1)
            if (this%leaf2stem(i - this%numStem(), j) /= 0) then
               stem_id = j
               exit
            end if
         end do

         distance_per_nodes(from_id:to_id) = &
            distance_per_nodes(from_id:to_id) &
            + dist_per_stem(stem_id)

      end do

      ! 1節目から順番に接いでいったと仮定する．
    !!$OMP parallel do private(i)
      do i = 1, size(this%Root)
         if (.not. this%Root(i)%empty()) then
            dist_per_Root(i) = this%getDistanceToGroundFromRootID( &
                               dist_in=0.0d0, &
                               Root_id=i)
         end if
      end do
    !!$OMP end parallel do

      ! comupte node-wise data from root-wise data
      do i = this%numstem() + this%numleaf() + 1, this%numstem() + this%numleaf() + this%numRoot()
         if (i == 1) then
            from_id = 1
            to_id = num_of_point(1)
         else
            from_id = sum(num_of_point(1:i - 1)) + 1
            to_id = sum(num_of_point(1:i))
         end if

         distance_per_nodes(from_id:to_id) = &
            distance_per_nodes(from_id:to_id) &
            + dist_per_root(i - this%numstem() - this%numleaf())

      end do

      ! node-to-node
      ! @stem
      id = 0
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%femdomain%empty()) then
            id = id + 1
            do j = 1, this%stem(i)%femdomain%nn()
               xA = this%stem(i)%getCoordinate("A")
               xB = this%stem(i)%femdomain%mesh%nodcoord(j, :)
               if (id == 1 .and. i == 1) then
                  node_id = j
               else
                  node_id = sum(num_of_point(1:id)) + j
               end if
               distance_per_nodes(node_id) = distance_per_nodes(node_id) &
                                             + norm(xA - xB)
            end do
         end if
      end do
      ! for each domain
      ! @leaf
      id = this%numStem() - 1
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%empty()) then
            id = id + 1
            do j = 1, this%leaf(i)%femdomain%nn()
               xA = this%leaf(i)%getCoordinate("A")
               xB = this%leaf(i)%femdomain%mesh%nodcoord(j, :)
               if (id == 1 .and. i == 1) then
                  node_id = j
               else
                  node_id = sum(num_of_point(1:id)) + j
               end if
               distance_per_nodes(node_id) = distance_per_nodes(node_id) &
                                             + norm(xA - xB)
            end do
         end if
      end do

      ! for each domain
      ! @root
      id = this%numStem() + this%numLeaf() - 1
      do i = 1, size(this%root)
         if (.not. this%root(i)%femdomain%empty()) then
            id = id + 1
            do j = 1, this%root(i)%femdomain%nn()
               xA = this%root(i)%getCoordinate("A")
               xB = this%root(i)%femdomain%mesh%nodcoord(j, :)
               if (id == 1 .and. i == 1) then
                  node_id = j
               else
                  node_id = sum(num_of_point(1:id)) + j
               end if
               distance_per_nodes(node_id) = distance_per_nodes(node_id) &
                                             + norm(xA - xB)
            end do
         end if
      end do

   end function
! ############################################################################

! ############################################################################
   function getNumberOfPointSoybean(this) result(NumberOfPoint)
      class(Soybean_), intent(in) :: this
      integer(int32), allocatable :: NumberOfPoint(:)
      integer(int32) :: i, id
      ! order :: stem -> leaf -> root

      NumberOfPoint = zeros(this%numStem() + this%numLeaf() + this%numRoot())
      id = 1
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%empty()) then
            NumberOfPoint(id) = this%stem(i)%femdomain%nn()
            id = id + 1
         end if
      end do

      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%empty()) then
            NumberOfPoint(id) = this%leaf(i)%femdomain%nn()
            id = id + 1
         end if
      end do

      do i = 1, size(this%root)
         if (.not. this%root(i)%empty()) then
            NumberOfPoint(id) = this%root(i)%femdomain%nn()
            id = id + 1
         end if
      end do

   end function
! ############################################################################

! ############################################################################
   function getNumberOfElementSoybean(this) result(NumberOfElement)
      class(Soybean_), intent(in) :: this
      integer(int32), allocatable :: NumberOfElement(:)
      integer(int32) :: i, id
      ! order :: stem -> leaf -> root

      NumberOfElement = zeros(this%numStem() + this%numLeaf() + this%numRoot())
      id = 1
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%empty()) then
            NumberOfElement(id) = this%stem(i)%femdomain%ne()
            id = id + 1
         end if
      end do

      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%empty()) then
            NumberOfElement(id) = this%leaf(i)%femdomain%ne()
            id = id + 1
         end if
      end do

      do i = 1, size(this%root)
         if (.not. this%root(i)%empty()) then
            NumberOfElement(id) = this%root(i)%femdomain%ne()
            id = id + 1
         end if
      end do

   end function
! ############################################################################

! ############################################################################
   recursive function getDistanceToGroundFromStemIDSoybean(this, dist_in, stem_id) result(dist_ground)
      class(Soybean_), intent(in) :: this
      real(real64), intent(in)    :: dist_in
      integer(int32), intent(in)  :: stem_id
      integer(int32) :: j, parent_id
      real(real64) :: dist_ground
      real(real64) :: dist_AB, dist_old
      real(real64), allocatable :: xA(:), xB(:)

      ! check stem-to-stem connectivity
      dist_ground = dist_in
      if (maxval(this%stem2stem(stem_id, :)) == 0) then
         return
      end if

      do j = 1, size(this%stem2stem, 1)

         if (this%stem2stem(stem_id, j) >= 1) then
            if (.not. this%stem(j)%femdomain%empty()) then
               ! found parent
               ! number of parent node should be 1
               parent_id = j
               xA = this%stem(parent_id)%getCoordinate("A")
               xB = this%stem(parent_id)%getCoordinate("B")
               dist_AB = sqrt(dot_product(xA - xB, xA - xB))
               dist_old = dist_in + dist_AB

               dist_ground = this%getDistanceToGroundFromStemID( &
                             dist_in=dist_old, &
                             stem_id=parent_id)
               return
            end if
         end if

      end do

   end function
! ############################################################################

! ############################################################################
   recursive function getDistanceToGroundFromRootIDSoybean(this, dist_in, root_id) result(dist_ground)
      class(Soybean_), intent(in) :: this
      real(real64), intent(in)    :: dist_in
      integer(int32), intent(in)  :: root_id
      integer(int32) :: j, parent_id
      real(real64) :: dist_ground
      real(real64) :: dist_AB, dist_old
      real(real64), allocatable :: xA(:), xB(:)

      ! check root-to-root connectivity
      dist_ground = dist_in
      if (maxval(this%root2root(root_id, :)) /= 1) then
         return
      end if

      do j = 1, size(this%root2root, 1)

         if (this%root2root(root_id, j) == 1) then
            if (.not. this%root(j)%femdomain%empty()) then
               ! found parent
               ! number of parent node should be 1
               parent_id = j
               xA = this%root(parent_id)%getCoordinate("A")
               xB = this%root(parent_id)%getCoordinate("B")
               dist_AB = sqrt(dot_product(xA - xB, xA - xB))
               dist_old = dist_in + dist_AB

               dist_ground = this%getDistanceToGroundFromrootID( &
                             dist_in=dist_old, &
                             root_id=parent_id)
               return
            end if
         end if

      end do

   end function
! ############################################################################

! ############################################################################
   function getRangeOfNodeIDSoybean(this, stem, leaf, root) result(id_range)
      class(Soybean_), intent(in) :: this
      integer(int32) :: id_range(2), numStemNode, numLeafNode, numRootNode, i
      logical, optional, intent(in) :: stem, leaf, root

      id_range(1:2) = [0, 0]

      numStemNode = 0
      do i = 1, size(this%stem)
         if (.not. this%stem(i)%femdomain%empty()) then
            numStemNode = numStemNode + this%stem(i)%femdomain%nn()
         end if
      end do

      if (present(stem)) then
         if (stem) then
            id_range(1) = 1
            id_range(2) = numStemNode
            return
         end if
      end if

      numLeafNode = 0
      do i = 1, size(this%Leaf)
         if (.not. this%Leaf(i)%femdomain%empty()) then
            numLeafNode = numLeafNode + this%Leaf(i)%femdomain%nn()
         end if
      end do

      if (present(leaf)) then
         if (leaf) then
            id_range = [numStemNode + 1, numStemNode + numLeafNode]
            return
         end if
      end if

      numRootNode = 0
      do i = 1, size(this%Root)
         if (.not. this%Root(i)%femdomain%empty()) then
            numRootNode = numRootNode + this%Root(i)%femdomain%nn()
         end if
      end do

      if (present(root)) then
         if (root) then
            id_range = [numStemNode + numLeafNode + 1, &
                        numStemNode + numLeafNode + numRootNode]
            return
         end if
      end if

   end function
! ############################################################################

! ############################################################################
   function getSpectrumSoybean(this, light, Transparency, Resolution, num_threads, leaf) result(spectrum)
      class(Soybean_), intent(inout) :: this
      type(Light_), intent(in)    :: light
      real(real64), optional, intent(in) :: Transparency, Resolution
      integer(int32), optional, intent(in) :: num_threads
      ! leaf of other plants
      type(Leaf_), optional, intent(inout) :: leaf(:)
      real(real64), allocatable :: ppfd(:), tp_ratio(:), spectrum(:, :)
      integeR(int32) :: i

      ppfd = this%getPPFD(light, Transparency, Resolution, num_threads, leaf)
      tp_ratio = ppfd/light%maxPPFD
      spectrum = zeros(size(ppfd), size(light%spectrum))

      ! 400-700nmはtransparencyに基づき減衰
      ! 1-399nm, 701nm以上はそのまま透過
      do i = 1, size(ppfd)
         spectrum(i, :) = light%spectrum(:)
         spectrum(i, 400:700) = light%spectrum(400:700)*tp_ratio(i)
      end do

   end function
! ############################################################################

! ############################################################################
   function getPPFDSoybean(this, light, Transparency, Resolution, num_threads, leaf) result(ppfd)
      class(Soybean_), intent(inout) :: this
      type(Light_), intent(in)    :: light
      real(real64), optional, intent(in) :: Transparency, Resolution
      integer(int32), optional, intent(in) :: num_threads
      ! leaf of other plants
      type(Leaf_), optional, intent(inout) :: leaf(:)

      real(real64), allocatable :: ppfd(:), NumberOfElement(:), NumberOfPoint(:)
      real(real64), allocatable :: nodcoord(:, :), radius_vec(:), elem_cosins(:)
      integer(int32), allocatable :: leaf_pass_num(:)
      real(real64) ::thickness, center_x(3), xmin(3), xmax(3), radius, radius_tr, coord(3), Transparency_val
      real(real64) :: zmin
      integer(int32) :: from, to, i, n, j, k, l, element_id

      logical :: inside, upside

      ! compute cosin
      !elem_cosins = this%getLeafCosValue(light)

      ! rotate soybean
      call this%rotate(z=radian(180.0d0 - light%angles(1)))
      call this%rotate(x=radian(90.0d0 - light%angles(2)))
      if (present(leaf)) then
         do i = 1, size(leaf)
            call leaf(i)%femdomain%rotate(z=radian(180.0d0 - light%angles(1)))
            call leaf(i)%femdomain%rotate(x=radian(90.0d0 - light%angles(2)))
         end do
      end if
      !本当にあってる？？
      ! after this, rotate this back again

      radius = input(default=0.0050d0, option=Resolution)
      Transparency_val = input(default=0.30d0, option=Transparency)

      ! ppfdが通過した葉の積算長さで減衰するモデル
      NumberOfElement = this%getNumberOfElement()
      ppfd = zeros(this%ne())
      elem_cosins = zeros(this%ne())
      !i = sum(NumberOfElement(1:this%numStem())+1)
      i = sum(NumberOfElement(1:this%numStem())) + 1
      j = sum(NumberOfElement(1:this%numStem() + this%numLeaf()))
      ppfd(i:j) = light%maxPPFD
      leaf_pass_num = int(zeros(this%ne()))

      n = sum(NumberOfElement(1:this%numStem()))
      from = n
      if (present(num_threads)) then
         call omp_set_num_threads(num_threads)
      end if

      ! leaf of other plants
      if (present(leaf)) then
         !$OMP parallel do default(shared), private(j,k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%empty()) then
               !print *, i, "/", this%numLeaf()
               !$OMP parallel do default(shared), private(k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
               do j = 1, this%leaf(i)%femdomain%ne()
                  ! 中心座標
                  center_x = this%leaf(i)%femdomain%centerPosition(ElementID=j)
                  ! 枚数のみカウント
                  ! 1枚あたりthicknessだけ距離加算

                  !$OMP parallel do default(shared), private(inside,upside,radius_vec,radius_tr,zmin,n,element_id)
                  do k = 1, size(leaf)
                     if (.not. leaf(k)%femdomain%empty()) then

                        inside = .false.
                        radius_vec = zeros(leaf(k)%femdomain%nn())
                        radius_vec = (leaf(k)%femdomain%mesh%nodcoord(:, 1) - center_x(1))**2 &
                                     + (leaf(k)%femdomain%mesh%nodcoord(:, 2) - center_x(2))**2

                        radius_tr = minval(radius_vec)

                        if (radius_tr < radius*radius) then
                           zmin = leaf(k)%femdomain%mesh%nodcoord(minvalID(radius_vec), 3)
                           inside = .true.
                        end if
                        !あるいは，zmin,xmax,ymin,ymaxの正負で場合分けできるのでは？
                        !>>なぜか失敗
                        !upside = (center_x(3) < zmin )
                        if (inside .eqv. .true.) then

                           if (center_x(3) <= zmin) then
                              !print *, center_x(3) , zmin

                              n = this%numStem() + (i - 1)

                              element_id = sum(NumberOfElement(1:n)) + j
                              leaf_pass_num(element_id) = leaf_pass_num(element_id) + 1
                           end if
                        end if
                     end if
                  end do
                  !$OMP end parallel do
               end do
               !$OMP end parallel do
            end if
         end do
         !$OMP end parallel do
      end if

      !$OMP parallel private(j,k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
      !$OMP do reduction(+:leaf_pass_num)
      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%empty()) then
            cycle
         else
            !print *, i, "/", this%numLeaf()
            !!$OMP parallel do default(shared), private(k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)

            do j = 1, this%leaf(i)%femdomain%ne()
               ! 中心座標
               center_x = this%leaf(i)%femdomain%centerPosition(ElementID=j)

               ! 枚数のみカウント
               ! 1枚あたりthicknessだけ距離加算
                !!$OMP parallel do default(shared), private(inside,upside,radius_vec,radius_tr,zmin,n,element_id)
               do k = 1, size(this%leaf)

                  if (i == k) cycle
                  if (this%leaf(k)%femdomain%empty()) then
                     cycle
                  else

                     inside = .false.
                     radius_vec = zeros(this%leaf(k)%femdomain%nn())
                     radius_vec = (this%leaf(k)%femdomain%mesh%nodcoord(:, 1) - center_x(1))**2 &
                                  + (this%leaf(k)%femdomain%mesh%nodcoord(:, 2) - center_x(2))**2

                     radius_tr = minval(radius_vec)
                     if (radius_tr < radius*radius) then
                        zmin = this%leaf(k)%femdomain%mesh%nodcoord(minvalID(radius_vec), 3)
                        inside = .true.
                     end if
                     !あるいは，zmin,xmax,ymin,ymaxの正負で場合分けできるのでは？
                     !>>なぜか失敗

                     if (inside .eqv. .true.) then
                        if (center_x(3) <= zmin) then
                           if (inside .eqv. .false.) then
                              cycle
                           end if
                           if (center_x(3) > zmin) then
                              cycle
                           end if
                           n = this%numStem() + (i - 1)
                           element_id = sum(NumberOfElement(1:n)) + j
                           leaf_pass_num(element_id) = leaf_pass_num(element_id) + 1

                        end if
                     end if
                  end if
               end do
                !!$OMP end parallel do

            end do
            !!$OMP end parallel do

         end if
      end do
      !$OMP end do
      !$OMP end parallel

      !ppfd = ppfd*reduction*cosin-value
      do i = 1, this%ne()
         ! 400-700を一律減衰
         ppfd(i) = ppfd(i)*Transparency_val**leaf_pass_num(i)
      end do
      !ppfd(:) = ppfd(:)*elem_cosins(:)

      ! get back
      call this%rotate(x=-radian(90.0d0 - light%angles(2)))
      call this%rotate(z=-radian(180.0d0 - light%angles(1)))

      if (present(leaf)) then
         do i = 1, size(leaf)
            call leaf(i)%femdomain%rotate(x=-radian(90.0d0 - light%angles(2)))
            call leaf(i)%femdomain%rotate(z=-radian(180.0d0 - light%angles(1)))
         end do
      end if

   end function
! ############################################################################

! ############################################################################
   function getLeafCosValueSoybean(this, light, num_threads) result(elem_cosins)
      class(Soybean_), intent(inout) :: this
      type(Light_), intent(in)    :: light
      integer(int32), optional, intent(in) :: num_threads

      real(real64), allocatable :: cosin_value(:), NumberOfElement(:), NumberOfPoint(:)
      real(real64), allocatable :: leaf_pass_num(:), nodcoord(:, :), radius_vec(:), elem_cosins(:), &
                                   N_Light(:), N_Leaf(:)
      real(real64) ::thickness, center_x(3), xmin(3), xmax(3), radius, radius_tr, coord(3), Transparency_val
      real(real64) :: zmin
      integer(int32) :: from, to, i, n, j, k, l, element_id

      logical :: inside, upside

      ! rotate soybean

      call this%rotate(z=radian(180.0d0 - light%angles(1)))
      call this%rotate(x=radian(90.0d0 - light%angles(2)))

      NumberOfElement = this%getNumberOfElement()

      elem_cosins = zeros(this%ne())

      if (present(num_threads)) then
         call omp_set_num_threads(num_threads)
      end if

      !$OMP parallel do default(shared), private(j,k,n,element_id,N_leaf,N_light)
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%empty()) then
            !print *, i, "/", this%numLeaf()
            !$OMP parallel do default(shared), private(k,n,element_id,N_leaf,N_light)
            do j = 1, this%leaf(i)%femdomain%ne()
               ! cosin rule
               n = this%numStem() + (i - 1)
               element_id = sum(NumberOfElement(1:n)) + j
               N_leaf = this%leaf(i)%getNormalVector(ElementID=j)
               N_light = [0.0d0, 0.0d0, 1.0d0]
               elem_cosins(element_id) = dble(dot_product(N_light, N_Leaf))
            end do
            !$OMP end parallel do
         end if
      end do
      !$OMP end parallel do

      ! get back
      call this%rotate(x=-radian(90.0d0 - light%angles(2)))
      call this%rotate(z=-radian(180.0d0 - light%angles(1)))
   end function
! ############################################################################
   function getPhotoSynthesisSoybean(this, light, air, dt, Transparency, Resolution, ppfd) result(photosynthesis)
      class(Soybean_), intent(inout) :: this
      type(Light_), intent(in)    :: light
      type(Air_), intent(in)    :: Air
      real(real64), intent(in) :: dt
      real(real64), optional, intent(in) :: Transparency, Resolution, ppfd(:)
      real(real64), allocatable :: photosynthesis(:)

      integer(int32), allocatable :: NumberOfElement(:)
      integer(int32) :: i, j, offset, elem_id

      if (.not. allocated(this%Photosynthate_n)) then
         this%Photosynthate_n = zeros(this%nn())
      end if

      photosynthesis = zeros(this%ne())

      NumberOfElement = this%getNumberOfElement()
      offset = sum(NumberOfElement(1:this%numStem()))

      ! before photosynthesis
      elem_id = offset
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%empty()) then
            do j = 1, this%leaf(i)%femdomain%ne()
               elem_id = elem_id + 1
               photosynthesis(elem_id) = this%leaf(i)%source(j)
            end do
         end if
      end do

      if (.not. present(ppfd)) then

         call this%laytracing(light=light, Transparency=Transparency, Resolution=Resolution)

         ! 光合成量を計算
         do i = 1, size(this%Leaf)
            if (this%Leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%leaf(i)%photosynthesis(dt=dt, air=air)
            end if
         end do
      else
         elem_id = offset

         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%empty()) then
               do j = 1, this%leaf(i)%femdomain%ne()
                  elem_id = elem_id + 1
                  this%leaf(i)%ppfd(j) = ppfd(elem_id) !- photosynthesis(elem_id)
               end do
            end if
         end do

      end if

      elem_id = offset
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%empty()) then
            do j = 1, this%leaf(i)%femdomain%ne()
               elem_id = elem_id + 1
               photosynthesis(elem_id) = this%leaf(i)%source(j) - photosynthesis(elem_id)
            end do
         end if
      end do

   end function
! ############################################################################

   function getPhotoSynthesis_by_env_soybean(this, env, dt, Transparency, soybean_canopy) result(elem_photosynthesis)
      class(Soybean_), intent(inout) :: this
      class(Environment_), intent(in):: env
      real(real64), intent(in) :: dt ! sec.
      type(Soybean_), optional, intent(inout) :: soybean_canopy(:)
      type(Leaf_), allocatable :: leaf(:)
      real(real64), optional, intent(in) :: Transparency
      real(real64), allocatable :: elem_ppfd(:), elem_volume(:), &
                                   elem_photospeed(:), elem_photosynthesis(:)

      if (.not. allocated(this%Photosynthate_n)) then
         this%Photosynthate_n = zeros(this%nn())
      end if

      elem_volume = this%getVolumePerElement()
      if (present(soybean_canopy)) then

         leaf = this%getIntersectLeaf(soybeans=soybean_canopy)
         elem_ppfd = this%getPPFD( &
                     Light=env%Light, &
                     Transparency=input(default=0.10d0, option=Transparency), &
                     leaf=leaf)
      else
         elem_ppfd = this%getPPFD( &
                     Light=env%Light, &
                     Transparency=input(default=0.10d0, option=Transparency))

      end if

      elem_photospeed = this%getPhotoSynthesisSpeedPerVolume( &
                        Light=env%Light, Air=env%Air, dt=dt, &
                        Transparency=input(default=0.10d0, option=Transparency), &
                        ppfd=elem_ppfd)

      elem_photosynthesis = elem_photospeed*elem_volume

   end function

! ############################################################################
   function getPhotoSynthesisSpeedPerVolumeSoybean(this, light, air, dt, Transparency, Resolution, ppfd) result(photosynthesis)
      class(Soybean_), intent(inout) :: this
      type(Light_), intent(in)    :: light
      type(Air_), intent(in)    :: Air
      real(real64), intent(in) :: dt
      real(real64), optional, intent(in) :: Transparency, Resolution, ppfd(:)
      real(real64), allocatable :: photosynthesis(:), Speed_PV(:)

      integer(int32), allocatable :: NumberOfElement(:)
      integer(int32) :: i, j, offset, elem_id

      if (.not. allocated(this%Photosynthate_n)) then
         this%Photosynthate_n = zeros(this%nn())
      end if

      photosynthesis = zeros(this%ne())

      NumberOfElement = this%getNumberOfElement()
      offset = sum(NumberOfElement(1:this%numStem()))

      if (.not. present(ppfd)) then

         call this%laytracing(light=light, Transparency=Transparency, Resolution=Resolution)

      else
         elem_id = offset

         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%empty()) then
               do j = 1, this%leaf(i)%femdomain%ne()
                  elem_id = elem_id + 1
                  this%leaf(i)%ppfd(j) = ppfd(elem_id) !- photosynthesis(elem_id)
               end do
            end if
         end do

      end if

      elem_id = offset
      do i = 1, size(this%leaf)
         if (.not. this%leaf(i)%femdomain%empty()) then
            Speed_PV = this%leaf(i)%getPhotoSynthesisSpeedPerVolume(dt=dt, air=air)
            do j = 1, this%leaf(i)%femdomain%ne()
               elem_id = elem_id + 1
               photosynthesis(elem_id) = Speed_PV(j)
            end do
         end if
      end do

   end function
! ############################################################################

   subroutine fixReversedElementsSoybean(this)
      class(Soybean_), intent(inout) :: this
      integer(int32) :: i, j
      real(Real64) :: v

      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%empty()) cycle

         do j = 1, this%stem(i)%femdomain%ne()
            v = this%stem(i)%femdomain%getvolume(elem=j)
            if (v <= 0) then
               call this%stem(i)%femdomain%fixReversedElements()
               if (this%stem(i)%femdomain%getvolume(elem=j) < 0.0d0) then
                  print *, "[ERROR] >> fixReversedElementsSoybean >> not fixed"
                  stop
               end if
               exit
            end if
         end do

      end do

      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%empty()) cycle

         do j = 1, this%Leaf(i)%femdomain%ne()
            v = this%Leaf(i)%femdomain%getvolume(elem=j)
            if (v <= 0) then
               call this%Leaf(i)%femdomain%fixReversedElements()
               if (this%Leaf(i)%femdomain%getvolume(elem=j) < 0.0d0) then
                  print *, "[ERROR] >> fixReversedElementsSoybean >> not fixed"
                  stop
               end if
               exit
            end if
         end do

      end do

      do i = 1, size(this%root)
         if (this%root(i)%femdomain%empty()) cycle

         do j = 1, this%root(i)%femdomain%ne()
            v = this%root(i)%femdomain%getvolume(elem=j)
            if (v <= 0) then
               call this%root(i)%femdomain%fixReversedElements()
               if (this%root(i)%femdomain%getvolume(elem=j) < 0.0d0) then
                  print *, "[ERROR] >> fixReversedElementsSoybean >> not fixed"
                  stop
               end if
               exit
            end if
         end do

      end do

   end subroutine
! ################################################################
   function convertDataFormatSoybean(this, scalar, new_format) result(ret)
      class(Soybean_), intent(in) :: this
      real(real64), intent(in) :: scalar(:)
      integer(int32), intent(in) :: new_format
      real(real64), allocatable :: ret(:)
      integer(int32), allocatable :: NumberOfElement(:), NumberOfPoint(:)
      integer(int32) :: i, k, j, n
      logical :: ELEMENT_WISE, POINT_WISE

      NumberOfPoint = this%getNumberOfPoint()
      NumberOfElement = this%getNumberOfElement()

      POINT_WISE = .false.
      ELEMENT_WISE = .false.
      if (sum(NumberOfPoint) == size(scalar)) then
         POINT_WISE = .true.
      elseif (sum(NumberOfElement) == size(scalar)) then
         ELEMENT_WISE = .true.
      else
         print *, "[ERROR] convertDataFormatSoybean >> "
         print *, "Invalid vector size", size(scalar)
         stop
      end if

      if (new_format == PF_SOY_OBJECT_WISE) then
         ! for each root/stem/soil object
         ret = zeros(this%numStem() + this%numLeaf() + this%numRoot())

         if (POINT_WISE) then
            n = 0
            k = 0
            do i = 1, this%numStem()
               k = k + 1
               do j = 1, this%stem(i)%femdomain%nn()
                  n = n + 1
                  ret(k) = ret(k) + scalar(n)
               end do
            end do

            do i = 1, this%numLeaf()
               k = k + 1
               do j = 1, this%Leaf(i)%femdomain%nn()
                  n = n + 1
                  ret(k) = ret(k) + scalar(n)
               end do
            end do

            do i = 1, this%numRoot()
               k = k + 1
               do j = 1, this%Root(i)%femdomain%nn()
                  n = n + 1
                  ret(k) = ret(k) + scalar(n)
               end do
            end do

         elseif (ELEMENT_WISE) then
            n = 0
            k = 0
            do i = 1, this%numStem()
               k = k + 1
               do j = 1, this%stem(i)%femdomain%ne()
                  n = n + 1
                  ret(k) = ret(k) + scalar(n)
               end do
            end do

            do i = 1, this%numLeaf()
               k = k + 1
               do j = 1, this%Leaf(i)%femdomain%ne()
                  n = n + 1
                  ret(k) = ret(k) + scalar(n)
               end do
            end do

            do i = 1, this%numRoot()
               k = k + 1
               do j = 1, this%Root(i)%femdomain%ne()
                  n = n + 1
                  ret(k) = ret(k) + scalar(n)
               end do
            end do
         end if

      end if
   end function
! ################################################################
   function getLeafAreaSoybean(this) result(LeafArea)
      class(Soybean_), intent(in) :: this
      real(real64) :: LeafArea
      integer(int32) :: i

      LeafArea = 0.0d0
      do i = 1, this%numLeaf()
         LeafArea = LeafArea + this%leaf(i)%getLeafArea()
      end do

   end function
! ################################################################

   function getIntersectLeafSoybean(this, soybeans, light, except) result(Leaf)
      class(Soybean_), intent(inout) :: this
      type(Soybean_), intent(inout) :: soybeans(:)
      type(Light_), optional, intent(in) :: light ! default is z+ direction
      type(Leaf_), allocatable :: leaf(:)
      real(real64), allocatable :: points(:, :), mypoints(:, :)

      integer(int32), optional, intent(in) :: except
      real(real64) :: obj_radius, obj_center(3)
      real(real64) :: chk_radius, chk_center(3), dist_2
      integer(int32) :: i, j, k, num_leaf
      logical, allocatable :: overset(:), overset_leaf(:)

      ! search Intersect leaf
      ! considering light position
      if (present(light)) then
         call this%rotate(z=radian(180.0d0 - light%angles(1)))
         call this%rotate(x=radian(90.0d0 - light%angles(2)))
         if (present(except)) then
            do i = 1, size(soybeans)
               if (i == except) cycle
               call soybeans(i)%rotate(z=radian(180.0d0 - light%angles(1)))
               call soybeans(i)%rotate(x=radian(90.0d0 - light%angles(2)))
            end do
         else
            do i = 1, size(soybeans)
               call soybeans(i)%rotate(z=radian(180.0d0 - light%angles(1)))
               call soybeans(i)%rotate(x=radian(90.0d0 - light%angles(2)))
            end do
         end if

      end if

      obj_radius = this%getRadius()
      obj_center = this%getCenter()

      ! search overlaped soybeans
      allocate (overset(size(soybeans)))
      overset(:) = .false.
      do i = 1, size(soybeans)
         if (soybeans(i)%uuid == this%uuid) cycle
         if (present(except)) then
            if (i == except) cycle
         end if
         chk_radius = soybeans(i)%getRadius()
         chk_Center = soybeans(i)%getCenter()
         dist_2 = norm(obj_center(1:2) - chk_center(1:2))
         if (dist_2 <= chk_radius + obj_radius) then
            ! added 2022/1/29, trial
            points = soybeans(i)%getPoints(leaf=.true., stem=.false., root=.false.)
            mypoints = this%getPoints(leaf=.true., stem=.false., root=.false.)
            ! if a soybean is above mysoy, count
            ! added 2022/1/29, trial
            if (minval(points(:, 3)) >= maxval(mypoints(:, 3))) then
               overset(i) = .true.
            end if
         else
            cycle
         end if
      end do

      ! count number of leaf
      num_leaf = 0
      do i = 1, size(soybeans)
         if (present(except)) then
            if (i == except) cycle
         end if
         if (overset(i)) then
            !allocate(overset_leaf(size(soybeans(i)%leaf ) ))
            !overset_leaf(:) = .false.
            do j = 1, size(soybeans(i)%leaf)
               if (soybeans(i)%leaf(j)%femdomain%empty()) cycle
               chk_radius = soybeans(i)%leaf(j)%getRadius()
               chk_Center = soybeans(i)%leaf(j)%getCenter()

               dist_2 = norm(obj_center(1:2) - chk_center(1:2))
               if (dist_2 <= chk_radius + obj_radius) then
                  !overset_leaf(k) = .true.
                  num_leaf = num_leaf + 1
               else
                  cycle
               end if
            end do
         end if
      end do

      allocate (leaf(num_leaf))
      num_leaf = 0
      do i = 1, size(soybeans)
         if (present(except)) then
            if (i == except) cycle
         end if
         if (overset(i)) then
            do j = 1, size(soybeans(i)%leaf)
               if (soybeans(i)%leaf(j)%femdomain%empty()) cycle

               chk_radius = soybeans(i)%leaf(j)%getRadius()
               chk_Center = soybeans(i)%leaf(j)%getCenter()

               dist_2 = norm(obj_center(1:2) - chk_center(1:2))
               if (dist_2 <= chk_radius + obj_radius) then
                  num_leaf = num_leaf + 1
                  leaf(num_leaf) = soybeans(i)%leaf(j)
               else
                  cycle
               end if
            end do
         end if
      end do

      if (present(light)) then
         call this%rotate(x=-radian(90.0d0 - light%angles(2)))
         call this%rotate(z=-radian(180.0d0 - light%angles(1)))
         if (present(except)) then
            do i = 1, size(soybeans)
               if (i == except) cycle
               call soybeans(i)%rotate(x=-radian(90.0d0 - light%angles(2)))
               call soybeans(i)%rotate(z=-radian(180.0d0 - light%angles(1)))
            end do
         else
            do i = 1, size(soybeans)
               call soybeans(i)%rotate(x=-radian(90.0d0 - light%angles(2)))
               call soybeans(i)%rotate(z=-radian(180.0d0 - light%angles(1)))
            end do
         end if
         do i = 1, size(leaf)
            call leaf(i)%femdomain%rotate(x=-radian(90.0d0 - light%angles(2)))
            call leaf(i)%femdomain%rotate(z=-radian(180.0d0 - light%angles(1)))
         end do
      end if

   end function
! ################################################################

! ################################################################
   pure function getRadiusSoybean(this) result(radius)
      class(Soybean_), intent(in) :: this
      real(real64), allocatable :: Points(:, :)
      real(real64) :: radius, center(3)

      Points = this%getPoints()

      ! search Intersect leaf
      center = this%getCenter()

      Points(:, 1) = Points(:, 1) - center(1)
      Points(:, 2) = Points(:, 2) - center(2)

      radius = maxval(Points(:, 1)*Points(:, 1) + Points(:, 2)*Points(:, 2))
      radius = sqrt(radius)

   end function
! ################################################################

! ################################################################
   pure function getCenterSoybean(this) result(Center)
      class(Soybean_), intent(in) :: this
      real(real64), allocatable :: Points(:, :)
      real(real64) :: center(3)

      Points = this%getPoints()

      ! search Intersect leaf
      center(1) = sum(Points(:, 1))/dble(size(Points, 1))
      center(2) = sum(Points(:, 2))/dble(size(Points, 1))
      center(3) = sum(Points(:, 3))/dble(size(Points, 1))

   end function
! ################################################################

! ################################################################
   subroutine syncSoybean(this, mpid, from)
      class(Soybean_), intent(inout) :: this
      type(MPI_), intent(inout) :: mpid
      integer(int32), intent(in) :: from

      call mpid%BcastMPIcharN(N=20, from=from, val=this%growth_habit(1:20))
      call mpid%BcastMPIcharN(N=2, from=from, val=this%growth_stage(1:2))

      call mpid%bcast(from=from, val=this%Num_Of_Node)
      call mpid%bcast(from=from, val=this%num_leaf)
      call mpid%bcast(from=from, val=this%num_stem_node)
      call mpid%bcast(from=from, val=this%Num_Of_Root)

      call mpid%bcast(from=from, val=this%MaxLeafNum)
      call mpid%bcast(from=from, val=this%MaxRootNum)
      call mpid%bcast(from=from, val=this%MaxStemNum)

      call mpid%bcast(from=from, val=this%determinate)
!!
      call mpid%bcast(from=from, val=this%ms_node)
      call mpid%BcastMPIIntVecFixedSize(from=from, val=this%br_node)
      call mpid%BcastMPIIntVecFixedSize(from=from, val=this%br_from)
!!
      call mpid%bcastMPIReal(from=from, val=this%ms_length)
      call mpid%bcastMPIReal(from=from, val=this%ms_width)
!
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%br_length)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%br_width)
!!
!!
      call mpid%bcast(from=from, val=this%ms_angle_ave)
      call mpid%bcast(from=from, val=this%ms_angle_sig)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%br_angle_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%br_angle_sig)
!!!
!!!
      call mpid%bcast(from=from, val=this%mr_node)
      call mpid%bcast(from=from, val=this%mr_length)
      call mpid%bcast(from=from, val=this%mr_width)
!
      call mpid%BcastMPIIntVecFixedSize(from=from, val=this%brr_node)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%brr_length)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%brr_width)
!
      call mpid%bcast(from=from, val=this%mr_angle_ave)
      call mpid%bcast(from=from, val=this%mr_angle_sig)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%brr_angle_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%brr_angle_sig)
!!
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%peti_size_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%peti_size_sig)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%peti_width_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%peti_width_sig)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%peti_angle_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%peti_angle_sig)
!
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%leaf_angle_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%leaf_angle_sig)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%leaf_length_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%leaf_length_sig)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%leaf_width_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%leaf_width_sig)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%leaf_thickness_ave)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=this%leaf_thickness_sig)
!!
      call mpid%BcastMPICharN(N=3, from=from, val=this%Stage) ! VE, CV, V1,V2, ..., R1, R2, ..., R8
      call mpid%BcastMPICharN(N=200, from=from, val=this%name)
      call mpid%bcast(from=from, val=this%stage_id)
      call mpid%bcast(from=from, val=this%dt)
!
!!
!!        ! material info
      call mpid%bcast(from=from, val=this%stemYoungModulus)
      call mpid%bcast(from=from, val=this%leafYoungModulus)
      call mpid%bcast(from=from, val=this%rootYoungModulus)
!!
      call mpid%bcast(from=from, val=this%stemPoissonRatio)
      call mpid%bcast(from=from, val=this%leafPoissonRatio)
      call mpid%bcast(from=from, val=this%rootPoissonRatio)
!!
      call mpid%bcast(from=from, val=this%stemDensity)
      call mpid%bcast(from=from, val=this%leafDensity)
      call mpid%bcast(from=from, val=this%rootDensity)
!!
      call mpid%bcast(from=from, val=this%leaf2stem)
      call mpid%bcast(from=from, val=this%stem2stem)
      call mpid%bcast(from=from, val=this%root2stem)
      call mpid%bcast(from=from, val=this%root2root)
!!
!
      call mpid%bcast(from=from, val=this%time)
      call mpid%bcast(from=from, val=this%seed_length)
      call mpid%bcast(from=from, val=this%seed_width)
      call mpid%bcast(from=from, val=this%seed_height)
      call mpid%bcast(from=from, val=this%stem_angle)
      call mpid%bcast(from=from, val=this%root_angle)
      call mpid%bcast(from=from, val=this%leaf_angle)
!!
      call mpid%BcastMPICharN(N=200, from=from, val=this%stemconfig)
      call mpid%BcastMPICharN(N=200, from=from, val=this%rootconfig)
      call mpid%BcastMPICharN(N=200, from=from, val=this%leafconfig)
!!
!!        ! for deformation analysis
      call mpid%bcast(from=from, val=this%property_deform_material_density)
      call mpid%bcast(from=from, val=this%property_deform_material_YoungModulus)
      call mpid%bcast(from=from, val=this%property_deform_material_CarbonDiffusionCoefficient)
      call mpid%bcast(from=from, val=this%property_deform_material_PoissonRatio)
      call mpid%bcast(from=from, val=this%property_deform_initial_Displacement)
      call mpid%bcast(from=from, val=this%property_deform_initial_Stress)
      call mpid%bcast(from=from, val=this%property_deform_boundary_TractionForce)
      call mpid%bcast(from=from, val=this%property_deform_boundary_Displacement)
      call mpid%bcast(from=from, val=this%property_deform_gravity)
!!
      call mpid%bcast(from=from, val=this%Gravity_acceralation)
      call mpid%bcast(from=from, val=this%PenaltyParameter)
      call mpid%bcast(from=from, val=this%GaussPointProjection)
!!
!!
!!
      call mpid%bcast(from=from, val=this%NodeID_MainStem)
!
!!
      call mpid%bcast(from=from, val=this%inLoop)
      call mpid%bcast(from=from, val=this%hours)

!        ! 節-節点データ構造
      call this%struct%sync(from=from, mpid=mpid)
!        ! 器官オブジェクト配列
      ! 20251219 廃止
      !call syncFEMDomainVector(this=this%leaf_list(:), from=from, mpid=mpid)
      !call syncFEMDomainVector(this=this%stem_list(:), from=from, mpid=mpid)
      !call syncFEMDomainVector(this=this%root_list(:), from=from, mpid=mpid)
!

!        type(Seed_) :: Seed
!        type(PlantNode_),allocatable :: NodeSystem(:)
!        type(PlantRoot_),allocatable :: RootSystem(:)

      call syncStemVector(obj=this%Stem, from=from, mpid=mpid)
      call syncLeafVector(obj=this%Leaf, from=from, mpid=mpid)
      call syncRootVector(obj=this%Root, from=from, mpid=mpid)

!        ! シミュレータ
!        type(ContactMechanics_) :: contact
      call syncsoybean_NodeID_BranchVector(this%NodeID_Branch, from=from, mpid=mpid)

   end subroutine

! ################################################################
   subroutine syncSoybeans(soybeans, mpid)
      type(MPI_), intent(inout) :: mpid
      type(Soybean_), intent(inout) :: soybeans(:)
      integer(int32) :: id, slot_id, stack_id
      integer(int32), allocatable :: localstack(:)

      if (.not. allocated(mpid%localstack)) then
         call mpid%createstack(size(soybeans))
      end if

      ! 同期
      do slot_id = 1, size(mpid%stack, 1)
         do stack_id = 1, size(mpid%stack, 2)
            id = mpid%stack(slot_id, stack_id)
            if (id == 0) then
               cycle
            else
               call soybeans(id)%sync(from=slot_id - 1, mpid=mpid)
            end if
         end do
      end do

   end subroutine
! ################################################################

   subroutine syncsoybean_NodeID_Branch(this, from, mpid)
      class(soybean_NodeID_Branch_), intent(inout) :: this
      integer(int32), intent(in) :: from
      type(MPI_), intent(inout) :: mpid

      call mpid%bcast(from=from, val=this%id)

   end subroutine
! ################################################################

   subroutine syncsoybean_NodeID_BranchVector(this, from, mpid)
      type(soybean_NodeID_Branch_), allocatable, intent(inout) :: this(:)
      integer(int32), intent(in) :: from
      type(MPI_), intent(inout) :: mpid
      integer(int32) :: vec_size, i

      vec_size = 0
      if (mpid%myrank == from) then
         if (.not. allocated(this)) then
            vec_size = -1
         else
            vec_size = size(this)
         end if
      end if
      call mpid%bcast(from=from, val=vec_size)
      if (vec_size < 1) then
         return
      end if

      if (from /= mpid%myrank) then
         if (allocated(this)) then
            deallocate (this)
         end if
         allocate (this(vec_size))
      end if

      do i = 1, vec_size
         call this(i)%sync(from=from, mpid=mpid)
      end do

   end subroutine

! ################################################################
   subroutine rotateSoybean(this, x, y, z)
      class(Soybean_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      type(FEMDomain_) :: domain

      ! get points
      domain%mesh%nodcoord = this%getpoints()

      ! rotate points
      call domain%rotate(x=x, y=y, z=z)

      ! set points
      call this%setPoints(domain%mesh%nodcoord)

   end subroutine
! ################################################################

! ################################################################
   function getDisplacementSoybean(this, ground_level, penalty, traction_force, debug, itrmax, tol) result(disp)
      class(Soybean_), target, intent(inout) :: this
      real(real64), intent(in) :: ground_level
      real(real64), optional, intent(in) :: penalty, tol, traction_force(:)
      logical, optional, intent(in) ::debug
      integer(int32), optional, intent(in) ::itrmax

      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)
      type(FEMSolver_) :: solver

      real(real64), allocatable :: disp(:)

      integer(int32) :: stem_id, leaf_id, root_id, DomainID, ElementID, i, n, offset
      integer(int32) :: myStemID, yourStemID, myLeafID, myRootID, yourRootID
      integer(int32), allocatable :: FixBoundary(:)
      ! linear elasticity with infinitesimal strain theory
      n = this%numStem() + this%numLeaf() + this%numRoot()
      allocate (FEMDomainPointers(n))

      !(1) >> compute overset
      ! For stems
      if (allocated(this%stem2stem)) then
         do myStemID = 1, size(this%stem2stem, 1)
            do yourStemID = 1, size(this%stem2stem, 2)
               if (this%stem2stem(myStemID, yourStemID) >= 1) then
                  ! connected
                  call this%stem(myStemID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=myStemID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%leaf2stem)) then
         do myLeafID = 1, size(this%leaf2stem, 1)
            do yourStemID = 1, size(this%leaf2stem, 2)
               if (this%leaf2stem(myLeafID, yourStemID) >= 1) then
                  ! connected
                  call this%leaf(myLeafID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=this%numStem() + myLeafID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%root2stem)) then
         do myRootID = 1, size(this%root2stem, 1)
            do yourStemID = 1, size(this%root2stem, 2)
               if (this%root2stem(myRootID, yourStemID) >= 1) then
                  ! connected
                  call this%root(myRootID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=this%numStem() + this%numLeaf() + myRootID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%root2root)) then
         do myRootID = 1, size(this%root2root, 1)
            do yourrootID = 1, size(this%root2root, 2)
               if (this%root2root(myRootID, yourrootID) >= 1) then
                  ! connected
                  call this%root(myRootID)%femdomain%overset( &
                     FEMDomain=this%root(yourrootID)%femdomain, &
                     DomainID=this%numroot() + this%numLeaf() + yourrootID, &
                     MyDomainID=this%numroot() + this%numLeaf() + myRootID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] overset >> done."
         end if
      end if

      call solver%init(NumDomain=this%numStem() + this%numLeaf() + this%numRoot())

      FEMDomainPointers = this%getFEMDomainPointers(algorithm=this%overset_algorithm)
      call solver%setDomain(FEMDomainPointers=FEMDomainPointers)

      if (present(debug)) then
         if (debug) then
            print *, "[ok] initSolver >> done."
         end if
      end if

      call solver%setCRS(DOF=3, debug=debug)

      ! CRS ready!

      if (.not. this%checkYoungModulus()) then
         print *, "[ERROR] YoungModulus(:) is not ready."
         stop
      end if
      if (.not. this%checkPoissonRatio()) then
         print *, "[ERROR] PoissonRatio(:) is not ready."
         stop
      end if
      if (.not. this%checkDensity()) then
         print *, "[ERROR] Density(:) is not ready."
         stop
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] setCRS >> done."
         end if
      end if

    !!$OMP parallel
    !!$OMP do
      !$OMP parallel do private(ElementID)
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix( &
                                  ElementID=ElementID, &
                                  E=this%getYoungModulus(DomainID=DomainID, ElementID=ElementID), &
                                  v=this%getPoissonRatio(DomainID=DomainID, ElementID=ElementID)))

            call solver%setVector(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Vector=FEMDomainPointers(DomainID)%femdomainp%MassVector( &
                                  ElementID=ElementID, &
                                  DOF=FEMDomainPointers(DomainID)%femdomainp%nd(), &
                                  Density=this%getDensity(DomainID=DomainID, ElementID=ElementID), &
                                  Accel=[0.0d0, 0.0d0, -9.80d0] &
                                  ) &
                                  )
         end do
      end do
      !$OMP end parallel do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] set Matrix & vectors >> done."
         end if
      end if

      call solver%setEbOM(penalty=input(default=10000000.0d0, option=penalty), DOF=3)

      if (present(debug)) then
         if (debug) then
            print *, "[ok] set EbOM >> done."
         end if
      end if

      ! traction boundary condition
      if (present(traction_force)) then
         if (size(traction_force) /= size(solver%CRS_RHS)) then
            print *, "[ERROR] > getDisplacementSoybean > (size(traction_force)/=size(solver%CRS_RHS) )"
            stop
         end if
         solver%CRS_RHS(:) = solver%CRS_RHS(:) + traction_force(:)
      end if

      ! fix-boundary conditions
      offset = 0
      do i = 1, size(FEMDomainPointers)
         if (FEMDomainPointers(i)%FEMDomainp%z_min() <= ground_level) then
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 2
            FixBoundary = FixBoundary + offset
            call solver%fix(IDs=FixBoundary, FixValue=0.0d0)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 1
            FixBoundary = FixBoundary + offset
            call solver%fix(IDs=FixBoundary, FixValue=0.0d0)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 0
            FixBoundary = FixBoundary + offset
            call solver%fix(IDs=FixBoundary, FixValue=0.0d0)
         end if
         offset = offset + FEMDomainPointers(i)%femdomainp%nn()*3
      end do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] FixBoundary >> done."
         end if
      end if

      if (present(debug)) then
         solver%debug = debug
      end if
      if (present(itrmax)) then
         solver%itrmax = itrmax
      end if

      if (present(tol)) then
         solver%er0 = tol
      end if

      print *, "dbf"

      disp = solver%solve()

      call solver%remove()

      if (present(debug)) then
         if (debug) then
            print *, "[ok] Solve >> done."
         end if
      end if
      ! japanese "ato-shimatsu"

   end function
! ################################################################

! ################################################################
   function getFEMDomainPointersSoybean(this, algorithm) result(FEMDomainPointers)
      class(Soybean_), target, intent(inout) :: this
      integer(int32), optional, intent(in) :: algorithm

      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)
      integer(int32) :: num_FEMDomain, i, n, yourStemID, myStemID, yourLeafID, myLeafID, &
                        yourRootID, myRootID, EbO_Algorithm

      EbO_algorithm = input(default=FEMDomain_Overset_GPP, option=algorithm)

      num_FEMDomain = this%numStem() + this%numLeaf() + this%numRoot()
      allocate (FEMDomainPointers(num_FEMDomain))
      n = 0
      do i = 1, this%numStem()
         if (.not. this%stem(i)%femdomain%empty()) then
            n = n + 1
            FEMDomainPointers(n)%femdomainp => this%stem(i)%femdomain
         end if
      end do
      do i = 1, this%numLeaf()
         if (.not. this%leaf(i)%femdomain%empty()) then
            n = n + 1
            FEMDomainPointers(n)%femdomainp => this%leaf(i)%femdomain
         end if
      end do
      do i = 1, this%numRoot()
         if (.not. this%root(i)%femdomain%empty()) then
            n = n + 1
            FEMDomainPointers(n)%femdomainp => this%root(i)%femdomain
         end if
      end do

      ! for overset
      n = this%numStem() + this%numLeaf() + this%numRoot()

      !(1) >> compute overset
      ! For stems
      if (allocated(this%stem2stem)) then
         do myStemID = 1, size(this%stem2stem, 1)
            do yourStemID = 1, size(this%stem2stem, 2)
               if (this%stem2stem(myStemID, yourStemID) >= 1) then
                  ! connected
                  call this%stem(myStemID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=myStemID, &
                     algorithm=this%overset_algorithm)
               end if
            end do
         end do
      end if

      if (allocated(this%leaf2stem)) then
         do myLeafID = 1, size(this%leaf2stem, 1)
            do yourStemID = 1, size(this%leaf2stem, 2)
               if (this%leaf2stem(myLeafID, yourStemID) >= 1) then
                  ! connected
                  call this%leaf(myLeafID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=this%numStem() + myLeafID, &
                     algorithm=this%overset_algorithm)
               end if
            end do
         end do
      end if

      if (allocated(this%root2stem)) then
         do myRootID = 1, size(this%root2stem, 1)
            do yourStemID = 1, size(this%root2stem, 2)
               if (this%root2stem(myRootID, yourStemID) >= 1) then
                  ! connected
                  call this%root(myRootID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=this%numStem() + this%numLeaf() + myRootID, &
                     algorithm=this%overset_algorithm)
               end if
            end do
         end do
      end if

      if (allocated(this%root2root)) then
         do myRootID = 1, size(this%root2root, 1)
            do yourrootID = 1, size(this%root2root, 2)
               if (this%root2root(myRootID, yourrootID) >= 1) then
                  ! connected
                  call this%root(myRootID)%femdomain%overset( &
                     FEMDomain=this%root(yourrootID)%femdomain, &
                     DomainID=this%numroot() + this%numLeaf() + yourrootID, &
                     MyDomainID=this%numroot() + this%numLeaf() + myRootID, &
                     algorithm=this%overset_algorithm)
               end if
            end do
         end do
      end if

   end function
! ################################################################

! ################################################################
   function getObjectPointersSoybean(this) result(FEMDomainPointers)
      class(Soybean_), target, intent(in) :: this
      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)
      integer(int32) :: num_FEMDomain, i, n

      ! order: stem -> leaf -> root
      num_FEMDomain = this%numStem() + this%numLeaf() + this%numRoot()
      allocate (FEMDomainPointers(num_FEMDomain))
      n = 0
      do i = 1, this%numStem()
         n = n + 1
         FEMDomainPointers(n)%femdomainp => this%stem(i)%femdomain
      end do
      do i = 1, this%numLeaf()
         n = n + 1
         FEMDomainPointers(n)%femdomainp => this%leaf(i)%femdomain
      end do
      do i = 1, this%numRoot()
         n = n + 1
         FEMDomainPointers(n)%femdomainp => this%root(i)%femdomain
      end do
   end function
! ################################################################

! ################################################################
   function checkYoungModulusSoybean(this) result(all_young_modulus_is_set)
      class(Soybean_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusSoybean >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusSoybean >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusSoybean >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

   end function
! ################################################################

! ################################################################
   function checkPoissonRatioSoybean(this) result(all_young_modulus_is_set)
      class(Soybean_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioSoybean >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioSoybean >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioSoybean >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

   end function
! ################################################################

! ################################################################
   function checkDensitySoybean(this) result(all_young_modulus_is_set)
      class(Soybean_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensitySoybean >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%Density)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensitySoybean >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%Density)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensitySoybean >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%Density)"
            return
         end if
      end do

   end function
! ################################################################

! ################################################################
   function getYoungModulusSoybean(this, DomainID, ElementID) result(YoungModulus)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: DomainID, ElementID
      real(real64) :: YoungModulus
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot()) then
         print *, "ERROR :: getYoungModulusSoybean >>  DomainID exceeds max_domain_size"
         return
      end if

      ! default >> search @ all domains
      ! order: stem -> leaf -> root
      if (DomainID <= this%numStem()) then
         n = DomainID - 0
         YoungModulus = this%stem(n)%YoungModulus(ElementID)
         return
      elseif (this%numStem() + 1 <= DomainID .and. DomainID <= this%numStem() + this%numLeaf()) then
         n = DomainID - this%numStem()
         YoungModulus = this%leaf(n)%YoungModulus(ElementID)
         return
      else
         n = DomainID - this%numStem() - this%numLeaf()
         YoungModulus = this%root(n)%YoungModulus(ElementID)
         return
      end if

   end function
! ################################################################

! ################################################################
   function getPoissonRatioSoybean(this, DomainID, ElementID) result(PoissonRatio)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: DomainID, ElementID
      real(real64) :: PoissonRatio
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot()) then
         print *, "ERROR :: getPoissonRatioSoybean >>  DomainID exceeds max_domain_size"
         return
      end if

      ! default >> search @ all domains
      ! order: stem -> leaf -> root
      if (DomainID <= this%numStem()) then
         n = DomainID - 0
         PoissonRatio = this%stem(n)%PoissonRatio(ElementID)
         return
      elseif (this%numStem() + 1 <= DomainID .and. DomainID <= this%numStem() + this%numLeaf()) then
         n = DomainID - this%numStem()
         PoissonRatio = this%leaf(n)%PoissonRatio(ElementID)
         return
      else
         n = DomainID - this%numStem() - this%numLeaf()
         PoissonRatio = this%root(n)%PoissonRatio(ElementID)
         return
      end if

   end function
! ################################################################

! ################################################################
   function getDensitySoybean(this, DomainID, ElementID) result(Density)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: DomainID, ElementID
      real(real64) :: Density
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot()) then
         print *, "ERROR :: getDensitySoybean >>  DomainID exceeds max_domain_size"
         return
      end if

      ! default >> search @ all domains
      ! order: stem -> leaf -> root
      if (DomainID <= this%numStem()) then
         n = DomainID - 0
         Density = this%stem(n)%Density(ElementID)
         return
      elseif (this%numStem() + 1 <= DomainID .and. DomainID <= this%numStem() + this%numLeaf()) then
         n = DomainID - this%numStem()
         Density = this%leaf(n)%Density(ElementID)
         return
      else
         n = DomainID - this%numStem() - this%numLeaf()
         Density = this%root(n)%Density(ElementID)
         return
      end if

   end function
! ################################################################
   subroutine checkMemoryRequirementSoybean(this)
      class(Soybean_), intent(in) :: this
      real(real64) :: re_val
      integer(int64) :: val

      print *, "===================================="
      print *, "checking Memory (RAM) Requirement..."
      print *, "------------------------------------"
      print *, "| Object type                     | Soybean"
      print *, "| Number of points                | "+str(this%nn())
      print *, "| Degree of freedom | Deformation | "+str(this%nn()*3)
      print *, "|                   | ModeAnalysis| "+str(this%nn()*3*this%nn()*3)
      print *, "|                   | Diffusion   | "+str(this%nn())
      print *, "|                   | Reaction    | "+str(this%nn())

      print *, "| DRAM requirement  | Deformation | "+str(this%nn()*3*40*30/1000/1000) + " (MB)"
      val = this%nn()*3*30
      val = val*this%nn()*3/1000/1000
      print *, "|                   | ModeAnalysis| ", str(val), " (MB)"
      print *, "|                   | Diffusion   | "+str(this%nn()*1*20*10/1000/1000) + " (MB)"
      print *, "|                   | Reaction    | "+str(this%nn()*1*20*10/1000/1000) + " (MB)"
      print *, "===================================="

   end subroutine

! ################################################################
   recursive subroutine setYoungModulusSoybean(this, YoungModulus, stem, root, leaf, ElementList)
      class(Soybean_), intent(inout) :: this
      logical, optional, intent(in) :: stem, root, leaf

      ! ElementList(Idx, [TYPE, DOMAIN, ELEMENT])
      integer(int32), optional, intent(in) :: ElementList(:, :)

      real(real64), intent(in) :: YoungModulus
      integer(int32) :: i, j, n, domain_idx, elem_idx

      n = 0
      if (present(stem)) then
         if (stem) then
            n = n + 1
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (this%stem(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_STEM) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%stem(domain_idx)%YoungModulus(elem_idx) = YoungModulus
                        end if
                     end do
                  else
                     this%stem(i)%YoungModulus = YoungModulus*eyes(this%stem(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(leaf)) then
         if (leaf) then
            n = n + 10
            if (allocated(this%leaf)) then
               do i = 1, size(this%leaf)
                  if (this%leaf(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_LEAF) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%LEAF(domain_idx)%YoungModulus(elem_idx) = YoungModulus
                        end if
                     end do
                  else
                     this%leaf(i)%YoungModulus = YoungModulus*eyes(this%leaf(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(root)) then
         if (root) then
            n = n + 100
            if (allocated(this%root)) then
               do i = 1, size(this%root)
                  if (this%root(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_ROOT) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%ROOT(domain_idx)%YoungModulus(elem_idx) = YoungModulus
                        end if
                     end do
                  else
                     this%root(i)%YoungModulus = YoungModulus*eyes(this%root(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (n == 0) then
         call this%setYoungModulus(YoungModulus=YoungModulus, stem=.true., root=.true., leaf=.true., &
                                  ElementList=ElementList)
      end if

   end subroutine
! ################################################################
! ################################################################
   recursive subroutine setPoissonRatioSoybean(this, PoissonRatio, stem, root, leaf, ElementList)
      class(Soybean_), intent(inout) :: this
      logical, optional, intent(in) :: stem, root, leaf

      ! ElementList(Idx, [TYPE, DOMAIN, ELEMENT])
      integer(int32), optional, intent(in) :: ElementList(:, :)

      real(real64), intent(in) :: PoissonRatio
      integer(int32) :: i, j, n, domain_idx, elem_idx

      n = 0
      if (present(stem)) then
         if (stem) then
            n = n + 1
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (this%stem(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_STEM) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%stem(domain_idx)%PoissonRatio(elem_idx) = PoissonRatio
                        end if
                     end do
                  else
                     this%stem(i)%PoissonRatio = PoissonRatio*eyes(this%stem(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(leaf)) then
         if (leaf) then
            n = n + 10
            if (allocated(this%leaf)) then
               do i = 1, size(this%leaf)
                  if (this%leaf(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_LEAF) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%LEAF(domain_idx)%PoissonRatio(elem_idx) = PoissonRatio
                        end if
                     end do
                  else
                     this%leaf(i)%PoissonRatio = PoissonRatio*eyes(this%leaf(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(root)) then
         if (root) then
            n = n + 100
            if (allocated(this%root)) then
               do i = 1, size(this%root)
                  if (this%root(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_ROOT) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%ROOT(domain_idx)%PoissonRatio(elem_idx) = PoissonRatio
                        end if
                     end do
                  else
                     this%root(i)%PoissonRatio = PoissonRatio*eyes(this%root(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (n == 0) then
         call this%setPoissonRatio(PoissonRatio=PoissonRatio, stem=.true., root=.true., leaf=.true., &
                                  ElementList=ElementList)
      end if

   end subroutine
! ################################################################

! ################################################################
   recursive subroutine setDensitySoybean(this, Density, stem, root, leaf, ElementList)
      class(Soybean_), intent(inout) :: this
      logical, optional, intent(in) :: stem, root, leaf

      ! ElementList(Idx, [TYPE, DOMAIN, ELEMENT])
      integer(int32), optional, intent(in) :: ElementList(:, :)

      real(real64), intent(in) :: Density
      integer(int32) :: i, j, n, domain_idx, elem_idx

      n = 0
      if (present(stem)) then
         if (stem) then
            n = n + 1
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (this%stem(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_STEM) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%stem(domain_idx)%Density(elem_idx) = Density
                        end if
                     end do
                  else
                     this%stem(i)%Density = Density*eyes(this%stem(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(leaf)) then
         if (leaf) then
            n = n + 10
            if (allocated(this%leaf)) then
               do i = 1, size(this%leaf)
                  if (this%leaf(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_LEAF) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%LEAF(domain_idx)%Density(elem_idx) = Density
                        end if
                     end do
                  else
                     this%leaf(i)%Density = Density*eyes(this%leaf(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(root)) then
         if (root) then
            n = n + 100
            if (allocated(this%root)) then
               do i = 1, size(this%root)
                  if (this%root(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_ROOT) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%ROOT(domain_idx)%Density(elem_idx) = Density
                        end if
                     end do
                  else
                     this%root(i)%Density = Density*eyes(this%root(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (n == 0) then
         call this%setDensity(Density=Density, stem=.true., root=.true., leaf=.true., &
                             ElementList=ElementList)
      end if

   end subroutine
! ################################################################

! ################################################################
   function getEigenModeSoybean(this, ground_level, penalty, debug, Frequency, EbOM_Algorithm, &
                                num_mode, femsolver) result(EigenVectors)
      class(Soybean_), target, intent(inout) :: this
      real(real64), intent(in) :: ground_level
      real(real64), optional, intent(in) :: penalty
      logical, optional, intent(in) :: debug
      real(real64), allocatable, intent(inout) :: Frequency(:)
      character(*), optional, intent(in) :: EbOM_Algorithm
      !integer(int32),optional,intent(in) :: num_mode

      integer(int32), optional, intent(in) :: num_mode
      type(FEMSolver_), optional, intent(inout):: femsolver

      integer(int32) :: num_freq

      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)

      type(FEMSolver_) :: solver
      type(Math_) :: math

      real(real64), allocatable :: EigenVectors(:, :), buf(:, :), buf_vec(:)

      integer(int32) :: stem_id, leaf_id, root_id, DomainID, ElementID, i, n
      integer(int32) :: myStemID, yourStemID, myLeafID, myRootID, yourRootID
      integer(int32), allocatable :: FixBoundary(:)
      integer(int32) :: nn_domains, EbOM_Algorithm_id
      real(real64) :: vec_norm
      real(real64), allocatable :: all_frequency(:), All_EigenVectors(:, :)

      if (present(femsolver)) then
         solver = femsolver
      end if

      num_freq = input(default=10, option=num_mode)

      
      if (present(EbOM_Algorithm)) then
         if("P2P" .in. EbOM_Algorithm)then
            this%overset_algorithm = FEMDomain_Overset_P2P
         else
            this%overset_algorithm = FEMDomain_Overset_GPP
         endif
      end if

      ! linear elasticity with infinitesimal strain theory
      n = this%numStem() + this%numLeaf() + this%numRoot()
      allocate (FEMDomainPointers(n))

      !(1) >> compute overset
      ! For stems
      if (allocated(this%stem2stem)) then
         if (allocated(this%stem)) then
            do myStemID = 1, size(this%stem2stem, 1)
               do yourStemID = 1, size(this%stem2stem, 2)
                  if (this%stem2stem(myStemID, yourStemID) >= 1) then
                     ! connected
                     call this%stem(myStemID)%femdomain%overset( &
                        FEMDomain=this%stem(yourStemID)%femdomain, &
                        DomainID=yourStemID, &
                        MyDomainID=myStemID, &
                        algorithm=this%overset_algorithm) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%stem(myStemID)%femdomain, &
                        DomainID=myStemID, &
                        MyDomainID=yourStemID, &
                        algorithm=this%overset_algorithm) ! or "P2P"
                  end if
               end do
            end do
         end if
      end if

      if (allocated(this%leaf2stem)) then
         if (allocated(this%leaf) .and. allocated(this%stem)) then
            do myLeafID = 1, size(this%leaf2stem, 1)
               do yourStemID = 1, size(this%leaf2stem, 2)
                  if (this%leaf2stem(myLeafID, yourStemID) >= 1) then
                     ! connected
                     call this%leaf(myLeafID)%femdomain%overset( &
                        FEMDomain=this%stem(yourStemID)%femdomain, &
                        DomainID=yourStemID, &
                        MyDomainID=this%numStem() + myLeafID, &
                        algorithm=this%overset_algorithm) ! or "P2P"
                     !call this%stem(yourStemID)%femdomain%overset(&
                     !    FEMDomain=this%leaf(myLeafID)%femdomain,
                     !    DomainID =this%numStem() + myLeafID,
                     !    MyDomainID= yourStemID,
                     !    algorithm=EbOM_Algorithm_id ) ! or "P2P"

                  end if
               end do
            end do
         end if
      end if

      if (allocated(this%root2stem)) then
         if (allocated(this%stem) .and. allocated(this%root)) then
            do myRootID = 1, size(this%root2stem, 1)
               do yourStemID = 1, size(this%root2stem, 2)
                  if (this%root2stem(myRootID, yourStemID) >= 1) then
                     ! connected
                     call this%root(myRootID)%femdomain%overset( &
                        FEMDomain=this%stem(yourStemID)%femdomain, &
                        DomainID=yourStemID, &
                        MyDomainID=this%numStem() + this%numLeaf() + myRootID, &
                        algorithm=this%overset_algorithm) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%root(myRootID)%femdomain, &
                        DomainID=this%numStem() + this%numLeaf() + myRootID, &
                        MyDomainID=yourStemID, &
                        algorithm=this%overset_algorithm) ! or "P2P"
                  end if
               end do
            end do
         end if
      end if

      if (allocated(this%root2root)) then
         if (allocated(this%root)) then
            do myRootID = 1, size(this%root2root, 1)
               do yourrootID = 1, size(this%root2root, 2)
                  if (this%root2root(myRootID, yourrootID) >= 1) then
                     ! connected
                     call this%root(myRootID)%femdomain%overset( &
                        FEMDomain=this%root(yourrootID)%femdomain, &
                        DomainID=this%numroot() + this%numLeaf() + yourrootID, &
                        MyDomainID=this%numroot() + this%numLeaf() + myRootID, &
                        algorithm=this%overset_algorithm) ! or "P2P"

                     call this%root(yourrootID)%femdomain%overset( &
                        FEMDomain=this%root(myRootID)%femdomain, &
                        DomainID=this%numroot() + this%numLeaf() + myRootID, &
                        MyDomainID=this%numroot() + this%numLeaf() + yourrootID, &
                        algorithm=this%overset_algorithm) ! or "P2P"
                  end if
               end do
            end do
         end if
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] overset >> done."
         end if
      end if

      call solver%init(NumDomain=this%numStem() + this%numLeaf() + this%numRoot())

      FEMDomainPointers = this%getFEMDomainPointers(algorithm=this%overset_algorithm)
      call solver%setDomain(FEMDomainPointers=FEMDomainPointers)

      if (present(debug)) then
         if (debug) then
            print *, "[ok] initSolver >> done."
         end if
      end if

      call solver%setCRS(DOF=3, debug=debug)

      ! CRS ready!

      if (.not. this%checkYoungModulus()) then
         print *, "[ERROR] YoungModulus(:) is not ready."
         stop
      end if
      if (.not. this%checkPoissonRatio()) then
         print *, "[ERROR] PoissonRatio(:) is not ready."
         stop
      end if
      if (.not. this%checkDensity()) then
         print *, "[ERROR] Density(:) is not ready."
         stop
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] setCRS >> done."
         end if
      end if

    !!$OMP parallel
    !!$OMP do
      !$OMP parallel do private(ElementID)
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix( &
                                  ElementID=ElementID, &
                                  E=this%getYoungModulus(DomainID=DomainID, ElementID=ElementID), &
                                  v=this%getPoissonRatio(DomainID=DomainID, ElementID=ElementID)))
         end do
      end do
      !$OMP end parallel do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] set Matrix & vectors >> done."
         end if
      end if

      call solver%setEbOM(penalty=input(default=10000000.0d0, option=penalty), DOF=3)

      if (present(debug)) then
         if (debug) then
            print *, "[ok] set EbOM >> done."
         end if
      end if

      call solver%keepThisMatrixAs("A")
      !call solver%saveMatrix(name="A",CRS_as_dense=.true.,zero_or_nonzero=.true)
      call solver%zeros()

      ! mass matrix
      !$OMP parallel do private(ElementID)
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%massMatrix( &
                                  ElementID=ElementID, &
                                  Density=this%getDensity(DomainID=DomainID, ElementID=ElementID), &
                                  DOF=3))
         end do
      end do
      !$OMP end parallel do
      call solver%keepThisMatrixAs("B")

      ! fix-boundary conditions
      nn_domains = 0
      do i = 1, size(FEMDomainPointers)
         if (FEMDomainPointers(i)%FEMDomainp%z_min() <= ground_level) then
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 2 + nn_domains*3
            call solver%fix_eig(IDs=FixBoundary)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 1 + nn_domains*3
            call solver%fix_eig(IDs=FixBoundary)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 0 + nn_domains*3
            call solver%fix_eig(IDs=FixBoundary)
         end if
         nn_domains = nn_domains + FEMDomainPointers(i)%FEMDomainp%nn()
      end do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] FixBoundary >> done."
         end if
      end if

      if (present(debug)) then
         solver%debug = debug
      end if

      call solver%eig(eigen_value=All_Frequency, eigen_vectors=All_EigenVectors)

      if (present(femsolver)) then
         femsolver = solver
      end if

      call solver%remove()

      if (All_Frequency(1) <= 0.0d0) then
         return
      end if

      ! simplify this part
      ! normalize EigenVectors
      do i = 1, size(All_EigenVectors, 2)
         vec_norm = norm(All_EigenVectors(:, i))
         All_EigenVectors(:, i) = All_EigenVectors(:, i)/vec_norm
      end do

      Frequency = zeros(num_freq)
      EigenVectors = zeros(size(All_EigenVectors, 1), num_freq)

      do i = 1, num_freq
         n = minvalID(All_Frequency)
         EigenVectors(:, i) = All_EigenVectors(:, n)
         Frequency(i) = All_Frequency(n)
         All_Frequency(n) = maxval(All_Frequency)
      end do

      do i = 1, size(Frequency)
         if (Frequency(i) < 0.0d0) then
            Frequency(i) = 0.0d0
         end if
      end do
      Frequency = sqrt((Frequency))/(2.0d0*math%PI)

      if (present(debug)) then
         if (debug) then
            print *, "[ok] Solve >> done."
         end if
      end if

   end function
! ################################################################

   subroutine resizeStemSoybean(this, StemID, InterNodeID, Length, Width)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: stemID, InterNodeID
      real(real64), optional, intent(in) :: Length, Width
      real(real64) :: current_length
      integer(int32) :: i, j, node_id

      node_id = 0
      do i = 1, size(this%stem, 1)
         if (this%stem(i)%stemID == StemID) then
            if (this%stem(i)%InterNodeID == InterNodeID) then
               node_id = i
            end if
         end if
      end do
      if (node_id == 0) then
         print *, "resizeStemSoybean 404 Not Found."
         return
      end if

      call this%stem(node_id)%change_length_or_width(length=Length, Width=Width)
      call this%update()

   end subroutine

   subroutine rotateStemSoybean(this, StemID, InterNodeID, Angles)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: stemID, InterNodeID
      real(real64), intent(in) :: Angles(1:3)
      real(real64) :: current_length
      integer(int32) :: i, j, node_id

      do i = 1, size(this%stem, 1)
         if (this%stem(i)%stemID == StemID) then
            if (this%stem(i)%InterNodeID == InterNodeID) then
               node_id = i
            end if
         end if
      end do

      if (node_id == 0) then
         print *, "resizeStemSoybean 404 Not Found."
         return
      end if

      call this%stem(node_id)%femdomain%rotate(x=Angles(1), y=Angles(2), z=Angles(3), deg=.true.)
      call this%update()

   end subroutine

   function searchStemSoybean(this, StemID, InterNodeID) result(node_id)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: stemID, InterNodeID
      real(real64) :: current_length
      integer(int32) :: i, j, node_id

      node_id = -404

      if (StemID == 0) then
         if (1 <= InterNodeID .and. InterNodeID <= size(this%NodeID_MainStem)) then
            node_id = this%NodeID_MainStem(InterNodeID)
         else
            return
         end if
      else
         if (.not. allocated(this%NodeID_Branch)) then
            return
         end if
         if (1 <= StemID .and. StemID <= size(this%NodeID_Branch)) then
            if (allocated(this%NodeID_Branch(StemID)%ID)) then
               node_id = this%NodeID_Branch(StemID)%ID(InterNodeID)
            end if
         end if
      end if

!    do i=1,size(this%stem,1)
!        if(this%stem(i)%stemID==StemID)then
!            if(this%stem(i)%InterNodeID==InterNodeID)then
!                node_id = i
!            endif
!        endif
!    enddo

   end function

   function searchPetioleSoybean(this, StemID, InterNodeID, PetioleID) result(node_id)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: stemID, InterNodeID, PetioleID
      real(real64) :: current_length
      integer(int32) :: i, j, node_id, n

      node_id = -404

      node_id = this%searchStem(StemID=StemID, InterNodeID=InterNodeID)

      if (node_id < 0) then
         return
      end if

      n = 0
      do i = 1, size(this%stem2stem, 1)
         if (this%stem2stem(i, node_id) >= 1 &
             .and. this%stem(i)%StemID == -1) then
            n = n + 1
            if (n == PetioleID) then
               node_id = i
               return
            end if
         end if
      end do

      node_id = -404404

   end function

   function searchLeafSoybean(this, StemID, InterNodeID, PetioleID, LeafID) result(leaf_id)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: stemID, InterNodeID, PetioleID, LeafID
      real(real64) :: current_length
      integer(int32) :: i, j, node_id, n, petiole_id, leaf_id

      leaf_id = -404404404
      petiole_id = -404404
      node_id = -404

      petiole_id = this%searchPetiole( &
                   StemID=StemID, & ! main=0, branch=1,2 ...
                   InterNodeID=InterNodeID, & ! 1,2,3...
                   PetioleID=PetioleID &
                   )

      if (petiole_id < 0) then
         leaf_id = -404
         return
      end if

      do i = 1, size(this%leaf2stem, 1)
         if (this%leaf2stem(i, petiole_id) == 1) then
            if (this%leaf(i)%leafID == LeafID) then
               leaf_id = i
            end if
         end if
      end do

   end function

! #############################################################################
   subroutine resizePetioleSoybean(this, StemID, InterNodeID, PetioleID, Length, Width)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: stemID, InterNodeID, PetioleID
      real(real64), optional, intent(in) :: Length, Width
      real(real64) :: current_length
      integer(int32) :: i, j, node_id

      node_id = this%searchPetiole(StemID=StemID, InterNodeID=InterNodeID, PetioleID=PetioleID)

      if (node_id < 1) then
         print *, "resizePetioleSoybean 404 Not Found."
         return
      end if

      call this%stem(node_id)%change_length_or_width(length=Length, Width=Width)
      call this%update()

   end subroutine
! #############################################################################

! #############################################################################
   subroutine rotatePetioleSoybean(this, StemID, InterNodeID, PetioleID, Angles)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: stemID, InterNodeID, PetioleID
      real(real64), intent(in) :: Angles(1:3)
      real(real64) :: current_length
      integer(int32) :: i, j, node_id

      node_id = this%searchPetiole(StemID=StemID, InterNodeID=InterNodeID, PetioleID=PetioleID)

      if (node_id < 1) then
         print *, "rotatePetioleSoybean 404 Not Found."
         return
      end if

      call this%stem(node_id)%femdomain%rotate(x=Angles(1), y=Angles(2), z=Angles(3), deg=.true.)
      call this%update()

   end subroutine
! #########################################################

   subroutine resizeLeafSoybean(this, StemID, InterNodeID, PetioleID, LeafID, Length, Width)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: stemID, InterNodeID, PetioleID, LeafID
      real(real64), optional, intent(in) :: Length, Width
      real(real64) :: current_length
      integer(int32) :: i, j, leaf_id

      leaf_id = this%searchLeaf(StemID=StemID, InterNodeID=InterNodeID, PetioleID=PetioleID, &
                                LeafID=LeafID)

      if (leaf_id < 1) then
         print *, "resizeLeafSoybean 404 Not Found."
         return
      end if

      call this%leaf(leaf_id)%resize(length=Length, Width=Width)

      call this%update()

   end subroutine

! #########################################################
   function maxStemIDSoybean(this) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32) :: ret, i, buf

      ret = 0
      do i = 1, size(this%Stem, 1)
         buf = this%maxInterNodeID(StemID=i)
         if (buf >= 1) then
            ret = ret + 1
         else
            return
         end if
      end do

   end function
! #########################################################

! #########################################################
   function maxInterNodeIDSoybean(this, StemID) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: StemID
      integer(int32) :: ret, i, buf

      ret = 0
      if (StemID == 0) then
         if (allocated(this%NodeID_MainStem)) then
            ret = size(this%NodeID_MainStem)
         else
            ret = 0
         end if
      else
         if (allocated(this%NodeID_Branch)) then
            ret = size(this%NodeID_Branch(StemID)%ID)
         else
            ret = 0
         end if
      end if

!    do i=1, size(this%Stem,1)
!        buf = this%searchStem(StemID=StemID,InterNodeID=i)
!        if(buf >=1)then
!            ret = ret + 1
!        else
!            cycle
!        endif
!    enddo

   end function
! #########################################################

! #########################################################
   function maxPetioleIDSoybean(this, StemID, InterNodeID) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: StemID, InterNodeID
      integer(int32) :: ret, i, buf, PerioleID

      ret = 0
      do i = 1, size(this%Stem, 1)
         buf = this%searchPetiole(StemID=StemID, InterNodeID=InterNodeID, PetioleID=i)
         if (buf >= 1) then
            ret = ret + 1
         else
            return
         end if
      end do

   end function
! #########################################################

! #########################################################
   function maxleafIDSoybean(this, StemID, InterNodeID, PetioleID) result(ret)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: StemID, InterNodeID, PetioleID
      integer(int32) :: ret, i, buf, PerioleID

      ret = 0
      do i = 1, size(this%Leaf, 1)
         buf = this%searchLeaf(StemID=StemID, InterNodeID=InterNodeID, PetioleID=PetioleID, &
                               LeafID=i)
         if (buf >= 1) then
            ret = ret + 1
         else
            return
         end if
      end do

   end function
! #########################################################

! ################################################################

   subroutine growStemSoybean(this, StemID, InterNodeID, dt)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: stemID, InterNodeID
      real(real64), intent(in) :: dt
      real(real64) :: current_length
      integer(int32) :: i, j, node_id

      node_id = 0
      do i = 1, size(this%stem, 1)
         if (this%stem(i)%stemID == StemID) then
            if (this%stem(i)%InterNodeID == InterNodeID) then
               node_id = i
            end if
         end if
      end do
      if (node_id == 0) then
         print *, "resizeStemSoybean 404 Not Found."
         return
      end if

      call this%stem(node_id)%change_length_or_width(dt)
      call this%update()

   end subroutine
! #############################################
   subroutine setFinalInternodeLengthSoybean(this, Length, StemID)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: StemID
      real(real64), intent(in)   :: Length(:)

      if (.not. allocated(this%InterNodeInfo)) then
         allocate (this%InterNodeInfo(0:this%MaxBranchNum))
      end if

      this%InterNodeInfo(StemID)%FinalInterNodeLength = Length

   end subroutine

! #############################################
   subroutine setFinalPetioleLengthSoybean(this, Length, StemID)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: StemID
      real(real64), intent(in)   :: Length(:)

      if (.not. allocated(this%InterNodeInfo)) then
         allocate (this%InterNodeInfo(0:this%MaxBranchNum))
      end if

      this%InterNodeInfo(StemID)%FinalPetioleLength = Length

   end subroutine

! #############################################
   subroutine setFinalLeafLengthSoybean(this, Length, StemID)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: StemID
      real(real64), intent(in)   :: Length(:)

      if (.not. allocated(this%InterNodeInfo)) then
         allocate (this%InterNodeInfo(0:this%MaxBranchNum))
      end if

      this%InterNodeInfo(StemID)%FinalLeafLength = Length

   end subroutine

! #############################################
   subroutine setFinalLeafWidthSoybean(this, Width, StemID)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: StemID
      real(real64), intent(in)   :: Width(:)

      if (.not. allocated(this%InterNodeInfo)) then
         allocate (this%InterNodeInfo(0:this%MaxBranchNum))
      end if

      this%InterNodeInfo(StemID)%FinalLeafWidth = Width

   end subroutine

! ################################################################

   function getYoungModulusFieldSoybean(this) result(YoungModulus)
      class(Soybean_), intent(inout) :: this
      real(real64), allocatable :: YoungModulus(:)
      integer(int32), allocatable :: ElementList(:, :)
      integer(int32) :: TYPE_IDX, DOMAIN_IDX, ELEMENT_IDX, i

      ElementList = this%getElementList()
      YoungModulus = zeros(size(ElementList, 1))

      do i = 1, size(ElementList, 1)
         TYPE_IDX = ElementList(i, 1)
         DOMAIN_IDX = ElementList(i, 2)
         ELEMENT_IDX = ElementList(i, 3)
         if (TYPE_IDX == this%TYPE_STEM) then
            YoungModulus(i) = this%stem(DOMAIN_IDX)%YoungModulus(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_LEAF) then
            YoungModulus(i) = this%LEAF(DOMAIN_IDX)%YoungModulus(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_ROOT) then
            YoungModulus(i) = this%ROOT(DOMAIN_IDX)%YoungModulus(ELEMENT_IDX)
         end if
      end do

   end function

! ################################################################

! ################################################################
   function getPoissonRatioFieldSoybean(this) result(PoissonRatio)
      class(Soybean_), intent(inout) :: this
      real(real64), allocatable :: PoissonRatio(:)
      integer(int32), allocatable :: ElementList(:, :)
      integer(int32) :: TYPE_IDX, DOMAIN_IDX, ELEMENT_IDX, i

      ElementList = this%getElementList()
      PoissonRatio = zeros(size(ElementList, 1))

      do i = 1, size(ElementList, 1)
         TYPE_IDX = ElementList(i, 1)
         DOMAIN_IDX = ElementList(i, 2)
         ELEMENT_IDX = ElementList(i, 3)
         if (TYPE_IDX == this%TYPE_STEM) then
            PoissonRatio(i) = this%stem(DOMAIN_IDX)%PoissonRatio(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_LEAF) then
            PoissonRatio(i) = this%LEAF(DOMAIN_IDX)%PoissonRatio(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_ROOT) then
            PoissonRatio(i) = this%ROOT(DOMAIN_IDX)%PoissonRatio(ELEMENT_IDX)
         end if
      end do

   end function

! ################################################################

! ################################################################
   function getDensityFieldSoybean(this) result(Density)
      class(Soybean_), intent(inout) :: this
      real(real64), allocatable :: Density(:)
      integer(int32), allocatable :: ElementList(:, :)
      integer(int32) :: TYPE_IDX, DOMAIN_IDX, ELEMENT_IDX, i

      ElementList = this%getElementList()
      Density = zeros(size(ElementList, 1))

      do i = 1, size(ElementList, 1)
         TYPE_IDX = ElementList(i, 1)
         DOMAIN_IDX = ElementList(i, 2)
         ELEMENT_IDX = ElementList(i, 3)
         if (TYPE_IDX == this%TYPE_STEM) then
            Density(i) = this%stem(DOMAIN_IDX)%Density(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_LEAF) then
            Density(i) = this%LEAF(DOMAIN_IDX)%Density(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_ROOT) then
            Density(i) = this%ROOT(DOMAIN_IDX)%Density(ELEMENT_IDX)
         end if
      end do

   end function
! #####################################################################
   function getGlobalElementIdxSoybean(this, x_min, x_max, y_min, y_max, z_min, z_max, debug) result(GlobalElementIdx)
      class(Soybean_), intent(inout) :: this
      integer(int32), allocatable :: GlobalElementIdx(:)
      logical, optional, intent(in) :: debug
      logical :: do_debug
      real(real64), optional, intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
      integer(int32) :: offset, idx

      if (present(debug)) then
         do_debug = debug
      else
         do_debug = .false.
      end if

      offset = 0
      allocate (GlobalElementIdx(0))
      if (allocated(this%stem)) then
         do idx = 1, size(this%stem)
            if (this%stem(idx)%femdomain%empty()) cycle
            GlobalElementIdx = &
               GlobalElementIdx//(this%stem(idx)%femdomain%mesh%getElementList( &
                                  xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, zmin=z_min, zmax=z_max) + offset)
            offset = offset + this%stem(idx)%femdomain%ne()
         end do

         if (do_debug) then
            print *, "[o] STEM"
         end if
      else
         if (do_debug) then
            print *, "NO STEM"
         end if
      end if

      if (allocated(this%leaf)) then
         do idx = 1, size(this%leaf)
            if (this%leaf(idx)%femdomain%empty()) cycle

            GlobalElementIdx = &
               GlobalElementIdx//(this%leaf(idx)%femdomain%mesh%getElementList( &
                                  xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, zmin=z_min, zmax=z_max) + offset)
            offset = offset + this%leaf(idx)%femdomain%ne()

         end do

         if (do_debug) then
            print *, "[o] LEAF"
         end if
      else
         if (do_debug) then
            print *, "NO LEAF"
         end if
      end if

      if (allocated(this%root)) then
         do idx = 1, size(this%root)
            if (this%root(idx)%femdomain%empty()) cycle

            GlobalElementIdx = &
               GlobalElementIdx//(this%root(idx)%femdomain%mesh%getElementList( &
                                  xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, zmin=z_min, zmax=z_max) + offset)
            offset = offset + this%root(idx)%femdomain%ne()

         end do

         if (do_debug) then
            print *, "[o] ROOT"
         end if
      else
         if (do_debug) then
            print *, "NO ROOT"
         end if
      end if

   end function
! #####################################################################

   function getElementListSoybean(this, x_min, x_max, y_min, y_max, z_min, z_max, debug) result(ElementList)
      class(Soybean_), intent(inout) :: this
      integer(int32), allocatable :: ElementList(:, :)
      integer(int32), allocatable :: obj_type(:), obj_idx(:), elem_idx(:)
      real(real64), optional, intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
      logical, optional, intent(in) :: debug
      logical :: do_debug
      integer(int32) :: idx, n, m

      do_debug = input(default=.false., option=debug)

      !ElementList(idx, [ObjType, ObjID, ElementID] )
      allocate (elem_idx(0))
      allocate (obj_type(0))
      allocate (obj_idx(0))

      if (allocated(this%stem)) then
         do idx = 1, size(this%stem)
            if (this%stem(idx)%femdomain%empty()) cycle
            m = size(elem_idx)
            elem_idx = &
               elem_idx//this%stem(idx)%femdomain%mesh%getElementList( &
               xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, zmin=z_min, zmax=z_max)

            obj_idx = obj_idx//idx*int(eyes(size(elem_idx) - m))
         end do

         if (do_debug) then
            print *, "[o] STEM"
         end if
      else
         if (do_debug) then
            print *, "NO STEM"
         end if
      end if

      ! debug

      obj_type = obj_type//this%TYPE_STEM*int(eyes(size(elem_idx)))

      if (allocated(this%leaf)) then
         do idx = 1, size(this%leaf)
            if (this%leaf(idx)%femdomain%empty()) cycle
            m = size(elem_idx)
            elem_idx = &
               elem_idx//this%leaf(idx)%femdomain%mesh%getElementList( &
               xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, zmin=z_min, zmax=z_max)
            obj_idx = obj_idx//idx*int(eyes(size(elem_idx) - m))
         end do

         if (do_debug) then
            print *, "[o] LEAF"
         end if
      else
         if (do_debug) then
            print *, "NO LEAF"
         end if
      end if

      n = size(obj_type)
      obj_type = obj_type//this%TYPE_LEAF*int(eyes(size(elem_idx) - n))

      if (allocated(this%root)) then
         do idx = 1, size(this%root)
            if (this%root(idx)%femdomain%empty()) cycle
            m = size(elem_idx)
            elem_idx = &
               elem_idx//this%root(idx)%femdomain%mesh%getElementList( &
               xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, zmin=z_min, zmax=z_max)
            obj_idx = obj_idx//idx*int(eyes(size(elem_idx) - m))
         end do

         if (do_debug) then
            print *, "[o] ROOT"
         end if
      else
         if (do_debug) then
            print *, "NO ROOT"
         end if
      end if
      n = size(obj_type)
      obj_type = obj_type//this%TYPE_ROOT*int(eyes(size(elem_idx) - n))

      ElementList = zeros(size(elem_idx), 3)
      ElementList(:, 1) = obj_type
      ElementList(:, 2) = obj_idx
      ElementList(:, 3) = elem_idx

   end function

! ################################################################

! ################################################################
   function getStressFieldSoybean(this, displacement, i, j, option) result(StressField)
      class(Soybean_), intent(inout) :: this
      real(real64), intent(in) :: displacement(:)
      integer(int32), optional, intent(in) :: i, j
      character(*), optional, intent(in) :: option

      real(real64), allocatable :: StressField(:)
      integer(int32) :: ii, jj, n, obj_idx

      StressField = zeros(0)
      n = 1
      if (allocated(this%stem)) then
         do obj_idx = 1, size(this%stem)
            if (this%stem(obj_idx)%femdomain%mesh%empty()) cycle
            StressField = StressField// &
                          this%stem(obj_idx)%femdomain%getElementCauchyStress( &
                          displacement=displacement(n:n + this%stem(obj_idx)%femdomain%nn() &
                                                    *this%stem(obj_idx)%femdomain%nd() - 1), &
                          E=this%stem(obj_idx)%YoungModulus(:), &
                          v=this%stem(obj_idx)%PoissonRatio(:), i=i, j=j, option=option)
            n = n + this%stem(obj_idx)%femdomain%nn() &
                *this%stem(obj_idx)%femdomain%nd()
         end do
      end if

      if (allocated(this%leaf)) then
         do obj_idx = 1, size(this%leaf)
            if (this%leaf(obj_idx)%femdomain%mesh%empty()) cycle
            StressField = StressField// &
                          this%leaf(obj_idx)%femdomain%getElementCauchyStress( &
                          displacement=displacement(n:n + this%leaf(obj_idx)%femdomain%nn() &
                                                    *this%leaf(obj_idx)%femdomain%nd() - 1), &
                          E=this%leaf(obj_idx)%YoungModulus(:), &
                          v=this%leaf(obj_idx)%PoissonRatio(:), i=i, j=j, option=option)
            n = n + this%leaf(obj_idx)%femdomain%nn() &
                *this%leaf(obj_idx)%femdomain%nd()
         end do
      end if

      if (allocated(this%root)) then
         do obj_idx = 1, size(this%root)
            if (this%root(obj_idx)%femdomain%mesh%empty()) cycle
            StressField = StressField// &
                          this%root(obj_idx)%femdomain%getElementCauchyStress( &
                          displacement=displacement(n:n + this%root(obj_idx)%femdomain%nn() &
                                                    *this%root(obj_idx)%femdomain%nd() - 1), &
                          E=this%root(obj_idx)%YoungModulus(:), &
                          v=this%root(obj_idx)%PoissonRatio(:), i=i, j=j, option=option)
            n = n + this%root(obj_idx)%femdomain%nn() &
                *this%root(obj_idx)%femdomain%nd()
         end do
      end if

   end function
! ################################################################

   subroutine export_eigSoybean(this, name, Frequency, ModeVectors, stress_type)
      class(Soybean_), intent(inout) :: this
      character(*), intent(in) :: Name
      character(*), optional, intent(in) :: stress_type
      real(real64), intent(in) :: Frequency(:), ModeVectors(:, :)
      real(real64), allocatable :: displacement(:), stress(:)
      integer(int32) :: i, j
      type(IO_) :: f

      call f%open(name + ".csv", "w")
      call f%write("# Mode, Eigenfrequency (Hz)")
      do i = 1, 10
         displacement = ModeVectors(:, i)
         do j = 1, 36
            call this%deform(displacement=cos(radian(j*10.0d0))*displacement)

            if (present(stress_type)) then
               stress = this%getStressField(Displacement=cos(radian(j*10.0d0))*displacement, option=stress_type)
               call this%vtk(name + zfill(i, 3) + "_"+zfill(j, 4), single_file=.true., scalar_field=stress)
            else
               call this%vtk(name + zfill(i, 3) + "_"+zfill(j, 4), single_file=.true.)
            end if
            call this%deform(displacement=-cos(radian(j*10.0d0))*displacement)
         end do
         write (f%fh, *) str(i) + " , ", Frequency(i)
      end do
      call f%close()

   end subroutine

! ################################################################

   function getCarbon_concentrationSoybean(this, env, FixBoundary, FixValue) result(ret)
      class(Soybean_), intent(in) :: this
      type(Environment_), intent(in) :: env
      integer(int32), allocatable, intent(inout) :: FixBoundary(:)
      real(real64), allocatable, intent(inout) ::  FixValue(:)
      real(real64), allocatable :: ret(:)
      integer(int32), allocatable :: apicals(:), NumberOfElement(:), NumberOfPoint(:)
      integer(int32) :: stemID, apical_id, from, to, itr, i, k
      ! getCarbon_concentrationSoybean

      if (allocated(FixValue)) deallocate (FixValue)
      if (allocated(FixBoundary)) deallocate (FixBoundary)
      ! Unit ::  ___________micro-gram/m^3/s_____________

      ! reaction term

      ! 濃度勾配駆動で流れるとして，その固定濃度，負値は無視
      ret = -1.0d0*eyes(this%ne())
      allocate (FixValue(0))
      allocate (FixBoundary(0))

      apicals = this%findApical()
      NumberOfElement = this%getNumberOfElement()
      NumberOfPoint = this%getNumberOfPoint()

      !itr = 0
      do apical_id = 1, size(apicals)
         if (this%stem(apicals(apical_id))%empty()) then
            cycle
         else
            ! element-wise values
            to = sum(NumberOfElement(1:apicals(apical_id)))
            from = to + 1 - this%stem(apicals(apical_id))%ne()
            ret(from + this%stem(apicals(apical_id))%B_PointElementID) = this%apical_carbon_concentration

            ! node-wise values
            to = sum(NumberOfPoint(1:apicals(apical_id)))
            from = to + 1 - this%stem(apicals(apical_id))%nn()
            FixBoundary = FixBoundary//[from + this%stem(apicals(apical_id))%B_PointNodeID]
            FixValue = FixValue//[this%apical_carbon_concentration]

            itr = itr + 1
         end if
      end do

   end function
! ################################################################
   function ElementID2NodeIDSoybean(this, ElementIDs) result(NodeIDs)
      class(Soybean_), intent(in) :: this
      integer(int32), intent(in) :: ElementIDs(:)
      integer(int32), allocatable :: NodeIDs(:), numberOfElement(:), domain_in(:), idx(:), nidx(:)
      integer(int32) :: offset, domainID, i, j

      numberOfElement = this%getNumberOfElement()

      domain_in = ElementIDs
      domain_in(:) = 0

      offset = 0
      do i = 1, size(numberOfElement)
         do j = 1, size(ElementIDs)
            if (offset + 1 <= ElementIDs(j) .and. ElementIDs(j) <= offset + numberOfElement(i)) then
               domain_in(j) = i
            else
               cycle
            end if
         end do
         offset = offset + numberOfElement(i)
      end do

      offset = 0
      domainID = 0
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) cycle

            domainID = domainID + 1
            idx = getIdx(domain_in, equal_to=DomainID)
            if (size(idx) == 0) then
               cycle
            else
               nidx = this%stem(i)%femdomain%ElementID2NodeID(ElementID=idx)
               if (.not. allocated(nodeIDs)) then
                  nodeIDs = nidx(:) + offset
               else
                  nodeIDs = nodeIDs//(nidx(:) + offset)
               end if
            end if
            offset = offset + this%stem(i)%nn()

         end do
      end if

   end function

! ################################################################

   function getRespirationSoybean(this, env) result(ret)
      class(Soybean_), intent(in) :: this
      type(Environment_), intent(in) :: env
      integer(int32) :: stemid, rootid, from, to
      real(real64), allocatable :: ret(:)
      integer(int32), allocatable :: NumberOfElement(:)

      ! getCarbon_concentrationSoybean micro-gram/m^3
      ! reaction term
      ! Unit ::  ___________micro-gram/m^3/s_____from mincro-mol/m-2/s

      NumberOfElement = this%getNumberOfElement()

      ret = zeros(this%ne())
      do stemid = 1, this%numStem()

         if (this%stem(stemid)%empty()) cycle

         to = sum(NumberOfElement(1:stemID))
         from = to - this%stem(stemID)%ne() + 1
         ret(from:to) = this%stem(stemid)%R_d*180.160d0/6.0d0/0.00020d0
      end do
      ! ignore leaf since it is contained in the %GetPhotosynthesis

      if (allocated(this%root)) then
         do rootID = 1, this%numRoot()

            if (this%root(rootID)%empty()) cycle

            to = sum(NumberOfElement(1:this%numStem() + this%numLeaf() + rootID))
            from = to - this%root(rootID)%ne() + 1

            ret(from:to) = this%root(rootID)%R_d*180.160d0/6.0d0/0.00020d0
         end do
      end if

   end function
! ######################################################
   function getCarbonFlowSoybean(this, photosynthesis, respiration, FixBoundary, FixValue, &
                                 Photosynthate_n, dt, penalty, DiffusionCoeff, debug, RHS, Matrix, &
                                 tol, algorithm) &
      result(Photosynthate)
      class(Soybean_), target, intent(inout) :: this
      type(CRS_), optional, intent(inout) :: Matrix
      real(real64), intent(in) :: photosynthesis(:), respiration(:), Photosynthate_n(:), &
                                  dt, DiffusionCoeff(:), FixValue(:)
      integer(int32), intent(in) :: FixBoundary(:)
      real(real64), intent(in) :: penalty
      real(real64), optional, intent(in) :: tol
      character(*), optional, intent(in) :: algorithm
      real(real64) :: dx
      real(real64), allocatable :: c(:)
      logical, optional, intent(in) :: debug
      real(real64), allocatable :: Photosynthate(:)
      real(real64), allocatable, optional, intent(inout) :: RHS(:)
      logical :: passed
      type(FEMDomain_), allocatable :: FEMDomains(:)
      type(DiffusionEq_) :: solver
      integer(int32) :: i, n, num_fix_bound, total_nn
      type(IO_) :: f

!    num_fix_bound = 0
!    do i=1,size(carbon_concentration)
!        if(carbon_concentration(i) > 0.0d0 ) then
!            num_fix_bound = num_fix_bound + 1
!        endif
!    enddo
!
!    FixValue    = zeros(num_fix_bound)
!    FixBoundary = int(zeros(num_fix_bound))
!
!
!    num_fix_bound = 0
!    do i=1,size(carbon_concentration)
!        if(carbon_concentration(i) > 0.0d0 ) then
!            num_fix_bound = num_fix_bound + 1
!            FixBoundary(num_fix_bound) = i
!            FixValue(num_fix_bound)    = carbon_concentration(i)
!        endif
!    enddo

      FEMDomains = this%to_FEMDomains()
      if (present(debug)) then
         solver%solver%debug = debug
      end if

      do i = 1, size(femdomains)
         call femdomains(i)%vtk("mesh_"+zfill(i, 4))
         if (femdomains(i)%empty()) stop
      end do

      if (present(tol)) then
         solver%solver%er0 = tol
         solver%solver%relative_er = tol
      end if
      if (allocated(this%reaction_n)) then
         solver%CRS_RHS_n = this%reaction_n
      end if

      dx = abs(FEMDomains(1)%position_x(1) - FEMDomains(1)%position_x(2))
      call solver%check_stability_condition(dt=dt, dx=dx, coefficient=average(DiffusionCoeff), passed=passed)
      if (.not. passed) then
         print *, "[getCarbonFlowSoybean] >> dt is too large!"
         stop
      end if

      if (present(algorithm)) then
         Photosynthate = solver%getDiffusionField( &
                         FEMDomains=FEMDomains, &
                         DiffusionCoeff=DiffusionCoeff, &
                         Reaction=photosynthesis - respiration, &
                         Penalty=penalty, &
                         FixBoundary=FixBoundary, &
                         FixValue=FixValue, &
                         C_n=Photosynthate_n, &
                         RHS=RHS, &
                         algorithm=algorithm, &
                         Matrix=Matrix, &
                         dt=dt &
                         )
      else
         Photosynthate = solver%getDiffusionField( &
                         FEMDomains=FEMDomains, &
                         DiffusionCoeff=DiffusionCoeff, &
                         Reaction=photosynthesis - respiration, &
                         Penalty=penalty, &
                         FixBoundary=FixBoundary, &
                         FixValue=FixValue, &
                         C_n=Photosynthate_n, &
                         RHS=RHS, &
                         Matrix=Matrix, &
                         dt=dt &
                         )
      end if
      this%reaction_n = solver%CRS_RHS_n

   end function
! ######################################################

! ######################################################
   function getDiffusionCoefficientSoybean(this) result(DiffusionCoefficient)
      class(Soybean_), intent(inout) :: this
      real(real64), allocatable :: DiffusionCoefficient(:)
      integer(int32) :: i, n, domainID

      allocate (DiffusionCoefficient(0))

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) cycle
            DiffusionCoefficient = DiffusionCoefficient//this%stem(i)%CarbonDiffusionCoefficient
         end do
      end if
      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) cycle
            DiffusionCoefficient = DiffusionCoefficient//this%leaf(i)%CarbonDiffusionCoefficient
         end do
      end if
      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%empty()) cycle
            DiffusionCoefficient = DiffusionCoefficient//this%root(i)%CarbonDiffusionCoefficient
         end do
      end if
   end function
! ######################################################

! ######################################################
   function to_FEMDomainsSoybean(this) result(femdomains)
      class(Soybean_), intent(inout) :: this
      type(FEMDomain_), allocatable :: femdomains(:)
      integer(int32) :: i, j, n, domainID
      integer(int32) :: stem_offset, leaf_offset, root_offset

      n = this%numStem() + this%numLeaf() + this%numRoot()

      allocate (femdomains(n))

      DomainID = 0
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) cycle
            DomainID = DomainID + 1
            femdomains(DomainID) = this%stem(i)%femdomain
         end do
      end if
      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) cycle
            DomainID = DomainID + 1
            femdomains(DomainID) = this%leaf(i)%femdomain
         end do
      end if
      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%empty()) cycle
            DomainID = DomainID + 1
            femdomains(DomainID) = this%root(i)%femdomain
         end do
      end if

      ! overset
      stem_offset = 0
      leaf_offset = this%numStem()
      root_offset = this%numStem() + this%numLeaf()

      do i = 1, size(femdomains)
         femdomains(i)%uuid = generate_uuid(1)
      end do

      if (allocated(this%Stem2Stem)) then
         do i = 1, size(this%Stem2Stem, 1)
            do j = 1, size(this%Stem2Stem, 2)
               if (this%Stem2Stem(i, j) >= 1) then
                  call femdomains(i + stem_offset)%overset( &
                     FEMDomains=femdomains, to=j + stem_offset, by="GPP")
                  call femdomains(j + stem_offset)%overset( &
                     FEMDomains=femdomains, to=i + stem_offset, by="GPP")
               end if
            end do
         end do
      end if

      if (allocated(this%Leaf2Stem)) then
         do i = 1, size(this%Leaf2Stem, 1)
            do j = 1, size(this%Leaf2Stem, 2)
               if (this%Leaf2Stem(i, j) > 0) then
                  call femdomains(i + leaf_offset)%overset( &
                     FEMDomains=femdomains, to=j + stem_offset, by="GPP")
                  call femdomains(j + stem_offset)%overset( &
                     FEMDomains=femdomains, to=i + leaf_offset, by="GPP")
               end if
            end do
         end do
      end if
      if (allocated(this%root2Stem)) then
         do i = 1, size(this%root2Stem, 1)
            do j = 1, size(this%root2Stem, 2)
               if (this%root2Stem(i, j) > 0) then
                  call femdomains(i + root_offset)%overset( &
                     FEMDomains=femdomains, to=j + stem_offset, by="GPP")
                  call femdomains(j + stem_offset)%overset( &
                     FEMDomains=femdomains, to=i + root_offset, by="GPP")
               end if
            end do
         end do
      end if
      if (allocated(this%root2root)) then
         do i = 1, size(this%root2root, 1)
            do j = 1, size(this%root2root, 2)
               if (this%root2root(i, j) > 0) then
                  call femdomains(i + root_offset)%overset( &
                     FEMDomains=femdomains, to=j + root_offset, by="GPP")
                  call femdomains(j + root_offset)%overset( &
                     FEMDomains=femdomains, to=i + root_offset, by="GPP")
               end if
            end do
         end do
      end if

   end function
! ######################################################

! ######################################################
   subroutine setFEMDomainsSoybean(this, femdomains)
      class(Soybean_), intent(inout) :: this
      type(FEMDomain_), intent(in) :: femdomains(:)
      integer(int32) :: i, n, domainID

      DomainID = 0
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%empty()) cycle
            DomainID = DomainID + 1
            this%stem(i)%femdomain = femdomains(DomainID)
         end do
      end if
      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%empty()) cycle
            DomainID = DomainID + 1
            this%leaf(i)%femdomain = femdomains(DomainID)
         end do
      end if
      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%empty()) cycle
            DomainID = DomainID + 1
            this%root(i)%femdomain = femdomains(DomainID)
         end do
      end if

   end subroutine
! ######################################################

! ##################################################################
   function nn_rangeSoybean(this, organ_type, ID) result(ret)
      class(Soybean_), intent(inout) :: this
      integer(int32), intent(in) :: ID
      character(*), intent(in) :: organ_type
      integer(int32) :: ret(1:2), i, offset

      ! get number of node (point)
      ret = [0, 0]

      offset = 0
      select case (organ_type)
      case ("Stem", "stem", "STEM")
         if (allocated(this%stem)) then
            do i = 1, ID - 1
               if (.not. this%stem(i)%femdomain%mesh%empty()) then
                  offset = offset + this%stem(i)%femdomain%nn()
               end if
            end do
            ret(1) = offset + 1
            ret(2) = offset + this%stem(ID)%femdomain%nn()
         end if
      case ("Leaf", "leaf", "LEAF")
         if (allocated(this%stem)) then
            do i = 1, size(this%stem)
               if (.not. this%stem(i)%femdomain%mesh%empty()) then
                  offset = offset + this%stem(i)%femdomain%nn()
               end if
            end do
         end if
         if (allocated(this%leaf)) then
            do i = 1, ID - 1
               if (.not. this%leaf(i)%femdomain%mesh%empty()) then
                  offset = offset + this%leaf(i)%femdomain%nn()
               end if
            end do
            ret(1) = offset + 1
            ret(2) = offset + this%leaf(ID)%femdomain%nn()
         end if

      case ("Root", "root", "ROOT")
         if (allocated(this%stem)) then
            do i = 1, size(this%stem)
               if (.not. this%stem(i)%femdomain%mesh%empty()) then
                  offset = offset + this%stem(i)%femdomain%nn()
               end if
            end do
         end if
         if (allocated(this%leaf)) then
            do i = 1, size(this%leaf)
               if (.not. this%leaf(i)%femdomain%mesh%empty()) then
                  offset = offset + this%leaf(i)%femdomain%nn()
               end if
            end do
         end if
         if (allocated(this%root)) then
            do i = 1, ID - 1
               if (.not. this%root(i)%femdomain%mesh%empty()) then
                  offset = offset + this%root(i)%femdomain%nn()
               end if
            end do
            ret(1) = offset + 1
            ret(2) = offset + this%root(ID)%femdomain%nn()
         end if
!        case ("Ear","ear","EAR")
!
!            if(allocated(this%stem) )then
!                if(allocated(this%stem) )then
!                    do i=1,size(this%stem)
!                        if( .not.this%stem(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%stem(i)%femdomain%nn()
!                        endif
!                    enddo
!                endif
!                if(allocated(this%leaf) )then
!                    do i=1,size(this%leaf)
!                        if( .not.this%leaf(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%leaf(i)%femdomain%nn()
!                        endif
!                    enddo
!                endif
!                if(allocated(this%root) )then
!                    do i=1,size(this%root)
!                        if( .not.this%root(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%root(i)%femdomain%nn()
!                        endif
!                    enddo
!                endif
!                if(allocated(this%ear) )then
!                    do i=1,ID-1
!                        if( .not.this%ear(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%ear(i)%femdomain%nn()
!                        endif
!                    enddo
!                    ret(1) = offset + 1
!                    ret(2) = offset + this%ear(ID)%femdomain%nn()
!                endif
!            endif
!        case ("Panicle","panicle","PANICLE")
!            if(allocated(this%stem) )then
!                if(allocated(this%stem) )then
!                    do i=1,size(this%stem)
!                        if( .not.this%stem(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%stem(i)%femdomain%nn()
!                        endif
!                    enddo
!                endif
!                if(allocated(this%leaf) )then
!                    do i=1,size(this%leaf)
!                        if( .not.this%leaf(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%leaf(i)%femdomain%nn()
!                        endif
!                    enddo
!                endif
!                if(allocated(this%root) )then
!                    do i=1,size(this%root)
!                        if( .not.this%root(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%root(i)%femdomain%nn()
!                        endif
!                    enddo
!                endif
!                if(allocated(this%ear) )then
!                    do i=1,size(this%ear)
!                        if( .not.this%ear(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%ear(i)%femdomain%nn()
!                        endif
!                    enddo
!                endif
!                if(allocated(this%panicle) )then
!                    do i=1,ID-1
!                        if( .not.this%panicle(i)%femdomain%mesh%empty() ) then
!                            offset = offset + this%panicle(i)%femdomain%nn()
!                        endif
!                    enddo
!                    ret(1) = offset + 1
!                    ret(2) = offset + this%panicle(ID)%femdomain%nn()
!                endif
!            endif
!
      end select

   end function
! ##################################################################

! ################################################################
   subroutine getVerticesSoybean(this, Vertices, VertexIDs)
      class(Soybean_), intent(inout) :: this
      real(real64), allocatable, intent(inout) :: Vertices(:)
      integer(int32), allocatable, intent(inout) :: VertexIDs(:)
      real(real64), allocatable :: this_vertices(:)
      integer(int32), allocatable :: nn_range(:), this_vertexIDs(:)
      integer(int32) :: i, n, new_idx
      real(real64), allocatable :: old_Vertices(:)
      integer(int32), allocatable :: old_VertexIDs(:)

      Vertices = zeros(this%nn()*3)
      VertexIDs = int(zeros(this%nn()))
      if (allocated(this%stem)) then
         !$OMP parallel do private(nn_range,this_vertices,this_vertexIDs)
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%Mesh%empty()) then
               call this%stem(i)%femdomain%getVertices(this_vertices, this_vertexIDs)
               nn_range = this%nn_range("stem", i)
               Vertices(3*(nn_range(1) - 1) + 1:3*(nn_range(1) - 1) + size(this_vertices)) = this_vertices(:)
               VertexIDs(nn_range(1):nn_range(1) - 1 + size(this_vertexIDs)) = nn_range(1) - 1 + this_vertexIDs(:)
               deallocate (this_vertices)
               deallocate (this_vertexIDs)
            end if
         end do
         !$OMP end parallel do
      end if

      if (allocated(this%leaf)) then
         !$OMP parallel do private(nn_range,this_vertices,this_vertexIDs)
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%Mesh%empty()) then
               call this%leaf(i)%femdomain%getVertices(this_vertices, this_vertexIDs)
               nn_range = this%nn_range("leaf", i)
               Vertices(3*(nn_range(1) - 1) + 1:3*(nn_range(1) - 1) + size(this_vertices)) = this_vertices(:)
               VertexIDs(nn_range(1):nn_range(1) - 1 + size(this_vertexIDs)) = nn_range(1) - 1 + this_vertexIDs(:)
               deallocate (this_vertices)
               deallocate (this_vertexIDs)
            end if
         end do
         !$OMP end parallel do
      end if

      if (allocated(this%root)) then
         !$OMP parallel do private(nn_range,this_vertices,this_vertexIDs)
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%Mesh%empty()) then
               call this%root(i)%femdomain%getVertices(this_vertices, this_vertexIDs)
               nn_range = this%nn_range("root", i)
               Vertices(3*(nn_range(1) - 1) + 1:3*(nn_range(1) - 1) + size(this_vertices)) = this_vertices(:)
               VertexIDs(nn_range(1):nn_range(1) - 1 + size(this_vertexIDs)) = nn_range(1) - 1 + this_vertexIDs(:)
               deallocate (this_vertices)
               deallocate (this_vertexIDs)
            end if
         end do
         !$OMP end parallel do
      end if

      ! if VertexIDs(:) = 0 then
      ! remove vertices
      old_VertexIDs = VertexIDs
      old_Vertices = Vertices
      n = size(VertexIDs) - countif(Array=VertexIDs, Equal=.true., Value=0)
      deallocate (VertexIDs)
      deallocate (Vertices)
      allocate (VertexIDs(n))
      allocate (Vertices(3*n))

      new_idx = 0
      do i = 1, size(old_VertexIDs)
         if (old_VertexIDs(i) == 0) then
            cycle
         else
            new_idx = new_idx + 1
            VertexIDs(new_idx) = old_VertexIDs(i)
            Vertices(3*(new_idx - 1) + 1:3*(new_idx - 1) + 3) = old_Vertices(3*(i - 1) + 1:3*(i - 1) + 3)
         end if
      end do

   end subroutine getVerticesSoybean
! ################################################################

   function MassMatrixSoybean(this, debug) result(ret)
      class(Soybean_), target, intent(inout) :: this
!    real(real64),intent(in) :: ground_level
!    real(real64),optional,intent(in) :: penalty, tol,traction_force(:)
      logical, optional, intent(in) ::debug
      type(CRS_) :: ret
!    integer(int32),optional,intent(in) ::itrmax

      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)
      type(FEMSolver_) :: solver

      integer(int32) :: stem_id, leaf_id, root_id, DomainID, ElementID, i, n, offset
      integer(int32) :: myStemID, yourStemID, myLeafID, myRootID, yourRootID
      integer(int32), allocatable :: FixBoundary(:)
      ! linear elasticity with infinitesimal strain theory
      n = this%numStem() + this%numLeaf() + this%numRoot()
      allocate (FEMDomainPointers(n))

      !(1) >> compute overset
      ! For stems
      if (allocated(this%stem2stem)) then
         do myStemID = 1, size(this%stem2stem, 1)
            do yourStemID = 1, size(this%stem2stem, 2)
               if (this%stem2stem(myStemID, yourStemID) >= 1) then
                  ! connected
                  call this%stem(myStemID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=myStemID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%leaf2stem)) then
         do myLeafID = 1, size(this%leaf2stem, 1)
            do yourStemID = 1, size(this%leaf2stem, 2)
               if (this%leaf2stem(myLeafID, yourStemID) >= 1) then
                  ! connected
                  call this%leaf(myLeafID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=this%numStem() + myLeafID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%root2stem)) then
         do myRootID = 1, size(this%root2stem, 1)
            do yourStemID = 1, size(this%root2stem, 2)
               if (this%root2stem(myRootID, yourStemID) >= 1) then
                  ! connected
                  call this%root(myRootID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=this%numStem() + this%numLeaf() + myRootID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%root2root)) then
         do myRootID = 1, size(this%root2root, 1)
            do yourrootID = 1, size(this%root2root, 2)
               if (this%root2root(myRootID, yourrootID) >= 1) then
                  ! connected
                  call this%root(myRootID)%femdomain%overset( &
                     FEMDomain=this%root(yourrootID)%femdomain, &
                     DomainID=this%numroot() + this%numLeaf() + yourrootID, &
                     MyDomainID=this%numroot() + this%numLeaf() + myRootID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] overset >> done."
         end if
      end if

      call solver%init(NumDomain=this%numStem() + this%numLeaf() + this%numRoot())

      FEMDomainPointers = this%getFEMDomainPointers()
      call solver%setDomain(FEMDomainPointers=FEMDomainPointers)

      if (present(debug)) then
         if (debug) then
            print *, "[ok] initSolver >> done."
         end if
      end if

      call solver%setCRS(DOF=3, debug=debug)

      ! CRS ready!

!    if( .not. this%checkYoungModulus())then
!        print *, "[ERROR] YoungModulus(:) is not ready."
!        stop
!    endif
!    if( .not. this%checkPoissonRatio())then
!        print *, "[ERROR] PoissonRatio(:) is not ready."
!        stop
!    endif
      if (.not. this%checkDensity()) then
         print *, "[ERROR] Density(:) is not ready."
         stop
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] setCRS >> done."
         end if
      end if

      !$OMP parallel do private(ElementID)
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%MassMatrix( &
                                  ElementID=ElementID, &
                                  DOF=FEMDomainPointers(DomainID)%femdomainp%nd(), &
                                  Density=this%getDensity(DomainID=DomainID, ElementID=ElementID)))
         end do
      end do
      !$OMP end parallel do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] set Matrix & vectors >> done."
         end if
      end if

!    call solver%setEbOM(penalty=input(default=10000000.0d0,option=penalty), DOF=3)

!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] set EbOM >> done."
!        endif
!    endif
!
!    ! traction boundary condition
!    if(present(traction_force) )then
!        if(size(traction_force)/=size(solver%CRS_RHS) )then
!            print *, "[ERROR] > getDisplacementSoybean > (size(traction_force)/=size(solver%CRS_RHS) )"
!            stop
!        endif
!        solver%CRS_RHS(:) = solver%CRS_RHS(:) + traction_force(:)
!    endif
!
!    ! fix-boundary conditions
!    offset = 0
!    do i=1,size(FEMDomainPointers)
!        if(FEMDomainPointers(i)%FEMDomainp%z_min() <= ground_level )then
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-2
!            FixBoundary = FixBoundary + offset
!            call solver%fix(IDs=FixBoundary,FixValue=0.0d0)
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-1
!            FixBoundary = FixBoundary + offset
!            call solver%fix(IDs=FixBoundary,FixValue=0.0d0)
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-0
!            FixBoundary = FixBoundary + offset
!            call solver%fix(IDs=FixBoundary,FixValue=0.0d0)
!        endif
!        offset = offset + FEMDomainPointers(i)%femdomainp%nn()*3
!    enddo
!
!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] FixBoundary >> done."
!        endif
!    endif
!
      if (present(debug)) then
         solver%debug = debug
      end if
!    if(present(itrmax) )then
!        solver%itrmax = itrmax
!    endif
!
!    if(present(tol) )then
!        solver%er0 = tol
!    endif

!    disp = solver%solve()
!
!
      ret = solver%getCRS()
      !call solver%remove()

!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] Solve >> done."
!        endif
!    endif
      ! japanese "ato-shimatsu"

   end function

! #####################################################################

   function StiffnessMatrixSoybean(this, penalty, debug) result(ret)
      class(Soybean_), target, intent(inout) :: this
!    real(real64),intent(in) :: ground_level
      real(real64), optional, intent(in) :: penalty!, tol,traction_force(:)
      logical, optional, intent(in) ::debug
      type(CRS_) :: ret
!    integer(int32),optional,intent(in) ::itrmax

      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)
      type(FEMSolver_) :: solver

      integer(int32) :: stem_id, leaf_id, root_id, DomainID, ElementID, i, n, offset
      integer(int32) :: myStemID, yourStemID, myLeafID, myRootID, yourRootID
      integer(int32), allocatable :: FixBoundary(:)
      ! linear elasticity with infinitesimal strain theory
      n = this%numStem() + this%numLeaf() + this%numRoot()

      allocate (FEMDomainPointers(n))

      !(1) >> compute overset
      ! For stems
      if (allocated(this%stem2stem)) then
         do myStemID = 1, size(this%stem2stem, 1)
            do yourStemID = 1, size(this%stem2stem, 2)
               if (this%stem2stem(myStemID, yourStemID) >= 1) then
                  ! connected
                  call this%stem(myStemID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=myStemID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%leaf2stem)) then
         do myLeafID = 1, size(this%leaf2stem, 1)
            do yourStemID = 1, size(this%leaf2stem, 2)
               if (this%leaf2stem(myLeafID, yourStemID) >= 1) then
                  ! connected
                  call this%leaf(myLeafID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=this%numStem() + myLeafID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%root2stem)) then
         do myRootID = 1, size(this%root2stem, 1)
            do yourStemID = 1, size(this%root2stem, 2)
               if (this%root2stem(myRootID, yourStemID) >= 1) then
                  ! connected
                  call this%root(myRootID)%femdomain%overset( &
                     FEMDomain=this%stem(yourStemID)%femdomain, &
                     DomainID=yourStemID, &
                     MyDomainID=this%numStem() + this%numLeaf() + myRootID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (allocated(this%root2root)) then
         do myRootID = 1, size(this%root2root, 1)
            do yourrootID = 1, size(this%root2root, 2)
               if (this%root2root(myRootID, yourrootID) >= 1) then
                  ! connected
                  call this%root(myRootID)%femdomain%overset( &
                     FEMDomain=this%root(yourrootID)%femdomain, &
                     DomainID=this%numroot() + this%numLeaf() + yourrootID, &
                     MyDomainID=this%numroot() + this%numLeaf() + myRootID, &
                     algorithm=this%overset_algorithm) ! or "P2P"
               end if
            end do
         end do
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] overset >> done."
         end if
      end if

      call solver%init(NumDomain=this%numStem() + this%numLeaf() + this%numRoot())

      FEMDomainPointers = this%getFEMDomainPointers()
      call solver%setDomain(FEMDomainPointers=FEMDomainPointers)

      if (present(debug)) then
         if (debug) then
            print *, "[ok] initSolver >> done."
         end if
      end if

      call solver%setCRS(DOF=3, debug=debug)

      ! CRS ready!

      if (.not. this%checkYoungModulus()) then
         print *, "[ERROR] YoungModulus(:) is not ready."
         stop
      end if
      if (.not. this%checkPoissonRatio()) then
         print *, "[ERROR] PoissonRatio(:) is not ready."
         stop
      end if
!    if( .not. this%checkDensity())then
!        print *, "[ERROR] Density(:) is not ready."
!        stop
!    endif

      if (present(debug)) then
         if (debug) then
            print *, "[ok] setCRS >> done."
         end if
      end if

      !$OMP parallel do private(ElementID)
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix( &
                                  ElementID=ElementID, &
                                  E=this%getYoungModulus(DomainID=DomainID, ElementID=ElementID), &
                                  v=this%getPoissonRatio(DomainID=DomainID, ElementID=ElementID)))

!            call solver%setVector(DomainID=DomainID,ElementID=ElementID,DOF=3,&
!                Vector=FEMDomainPointers(DomainID)%femdomainp%MassVector(&
!                    ElementID=ElementID,&
!                    DOF=FEMDomainPointers(DomainID)%femdomainp%nd() ,&
!                    Density= this%getDensity(DomainID=DomainID,ElementID=ElementID) ,&
!                    Accel=[0.0d0, 0.0d0, -9.80d0]&
!                    ) &
!                )
         end do
      end do
      !$OMP end parallel do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] set Matrix & vectors >> done."
         end if
      end if

      call solver%setEbOM(penalty=input(default=10000000.0d0, option=penalty), DOF=3)

      if (present(debug)) then
         if (debug) then
            print *, "[ok] set EbOM >> done."
         end if
      end if

!    ! traction boundary condition
!    if(present(traction_force) )then
!        if(size(traction_force)/=size(solver%CRS_RHS) )then
!            print *, "[ERROR] > getDisplacementSoybean > (size(traction_force)/=size(solver%CRS_RHS) )"
!            stop
!        endif
!        solver%CRS_RHS(:) = solver%CRS_RHS(:) + traction_force(:)
!    endif
!
!    ! fix-boundary conditions
!    offset = 0
!    do i=1,size(FEMDomainPointers)
!        if(FEMDomainPointers(i)%FEMDomainp%z_min() <= ground_level )then
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-2
!            FixBoundary = FixBoundary + offset
!            call solver%fix(IDs=FixBoundary,FixValue=0.0d0)
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-1
!            FixBoundary = FixBoundary + offset
!            call solver%fix(IDs=FixBoundary,FixValue=0.0d0)
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-0
!            FixBoundary = FixBoundary + offset
!            call solver%fix(IDs=FixBoundary,FixValue=0.0d0)
!        endif
!        offset = offset + FEMDomainPointers(i)%femdomainp%nn()*3
!    enddo

      if (present(debug)) then
         if (debug) then
            print *, "[ok] FixBoundary >> done."
         end if
      end if

      if (present(debug)) then
         solver%debug = debug
      end if
!    if(present(itrmax) )then
!        solver%itrmax = itrmax
!    endif
!
!    if(present(tol) )then
!        solver%er0 = tol
!    endif
!

!    disp = solver%solve()
!
!

      ret = solver%getCRS()
      !call solver%remove()

!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] Solve >> done."
!        endif
!    endif
      ! japanese "ato-shimatsu"

   end function

! #####################################################################

   function to_R_FRSoybean(this, spectrum) result(ret)
      class(Soybean_), intent(in) :: this
      real(real64), intent(in) :: spectrum(:, :) ! global_elem_idx, nm
      real(real64), allocatable :: ret(:)
      integer(int32) :: idx

      ret = zeros(size(spectrum, 1))
      do idx = 1, size(ret)
         ret(idx) = sum(spectrum(idx, 600:700))/sum(spectrum(idx, 700:800))
      end do

   end function
! #####################################################################

! #####################################################################
   function get_stem_length_list_Soybean(this) result(ret)
      class(Soybean_),intent(in) :: this
      real(real64),allocatable :: ret(:)
      integer(int32) :: idx,i


      ret = zeros(this%numStem())

      if (.not. allocated(this%stem)) then
         return
      end if
      
      idx = 0
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%Mesh%empty() .eqv. .false.) then
            idx = idx + 1
            ret(idx) = this%stem(idx)%getLength()
         end if
      end do

   end function
! #####################################################################


! #####################################################################
   subroutine set_stem_length_by_list_Soybean(this,stem_length_list)
      class(Soybean_),intent(inout) :: this
      real(real64),intent(in) :: stem_length_list(:)
      integer(int32) :: idx,i


      if (.not. allocated(this%stem)) then
         return
      end if
      
      idx = 0
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%Mesh%empty() .eqv. .false.) then
            idx = idx + 1
            this%stem(idx)%already_grown = False
            call this%stem(idx)%change_length_or_width(length=stem_length_list(idx))

         end if
      end do

      call this%update()
   end subroutine
! #####################################################################


! #####################################################################
   subroutine set_stem_angle_by_list_Soybean(this,stem_angle_list)
      class(Soybean_),intent(inout) :: this
      real(real64),intent(in) :: stem_angle_list(:,:)
      real(real64) :: original_position(1:3),disp(1:3)
      integer(int32) :: idx,i


      if (.not. allocated(this%stem)) then
         return
      end if
      
      idx = 0
      original_position(1:3) = this%stem(1)%femdomain%Mesh%nodcoord(1,1:3)
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%Mesh%empty() .eqv. .false.) then
            idx = idx + 1
            call this%stem(idx)%rotate(&
               x=stem_angle_list(idx,1),&
               y=stem_angle_list(idx,2),&
               z=stem_angle_list(idx,3),reset=True)
         end if
      end do
      call this%update()
      disp = original_position(1:3) - this%stem(1)%femdomain%Mesh%nodcoord(1,1:3)
      call this%move(x=disp(1),y=disp(2),z=disp(3))
      call this%update()

   end subroutine
! #####################################################################




! #####################################################################
   function get_stem_angle_list_Soybean(this) result(ret)
      class(Soybean_),intent(in) :: this
      real(real64),allocatable :: ret(:,:)
      integer(int32) :: idx,i

      ret = zeros(this%numStem(),3)

      if (.not. allocated(this%stem)) then
         return
      end if
      
      idx = 0
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%Mesh%empty() .eqv. .false.) then
            idx = idx + 1
            ret(idx,1:3) = this%stem(idx)%getAngles()
         end if
      end do

   end function
! #####################################################################

! #####################################################################
function height_Soybean(this) result(ret)
   class(Soybean_),intent(in) :: this
   real(real64) :: ret

   ret = this%z_max() - this%stem(1)%femdomain%z_min()
   
end function
! #####################################################################


! #####################################################################
function x_min_Soybean(this) result(ret)
   class(Soybean_),intent(in) :: this
   real(real64) :: ret
   integer(int32) :: i

   ret = this%stem(1)%femdomain%x_min()
   if (allocated(this%stem))then
      do i=1,size(this%stem)
         if (this%stem(i)%FEMDomain%mesh%empty()) cycle
         if (this%stem(i)%femdomain%x_min() <= ret)then
            ret = this%stem(i)%femdomain%x_min()
         endif 
      enddo
   endif
   
   if (allocated(this%leaf))then
      do i=1,size(this%leaf)
         if (this%leaf(i)%FEMDomain%mesh%empty()) cycle
         if (this%leaf(i)%femdomain%x_min() <= ret)then
            ret = this%leaf(i)%femdomain%x_min()
         endif 
      enddo
   endif

   if (allocated(this%root))then
      do i=1,size(this%root)
         if (this%root(i)%FEMDomain%mesh%empty()) cycle
         if (this%root(i)%femdomain%x_min() <= ret)then
            ret = this%root(i)%femdomain%x_min()
         endif 
      enddo
   endif
   
end function
! #####################################################################


! #####################################################################
function x_max_Soybean(this) result(ret)
   class(Soybean_),intent(in) :: this
   real(real64) :: ret
   integer(int32) :: i

   ret = this%stem(1)%femdomain%x_max()
   if (allocated(this%stem))then
      do i=1,size(this%stem)
         if (this%stem(i)%FEMDomain%mesh%empty()) cycle
         if (this%stem(i)%femdomain%x_max() >= ret)then
            ret = this%stem(i)%femdomain%x_max()
         endif 
      enddo
   endif
   
   if (allocated(this%leaf))then
      do i=1,size(this%leaf)
         if (this%leaf(i)%FEMDomain%mesh%empty()) cycle
         if (this%leaf(i)%femdomain%x_max() >= ret)then
            ret = this%leaf(i)%femdomain%x_max()
         endif 
      enddo
   endif

   if (allocated(this%root))then
      do i=1,size(this%root)
         if (this%root(i)%FEMDomain%mesh%empty()) cycle
         if (this%root(i)%femdomain%x_max() >= ret)then
            ret = this%root(i)%femdomain%x_max()
         endif 
      enddo
   endif
   
end function
! #####################################################################



! #####################################################################
function y_min_Soybean(this) result(ret)
   class(Soybean_),intent(in) :: this
   real(real64) :: ret
   integer(int32) :: i

   ret = this%stem(1)%femdomain%y_min()
   if (allocated(this%stem))then
      do i=1,size(this%stem)
         if (this%stem(i)%FEMDomain%mesh%empty()) cycle
         if (this%stem(i)%femdomain%y_min() <= ret)then
            ret = this%stem(i)%femdomain%y_min()
         endif 
      enddo
   endif
   
   if (allocated(this%leaf))then
      do i=1,size(this%leaf)
         if (this%leaf(i)%FEMDomain%mesh%empty()) cycle
         if (this%leaf(i)%femdomain%y_min() <= ret)then
            ret = this%leaf(i)%femdomain%y_min()
         endif 
      enddo
   endif

   if (allocated(this%root))then
      do i=1,size(this%root)
         if (this%root(i)%FEMDomain%mesh%empty()) cycle
         if (this%root(i)%femdomain%y_min() <= ret)then
            ret = this%root(i)%femdomain%y_min()
         endif 
      enddo
   endif
   
end function
! #####################################################################


! #####################################################################
function y_max_Soybean(this) result(ret)
   class(Soybean_),intent(in) :: this
   real(real64) :: ret
   integer(int32) :: i

   ret = this%stem(1)%femdomain%y_max()
   if (allocated(this%stem))then
      do i=1,size(this%stem)
         if (this%stem(i)%FEMDomain%mesh%empty()) cycle
         if (this%stem(i)%femdomain%y_max() >= ret)then
            ret = this%stem(i)%femdomain%y_max()
         endif 
      enddo
   endif
   
   if (allocated(this%leaf))then
      do i=1,size(this%leaf)
         if (this%leaf(i)%FEMDomain%mesh%empty()) cycle
         if (this%leaf(i)%femdomain%y_max() >= ret)then
            ret = this%leaf(i)%femdomain%y_max()
         endif 
      enddo
   endif

   if (allocated(this%root))then
      do i=1,size(this%root)
         if (this%root(i)%FEMDomain%mesh%empty()) cycle
         if (this%root(i)%femdomain%y_max() >= ret)then
            ret = this%root(i)%femdomain%y_max()
         endif 
      enddo
   endif
   
end function
! #####################################################################




! #####################################################################
function z_min_Soybean(this) result(ret)
   class(Soybean_),intent(in) :: this
   real(real64) :: ret
   integer(int32) :: i

   ret = this%stem(1)%femdomain%z_min()
   if (allocated(this%stem))then
      do i=1,size(this%stem)
         if (this%stem(i)%FEMDomain%mesh%empty()) cycle
         if (this%stem(i)%femdomain%z_min() <= ret)then
            ret = this%stem(i)%femdomain%z_min()
         endif 
      enddo
   endif
   
   if (allocated(this%leaf))then
      do i=1,size(this%leaf)
         if (this%leaf(i)%FEMDomain%mesh%empty()) cycle
         if (this%leaf(i)%femdomain%z_min() <= ret)then
            ret = this%leaf(i)%femdomain%z_min()
         endif 
      enddo
   endif

   if (allocated(this%root))then
      do i=1,size(this%root)
         if (this%root(i)%FEMDomain%mesh%empty()) cycle
         if (this%root(i)%femdomain%z_min() <= ret)then
            ret = this%root(i)%femdomain%z_min()
         endif 
      enddo
   endif
   
end function
! #####################################################################


! #####################################################################
function z_max_Soybean(this) result(ret)
   class(Soybean_),intent(in) :: this
   real(real64) :: ret
   integer(int32) :: i

   ret = this%stem(1)%femdomain%z_max()
   if (allocated(this%stem))then
      do i=1,size(this%stem)
         if (this%stem(i)%FEMDomain%mesh%empty()) cycle
         if (this%stem(i)%femdomain%z_max() >= ret)then
            ret = this%stem(i)%femdomain%z_max()
         endif 
      enddo
   endif
   
   if (allocated(this%leaf))then
      do i=1,size(this%leaf)
         if (this%leaf(i)%FEMDomain%mesh%empty()) cycle
         if (this%leaf(i)%femdomain%z_max() >= ret)then
            ret = this%leaf(i)%femdomain%z_max()
         endif 
      enddo
   endif

   if (allocated(this%root))then
      do i=1,size(this%root)
         if (this%root(i)%FEMDomain%mesh%empty()) cycle
         if (this%root(i)%femdomain%z_max() >= ret)then
            ret = this%root(i)%femdomain%z_max()
         endif 
      enddo
   endif
   
end function
! #####################################################################

! #####################################################################
subroutine init_as_seed_soybean(this,radius,division) 
   class(Soybean_),intent(inout) :: this
   real(real64),intent(in) :: radius(1:3)
   integer(int32),intent(in) :: division(1:3)
   real(real64),allocatable :: x(:)
   real(real64) :: epsilon,cv_peti_angles(1:2),cv_peti_angles_z(1:2),cv_leaf_angles(1:2)
   type(Stem_) :: stem
   type(Leaf_) :: leaf
   type(Random_) :: random
   integer(int32) :: i,j

   ! 一旦地上部のみ

   this%stem_division = division
   call stem%init( &
               x_num=this%stem_division(1), &
               y_num=this%stem_division(2), &
               z_num=this%stem_division(3) &
               )

   this%peti_width_ave = 0.0d0
   this%peti_width_sig = 0.0d0

   ! num_stem: 2, num_peti: 2
   this%MaxStemNum = 4
   ! num_leaf: 2
   this%MaxLeafNum = 2
   ! num_root: 1
   this%MaxRootNum = 0

   allocate (this%stem(this%MaxstemNum))
   allocate (this%leaf(this%MaxLeafNum))
   !allocate (this%root(this%MaxrootNum))

   allocate (this%stem2stem(this%MaxstemNum, this%MaxstemNum))
   allocate (this%leaf2stem(this%MaxLeafNum, this%MaxStemNum))
   !allocate (this%root2stem(this%MaxrootNum, this%MaxstemNum))
   !allocate (this%root2root(this%MaxrootNum, this%MaxrootNum))
   
   this%stem2stem(:,:) = 0
   this%leaf2stem(:,:) = 0
   !this%root2stem(:,:) = 0
   !this%root2root(:,:) = 0

   this%stem2stem(2, 1) = PF_SOYBEAN_MAINSTEM_TO_MAINSTEM
   this%leaf2stem(1:2, 1) = 1
   !this%root2stem(1, 1) = 1
   !this%root2root(:, :) = 0

   this%ms_node = 2
   allocate (this%NodeID_MainStem(this%ms_node))
   this%NodeID_MainStem = [1,2]

   ! 胚軸の比率
   this%ms_width = radius(2)*0.10d0
   this%ms_length = radius(3)*0.50d0
   epsilon = radius(2)*0.030d0

   this%leaf_division = division

   do i = 1, this%ms_node


      this%stem(i) = stem

      this%stem(i)%stemID = 0
      this%stem(i)%InterNodeID = i
      this%stem(i)%already_grown = .true.

      this%NodeID_MainStem(i) = i
      
      call this%stem(i)%resize( &
         x=this%ms_width, &
         y=this%ms_width, &
         z=this%ms_length/dble(this%ms_node) &
         )
      if (i>1)then
         x = this%stem(i-1)%getCoordinate("B") - this%stem(i)%getCoordinate("A")
         call this%stem(i)%move( &
            x=x(1), &
            y=x(2), &
            z=x(3)-epsilon &
            )
      endif

      call this%stem(i)%rotate(y=radian(-30.0d0)*dble(i-1))
      !call this%stem(i)%rotate( &
      !      x=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)), &
      !      y=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)), &
      !      z=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)) &
      !   )

   end do
   
   


   call leaf%init(species=PF_SOYBEAN_CV, &
                           width=radius(1),&
                           length=radius(2),&
                           thickness=radius(3),&
                           x_num=this%leaf_division(1), &
                           y_num=this%leaf_division(2), &
                           z_num=this%leaf_division(3) &
                           )
   

   this%num_stem_node = this%ms_node
   this%num_leaf = 0
   cv_peti_angles_z = [radian(-10.0d0),radian(10.0d0)]
   cv_peti_angles = [radian(80.0d0),radian(-80.0d0)]
   cv_leaf_angles = [radian(0.0d0),radian(180.0d0)]
   do i = 1, 2
      ! 子葉につき複葉なし
      ! add peti
      this%num_stem_node = this%num_stem_node + 1
      this%stem(this%num_stem_node) = stem
      this%stem(this%num_stem_node)%already_grown = .true.

      ! 胚軸葉柄サイズ
      call this%stem(this%num_stem_node)%resize( &
         x=radius(1)*0.050d0, &
         y=radius(2)*0.050d0, &
         z=radius(3)*0.10d0 &
         )
      call this%stem(this%num_stem_node)%rotate( &
         x=cv_peti_angles(i), &
         y=0.0d0, &
         z=cv_peti_angles_z(i) &
         )
      call this%stem(this%num_stem_node)%connect("=>", this%stem(1))
      this%stem2stem(this%num_stem_node, 1) = PF_SOYBEAN_PETIOLE_TO_MAINSTEM

      
      ! add leaves

      ! ??
      !leaf_z_angles = linspace([0.0d0, 360.0d0], this%max_num_leaf_per_petiole + 1)
      !do j = 1, this%max_num_leaf_per_petiole
      !   leaf_z_angles(j) = radian(leaf_z_angles(j))
      !end do
      !leaf_z_angles(:) = leaf_z_angles(:) + radian(random%random()*360.0d0)

      
      
      do j = 1, 1 !this%max_num_leaf_per_petiole
         this%num_leaf = this%num_leaf + 1
         this%leaf(this%num_leaf) = leaf
         this%leaf(this%num_leaf)%LeafID = j

         !y_val = random%gauss(mu=this%leaf_thickness_ave(i), sigma=this%leaf_thickness_sig(i))
         !z_val = random%gauss(mu=this%leaf_length_ave(i), sigma=this%leaf_length_sig(i))
         !x_val = random%gauss(mu=this%leaf_width_ave(i), sigma=this%leaf_width_sig(i))

         this%leaf(this%num_leaf)%already_grown = .true.

         
         !call this%leaf(this%num_leaf)%move( &
         !   y=-y_val/2.0d0, &
         !   z=-z_val/2.0d0, &
         !   x=-x_val/2.0d0 &
         !   )

         call this%leaf(this%num_leaf)%rotate( &
            x=cv_leaf_angles(i), &
            y=50.0d0, &
            z=0.0d0 &
            )
         
         call this%leaf(this%num_leaf)%connect("=>", this%stem(this%num_stem_node))

         this%leaf2stem(this%num_leaf, this%num_stem_node) = 1
      end do
      
   end do
   
   call this%update()

end subroutine
! #####################################################################

subroutine easy_grow_SoybeanClass(this,dt,&
      stem_grow_speed_coeff,l_max)
   class(Soybean_),intent(inout) :: this
   real(real64),intent(in) :: dt

   real(real64) :: stem_enlong_time
   real(real64),intent(in) :: l_max != 0.10d0 ! matured internode length
   real(real64),intent(in) :: stem_grow_speed_coeff != 0.10d0 ! 1/m/s


   real(real64) :: l_current,dldt

   integer(int32) :: stem_idx

   ! 理想的な成長をシミュレートする．
   
   ! (1) まずは節間伸長
   ! 節間伸長期間(stem_enlong_time (s))
   stem_enlong_time = 1.0d0*60.0d0*60.0d0 ! 3日程度
   do stem_idx = 1, size(this%stem)
      if(this%stem(stem_idx)%empty() ) cycle
      !if(this%stem(stem_idx)%already_grown) cycle

      ! if this stem internode is not grown, grow by logistic equation
      l_current = this%stem(stem_idx)%getLength()
      !print *, l_current
      if(l_current >= l_max)then
         dldt=0.0d0
      else
         dldt = stem_grow_speed_coeff*l_current*(1.0d0-l_current/l_max)
      endif
      print *, l_current, dldt*dt
      call this%stem(stem_idx)%change_length_or_width(length=l_current + dldt*dt)

   enddo

   call this%update()

end subroutine
! ############################################################
subroutine setMeristem_soybeanclass(this,stemIdx,dt,params)
   class(Soybean_),intent(inout) :: this
   real(real64),intent(in) :: dt, params(:)
   integer(int32),intent(in) :: stemIdx

   type(Meristem_),allocatable :: meristem_buf(:)
   integer(int32),allocatable :: meristem2stem_buf(:,:)
   integer(int32) :: meristem_idx

   if(.not.allocated(this%meristem))then
      allocate(this%meristem(1))
      
      this%meristem2stem = int(zeros(1,this%numStem()))
      meristem_idx = 1
   else
      meristem_buf = this%meristem
      deallocate(this%meristem)
      allocate(this%meristem(size(meristem_buf)+1))
      this%meristem(1:size(meristem_buf)) = meristem_buf(:)
      
      meristem_idx = size(this%meristem)
      meristem2stem_buf = this%meristem2stem
      this%meristem2stem = zeros(this%numMeristem(),size(meristem2stem_buf,2))
      this%meristem2stem(1:size(meristem2stem_buf,1),1:size(meristem2stem_buf,2)) &
         = meristem2stem_buf(:,:)
   endif
   call this%meristem(meristem_idx)%init(&
      Meristem_type=PF_MERISTEM_TYPE_SHOOT,&
      params=params, &
      dt=dt)
   if(this%isMainStem(stemIdx))then
      this%meristem2stem(meristem_idx,stemIdx) = PF_SOYBEAN_MERISTEM_TO_MAINSTEM
   else
      this%meristem2stem(meristem_idx,stemIdx) = PF_SOYBEAN_MERISTEM_TO_BRANCH
   endif
   call this%update()

end subroutine
! ##################################################

! ############################################################
subroutine setMeristem_multi_soy(this,stemIdx,dt,params)
   class(Soybean_),intent(inout) :: this
   real(real64),intent(in) :: dt, params(:)
   integer(int32),intent(in) :: stemIdx(:)

   type(Meristem_),allocatable :: meristem_buf(:)
   integer(int32),allocatable :: meristem2stem_buf(:,:)
   integer(int32) :: idx, this_stemIdx

   do idx=1,size(stemIdx)
      this_stemIdx = stemIdx(idx)
      call this%setMeristem(stemIdx=this_stemIdx,dt=dt,params=params(:))
      call this%update()
   enddo

end subroutine
! ##################################################


! ##################################################
function numMeristemSoybean(this) result(ret)
   class(Soybean_),intent(in) :: this
   integer(int32) :: ret

   if(allocated(this%meristem))then
      ret = size(this%meristem)
   else
      ret = 0
   endif

end function
! ##################################################

! ##################################################
function getMainStemTipIdx_Soy(this) result(StemIdx)
   class(Soybean_),intent(in) :: this
   integer(int32) :: StemIdx

   StemIdx = maxval(this%NodeID_MainStem)

end function
! ##################################################

! ##################################################
function getBranchStemTipIdx_Soy(this) result(StemIdxList)
   class(Soybean_),intent(in) :: this
   integer(int32),allocatable :: StemIdxList(:)
   integer(int32),allocatable :: BranchBaseStemIdx(:)
   integer(int32) :: branch_idx
   
   BranchBaseStemIdx = this%getBranchBaseStemIdx()
   allocate(StemIdxList(size(BranchBaseStemIdx)))
   do branch_idx=1,size(BranchBaseStemIdx)
      StemIdxList(branch_idx) = this%getTipOfStem(stemIdx = BranchBaseStemIdx(branch_idx))
   enddo

end function
! ##################################################

! ##################################################
function getBranchBaseStemIdx_Soy(this) result(ret)
   class(Soybean_),intent(in) :: this
   integer(int32),allocatable :: ret(:)
   integer(int32) :: n_branch, branch_idx, i,j

   n_branch = this%getNumberOfBranch()
   allocate(ret(n_branch))
   branch_idx=0
   if(allocated(this%stem2stem))then
      do i=1,size(this%stem2stem,1) ! branch
         do j=1,size(this%stem2stem,2) ! mainstem
            if(this%stem2stem(i,j)==PF_SOYBEAN_BRANCH_TO_MAINSTEM) then
               branch_idx = branch_idx + 1
               ret(branch_idx) = i
            endif
         enddo
      enddo
   endif

   

end function
! ##################################################


! ##################################################
function getNumberOfBranch_Soy(this) result(ret)
   class(Soybean_),intent(in) :: this
   integer(int32) :: ret,i,j

   ret = 0
   if(allocated(this%stem2stem))then
      do i=1,size(this%stem2stem,1)
         do j=1,size(this%stem2stem,2)
            if(this%stem2stem(i,j)==PF_SOYBEAN_BRANCH_TO_MAINSTEM) then
               ret = ret + 1
            endif
         enddo
      enddo
   endif

end function
! ##################################################


! ##################################################
recursive function getTipOfStem_SoybeanClass(this,stemIdx) result(ret)
   class(Soybean_),intent(in) :: this
   integer(int32),intent(in)  :: stemIdx
   integer(int32) :: ret, i, j
   
   ret = stemIdx
   if(allocated(this%stem2stem))then
      if(this%isMainStem(stemIdx))then 
         do i=1,size(this%stem2stem,2)   
            if( this%stem2stem(i,stemIdx)==PF_SOYBEAN_MAINSTEM_TO_MAINSTEM ) then
               ret = this%getTipOfStem(stemIdx=i)
               return
            else
               cycle
            endif
         enddo
      else
         do i=1,size(this%stem2stem,2)   
            if( this%stem2stem(i,stemIdx)==PF_SOYBEAN_BRANCH_TO_BRANCH ) then
               ret = this%getTipOfStem(stemIdx=i)
               return
            else
               cycle
            endif
         enddo
      endif
   endif
   

end function
! ##################################################


! ##################################################
recursive function getNumberOfInternode_Soy(this,BranchIdx,stemIdx) result(ret)
   class(Soybean_),intent(in) :: this
   integer(int32),intent(in) :: BranchIdx
   integer(int32),optional,intent(in) :: stemIdx
   integer(int32) :: ret,n,i,current_stem_idx
   integer(int32),allocatable :: BranchBaseStemIdx(:)

   ! Main stem => branch_idx = 0 
   ! Branch stem => branch_idx >= 1
   ret = 1
   if(branchIdx<=0)then
      current_stem_idx = 1
   else
      BranchBaseStemIdx = this%getBranchBaseStemIdx() 
      if(BranchIdx > size(BranchBaseStemIdx)) then
         ret = 0
         return
      endif
      current_stem_idx  = BranchBaseStemIdx(BranchIdx)
   endif
   
   if(present(stemIdx))then
      current_stem_idx = stemIdx
   endif

   if(BranchIdx<=0)then
      ! main stem
      do i=1,size(this%stem2stem,2)
         if(this%stem2stem(i,current_stem_idx)==PF_SOYBEAN_MAINSTEM_TO_MAINSTEM)then
            ret = ret + this%getNumberOfInternode(BranchIdx=BranchIdx,stemIdx=i)
            return
         endif
      enddo
   else
      ! branch
      do i=1,size(this%stem2stem,2)
         if(this%stem2stem(i,current_stem_idx)==PF_SOYBEAN_BRANCH_TO_BRANCH)then
            ret = ret + this%getNumberOfInternode(BranchIdx=BranchIdx,stemIdx=i)
            return
         endif
      enddo
   endif
   ret = 1

end function
! ##################################################


! ##################################################
recursive function getInternodes_Soy(this,BranchIdx,stemIdx) result(ret)
   class(Soybean_),intent(in) :: this
   integer(int32),intent(in) :: BranchIdx
   integer(int32),optional,intent(in) :: stemIdx
   integer(int32) :: n,i,current_stem_idx
   integer(int32),allocatable :: BranchBaseStemIdx(:), ret(:)

   ! Main stem => branch_idx = 0 
   ! Branch stem => branch_idx >= 1
   
   if(branchIdx<=0)then
      current_stem_idx = 1
   else
      BranchBaseStemIdx = this%getBranchBaseStemIdx() 
      if(BranchIdx > size(BranchBaseStemIdx)) then
         ret = zeros(0)
         return
      endif
      current_stem_idx  = BranchBaseStemIdx(BranchIdx)
   endif
   
   if(present(stemIdx))then
      current_stem_idx = stemIdx
   endif

   ret = [current_stem_idx]

   if(BranchIdx<=0)then
      ! main stem
      do i=1,size(this%stem2stem,2)
         if(this%stem2stem(i,current_stem_idx)==PF_SOYBEAN_MAINSTEM_TO_MAINSTEM)then
            ret = ret // this%getInternodes(BranchIdx=BranchIdx,stemIdx=i)
            return
         endif
      enddo
   else
      ! branch
      do i=1,size(this%stem2stem,2)
         if(this%stem2stem(i,current_stem_idx)==PF_SOYBEAN_BRANCH_TO_BRANCH)then
            ret = ret // this%getInternodes(BranchIdx=BranchIdx,stemIdx=i)
            return
         endif
      enddo
   endif
   ret(1) = stemIdx

end function
! ##################################################


! ##################################################
function to_soybean_soybeanclass(&
      node_length,node_weight_g,node_diameter,peti_diameter,&
      peti_length,leaf_length,leaf_width,&
      leaf_thickness,&
      num_leaf_set,num_leaf_per_set,leaf_peti_weight_g) result(ret)
   
   real(real64),  intent(in) :: node_length(:)
   real(real64),  intent(in) :: node_weight_g(:)
   real(real64),  intent(in) :: node_diameter(:)

   
   real(real64),  intent(in) :: peti_length(:)
   real(real64),  intent(in) :: peti_diameter(:)

   real(real64),  intent(in) :: leaf_length(:)
   real(real64),  intent(in) :: leaf_width(:)

   integer(int32),intent(in) :: num_leaf_set(:)
   integer(int32),intent(in) :: num_leaf_per_set(:)
   real(real64),  intent(in) :: leaf_peti_weight_g(:),leaf_thickness

   type(Soybean_) :: ret
   type(Math_) :: math
   type(Random_) :: random
   integer(int32) :: i, j, stem_idx,leaf_idx,k,leaf_ne_sum
   real(real64) ::  y_val,z_val,x_val,z_angle,leafset_volume,&
      stem_volume,stem_weight_g,stem_density,leafset_weight_g,leafset_density
   integer(int32) :: leaf_idx_range(1:2),stem_idx_range(1:2)

   allocate(ret%stem(size(node_length)+sum(num_leaf_set)))
   allocate(ret%leaf(dot_product(num_leaf_per_set,num_leaf_set)))
   allocate(ret%stem2stem(size(ret%stem),size(ret%stem)))
   allocate(ret%leaf2stem(size(ret%leaf),size(ret%stem)))
   allocate(ret%stemDensity(0))
   allocate(ret%leafDensity(0))
   
   ret%stem2stem = 0
   ret%leaf2stem = 0
   ! main stem
   stem_idx = 0
   do i=1,size(node_length)
      stem_idx = stem_idx + 1
      call ret%stem(i)%init()
      call ret%stem(i)%resize(&
         x = node_diameter(i),&
         y = node_diameter(i),&
         z = node_length(i))
      if(i>=2)then
         ret%stem2stem(i,i-1)=PF_SOYBEAN_MERISTEM_TO_MAINSTEM
      endif
      call ret%update()

      stem_volume = ret%stem(i)%getVolume()!m^3
      stem_weight_g = node_weight_g(stem_idx) !kg
      stem_density = stem_weight_g/stem_volume/1000.0d0/1000.0d0
      ret%stem(i)%density = stem_density*ones(ret%stem(i)%ne())
      ret%stemDensity = ret%stemDensity // stem_density*ones(ret%stem(i)%ne())
      
   enddo
   
   ! petiole and leaf
   leaf_idx = 0
   do i=1,size(num_leaf_set)
      leafset_volume = 0.0d0
      leaf_idx_range(1) = leaf_idx + 1
      stem_idx_range(1) = stem_idx + 1
      do j=1,num_leaf_set(i)
         stem_idx = stem_idx + 1
         call ret%stem(stem_idx)%init()
         call ret%stem(stem_idx)%resize(&
               x = peti_diameter(i),&
               y = peti_diameter(i),&
               z = peti_length(i))
         z_angle = mod(stem_idx,2)*math%pi
         call ret%stem(stem_idx)%rotate(&
               x = radian(50.0d0),&
               z = z_angle &
            )
         ret%stem2stem(stem_idx,i)=PF_SOYBEAN_PETIOLE_TO_MAINSTEM
         call ret%update()

         leafset_volume = leafset_volume + ret%stem(stem_idx)%getVolume()
         
         
         
         ! leafset 
         leaf_ne_sum = 0
         do k=1,num_leaf_per_set(i)
            leaf_idx = leaf_idx + 1
            call ret%leaf(leaf_idx)%init(species=PF_GLYCINE_SOJA)
            leaf_ne_sum = leaf_ne_sum + ret%leaf(leaf_idx)%ne()
            ret%leaf(leaf_idx)%already_grown = .true.

            y_val = leaf_thickness
            z_val = leaf_length(i)
            x_val = leaf_width(i)
            call ret%leaf(leaf_idx)%resize( &
                  y=y_val, &
                  z=z_val, &
                  x=x_val &
               )
            call ret%leaf(leaf_idx)%move( &
               y=-y_val/2.0d0, &
               z=-z_val/2.0d0, &
               x=-x_val/2.0d0 &
               )
            call ret%leaf(leaf_idx)%rotate( &
               x=radian(90.0d0), &
               y=0.0d0, &
               z= z_angle + radian(360.0d0/dble(num_leaf_per_set(i)))*(k-1), reset=.true. &
               )
            call ret%leaf(leaf_idx)%connect("=>", ret%stem(stem_idx))
            ret%leaf2stem(leaf_idx, stem_idx) = PF_SOYBEAN_LEAF_TO_PETIOLE

            leafset_volume = leafset_volume + ret%leaf(leaf_idx)%getVolume()
         enddo
      enddo
      leafset_weight_g = leaf_peti_weight_g(i)
      leafset_density = leafset_weight_g/leafset_volume/1000.0d0/1000.0d0 ! t/m^3 or g/cm^3
      ! petiole density
      
      ret%leafDensity = ret%leafDensity // leafset_density*ones(leaf_ne_sum)
      leaf_idx_range(2) = leaf_idx
      stem_idx_range(2) = stem_idx
      do j=leaf_idx_range(1),leaf_idx_range(2)
         ret%leaf(j)%density = leafset_density*ones(ret%leaf(j)%ne())
      enddo
      print *, stem_idx_range(:)
      do j=stem_idx_range(1),stem_idx_range(2)        
         ret%stem(j)%density = leafset_density*ones(ret%stem(j)%ne())
         ret%stemDensity = ret%stemDensity // ret%stem(j)%density 
      enddo

   enddo
   




end function
! ##################################################
end module
