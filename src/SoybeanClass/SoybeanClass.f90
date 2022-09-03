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
    implicit none

    integer(int32),parameter :: PF_SOY_OBJECT_WISE  = 1

    type :: soybean_internode_info_
        real(real64),allocatable :: FinalInterNodeLength(:)
        real(real64),allocatable :: FinalPetioleLength(:)
        real(real64),allocatable :: FinalLeafLength(:)
        real(real64),allocatable :: FinalLeafWidth(:)
    end type
    
    type :: soybean_NodeID_Branch_
        integer(int32),allocatable :: ID(:)
    contains 
        procedure, public :: sync => syncsoybean_NodeID_Branch
    end type
    
    integer(int32),parameter :: PF_DEFORMATION_ANALYSIS=100
    integer(int32),parameter :: PF_DEFAULT_SOYBEAN_ASIZE=300
    
    type :: soybean_


        ! setting
        integer(int32) :: stem_division(1:3) = [3, 3, 30]
        integer(int32) :: peti_division(1:3) = [3, 3, 30]
        integer(int32) :: leaf_division(1:3) = [10, 1, 20]
        integer(int32) :: root_division(1:3)  = [2, 2, 20]

        ! growth_habit = determinate, indeterminate, semi-indeterminate, or vine
        character*20 :: growth_habit
        character*2  :: growth_stage
        integer(int32) :: Num_Of_Node
        integer(int32) :: num_leaf
        integer(int32) :: num_stem_node
        integer(int32) :: Num_Of_Root

        integer(int32) :: TYPE_STEM    = 1
        integer(int32) :: TYPE_LEAF    = 2
        integer(int32) :: TYPE_ROOT    = 3

        integer(int32) :: MaxLeafNum= PF_DEFAULT_SOYBEAN_ASIZE
        integer(int32) :: MaxRootNum= PF_DEFAULT_SOYBEAN_ASIZE
        integer(int32) :: MaxStemNum= PF_DEFAULT_SOYBEAN_ASIZE

        logical :: determinate
        integer(int32) :: max_num_leaf_per_petiole = 3 ! as default

        integer(int32)  :: ms_node,br_node(PF_DEFAULT_SOYBEAN_ASIZE),br_from(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: ms_length,br_length(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: ms_width,br_width(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: ms_angle_ave,br_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: ms_angle_sig,br_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE)


        integer(int32)  :: mr_node,brr_node(PF_DEFAULT_SOYBEAN_ASIZE),brr_from(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: mr_length,brr_length(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: mr_width,brr_width(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: mr_angle_ave,brr_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: mr_angle_sig,brr_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE)

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
        integer(int32)::stage_id=0
        real(real64) :: dt
        type(Seed_) :: Seed
        type(PlantNode_),allocatable :: NodeSystem(:)
        type(PlantRoot_),allocatable :: RootSystem(:)

        type(Stem_),allocatable :: Stem(:)
        type(Leaf_),allocatable :: Leaf(:)
        type(Root_),allocatable :: Root(:)


        ! material info
        real(real64),allocatable :: stemYoungModulus(:)
        real(real64),allocatable :: leafYoungModulus(:)
        real(real64),allocatable :: rootYoungModulus(:)

        real(real64),allocatable :: stemPoissonRatio(:)
        real(real64),allocatable :: leafPoissonRatio(:)
        real(real64),allocatable :: rootPoissonRatio(:)

        real(real64),allocatable :: stemDensity(:)
        real(real64),allocatable :: leafDensity(:)
        real(real64),allocatable :: rootDensity(:)

        ! 節-節点データ構造
        type(Mesh_) :: struct 
        integer(int32),allocatable :: leaf2stem(:,:)
        integer(int32),allocatable :: stem2stem(:,:)
        integer(int32),allocatable :: root2stem(:,:)
        integer(int32),allocatable :: root2root(:,:)
        
        ! 器官オブジェクト配列 (regacy)
        type(FEMDomain_),allocatable :: leaf_list(:)
        type(FEMDomain_),allocatable :: stem_list(:)
        type(FEMDomain_),allocatable :: root_list(:)
        
        ! シミュレータ
        type(ContactMechanics_) :: contact
        real(real64) :: time
        real(real64) :: seed_length
        real(real64) :: seed_width
        real(real64) :: seed_height
        real(real64),allocatable :: stem_angle(:,:)
        real(real64),allocatable :: root_angle(:,:)
        real(real64),allocatable :: leaf_angle(:,:)
        
        character(200) :: stemconfig=""
        character(200) :: rootconfig=""
        character(200) :: leafconfig=""
        
        ! for deformation analysis
        logical :: property_deform_material_density = .false.
        logical :: property_deform_material_YoungModulus = .false.
        logical :: property_deform_material_PoissonRatio = .false.
        logical :: property_deform_initial_Displacement = .false.
        logical :: property_deform_initial_Stress = .false.
        logical :: property_deform_boundary_TractionForce = .false.
        logical :: property_deform_boundary_Displacement = .false.
        logical :: property_deform_gravity = .false.
        
        real(real64) :: Gravity_acceralation = 9.810d0
        real(real64) :: PenaltyParameter = 100000.0d0
        logical :: GaussPointProjection = .false.
        
        
        integer(int32),allocatable :: NodeID_MainStem(:)
        type(soybean_NodeID_Branch_),allocatable :: NodeID_Branch(:)
        
        logical ::  inLoop = .false.
        real(real64) :: hours = 0.0d0
        
        ! growth simulation
        real(real64) :: FullyExpanded_stem_threshold = 0.10d0
        integer(int32) :: MaxBranchNum = 20
        type(soybean_internode_info_),allocatable :: InterNodeInfo(:)
        real(real64) :: default_Leaf_growth_ratio = 1.0d0/3.0d0
        real(real64) :: default_Stem_growth_ratio = 1.0d0/3.0d0
        integer(int32),allocatable :: MainStem_num_branch(:)
        real(real64) :: apical_dominance_distance = 1.0d0


        ! create CV
        real(real64) :: CV_stem_length_ave    = 0.03d0
        real(real64) :: CV_stem_length_sig    = 0.001d0
        real(real64) :: CV_stem_width_ave     = 0.003d0
        real(real64) :: CV_stem_width_sig     = 0.00001d0
        real(real64) :: CV_leaf_length_ave    = 0.03d0
        real(real64) :: CV_leaf_length_sig    = 0.001d0
        real(real64) :: CV_leaf_width_ave     = 0.02d0
        real(real64) :: CV_leaf_width_sig     = 0.0005d0
        real(real64) :: CV_leaf_thickness_ave = 0.005d0
        real(real64) :: CV_leaf_thickness_sig = 0.0001d0

        real(real64) :: VC_stem_length_ave    = 0.04d0
        real(real64) :: VC_stem_length_sig    = 0.001d0
        real(real64) :: VC_stem_width_ave     = 0.004d0
        real(real64) :: VC_stem_width_sig     = 0.0001d0

        real(real64) :: VC_leaf_length_ave    = 0.03d0
        real(real64) :: VC_leaf_length_sig    = 0.001d0
        real(real64) :: VC_leaf_width_ave     = 0.03d0
        real(real64) :: VC_leaf_width_sig     = 0.0005d0
        real(real64) :: VC_leaf_thickness_ave = 0.001d0
        real(real64) :: VC_leaf_thickness_sig = 0.00001d0

    contains
        !procedure,public :: addRoot => addRootSoybean
        !procedure,public :: addLeaf => addLeafSoybean

        ! creation
        procedure,public :: Init => initsoybean
        procedure,public :: VC => VCSoybean


        procedure,public :: remove => removeSoybean
        procedure,public :: create => initsoybean
        procedure,public :: new => initsoybean
        procedure,public :: sowing => initsoybean
        procedure,public :: export => exportSoybean
        procedure,public :: expanition => expanitionSoybean
        procedure,public :: development => developmentSoybean

        !  Simulator
        procedure,public :: checkProperties => checkPropertiesSoybean
        procedure,public :: setPoints    => setPointsSoybean
        procedure,public :: setProperties => setPropertiesSoybean


        ! simple setters
        procedure,public :: addStem => addStemSoybean
        procedure,public :: setPropertiesDensity => setPropertiesDensitySoybean
        procedure,public :: setPropertiesYoungModulus => setPropertiesYoungModulusSoybean
        procedure,public :: setPropertiesPoissonRatio => setPropertiesPoissonRatioSoybean
        procedure,public :: setPropertiesInitialDisplacement => setPropertiesInitialDisplacementSoybean
        procedure,public :: setPropertiesInitialStress => setPropertiesInitialStressSoybean
        procedure,public :: setPropertiesBoundaryTractionForce => setPropertiesBoundaryTractionForceSoybean
        procedure,public :: setPropertiesBoundaryDisplacement => setPropertiesBoundaryDisplacementSoybean
        procedure,public :: setPropertiesGravity => setPropertiesGravitySoybean

        ! alternative setters
        procedure,public :: setYoungModulus => setYoungModulusSoybean
        procedure,public :: setPoissonRatio => setPoissonRatioSoybean
        procedure,public :: setDensity => setDensitySoybean

        procedure,public :: runSimulation => runSimulationSoybean
        procedure,public :: runSimulator => runSimulationSoybean
        ! readyForSoybean
        procedure,public :: readyFor => readyForSoybean



        ! observation/info
        procedure,public :: stemlength => stemlengthSoybean
        procedure,public :: NumberOfBranch => NumberOfBranchSoybean
        procedure,public :: isMainStem => isMainStemSoybean   
        procedure,public :: isBranchStem => isBranchStemSoybean     

        procedure,public :: checkYoungModulus => checkYoungModulusSoybean
        procedure,public :: checkPoissonRatio => checkPoissonRatioSoybean
        procedure,public :: checkDensity => checkDensitySoybean

        procedure,public :: checkMemoryRequirement => checkMemoryRequirementSoybean
        
        
        procedure,public :: getYoungModulus => getYoungModulusSoybean
        procedure,public :: getPoissonRatio => getPoissonRatioSoybean
        procedure,public :: getDensity => getDensitySoybean

        procedure,public :: getYoungModulusField => getYoungModulusFieldSoybean
        procedure,public :: getPoissonRatioField => getPoissonRatioFieldSoybean
        procedure,public :: getDensityField => getDensityFieldSoybean

        procedure,public :: getElementList => getElementListSoybean
        
        ! operation
        procedure,public :: findApical => findApicalSoybean
        
        procedure,public :: grow => growSoybean
        procedure,public :: getVolume => getVolumeSoybean
        procedure,public :: getVolumePerElement => getVolumePerElementSoybean
        procedure,public :: getBioMass => getBioMassSoybean
        procedure,public :: getTotalWeight => getTotalWeightSoybean
        procedure,public :: getSubDomain => getSubDomainSoybean
        procedure,public :: getSubDomainType => getSubDomainTypeSoybean
        procedure,public :: setSubDomain => setSubDomainSoybean
        procedure,public :: getPoints    => getPointsSoybean
        procedure,public :: getRadius    => getRadiusSoybean
        procedure,public :: getCenter    => getCenterSoybean
        procedure,public :: getDistanceFromGround    => getDistanceFromGroundSoybean
        procedure,public :: getNumberOfPoint => getNumberOfPointSoybean
        procedure,public :: getNumberOfElement => getNumberOfElementSoybean
        procedure,public :: getDistanceToGroundFromStemID &
            => getDistanceToGroundFromStemIDSoybean
        procedure,public :: getDistanceToGroundFromRootID &
            => getDistanceToGroundFromRootIDSoybean
        procedure,public :: getLeafCosValue => getLeafCosValueSoybean
        
        procedure,public :: getRangeOfNodeID => getRangeOfNodeIDSoybean
        procedure,public :: getFEMDomainPointers => getFEMDomainPointersSoybean
        procedure,public :: fall_leaf => fall_leafSoybean

        ! >> simulation 
        procedure,public :: getPPFD => getPPFDSoybean
        
        procedure,public :: getDisplacement => getDisplacementSoybean
        procedure,public :: getEigenMode => getEigenModeSoybean
        
        procedure,public :: getPhotoSynthesis => getPhotoSynthesisSoybean
        procedure,public :: getPhotoSynthesisSpeedPerVolume => getPhotoSynthesisSpeedPerVolumeSoybean
        procedure,public :: getLeafArea => getLeafAreaSoybean
        procedure,public :: getIntersectLeaf => getIntersectLeafSoybean
        procedure,public :: getOverwrapLeaf => getIntersectLeafSoybean
        
        procedure,public :: searchStem => searchStemSoybean
        procedure,public :: searchPetiole =>searchPetioleSoybean
        procedure,public :: searchLeaf => searchLeafSoybean
        
        ! post-processing
        procedure,public :: export_eig => export_eigSoybean
        procedure,public :: getStressField => getStressFieldSoybean

        ! max *** ID
        procedure,public :: maxleafID => maxleafIDSoybean
        procedure,public :: maxInterNodeID => maxInterNodeIDSoybean
        procedure,public :: maxPetioleID   => maxPetioleIDSoybean
        procedure,public :: maxStemID      => maxStemIDSoybean


        ! data-format converter
        procedure,public :: convertDataFormat => convertDataFormatSoybean
        
        procedure,public :: fixReversedElements => fixReversedElementsSoybean
        
        procedure,public :: resize => resizeSoybean
        procedure,public :: deform => deformSoybean
        ! MPI
        procedure,public :: sync => syncSoybean
        
        ! visualization
        procedure,public :: show => showSoybean
        procedure,public :: gmsh => gmshSoybean
        procedure,public :: msh => mshSoybean
        procedure,public :: vtk => vtkSoybean
        procedure,public :: stl => stlSoybean
        procedure,public :: json => jsonSoybean
        
        ! get info
        !procedure,public :: properties => propertiesSoybean
        ! number of subdomain
        procedure,public :: ns => nsSoybean
        ! number of element
        procedure,public :: ne => neSoybean
        ! number of points
        procedure,public :: nn => nnSoybean
        procedure,public :: np => nnSoybean
        procedure,public :: branchID =>branchIDSoybean

        ! regacy/experimental
        procedure,public :: WaterAbsorption => WaterAbsorptionSoybean
        procedure,public :: move => moveSoybean
        procedure,public :: rotate => rotateSoybean

        procedure,public :: numleaf => numleafsoybean
        procedure,public :: numstem => numstemsoybean
        procedure,public :: numroot => numrootsoybean

        procedure,public :: laytracing => laytracingsoybean
        procedure,public :: SinkSourceFlow => SinkSourceFlowSoybean

        procedure,public :: update => updateSoybean
        procedure,public :: updateFlowers => updateFlowersSoybean
        procedure,public :: updatePods => updatePodsSoybean
        procedure,public :: AddNode => AddNodeSoybean
        procedure,public :: AddPhytomere => AddNodeSoybean

        ! structure editor/analyzer
        procedure, pass ::  resizeStem => resizeStemSoybean
        procedure, pass ::  rotateStem => rotateStemSoybean
        procedure, pass ::  resizePetiole => resizePetioleSoybean
        procedure, pass ::  rotatePetiole => rotatePetioleSoybean
        procedure, pass ::  resizeLeaf => resizeLeafSoybean

        ! growth parameters
        procedure, pass :: setFinalInterNodeLength => setFinalInterNodeLengthSoybean
        procedure, pass :: setFinalPetioleLength   => setFinalPetioleLengthSoybean
        procedure, pass :: setFinalLeafLength      => setFinalLeafLengthSoybean
        procedure, pass :: setFinalLeafWidth       => setFinalLeafWidthSoybean

        
    end type

    type :: SoybeanCanopy_
        real(real64) :: inter_row, intra_row
        type(soybean_),allocatable :: Canopy(:,:)
    end type

contains

! 
subroutine VCSoybean(this) 
    class(Soybean_),intent(inout) :: this
    type(Random_) :: random
    integer(int32) :: i
    real(real64) :: y_val, z_val, x_val,leaf_z_angles(4)

    !initialize

    call this%remove()

    ! set default parameters

    ! set default parameters
    ! stem
    this%br_node(:)=0
    this%br_from(:)=0
    this%br_length(:)=0.0d0

    this%br_angle_ave(:)= 0.0d0
    this%br_angle_sig(:)=10.0d0
    !this%br_angle_ave(1)=10.0d0
    !this%br_angle_sig(1)=2.0d0
    
    this%ms_angle_ave=0.0d0
    this%ms_angle_sig=2.0d0

    ! for roots
    this%brr_node(:)=0
    this%brr_from(:)=0
    this%brr_length(:)=0.0d0

    this%brr_angle_ave(:)= 0.0d0
    this%brr_angle_sig(:)=10.0d0
    this%brr_angle_ave(1)=30.0d0
    this%brr_angle_sig(1)=2.0d0
    
    this%mr_angle_ave=0.0d0
    this%mr_angle_sig=2.0d0
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
    

    allocate(this%leaf(this%MaxLeafNum) )
    allocate(this%root(this%MaxrootNum) )
    allocate(this%stem(this%MaxstemNum) )

    allocate(this%leafYoungModulus(this%MaxLeafNum) )
    allocate(this%rootYoungModulus(this%MaxrootNum) )
    allocate(this%stemYoungModulus(this%MaxstemNum) )
    ! default value
    this%leafYoungModulus(:) = 1000.0d0
    this%rootYoungModulus(:) = 1000.0d0
    this%stemYoungModulus(:) = 1000.0d0
    
    allocate(this%leafPoissonRatio(this%MaxLeafNum) )
    allocate(this%rootPoissonRatio(this%MaxrootNum) )
    allocate(this%stemPoissonRatio(this%MaxstemNum) )
    this%leafPoissonRatio(:) = 0.30d0
    this%rootPoissonRatio(:) = 0.30d0
    this%stemPoissonRatio(:) = 0.30d0
    
    allocate(this%leafDensity(this%MaxLeafNum) )
    allocate(this%rootDensity(this%MaxrootNum) )
    allocate(this%stemDensity(this%MaxstemNum) )

    this%leafDensity(:) = 0.0d0
    this%rootDensity(:) = 0.0d0
    this%stemDensity(:) = 0.0d0

    allocate(this%stem2stem(this%MaxstemNum,this%MaxstemNum) )
    allocate(this%leaf2stem(this%MaxstemNum,this%MaxLeafNum) )
    allocate(this%root2stem(this%MaxrootNum,this%MaxstemNum) )
    allocate(this%root2root(this%MaxrootNum,this%MaxrootNum) )
    this%stem2stem(:,:) = 0
    this%leaf2stem(:,:) = 0
    this%root2stem(:,:) = 0
    this%root2root(:,:) = 0

    ! create VC plant
    ! create stage CV
    this%NodeID_MainStem = eyes(1)
    call this%stem(1)%init()
    
    this%stem(1)%stemID = 0
    this%stem(1)%InterNodeID = 1
    this%stem(1)%already_grown = .true.

    call this%stem(1)%resize(&
        x = random%gauss(mu=this%CV_stem_width_ave,sigma=this%CV_stem_width_sig) ,&
        y = random%gauss(mu=this%CV_stem_width_ave,sigma=this%CV_stem_width_sig) ,&
        z = random%gauss(mu=this%CV_stem_length_ave,sigma=this%CV_stem_length_sig) &
        )
    call this%stem(1)%move(&
        x = -this%stem(1)%femdomain%xmax()/2.0d0 ,&
        y = -this%stem(1)%femdomain%ymax()/2.0d0 ,&
        z = 0.0d0 &
        )
    
    call this%stem(1)%rotate( &
        z = radian( 360.0d0*random%random() )   &
        )     
    
    ! end of primary growth
    this%stem(1)%already_grown = .true.
    
    

    
    leaf_z_angles(1)  = random%random()*360.0d0
    leaf_z_angles(2)  = leaf_z_angles(1)+180.0d0
    leaf_z_angles(3)  = random%random()*360.0d0
    leaf_z_angles(4)  = leaf_z_angles(3)+180.0d0
    this%num_leaf = 0

    do i=1, 2
        this%num_leaf=this%num_leaf+1
        call this%leaf(i)%init(species=PF_SOYBEAN_CV)

        y_val = random%gauss(mu=this%CV_leaf_thickness_ave,sigma=this%CV_leaf_thickness_sig)  
        z_val = random%gauss(mu=this%CV_leaf_length_ave   ,sigma=this%CV_leaf_length_sig) 
        x_val = random%gauss(mu=this%CV_leaf_width_ave    ,sigma=this%CV_leaf_width_sig)
        
        this%leaf(i)%already_grown = .true.
        
        call this%leaf(i)%resize(&
            y =y_val , &
            z =z_val , &
            x =x_val  &
        )
        call this%leaf(i)%move(&
            y =-y_val/2.0d0 , &
            z =-z_val/2.0d0 , &
            x =-x_val/2.0d0  &
        )
        call this%leaf(i)%rotate(&
            x = radian(90.0d0), &
            y = 0.0d0, &
            z = radian(leaf_z_angles(i)) ,reset=.true.&
        )
        call this%leaf(i)%connect("=>",this%stem(1))
        this%leaf2stem(i,1) = 1
    enddo
    call this%update()

    ! create stage VC
    if(allocated(this%NodeID_Branch))then
        deallocate(this%NodeID_Branch)
    endif
    this%NodeID_MainStem = this%NodeID_MainStem // [2] 
    
    call this%stem(2)%init()
    this%stem(2)%stemID = 0
    this%stem(2)%InterNodeID = 1
    this%stem(2)%already_grown = .true.

    call this%stem(2)%resize(&
        x = random%gauss(mu=this%VC_stem_width_ave,sigma=this%VC_stem_width_sig) ,&
        y = random%gauss(mu=this%VC_stem_width_ave,sigma=this%VC_stem_width_sig) ,&
        z = random%gauss(mu=this%VC_stem_length_ave,sigma=this%VC_stem_length_sig) &
        )
    call this%stem(2)%move(&
        x = -this%stem(2)%femdomain%xmax()/2.0d0 ,&
        y = -this%stem(2)%femdomain%ymax()/2.0d0 ,&
        z = 0.0d0 &
        )
    
    call this%stem(2)%rotate( &
        z = radian( 360.0d0*random%random() )   &
        )     
    
    ! end of primary growth
    this%stem(2)%already_grown = .true.

    this%stem2stem(2,1) = 1



    
    
    do i=3,4
        this%num_leaf=this%num_leaf+1
        call this%leaf(i)%init(SoyWidthRatio=0.50d0)

        y_val = random%gauss(mu=this%VC_leaf_thickness_ave,sigma=this%VC_leaf_thickness_sig)  
        z_val = random%gauss(mu=this%VC_leaf_length_ave   ,sigma=this%VC_leaf_length_sig) 
        x_val = random%gauss(mu=this%VC_leaf_width_ave    ,sigma=this%VC_leaf_width_sig)
        
        this%leaf(i)%already_grown = .true.
        
        call this%leaf(i)%resize(&
            y =y_val , &
            z =z_val , &
            x =x_val  &
        )
        call this%leaf(i)%move(&
            y =-y_val/2.0d0 , &
            z =-z_val/2.0d0 , &
            x =-x_val/2.0d0  &
        )
        
        call this%leaf(i)%rotate(&
            x = radian(90.0d0), &
            y = 0.0d0, &
            z = radian(leaf_z_angles(i)) ,reset=.true.&
        )
        call this%leaf(i)%connect("=>",this%stem(2))
        this%leaf2stem(i,2) = 1
    enddo

    call this%update()
    ! no root

    this%stem2stem(2,1) = 1


end subroutine

! ########################################
recursive subroutine updateSoybean(obj,stem_id, root_id, leaf_id, overset_margin,debug)
    class(Soybean_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: stem_id, root_id, leaf_id    
    real(real64),optional,intent(in) :: overset_margin
    integer(int32) :: i,j,this_stem_id,next_stem_id,A_id,B_id,itr_tol,itr,k,kk
    integer(int32) :: this_leaf_id,next_leaf_id
    integer(int32) :: this_root_id,next_root_id,InterNodeID,PetioleID,StemID,LeafID
    real(real64) :: x_A(3),x_B(3),diff(3),error,last_error,mgn,overset_m,error_tol
    logical,optional,intent(in) :: debug

    if(obj%default_Leaf_growth_ratio > 0.0d0)then
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() ) cycle
            obj%leaf(i)%length_growth_ratio = obj%default_Leaf_growth_ratio
            obj%leaf(i)%Width_growth_ratio = obj%default_Leaf_growth_ratio
        enddo
    endif


    if(obj%default_stem_growth_ratio > 0.0d0)then
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() ) cycle
            obj%stem(i)%length_growth_ratio = obj%default_stem_growth_ratio
            obj%stem(i)%Width_growth_ratio = obj%default_stem_growth_ratio
        enddo
    endif

    ! if soybean_internode_info_ is active
    ! update parameters
    if(allocated(obj%InterNodeInfo) )then
        do i=0,obj%MaxBranchNum
            
            if(allocated(obj%InterNodeInfo(i)%FinalInterNodeLength ) )then
                do j=1,obj%maxInterNodeID(StemID=i)
                    InterNodeID = obj%searchStem(StemID=i,InterNodeID=j)
                    if(size(obj%InterNodeInfo(i)%FinalInterNodeLength) < j ) then
                        print *, "ERROR :: updateSoybean >> "
                        print *, "size(obj%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
                        stop
                    endif
                    if(InterNodeID<1)then
                        cycle
                    endif
                    obj%stem(InterNodeID)%final_length = obj%InterNodeInfo(i)%FinalInterNodeLength(j)
                enddo
            endif
            
            if(allocated(obj%InterNodeInfo(i)%FinalPetioleLength) )then
                do j=1,obj%maxInterNodeID(StemID=i)
                    do k=1,obj%maxPetioleID(StemID=i,InterNodeID=j)
                        if(size(obj%InterNodeInfo(i)%FinalPetioleLength) < j ) then
                            print *, "ERROR :: updateSoybean >> "
                            print *, "size(obj%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
                            stop
                        endif

                        PetioleID = obj%searchPetiole(StemID=i,InterNodeID=j,PetioleID=k)
                        
                        obj%stem(PetioleID)%final_length = obj%InterNodeInfo(i)%FinalPetioleLength(j)
                    enddo
                enddo
            endif

                if(allocated(obj%InterNodeInfo(i)%FinalLeafLength) )then
                    do j=1,obj%maxInterNodeID(StemID=i)
                        do k=1,obj%maxPetioleID(StemID=i,InterNodeID=j)
                            do kk = 1, obj%maxleafID(StemID=i,InterNodeID=j,PetioleID=k)
                                if(size(obj%InterNodeInfo(i)%FinalLeafLength) < j ) then
                                    print *, "ERROR :: updateSoybean >> "
                                    print *, "size(obj%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
                                    stop
                                endif
                                LeafID = obj%searchleaf(StemID=i,InterNodeID=j,PetioleID=k,LeafID=kk)
                                obj%leaf(LeafID)%final_length = obj%InterNodeInfo(i)%FinalLeafLength(j)
                            enddo
                        enddo
                    enddo
                endif


                if(allocated(obj%InterNodeInfo(i)%FinalLeafWidth) )then
                    do j=1,obj%maxInterNodeID(StemID=i)
                        do k=1,obj%maxPetioleID(StemID=i,InterNodeID=j)
                            do kk = 1, obj%maxleafID(StemID=i,InterNodeID=j,PetioleID=k)
                                if(size(obj%InterNodeInfo(i)%FinalLeafWidth) < j ) then
                                    print *, "ERROR :: updateSoybean >> "
                                    print *, "size(obj%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
                                    stop
                                endif
                                LeafID = obj%searchleaf(StemID=i,InterNodeID=j,PetioleID=k,LeafID=kk)
                                obj%leaf(LeafID)%final_Width = obj%InterNodeInfo(i)%FinalLeafWidth(j)
                            enddo
                        enddo
                    enddo
                endif
            
        enddo
    endif

    ! update connectivity
    if(.not. allocated(obj%stem2stem ))then
        print *, "updateSoybean >> ERROR :: .not. allocated(obj%stem2stem )"
        return
    endif



    error_tol = dble(1.0e-14)

    ! margin between subdomains
    overset_m = input(default=0.03d0, option=overset_margin)
    
    itr_tol = 100
    itr=0

    ! if debug
    !if(present(debug) )then
    !    if(debug)then
    !        print *, "obj%stem2stem"
    !        call print(obj%stem2stem)
    !    endif
    !endif

    ! stem to stem
    last_error = 1.0d0
    if(maxval(obj%stem2stem)/=0) then
        

        do 
            itr=itr+1
            error = 0.0d0
            do i=1, size(obj%stem2stem,1)
                do j=1, size(obj%stem2stem,2)
                    this_stem_id = j
                    next_stem_id = i
                    if(obj%stem2stem(i,j)/=0 .and. i /= j)then
                        ! this_stem_id ===>>> next_stem_id, connected!

                        !x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                        !x_A(:) = obj%stem(next_stem_id)%getCoordinate("A")
                        ! Overset分食い込ませる
                        x_B(:) = (1.0d0-overset_m)*obj%stem(this_stem_id)%getCoordinate("B")&
                            + overset_m*obj%stem(this_stem_id)%getCoordinate("A")
                        ! Overset分食い込ませる
                        x_A(:) = (1.0d0-overset_m)*obj%stem(next_stem_id)%getCoordinate("A") &
                            + overset_m*obj%stem(next_stem_id)%getCoordinate("B")


                        diff(:) = x_B(:) - x_A(:)
                        
                        error = error + dot_product(diff,diff)
                        call obj%stem(next_stem_id)%move(x=diff(1),y=diff(2),z=diff(3) )

                    endif
                enddo
            enddo
            if(present(debug) )then
                if(debug)then
                    print *, "soybean % update s2s >> error :: ",error
                endif
            endif
            if(itr > itr_tol) then
                print *, "soybean % update s2s >> ERROR :: not converged"
                stop
            endif

            if( abs(error) + abs(last_error) < error_tol) exit
            last_error = error
        enddo
    endif

    ! root to stem
    if(allocated(obj%root2stem) )then
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%root2stem,1)
            do j=1, size(obj%root2stem,2)
                this_stem_id = j
                next_root_id = i
                if(obj%root2stem(i,j)==1)then
                    ! this_stem_id ===>>> next_root_id, connected!
                    !x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    !x_A(:) = obj%root(next_root_id)%getCoordinate("A")

                    ! Overset分食い込ませる
                    x_B(:) = (1.0d0-overset_m)*obj%stem(this_stem_id)%getCoordinate("A")&
                        + overset_m*obj%stem(this_stem_id)%getCoordinate("B")
                    ! Overset分食い込ませる
                    x_A(:) = (1.0d0-overset_m)*obj%root(next_root_id)%getCoordinate("A") &
                        + overset_m*obj%root(next_root_id)%getCoordinate("B")
                        

                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%root(next_root_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "soybean % update r2s >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "soybean % update r2s  >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error)  < error_tol) exit
        last_error = error
    enddo
    endif

    
    if(allocated(obj%root2root) )then
    ! root to root
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%root2root,1)
            do j=1, size(obj%root2root,2)
                this_root_id = j
                next_root_id = i
                if(next_root_id==1)then
                    cycle
                endif
                if(obj%root2root(i,j)/=0 .and. i /= j)then
                    ! this_root_id ===>>> next_root_id, connected!
                    !x_B(:) = obj%root(this_root_id)%getCoordinate("B")
                    !x_A(:) = obj%root(next_root_id)%getCoordinate("A")
                    
                    ! Overset分食い込ませる
                    x_B(:) = (1.0d0-overset_m)*obj%root(this_root_id)%getCoordinate("B")&
                        + overset_m*obj%root(this_root_id)%getCoordinate("A")
                    ! Overset分食い込ませる
                    x_A(:) = (1.0d0-overset_m)*obj%root(next_root_id)%getCoordinate("A") &
                        + overset_m*obj%root(next_root_id)%getCoordinate("B")


                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%root(next_root_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "soybean % update r2r >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "soybean % update r2r >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error)  < error_tol) exit
        last_error = error
    enddo
    endif

    ! leaf to stem
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%leaf2stem,1)
            do j=1, size(obj%leaf2stem,2)
                this_stem_id = j
                next_leaf_id = i
                if(obj%leaf2stem(i,j)==1)then
                    ! this_stem_id ===>>> next_leaf_id, connected!
                    !x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    !x_A(:) = obj%leaf(next_leaf_id)%getCoordinate("A")

                    ! Overset分食い込ませる
                    x_B(:) = (1.0d0-overset_m)*obj%stem(this_stem_id)%getCoordinate("B")&
                        + overset_m*obj%stem(this_stem_id)%getCoordinate("A")
                    ! Overset分食い込ませる
                    x_A(:) = obj%leaf(next_leaf_id)%getCoordinate("A")
                        

                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%leaf(next_leaf_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "soybean % update l2s >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "soybean % update l2s  >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error)  < error_tol) exit
        last_error = error
    enddo

    
    
end subroutine
! ########################################

! ########################################
subroutine initsoybean(obj,config,&
    regacy,mass,water_content,radius,location,x,y,z,&
    PlantRoot_diameter_per_seed_radius,max_PlantNode_num,Variety,FileName,&
    max_leaf_num,max_stem_num,max_root_num,profiler)
    class(Soybean_),intent(inout) :: obj

    real(real64),optional,intent(in) :: mass,water_content,radius,location(3),x,y,z
    real(real64),optional,intent(in) :: PlantRoot_diameter_per_seed_radius
    character(*),optional,intent(in) :: Variety,FileName,config
    logical,optional,intent(in) :: regacy, profiler
    character(:),allocatable :: fn,conf,line
    integer(int32),optional,intent(in) :: max_PlantNode_num,max_leaf_num,max_stem_num,max_root_num
    real(real64) :: MaxThickness,Maxwidth,loc(3),vec(3),rot(3),zaxis(3),meshloc(3),meshvec(3),&
        x_val,y_val,z_val
    integer(int32) :: i,j,k,blcount,id,rmc,n,node_id,node_id2,elemid,branch_id,num_stem_node
    
    real(real64)::readvalreal
    real(real64),allocatable :: leaf_z_angles(:)
    integer(int32) :: readvalint
    logical :: debug=.false.
    logical :: timeOpt = .false.
    type(IO_) :: soyconf
    type(Random_) :: random
    type(Time_) :: time
    type(Stem_) :: stem
    type(Leaf_) :: leaf
    type(Root_) :: root


    timeOpt = input(default=.false.,option=profiler)

    !if(.not.allocated(obj%InterNodeInfo) )then
    !    allocate(this%InterNodeInfo(0:obj%MaxBranchNum) )
    !    ! default value
    !    do i=0,size(obj%InterNodeInfo)
    !        obj%InterNodeInfo(i)%FinalInterNodeLength = linspace([0.030d0,0.060d0],30)
    !        obj%InterNodeInfo(i)%FinalLeafLength      = linspace([0.050d0,0.20d0],30)
    !        obj%InterNodeInfo(i)%FinalLeafWidth       = linspace([0.020d0,0.25d0],30)
    !        obj%InterNodeInfo(i)%FinalPetioleLength   = linspace([0.050d0,0.25d0],30)
    !    enddo
    !endif

    if(timeOpt) then
        call time%start()
    endif


    call obj%remove()
    ! set default parameters
    ! stem
    obj%br_node(:)=0
    obj%br_from(:)=0
    obj%br_length(:)=0.0d0

    obj%br_angle_ave(:)= 0.0d0
    obj%br_angle_sig(:)=10.0d0
    !obj%br_angle_ave(1)=10.0d0
    !obj%br_angle_sig(1)=2.0d0
    
    obj%ms_angle_ave=0.0d0
    obj%ms_angle_sig=2.0d0

    ! for roots
    obj%brr_node(:)=0
    obj%brr_from(:)=0
    obj%brr_length(:)=0.0d0

    obj%brr_angle_ave(:)= 0.0d0
    obj%brr_angle_sig(:)=10.0d0
    obj%brr_angle_ave(1)=30.0d0
    obj%brr_angle_sig(1)=2.0d0
    
    obj%mr_angle_ave=0.0d0
    obj%mr_angle_sig=2.0d0
    ! peti
    ! is also stem
    
    obj%peti_size_ave(:) = 0.20d0
    obj%peti_size_sig(:) = 0.010d0

    obj%peti_width_ave(:) = 0.0050d0
    obj%peti_width_sig(:) = 0.00010d0

    obj%peti_angle_ave(:) = 30.0d0
    obj%peti_angle_sig(:) = 1.00d0

    ! leaf
    obj%leaf_length_ave(:) = 0.20d0
    obj%leaf_length_sig(:) = 0.01d0

    obj%leaf_width_ave(:) = 0.050d0
    obj%leaf_width_sig(:) = 0.010d0

    obj%leaf_thickness_ave(:) = 0.00100d0
    obj%leaf_thickness_sig(:) = 0.00050d0

    obj%leaf_angle_ave(:) = 80.0d0
    obj%leaf_angle_sig(:) = 10.0d0
    

    
    if(timeOpt) then
        print *, "[1] set default values :: "
        call time%show()
    endif
    

    
    ! 子葉節、初生葉節、根の第1節まで種子の状態で存在

    ! 節を生成するためのスクリプトを開く
    if(.not.present(config).or. index(config,".json")==0 )then
        ! デフォルトの設定を生成
        print *, "New soybean-configuration >> soyconfig.json"
        call soyconf%open("soyconfig.json")
        write(soyconf%fh,*) '{'
        write(soyconf%fh,*) '   "type": "soybean",'
        write(soyconf%fh,*) '   "stage": 0,'
        write(soyconf%fh,*) '   "length": 0.0090,'
        write(soyconf%fh,*) '   "width" : 0.0081,'
        write(soyconf%fh,*) '   "height": 0.0072,'
        write(soyconf%fh,*) '   "MaxLeafNum": 50,'
        write(soyconf%fh,*) '   "MaxRootNum":200,'
        write(soyconf%fh,*) '   "MaxStemNum": 50,'

        ! stem
        write(soyconf%fh,*) '   "br_node" : 0,'
        write(soyconf%fh,*) '   "br_from" : 0,'
        write(soyconf%fh,*) '   "br_length" : 0.00,'
        write(soyconf%fh,*) '   "br_angle_ave" : 0.00,'
        write(soyconf%fh,*) '   "br_angle_sig" : 10.00,'
        write(soyconf%fh,*) '   "br_angle_ave(1)": 0.00,'
        write(soyconf%fh,*) '   "br_angle_sig(1)": 10.00,'
        write(soyconf%fh,*) '   "ms_angle_ave": 0.00,'
        write(soyconf%fh,*) '   "ms_angle_sig": 2.00,'


        ! root
        write(soyconf%fh,*) '   "brr_node" : 0,'
        write(soyconf%fh,*) '   "brr_from" : 0,'
        write(soyconf%fh,*) '   "brr_length" : 0.00,'
        write(soyconf%fh,*) '   "brr_angle_ave" : 0.00,'
        write(soyconf%fh,*) '   "brr_angle_sig" : 10.00,'
        write(soyconf%fh,*) '   "brr_angle_ave(1)": 360.00,'
        write(soyconf%fh,*) '   "brr_angle_sig(1)": 2.00,'
        write(soyconf%fh,*) '   "mr_angle_ave": 0.00,'
        write(soyconf%fh,*) '   "mr_angle_sig": 2.00,'
        ! peti
        ! is also stem
        write(soyconf%fh,*) '   "peti_size_ave"  :  0.200,'
        write(soyconf%fh,*) '   "peti_size_sig"  :  0.0100,'
        write(soyconf%fh,*) '   "peti_width_ave"  :  0.00500,'
        write(soyconf%fh,*) '   "peti_width_sig"  :  0.000100,'
        write(soyconf%fh,*) '   "peti_angle_ave"  :  30.00,'
        write(soyconf%fh,*) '   "peti_angle_sig"  :  1.000,'
        ! leaf
        write(soyconf%fh,*) '   "leaf_length_ave"  :  0.200,'
        write(soyconf%fh,*) '   "leaf_length_sig"  :  0.010,'
        write(soyconf%fh,*) '   "leaf_width_ave"  :  0.0500,'
        write(soyconf%fh,*) '   "leaf_width_sig"  :  0.0100,'
        write(soyconf%fh,*) '   "leaf_thickness_ave"  :  0.001000,'
        write(soyconf%fh,*) '   "leaf_thickness_sig"  :  0.000500,'
        write(soyconf%fh,*) '   "leaf_angle_ave"  :  80.00,'
        write(soyconf%fh,*) '   "leaf_angle_sig"  :  10.00'
        write(soyconf%fh,*) '}'
        conf="soyconfig.json"
        call soyconf%close()
    else
        conf = config
    endif
    

    if(timeOpt) then
        print *, "[2] create default config "
        call time%show()
    endif

    line = soyconf%parse(conf,key1="Genotype",key2="Dt1")
    if(index(line,"Dt1")/=0 )then
        obj%determinate=.False.
    else
        obj%determinate=.True.
    endif

    call soyconf%open(conf)
    blcount=0
    do
        read(soyconf%fh,'(a)') line
        if(debug) print *, line
        if( adjustl(line)=="{" )then
            blcount=1
            cycle
        endif
        if( adjustl(line)=="}" )then
            exit
        endif
        
        if(blcount==1)then
            
            if(index(line,"Name")/=0)then
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%name
            endif

            if(index(line,"Mainstem")/=0)then
                do
                    read(soyconf%fh,'(a)') line
                    if(debug) print *, line
                    if( index(line,"}")/=0 )then
                        exit
                    endif
                    
                    if(index(line,"Length")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%ms_length
                    endif

                    if(index(line,"Width")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%ms_width
                    endif
                    
                    if(index(line,"Node")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%ms_node
                    endif

                    
                
                enddo
            endif

            if(index(line,"Branch#")/=0)then
                rmc=index(line,"{")
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,'"')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,'"')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,':')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,"#")
                if(debug) print *, line
                read(line(id+1:),*) branch_id

                do
                    read(soyconf%fh,'(a)') line
                    if(debug) print *, line
                    if( index(line,"}")/=0 )then
                        exit
                    endif
                    
                    if(index(line,"Length")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%br_length(branch_id)
                    endif

                    if(index(line,"Width")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%br_Width(branch_id)
                    endif
                    
                    if(index(line,"Node")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%br_node(branch_id)
                    endif

                    if(index(line,"From")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%br_from(branch_id)
                    endif
                
                enddo
            endif

            ! for roots

            if(index(line,"Mainroot")/=0)then
                do
                    read(soyconf%fh,'(a)') line
                    if(debug) print *, line
                    if( index(line,"}")/=0 )then
                        exit
                    endif
                    
                    if(index(line,"Length")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%mr_length
                    endif

                    if(index(line,"Width")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%mr_width
                    endif
                    
                    if(index(line,"Node")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%mr_node
                    endif

                    
                
                enddo
            endif

            if(index(line,"Branchroot#")/=0)then
                rmc=index(line,"{")
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,'"')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,'"')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,':')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,"#")
                if(debug) print *, line
                read(line(id+1:),*) branch_id

                do
                    read(soyconf%fh,'(a)') line
                    if(debug) print *, line
                    if( index(line,"}")/=0 )then
                        exit
                    endif
                    
                    if(index(line,"Length")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%brr_length(branch_id)
                    endif

                    if(index(line,"Width")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%brr_Width(branch_id)
                    endif
                    
                    if(index(line,"Node")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%brr_node(branch_id)
                    endif

                    if(index(line,"From")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%brr_from(branch_id)
                    endif
                
                enddo
            endif


            if(index(line,"rootconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%rootconfig
            endif

            if(index(line,"stemconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%stemconfig
            endif

            if(index(line,"leafconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%leafconfig
            endif


            if(index(line,"stage")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%stage_id
            endif


            if(index(line,"MaxLeafNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxLeafNum
            endif


            if(index(line,"MaxStemNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxStemNum
            endif


            if(index(line,"MaxRootNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxRootNum
            endif

            if(index(line,"length")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_length
            endif

            if(index(line,"width")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_width
            endif

            if(index(line,"height")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_height
            endif


            ! for version 2020.11.24

            ! stem
            if(index(line,"br_angle_ave") /=0 .and. index(line,"br_angle_ave(") ==0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%br_angle_ave(:) = readvalreal
            endif
            
            if(index(line,"br_angle_sig") /=0 .and. index(line,"br_angle_sig(") ==0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%br_angle_sig(:) = readvalreal
            endif

            if(index(line,"br_angle_ave(1)")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%br_angle_ave(1) = readvalreal
            endif
            if(index(line,"br_angle_sig(1)")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%br_angle_sig(1) = readvalreal
            endif

            if(index(line,"ms_angle_ave")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%ms_angle_ave = readvalreal
            endif
            
            if(index(line,"ms_angle_sig")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%ms_angle_sig = readvalreal
            endif
            ! peti
            ! is also stem
            
            if(index(line,"peti_size_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_size_ave(:) = readvalreal
            endif
            
            if(index(line,"peti_size_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_size_sig(:) = readvalreal
            endif
            
            if(index(line,"peti_width_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_width_ave(:) = readvalreal
            endif
            
            if(index(line,"peti_width_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_width_sig(:) = readvalreal
            endif
            
            if(index(line,"peti_angle_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_angle_ave(:) = readvalreal
            endif
            
            if(index(line,"peti_angle_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_angle_sig(:) = readvalreal
            endif
            ! leaf
            
            if(index(line,"leaf_length_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_length_ave(:) = readvalreal
            endif
            
            if(index(line,"leaf_length_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_length_sig(:) = readvalreal
            endif
            
            if(index(line,"leaf_width_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_width_ave(:) = readvalreal
            endif
            
            if(index(line,"leaf_width_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_width_sig(:) = readvalreal
            endif
            
            if(index(line,"leaf_thickness_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_thickness_ave(:) = readvalreal
            endif
            
            if(index(line,"leaf_thickness_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_thickness_sig(:) = readvalreal
            endif
            
            if(index(line,"leaf_angle_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_angle_ave(:) = readvalreal
            endif
            
            if(index(line,"leaf_angle_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_angle_sig(:) = readvalreal
            endif


            ! added in 2020/12/15
            ! for roots



            if(index(line,"brr_angle_ave") /=0 .and. index(line,"brr_angle_ave(") ==0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%brr_angle_ave(:) = readvalreal
            endif
            
            if(index(line,"brr_angle_sig") /=0 .and. index(line,"brr_angle_sig(") ==0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%brr_angle_sig(:) = readvalreal
            endif

            if(index(line,"brr_angle_ave(1)")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%brr_angle_ave(1) = readvalreal
            endif
            if(index(line,"brr_angle_sig(1)")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%brr_angle_sig(1) = readvalreal
            endif

            if(index(line,"mr_angle_ave")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%mr_angle_ave = readvalreal
            endif
            
            if(index(line,"mr_angle_sig")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%mr_angle_sig = readvalreal
            endif


            cycle

        endif

    enddo
    call soyconf%close()

    

    if(index(config,".json")==0 )then
        obj%stemconfig=" "
        obj%rootconfig=" "
        obj%leafconfig=" "
    endif


    if(timeOpt) then
        print *, "[3] read config "
        call time%show()
    endif

    if(obj%ms_node/=0)then
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
        !num_node = countif(obj%ms_node,notEquai=.true.,0)
        !num_node = num_node + countif(obj%br_node,notEquai=.true.,0)
        
        allocate(obj%leaf(obj%MaxLeafNum) )
        allocate(obj%root(obj%MaxrootNum) )
        allocate(obj%stem(obj%MaxstemNum) )

        allocate(obj%leafYoungModulus(obj%MaxLeafNum) )
        allocate(obj%rootYoungModulus(obj%MaxrootNum) )
        allocate(obj%stemYoungModulus(obj%MaxstemNum) )
        ! default value
        obj%leafYoungModulus(:) = 1000.0d0
        obj%rootYoungModulus(:) = 1000.0d0
        obj%stemYoungModulus(:) = 1000.0d0
        
        allocate(obj%leafPoissonRatio(obj%MaxLeafNum) )
        allocate(obj%rootPoissonRatio(obj%MaxrootNum) )
        allocate(obj%stemPoissonRatio(obj%MaxstemNum) )
        obj%leafPoissonRatio(:) = 0.30d0
        obj%rootPoissonRatio(:) = 0.30d0
        obj%stemPoissonRatio(:) = 0.30d0
        
        allocate(obj%leafDensity(obj%MaxLeafNum) )
        allocate(obj%rootDensity(obj%MaxrootNum) )
        allocate(obj%stemDensity(obj%MaxstemNum) )

        obj%leafDensity(:) = 0.0d0
        obj%rootDensity(:) = 0.0d0
        obj%stemDensity(:) = 0.0d0

        allocate(obj%stem2stem(obj%MaxstemNum,obj%MaxstemNum) )
        allocate(obj%leaf2stem(obj%MaxstemNum,obj%MaxLeafNum) )
        allocate(obj%root2stem(obj%MaxrootNum,obj%MaxstemNum) )
        allocate(obj%root2root(obj%MaxrootNum,obj%MaxrootNum) )
        obj%stem2stem(:,:) = 0
        obj%leaf2stem(:,:) = 0
        obj%root2stem(:,:) = 0
        obj%root2root(:,:) = 0

        ! set mainstem
        
        allocate(obj%NodeID_MainStem(obj%ms_node) )

        
        if( index(obj%stemconfig,".json")==0 )then
            call stem%init( &
                x_num = obj%stem_division(1),&
                y_num = obj%stem_division(2),&
                z_num = obj%stem_division(3) &
            )
        else
            call stem%init(config=obj%stemconfig)
        endif

        do i=1,obj%ms_node

            !call obj%stem(i)%init(config=obj%stemconfig)

            obj%stem(i) = stem
            
            obj%stem(i)%stemID = 0
            obj%stem(i)%InterNodeID = i
            obj%stem(i)%already_grown = .true.

            obj%NodeID_MainStem(i) = i
            call obj%stem(i)%resize(&
                x = obj%ms_width, &
                y = obj%ms_width, &
                z = obj%ms_length/dble(obj%ms_node) &
                )
            call obj%stem(i)%move(&
                x = -obj%ms_width/2.0d0, &
                y = -obj%ms_width/2.0d0, &
                z = -obj%ms_length/dble(obj%ms_node)/2.0d0 &
                )
            
            call obj%stem(i)%rotate(&
                x = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
                y = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
                z = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig))   &
                )
            
        enddo


        if(timeOpt) then
            print *, "[4] created Main stem."
            call time%show()
        endif
        
        do i=1,obj%ms_node-1
            call obj%stem(i+1)%connect("=>",obj%stem(i))
            obj%stem2stem(i+1,i) = 1
        enddo

        ! set branches
        k=obj%ms_node
        allocate(obj%NodeID_Branch( size(obj%br_node) ) )
        do i=1,size(obj%br_node) ! num branch
            allocate( obj%NodeID_Branch(i)%ID(obj%br_node(i))  )
            do j=1, obj%br_node(i)

                k = k + 1
                !call obj%stem(k)%init(config=obj%stemconfig)
                obj%stem(k) = stem
                obj%stem(k)%stemID = i
                obj%stem(k)%InterNodeID = j
                obj%stem(k)%already_grown = .true.

                obj%NodeID_Branch(i)%ID(j) = k

                call obj%stem(k)%resize(&
                    x = obj%br_width(i), &
                    y = obj%br_width(i), &
                    z = obj%br_length(i)/dble(obj%br_node(i) ) &
                    )
                
                call obj%stem(k)%move(&
                    x = -obj%br_width(i)/2.0d0, &
                    y = -obj%br_width(i)/2.0d0, &
                    z = -obj%br_length(i)/dble(obj%br_node(i) )/2.0d0 &
                    )
                call obj%stem(k)%rotate(&
                    x = radian(random%gauss(mu=obj%br_angle_ave(j),sigma=obj%br_angle_sig(j) )),  &
                    y = 0.0d0,  &
                    z = radian(360.0d0*random%random() )   &
                    )                
                
                if(j==1)then
                    call obj%stem(k)%connect("=>",obj%stem(obj%br_from(i)  ))
                    obj%stem2stem(k,obj%br_from(i) ) = 1
                else
                    call obj%stem(k)%connect("=>",obj%stem(k-1))
                    obj%stem2stem(k,k-1) = 1
                endif
                    
            enddo
        enddo
        
        if(timeOpt) then
            print *, "[4] created Branches."
            call time%show()
        endif




        ! peti and leaf
        obj%num_stem_node = k
        obj%num_leaf = 0
        ! bugfix 2021/08/18
        !call leaf%init(config=obj%leafconfig,species=PF_GLYCINE_SOJA)

        if( index(obj%leafconfig,".json")==0 )then
            call leaf%init(species=PF_GLYCINE_SOJA, &
                x_num = obj%leaf_division(1),&
                y_num = obj%leaf_division(2),&
                z_num = obj%leaf_division(3) &
            )
        else
            call leaf%init(config=obj%leafconfig,species=PF_GLYCINE_SOJA)
        endif


        if(.not.stem%empty())then
            call stem%remove()
        endif

        if( index(obj%stemconfig,".json")==0 )then
            
            call stem%init( &
                x_num = obj%peti_division(1),&
                y_num = obj%peti_division(2),&
                z_num = obj%peti_division(3) &
            )
        else
            call stem%init(config=obj%stemconfig)
        endif

        do i=1, k
            ! ３複葉
            ! add peti
            obj%num_stem_node = obj%num_stem_node +1
            !call obj%stem(obj%num_stem_node)%init(config=obj%stemconfig)
            obj%stem(obj%num_stem_node) = stem
            obj%stem(obj%num_stem_node)%already_grown = .true.
            
            call obj%stem(obj%num_stem_node)%resize(&
                x = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
                y = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
                z = random%gauss(mu=obj%peti_size_ave(i),sigma=obj%peti_size_sig(i)) &
                )
            call obj%stem(obj%num_stem_node)%rotate(&
                x = radian(random%gauss(mu=obj%peti_angle_ave(i),sigma=obj%peti_angle_sig(i) )),  &
                y = 0.0d0,  &
                z = radian(360.0d0*random%random() )   &
                )      
            call obj%stem(obj%num_stem_node)%connect("=>",obj%stem(i))
            !obj%leaf2stem(num_stem_node,i) = 1   
            obj%stem2stem(obj%num_stem_node,i) = 1            

            

            ! add leaves
            
            
            leaf_z_angles = linspace([0.0d0,360.0d0],obj%max_num_leaf_per_petiole+1 )
            do j = 1, obj%max_num_leaf_per_petiole
                leaf_z_angles(j) = radian(leaf_z_angles(j))
            enddo

            leaf_z_angles(:) = leaf_z_angles(:) + radian(random%random()*360.0d0)

            do j=1,obj%max_num_leaf_per_petiole
                obj%num_leaf=obj%num_leaf+1
                !call obj%leaf(obj%num_leaf)%init(config=obj%leafconfig,species=PF_GLYCINE_SOJA)
                obj%leaf(obj%num_leaf) = leaf
                obj%leaf(obj%num_leaf)%LeafID = j

                y_val = random%gauss(mu=obj%leaf_thickness_ave(i),sigma=obj%leaf_thickness_sig(i))  
                z_val = random%gauss(mu=obj%leaf_length_ave(i)   ,sigma=obj%leaf_length_sig(i)) 
                x_val = random%gauss(mu=obj%leaf_width_ave(i)    ,sigma=obj%leaf_width_sig(i))
                
                obj%leaf(obj%num_leaf)%already_grown = .true.
                
                call obj%leaf(obj%num_leaf)%resize(&
                    y =y_val , &
                    z =z_val , &
                    x =x_val  &
                )
                call obj%leaf(obj%num_leaf)%move(&
                    y =-y_val/2.0d0 , &
                    z =-z_val/2.0d0 , &
                    x =-x_val/2.0d0  &
                )
                
                call obj%leaf(obj%num_leaf)%rotate(&
                    x = radian(random%gauss(mu=obj%leaf_angle_ave(i),sigma=obj%leaf_angle_sig(i))), &
                    y = 0.0d0, &
                    z = leaf_z_angles(j) &
                )
                call obj%leaf(obj%num_leaf)%connect("=>",obj%stem(obj%num_stem_node))
                obj%leaf2stem(obj%num_leaf,obj%num_stem_node) = 1
            enddo
            
        enddo


        if(timeOpt) then
            print *, "[4] created Peti and Leaves."
            call time%show()
        endif

        ! set mainroot
        !call root%init(obj%rootconfig)

        if( index(obj%rootconfig,".json")==0 )then
            call root%init( &
                x_num = obj%root_division(1),&
                y_num = obj%root_division(2),&
                z_num = obj%root_division(3) &
            )
        else
            call root%init(config=obj%rootconfig)
        endif

        
        do i=1,obj%mr_node

            obj%root(i) = root
            obj%root(i)%already_grown = .true.

            call obj%root(i)%resize(&
                x = obj%mr_width, &
                y = obj%mr_width, &
                z = obj%mr_length/dble(obj%mr_node) &
                )
            call obj%root(i)%move(&
                x = -obj%mr_width/2.0d0, &
                y = -obj%mr_width/2.0d0, &
                z = -obj%mr_length/dble(obj%mr_node)/2.0d0 &
                )
            call obj%root(i)%rotate(&
                x = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig)),  &
                y = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig)),  &
                z = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig))   &
                )                
        enddo

        do i=1,obj%mr_node-1
            if(i==1)then
                call obj%root(1)%connect("=>",obj%stem(1))    
                obj%root2stem(1,1) = 1
            endif
            call obj%root(i+1)%connect("=>",obj%root(i))
            obj%root2root(i+1,i) = 1
        enddo

        ! set branches
        k=obj%mr_node
        do i=1,size(obj%brr_node)
            do j=1, obj%brr_node(i)
                k = k + 1
                !call obj%root(k)%init(config=obj%rootconfig)
                obj%root(k) = root
                obj%root(k)%already_grown = .true.

                call obj%root(k)%resize(&
                    x = obj%mr_width, &
                    y = obj%mr_width, &
                    z = obj%mr_length/dble(obj%mr_node) &
                    )
                call obj%root(k)%move(&
                    x = -obj%mr_width/2.0d0, &
                    y = -obj%mr_width/2.0d0, &
                    z = -obj%mr_length/dble(obj%mr_node)/2.0d0 &
                    )
                call obj%root(k)%rotate(&
                    x = radian(random%gauss(mu=obj%brr_angle_ave(j),sigma=obj%brr_angle_sig(j) )),  &
                    y = 0.0d0,  &
                    z = radian(360.0d0*random%random() )   &
                    )                
                
                if(j==1)then
                    call obj%root(k)%connect("=>",obj%root(obj%brr_from(i)  ))
                    obj%root2root(k,obj%brr_from(i) ) = 1
                else
                    call obj%root(k)%connect("=>",obj%root(k-1))
                    obj%root2root(k,k-1) = 1
                endif
                    
            enddo
        enddo
        
        
        obj%stage = "V"//str(obj%ms_node)
        
        call obj%update()

        call obj%fixReversedElements()

        if(timeOpt) then
            print *, "[4] create objects."
        call time%show()
    endif
        return
    else
        ! create leaf, root, stem
        allocate(obj%leaf(obj%MaxLeafNum) )
        allocate(obj%root(obj%MaxrootNum) )
        allocate(obj%stem(obj%MaxstemNum) )

        allocate(obj%leafYoungModulus(obj%MaxLeafNum) )
        allocate(obj%rootYoungModulus(obj%MaxrootNum) )
        allocate(obj%stemYoungModulus(obj%MaxstemNum) )
        ! default value
        obj%leafYoungModulus(:) = 1000.0d0
        obj%rootYoungModulus(:) = 1000.0d0
        obj%stemYoungModulus(:) = 1000.0d0
        
        allocate(obj%leafPoissonRatio(obj%MaxLeafNum) )
        allocate(obj%rootPoissonRatio(obj%MaxrootNum) )
        allocate(obj%stemPoissonRatio(obj%MaxstemNum) )
        obj%leafPoissonRatio(:) = 0.30d0
        obj%rootPoissonRatio(:) = 0.30d0
        obj%stemPoissonRatio(:) = 0.30d0
        
        allocate(obj%leafDensity(obj%MaxLeafNum) )
        allocate(obj%rootDensity(obj%MaxrootNum) )
        allocate(obj%stemDensity(obj%MaxstemNum) )

        obj%leafDensity(:) = 0.0d0
        obj%rootDensity(:) = 0.0d0
        obj%stemDensity(:) = 0.0d0

        allocate(obj%stem2stem(obj%MaxstemNum,obj%MaxstemNum) )
        allocate(obj%leaf2stem(obj%MaxstemNum,obj%MaxLeafNum) )
        allocate(obj%root2stem(obj%MaxrootNum,obj%MaxstemNum) )
        allocate(obj%root2root(obj%MaxrootNum,obj%MaxrootNum) )
        
        !allocate(obj%struct%NodCoord(4,3) )
        !allocate(obj%struct%ElemNod(3,2) )
        !allocate(obj%struct%ElemMat(3) )
        ! 子葉結節部=(0,0,0)
        !obj%struct%NodCoord(1,1:3) = 0.0d0
        call obj%leaf(1)%init(obj%leafconfig,species=PF_GLYCINE_SOJA)
        call obj%leaf(1)%rotate(x=radian(90.0d0),y=radian(90.0d0),z=radian(10.0d0) )
        obj%leaf(1)%already_grown = .true.

        call obj%leaf(2)%init(obj%leafconfig,species=PF_GLYCINE_SOJA)
        call obj%leaf(2)%rotate(x=radian(90.0d0),y=radian(90.0d0),z=radian(-10.0d0) )
        obj%leaf(2)%already_grown = .true.

        call obj%stem(1)%init(obj%stemconfig)
        call obj%stem(1)%rotate(x=radian(40.0d0) )
        obj%stem(1)%already_grown = .true.
        
        call obj%stem(2)%init(obj%stemconfig)
        call obj%stem(2)%rotate(x=radian(80.0d0) )
        obj%stem(2)%already_grown = .true.
    
        call obj%root(1)%init(obj%rootconfig)
        call obj%root(1)%fix(x=0.0d0,y=0.0d0,z=0.0d0)
        call obj%root(1)%rotate(x=radian(-60.0d0) )
        obj%root(1)%already_grown = .true.
    
        call obj%leaf(1)%connect("=>",obj%stem(1))
        obj%leaf2stem(1,1) = 1
        
        call obj%leaf(2)%connect("=>",obj%stem(1))
        obj%leaf2stem(2,1) = 1
        
        call obj%stem(2)%connect("=>",obj%stem(1))
        obj%stem2stem(2,1) = 1
        
        call obj%root(1)%connect("=>",obj%stem(1))
        obj%root2stem(1,1) = 1
        
        obj%stage = "VE"
        ! 初生葉結節部
        !obj%struct%NodCoord(2,1) = 0.0d0
        !obj%struct%NodCoord(2,2) = 0.0d0
        !obj%struct%NodCoord(2,3) = 1.0d0/20.0d0*obj%seed_height
        ! 地際部
        !obj%struct%NodCoord(3,1) = 1.0d0/4.0d0*obj%seed_length
        !obj%struct%NodCoord(3,2) = 0.0d0
        !obj%struct%NodCoord(3,3) = -1.0d0/3.0d0*obj%seed_height
        ! 根冠
        !obj%struct%NodCoord(4,1) = 1.0d0/2.0d0*obj%seed_length
        !obj%struct%NodCoord(4,2) = 0.0d0
        !obj%struct%NodCoord(4,3) = -1.0d0/2.0d0*obj%seed_height
    
        ! 子葉-初生葉節
        !obj%struct%ElemNod(1,1) = 1
        !obj%struct%ElemNod(1,2) = 2
        ! 地際-子葉節
        !obj%struct%ElemNod(2,1) = 3
        !obj%struct%ElemNod(2,2) = 1
        ! 地際-根冠節
        !obj%struct%ElemNod(3,1) = 3
        !obj%struct%ElemNod(3,2) = 4
    
        ! 子葉-初生葉節 stem: 1
        !obj%struct%ElemMat(1) = 1
        ! 地際-子葉節 stem: 1
        !obj%struct%ElemMat(2) = 1
        ! 地際-根冠節 primary root: -1
        !obj%struct%ElemMat(3) = -1
    
        ! FEメッシュを生成
        ! 領域を確保
    !    n = input(default=80,option=max_leaf_num)
    !    allocate(obj%leaf_list(n) )
    !    n = input(default=80,option=max_stem_num)
    !    allocate(obj%stem_list(n) )
    !    n = input(default=80,option=max_root_num)
    !    allocate(obj%root_list(n) )
    !
    !    ! 子葉のメッシュを生成
    !    call obj%leaf_list(1)%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
    !        x_len=obj%seed_length,y_len=obj%seed_width,z_len=obj%seed_height)
    !    call obj%leaf_list(1)%move(x=0.0d0,y=-0.50d0*obj%seed_width,z=-0.50d0*obj%seed_height)
    !
    !    call obj%leaf_list(2)%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
    !        x_len=obj%seed_length,y_len=obj%seed_width,z_len=obj%seed_height)
    !    call obj%leaf_list(2)%rotate(x=radian(180.0d0) )
    !    call obj%leaf_list(2)%move(x=0.0d0,y=-0.50d0*obj%seed_width,z=-0.50d0*obj%seed_height)
    !
    !
    !
    !    ! 子葉-初生葉節のメッシュを生成
    !    rot(:) = 0.0d0
    !    call obj%stem_list(1)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
    !        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
    !    ! 節基部の節点ID
    !    node_id = obj%struct%ElemNod(1,1)
    !    ! 節先端部の節点ID
    !    node_id2= obj%struct%ElemNod(1,2)
    !    ! 節基部の位置ベクトル
    !    loc(:) = obj%struct%NodCoord( node_id  ,:)
    !    ! 節先端部までの方向ベクトル
    !    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
    !    
    !    ! structの構造データにメッシュデータを合わせる。
    !    print *, obj%stem_list(1)%Mesh%BottomElemID
    !    print *, obj%stem_list(1)%Mesh%TopElemID
    !
    !    elemid = obj%stem_list(1)%Mesh%BottomElemID
    !    node_id = obj%stem_list(1)%Mesh%ElemNod(elemID,1)
    !    meshloc(:) = obj%stem_list(1)%Mesh%NodCoord(node_id,:)
    !
    !    elemid = obj%stem_list(1)%Mesh%TopElemID
    !    node_id = obj%stem_list(1)%Mesh%ElemNod(elemID,1)
    !    meshvec(:) = obj%stem_list(1)%Mesh%NodCoord(node_id,:)-meshloc(:)
    
        !print *, "loc",loc
        !print *, "meshloc",meshloc
        !print *, "vec",vec
        !print *, "meshvec",meshvec
        
    !    ! 節中央を原点へ
    !    call obj%stem_list(1)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0)
    !    
    !    print *, "loc",loc
    !    print *, "vec",vec
    !    print *, "rot",rot
    !    zaxis(:)=0.0d0
    !    zaxis(3)=obj%seed_length/5.0d0
    !    rot(:) = angles(zaxis,vec)
    !    call obj%stem_list(1)%move(x=loc(1),y=loc(2),z=loc(3) )
    !    call obj%stem_list(1)%rotate(x=0.0d0,y=0.0d0,z=0.0d0 )
    !!    
    !    
    !!    
    !
    !
    !    ! 地際-子葉節のメッシュを生成
    !    rot(:) = 0.0d0
    !    call obj%stem_list(2)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
    !        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
    !    ! 節基部の節点ID
    !    node_id = obj%struct%ElemNod(2,1)
    !    ! 節先端部の節点ID
    !    node_id2= obj%struct%ElemNod(2,2)
    !    ! 節基部の位置ベクトル
    !    loc(:) = obj%struct%NodCoord( node_id  ,:)
    !    ! 節先端部までの方向ベクトル
    !    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
    !    ! 節中央を原点へ
    !    call obj%stem_list(2)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0,&
    !        z=-obj%seed_length/8.0d0)
    !    zaxis(:)=0.0d0
    !    zaxis(3)=obj%seed_length/5.0d0
    !    rot(:) = angles(zaxis,vec)
    !    print *, "loc",loc
    !    print *, "vec",vec
    !    print *, "rot",rot
    !    !call obj%stem_list(2)%rotate(x=rot(1),y=rot(2),z=rot(3) )
    !    call obj%stem_list(2)%move(x=loc(1),y=loc(2),z=loc(3) )
    !    
    !
    !
    !    ! 地際-根冠節のメッシュ生成
    !    rot(:) = 0.0d0
    !    call obj%root_list(1)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
    !        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
    !    ! 節基部の節点ID
    !    node_id = obj%struct%ElemNod(3,1)
    !    ! 節先端部の節点ID
    !    node_id2= obj%struct%ElemNod(3,2)
    !    ! 節基部の位置ベクトル
    !    loc(:) = obj%struct%NodCoord( node_id  ,:)
    !    ! 節先端部までの方向ベクトル
    !    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
    !    ! 節基部へ移動
    !    call obj%root_list(1)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0,&
    !        z=-obj%seed_length/8.0d0)
    !    call obj%root_list(1)%move(x=loc(1),y=loc(2),z=loc(3) )
    !    zaxis(:)=0.0d0
    !    zaxis(3)=obj%seed_length/5.0d0
    !    rot(:) = angles(zaxis,vec)
    !    !call obj%root_list(1)%rotate(x=rot(1),y=rot(2),z=rot(3) )
    !    print *, "loc",loc
    !    print *, "vec",vec
    !    print *, "rot",rot    
        call obj%update()
        call obj%fixReversedElements()
        
    endif




    ! ここからレガシーモード
    if(present(regacy) )then
        if(regacy .eqv. .true.)then
            obj%Stage = "VE"
            if(present(FileName) )then
                fn=FileName
            else
                fn="untitled"
            endif

            loc(:)=0.0d0

            if(present(x) )then
                loc(1)=x
            endif

            if(present(y) )then
                loc(2)=y
            endif

            if(present(z) )then
                loc(3)=z
            endif

            if(present(location) )then
                loc(:)=location(:)    
            endif

            ! initialize RootSystem and NodeSystem
            if(.not.allocated( obj%RootSystem) )then
                allocate(obj%RootSystem( input(default=1000,option=max_PlantNode_num) ) ) 
                obj%num_of_root=1
            endif
            if(.not.allocated( obj%NodeSystem) )then
                allocate(obj%NodeSystem( input(default=1000,option=max_PlantNode_num) ) ) 
                obj%num_of_node=1
            endif

            ! setup seed
            if(Variety=="Tachinagaha" .or. Variety=="tachinagaha" )then
                call obj%Seed%init(mass=mass,width1=9.70d0,width2=8.20d0,&
                    width3=7.70d0,&
                    water_content=water_content,radius=radius,location=loc)    
                call obj%Seed%createMesh(FileName=fn//".stl",&
                ElemType="Tetrahedra")

                call obj%Seed%convertMeshType(Option="TetraToHexa")

            else
                print *, "Variety name :: is not implemented."
                stop
            endif


            ! setup primary node (plumule)
            call obj%NodeSystem(1)%init(Stage=obj%Stage,&
            Plantname="soybean",location=loc)

            ! setup primary node (radicle))
            MaxThickness=input(default=0.20d0,&
            option=PlantRoot_diameter_per_seed_radius)*obj%Seed%radius
            Maxwidth    =input(default=0.20d0,&
            option=PlantRoot_diameter_per_seed_radius)*obj%Seed%radius
            call obj%RootSystem(1)%init(Plantname="soybean",&
            Stage=obj%Stage,MaxThickness=MaxThickness,Maxwidth=Maxwidth,location=loc)

            obj%time=0.0d0
            call obj%update()
            call obj%fixReversedElements()
        
            return
        endif
    endif


end subroutine
! ########################################

! ########################################
subroutine growSoybean(obj,dt,light,air,temp,simple,add_apical)
    class(Soybean_),intent(inout) :: obj
    type(Light_),optional,intent(inout) :: light
    type(air_),optional,intent(in) :: air
    real(real64),optional,intent(in) :: temp
    real(real64),intent(in) :: dt! time-interval
    real(real64) :: ac_temp ! time-interval
    logical,optional,intent(in) :: add_apical
    integer(int32) :: i, j
    logical,optional,intent(in) :: simple
    integer(int32),allocatable :: apicals(:)
    integer(int32),allocatable :: last_apicals(:)
    integer(int32),allocatable :: last_last_apicals(:)
    integer(int32),allocatable :: has_branch(:)
    integer(int32) :: StemID, InterNodeID,PetioleID, LeafID,N_StemID
    logical :: add_node = .false.
    real(real64) :: count_dist

    obj%dt = dt
    call obj%update()

    if(present(simple) )then
        if(simple)then
            ! simple algorithmic growth
            ! growth by temp by time

            do i=1,size(obj%stem)
                call obj%stem(i)%grow(dt=dt)
            enddo
            call obj%update()
            do i=1,size(obj%leaf)
                call obj%leaf(i)%grow(dt=dt)
            enddo
            call obj%update()

            if(present(add_apical) )then
                if(add_apical)then
                    apicals = obj%findApical()
                    do i=1,size(apicals)
                        ! add stem&leaf
                        if(i==1)then
                            ! main stem
                            StemID = apicals(i)
                            add_node = .false.
                            
                            j=size(obj%NodeID_MainStem)
                            if(j >= 1 )then
                                N_StemID = obj%NodeID_MainStem(j)

                                if(N_StemID >= 1)then

                                
                                    if( obj%stem(N_StemID)%FullyExpanded(threshold=obj%FullyExpanded_stem_threshold ))then

                                        add_node = .true.

                                    endif
                                endif
                            endif
                        else
                            ! branch to
                            StemID = apicals(i)
                            add_node = .false.
                            
                            N_StemID = maxval(obj%NodeID_Branch(i-1)%ID)
                            ! 1個前の節ID
                            
                            if( obj%stem(N_StemID)%FullyExpanded(threshold=obj%FullyExpanded_stem_threshold ))then
                                add_node = .true.
                            endif
                            
                        endif


                        if(add_node)then
                            call obj%addNode(StemNodeID=apicals(i),mainstem_to_branch=.false.)
                        endif
                        call obj%update()
                    enddo

                    ! branch
                    has_branch = zeros(size(obj%NodeID_MainStem) )
                    if(allocated(obj%MainStem_num_branch) )then
                        has_branch(1:size(obj%MainStem_num_branch) ) = obj%MainStem_num_branch(:)
                    endif
                    obj%MainStem_num_branch = has_branch
                        

                    ! we introduced an apploximation of the apical dominance.
                    do i=1, size(obj%NodeID_MainStem)-1
                        if(obj%MainStem_num_branch(i)>=1 )then
                            cycle
                        else
                            count_dist = 0.0d0
                            do j=i+1,size(obj%NodeID_MainStem)
                                count_dist = count_dist + obj%stem( obj%NodeID_MainStem(j) )%getLength()
                            enddo
                            if(count_dist > obj%apical_dominance_distance)then
                                ! add Node
                                if( obj%stem(i)%StemID /= 0 ) cycle
                                !debug
                                call obj%addNode(StemNodeID=i,mainstem_to_branch=.true.)
                                !has_branch(i) = has_branch(i) + 1
                                obj%MainStem_num_branch(i) = obj%MainStem_num_branch(i) + 1
                                call obj%update()
                                
                            endif

                        endif
                    enddo
                    
                    


                endif
            endif


!            if(present(add_apical) )then
!                if(add_apical)then
!                    apicals = obj%findApical()
!                    do i=1,size(apicals)-1
!                        ! add stem&leaf
!                        StemID = apicals(i+1)
!                        add_node = .false.
!
!                        do j=size(obj%NodeID_Branch(i)%ID)-4,size(obj%NodeID_Branch(i)%ID)-1
!                            
!                            if(j < 0 )then
!                                cycle
!                            else
!                                N_StemID = obj%NodeID_Branch(i)%ID(j)
!                                LeafID = obj%searchLeaf(StemID=i,InterNodeID=j,PetioleID=1,LeafID=1)
!                        
!                                if(LeafID < 1)then
!                                    cycle
!                                endif
!
!                                if( obj%leaf(LeafID)%FullyExpanded(threshold=0.90d0 ))then
!
!                                    add_node = .true.
!                                    exit
!                                endif
!
!                            endif
!                        enddo
!                        if(add_node)then
!                            call obj%addNode(StemNodeID=apicals(i))
!                        endif
!                    enddo
!                endif
!            endif

            call obj%update()
            return
        endif
    endif

    ! 光量子量を計算
    call obj%laytracing(light=light)

    ! 光合成量を計算
    do i=1,size(obj%Leaf)
        if(obj%Leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%photosynthesis(dt=dt,air=air)
        endif
    enddo

    ! シンクソース輸送を計算
    !call obj%SinkSourceFlow()

    ! ソースの消耗、拡散を計算
    !call obj%source2sink()

    ! 伸長を計算
    !call obj%extention()

    ! 分化を計算、構造の更新
    !call obj%development()

    !限界日長以下>> 花成 & 子実成長
    !if( obj%DayLengthIsShort() .eqv. .true. )then
        !call soybean%updateFlowers()
        !call soybean%updatePods()
    !endif


end subroutine
! ########################################

! ########################################
subroutine SinkSourceFlowSoybean(obj,simple)
    class(Soybean_),intent(inout) :: obj
    logical,optional,intent(in) :: simple
    !type(DiffusionEq_) :: DiffusionEq

    if(present(simple) )then
        if(simple)then
            ! simple flow
            ! for each stem,
            
            return
        endif
    endif
    !call obj%lossEnergy()

    ! solve diffusion equation for multi-domains
    !call DiffusionEq%init(multiDomain=.true.,connectivity=obj%domainConnectivity)
    !call DiffusionEq%add(domainlist=obj%stem(:)%femdomain)
    !call DiffusionEq%add(domainlist=obj%leaf(:)%femdomain)
    !call DiffusionEq%add(domainlist=obj%root(:)%femdomain)
    !call DiffusionEq%FixValue(&
        !range=soil%femdonain, projection=true, values=soil%watercontent)
    
    !call DiffusionEq%run(dt=obj%dt)
    !soybean%sourceContent = Diffusion%unknowns

    
end subroutine
! ########################################


! ########################################
subroutine expanitionSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    !type(ContactMechanics_) :: contact

    !contact%init(connectivity=obj%connectivity)
    !contact%add(domain=obj%stem(:)%domain)
    !contact%add(domain=obj%leaf(:)%domain)
    !contact%add(domain=obj%root(:)%domain)
    !contact%add(domain=soil)
    !contact%Density = obj%density()
    !contact%PoissonRatio = obj%PoissonRatio()
    !contact%PenaltyParameter = obj%PenaltyParameter()
    !contact%fix(bottom=.true., direction="xyz",displacement=[0.0d0,0.0d0,0.0d0] )
    !contact%fix(side=.true., direction="xyz",displacement=[0.0d0,0.0d0,0.0d0] )
    !contact%solve(dt=obj%dt)
    !soybean%CauchyStress = contact%CauchyStress
    !soybean%displacement = contact%displacement

end subroutine
! ########################################


! ########################################
subroutine developmentSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    integer(int32) :: i, new_stem_id,new_leaf_id,new_root_id
    
    !do i=1, obj%numStemApical
    !   stemID=obj%StemApical(i) 
    !   if(obj%stem(stemID)%source => obj%minimalSource )then
    !       new_stem_id = obj%newStem(from=StemID,how=obj%stem(stemID)%properties)
    !       new_leaf_id = obj%newLeaf(from=new_stem_id,how=obj%stem(stemID)%properties)
    !   endif
    !enddo

    !do i=1, obj%numleaf
    !   leafID=i
    !   call obj%leaf(leafID)%grow()
    !enddo

    !do i=1, obj%numrootApical
    !   rootID=obj%rootApical(i) 
    !   if(obj%root(rootID)%source => obj%minimalSource )then
    !       new_root_id = obj%newroot(from=rootID,how=obj%root(rootID)%properties)
    !   endif
    !enddo

end subroutine
! ########################################



! ########################################
subroutine updateFlowersSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    integer(int32) :: i
    
    !do i=1, obj%numStem() 
    !   call obj%stem(i)%updateFlowerCapacity()
    !enddo

end subroutine
! ########################################


! ########################################
subroutine updatePodsSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    integer(int32) :: i
    
    !do i=1, obj%numStem() 
    !   call obj%stem(i)%updatePodCapacity()
    !   call obj%stem(i)%growPod()
    !enddo

end subroutine
! ########################################





! ########################################
subroutine WaterAbsorptionSoybean(obj,temp,dt)
    class(Soybean_),intent(inout) :: obj
    real(real64),intent(in) :: temp,dt
    real(real64) :: a,b,c,d,AA,BB,w1max,w2max,w3max,time
    real(real64) :: x_rate,y_rate,z_rate,wx,wy,wz

    obj%time=obj%time+dt


    ! tested by tachinagaha, 2019
    a=0.00910d0
    b=-1.76450d0
    c=3.32E-04	
    d=-0.0905180d0
    AA=a*temp+b
    !BB=c*exp(d*temp)
    BB=c*temp+d
    ! width1 becomes 1.7 times, width2 becomes 1.2, width3 becomes 1.1
    w1max=1.70d0
    w2max=1.20d0
    w3max=1.10d0
    obj%seed%width1=obj%seed%width1_origin*(w1max - AA*exp(-BB*obj%time)   ) 
    obj%seed%width2=obj%seed%width2_origin*(w2max - AA*exp(-BB*obj%time)   ) 
    obj%seed%width3=obj%seed%width3_origin*(w3max - AA*exp(-BB*obj%time)   ) 

    ! linear model; it should be changed in near future.
    if(obj%time > 60.0d0*6.0d0)then
        obj%seed%width2=obj%seed%width2_origin*(w2max ) 
        obj%seed%width3=obj%seed%width3_origin*(w3max ) 
    else
        obj%seed%width2=obj%seed%width2_origin + obj%seed%width2_origin*(w2max-1.0d0 )*(obj%time)/(60.0d0*6.0d0) 
        obj%seed%width3=obj%seed%width3_origin + obj%seed%width3_origin*(w3max-1.0d0 )*(obj%time)/(60.0d0*6.0d0)
    endif

    wx = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,1))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,1)) 
    wy = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,2))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,2)) 
    wz = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,3))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,3)) 
    !print *, wx,wy,wz
    x_rate =  1.0d0/wx
    y_rate =  1.0d0/wy
    z_rate =  1.0d0/wz
    call obj%Seed%FEMDomain%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate)
    x_rate = obj%seed%width1
    y_rate = obj%seed%width2
    z_rate = obj%seed%width3
    call obj%Seed%FEMDomain%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate)


end subroutine
! ########################################


! ########################################
subroutine exportSoybean(obj,FilePath,FileName,SeedID,withSTL,withMesh)
    class(Soybean_),intent(inout) :: obj
    character(*),optional,intent(in) :: FilePath
    character(*),intent(in) :: FileName
    integer(int32),optional,intent(inout) :: SeedID
    logical,optional,intent(in) :: withSTL,withMesh
    integer(int32) :: i,itr

    itr=SeedID
    ! if seed exists => output
    if(obj%Seed%num_of_seed>=0)then
        if(present(withSTL) )then
            if(withSTL .eqv. .true.)then
                call obj%Seed%export(FileName=FileName,SeedID=itr,extention=".stl")    
            endif
        endif
        if(present(withMesh) )then
            if(withMesh .eqv. .true.)then
                call obj%Seed%export(FileName=FileName,SeedID=itr,extention=".pos")    
            endif
        endif

            
        if(present(FilePath) )then
            call obj%Seed%export(FileName=FilePath//"/seed.geo",SeedID=itr)
        else
            call obj%Seed%export(FileName=FileName,SeedID=itr)
        endif
    endif

    itr=itr+1
    ! export NodeSystem
    do i=1,size(obj%NodeSystem)
            
        if(present(FilePath) )then
            call obj%NodeSystem(i)%export(FileName=FilePath//"/Node.geo",objID=itr)
        else
            call obj%NodeSystem(i)%export(FileName=FileName//"_Node.geo",objID=itr)
        endif
        if(i==obj%num_of_node  )then
            exit
        endif
    enddo

    
    ! export RootSystem
    do i=1,size(obj%RootSystem)
            
        if(present(FilePath) )then
            call obj%RootSystem(i)%export(FileName=FilePath//"/Root.geo",RootID=itr)
        else
            call obj%RootSystem(i)%export(FileName=FileName//"_Root.geo",RootID=itr)
        endif
        if(i==obj%num_of_root  )then
            exit
        endif
    enddo
    SeedID=itr




end subroutine
! ########################################



! ########################################

! ########################################
!subroutine initsoybean(obj,growth_habit,Max_Num_of_Node)
!    class(soybean_) :: obj
!    character(*),optional,intent(in) :: growth_habit
!    integer(int32),optional,intent(in)::Max_Num_of_Node
!    integer(int32) ::n
!
!    if(present(growth_habit) )then
!        obj%growth_habit=growth_habit
!    else
!        obj%growth_habit="determinate"
!    endif
!
!    obj%growth_stage="VE"
!
!    n=input(default=100,option=Max_Num_of_Node)
!
!    allocate(obj%NodeSystem(n))
!    obj%NumOfNode=0
!    obj%NumOfRoot=0
!
!    ! set an initial node and root
!    ! two leaves, one root.
!
!    call obj%AddNode()
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
!subroutine AddNodeSoybean(obj,SizeRatio)
!    class(soybean_),intent(inout)::obj
!    real(real64),optional,intent(in)::SizeRatio
!    real(real64) :: magnif
!
!    magnif=input(default=1.0d0,option=SizeRatio)
!    obj%NumOfNode=obj%NumOfNode+1
!    
!    ! add leaves
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%leaf(2) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    else        
!        allocate(obj%NodeSystem(obj%NumOfNode)%leaf(3) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!    ! add stem
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%Stem(1) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!    ! add Peti
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%Peti(1) )
!        call obj%NodeSystem(obj%NumOfNode)%Peti(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!end subroutine
!! ########################################
!

! ########################################
subroutine showSoybean(obj,name)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in)::name

    if( obj%struct%empty() .eqv. .true.)then
        print *, "Error :: showSoybean>> no structure is imported."
        return
    endif

    call obj%struct%export(name=name)

end subroutine
! ########################################



! ########################################
function numleafsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    if(.not.allocated(obj%leaf) )then
        return
    endif
    
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################

! ########################################
function numstemsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    if(.not.allocated(obj%stem) )then
        return
    endif

    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################

! ########################################
function numrootsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    if(.not.allocated(obj%root) )then
        return
    endif
    

    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################


! ########################################
subroutine gmshSoybean(obj,name,num_threads,single_file)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    type(FEMDomain_) :: femdomain
    integer(int32),optional,intent(in) :: num_threads
    logical,optional,intent(in) :: single_file
    integer(int32) :: i,n


    if(present(single_file) )then
        if(single_file)then
            ! export mesh for a single file
            if(allocated(obj%stem) )then
                do i=1,size(obj%stem)
                    if(.not.obj%stem(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%stem(i)%femdomain
                    endif
                enddo
            endif
            
            if(allocated(obj%leaf) )then
                do i=1,size(obj%leaf)
                    if(.not.obj%leaf(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%leaf(i)%femdomain
                    endif
                enddo
            endif

            if(allocated(obj%root) )then
                do i=1,size(obj%root)
                    if(.not.obj%root(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%root(i)%femdomain
                    endif
                enddo
            endif
            call femdomain%gmsh(name=name)
            return
        endif
    endif


    n = input(default=1,option=num_threads)
    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 

    do i=1,size(obj%stem)
        !if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%gmsh(name=name//"_stem"//str(i))
        !endif
    enddo
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 

    do i=1,size(obj%root)
        !if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%gmsh(name=name//"_root"//str(i))
        !endif
    enddo
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 
    do i=1,size(obj%leaf)
        !if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%gmsh(name=name//"_leaf"//str(i))
        !endif
    enddo
    !!$OMP end do
    !!$OMP end parallel


end subroutine
! ########################################


! ########################################
subroutine mshSoybean(obj,name,num_threads)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n
    type(IO_) :: f
    ! index file
    call f%open(name//"_index.txt","w")

    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                call f%write(name//"_stem"//str(i)//".msh")
            endif
        enddo
    endif
    
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                call f%write(name//"_root"//str(i)//".msh")
            endif
        enddo
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                call f%write(name//"_leaf"//str(i)//".msh")
            endif
        enddo
    endif
    call f%close()
    
    n = input(default=1,option=num_threads)
    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 
    do i=1,size(obj%stem)
        !if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%msh(name=name//"_stem"//str(i))
        !endif
    enddo
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 
    do i=1,size(obj%root)
        !if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%msh(name=name//"_root"//str(i))
        !endif
    enddo
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 
    do i=1,size(obj%leaf)
        !if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%msh(name=name//"_leaf"//str(i))
        !endif
    enddo
    !!$OMP end do
    !!$OMP end parallel

end subroutine
! ########################################


! ########################################
subroutine vtkSoybean(obj,name,num_threads,single_file,&
    scalar_field,vector_field,tensor_field,field_name)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    character(*),optional,intent(in) :: field_name
    
    type(IO_) :: f
    type(FEMDomain_) :: femdomain
    integer(int32),optional,intent(in) :: num_threads
    real(real64),optional,intent(in) :: scalar_field(:)
    real(real64),optional,intent(in) :: vector_field(:,:)
    real(real64),optional,intent(in) :: tensor_field(:,:,:)
    integer(int32) :: i, n
    logical,optional,intent(in) :: single_file

    if(.not.allocated(obj%stem) )then
        if(.not.allocated(obj%leaf) )then
            if(.not.allocated(obj%root) )then
                return
            endif
        endif
    endif

    if(present(single_file) )then
        if(single_file)then
            ! export mesh for a single file
            if(allocated(obj%stem) )then
                do i=1,size(obj%stem)
                    if(.not.obj%stem(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%stem(i)%femdomain
                    endif
                enddo
            endif

            if(allocated(obj%leaf) )then
                do i=1,size(obj%leaf)
                    if(.not.obj%leaf(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%leaf(i)%femdomain
                    endif
                enddo
            endif

            if(allocated(obj%root) )then
                do i=1,size(obj%root)
                    if(.not.obj%root(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%root(i)%femdomain
                    endif
                enddo
            endif

            if(present(scalar_field) )then
                ! export scalar-valued field 
                ! as a single file
                call femdomain%vtk(field=field_name,name=name,scalar=scalar_field)
            elseif(present(vector_field) )then
                ! export vector-valued field 
                ! as a single file
                call femdomain%vtk(field=field_name,name=name,vector=vector_field)
            elseif(present(tensor_field) )then
                ! export tensor-valued field 
                ! as a single file
                call femdomain%vtk(field=field_name,name=name,tensor=tensor_field)
            else
                call femdomain%vtk(field=field_name,name=name)
            endif
            return
        endif
    endif

    n = input(default=1,option=num_threads)
    
    ! index file
    call f%open(name//"_index.txt","w")

    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                call f%write(name//"_stem"//str(i)//".vtk")
            endif
        enddo
    endif
    
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                call f%write(name//"_root"//str(i)//".vtk")
            endif
        enddo
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                call f%write(name//"_leaf"//str(i)//".vtk")
            endif
        enddo
    endif
    call f%close()

    if(allocated(obj%stem) )then
        !!$OMP parallel num_threads(n) private(i)
        !!$OMP do 
        do i=1,size(obj%stem)
            !if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%stem(i)%vtk(field_name=field_name,name=name//"_stem"//str(i))
            !endif
        enddo
        !!$OMP end do
        !!$OMP end parallel
    endif


    if(allocated(obj%root))then

        !!$OMP parallel num_threads(n) private(i)
        !!$OMP do 
        do i=1,size(obj%root)
            !if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%root(i)%vtk(field_name=field_name,name=name//"_root"//str(i))
            !endif
        enddo

        !!$OMP end do
        !!$OMP end parallel
    endif


    if(allocated(obj%leaf))then

        !!$OMP parallel num_threads(n) private(i)
        !!$OMP do 
        do i=1,size(obj%leaf)
            !if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%leaf(i)%vtk(field_name=field_name,name=name//"_leaf"//str(i))
            !endif
        enddo
        !!$OMP end do
        !!$OMP end parallel
    endif

    

end subroutine
! ########################################


! ########################################
subroutine jsonSoybean(obj,name)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32) :: i,countnum
    type(IO_) :: f

    call f%open(name//".json")
    call f%write("{")
    countnum=0
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"stem"//str(i)//'":')
            call obj%stem(i)%femdomain%json(name=name//"_stem"//str(i),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_stem":'//str(countnum)//',' )

    countnum=0
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"root"//str(i)//'":')
            call obj%root(i)%femdomain%json(name=name//"_root"//str(i),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_root":'//str(countnum)//',' )
    
    countnum=0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"leaf"//str(i)//'":')
            call obj%leaf(i)%femdomain%json(name=name//"_leaf"//str(i),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"obj%num_leaf":'//str(countnum)//',' )
    call f%write('"return_soybean":0')
    call f%write("}")
    call f%close()
end subroutine
! ########################################

! ########################################
subroutine stlSoybean(obj,name,num_threads,single_file)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    type(FEMDomain_) :: femdomain
    logical,optional,intent(in) :: single_file
    integer(int32) :: i,n

    type(IO_) :: f


    if(present(single_file) )then
        if(single_file)then
            ! export mesh for a single file
            if(allocated(obj%stem) )then
                do i=1,size(obj%stem)
                    if(.not.obj%stem(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%stem(i)%femdomain
                    endif
                enddo
            endif
            
            if(allocated(obj%leaf) )then
                do i=1,size(obj%leaf)
                    if(.not.obj%leaf(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%leaf(i)%femdomain
                    endif
                enddo
            endif

            if(allocated(obj%root) )then
                do i=1,size(obj%root)
                    if(.not.obj%root(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%root(i)%femdomain
                    endif
                enddo
            endif
            call femdomain%stl(name=name)
            return
        endif
    endif


    ! index file
    call f%open(name//"_index.txt","w")

    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                call f%write(name//"_stem"//str(i)//".stl")
            endif
        enddo
    endif
    
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                call f%write(name//"_root"//str(i)//".stl")
            endif
        enddo
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                call f%write(name//"_leaf"//str(i)//".stl")
            endif
        enddo
    endif
    call f%close()
    
    n = input(default=1,option=num_threads)
    !call execute_command_line("echo ' ' > "//name//".stl")
    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%stl(name=name//"_stem"//str(i))
            !call execute_command_line("cat "//name//"_stem"//str(i)//"_000001.stl >> "//name//".stl")
        endif
    enddo
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%stl(name=name//"_root"//str(i))
            !call execute_command_line("cat "//name//"_root"//str(i)//"_000001.stl >> "//name//".stl")
        endif
    enddo
    !!$OMP end do
    !!$OMP end parallel

    !!$OMP parallel num_threads(n) private(i)
    !!$OMP do 
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%stl(name=name//"_leaf"//str(i))
            !call execute_command_line("cat "//name//"_leaf"//str(i)//"_000001.stl >> "//name//".stl")
        endif
    enddo
    !!$OMP end do
    !!$OMP end parallel

    call execute_command_line("cat "//name//"*_leaf*.stl > "//name//"_leaf.stl" )
    call execute_command_line("cat "//name//"*_stem*.stl > "//name//"_stem.stl" )
    call execute_command_line("cat "//name//"*_root*.stl > "//name//"_root.stl" )
    call execute_command_line("cat "//name//"_leaf.stl "//name//"_stem.stl "&
        //name//"_root.stl > "//name//".stl" )

end subroutine
! ########################################

! ########################################
subroutine moveSoybean(obj,x,y,z)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    integer(int32) :: i

    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%stem(i)%move(x=x,y=y,z=z)
            endif
        enddo
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%leaf(i)%move(x=x,y=y,z=z)
            endif
        enddo
    endif

    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%root(i)%move(x=x,y=y,z=z)
            endif
        enddo
    endif

end subroutine
! ########################################

! ########################################
subroutine laytracingsoybean(obj,light,Transparency,Resolution)
    class(Soybean_),intent(inout) :: obj
    type(Light_),intent(in) :: light
    real(real64),optional,intent(in) :: Transparency,Resolution
    real(real64),allocatable :: ppfd(:)
    integer(int32),allocatable ::  NumberOfElement(:)
    integer(int32) :: from, elem_id

    ! >>> regacy
    real(real64),allocatable :: stemcenter(:,:),stemradius(:)
    real(real64),allocatable :: leafcenter(:,:),leafradius(:)
    real(real64),allocatable :: elemnodcoord(:,:),x(:),x2(:)
    real(real64) :: max_PPFD,r,rc,r0
    real(real64),parameter :: extinction_ratio = 100.0d0 ! ratio/m
    type(IO_) :: f
    integer(int32) :: i,j,n,num_particle,k,l,nodeid,m,totcount
    integer(int32) :: num_particle_leaf,tocount_leaf
    ! <<< regacy

    ppfd = obj%getPPFD(Light=Light,Transparency=Transparency,Resolution=Resolution)

    NumberOfElement = obj%getNumberOfElement()
    from = sum(NumberOfElement(1:obj%numstem() ))

    elem_id = from
    do i=1, size(obj%leaf)
        if(.not. obj%leaf(i)%femdomain%empty() )then
            obj%leaf(i)%ppfd = zeros( obj%leaf(i)%femdomain%ne() )
            do j=1,obj%leaf(i)%femdomain%ne()
                elem_id = elem_id + 1
                obj%leaf(i)%ppfd(j) = ppfd(elem_id)
            enddo
        endif
    enddo
    return

    ! >>> Regacy

    ! 総当りで、総遮蔽長を割り出す
    ! 茎は光を通さない、葉は透過率あり、空間は透過率ゼロ
    ! 要素中心から頂点への平均長さを半径に持ち、要素中心を中心とする球
    ! を考え、Layとの公差判定を行う。
    num_particle = 0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            num_particle=num_particle+size(obj%leaf(i)%femdomain%mesh%ElemNod,1)
        endif
    enddo
    allocate(leafcenter(num_particle,3),leafradius(num_particle) )
    leafcenter(:,:) = 0.0d0
    leafradius(:) = 0.0d0

    num_particle = 0
    do i=1,size(obj%leaf)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            num_particle=num_particle+size(obj%stem(i)%femdomain%mesh%ElemNod,1)
        endif
    enddo
    allocate(stemcenter(num_particle,3),stemradius(num_particle) )
    stemcenter(:,:) = 0.0d0
    stemradius(:) = 0.0d0

    num_particle = 0
    
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            n = size(obj%leaf(i)%femdomain%mesh%Elemnod,2)
            m = size(obj%leaf(i)%femdomain%mesh%Nodcoord,2)
            allocate(elemnodcoord(n,m) )
            allocate(x(m) )
            do j=1,size(obj%leaf(i)%femdomain%mesh%elemnod,1)
                do k=1,size(obj%leaf(i)%femdomain%mesh%elemnod,2)
                    nodeid = obj%leaf(i)%femdomain%mesh%elemnod(j,k)
                    elemnodcoord(k,:) = obj%leaf(i)%femdomain%mesh%Nodcoord(nodeid,:)
                enddo
                num_particle = num_particle+1
                do k=1, size(elemnodcoord,1)
                    do l=1, size(elemnodcoord,2)
                        leafcenter(num_particle,l) = &
                        + leafcenter(num_particle,l) &
                        + 1.0d0/dble(size(elemnodcoord,1))*elemnodcoord(k,l)
                    enddo
                enddo
                do k=1, size(elemnodcoord,1)
                    x(:) = elemnodcoord(k,:)
                    x(:) = x(:) - leafcenter(num_particle,:)
                    if(k>=2 .and. leafradius(num_particle) > sqrt(dot_product(x,x))  )then
                        leafradius(num_particle) = sqrt(dot_product(x,x))
                    elseif(k==1)then
                        leafradius(num_particle) = sqrt(dot_product(x,x))    
                    else
                        cycle
                    endif
                enddo
            enddo
            deallocate(elemnodcoord)
            deallocate(x)
        endif
    enddo


    num_particle = 0
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            n = size(obj%stem(i)%femdomain%mesh%Elemnod,2)
            m = size(obj%stem(i)%femdomain%mesh%Nodcoord,2)
            allocate(elemnodcoord(n,m) )
            allocate(x(m) )
            do j=1,size(obj%stem(i)%femdomain%mesh%elemnod,1)
                do k=1,size(obj%stem(i)%femdomain%mesh%elemnod,2)
                    nodeid = obj%stem(i)%femdomain%mesh%elemnod(j,k)
                    elemnodcoord(k,:) = obj%stem(i)%femdomain%mesh%Nodcoord(nodeid,:)
                enddo
                num_particle = num_particle+1
                do k=1, size(elemnodcoord,1)
                    do l=1, size(elemnodcoord,2)
                        stemcenter(num_particle,l) = &
                        + stemcenter(num_particle,l) &
                        + 1.0d0/dble(size(elemnodcoord,1))*elemnodcoord(k,l)
                    enddo
                enddo
                do k=1, size(elemnodcoord,1)
                    x(:) = elemnodcoord(k,:)
                    x(:) = x(:) - stemcenter(num_particle,:)
                    !最小半径で考える
                    if(k>=2 .and. stemradius(num_particle) > sqrt(dot_product(x,x))  )then
                        stemradius(num_particle) = sqrt(dot_product(x,x))
                    elseif(k==1)then
                        stemradius(num_particle) = sqrt(dot_product(x,x))    
                    else
                        cycle
                    endif
                enddo
            enddo
            deallocate(elemnodcoord)
            deallocate(x)
        endif
    enddo
    

    ! DEBUG
    call f%open("leaf.txt")
    do i=1,size(leafcenter,1)
        write(f%fh,*) leafcenter(i,:)
    enddo
    call f%close()
    
    call f%open("stem.txt")
    do i=1,size(stemcenter,1)
        write(f%fh,*) stemcenter(i,:)
    enddo
    call f%close()
    
    allocate(x(3),x2(3) )
    
    
    num_particle = 0
    totcount = 0
    tocount_leaf = 0
    num_particle_leaf = 0

    do i=1,size(obj%leaf)
        !print *, i,"/",obj%numleaf()
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            ! 葉あり
            obj%leaf(i)%PPFD(:) = max_PPFD

            !!$OMP parallel do private(j)
            do j=1,size(obj%leaf(i)%PPFD)

                totcount = tocount_leaf + j

                num_particle = num_particle_leaf + j
                ! それぞれの要素について、遮蔽particleを探索
                ! 茎：全減衰
                ! 葉：半減衰
                ! 簡単のため上からのみ
                ! x-yのみについて見て、上方かつx-y平面距離が半径以内で覆陰判定
                x(:) = leafcenter(num_particle,:)
                r0   = leafradius(num_particle)
                ! 枝による覆陰判定
                
                do k=1, size(stemcenter,1)
                    x2(:) = stemcenter(k,:)
                    r     = stemradius(k)
                    rc    = ( x(1)-x2(1) )**(2.0d0) + ( x(2)-x2(2) )**(2.0d0) 
                    rc    = sqrt(rc)
                    if(rc <= r0 + r .and. x(3) < x2(3) )then
                        ! 茎により覆陰されてる
                        obj%leaf(i)%PPFD(j) = 0.0d0
                        exit
                    endif
                enddo
                if(obj%leaf(i)%PPFD(j) == 0.0d0)then
                    cycle
                endif

                do k=1, size(leafcenter,1)
                    ! もし自信だったら除外
                    if(totcount == k)then
                        cycle
                    endif
                    
                    x2(:) = leafcenter(k,:)
                    r     = leafradius(k)
                    rc    = ( x(1)-x2(1) )**(2.0d0) + ( x(2)-x2(2) )**(2.0d0) 
                    rc    = sqrt(rc)
                    if(rc <= (r0 + r)/2.0d0 .and. x(3) < x2(3) )then
                        ! 茎により覆陰されてる
                        obj%leaf(i)%PPFD(j) = &
                        obj%leaf(i)%PPFD(j)*(1.0d0-extinction_ratio*2.0d0*r)
                        if( obj%leaf(i)%PPFD(j) <= 0.0d0 )then
                            obj%leaf(i)%PPFD(j) = 0.0d0
                        endif
                    endif
                enddo
                
            enddo
            !!$OMP end parallel do

            tocount_leaf = tocount_leaf + size(obj%leaf(i)%PPFD)
            num_particle_leaf = num_particle_leaf + size(obj%leaf(i)%PPFD)
        endif
    enddo
    
    call f%open("PPFD.txt")
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            ! 葉あり
            do j=1,size(obj%leaf(i)%PPFD,1)
                write(f%fh,*) obj%leaf(i)%PPFD(j),"leaf_id: ",str(i),"elem_id: ",str(j)
            enddo
        endif
    enddo
    call f%close()
    


end subroutine
! ########################################

subroutine addNodeSoybean(obj,StemNodeID,RootNodeID,peti_width_ave,peti_width_sig,peti_size_ave &
    ,peti_size_sig,peti_angle_ave,peti_angle_sig,leaf_thickness_ave,leaf_thickness_sig &
    ,leaf_length_ave,leaf_length_sig,leaf_width_ave,leaf_width_sig,leaf_angle_sig &
    ,leaf_angle_ave,mainstem_to_branch)
    class(Soybean_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: StemNodeID,RootNodeID
    real(real64),optional,intent(in) :: peti_width_ave,peti_width_sig,peti_size_ave &
    ,peti_size_sig,peti_angle_ave,peti_angle_sig,leaf_thickness_ave,leaf_thickness_sig &
    ,leaf_length_ave,leaf_length_sig,leaf_width_ave,leaf_width_sig,leaf_angle_sig &
    ,leaf_angle_ave
    logical,optional,intent(in) :: mainstem_to_branch
    logical :: mainstem_2_branch = .false.
    real(real64),allocatable :: leaf_z_angles(:)
    type(Random_) :: random
    type(soybean_NodeID_Branch_),allocatable :: old_NodeID_Branch(:)
    integer(int32) :: i,j,branch_id,My_StemID

    if(present(mainstem_to_branch) )then    
        mainstem_2_branch = mainstem_to_branch
    endif

    call obj%update()

    if(present(StemNodeID) )then
        i = StemNodeID
        
        obj%leaf_thickness_ave(obj%num_leaf)=input(&
            default=obj%leaf_thickness_ave(obj%num_leaf),&
            option=leaf_thickness_ave)
        obj%leaf_thickness_sig(obj%num_leaf)=input(&
            default=obj%leaf_thickness_sig(obj%num_leaf),&
            option=leaf_thickness_sig)
        
        obj%leaf_length_ave(obj%num_leaf)=input(&
            default=obj%leaf_length_ave(obj%num_leaf),&
            option=leaf_length_ave)
        obj%leaf_length_sig(obj%num_leaf)=input(&
            default=obj%leaf_length_sig(obj%num_leaf),&
            option=leaf_length_sig)
        obj%leaf_width_ave(obj%num_leaf)=input(&
            default=obj%leaf_width_ave(obj%num_leaf),&
            option=leaf_width_ave)
        obj%leaf_width_sig(obj%num_leaf)=input(&
            default=obj%leaf_width_sig(obj%num_leaf),&
            option=leaf_width_sig)
        obj%leaf_angle_sig(obj%num_leaf)=input(&
            default=obj%leaf_angle_sig(obj%num_leaf),&
            option=leaf_angle_sig)
        
        obj%leaf_angle_ave(obj%num_leaf)=input(&
            default=obj%leaf_angle_ave(obj%num_leaf),&
            option=leaf_angle_ave)

        
        ! main stem -> main stem
        if(obj%isMainStem(StemNodeID) .and. .not.mainstem_2_branch)then
            print *, "Main -> Main", StemNodeID
            ! main stem
            i = StemNodeID
            call obj%stem(obj%numStem()+1 )%init(config=obj%stemconfig)
            
            
            call extend(obj%NodeID_MainStem)
            obj%NodeID_MainStem( size(obj%NodeID_MainStem) ) = obj%numStem()
            
            if(obj%ms_node > 0.0d0)then
                call obj%stem(i)%resize(&
                    x = obj%ms_width, &
                    y = obj%ms_width, &
                    z = obj%ms_length/dble(obj%ms_node) &
                    )
            endif
            
            call obj%stem(i)%rotate(&
                x = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
                y = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
                z = obj%stem(StemNodeID)%femdomain%total_rotation(3) + radian((random%random()-0.50d0)*90.0d0)    &
                )           
            call obj%stem(i)%grow(dt = 0.0d0)    
            obj%stem(i)%StemID=0 
            obj%stem(i)%InterNodeID = size(obj%NodeID_MainStem)
            
        ! branch -> branch
        elseif(.not.obj%isMainStem(StemNodeID) .and. .not.mainstem_2_branch)then
            
            i = StemNodeID

            branch_id = obj%BranchID(i)
            print *, "Branch -> Branch branch id", branch_id

            call obj%stem(obj%numStem()+1 )%init(config=obj%stemconfig)
            

            if(.not. allocated(obj%NodeID_Branch(branch_id)%ID) )then
                obj%NodeID_Branch(branch_id)%ID = [obj%numStem()]
            else
                obj%NodeID_Branch(branch_id)%ID = obj%NodeID_Branch(branch_id)%ID // [obj%numStem()]
            endif

            My_StemID = obj%numStem()
            call obj%stem(My_StemID)%rotate(&
            x = radian(random%gauss(mu=obj%br_angle_ave(branch_id),sigma=obj%br_angle_sig(branch_id) )),  &
            y = radian(random%gauss(mu=obj%br_angle_ave(branch_id),sigma=obj%br_angle_sig(branch_id) )),  &
            z = obj%stem(StemNodeID)%femdomain%total_rotation(3) + radian((random%random()-0.50d0)*90.0d0)   &
            )
            
            call obj%stem(My_StemID)%grow(dt = 0.0d0)    
            obj%stem(My_StemID)%StemID=branch_id 
            obj%stem(My_StemID)%InterNodeID = size(obj%NodeID_Branch(branch_id)%ID)
        ! main stem -> branch
        elseif(obj%isMainStem(StemNodeID) .and. mainstem_2_branch)then
            ! branch
            i = StemNodeID ! 1 : stem ID of main stem
            My_StemID = i

            ! create new internode
            call obj%stem(obj%numStem()+1 )%init(config=obj%stemconfig)
            
            ! if mainstem -> branch
            
            
            if(allocated(obj%MainStem_num_branch) )then
                branch_id = 1
                do j=1,size(obj%NodeID_MainStem)
                    if(obj%NodeID_MainStem(j)==StemNodeID )then
                        exit
                    elseif(obj%MainStem_num_branch(j)/=0 )then
                        branch_id = branch_id + obj%MainStem_num_branch(j)
                        cycle
                    else
                        cycle
                    endif
                enddo
            else
                branch_id = 1
            endif

            print *, "Main -> Branch branch id", branch_id
                
            if(.not.allocated(obj%NodeID_Branch) )then
                allocate(obj%NodeID_Branch(obj%MaxStemNum) )
            endif

            if(.not. allocated(obj%NodeID_Branch(branch_id)%ID) )then
                obj%NodeID_Branch(branch_id)%ID = [obj%numStem()]
            else
                obj%NodeID_Branch(branch_id)%ID = obj%NodeID_Branch(branch_id)%ID // [obj%numStem()]
            endif
            
            My_StemID = obj%numStem()

            obj%stem(obj%numStem() )%StemID = branch_id

            call obj%stem(My_StemID)%rotate(&
            x = radian(random%gauss(mu=obj%br_angle_ave(branch_id),sigma=obj%br_angle_sig(branch_id) )),  &
            y = radian(random%gauss(mu=obj%br_angle_ave(branch_id),sigma=obj%br_angle_sig(branch_id) )),  &
            z = radian(random%random()*360.0d0)    &
            )
            
            call obj%stem(My_StemID)%grow(dt = 0.0d0)
            obj%stem(My_StemID)%StemID=branch_id
            obj%stem(My_StemID)%InterNodeID = 1
        else
            print *, obj%isMainStem(StemNodeID) 
            print *, mainstem_2_branch
            print *, "[ERROR] addNode"
            stop
        endif
        
        call obj%stem( obj%numStem() )%connect("=>",obj%stem(StemNodeID))
        obj%stem2stem( obj%numStem() , StemNodeID ) = 1
        ! petiole 


        call obj%stem(obj%numStem()+1 )%init(config=obj%stemconfig)

        obj%stem(obj%numStem() )%StemID = -1

        call obj%stem(obj%numStem() )%resize(&
            x = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
            y = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
            z = random%gauss(mu=obj%peti_size_ave(i),sigma=obj%peti_size_sig(i)) &
            )
        call obj%stem(obj%numStem() )%grow(dt = 0.0d0)
        
        call obj%stem(obj%numStem() )%rotate(&
            x = radian(random%gauss(mu=obj%peti_angle_ave(i),sigma=obj%peti_angle_sig(i) )),  &
            y = 0.0d0,  &
            z = radian(360.0d0*random%random() )   &
            ) 
        
        !call obj%stem(obj%numStem() )%connect("=>",obj%stem(i))
        !obj%stem2stem(obj%numStem() ,i) = 1     
        call obj%stem(obj%numStem() )%connect("=>",obj%stem(obj%numStem()-1 ))
        obj%stem2stem(obj%numStem() ,obj%numStem()-1 ) = 1             


            
        leaf_z_angles = linspace([0.0d0,360.0d0],obj%max_num_leaf_per_petiole+1 )
        do j = 1, obj%max_num_leaf_per_petiole
            leaf_z_angles(j) = radian(leaf_z_angles(j))
        enddo

        leaf_z_angles(:) = leaf_z_angles(:) + radian(random%random()*360.0d0)

        

        ! add leaves
        do j=1,obj%max_num_leaf_per_petiole
            obj%num_leaf=obj%num_leaf+1

            call obj%leaf(obj%num_leaf)%init(config=obj%leafconfig,species=PF_GLYCINE_SOJA)
            call obj%leaf(obj%num_leaf)%resize(&
                y = random%gauss(mu=obj%leaf_thickness_ave(i),sigma=obj%leaf_thickness_sig(i))  , &
                z = random%gauss(mu=obj%leaf_length_ave(i)   ,sigma=obj%leaf_length_sig(i)) , &
                x = random%gauss(mu=obj%leaf_width_ave(i)    ,sigma=obj%leaf_width_sig(i)) &
            )
            call obj%leaf(obj%num_leaf)%rotate(&
                x = radian(random%gauss(mu=obj%leaf_angle_ave(i),sigma=obj%leaf_angle_sig(i))), &
                y = 0.0d0, &
                z = leaf_z_angles(j)  &
            )
            call obj%leaf(obj%num_leaf)%connect("=>",obj%stem(obj%numStem() ))
            obj%leaf2stem(obj%num_leaf,obj%numStem() ) = 1
            
            call obj%leaf(obj%num_leaf)%grow(dt = 0.0d0)

        enddo
    elseif(present(RootNodeID) )then

        ! set mainroot
        call obj%root(obj%numRoot()+1 )%init(obj%rootconfig)
        call obj%root(i)%resize(&
            x = obj%mr_width, &
            y = obj%mr_width, &
            z = obj%mr_length/dble(obj%mr_node) &
            )
        call obj%root(i)%rotate(&
            x = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig)),  &
            y = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig)),  &
            z = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig))   &
            )                
        
        i = RootNodeID
        call obj%root(obj%numRoot()  )%connect("=>",obj%root(i))
        obj%root2root(obj%numRoot(),i) = 1
        
    else
        print *, "ERROR :: add Node ` soybean >> RootNodeID or StemNodeID should be identified."  
        stop
    endif
    
    call obj%update()
end subroutine
! ########################################


! ########################################
subroutine addStemSoybean(obj,stemid,rotx,roty,rotz,json)
    class(Soybean_),intent(inout) :: obj
    integer(int32),intent(in) :: stemid
    character(*),optional,intent(in) :: json
    real(real64),optional,intent(in) :: rotx,roty,rotz
    integer(int32) :: i

    ! add a stem after stem(stemid)
    do i=1,size(obj%stem)
        if( obj%stem(i)%femdomain%mesh%empty() .eqv. .true. )then
            if(present(json) )then
                call obj%stem(i)%init(json)
                call obj%stem(i)%rotate(x=rotx,y=roty,z=rotz)
                call obj%stem(i)%connect("=>",obj%stem(stemid))
                return
            else
                call obj%stem(i)%init()
                call obj%stem(i)%rotate(x=rotx,y=roty,z=rotz)
                call obj%stem(i)%connect("=>",obj%stem(stemid))
                return
            endif
        else
            cycle
        endif
    enddo



end subroutine
! #############################################################

subroutine deformSoybean(obj,displacement,penaltyparameter,groundLevel,disp,&
    x_min,x_max,y_min,y_max,z_min,z_max) 

    class(Soybean_),target,intent(inout) :: obj

    ! deform soybean by displacement
    real(real64),optional,intent(in) :: displacement(:)

    ! >> regacy
    real(real64),optional,intent(in) :: groundLevel,disp(3)
    real(real64),optional,intent(in) :: penaltyparameter,x_min,x_max,y_min,y_max,z_min,z_max
    type(FEMDomainp_),allocatable :: domainsp(:)
    integer(int32),allocatable :: contactList(:,:)
    integer(int32) :: i,j,numDomain,stemDomain,leafDomain,rootDomain,from,to,nd,nn
    real(real64) :: penalty,GLevel

    if(present(displacement) )then
        if(size(displacement)/=obj%nn()*3 )then
            print *, "ERROR :: deformSoybean >> size(displacement) should be (obj%numStem() + obj%numLeaf() + obj%numRoot())*3"
            return
        endif

        ! order :: stem -> leaf -> root
        from = 1
        to   = 0
        if(allocated(obj%stem) )then
            do i=1,size(obj%stem)
                if(.not. obj%stem(i)%femdomain%Mesh%empty() )then
                    nn = obj%stem(i)%femdomain%nn()
                    nd = obj%stem(i)%femdomain%nd()

                    to = from + obj%stem(i)%femdomain%nn()*obj%stem(i)%femdomain%nd() -1

                    obj%stem(i)%femdomain%mesh%nodcoord(:,:) = &
                    obj%stem(i)%femdomain%mesh%nodcoord(:,:) + &
                    reshape(displacement(from:to),nn,nd )

                    from = to + 1
                endif
            enddo
        endif

        if(allocated(obj%leaf) )then
            do i=1,size(obj%leaf)
                if(.not. obj%leaf(i)%femdomain%Mesh%empty() )then
                    nn = obj%leaf(i)%femdomain%nn()
                    nd = obj%leaf(i)%femdomain%nd()

                    to = from + obj%leaf(i)%femdomain%nn()*obj%leaf(i)%femdomain%nd() -1

                    obj%leaf(i)%femdomain%mesh%nodcoord(:,:) = &
                    obj%leaf(i)%femdomain%mesh%nodcoord(:,:) + &
                    reshape(displacement(from:to),nn,nd )

                    from = to + 1
                endif
            enddo
        endif

        if(allocated(obj%root) )then
            do i=1,size(obj%root)
                if(.not. obj%root(i)%femdomain%Mesh%empty() )then
                    nn = obj%root(i)%femdomain%nn()
                    nd = obj%root(i)%femdomain%nd()

                    to = from + obj%root(i)%femdomain%nn()*obj%root(i)%femdomain%nd() -1

                    obj%root(i)%femdomain%mesh%nodcoord(:,:) = &
                    obj%root(i)%femdomain%mesh%nodcoord(:,:) + &
                    reshape(displacement(from:to),nn,nd )

                    from = to + 1
                endif
            enddo
        endif
        return
    endif

    ! >> regacy >>
    if(.not. allocated(obj%Stem) )then
        print *, "ERROR :: deformSoybean >> no soybean is found!"
        return
    endif
    numDomain = 0
    
    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
            endif
        enddo
    endif
    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
            endif
        enddo
    endif
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if(obj%root(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
            endif
        enddo
    endif
    
    allocate(domainsp(numDomain) )
    numDomain=0
    stemDomain=0
    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                stemDomain = stemDomain + 1
                domainsp(numDomain)%femdomainp =>  obj%stem(i)%femdomain
            endif
        enddo
    endif

    leafDomain = 0
    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                leafDomain = leafDomain + 1
                domainsp(numDomain)%femdomainp =>  obj%leaf(i)%femdomain
            endif
        enddo
    endif

    rootDomain = 0
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if(obj%root(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                rootDomain = rootDomain + 1
                domainsp(numDomain)%femdomainp =>  obj%root(i)%femdomain
            endif
        enddo
    endif

    ! (1) create contact-list for all domains

    contactlist = zeros(numDomain,numDomain)
    if(allocated(obj%stem2stem))then
        do i=1,stemDomain
            do j=1,stemDomain
                contactlist( i, j  ) = obj%stem2stem(i,j)
            enddo
        enddo
    endif

    if(allocated(obj%leaf2stem) )then
        do i=1,leafDomain
            do j=1,stemDomain
                contactlist( i + stemDomain, j  ) = obj%leaf2stem(i,j)
            enddo
        enddo
    endif

    if(allocated(obj%root2stem) )then
        do i=1,rootDomain
            do j=1,stemDomain
                contactlist( i + stemDomain + leafDomain, j  ) = obj%root2stem(i,j)
            enddo
        enddo
    endif

    if(allocated(obj%root2root) )then
        do i=1,rootDomain
            do j=1,rootDomain
                contactlist( i + stemDomain + leafDomain, j+ stemDomain + leafDomain  ) = obj%root2root(i,j)
            enddo
        enddo
    endif
    
    call obj%contact%init(femdomainsp=domainsp,contactlist=contactlist)

    ! load material info
    numDomain = 0
    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                call obj%contact%setYoungModulus(YoungModulus=obj%stemYoungModulus(i),DomainID=numDomain) 
                call obj%contact%setPoissonRatio(PoissonRatio=obj%stemPoissonRatio(i),DomainID=numDomain) 
                call obj%contact%setDensity(density=obj%stemDensity(i),DomainID=numDomain) 
            endif
        enddo
    endif
    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                call obj%contact%setYoungModulus(YoungModulus=obj%leafYoungModulus(i),DomainID=numDomain) 
                call obj%contact%setPoissonRatio(PoissonRatio=obj%leafPoissonRatio(i),DomainID=numDomain) 
                call obj%contact%setDensity(density=obj%leafDensity(i),DomainID=numDomain) 
            endif
        enddo
    endif
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if(obj%root(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                call obj%contact%setYoungModulus(YoungModulus=obj%rootYoungModulus(i),DomainID=numDomain) 
                call obj%contact%setPoissonRatio(PoissonRatio=obj%rootPoissonRatio(i),DomainID=numDomain) 
                call obj%contact%setDensity(density=obj%rootDensity(i),DomainID=numDomain) 
            endif
        enddo
    endif
    !


    penalty = input(default=1000.0d0, option=penaltyparameter)
    
    call obj%contact%setup(penaltyparameter=penalty)

    ! if displacement is set, load displacement
    if(present(disp) )then
        do i=1,numDomain
            call obj%contact%fix(direction="x",disp=disp(1), DomainID=i,&
                x_min=x_min,x_max=x_max,&
                y_min=y_min,y_max=y_max,&
                z_min=z_min,z_max=z_max)
            call obj%contact%fix(direction="y",disp=disp(2), DomainID=i,&
                x_min=x_min,x_max=x_max,&
                y_min=y_min,y_max=y_max,&
                z_min=z_min,z_max=z_max)
            call obj%contact%fix(direction="z",disp=disp(3), DomainID=i,&
                x_min=x_min,x_max=x_max,&
                y_min=y_min,y_max=y_max,&
                z_min=z_min,z_max=z_max)
        enddo    
    endif
    

    Glevel = input(default=0.0d0,option=groundLevel)
    ! under-ground parts are fixed.
    do i=1,numDomain
        call obj%contact%fix(direction="x",disp=0.0d0, DomainID=i,&
            z_max=Glevel)
        call obj%contact%fix(direction="y",disp=0.0d0, DomainID=i,&
            z_max=Glevel)
        call obj%contact%fix(direction="z",disp=0.0d0, DomainID=i,&
            z_max=Glevel)
    enddo

    ! solve > get displacement
    call obj%contact%solver%solve("BiCGSTAB")
    ! update mesh
    call obj%contact%updateMesh()


end subroutine
! #####################################################################


! #####################################################################
function getVolumeSoybean(obj,stem,leaf,root) result(ret)
    class(Soybean_),intent(in) :: obj
    logical,optional,intent(in) :: stem, leaf, root
    logical :: all
    integer(int32) :: i,j
    real(real64) :: ret

    all = .false.
    if(.not.present(stem) .and..not.present(leaf)  )then
        if(.not. present(root) )then
            all = .true.
        endif
    endif

    ret =0.0d0
    if(all)then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                do j=1,obj%stem(i)%femdomain%ne()
                    ret = ret + obj%stem(i)%femdomain%getVolume(elem=j)
                enddo
            endif
        enddo
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                do j=1,obj%leaf(i)%femdomain%ne()
                    ret = ret + obj%leaf(i)%femdomain%getVolume(elem=j)
                enddo
            endif
        enddo
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                do j=1,obj%root(i)%femdomain%ne()
                    ret = ret + obj%root(i)%femdomain%getVolume(elem=j)
                enddo
            endif
        enddo
        return
    endif

    if(present(stem))then
        if(stem  .or. all)then
            do i=1,size(obj%stem)
                if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                    do j=1,obj%stem(i)%femdomain%ne()
                        ret = ret + obj%stem(i)%femdomain%getVolume(elem=j)
                    enddo
                endif
            enddo
        endif
    endif
    if(present(leaf) )then
        if(leaf )then
            do i=1,size(obj%leaf)
                if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                    do j=1,obj%leaf(i)%femdomain%ne()
                        ret = ret + obj%leaf(i)%femdomain%getVolume(elem=j)
                    enddo
                endif
            enddo
        endif
    endif
    if(present(root))then
        if(root)then
            do i=1,size(obj%root)
                if( .not.obj%root(i)%femdomain%mesh%empty() )then
                    do j=1,obj%root(i)%femdomain%ne()
                        ret = ret + obj%root(i)%femdomain%getVolume(elem=j)
                    enddo
                endif
            enddo
        endif
    endif

end function
! ############################################################################


! #####################################################################
function getVolumePerElementSoybean(obj) result(volume)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: i,j,elem_id
    real(real64),allocatable :: volume(:)

    elem_id = 0
    volume = zeros(obj%ne() )

    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() )then
            do j=1,obj%stem(i)%femdomain%ne()
                elem_id = elem_id + 1
                volume(elem_id) = obj%stem(i)%femdomain%getVolume(elem=j)
            enddo
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
            do j=1,obj%leaf(i)%femdomain%ne()
                elem_id = elem_id + 1
                volume(elem_id) = obj%leaf(i)%femdomain%getVolume(elem=j)
            enddo
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() )then
            do j=1,obj%root(i)%femdomain%ne()
                elem_id = elem_id + 1
                volume(elem_id) = obj%root(i)%femdomain%getVolume(elem=j)
            enddo
        endif
    enddo
    
end function
! ############################################################################


! ############################################################################
function getBiomassSoybean(obj,stem,leaf,root) result(ret)
    class(Soybean_),intent(in) :: obj
    logical,optional,intent(in) :: stem, leaf, root
    logical :: all
    integer(int32) :: i,j
    real(real64) :: ret,volume

    all = .false.
    if(.not.present(stem) .and..not.present(leaf)  )then
        if(.not. present(root) )then
            all = .true.
        endif
    endif

    ret =0.0d0
    if(all)then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                do j=1,obj%stem(i)%femdomain%ne()
                    volume = obj%stem(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume)
                    ret = ret + volume*obj%stem(i)%drydensity(j) 
                enddo
                
            endif
        enddo
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                do j=1,obj%leaf(i)%femdomain%ne()
                    volume = obj%leaf(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) 
                    ret = ret + volume*obj%leaf(i)%drydensity(j) 
                enddo
                
            endif
        enddo
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                do j=1,obj%root(i)%femdomain%ne()
                    volume = obj%root(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) 
                    ret = ret + volume*obj%root(i)%drydensity(j) 
                enddo
                
            endif
        enddo
        return
    endif

    if(present(stem))then
        if(stem  .or. all)then
            do i=1,size(obj%stem)
                if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                    do j=1,obj%stem(i)%femdomain%ne()
                        volume = obj%stem(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) 
                        ret = ret + volume*obj%stem(i)%drydensity(j) 
                    enddo 
                endif
            enddo
        endif
    endif
    if(present(leaf) )then
        if(leaf )then
            do i=1,size(obj%leaf)
                if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                    do j=1,obj%leaf(i)%femdomain%ne()
                        volume = obj%leaf(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) 
                        ret = ret + volume*obj%leaf(i)%drydensity(j) 
                    enddo
                endif
            enddo
        endif
    endif
    if(present(root))then
        if(root)then
            do i=1,size(obj%root)
                if( .not.obj%root(i)%femdomain%mesh%empty() )then
                    do j=1,obj%root(i)%femdomain%ne()
                        volume = obj%root(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) 
                        ret = ret + volume*obj%root(i)%drydensity(j) 
                    enddo
                endif
            enddo
        endif
    endif

end function


function getTotalWeightSoybean(obj,stem,leaf,root,waterDensity) result(ret)
    class(Soybean_),intent(in) :: obj
    logical,optional,intent(in) :: stem, leaf, root
    real(real64),optional,intent(in) :: waterDensity
    logical :: all
    integer(int32) :: i,j
    real(real64) :: ret,volume,water_density

    ! kg, m
    water_density=input(default=1000.0d0,option=waterDensity)

    all = .false.
    if(.not.present(stem) .and..not.present(leaf)  )then
        if(.not. present(root) )then
            all = .true.
        endif
    endif

    ret =0.0d0
    if(all)then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                do j=1,obj%stem(i)%femdomain%ne()
                    volume = obj%stem(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                    ret = ret + volume*obj%stem(i)%drydensity(j) + volume*obj%stem(i)%watercontent(j)*water_density
                enddo
                
            endif
        enddo
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                do j=1,obj%leaf(i)%femdomain%ne()
                    volume = obj%leaf(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                    ret = ret + volume*obj%leaf(i)%drydensity(j) + volume*obj%leaf(i)%watercontent(j)*water_density
                enddo
                
            endif
        enddo
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                do j=1,obj%root(i)%femdomain%ne()
                    volume = obj%root(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                    ret = ret + volume*obj%root(i)%drydensity(j) + volume*obj%root(i)%watercontent(j)*water_density
                enddo
                
            endif
        enddo
        return
    endif

    if(present(stem))then
        if(stem  .or. all)then
            do i=1,size(obj%stem)
                if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                    do j=1,obj%stem(i)%femdomain%ne()
                        volume = obj%stem(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                        ret = ret + volume*obj%stem(i)%drydensity(j) + volume*obj%stem(i)%watercontent(j)*water_density
                    enddo 
                endif
            enddo
        endif
    endif
    if(present(leaf) )then
        if(leaf )then
            do i=1,size(obj%leaf)
                if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                    do j=1,obj%leaf(i)%femdomain%ne()
                        volume = obj%leaf(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                        ret = ret + volume*obj%leaf(i)%drydensity(j) + volume*obj%leaf(i)%watercontent(j)*water_density
                    enddo
                endif
            enddo
        endif
    endif
    if(present(root))then
        if(root)then
            do i=1,size(obj%root)
                if( .not.obj%root(i)%femdomain%mesh%empty() )then
                    do j=1,obj%root(i)%femdomain%ne()
                        volume = obj%root(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                        ret = ret + volume*obj%root(i)%drydensity(j) + volume*obj%root(i)%watercontent(j)*water_density
                    enddo
                endif
            enddo
        endif
    endif

end function


!function getBioMassSoybean(obj,stemDensity,leafDensity,rootDensity) result(ret)
!    class(Soybean_),intent(in) :: obj
!    real(real64),optional,intent(in) :: stemDensity,leafDensity,rootDensity
!    logical :: all
!    integer(int32) :: i,j
!    real(real64) :: ret
!
!    ret = 0.0d0
!
!    if(present(stemDensity))then
!        ret = ret + obj%getVolume(stem=.true.) * stemDensity
!    endif
!
!    if(present(leafDensity))then
!        ret = ret + obj%getVolume(leaf=.true.) * leafDensity
!    endif
!
!    if(present(rootDensity))then
!        ret = ret + obj%getVolume(root=.true.) * rootDensity
!    endif
!
!
!
!end function
subroutine fall_leafSoybean(obj,BranchID,InterNodeID,with_petiole)
    class(Soybean_),intent(inout) :: obj
    integer(int32),intent(in) :: BranchID, InterNodeID
    logical,optional,intent(in) :: with_petiole
    integer(int32) :: i, j,stemID
    integer(int32),allocatable :: petioleIDs(:)

    ! fall leaves
    do i=1, size(obj%stem)
        if(obj%stem(i)%empty() )cycle
        if(obj%stem(i)%stemID == branchID .and.&
            obj%stem(i)%InterNodeID == InterNodeID   )then
            stemID = i
        endif
    enddo

    allocate(petioleIDs(0) )
    do i=1,size(obj%stem2stem,1)
        ! stem id i -> stemID 
        if(obj%stem2stem(i,StemID) /=0 .and. obj%stem(i)%InterNodeID < 1 )then
            ! petiole
            petioleIDs = petioleIDs // [i]
        endif
    enddo

    print *, "petioleIDs",petioleIDs
    ! remove leaves
    do i=1,size(petioleIDs)
        do j=1,size(obj%leaf2stem,1)
            if(obj%leaf2stem(j, petioleIDs(i) )/=0 )then
                obj%leaf2stem(j, petioleIDs(i) ) =0
                call obj%leaf(j)%remove()
            endif
        enddo
    enddo

    if(present(with_petiole) )then
        if(with_petiole)then
            do i=1,size(petioleIDs)
                obj%stem2stem(petioleIDs(i),: ) =0
                call obj%stem( petioleIDs(i) )%remove()
            enddo
        endif
    endif

end subroutine



subroutine removeSoybean(obj,root)
    class(Soybean_),intent(inout) :: obj
    logical,optional,intent(in) :: root

    if(present(root) )then
        if(root)then
            
            obj%mr_node=0
            obj%brr_node(:)=0
            obj%brr_from(:)=0
            obj%mr_length=0.0d0
            obj%brr_length(:)=0.0d0
            obj%mr_width=0.0d0
            obj%brr_width(:)=0.0d0
            obj%mr_angle_ave=0.0d0
            obj%brr_angle_ave(:)=0.0d0
            obj%mr_angle_sig=0.0d0
            obj%brr_angle_sig(:)=0.0d0
            if (allocated(obj%RootSystem) ) deallocate(obj%RootSystem)
            if (allocated(obj%Root) ) deallocate(obj%Root)
            if (allocated(obj%rootYoungModulus) ) deallocate(obj%rootYoungModulus)
            if (allocated(obj%rootPoissonRatio) ) deallocate(obj%rootPoissonRatio)
            if (allocated(obj%rootDensity) ) deallocate(obj%rootDensity)


            if (allocated(obj%root2stem) ) deallocate(obj%root2stem)
            if (allocated(obj%root2root) ) deallocate(obj%root2root)
            if (allocated(obj%root_list) ) deallocate(obj%root_list)

            if (allocated(obj%root_angle) ) deallocate(obj%root_angle)
            obj%rootconfig=" "
            obj%Num_Of_Root = 0


        endif
        return
    endif

    obj%growth_habit = " "
    obj%growth_stage = " "
    obj%Num_Of_Node = 0
    obj%Num_Of_Root = 0
    
    obj%MaxLeafNum= 300
    obj%MaxRootNum=300
    obj%MaxStemNum= 300

    
    
    obj%ms_node=0
    obj%br_node(:) = 0
    obj%br_from(:) = 0
    obj%ms_length=0.0d0
    obj%br_length(:)=0.0d0
    obj%ms_width=0.0d0
    obj%br_width(:)=0.0d0
    obj%ms_angle_ave=0.0d0
    obj%br_angle_ave(:)=0.0d0
    obj%ms_angle_sig=0.0d0
    obj%br_angle_sig(:)=0.0d0
    

    obj%mr_node=0
    obj%brr_node(:)=0
    obj%brr_from(:)=0
    obj%mr_length=0.0d0
    obj%brr_length(:)=0.0d0
    obj%mr_width=0.0d0
    obj%brr_width(:)=0.0d0
    obj%mr_angle_ave=0.0d0
    obj%brr_angle_ave(:)=0.0d0
    obj%mr_angle_sig=0.0d0
    obj%brr_angle_sig(:)=0.0d0

    obj%peti_size_ave(:)=0.0d0
    obj%peti_size_sig(:)=0.0d0
    obj%peti_width_ave(:)=0.0d0
    obj%peti_width_sig(:)=0.0d0
    obj%peti_angle_ave(:)=0.0d0
    obj%peti_angle_sig(:)=0.0d0

    obj%leaf_angle_ave(:)=0.0d0
    obj%leaf_angle_sig(:)=0.0d0
    obj%leaf_length_ave(:)=0.0d0
    obj%leaf_length_sig(:)=0.0d0
    obj%leaf_width_ave(:)=0.0d0
    obj%leaf_width_sig(:)=0.0d0
    obj%leaf_thickness_ave(:)=0.0d0
    obj%leaf_thickness_sig(:)=0.0d0
    
    obj%Stage="" ! VE, CV, V1,V2, ..., R1, R2, ..., R8
    obj%name=""
    obj%stage_id=0
    obj%dt=0.0d0
    call obj%Seed%remove()
    if (allocated(obj%NodeSystem) ) deallocate(obj%NodeSystem)
    if (allocated(obj%RootSystem) ) deallocate(obj%RootSystem)

    if (allocated(obj%Stem) ) deallocate(obj%Stem)
    if (allocated(obj%Leaf) ) deallocate(obj%Leaf)
    if (allocated(obj%Root) ) deallocate(obj%Root)
    

    ! material info
    if (allocated(obj%stemYoungModulus) ) deallocate(obj%stemYoungModulus)
    if (allocated(obj%leafYoungModulus) ) deallocate(obj%leafYoungModulus)
    if (allocated(obj%rootYoungModulus) ) deallocate(obj%rootYoungModulus)

    if (allocated(obj%stemPoissonRatio) ) deallocate(obj%stemPoissonRatio)
    if (allocated(obj%leafPoissonRatio) ) deallocate(obj%leafPoissonRatio)
    if (allocated(obj%rootPoissonRatio) ) deallocate(obj%rootPoissonRatio)

    if (allocated(obj%stemDensity) ) deallocate(obj%stemDensity)
    if (allocated(obj%leafDensity) ) deallocate(obj%leafDensity)
    if (allocated(obj%rootDensity) ) deallocate(obj%rootDensity)
    

    if(allocated(obj%NodeID_MainStem)) deallocate(obj%NodeID_MainStem)
    if(allocated(obj%NodeID_Branch)) deallocate(obj%NodeID_Branch)
    ! 節-節点データ構造
    call obj%struct%remove(all=.true.)
    if (allocated(obj%leaf2stem) ) deallocate(obj%leaf2stem)
    if (allocated(obj%stem2stem) ) deallocate(obj%stem2stem)
    if (allocated(obj%root2stem) ) deallocate(obj%root2stem)
    if (allocated(obj%root2root) ) deallocate(obj%root2root)
    
    ! 器官オブジェクト配列
    if (allocated(obj%leaf_list) ) deallocate(obj%leaf_list)
    if (allocated(obj%stem_list) ) deallocate(obj%stem_list)
    if (allocated(obj%root_list) ) deallocate(obj%root_list)

    ! シミュレータ
    call obj%contact%remove()
    obj%time=0.0d0
    obj%seed_length=0.0d0
    obj%seed_width=0.0d0
    obj%seed_height=0.0d0
    if (allocated(obj%stem_angle) ) deallocate(obj%stem_angle)
    if (allocated(obj%root_angle) ) deallocate(obj%root_angle)
    if (allocated(obj%leaf_angle) ) deallocate(obj%leaf_angle)

    obj%stemconfig=" "
    obj%rootconfig=" "
    obj%leafconfig=" "

end subroutine

function stemlengthSoybean(obj,StemID) result(ret)
    class(Soybean_),intent(inout) :: obj
    integer(int32),intent(in) :: StemID ! 0, 1, 2...
    integer(int32) :: num_snode,i
    real(real64),allocatable :: ret(:)

    if(StemID==0)then
        ! main stem
        num_snode = size(obj%NodeID_MainStem)
        allocate(ret(num_snode) )
        do i=1,num_snode
            ret(i) = obj%stem(obj%NodeID_MainStem(i))%getLength()
        enddo
    else
        if(StemID >=size(obj%NodeID_Branch)) then
            print *, "ERROR :: stemlengthSoybean >> StemID >=size(obj%NodeID_Branch)"
            ret = zeros(1)
            return
        endif
        ! main stem
        num_snode = size(obj%NodeID_Branch(StemID)%ID)
        allocate(ret(num_snode) )
        do i=1,num_snode
            ret(i) = obj%stem(obj%NodeID_Branch(StemID)%ID(i))%getLength()
        enddo
    endif



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
subroutine resizeSoybean(obj,StemID,StemLength)
    class(Soybean_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: StemID
    real(real64),optional,intent(in) :: StemLength(:)
    integer(int32) :: num_snode,i

    if(present(StemID) )then
        if(.not.present(StemLength) )then
            print *, "ERROR :: resizeSoybean >> needs StemLength(:) "
            stop
        endif

        if(StemID==0)then
            ! main stem
            num_snode = size(obj%NodeID_MainStem)
            
            do i=1,num_snode
                call obj%stem(obj%NodeID_MainStem(i))%grow(length=StemLength(i))
            enddo
        else
            if(StemID >=size(obj%NodeID_Branch)) then
                print *, "ERROR :: resizeSoybean >> StemID >=size(obj%NodeID_Branch)"
                
                return
            endif
            ! main stem
            num_snode = size(obj%NodeID_Branch(StemID)%ID)
            
            do i=1,num_snode
                call obj%stem(obj%NodeID_Branch(StemID)%ID(i))%grow(length=StemLength(i) )
            enddo
        endif
        call obj%update()
    

    endif
end subroutine
! ###################################################################


! ###################################################################
function NumberOfBranchSoybean(obj)  result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret = 0
    if(allocated(obj%NodeID_Branch) )then
        do i=1,size(obj%NodeID_Branch)
            if(allocated(obj%NodeID_Branch(i)%ID ))then
                if(size(obj%NodeID_Branch(i)%ID) >= 1)then
                    ret = ret + 1
                endif
            endif
        enddo
    endif
end function
! ###################################################################

! ###################################################################
function findApicalSoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),allocatable :: ret(:)
    !integer(int32),optional,intent(in) :: StemID
    integer(int32),allocatable :: stem
    integer(int32) :: i,j,itr

    ret = zeros( obj%NumberOfBranch()+1 )

    ret(1)=maxval(obj%NodeID_MainStem(:) )

    itr = 1
    do i=1,obj%NumberOfBranch()
        if(allocated(obj%NodeID_Branch(i)%ID))then
            itr = itr + 1
            ret(itr) = maxval(obj%NodeID_Branch(i)%ID(:) )
        endif
    enddo
    
!    if(present(StemID) )then
!        if(StemID > size(obj%br_node) )then
!            print *, "ERROR >> findApicalSoybean >> number of branch is ",size(obj%br_node)
!            print *, "StemID=",StemID,"is larger than it."
!            return
!        endif
!
!
!        return
!    endif

end function
! ###################################################################

!function propertiesSoybean(obj) result(ret)
!    class(Soybean_) ,intent(in) :: obj
!
!    
!end function

! ##################################################################
pure function nnSoybean(obj) result(ret)
    class(Soybean_) ,intent(in) :: obj
    integer(int32) :: ret, i

    ! get number of node (point)
    ret = 0
    
    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
                ret = ret + obj%stem(i)%femdomain%nn()
            endif
        enddo
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
                ret = ret + obj%leaf(i)%femdomain%nn()
            endif
        enddo
    endif

    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() ) then
                ret = ret + obj%root(i)%femdomain%nn()
            endif
        enddo
    endif

end function
! ##################################################################



! ##################################################################
function neSoybean(obj) result(ret)
    class(Soybean_) ,intent(in) :: obj
    integer(int32) :: ret, i

    ! get number of element
    ret = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%stem(i)%femdomain%ne()
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%leaf(i)%femdomain%ne()
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%root(i)%femdomain%ne()
        endif
    enddo

end function
! ##################################################################


! ##################################################################
function nsSoybean(obj) result(ret)
    class(Soybean_) ,intent(in) :: obj
    integer(int32) :: ret, i

    ! get number of subdomain
    ret = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret = ret + 1
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret = ret + 1
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret = ret + 1
        endif
    enddo

end function


! ##################################################################

function getSubDomainSoybean(obj,id) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in) :: id
    type(FEMDomain_) :: ret
    integer(int32) :: i,ret_id
    
    ! get number of subdomain
    ret_id = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = obj%stem(i)%femdomain
                return
            endif
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = obj%stem(i)%femdomain
                return
            endif
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = obj%stem(i)%femdomain
                return
            endif
        endif
    enddo

    print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains",ret_id
    return

    

end function
! ##################################################################

! ##################################################################

subroutine setSubDomainSoybean(obj,domain,id) 
    class(Soybean_),intent(inout) :: obj
    type(FEMDomain_),intent(in) :: domain
    integer(int32),intent(in) :: id
    integer(int32) :: i,domain_id
    
    ! get number of subdomain
    domain_id = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            domain_id = domain_id + 1
            if(id==domain_id)then
                obj%stem(i)%femdomain = domain
                return
            endif
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            domain_id = domain_id + 1
            if(id==domain_id)then
                obj%stem(i)%femdomain = domain
                return
            endif
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            domain_id = domain_id + 1
            if(id==domain_id)then
                obj%stem(i)%femdomain = domain
                return
            endif
        endif
    enddo

    print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains",domain_id
    return

    

end subroutine
! ##################################################################


! ##################################################################

function getSubDomainTypeSoybean(obj,id) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in) :: id
    character(:),allocatable :: ret
    integer(int32) :: i,ret_id
    
    ! get number of subdomain
    ret_id = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = "stem"
                return
            endif
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = "leaf"
                return
            endif
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = "root"
                return
            endif
        endif
    enddo

    print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains",ret_id
    return

    

end function
! ##################################################################


! ##################################################################
pure function isMainStemSoybean(obj,StemNodeID) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in)  :: StemNodeID
    logical :: ret

    ret = exists(vector=obj%NodeID_MainStem, val=StemNodeID)
    
end function
! ##################################################################


! ##################################################################
pure function isBranchStemSoybean(obj,StemNodeID) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in)  :: StemNodeID
    logical :: ret

    if(obj%branchID(StemNodeID)==0 )then
        ret = .False.
    else
        ret = .True.
    endif

end function
! ##################################################################

pure function branchIDSoybean(obj,StemNodeID) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in)  :: StemNodeID
    integer(int32),allocatable :: ret
    integer(int32) :: i,j,k,l,m,n,ret_id

    do i=1, size(obj%NodeID_Branch)
        if(exist(obj%NodeID_Branch(i)%ID(:),StemNodeID ) )then
            ret = i
            return
        endif
    enddo
    ret = 0
    
end function
! ##################################################################

subroutine checkPropertiesSoybean(obj, Simulator)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in)  ::  Simulator
    type(Time_) :: time
    type(IO_) :: f
    
    call f%open("__soybeanclass__checkPropertiesSoybean.log")
    if( Simulator == PF_DEFORMATION_ANALYSIS )then
        call print("---------------------------------------")
        call print("-- checkProperties @ SoybeanClass  ----")
        call print("---------------------------------------")
        call print(" Simulator mode :: Deformation analysis")
        call print("---------------------------------------")
        call print("Date and time: "//time%DateAndTime())
        call print("---------------------------------------")
        call print("Checking datasets for deformation analysis...")
        ! check if it ready or not.
        print *, "property_deform_material_density       |",&
            obj%property_deform_material_density
        print *, "property_deform_material_YoungModulus  |",&
            obj%property_deform_material_YoungModulus
        print *, "property_deform_material_PoissonRatio  |",&
            obj%property_deform_material_PoissonRatio
        print *, "property_deform_initial_Displacement   |",&
            obj%property_deform_initial_Displacement
        print *, "property_deform_initial_Stress         |",&
            obj%property_deform_initial_Stress
        print *, "property_deform_boundary_TractionForce |",&
            obj%property_deform_boundary_TractionForce
        print *, "property_deform_boundary_Displacement  |",&
            obj%property_deform_boundary_Displacement
        print *, "property_deform_gravity                |",&
            obj%property_deform_gravity
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
            str(obj%property_deform_material_density)           )
        call f%write("property_deform_material_YoungModulus  |"// &
            str(obj%property_deform_material_YoungModulus)      )
        call f%write("property_deform_material_PoissonRatio  |"// &
            str(obj%property_deform_material_PoissonRatio)      )
        call f%write("property_deform_initial_Displacement   |"// &
            str(obj%property_deform_initial_Displacement)       )
        call f%write("property_deform_initial_Stress         |"// &
            str(obj%property_deform_initial_Stress)             )
        call f%write("property_deform_boundary_TractionForce |"// &
            str(obj%property_deform_boundary_TractionForce)     )
        call f%write("property_deform_boundary_Displacement  |"// &
            str(obj%property_deform_boundary_Displacement)      )
        call f%write("property_deform_gravity                |"// &
            str(obj%property_deform_gravity)                    )
        call f%write("---------------------------------------")
    else
        call print("Invalid  Simulator ID :: "//str( Simulator) )
        
    endif
    call f%close()

end subroutine
! ##################################################################
! ##################################################################
subroutine setPropertiesDensitySoybean(obj)
    class(Soybean_),intent(inout) :: obj
    integer(int32) :: i, j
    
            
    ! default == false
    obj%property_deform_material_density = .false.
    
    ! check 
    ! Does stem/leaf/root have Density for each element?
    if(allocated(obj%leaf) )then
        print *, "[ok] setPropertiesSoybean >> leaf exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                ! exists
                ! check 
                !  (1) allocation of density(:)
                !  (2) size of density(:)
                !  if invalid, compute from drydensity(:)and watercontent(:)
                !  if not both do not exists, create all as 0.0
                if( allocated(obj%leaf(i)%density ))then
                    ! check size
                    
                    !if(size(obj%leaf(i)%density)/=obj%leaf(i)%femdomain%ne() )then
                        !print *, "[Caution] setPropertiesSoybean >> stem("//str(i)//")%density >> "//&
                        !"size(obj%leaf(i)%density)/=obj%leaf(i)%femdomain%ne() >> reset by zero!!"
                        !deallocate(obj%leaf(i)%density)
                        ! let's go to next
                    !else
                        print *, "[ok] setPropertiesSoybean &
                        >> leaf("//str(i)//")%density >> allocated"
                        ! then ok. let's return
                        obj%property_deform_material_density = .true.
                        
                    !endif
                else
                ! density is not allocated.
                
                    obj%leaf(i)%density = zeros(obj%leaf(i)%femdomain%ne())
                
                    ! >> try to compute from drydensity(:) and watercontent(:)
                    ! >> check existatce of drydensity(:) and watercontent(:) 
                    if(.not.allocated(obj%leaf(i)%drydensity ) )then
                        obj%leaf(i)%drydensity = zeros( obj%leaf(i)%femdomain%ne() )
                    endif
                    if(.not.allocated(obj%leaf(i)%watercontent ) )then
                        obj%leaf(i)%watercontent = zeros( obj%leaf(i)%femdomain%ne() )
                    endif
                    
                    if(size(obj%leaf(i)%drydensity ) /=obj%leaf(i)%femdomain%ne()  )then
                        obj%leaf(i)%drydensity = zeros( obj%leaf(i)%femdomain%ne() )
                    endif
                    if(size(obj%leaf(i)%watercontent ) /=obj%leaf(i)%femdomain%ne()  )then
                        obj%leaf(i)%watercontent = zeros( obj%leaf(i)%femdomain%ne() )
                    endif
                    
                    ! compute density from drydensity and water content
                    ! \rho_t = \rho_d * (1 - w )
                    !!$OMP parallel do private(j)
                    do j=1,obj%leaf(i)%femdomain%ne()
                        obj%leaf(i)%density(j) = obj%leaf(i)%drydensity(j) * (1.0d0 - obj%leaf(i)%watercontent(j))
                    enddo
                    !!$OMP end parallel do
                endif
            endif
        enddo
        !!$OMP end parallel do
        obj%property_deform_material_density = .true.
    else
        print *, "[Notice] setPropertiesSoybean >> no leaf"
    endif
    !! stem 
    if(allocated(obj%stem) )then
        print *, "[ok] setPropertiesSoybean >> stems exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                ! exists
                ! check 
                !  (1) allocation of density(:)
                !  (2) size of density(:)
                !  if invalid, compute from drydensity(:)and watercontent(:)
                !  if not both do not exists, create all as 0.0
                if( allocated(obj%stem(i)%density ))then
                    ! check size
                    
                    !if(size(obj%stem(i)%density)/=obj%stem(i)%femdomain%ne() )then
                        !print *, "[Caution] setPropertiesSoybean >> stem("//str(i)//")%density >> "//&
                        !"size(obj%stem(i)%density)/=obj%stem(i)%femdomain%ne() >> reset by zero!!"
                        !deallocate(obj%stem(i)%density)
                        ! let's go to next
                    !else
                        print *, "[ok] setPropertiesSoybean >> stem("//str(i)//")%density >> allocated"
                        ! then ok. let's return
                        obj%property_deform_material_density = .true.
                        
                    !endif
                else
                ! density is not allocated.
                
                    obj%stem(i)%density = zeros(obj%stem(i)%femdomain%ne())
                
                    ! >> try to compute from drydensity(:) and watercontent(:)
                    ! >> check existatce of drydensity(:) and watercontent(:) 
                    if(.not.allocated(obj%stem(i)%drydensity ) )then
                        obj%stem(i)%drydensity = zeros( obj%stem(i)%femdomain%ne() )
                    endif
                    if(.not.allocated(obj%stem(i)%watercontent ) )then
                        obj%stem(i)%watercontent = zeros( obj%stem(i)%femdomain%ne() )
                    endif
                    
                    if(size(obj%stem(i)%drydensity ) /=obj%stem(i)%femdomain%ne()  )then
                        obj%stem(i)%drydensity = zeros( obj%stem(i)%femdomain%ne() )
                    endif
                    if(size(obj%stem(i)%watercontent ) /=obj%stem(i)%femdomain%ne()  )then
                        obj%stem(i)%watercontent = zeros( obj%stem(i)%femdomain%ne() )
                    endif
                    
                    ! compute density from drydensity and water content
                    ! \rho_t = \rho_d * (1 - w )
                    !!$OMP parallel do private(j)
                    do j=1,obj%stem(i)%femdomain%ne()
                        obj%stem(i)%density(j) = obj%stem(i)%drydensity(j) * (1.0d0 - obj%stem(i)%watercontent(j))
                    enddo
                    !!$OMP end parallel do
                endif
            endif
        enddo
        !!$OMP end parallel do
        obj%property_deform_material_density = .true.
    else
        print *, "[Notice] setPropertiesSoybean >> no stems"
    endif
    !! root 
    if(allocated(obj%root) )then
        print *, "[ok] setPropertiesSoybean >> roots exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                ! exists
                ! check 
                !  (1) allocation of density(:)
                !  (2) size of density(:)
                !  if invalid, compute from drydensity(:)and watercontent(:)
                !  if not both do not exists, create all as 0.0
                if( allocated(obj%root(i)%density ))then
                    ! check size
                    
                    !if(size(obj%root(i)%density)/=obj%root(i)%femdomain%ne() )then
                        !print *, "[Caution] setPropertiesSoybean >> root("//str(i)//")%density >> "//&
                        !"size(obj%root(i)%density)/=obj%root(i)%femdomain%ne() >> reset by zero!!"
                        !deallocate(obj%root(i)%density)
                        ! let's go to next
                    !else
                        print *, "[ok] setPropertiesSoybean >> root("//str(i)//")%density >> allocated"
                        ! then ok. let's return
                        obj%property_deform_material_density = .true.
                        
                    !endif
                else
                ! density is not allocated.
                
                    obj%root(i)%density = zeros(obj%root(i)%femdomain%ne())
                
                    ! >> try to compute from drydensity(:) and watercontent(:)
                    ! >> check existatce of drydensity(:) and watercontent(:) 
                    if(.not.allocated(obj%root(i)%drydensity ) )then
                        obj%root(i)%drydensity = zeros( obj%root(i)%femdomain%ne() )
                    endif
                    if(.not.allocated(obj%root(i)%watercontent ) )then
                        obj%root(i)%watercontent = zeros( obj%root(i)%femdomain%ne() )
                    endif
                    
                    if(size(obj%root(i)%drydensity ) /=obj%root(i)%femdomain%ne()  )then
                        obj%root(i)%drydensity = zeros( obj%root(i)%femdomain%ne() )
                    endif
                    if(size(obj%root(i)%watercontent ) /=obj%root(i)%femdomain%ne()  )then
                        obj%root(i)%watercontent = zeros( obj%root(i)%femdomain%ne() )
                    endif
                    
                    ! compute density from drydensity and water content
                    ! \rho_t = \rho_d * (1 - w )
                    !!$OMP parallel do private(j)
                    do j=1,obj%root(i)%femdomain%ne()
                        obj%root(i)%density(j) = obj%root(i)%drydensity(j) * (1.0d0 - obj%root(i)%watercontent(j))
                    enddo
                    !!$OMP end parallel do
                endif
            endif
        enddo
        !!$OMP end parallel do
        obj%property_deform_material_density = .true.
    else
        print *, "[Notice] setPropertiesSoybean >> no roots"
    endif
    

end subroutine
! ##################################################################
subroutine setPropertiesYoungModulusSoybean(obj,default_value)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional, intent(in) :: default_value
    real(real64) :: defval
    integer(int32) :: i,j
    

    defval = input(default=0.0d0,option=default_value)

    if(allocated(obj%stem) )then
        print *, "[ok] setPropertiesYoungModulusSoybean >> stems exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if stem(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%stem(i)%youngmodulus ))then
                
                    print *, "[ok] setPropertiesYoungModulusSoybean >> stem("//str(i)//")%youngmodulus >> allocated"
                    ! then ok. let's return
                    obj%property_deform_material_youngmodulus = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%stem(i)%youngmodulus = zeros(obj%stem(i)%femdomain%ne())
                    obj%stem(i)%youngmodulus = defval
                endif
            endif
        enddo
    endif   
    
    ! same as this
    if(allocated(obj%root) )then
        print *, "[ok] setPropertiesYoungModulusSoybean >> roots exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if root(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%root(i)%youngmodulus ))then
                
                    print *, "[ok] setPropertiesYoungModulusSoybean >> root("//str(i)//")%youngmodulus >> allocated"
                    ! then ok. let's return
                    obj%property_deform_material_youngmodulus = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%root(i)%youngmodulus = zeros(obj%root(i)%femdomain%ne())
                    obj%root(i)%youngmodulus = defval
                endif
            endif
        enddo
    endif

    ! same for leaf
    if(allocated(obj%leaf) )then
        print *, "[ok] setPropertiesYoungModulusSoybean >> leafs exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if leaf(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%leaf(i)%youngmodulus ))then
            
                    print *, "[ok] setPropertiesYoungModulusSoybean &
                    >> leaf("//str(i)//")%youngmodulus >> allocated"
                    ! then ok. let's return
                    obj%property_deform_material_youngmodulus = .true.
                        
                else
                    ! youngmodulus is not allocated.
                    
                    obj%leaf(i)%youngmodulus = zeros(obj%leaf(i)%femdomain%ne())
                    obj%leaf(i)%youngmodulus = defval
                endif
            endif
        enddo
    endif
    obj%property_deform_material_youngmodulus = .true.

end subroutine
! ##################################################################

! ##################################################################
!same for poissonratio
subroutine setPropertiesPoissonRatioSoybean(obj,default_value)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional, intent(in) :: default_value
    real(real64) :: defval
    integer(int32) :: i,j
    

    defval = input(default=0.0d0,option=default_value)

    if(allocated(obj%stem) )then
        print *, "[ok] setPropertiesPoissonRatioSoybean >> stems exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if stem(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%stem(i)%poissonratio ))then
                
                    print *, "[ok] setPropertiesPoissonRatioSoybean >> stem("//str(i)//")%poissonratio >> allocated"
                    ! then ok. let's return
                    obj%property_deform_material_poissonratio = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%stem(i)%poissonratio = zeros(obj%stem(i)%femdomain%ne())
                    obj%stem(i)%poissonratio = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif   
    
    ! same for leaf
    if(allocated(obj%leaf) )then
        print *, "[ok] setPropertiesPoissonRatioSoybean >> leafs exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if leaf(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%leaf(i)%poissonratio ))then
            
                    print *, "[ok] setPropertiesPoissonRatioSoybean >> leaf("//str(i)//")%poissonratio >> allocated"
                    ! then ok. let's return
                    obj%property_deform_material_poissonratio = .true.
                        
                else
                    ! youngmodulus is not allocated.
                    
                    obj%leaf(i)%poissonratio = zeros(obj%leaf(i)%femdomain%ne())
                    obj%leaf(i)%poissonratio = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif

    ! same for root
    if(allocated(obj%root) )then
        print *, "[ok] setPropertiesPoissonRatioSoybean >> roots exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if root(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%root(i)%poissonratio ))then
                
                    print *, "[ok] setPropertiesPoissonRatioSoybean >> root("//str(i)//")%poissonratio >> allocated"
                    ! then ok. let's return
                    obj%property_deform_material_poissonratio = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%root(i)%poissonratio = zeros(obj%root(i)%femdomain%ne())
                    obj%root(i)%poissonratio = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif
    obj%property_deform_material_poissonratio = .true.
end subroutine

! ##################################################################
! similar subroutine for Initialdisplacement
subroutine setPropertiesInitialDisplacementSoybean(obj,default_value)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional, intent(in) :: default_value
    real(real64) :: defval
    integer(int32) :: i,j
    

    defval = input(default=0.0d0,option=default_value)

    if(allocated(obj%stem) )then
        print *, "[ok] setPropertiesInitialDisplacementSoybean >> stems exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if stem(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%stem(i)%Displacement ))then
                
                    print *, "[ok] setPropertiesInitialDisplacementSoybean >> &
                    stem("//str(i)//")%Displacement >> allocated"
                    ! then ok. let's return
                    obj%property_deform_initial_displacement = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%stem(i)%Displacement = zeros(obj%stem(i)%femdomain%nn(),3)
                    obj%stem(i)%Displacement = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif   
    
    ! same for leaf
    if(allocated(obj%leaf) )then
        print *, "[ok] setPropertiesInitialDisplacementSoybean >> leafs exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if leaf(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%leaf(i)%Displacement ))then
            
                    print *, "[ok] setPropertiesInitialDisplacementSoybean >> leaf("//str(i)//")%Displacement >> allocated"
                    ! then ok. let's return
                    obj%property_deform_initial_displacement = .true.
                        
                else
                    ! youngmodulus is not allocated.
                    
                    obj%leaf(i)%Displacement = zeros(obj%leaf(i)%femdomain%nn(),3)
                    obj%leaf(i)%Displacement = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif

    ! same for root
    if(allocated(obj%root) )then
        print *, "[ok] setPropertiesInitialDisplacementSoybean >> roots exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if root(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%root(i)%Displacement ))then
                
                    print *, "[ok] setPropertiesInitialDisplacementSoybean >> root("//str(i)//")%Displacement >> allocated"
                    ! then ok. let's return
                    obj%property_deform_initial_displacement = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%root(i)%Displacement = zeros(obj%root(i)%femdomain%nn(),3)
                    obj%root(i)%Displacement = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif
    obj%property_deform_initial_displacement = .true.

end subroutine

! same for initialstress but dimension = 3
subroutine setPropertiesInitialStressSoybean(obj,default_value)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional, intent(in) :: default_value
    real(real64) :: defval
    integer(int32) :: i,j
    

    defval = input(default=0.0d0,option=default_value)

    if(allocated(obj%stem) )then
        print *, "[ok] setPropertiesInitialStressSoybean >> stems exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if stem(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%stem(i)%stress ))then
                
                    print *, "[ok] setPropertiesInitialStressSoybean >> &
                    stem("//str(i)//")%stress >> allocated"
                    ! then ok. let's return
                    obj%property_deform_initial_stress = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%stem(i)%stress = zeros(obj%stem(i)%femdomain%ne(),obj%stem(i)%femdomain%nne(),6)
                    obj%stem(i)%stress = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif   
    ! same for leaf
    if(allocated(obj%leaf) )then
        print *, "[ok] setPropertiesInitialStressSoybean >> leafs exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if leaf(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%leaf(i)%stress ))then
            
                    print *, "[ok] setPropertiesInitialStressSoybean >> leaf("//str(i)//")%stress >> allocated"
                    ! then ok. let's return
                    obj%property_deform_initial_stress = .true.
                        
                else
                    ! youngmodulus is not allocated.
                    
                    obj%leaf(i)%stress = zeros(obj%leaf(i)%femdomain%ne(),obj%leaf(i)%femdomain%nne(),6)
                    obj%leaf(i)%stress = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif

    ! same for root
    if(allocated(obj%root) )then
        print *, "[ok] setPropertiesInitialStressSoybean >> roots exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if root(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%root(i)%stress ))then
                
                    print *, "[ok] setPropertiesInitialStressSoybean >> root("//str(i)//")%stress >> allocated"
                    ! then ok. let's return
                    obj%property_deform_initial_stress = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%root(i)%stress = zeros(obj%root(i)%femdomain%ne(),obj%root(i)%femdomain%nne(),6)
                    obj%root(i)%stress = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif
    obj%property_deform_initial_stress = .true.
end subroutine

! ################################################################################

! same as initdisplacement for BoundaryTractionForce
subroutine setPropertiesBoundaryTractionForceSoybean(obj,default_value,xrange,yrange,zrange)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional, intent(in) :: default_value,xrange(2),yrange(2),zrange(2)
    real(real64) :: defval
    integer(int32) :: i,j
    

    defval = input(default=0.0d0,option=default_value)

    if(allocated(obj%stem) )then
        print *, "[ok] setPropertiesBoundaryTractionForceSoybean >> stems exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if stem(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%stem(i)%BoundaryTractionForce ))then
                
                    print *, "[ok] setPropertiesBoundaryTractionForceSoybean >> &
                    stem("//str(i)//")%BoundaryTractionForce >> allocated"
                    ! then ok. let's return
                    obj%property_deform_boundary_tractionforce = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%stem(i)%BoundaryTractionForce = zeros(obj%stem(i)%femdomain%nn(),3)
                    obj%stem(i)%BoundaryTractionForce = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif   
    ! same for leaf
    if(allocated(obj%leaf) )then
        print *, "[ok] setPropertiesBoundaryTractionForceSoybean >> leafs exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if leaf(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%leaf(i)%BoundaryTractionForce ))then
            
                    print *, "[ok] setPropertiesBoundaryTractionForceSoybean &
                    >> leaf("//str(i)//")%BoundaryTractionForce >> allocated"
                    ! then ok. let's return
                    obj%property_deform_boundary_tractionforce = .true.
                        
                else
                    ! youngmodulus is not allocated.
                    
                    obj%leaf(i)%BoundaryTractionForce = zeros(obj%leaf(i)%femdomain%nn(),3)
                    obj%leaf(i)%BoundaryTractionForce = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif

    ! same for root
    if(allocated(obj%root) )then
        print *, "[ok] setPropertiesBoundaryTractionForceSoybean >> roots exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if root(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%root(i)%BoundaryTractionForce ))then
                
                    print *, "[ok] setPropertiesBoundaryTractionForceSoybean &
                    >> root("//str(i)//")%BoundaryTractionForce >> allocated"
                    ! then ok. let's return
                    obj%property_deform_boundary_tractionforce = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%root(i)%BoundaryTractionForce = zeros(obj%root(i)%femdomain%nn(),3)
                    obj%root(i)%BoundaryTractionForce = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif
    obj%property_deform_boundary_tractionforce = .true.
end subroutine
! ##################################################################

! ################################################################################

! same as initdisplacement for BoundaryTractionForce
subroutine setPropertiesBoundaryDisplacementSoybean(obj,default_value,xrange,yrange,zrange)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional, intent(in) :: default_value,xrange(2),yrange(2),zrange(2)
    real(real64) :: defval
    integer(int32) :: i,j
    

    defval = input(default=0.0d0,option=default_value)

    if(allocated(obj%stem) )then
        print *, "[ok] setPropertiesBoundaryDisplacementSoybean >> stems exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if stem(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%stem(i)%BoundaryDisplacement ))then
                
                    print *, "[ok] setPropertiesBoundaryDisplacementSoybean >> &
                    stem("//str(i)//")%BoundaryDisplacement >> allocated"
                    ! then ok. let's return
                    obj%property_deform_boundary_displacement = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%stem(i)%BoundaryDisplacement = zeros(obj%stem(i)%femdomain%nn(),3)
                    obj%stem(i)%BoundaryDisplacement = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif   
    ! same for leaf
    if(allocated(obj%leaf) )then
        print *, "[ok] setPropertiesBoundaryDisplacementSoybean >> leafs exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if leaf(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%leaf(i)%BoundaryDisplacement ))then
            
                    print *, "[ok] setPropertiesBoundaryDisplacementSoybean &
                    >> leaf("//str(i)//")%BoundaryDisplacement >> allocated"
                    ! then ok. let's return
                    obj%property_deform_boundary_Displacement = .true.
                        
                else
                    ! youngmodulus is not allocated.
                    
                    obj%leaf(i)%BoundaryDisplacement = zeros(obj%leaf(i)%femdomain%nn(),3)
                    obj%leaf(i)%BoundaryDisplacement = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif

    ! same for root
    if(allocated(obj%root) )then
        print *, "[ok] setPropertiesBoundaryDisplacementSoybean >> roots exist."
        ! leaf exists
        !!$OMP parallel do private(i)
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                ! allocate youngmoludus if root(i) exists and not allocated
                ! the default value is defval
                if( allocated(obj%root(i)%BoundaryDisplacement ))then
                
                    print *, "[ok] setPropertiesBoundaryDisplacementSoybean &
                    >> root("//str(i)//")%BoundaryDisplacement >> allocated"
                    ! then ok. let's return
                    obj%property_deform_boundary_displacement = .true.
                        
                else

                    ! youngmodulus is not allocated.
                    
                    obj%root(i)%BoundaryDisplacement = zeros(obj%root(i)%femdomain%nn(),3)
                    obj%root(i)%BoundaryDisplacement = defval
                endif
            endif
        enddo
        !!$OMP end parallel do
    endif
    obj%property_deform_boundary_Displacement = .true.
end subroutine
! ##################################################################

subroutine setPropertiesGravitySoybean(obj,default_value,xrange,yrange,zrange)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional, intent(in) :: default_value,xrange(2),yrange(2),zrange(2)
    real(real64) :: defval
    integer(int32) :: i,j
    

    defval = input(default=9.810d0,option=default_value)

    obj%Gravity_acceralation = defval

    obj%property_deform_gravity = .true.

end subroutine



! ##################################################################
subroutine setPropertiesSoybean(obj,density,YoungModulus,PoissonRatio,&
    InitialStress,InitialDisplacement,&
    BoundaryTractionForce,BoundaryDisplacement,Gravity,xr,yr,zr,&
    default_value)
    class(Soybean_),intent(inout) :: obj
    logical,optional,intent(in) :: density,YoungModulus,PoissonRatio,InitialStress,&
    InitialDisplacement,&
    BoundaryTractionForce,BoundaryDisplacement,Gravity
    real(real64),optional,intent(in) :: xr(2),yr(2),zr(2),default_value
    integer(int32) :: i, j
    
    ! set each conditions
    if(present(density) )then
        if(density)then
            call obj%setPropertiesDensity()
        endif
    endif

    if(present(YoungModulus) )then
        if(YoungModulus)then
            call obj%setPropertiesYoungModulus(default_value=default_value)
        endif
    endif


    if(present(PoissonRatio) )then
        if(PoissonRatio)then
            call obj%setPropertiesPoissonRatio(default_value=default_value)
        endif
    endif

    if(present(InitialDisplacement) )then
        if(InitialDisplacement)then
            call obj%setPropertiesInitialDisplacement(default_value=default_value)
        endif
    endif
    
    if(present(InitialStress) )then
        if(InitialStress)then
            call obj%setPropertiesInitialStress(default_value=default_value)
        endif
    endif
    

    if(present(BoundaryTractionForce) )then
        if(BoundaryTractionForce)then
            call obj%setPropertiesBoundaryTractionForce(default_value=default_value)
        endif
    endif


    if(present(BoundaryDisplacement) )then
        if(BoundaryDisplacement)then
            call obj%setPropertiesBoundaryDisplacement(default_value=default_value)
        endif
    endif


    if(present(Gravity) )then
        if(Gravity)then
            call obj%setPropertiesGravity(default_value=default_value)
        endif
    endif
end subroutine
! ##################################################################

function readyForSoybean(obj, Simulator) result(ready)
    class(Soybean_),intent(inout) :: obj
    integer(int32),intent(in) ::  Simulator
    logical :: ready
    ! default = ready!
    ! if all the properties are set, then ready = true
    if( Simulator == PF_DEFORMATION_ANALYSIS )then
        ready = .true.
        ready = ready .and. obj%property_deform_material_density
        ready = ready .and. obj%property_deform_material_YoungModulus
        ready = ready .and. obj%property_deform_material_PoissonRatio
        ready = ready .and. obj%property_deform_initial_Displacement
        ready = ready .and. obj%property_deform_initial_Stress
        ready = ready .and. obj%property_deform_boundary_TractionForce
        ready = ready .and. obj%property_deform_boundary_Displacement
        ready = ready .and. obj%property_deform_gravity
    else
        print *, "[ERROR] readyForSoybean >> invalid  Simulator type.", Simulator
    endif
end function
! ##################################################################

subroutine runSimulationSoybean(obj, Simulator,error_tolerance,debug,z_min)
    class(Soybean_),target,intent(inout) :: obj
    type(ContactMechanics_) :: contact
    type(FEMDomainp_),allocatable :: femdomainp(:)
    type(FEMDomain_),allocatable :: femdomains(:)
    
    type(Dictionary_) :: YoungModulusList
    type(Dictionary_) :: PoissonRatioList
    type(Dictionary_) :: DensityList
    real(real64),optional,intent(in) :: error_tolerance
    real(real64),intent(in) :: z_min

    logical,optional,intent(in) :: debug
    
    integer(int32),allocatable :: contactlist(:,:)
    integer(int32),intent(in) ::  Simulator
    integer(int32) :: i,j,k,i_offset, j_offset
    type(IO_)  :: f
    
    if( .not.obj%readyFor(Simulator)  )then
        call obj%checkProperties(Simulator=Simulator)
        print *, "[ERROR] :: runSimulationSoybean >> .not.obj%readyFor(Simulator) "
        return
    endif
    
    if( Simulator == PF_DEFORMATION_ANALYSIS )then
        ! run
        print *, "[ok] Running PF_DEFORMATION_ANALYSIS..."
        ! 全てのdomainのpointer
        allocate(femdomainp( obj%numleaf() + obj%numStem() + obj%numRoot()  ) )
        allocate(femdomains( obj%numleaf() + obj%numStem() + obj%numRoot()  ) )
        k = 0
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                k  = k+ 1
                femdomainp(k)%femdomainp => obj%stem(i)%femdomain
                femdomains(k) = obj%stem(i)%femdomain
            endif
        enddo
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                k  = k+ 1
                femdomainp(k)%femdomainp => obj%leaf(i)%femdomain
                femdomains(k) = obj%leaf(i)%femdomain
            endif
        enddo
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                k  = k+ 1
                femdomainp(k)%femdomainp => obj%root(i)%femdomain
                femdomains(k)  = obj%root(i)%femdomain
            endif
        enddo
        
        ! >>>>>>>>>>>>>>>>>>>>>>>
        ! connectivitylist
        ! >>>>>>>>>>>>>>>>>>>>>>>
        
        
        k = obj%numStem() + obj%numleaf() + obj%numRoot()
        contactlist = zeros(k, k)


        ! leaf to stem
        i_offset = obj%numStem()
        j_offset = 0
        !!!$OMP parallel do private(i,j)
        do i=1,obj%numleaf()
            do j=1, obj%numstem()
                if(obj%leaf2stem(i,j)/=0 )then
                    contactlist(i+i_offset, j+j_offset) = obj%leaf2stem(i,j)
                endif
            enddo
        enddo
        !!!$OMP end parallel do

        ! stem to stem
        i_offset = 0
        j_offset = 0
        !!!$OMP parallel do private(i,j)
        do i=1,obj%numstem()
            do j=1, obj%numstem()
                if(obj%stem2stem(i,j)/=0 )then
                    contactlist(i+i_offset, j+j_offset) = obj%stem2stem(i,j)
                endif
            enddo
        enddo
        !!!$OMP end parallel do

        ! root to stem
        i_offset = obj%numstem() + obj%numleaf()
        j_offset = 0
        !!!$OMP parallel do private(i,j)
        do i=1,obj%numroot()
            do j=1, obj%numstem()
                if(obj%root2stem(i,j)/=0 )then
                    contactlist(i+i_offset, j+j_offset) = obj%root2stem(i,j)
                endif
            enddo
        enddo
        !!!$OMP end parallel do

        ! root to root
        i_offset = obj%numstem() + obj%numroot()
        j_offset = obj%numstem() + obj%numroot()
        !!!$OMP parallel do private(i,j)
        do i=1,obj%numroot()
            do j=1, obj%numroot()
                if(obj%root2root(i,j)/=0 )then
                    contactlist(i+i_offset, j+j_offset) = obj%root2root(i,j)
                endif
            enddo
        enddo
        !!!$OMP end parallel do

        ! YoungModulusListを作る
        allocate(YoungModulusList%dictionary(obj%numleaf() + obj%numStem() + obj%numRoot()))
        k=0
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                k  = k+ 1
                YoungModulusList%dictionary(k)%realist = obj%stem(i)%YoungModulus
            endif
        enddo
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                k  = k+ 1
                YoungModulusList%dictionary(k)%realist = obj%leaf(i)%YoungModulus
            endif
        enddo
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                k  = k+ 1
                YoungModulusList%dictionary(k)%realist = obj%root(i)%YoungModulus
            endif
        enddo

        ! PoissonRatioListを作る
        allocate(PoissonRatioList%dictionary(obj%numstem() + obj%numleaf() + obj%numRoot()))
        k=0
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                k  = k+ 1
                PoissonRatioList%dictionary(k)%realist = obj%stem(i)%PoissonRatio
            endif
        enddo
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                k  = k+ 1
                PoissonRatioList%dictionary(k)%realist = obj%leaf(i)%PoissonRatio
            endif
        enddo
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                k  = k+ 1
                PoissonRatioList%dictionary(k)%realist = obj%root(i)%PoissonRatio
            endif
        enddo

        ! DensityListを作る
        allocate(DensityList%dictionary(obj%numStem() + obj%numLeaf() + obj%numRoot()))
        k=0
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                k  = k+ 1
                DensityList%dictionary(k)%realist = obj%stem(i)%Density
            endif
        enddo
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                k  = k+ 1
                DensityList%dictionary(k)%realist = obj%leaf(i)%Density
            endif
        enddo
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                k  = k+ 1
                DensityList%dictionary(k)%realist = obj%root(i)%Density
            endif
        enddo


        


        ! ContactMechanicsClassを呼ぶ
        call contact%init(femdomains=femdomains, contactlist=contactlist)

        print *, "[ok] Initialized simulator"

        ! 要修正(1) 材料パラメータをElement-wiseに導入する．
        ! Import material parameters
        ! Element-wise にする．

        contact%YoungModulusList = YoungModulusList
        contact%PoissonRatioList = PoissonRatioList
        contact%DensityList = DensityList
        contact%gravity = obj%Gravity_acceralation

        ! 
        call contact%setup(penaltyparameter=obj%PenaltyParameter,&
            GaussPointProjection=obj%GaussPointProjection)

        ! 要修正(2) 境界条件を課す節点のリスト+値から境界条件を導入．
        ! Boundary conditions
        
        ! fix displacement
        ! Listから選択
        print *, "[ok] set up done."
        call contact%fix(direction="x",disp= 0.0d0, DomainID=1,z_max=0.010d0)
        call contact%fix(direction="y",disp= 0.0d0, DomainID=1,z_max=0.010d0)
        call contact%fix(direction="z",disp= 0.0d0, DomainID=1,z_max=0.010d0)
        

        !do i=1,size(contact%femdomains)
            call contact%fix(direction="x",disp= -0.01d0, DomainID=5,z_min=0.30d0,z_max=0.410d0)
        !enddo

        ! traction forceを入れる．


        ! solve > get displacement
        !call f%open("debug.txt")
        !call f%write(contact%contactlist)
        !call f%close()
        !stop
        contact%solver%er0 = input(default=dble(1.0e-7),option=error_tolerance)
        if(present(debug) )then
            contact%solver%debug = debug
        endif
        call contact%solver%solve("BiCGSTAB")
        
        ! update mesh
        call contact%updateMesh()
        k = 0
        do i=1,size(obj%stem)
            if(obj%stem(i)%empty() )then
                cycle
            else
                k  = k+ 1
                obj%stem(i)%femdomain = femdomains(k) 
            endif
        enddo
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%empty() )then
                cycle
            else
                k  = k+ 1
                obj%leaf(i)%femdomain = femdomains(k)
            endif
        enddo
        do i=1,size(obj%root)
            if(obj%root(i)%empty() )then
                cycle
            else
                k  = k+ 1
                obj%root(i)%femdomain = femdomains(k)
            endif
        enddo
        

        ! 要修正(3) 変位から応力，等価節点力を計算
        


    else
        print *, "[ERROR] readyForSoybean >> invalid  Simulator type.", Simulator
    endif
end subroutine
! ##################################################################

pure function getPointsSoybean(obj,leaf,stem,root) result(points)
    class(Soybean_),intent(in) :: obj
    logical,optional,intent(in) :: leaf,stem,root
    real(real64),allocatable :: points(:,:),buf(:,:)
    logical :: count_leaf,count_stem,count_root
    integer(int32) :: i,n, id

    if(present(leaf) )then
        count_leaf = Leaf
    endif
    
    if(present(stem) )then
        count_stem = Stem
    endif

    if(present(root) )then
        count_root = Root
    endif
    
    n = obj%nn()
    points = zeros(n,3)

    id = 1
    !if(count_stem)then
        if(allocated(obj%stem) )then
            do i=1,size(obj%stem)
                if( .not. obj%stem(i)%femdomain%empty() )then
                    points(id:id + obj%stem(i)%femdomain%nn()-1  ,1:3) =&
                     obj%stem(i)%femdomain%mesh%nodcoord(1:obj%stem(i)%femdomain%nn(),1:3)
                    id = id + obj%stem(i)%femdomain%nn()
                endif
            enddo    
        endif
    !endif

    !if(count_leaf)then
        if(allocated(obj%leaf) )then
            do i=1,size(obj%leaf)
                if( .not. obj%leaf(i)%femdomain%empty() )then
                    points(id:id + obj%leaf(i)%femdomain%nn() -1 ,1:3) =&
                     obj%leaf(i)%femdomain%mesh%nodcoord(:,:)
                    id = id + obj%leaf(i)%femdomain%nn()
                endif
            enddo    
        endif
    !endif

    !if(count_root)then
        if(allocated(obj%root) )then
            do i=1,size(obj%root)
                if( .not. obj%root(i)%femdomain%empty() )then
                    points(id:id + obj%root(i)%femdomain%nn() -1 ,1:3) =&
                     obj%root(i)%femdomain%mesh%nodcoord(:,:)
                    id = id + obj%root(i)%femdomain%nn()
                endif
            enddo    
        endif
    !endif

    !if(id /=n)then
    !    buf = points
    !    points = zeros(id,3)
    !    points(1:id,:) = buf(1:id,:)
    !endif


end function
! ############################################################################

! ##################################################################

subroutine setPointsSoybean(obj,points)
    class(Soybean_),intent(inout) :: obj
    real(real64),intent(in) :: points(:,:)
    integer(int32) :: i,n, id
    
    if(size(points,1)/=obj%nn() )then
        print *, "[ERROR] setPointsSoybean >> Invalid size of arg points"
        print *, "size(points,1)/=obj%nn()",size(points,1),obj%nn()
        return
    endif

    id = 1
    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if( .not. obj%stem(i)%femdomain%empty() )then
                obj%stem(i)%femdomain%mesh%nodcoord(1:obj%stem(i)%femdomain%nn(),1:3) &
                =points(id:id + obj%stem(i)%femdomain%nn()-1  ,1:3)
                id = id + obj%stem(i)%femdomain%nn()
            endif
        enddo    
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if( .not. obj%leaf(i)%femdomain%empty() )then
                obj%leaf(i)%femdomain%mesh%nodcoord(:,:) &
                =points(id:id + obj%leaf(i)%femdomain%nn() -1 ,1:3)
                id = id + obj%leaf(i)%femdomain%nn()
            endif
        enddo    
    endif

    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if( .not. obj%root(i)%femdomain%empty() )then
                obj%root(i)%femdomain%mesh%nodcoord(:,:)&
                =points(id:id + obj%root(i)%femdomain%nn() -1 ,1:3) 
                
                id = id + obj%root(i)%femdomain%nn()
            endif
        enddo    
    endif


end subroutine
! ############################################################################

function getDistanceFromGroundSoybean(obj) result(distance_per_nodes)
    class(Soybean_),intent(inout) :: obj
    real(real64),allocatable :: distance_per_nodes(:),xA(:),xB(:),dist_per_stem(:),&
        dist_per_root(:)
    integer(int32),allocatable :: num_of_point(:)
    real(real64)   :: dist_AB,dist_add,dist_parent
    integer(int32) :: i,j,k,node_id, num_node,id,from_id, to_id,stem_id

    
    ! get distance from the intersection between root and stem
    node_id = 1

    
    ! get the intersection    
    num_node = obj%nn()
    
    distance_per_nodes = zeros(obj%nn() )
    dist_per_stem = zeros(size(obj%stem) )
    dist_per_root = zeros(size(obj%root) )

    num_of_point = obj%getNumberOfPoint()
    id = 0
    ! global search
    ! calculate distance from bottom to "A" node of each stem domains
    ! 1節目から順番に接いでいったと仮定する．

    !!$OMP parallel do private(i)
    do i=1, size(obj%stem)
        if(.not. obj%stem(i)%empty() )then
            dist_per_stem(i) = obj%getDistanceToGroundFromStemID(&
                dist_in = 0.0d0,&
                stem_id = i)
        endif
    enddo
    !!$OMP end parallel do


    ! comupte node-wise data from stem-wise data
    
    do i=1,size(num_of_point)
        if(i==1)then
            from_id = 1
            to_id   = num_of_point(1)
        else
            from_id = sum(num_of_point(1:i-1))+1
            to_id   = sum(num_of_point(1:i  ))
        endif

        distance_per_nodes(from_id:to_id) = &
        distance_per_nodes(from_id:to_id)   &
        + dist_per_stem(i)

    enddo

    do i=obj%numStem()+1,obj%numStem()+obj%numLeaf()
        
        if(i==1)then
            from_id = 1
            to_id   = num_of_point(1)
        else
            from_id = sum(num_of_point(1:i-1))+1
            to_id   = sum(num_of_point(1:i  ))
        endif

        do j=1,size(obj%leaf2stem,1)
            if(obj%leaf2stem( i-obj%numStem(), j )/=0)then
                stem_id = j
                exit
            endif
        enddo

        distance_per_nodes(from_id:to_id) = &
        distance_per_nodes(from_id:to_id)   &
        + dist_per_stem(stem_id)

    enddo

    ! 1節目から順番に接いでいったと仮定する．
    !!$OMP parallel do private(i)
    do i=1, size(obj%Root)
        if(.not. obj%Root(i)%empty() )then
            dist_per_Root(i) = obj%getDistanceToGroundFromRootID(&
                dist_in = 0.0d0,&
                Root_id = i)
        endif
    enddo
    !!$OMP end parallel do


    ! comupte node-wise data from root-wise data
    do i=obj%numstem()+obj%numleaf()+1,obj%numstem()+obj%numleaf()+obj%numRoot()
        if(i==1)then
            from_id = 1
            to_id   = num_of_point(1)
        else
            from_id = sum(num_of_point(1:i-1))+1
            to_id   = sum(num_of_point(1:i  ))
        endif

        distance_per_nodes(from_id:to_id) = &
        distance_per_nodes(from_id:to_id)   &
        + dist_per_root(i-obj%numstem()-obj%numleaf())

    enddo

    ! node-to-node
    ! @stem
    id = 0
    do i=1,size(obj%stem)
        if( .not. obj%stem(i)%femdomain%empty()  )then
            id = id + 1
            do j=1,obj%stem(i)%femdomain%nn()
                xA = obj%stem(i)%getCoordinate("A")
                xB = obj%stem(i)%femdomain%mesh%nodcoord(j,:)
                if(id==1 .and. i==1)then
                    node_id = j
                else
                    node_id = sum( num_of_point(1:id) ) + j
                endif
                distance_per_nodes(node_id) = distance_per_nodes(node_id) &
                    + norm(xA-xB)
            enddo
        endif
    enddo
    ! for each domain
    ! @leaf
    id = obj%numStem() - 1
    do i=1,size(obj%leaf)
        if( .not. obj%leaf(i)%femdomain%empty()  )then
            id = id + 1
            do j=1,obj%leaf(i)%femdomain%nn()
                xA = obj%leaf(i)%getCoordinate("A")
                xB = obj%leaf(i)%femdomain%mesh%nodcoord(j,:)
                if(id==1 .and. i==1)then
                    node_id = j
                else
                    node_id = sum( num_of_point(1:id) ) + j
                endif
                distance_per_nodes(node_id) = distance_per_nodes(node_id) &
                    + norm(xA-xB)
            enddo
        endif
    enddo

    
    ! for each domain
    ! @root
    id = obj%numStem() + obj%numLeaf() -1
    do i=1,size(obj%root)
        if( .not. obj%root(i)%femdomain%empty()  )then
            id = id + 1
            do j=1,obj%root(i)%femdomain%nn()
                xA = obj%root(i)%getCoordinate("A")
                xB = obj%root(i)%femdomain%mesh%nodcoord(j,:)
                if(id==1 .and. i==1)then
                    node_id = j
                else
                    node_id = sum( num_of_point(1:id) ) + j
                endif
                distance_per_nodes(node_id) = distance_per_nodes(node_id) &
                    + norm(xA-xB)
            enddo
        endif
    enddo
    


end function
! ############################################################################


! ############################################################################
function getNumberOfPointSoybean(obj) result(NumberOfPoint)
    class(Soybean_),intent(in) :: obj
    integer(int32), allocatable :: NumberOfPoint(:)
    integer(int32) :: i,id
    ! order :: stem -> leaf -> root

    NumberOfPoint = zeros(obj%numStem()+obj%numLeaf()+obj%numRoot()  )
    id = 1
    do i=1,size(obj%stem)
        if(.not. obj%stem(i)%empty() )then
            NumberOfPoint(id) = obj%stem(i)%femdomain%nn()
            id = id + 1
        endif
    enddo

    do i=1,size(obj%leaf)
        if(.not. obj%leaf(i)%empty() )then
            NumberOfPoint(id) = obj%leaf(i)%femdomain%nn()
            id = id + 1
        endif
    enddo

    do i=1,size(obj%root)
        if(.not. obj%root(i)%empty() )then
            NumberOfPoint(id) = obj%root(i)%femdomain%nn()
            id = id + 1
        endif
    enddo
    
end function
! ############################################################################


! ############################################################################
function getNumberOfElementSoybean(obj) result(NumberOfElement)
    class(Soybean_),intent(in) :: obj
    integer(int32), allocatable :: NumberOfElement(:)
    integer(int32) :: i,id
    ! order :: stem -> leaf -> root

    NumberOfElement = zeros(obj%numStem()+obj%numLeaf()+obj%numRoot()  )
    id = 1
    do i=1,size(obj%stem)
        if(.not. obj%stem(i)%empty() )then
            NumberOfElement(id) = obj%stem(i)%femdomain%ne()
            id = id + 1
        endif
    enddo

    do i=1,size(obj%leaf)
        if(.not. obj%leaf(i)%empty() )then
            NumberOfElement(id) = obj%leaf(i)%femdomain%ne()
            id = id + 1
        endif
    enddo

    do i=1,size(obj%root)
        if(.not. obj%root(i)%empty() )then
            NumberOfElement(id) = obj%root(i)%femdomain%ne()
            id = id + 1
        endif
    enddo
    
end function
! ############################################################################



! ############################################################################
recursive function getDistanceToGroundFromStemIDSoybean(obj,dist_in,stem_id) result(dist_ground)
    class(Soybean_),intent(in) :: obj
    real(real64),intent(in)    :: dist_in
    integer(int32),intent(in)  :: stem_id
    integer(int32) :: j, parent_id
    real(real64) :: dist_ground
    real(real64) :: dist_AB,dist_old
    real(real64),allocatable :: xA(:),xB(:)

    ! check stem-to-stem connectivity
    dist_ground = dist_in
    if( maxval(obj%stem2stem(stem_id,:))/=1 )then
        return
    endif

    do j=1,size(obj%stem2stem,1)
        
        if(obj%stem2stem(stem_id,j)==1)then
            if(.not. obj%stem(j)%femdomain%empty() )then
                ! found parent
                ! number of parent node should be 1
                parent_id = j
                xA = obj%stem(parent_id)%getCoordinate("A")
                xB = obj%stem(parent_id)%getCoordinate("B")
                dist_AB = sqrt(dot_product(xA-xB,xA-xB))
                dist_old = dist_in + dist_AB
                
                dist_ground = obj%getDistanceToGroundFromStemID(&
                    dist_in = dist_old,&
                    stem_id = parent_id  )
                return
            endif
        endif

    enddo

    

end function
! ############################################################################


! ############################################################################
recursive function getDistanceToGroundFromRootIDSoybean(obj,dist_in,root_id) result(dist_ground)
    class(Soybean_),intent(in) :: obj
    real(real64),intent(in)    :: dist_in
    integer(int32),intent(in)  :: root_id
    integer(int32) :: j, parent_id
    real(real64) :: dist_ground
    real(real64) :: dist_AB,dist_old
    real(real64),allocatable :: xA(:),xB(:)

    ! check root-to-root connectivity
    dist_ground = dist_in
    if( maxval(obj%root2root(root_id,:))/=1 )then
        return
    endif

    do j=1,size(obj%root2root,1)
        
        if(obj%root2root(root_id,j)==1)then
            if(.not. obj%root(j)%femdomain%empty() )then
                ! found parent
                ! number of parent node should be 1
                parent_id = j
                xA = obj%root(parent_id)%getCoordinate("A")
                xB = obj%root(parent_id)%getCoordinate("B")
                dist_AB = sqrt(dot_product(xA-xB,xA-xB))
                dist_old = dist_in + dist_AB
                
                dist_ground = obj%getDistanceToGroundFromrootID(&
                    dist_in = dist_old,&
                    root_id = parent_id  )
                return
            endif
        endif

    enddo

    

end function
! ############################################################################


! ############################################################################
function getRangeOfNodeIDSoybean(obj,stem,leaf,root) result(id_range)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: id_range(2),numStemNode,numLeafNode,numRootNode,i
    logical,optional,intent(in) :: stem,leaf,root

    id_range(1:2)=[0,0]


    numStemNode = 0
    do i=1,size(obj%stem)
        if(.not.obj%stem(i)%femdomain%empty() )then
            numStemNode = numStemNode + obj%stem(i)%femdomain%nn()
        endif
    enddo

    if(present(stem) )then
        if(stem)then
            id_range(1) = 1
            id_range(2) = numStemNode
            return
        endif
    endif

    numLeafNode = 0
    do i=1,size(obj%Leaf)
        if(.not.obj%Leaf(i)%femdomain%empty() )then
            numLeafNode = numLeafNode + obj%Leaf(i)%femdomain%nn()
        endif
    enddo

    if(present(leaf) )then
        if(leaf)then
            id_range = [  numStemNode +1,  numStemNode + numLeafNode   ]
            return
        endif
    endif

    numRootNode = 0
    do i=1,size(obj%Root)
        if(.not.obj%Root(i)%femdomain%empty() )then
            numRootNode = numRootNode + obj%Root(i)%femdomain%nn()
        endif
    enddo

    if(present(root) )then
        if(root)then
            id_range = [  numStemNode+numLeafNode+1,  &
            numStemNode + numLeafNode +numRootNode  ]
            return
        endif
    endif

end function
! ############################################################################


! ############################################################################
function getPPFDSoybean(obj,light,Transparency,Resolution,num_threads,leaf)  result(ppfd)
    class(Soybean_),intent(inout) :: obj 
    type(Light_),intent(in)    :: light
    real(real64),optional,intent(in) :: Transparency,Resolution
    integer(int32),optional,intent(in) :: num_threads
    ! leaf of other plants
    type(Leaf_),optional,intent(inout) :: leaf(:)

    real(real64),allocatable :: ppfd(:), NumberOfElement(:), NumberOfPoint(:)
    real(real64),allocatable :: leaf_pass_num(:),nodcoord(:,:),radius_vec(:),elem_cosins(:)
    real(real64) ::thickness,center_x(3),xmin(3),xmax(3),radius,radius_tr,coord(3),Transparency_val
    real(real64) :: zmin
    integer(int32) :: from, to, i,n,j,k,l,element_id
    
    logical :: inside, upside


    ! compute cosin
    !elem_cosins = obj%getLeafCosValue(light)

    ! rotate soybean
    call obj%rotate(z=radian(180.0d0 - light%angles(1)))
    call obj%rotate(x=radian(90.0d0 - light%angles(2)))
    if(present(leaf) )then
        do i=1,size(leaf)
            call leaf(i)%femdomain%rotate(z=radian(180.0d0 - light%angles(1)))
            call leaf(i)%femdomain%rotate(x=radian(90.0d0 - light%angles(2)))
        enddo
    endif
    !本当にあってる？？
    ! after this, rotate this back again
    
    

    radius = input(default=0.0050d0,option=Resolution)
    Transparency_val = input(default=0.30d0,option=Transparency)
    
    ! ppfdが通過した葉の積算長さで減衰するモデル
    NumberOfElement = obj%getNumberOfElement()
    ppfd = zeros(obj%ne() ) 
    elem_cosins = zeros(obj%ne() ) 
    i = sum(NumberOfElement(1:obj%numStem())+1)
    j = sum(NumberOfElement(1:obj%numStem() + obj%numLeaf()))
    ppfd( i:j ) = light%maxPPFD
    leaf_pass_num = zeros(obj%ne() )

    n = sum(NumberOfElement(1:obj%numStem()))
    from = n
    if(present(num_threads) )then
        call omp_set_num_threads(num_threads)
    endif


    ! leaf of other plants
    if(present(leaf) )then
        !$OMP parallel do default(shared), private(j,k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
        do i=1, size(obj%leaf)
            if(.not. obj%leaf(i)%femdomain%empty() )then
                !print *, i, "/", obj%numLeaf()
                !$OMP parallel do default(shared), private(k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
                do j=1,obj%leaf(i)%femdomain%ne()
                    ! 中心座標
                    center_x = obj%leaf(i)%femdomain%centerPosition(ElementID=j)
                    ! 枚数のみカウント
                    ! 1枚あたりthicknessだけ距離加算

                    !$OMP parallel do default(shared), private(inside,upside,radius_vec,radius_tr,zmin,n,element_id)
                    do k=1,size(leaf)
                        if(.not. leaf(k)%femdomain%empty() )then
                            
                            inside=.false.
                            radius_vec = zeros(leaf(k)%femdomain%nn() )
                            radius_vec =(leaf(k)%femdomain%mesh%nodcoord(:,1)-center_x(1))**2 &
                            + (leaf(k)%femdomain%mesh%nodcoord(:,2)-center_x(2))**2
                        
                            radius_tr = minval(radius_vec)

                            if(radius_tr < radius*radius)then
                                zmin = leaf(k)%femdomain%mesh%nodcoord(minvalID(radius_vec),3)
                                inside=.true.
                            endif
                            !あるいは，zmin,xmax,ymin,ymaxの正負で場合分けできるのでは？
                            !>>なぜか失敗
                            upside = (center_x(3) < zmin )
                            if(inside .eqv. .true.)then
                                
                                if(upside .eqv. .true.)then
                                    !print *, center_x(3) , zmin  
                                          
                                    n = obj%numStem() + (i-1)
                                    
                                    element_id = sum(NumberOfElement(1:n)) + j
                                    leaf_pass_num(element_id) = leaf_pass_num(element_id) + 1.0d0
                                endif
                            endif
                        endif
                    enddo
                    !$OMP end parallel do    
                enddo
                !$OMP end parallel do
            endif
        enddo
        !$OMP end parallel do
    endif



    !$OMP parallel do default(shared), private(j,k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
    do i=1, size(obj%leaf)
        if(.not. obj%leaf(i)%femdomain%empty() )then
            !print *, i, "/", obj%numLeaf()
            !$OMP parallel do default(shared), private(k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
            do j=1,obj%leaf(i)%femdomain%ne()
                ! 中心座標
                center_x = obj%leaf(i)%femdomain%centerPosition(ElementID=j)
                
                
                ! 枚数のみカウント
                ! 1枚あたりthicknessだけ距離加算
                !$OMP parallel do default(shared), private(inside,upside,radius_vec,radius_tr,zmin,n,element_id)
                do k=1,size(obj%leaf)

                    if(i==k) cycle
                    if(.not. obj%leaf(k)%femdomain%empty() )then
                        
                        inside=.false.
                        radius_vec = zeros(obj%leaf(k)%femdomain%nn() )
                        radius_vec =(obj%leaf(k)%femdomain%mesh%nodcoord(:,1)-center_x(1))**2 &
                        + (obj%leaf(k)%femdomain%mesh%nodcoord(:,2)-center_x(2))**2
                        
                        radius_tr = minval(radius_vec)
                        if(radius_tr < radius*radius)then
                            zmin = obj%leaf(k)%femdomain%mesh%nodcoord(minvalID(radius_vec),3)
                            inside=.true.
                        endif
                        !あるいは，zmin,xmax,ymin,ymaxの正負で場合分けできるのでは？
                        !>>なぜか失敗
                        upside = (center_x(3) < zmin )
                        if(inside .eqv. .true.)then
                            if(upside .eqv. .true.)then
                                if(inside .eqv. .false.)then
                                    cycle
                                endif
                                if(upside .eqv. .false.)then
                                    cycle
                                endif
                                n = obj%numStem() + (i-1)
                                element_id = sum(NumberOfElement(1:n)) + j
                                leaf_pass_num(element_id) = leaf_pass_num(element_id) + 1.0d0
                                
                            endif
                        endif
                    endif
                enddo
                !$OMP end parallel do    
            enddo
            !$OMP end parallel do
        endif
    enddo
    !$OMP end parallel do

    
    !ppfd = ppfd*reduction*cosin-value
    ppfd(:) = ppfd(:) * Transparency_val** leaf_pass_num(:)
    !ppfd(:) = ppfd(:)*elem_cosins(:)
    

    
    ! get back
    call obj%rotate(x=-radian(90.0d0 - light%angles(2)))
    call obj%rotate(z=-radian(180.0d0 - light%angles(1)))
    if(present(leaf) )then
        do i=1,size(leaf)
            call leaf(i)%femdomain%rotate(x=-radian(90.0d0 - light%angles(2)))
            call leaf(i)%femdomain%rotate(z=-radian(180.0d0 - light%angles(1)))
        enddo
    endif
    

end function
! ############################################################################

! ############################################################################
function getLeafCosValueSoybean(obj,light,num_threads)  result(elem_cosins)
    class(Soybean_),intent(inout) :: obj 
    type(Light_),intent(in)    :: light
    integer(int32),optional,intent(in) :: num_threads

    real(real64),allocatable :: cosin_value(:), NumberOfElement(:), NumberOfPoint(:)
    real(real64),allocatable :: leaf_pass_num(:),nodcoord(:,:),radius_vec(:),elem_cosins(:),&
        N_Light(:),N_Leaf(:)
    real(real64) ::thickness,center_x(3),xmin(3),xmax(3),radius,radius_tr,coord(3),Transparency_val
    real(real64) :: zmin
    integer(int32) :: from, to, i,n,j,k,l,element_id
    
    logical :: inside, upside

    ! rotate soybean

    call obj%rotate(z=radian(180.0d0 - light%angles(1)))
    call obj%rotate(x=radian(90.0d0 - light%angles(2)))

    NumberOfElement = obj%getNumberOfElement()
    
    elem_cosins = zeros(obj%ne() ) 
    

    if(present(num_threads) )then
        call omp_set_num_threads(num_threads)
    endif


    !$OMP parallel do default(shared), private(j,k,n,element_id,N_leaf,N_light)
    do i=1, size(obj%leaf)
        if(.not. obj%leaf(i)%femdomain%empty() )then
            !print *, i, "/", obj%numLeaf()
            !$OMP parallel do default(shared), private(k,n,element_id,N_leaf,N_light)
            do j=1,obj%leaf(i)%femdomain%ne()
                ! cosin rule
                n = obj%numStem() + (i-1)                    
                element_id = sum(NumberOfElement(1:n)) + j
                N_leaf = obj%leaf(i)%getNormalVector(ElementID=j)
                N_light = [0.0d0,0.0d0,1.0d0]
                elem_cosins(element_id) = dble(dot_product(N_light,N_Leaf))
            enddo
            !$OMP end parallel do
        endif
    enddo
    !$OMP end parallel do

    ! get back
    call obj%rotate(x=-radian(90.0d0 - light%angles(2)))
    call obj%rotate(z=-radian(180.0d0 - light%angles(1)))
end function
! ############################################################################
function getPhotoSynthesisSoybean(obj,light,air,dt,Transparency,Resolution,ppfd)  result(photosynthesis)
    class(Soybean_),intent(inout) :: obj 
    type(Light_),intent(in)    :: light
    type(Air_),intent(in)    :: Air
    real(real64),intent(in) :: dt
    real(real64),optional,intent(in) :: Transparency,Resolution,ppfd(:)
    real(real64),allocatable :: photosynthesis(:)
    
    integer(int32),allocatable :: NumberOfElement(:)
    integer(int32) :: i,j,offset,elem_id
    
    photosynthesis = zeros(obj%ne() )
    
    NumberOfElement = obj%getNumberOfElement()
    offset = sum(NumberOfElement(1:obj%numStem() ) )

    ! before photosynthesis
    elem_id = offset
    do i=1,size(obj%leaf)
        if( .not. obj%leaf(i)%femdomain%empty()  )    then
            do j=1,obj%leaf(i)%femdomain%ne()
                elem_id = elem_id + 1
                photosynthesis(elem_id) = obj%leaf(i)%source(j)
            enddo
        endif
    enddo

    if(.not. present(ppfd) )then
        
        call obj%laytracing(light=light,Transparency=Transparency,Resolution=Resolution)

        ! 光合成量を計算
        do i=1,size(obj%Leaf)
            if(obj%Leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%leaf(i)%photosynthesis(dt=dt,air=air)
            endif
        enddo
    else
        elem_id = offset
    
        do i=1,size(obj%leaf)
            if( .not. obj%leaf(i)%femdomain%empty()  )    then
                do j=1,obj%leaf(i)%femdomain%ne()
                    elem_id = elem_id + 1
                    obj%leaf(i)%ppfd(j) = ppfd(elem_id) !- photosynthesis(elem_id)
                enddo
            endif
        enddo
    
    endif
    
    elem_id = offset
    do i=1,size(obj%leaf)
        if( .not. obj%leaf(i)%femdomain%empty()  )    then
            do j=1,obj%leaf(i)%femdomain%ne()
                elem_id = elem_id + 1
                photosynthesis(elem_id) = obj%leaf(i)%source(j) - photosynthesis(elem_id)
            enddo
        endif
    enddo


    
end function
! ############################################################################

! ############################################################################
function getPhotoSynthesisSpeedPerVolumeSoybean(obj,light,air,dt,Transparency,Resolution,ppfd)  result(photosynthesis)
    class(Soybean_),intent(inout) :: obj 
    type(Light_),intent(in)    :: light
    type(Air_),intent(in)    :: Air
    real(real64),intent(in) :: dt
    real(real64),optional,intent(in) :: Transparency,Resolution,ppfd(:)
    real(real64),allocatable :: photosynthesis(:),Speed_PV(:)
    
    integer(int32),allocatable :: NumberOfElement(:)
    integer(int32) :: i,j,offset,elem_id
    
    photosynthesis = zeros(obj%ne() )
    
    NumberOfElement = obj%getNumberOfElement()
    offset = sum(NumberOfElement(1:obj%numStem() ) )


    if(.not. present(ppfd) )then
        
        call obj%laytracing(light=light,Transparency=Transparency,Resolution=Resolution)

    else
        elem_id = offset
    
        do i=1,size(obj%leaf)
            if( .not. obj%leaf(i)%femdomain%empty()  )    then
                do j=1,obj%leaf(i)%femdomain%ne()
                    elem_id = elem_id + 1
                    obj%leaf(i)%ppfd(j) = ppfd(elem_id) !- photosynthesis(elem_id)
                enddo
            endif
        enddo
    
    endif
    
    elem_id = offset
    do i=1,size(obj%leaf)
        if( .not. obj%leaf(i)%femdomain%empty()  )    then
            Speed_PV = obj%leaf(i)%getPhotoSynthesisSpeedPerVolume(dt=dt,air=air)
            do j=1,obj%leaf(i)%femdomain%ne()
                elem_id = elem_id + 1
                photosynthesis(elem_id) = Speed_PV(j)
            enddo
        endif
    enddo


    
end function
! ############################################################################

subroutine fixReversedElementsSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    integer(int32) :: i,j
    real(Real64) :: v
    
    
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%empty() ) cycle

        do j=1,obj%stem(i)%femdomain%ne()
            v = obj%stem(i)%femdomain%getvolume(elem=j)
            if(v<=0)then
                call obj%stem(i)%femdomain%fixReversedElements()
                if( obj%stem(i)%femdomain%getvolume(elem=j) < 0.0d0 )then
                    print *, "[ERROR] >> fixReversedElementsSoybean >> not fixed"
                    stop
                endif
                exit    
            endif
        enddo

    enddo

    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%empty() ) cycle

        do j=1,obj%Leaf(i)%femdomain%ne()
            v = obj%Leaf(i)%femdomain%getvolume(elem=j)
            if(v<=0)then
                call obj%Leaf(i)%femdomain%fixReversedElements()
                if( obj%Leaf(i)%femdomain%getvolume(elem=j) < 0.0d0 )then
                    print *, "[ERROR] >> fixReversedElementsSoybean >> not fixed"
                    stop
                endif
                exit    
            endif
        enddo

    enddo


    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%empty() ) cycle

        do j=1,obj%root(i)%femdomain%ne()
            v = obj%root(i)%femdomain%getvolume(elem=j)
            if(v<=0)then
                call obj%root(i)%femdomain%fixReversedElements()
                if( obj%root(i)%femdomain%getvolume(elem=j) < 0.0d0 )then
                    print *, "[ERROR] >> fixReversedElementsSoybean >> not fixed"
                    stop
                endif
                exit    
            endif
        enddo

    enddo

end subroutine
! ################################################################
function convertDataFormatSoybean(obj,scalar,new_format) result(ret)
    class(Soybean_),intent(in) :: obj
    real(real64),intent(in) :: scalar(:)
    integer(int32),intent(in) :: new_format
    real(real64),allocatable :: ret(:)
    integer(int32),allocatable :: NumberOfElement(:),NumberOfPoint(:)
    integer(int32) :: i,k,j,n
    logical :: ELEMENT_WISE,POINT_WISE
    
    NumberOfPoint = obj%getNumberOfPoint()
    NumberOfElement = obj%getNumberOfElement()
    
    POINT_WISE  = .false.
    ELEMENT_WISE = .false.
    if(sum(NumberOfPoint)==size(scalar) )then
        POINT_WISE = .true.
    elseif( sum(NumberOfElement)==size(scalar) )then
        ELEMENT_WISE = .true.
    else
        print *, "[ERROR] convertDataFormatSoybean >> "
        print *, "Invalid vector size",size(scalar)
        stop
    endif

    if(new_format==PF_SOY_OBJECT_WISE)then
        ! for each root/stem/soil object
        ret = zeros(obj%numStem()+obj%numLeaf()+obj%numRoot() )
        
        if(POINT_WISE)then
            n = 0
            k = 0
            do i=1,obj%numStem()
                k=k+1
                do j=1,obj%stem(i)%femdomain%nn()
                    n=n+1
                    ret(k)=ret(k)+scalar(n)    
                enddo
            enddo
            
            do i=1,obj%numLeaf()
                k=k+1
                do j=1,obj%Leaf(i)%femdomain%nn()
                    n=n+1
                    ret(k)=ret(k)+scalar(n)    
                enddo
            enddo

            do i=1,obj%numRoot()
                k=k+1
                do j=1,obj%Root(i)%femdomain%nn()
                    n=n+1
                    ret(k)=ret(k)+scalar(n)    
                enddo
            enddo

        elseif(ELEMENT_WISE)then
            n = 0
            k = 0
            do i=1,obj%numStem()
                k=k+1
                do j=1,obj%stem(i)%femdomain%ne()
                    n=n+1
                    ret(k)=ret(k)+scalar(n)    
                enddo
            enddo
            
            do i=1,obj%numLeaf()
                k=k+1
                do j=1,obj%Leaf(i)%femdomain%ne()
                    n=n+1
                    ret(k)=ret(k)+scalar(n)    
                enddo
            enddo

            do i=1,obj%numRoot()
                k=k+1
                do j=1,obj%Root(i)%femdomain%ne()
                    n=n+1
                    ret(k)=ret(k)+scalar(n)    
                enddo
            enddo
        endif

    endif
end function
! ################################################################
function getLeafAreaSoybean(obj) result(LeafArea)
    class(Soybean_),intent(in) :: obj
    real(real64) :: LeafArea
    integer(int32) :: i

    LeafArea = 0.0d0
    do i=1,obj%numLeaf()
        LeafArea = LeafArea + obj%leaf(i)%getLeafArea()    
    enddo

end function
! ################################################################

function getIntersectLeafSoybean(obj,soybeans,light,except)  result(Leaf)
    class(Soybean_),intent(inout) :: obj
    type(Soybean_),intent(inout) :: soybeans(:)
    type(Light_),optional,intent(in) :: light ! default is z+ direction
    type(Leaf_),allocatable :: leaf(:)
    real(real64),allocatable :: points(:,:), mypoints(:,:)
    
    integer(int32),optional,intent(in) :: except
    real(real64) :: obj_radius,obj_center(3)
    real(real64) :: chk_radius,chk_center(3),dist_2
    integer(int32) :: i,j,k,num_leaf
    logical,allocatable :: overset(:),overset_leaf(:)
    ! search Intersect leaf
    ! considering light position
    if(present(light))then
        call obj%rotate(z=radian(180.0d0 - light%angles(1)))
        call obj%rotate(x=radian(90.0d0 - light%angles(2)))
        if(present(except) )then
            do i=1,size(soybeans)
                if(i==except)cycle
                call soybeans(i)%rotate(z=radian(180.0d0 - light%angles(1)))
                call soybeans(i)%rotate(x=radian(90.0d0 - light%angles(2)))
            enddo
        else
            do i=1,size(soybeans)
                call soybeans(i)%rotate(z=radian(180.0d0 - light%angles(1)))
                call soybeans(i)%rotate(x=radian(90.0d0 - light%angles(2)))
            enddo
        endif

    endif

    obj_radius = obj%getRadius()
    obj_center = obj%getCenter()


    ! search overlaped soybeans
    allocate(overset( size(soybeans) ) )
    overset(:) = .false.
    do i=1,size(soybeans)
        if(present(except) )then
            if(i==except) cycle
        endif
        chk_radius = soybeans(i)%getRadius()
        chk_Center = soybeans(i)%getCenter()
        dist_2 = norm( obj_center(1:2) - chk_center(1:2) )
        if(dist_2 <= chk_radius + obj_radius )then
            ! added 2022/1/29, trial
            points=soybeans(i)%getPoints(leaf=.true.,stem=.false.,root=.false.)
            mypoints=obj%getPoints(leaf=.true.,stem=.false.,root=.false.)
            ! if a soybean is above mysoy, count
            ! added 2022/1/29, trial
            if(minval(points(:,3)) >= maxval(mypoints(:,3) ))then
                overset(i) = .true.
            endif
        else
            cycle
        endif
    enddo

    ! count number of leaf
    num_leaf = 0
    do i=1,size(soybeans)
        if(present(except) )then
            if(i==except) cycle
        endif
        if(overset(i) )then
            !allocate(overset_leaf(size(soybeans(i)%leaf ) ))
            !overset_leaf(:) = .false.
            do j=1,size(soybeans(i)%leaf )
                if(soybeans(i)%leaf(j)%femdomain%empty() ) cycle
                chk_radius = soybeans(i)%leaf(j)%getRadius()
                chk_Center = soybeans(i)%leaf(j)%getCenter()

                dist_2 = norm( obj_center(1:2) - chk_center(1:2) )
                if(dist_2 <= chk_radius + obj_radius )then
                    !overset_leaf(k) = .true.
                    num_leaf = num_leaf + 1
                else
                    cycle
                endif
            enddo   
        endif
    enddo

    allocate(leaf(num_leaf) )
    num_leaf = 0
    do i=1,size(soybeans)
        if(present(except) )then
            if(i==except) cycle
        endif
        if(overset(i) )then
            do j=1,size(soybeans(i)%leaf )
                if(soybeans(i)%leaf(j)%femdomain%empty() ) cycle
                
                chk_radius = soybeans(i)%leaf(j)%getRadius()
                chk_Center = soybeans(i)%leaf(j)%getCenter()

                dist_2 = norm( obj_center(1:2) - chk_center(1:2) )
                if(dist_2 <= chk_radius + obj_radius )then
                    num_leaf = num_leaf + 1
                    leaf(num_leaf) = soybeans(i)%leaf(j)
                else
                    cycle
                endif
            enddo   
        endif
    enddo

    if(present(light))then
        call obj%rotate(x=-radian(90.0d0 - light%angles(2)))
        call obj%rotate(z=-radian(180.0d0 - light%angles(1)))
        if(present(except) )then
            do i=1,size(soybeans)
                if(i==except)cycle
                call soybeans(i)%rotate(x=-radian(90.0d0 - light%angles(2)))
                call soybeans(i)%rotate(z=-radian(180.0d0 - light%angles(1)))
            enddo
        else
            do i=1,size(soybeans)
                call soybeans(i)%rotate(x=-radian(90.0d0 - light%angles(2)))
                call soybeans(i)%rotate(z=-radian(180.0d0 - light%angles(1)))
            enddo
        endif
        do i=1,size(leaf)
            call leaf(i)%femdomain%rotate(x=-radian(90.0d0 - light%angles(2)))
            call leaf(i)%femdomain%rotate(z=-radian(180.0d0 - light%angles(1)))
        enddo
    endif




end function
! ################################################################

! ################################################################
pure function getRadiusSoybean(obj)  result(radius)
    class(Soybean_),intent(in) :: obj
    real(real64),allocatable :: Points(:,:)
    real(real64) :: radius,center(3)
    
    Points = obj%getPoints()

    ! search Intersect leaf
    center = obj%getCenter() 

    Points(:,1) = Points(:,1) - center(1)
    Points(:,2) = Points(:,2) - center(2)

    radius = maxval(Points(:,1)*Points(:,1) +Points(:,2)*Points(:,2)   )
    radius = sqrt(radius)

end function
! ################################################################

! ################################################################
pure function getCenterSoybean(obj)  result(Center)
    class(Soybean_),intent(in) :: obj
    real(real64),allocatable :: Points(:,:)
    real(real64) :: center(3)
    
    Points = obj%getPoints()

    ! search Intersect leaf
    center(1) = sum(Points(:,1) )/dble(size(Points,1))
    center(2) = sum(Points(:,2) )/dble(size(Points,1))
    center(3) = sum(Points(:,3) )/dble(size(Points,1))

end function
! ################################################################


! ################################################################
subroutine syncSoybean(obj,mpid,from) 
    class(Soybean_),intent(inout) :: obj
    type(MPI_),intent(inout) :: mpid
    integer(int32),intent(in) :: from

    call mpid%BcastMPIcharN(N=20,from=from,val=obj%growth_habit(1:20))
    call mpid%BcastMPIcharN(N=2,from=from,val=obj%growth_stage(1:2) )

    call mpid%bcast(from=from,val=obj%Num_Of_Node)
    call mpid%bcast(from=from,val=obj%num_leaf)
    call mpid%bcast(from=from,val=obj%num_stem_node)
    call mpid%bcast(from=from,val=obj%Num_Of_Root)
        
    call mpid%bcast(from=from,val=obj%MaxLeafNum)
    call mpid%bcast(from=from,val=obj%MaxRootNum)
    call mpid%bcast(from=from,val=obj%MaxStemNum)

    call mpid%bcast(from=from,val=obj%determinate)
!!        
    call mpid%bcast(from=from,val=obj% ms_node)
    call mpid%BcastMPIIntVecFixedSize(from=from,val=obj% br_node)
    call mpid%BcastMPIIntVecFixedSize(from=from,val=obj% br_from)
!!    
    call mpid%bcastMPIReal(from=from,val=obj% ms_length)
    call mpid%bcastMPIReal(from=from,val=obj% ms_width)
!
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% br_length)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% br_width)
!!    
!!
    call mpid%bcast(from=from,val=obj% ms_angle_ave)
    call mpid%bcast(from=from,val=obj% ms_angle_sig)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% br_angle_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% br_angle_sig)
!!!        
!!!
    call mpid%bcast(from=from,val=obj% mr_node)
    call mpid%bcast(from=from,val=obj% mr_length)
    call mpid%bcast(from=from,val=obj% mr_width)
!    
    call mpid%BcastMPIIntVecFixedSize(from=from,val=obj% brr_node)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% brr_length)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% brr_width)
!
    call mpid%bcast(from=from,val=obj% mr_angle_ave)
    call mpid%bcast(from=from,val=obj% mr_angle_sig)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% brr_angle_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% brr_angle_sig)
!!
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% peti_size_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% peti_size_sig)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% peti_width_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% peti_width_sig)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% peti_angle_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% peti_angle_sig)
!
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% leaf_angle_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% leaf_angle_sig)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% leaf_length_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% leaf_length_sig)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% leaf_width_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% leaf_width_sig)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% leaf_thickness_ave)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% leaf_thickness_sig)
!!        
    call mpid%BcastMPICharN(N=3,from=from, val=obj%Stage) ! VE, CV, V1,V2, ..., R1, R2, ..., R8
    call mpid%BcastMPICharN(N=200,from=from, val=obj%name)
    call mpid%bcast(from=from,val=obj%stage_id)
    call mpid%bcast(from=from,val=obj%dt)
!
!!
!!        ! material info
    call mpid%bcast(from=from,val=obj%stemYoungModulus)
    call mpid%bcast(from=from,val=obj%leafYoungModulus)
    call mpid%bcast(from=from,val=obj%rootYoungModulus)
!!
    call mpid%bcast(from=from,val=obj%stemPoissonRatio)
    call mpid%bcast(from=from,val=obj%leafPoissonRatio)
    call mpid%bcast(from=from,val=obj%rootPoissonRatio)
!!
    call mpid%bcast(from=from,val=obj%stemDensity)
    call mpid%bcast(from=from,val=obj%leafDensity)
    call mpid%bcast(from=from,val=obj%rootDensity)
!!        
    call mpid%bcast(from=from,val=obj%leaf2stem)
    call mpid%bcast(from=from,val=obj%stem2stem)
    call mpid%bcast(from=from,val=obj%root2stem)
    call mpid%bcast(from=from,val=obj%root2root)
!!        
!
    call mpid%bcast(from=from,val=obj%time)
    call mpid%bcast(from=from,val=obj%seed_length)
    call mpid%bcast(from=from,val=obj%seed_width)
    call mpid%bcast(from=from,val=obj%seed_height)
    call mpid%bcast(from=from,val=obj%stem_angle)
    call mpid%bcast(from=from,val=obj%root_angle)
    call mpid%bcast(from=from,val=obj%leaf_angle)
!!
    call mpid%BcastMPICharN(N=200,from=from,val=obj%stemconfig)
    call mpid%BcastMPICharN(N=200,from=from,val=obj%rootconfig)
    call mpid%BcastMPICharN(N=200,from=from,val=obj%leafconfig)
!!
!!        ! for deformation analysis
    call mpid%bcast(from=from,val=obj%property_deform_material_density )
    call mpid%bcast(from=from,val=obj%property_deform_material_YoungModulus )
    call mpid%bcast(from=from,val=obj%property_deform_material_PoissonRatio )
    call mpid%bcast(from=from,val=obj%property_deform_initial_Displacement )
    call mpid%bcast(from=from,val=obj%property_deform_initial_Stress )
    call mpid%bcast(from=from,val=obj%property_deform_boundary_TractionForce )
    call mpid%bcast(from=from,val=obj%property_deform_boundary_Displacement )
    call mpid%bcast(from=from,val=obj%property_deform_gravity )
!!
    call mpid%bcast(from=from,val=obj%Gravity_acceralation )
    call mpid%bcast(from=from,val=obj%PenaltyParameter )
    call mpid%bcast(from=from,val=obj%GaussPointProjection )
!!        
!!
!!        
    call mpid%bcast(from=from,val=obj%NodeID_MainStem)
!
!!
    call mpid%bcast(from=from,val=  obj%inLoop )
    call mpid%bcast(from=from,val= obj%hours )


!        ! 節-節点データ構造
    call obj%struct%sync(from=from, mpid=mpid)
!        ! 器官オブジェクト配列
    call syncFEMDomainVector(obj=obj%leaf_list,from=from,mpid=mpid)
    call syncFEMDomainVector(obj=obj%stem_list,from=from,mpid=mpid)
    call syncFEMDomainVector(obj=obj%root_list,from=from,mpid=mpid)
!

!        type(Seed_) :: Seed
!        type(PlantNode_),allocatable :: NodeSystem(:)
!        type(PlantRoot_),allocatable :: RootSystem(:)

    call syncStemVector(obj=obj%Stem,from=from,mpid=mpid)
    call syncLeafVector(obj=obj%Leaf,from=from,mpid=mpid)
    call syncRootVector(obj=obj%Root,from=from,mpid=mpid)
    

!        ! シミュレータ
!        type(ContactMechanics_) :: contact
    call syncsoybean_NodeID_BranchVector(obj%NodeID_Branch,from=from,mpid=mpid)

end subroutine

! ################################################################
subroutine syncSoybeans(soybeans,mpid) 
    type(MPI_),intent(inout) :: mpid
    type(Soybean_),intent(inout) :: soybeans(:)
    integer(int32) :: id,slot_id,stack_id
    integer(int32),allocatable :: localstack(:)
    
    if(.not. allocated(mpid%localstack))then
        call mpid%createstack(size(soybeans) )
    endif

    ! 同期
    do slot_id=1,size(mpid%stack,1)
        do stack_id=1,size(mpid%stack,2)
            id = mpid%stack(slot_id,stack_id)
            if(id==0)then
                cycle
            else
                call soybeans(id)%sync(from=slot_id-1,mpid=mpid)
            endif
        enddo
    enddo


end subroutine
! ################################################################

subroutine syncsoybean_NodeID_Branch(obj,from,mpid)
    class(soybean_NodeID_Branch_),intent(inout) :: obj
    integer(int32),intent(in) :: from
    type(MPI_),intent(inout) :: mpid

    call mpid%bcast(from=from,val=obj%id)

end subroutine
! ################################################################

subroutine syncsoybean_NodeID_BranchVector(obj,from,mpid)
	type(soybean_NodeID_Branch_),allocatable,intent(inout) :: obj(:)
	integer(int32),intent(in) :: from
	type(MPI_),intent(inout) :: mpid
	integer(int32) :: vec_size, i

	vec_size=0
	if(mpid%myrank==from)then
		if(.not.allocated(obj) )then
			vec_size = -1
        else
            vec_size = size(obj)
		endif
	endif
	call mpid%bcast(from=from,val=vec_size)
	if(vec_size<1)then
		return
	endif

	if(from /= mpid%myrank)then
		if(allocated(obj) )then
			deallocate(obj)
		endif
		allocate(obj(vec_size) )
	endif

	do i=1,vec_size
		call obj(i)%sync(from=from, mpid=mpid)
	enddo

end subroutine


! ################################################################
subroutine rotateSoybean(obj,x,y,z)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    type(FEMDomain_) :: domain
    
    ! get points
    domain%mesh%nodcoord = obj%getpoints()

    ! rotate points
    call domain%rotate(x=x,y=y,z=z)

    ! set points
    call obj%setPoints(domain%mesh%nodcoord)
    
end subroutine
! ################################################################


! ################################################################
function getDisplacementSoybean(obj, ground_level,penalty,debug,itrmax,tol) result(disp)
    class(Soybean_),target,intent(inout) :: obj
    real(real64),intent(in) :: ground_level
    real(real64),optional,intent(in) :: penalty, tol
    logical,optional,intent(in) ::debug
    integer(int32),optional,intent(in) ::itrmax
    

    type(FEMDomainp_),allocatable :: FEMDomainPointers(:)
    type(FEMSolver_) :: solver

    real(real64),allocatable :: disp(:)


    integer(int32) :: stem_id, leaf_id, root_id,DomainID,ElementID,i,n
    integer(int32) :: myStemID, yourStemID, myLeafID,myRootID, yourRootID
    integer(int32),allocatable :: FixBoundary(:)
    ! linear elasticity with infinitesimal strain theory
    n = obj%numStem() + obj%numLeaf() + obj%numRoot() 
    allocate(FEMDomainPointers(n) )

    !(1) >> compute overset
    ! For stems
    if(allocated(obj%stem2stem) )then
        do myStemID = 1,size(obj%stem2stem,1)
            do yourStemID = 1, size(obj%stem2stem,2)
                if(obj%stem2stem(myStemID,yourStemID)>=1 )then
                    ! connected
                    call obj%stem(myStemID)%femdomain%overset(&
                        FEMDomain=obj%stem(yourStemID)%femdomain,&
                        DomainID   = yourStemID    ,& 
                        MyDomainID = myStemID  ,&
                        algorithm=FEMDomain_Overset_GPP) ! or "P2P"
                endif
            enddo
        enddo
    endif

    if(allocated(obj%leaf2stem) )then
        do myLeafID = 1,size(obj%leaf2stem,1)
            do yourStemID = 1, size(obj%leaf2stem,2)
                if(obj%leaf2stem(myLeafID,yourStemID)>=1 )then
                    ! connected
                    call obj%leaf(myLeafID)%femdomain%overset(&
                        FEMDomain=obj%stem(yourStemID)%femdomain,&
                        DomainID   = yourStemID    ,& 
                        MyDomainID = obj%numStem() + myLeafID  , &
                        algorithm=FEMDomain_Overset_GPP) ! or "P2P"
                endif
            enddo
        enddo
    endif
    

    if(allocated(obj%root2stem) )then
        do myRootID = 1,size(obj%root2stem,1)
            do yourStemID = 1, size(obj%root2stem,2)
                if(obj%root2stem(myRootID,yourStemID)>=1 )then
                    ! connected
                    call obj%root(myRootID)%femdomain%overset(&
                        FEMDomain=obj%stem(yourStemID)%femdomain,&
                        DomainID   = yourStemID    ,& 
                        MyDomainID = obj%numStem() +obj%numLeaf() + myRootID  , &
                        algorithm=FEMDomain_Overset_GPP) ! or "P2P"
                endif
            enddo
        enddo
    endif


    if(allocated(obj%root2root) )then
        do myRootID = 1,size(obj%root2root,1)
            do yourrootID = 1, size(obj%root2root,2)
                if(obj%root2root(myRootID,yourrootID)>=1 )then
                    ! connected
                    call obj%root(myRootID)%femdomain%overset(&
                        FEMDomain=obj%root(yourrootID)%femdomain,&
                        DomainID   = obj%numroot() +obj%numLeaf() + yourrootID    ,& 
                        MyDomainID = obj%numroot() +obj%numLeaf() + myRootID  , &
                        algorithm=FEMDomain_Overset_GPP) ! or "P2P"
                endif
            enddo
        enddo
    endif


    if(present(debug) )then
        if(debug)then
            print *, "[ok] overset >> done."        
        endif
    endif



    call solver%init(NumDomain=obj%numStem() +obj%numLeaf() + obj%numRoot() )
    
    FEMDomainPointers = obj%getFEMDomainPointers()
    call solver%setDomain(FEMDomainPointers=FEMDomainPointers )
    
    if(present(debug) )then
        if(debug)then
            print *, "[ok] initSolver >> done."        
        endif
    endif

    call solver%setCRS(DOF=3,debug=debug)

    ! CRS ready!

    if( .not. obj%checkYoungModulus())then
        print *, "[ERROR] YoungModulus(:) is not ready."
        stop
    endif
    if( .not. obj%checkPoissonRatio())then
        print *, "[ERROR] PoissonRatio(:) is not ready."
        stop
    endif
    if( .not. obj%checkDensity())then
        print *, "[ERROR] Density(:) is not ready."
        stop
    endif


    if(present(debug) )then
        if(debug)then
            print *, "[ok] setCRS >> done."        
        endif
    endif
    
    !$OMP parallel 
    !$OMP do
    do DomainID=1,size(FEMDomainPointers)
        do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
               Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix(&
               ElementID=ElementID,&
               E=obj%getYoungModulus(DomainID=DomainID,ElementID=ElementID), &
               v=obj%getPoissonRatio(DomainID=DomainID,ElementID=ElementID)  ) )

            call solver%setVector(DomainID=DomainID,ElementID=ElementID,DOF=3,&
                Vector=FEMDomainPointers(DomainID)%femdomainp%MassVector(&
                    ElementID=ElementID,&
                    DOF=FEMDomainPointers(DomainID)%femdomainp%nd() ,&
                    Density= obj%getDensity(DomainID=DomainID,ElementID=ElementID) ,&
                    Accel=[0.0d0, 0.0d0, -9.80d0]&
                    ) & 
                )
        enddo
    enddo
    !$OMP end do
    !$OMP end parallel
    
    if(present(debug) )then
        if(debug)then
            print *, "[ok] set Matrix & vectors >> done."        
        endif
    endif
    

    call solver%setEbOM(penalty=input(default=10000000.0d0,option=penalty), DOF=3)


    if(present(debug) )then
        if(debug)then
            print *, "[ok] set EbOM >> done."        
        endif
    endif
    
    ! fix-boundary conditions
    do i=1,size(FEMDomainPointers)
        if(FEMDomainPointers(i)%FEMDomainp%z_min() <= ground_level )then
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-2
            call solver%fix(DomainID=i,IDs=FixBoundary,FixValue=0.0d0)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-1
            call solver%fix(DomainID=i,IDs=FixBoundary,FixValue=0.0d0)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-0
            call solver%fix(DomainID=i,IDs=FixBoundary,FixValue=0.0d0)
        endif
    enddo

    if(present(debug) )then
        if(debug)then
            print *, "[ok] FixBoundary >> done."        
        endif
    endif

    if(present(debug) )then
        solver%debug = debug
    endif
    if(present(itrmax) )then
        solver%itrmax = itrmax
    endif

    if(present(tol) )then
        solver%er0 = tol
    endif


    disp = solver%solve()


    

    call solver%remove()


    if(present(debug) )then
        if(debug)then
            print *, "[ok] Solve >> done."        
        endif
    endif
    ! japanese "ato-shimatsu"


end function
! ################################################################


! ################################################################
function getFEMDomainPointersSoybean(obj) result(FEMDomainPointers)
    class(Soybean_),target,intent(in) :: obj
    type(FEMDomainp_),allocatable :: FEMDomainPointers(:)
    integer(int32) :: num_FEMDomain,i, n

    num_FEMDomain = obj%numStem() + obj%numLeaf() + obj%numRoot()
    allocate(FEMDomainPointers(num_FEMDomain) )
    n = 0
    do i=1,obj%numStem()
        if(.not.obj%stem(i)%femdomain%empty() )then
            n = n + 1
            FEMDomainPointers(n)%femdomainp => obj%stem(i)%femdomain
        endif
    enddo
    do i=1,obj%numLeaf()
        if(.not.obj%leaf(i)%femdomain%empty() )then
            n = n + 1
            FEMDomainPointers(n)%femdomainp => obj%leaf(i)%femdomain
        endif
    enddo
    do i=1,obj%numRoot()
        if(.not.obj%root(i)%femdomain%empty() )then
            n = n + 1
            FEMDomainPointers(n)%femdomainp => obj%root(i)%femdomain
        endif
    enddo
end function
! ################################################################


! ################################################################
function getObjectPointersSoybean(obj) result(FEMDomainPointers)
    class(Soybean_),target,intent(in) :: obj
    type(FEMDomainp_),allocatable :: FEMDomainPointers(:)
    integer(int32) :: num_FEMDomain,i, n

    ! order: stem -> leaf -> root
    num_FEMDomain = obj%numStem() + obj%numLeaf() + obj%numRoot()
    allocate(FEMDomainPointers(num_FEMDomain) )
    n = 0
    do i=1,obj%numStem()
        n = n + 1
        FEMDomainPointers(n)%femdomainp => obj%stem(i)%femdomain
    enddo
    do i=1,obj%numLeaf()
        n = n + 1
        FEMDomainPointers(n)%femdomainp => obj%leaf(i)%femdomain
    enddo
    do i=1,obj%numRoot()
        n = n + 1
        FEMDomainPointers(n)%femdomainp => obj%root(i)%femdomain
    enddo
end function
! ################################################################

! ################################################################
function checkYoungModulusSoybean(obj) result(all_young_modulus_is_set)
    class(Soybean_),intent(in) :: obj
    logical :: all_young_modulus_is_set 
    integer(int32) :: i
    ! order: stem -> leaf -> root

    all_young_modulus_is_set = .true.
    do i=1,obj%numStem()
        if(.not.allocated(obj%stem(i)%YoungModulus) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusSoybean >> Young Modulus is not set"
            print *, "@ Stem ID:",i
            print *, "check it by: allocated(this%stem("+str(i)+")%YoungModulus)"
            return
        endif
    enddo

    do i=1,obj%numLeaf()
        if(.not.allocated(obj%Leaf(i)%YoungModulus) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusSoybean >> Young Modulus is not set"
            print *, "@ Leaf ID:",i
            print *, "check it by: allocated(this%Leaf("+str(i)+")%YoungModulus)"
            return
        endif
    enddo

    do i=1,obj%numRoot()
        if(.not.allocated(obj%Root(i)%YoungModulus) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusSoybean >> Young Modulus is not set"
            print *, "@ Root ID:",i
            print *, "check it by: allocated(this%Root("+str(i)+")%YoungModulus)"
            return
        endif
    enddo

end function
! ################################################################



! ################################################################
function checkPoissonRatioSoybean(obj) result(all_young_modulus_is_set)
    class(Soybean_),intent(in) :: obj
    logical :: all_young_modulus_is_set 
    integer(int32) :: i
    ! order: stem -> leaf -> root

    all_young_modulus_is_set = .true.
    do i=1,obj%numStem()
        if(.not.allocated(obj%stem(i)%PoissonRatio) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioSoybean >> Young Modulus is not set"
            print *, "@ Stem ID:",i
            print *, "check it by: allocated(this%stem("+str(i)+")%PoissonRatio)"
            return
        endif
    enddo

    do i=1,obj%numLeaf()
        if(.not.allocated(obj%Leaf(i)%PoissonRatio) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioSoybean >> Young Modulus is not set"
            print *, "@ Leaf ID:",i
            print *, "check it by: allocated(this%Leaf("+str(i)+")%PoissonRatio)"
            return
        endif
    enddo

    do i=1,obj%numRoot()
        if(.not.allocated(obj%Root(i)%PoissonRatio) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioSoybean >> Young Modulus is not set"
            print *, "@ Root ID:",i
            print *, "check it by: allocated(this%Root("+str(i)+")%PoissonRatio)"
            return
        endif
    enddo

end function
! ################################################################


! ################################################################
function checkDensitySoybean(obj) result(all_young_modulus_is_set)
    class(Soybean_),intent(in) :: obj
    logical :: all_young_modulus_is_set 
    integer(int32) :: i
    ! order: stem -> leaf -> root

    all_young_modulus_is_set = .true.
    do i=1,obj%numStem()
        if(.not.allocated(obj%stem(i)%Density) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensitySoybean >> Young Modulus is not set"
            print *, "@ Stem ID:",i
            print *, "check it by: allocated(this%stem("+str(i)+")%Density)"
            return
        endif
    enddo

    do i=1,obj%numLeaf()
        if(.not.allocated(obj%Leaf(i)%Density) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensitySoybean >> Young Modulus is not set"
            print *, "@ Leaf ID:",i
            print *, "check it by: allocated(this%Leaf("+str(i)+")%Density)"
            return
        endif
    enddo

    do i=1,obj%numRoot()
        if(.not.allocated(obj%Root(i)%Density) )then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensitySoybean >> Young Modulus is not set"
            print *, "@ Root ID:",i
            print *, "check it by: allocated(this%Root("+str(i)+")%Density)"
            return
        endif
    enddo

end function
! ################################################################



! ################################################################
function getYoungModulusSoybean(obj,DomainID,ElementID) result(YoungModulus)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in) :: DomainID, ElementID
    real(real64) :: YoungModulus 
    integer(int32) :: i, n
    
    if(DomainID > obj%numStem() + obj%numLeaf() + obj%numRoot()  )then
        print *, "ERROR :: getYoungModulusSoybean >>  DomainID exceeds max_domain_size"
        return
    endif

    ! default >> search @ all domains
    ! order: stem -> leaf -> root
    if(DomainID <= obj%numStem() )then
        n = DomainID - 0    
        YoungModulus = obj%stem(n)%YoungModulus(ElementID)
        return
    elseif(obj%numStem() + 1 <= DomainID .and. DomainID <= obj%numStem() + obj%numLeaf()  )then
        n = DomainID - obj%numStem()
        YoungModulus = obj%leaf(n)%YoungModulus(ElementID)
        return
    else
        n = DomainID - obj%numStem() - obj%numLeaf()
        YoungModulus = obj%root(n)%YoungModulus(ElementID)
        return
    endif

    
end function
! ################################################################

! ################################################################
function getPoissonRatioSoybean(obj,DomainID,ElementID) result(PoissonRatio)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in) :: DomainID, ElementID
    real(real64) :: PoissonRatio 
    integer(int32) :: i, n
    
    if(DomainID > obj%numStem() + obj%numLeaf() + obj%numRoot()  )then
        print *, "ERROR :: getPoissonRatioSoybean >>  DomainID exceeds max_domain_size"
        return
    endif

    ! default >> search @ all domains
    ! order: stem -> leaf -> root
    if(DomainID <= obj%numStem() )then
        n = DomainID - 0    
        PoissonRatio = obj%stem(n)%PoissonRatio(ElementID)
        return
    elseif(obj%numStem() + 1 <= DomainID .and. DomainID <= obj%numStem() + obj%numLeaf()  )then
        n = DomainID - obj%numStem()
        PoissonRatio = obj%leaf(n)%PoissonRatio(ElementID)
        return
    else
        n = DomainID - obj%numStem() - obj%numLeaf()
        PoissonRatio = obj%root(n)%PoissonRatio(ElementID)
        return
    endif

    
end function
! ################################################################


! ################################################################
function getDensitySoybean(obj,DomainID,ElementID) result(Density)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in) :: DomainID, ElementID
    real(real64) :: Density 
    integer(int32) :: i, n
    
    if(DomainID > obj%numStem() + obj%numLeaf() + obj%numRoot()  )then
        print *, "ERROR :: getDensitySoybean >>  DomainID exceeds max_domain_size"
        return
    endif

    ! default >> search @ all domains
    ! order: stem -> leaf -> root
    if(DomainID <= obj%numStem() )then
        n = DomainID - 0    
        Density = obj%stem(n)%Density(ElementID)
        return
    elseif(obj%numStem() + 1 <= DomainID .and. DomainID <= obj%numStem() + obj%numLeaf()  )then
        n = DomainID - obj%numStem()
        Density = obj%leaf(n)%Density(ElementID)
        return
    else
        n = DomainID - obj%numStem() - obj%numLeaf()
        Density = obj%root(n)%Density(ElementID)
        return
    endif

    
end function
! ################################################################
subroutine checkMemoryRequirementSoybean(obj)
    class(Soybean_),intent(in) :: Obj
    real(real64) :: re_val
    integer(int64) :: val
    
    print *, "===================================="
    print *, "checking Memory (RAM) Requirement..."
    print *, "------------------------------------"
    print *, "| Object type                     | Soybean"
    print *, "| Number of points                | "+str(obj%nn())
    print *, "| Degree of freedom | Deformation | "+str(obj%nn()*3)
    print *, "|                   | ModeAnalysis| "+str(obj%nn()*3*obj%nn()*3)
    print *, "|                   | Diffusion   | "+str(obj%nn())
    print *, "|                   | Reaction    | "+str(obj%nn())

    print *, "| DRAM requirement  | Deformation | "+str(obj%nn()*3*40*30/1000/1000)+" (MB)"
    val = obj%nn()*3*30
    val = val * obj%nn()*3/1000/1000
    print *, "|                   | ModeAnalysis| ",str(val)," (MB)"
    print *, "|                   | Diffusion   | "+str(obj%nn()*1*20*10/1000/1000)+" (MB)"
    print *, "|                   | Reaction    | "+str(obj%nn()*1*20*10/1000/1000)+" (MB)"
    print *, "===================================="


end subroutine

! ################################################################
recursive subroutine setYoungModulusSoybean(obj,YoungModulus,stem,root,leaf,ElementList)
    class(Soybean_),intent(inout) :: obj
    logical,optional,intent(in) :: stem, root, leaf

    ! ElementList(Idx, [TYPE, DOMAIN, ELEMENT]) 
    integer(int32),optional,intent(in) :: ElementList(:,:)

    real(real64),intent(in) :: YoungModulus
    integer(int32) :: i, j,  n, domain_idx, elem_idx

    n = 0
    if(present(stem) )then
        if(stem)then
            n=n+1
            if(allocated(obj%stem) )then
                do i=1,size(obj%stem)
                    if(obj%stem(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_STEM )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%stem(domain_idx)%YoungModulus(elem_idx) = YoungModulus
                            endif
                        enddo
                    else
                        obj%stem(i)%YoungModulus = YoungModulus*eyes(obj%stem(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif

    if(present(leaf) )then
        if(leaf)then
            n=n+10
            if(allocated(obj%leaf) )then
                do i=1,size(obj%leaf)
                    if(obj%leaf(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_LEAF )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%LEAF(domain_idx)%YoungModulus(elem_idx) = YoungModulus
                            endif
                        enddo
                    else
                        obj%leaf(i)%YoungModulus = YoungModulus*eyes(obj%leaf(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif

    if(present(root) )then
        if(root)then
            n=n+100
            if(allocated(obj%root) )then
                do i=1,size(obj%root)
                    if(obj%root(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_ROOT )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%ROOT(domain_idx)%YoungModulus(elem_idx) = YoungModulus
                            endif
                        enddo
                    else
                        obj%root(i)%YoungModulus = YoungModulus*eyes(obj%root(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif


    if(n==0)then
        call obj%setYoungModulus(YoungModulus=YoungModulus,stem=.true.,root=.true.,leaf=.true.,&
            ElementList=ElementList)
    endif
    
end subroutine
! ################################################################
! ################################################################
recursive subroutine setPoissonRatioSoybean(obj,PoissonRatio,stem,root,leaf,ElementList)
    class(Soybean_),intent(inout) :: obj
    logical,optional,intent(in) :: stem, root, leaf

    ! ElementList(Idx, [TYPE, DOMAIN, ELEMENT]) 
    integer(int32),optional,intent(in) :: ElementList(:,:)

    real(real64),intent(in) :: PoissonRatio
    integer(int32) :: i, j,  n, domain_idx, elem_idx

    n = 0
    if(present(stem) )then
        if(stem)then
            n=n+1
            if(allocated(obj%stem) )then
                do i=1,size(obj%stem)
                    if(obj%stem(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_STEM )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%stem(domain_idx)%PoissonRatio(elem_idx) = PoissonRatio
                            endif
                        enddo
                    else
                        obj%stem(i)%PoissonRatio = PoissonRatio*eyes(obj%stem(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif

    if(present(leaf) )then
        if(leaf)then
            n=n+10
            if(allocated(obj%leaf) )then
                do i=1,size(obj%leaf)
                    if(obj%leaf(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_LEAF )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%LEAF(domain_idx)%PoissonRatio(elem_idx) = PoissonRatio
                            endif
                        enddo
                    else
                        obj%leaf(i)%PoissonRatio = PoissonRatio*eyes(obj%leaf(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif

    if(present(root) )then
        if(root)then
            n=n+100
            if(allocated(obj%root) )then
                do i=1,size(obj%root)
                    if(obj%root(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_ROOT )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%ROOT(domain_idx)%PoissonRatio(elem_idx) = PoissonRatio
                            endif
                        enddo
                    else
                        obj%root(i)%PoissonRatio = PoissonRatio*eyes(obj%root(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif


    if(n==0)then
        call obj%setPoissonRatio(PoissonRatio=PoissonRatio,stem=.true.,root=.true.,leaf=.true.,&
            ElementList=ElementList)
    endif
    
end subroutine
! ################################################################

! ################################################################
recursive subroutine setDensitySoybean(obj,Density,stem,root,leaf,ElementList)
    class(Soybean_),intent(inout) :: obj
    logical,optional,intent(in) :: stem, root, leaf

    ! ElementList(Idx, [TYPE, DOMAIN, ELEMENT]) 
    integer(int32),optional,intent(in) :: ElementList(:,:)

    real(real64),intent(in) :: Density
    integer(int32) :: i, j,  n, domain_idx, elem_idx

    n = 0
    if(present(stem) )then
        if(stem)then
            n=n+1
            if(allocated(obj%stem) )then
                do i=1,size(obj%stem)
                    if(obj%stem(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_STEM )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%stem(domain_idx)%Density(elem_idx) = Density
                            endif
                        enddo
                    else
                        obj%stem(i)%Density = Density*eyes(obj%stem(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif

    if(present(leaf) )then
        if(leaf)then
            n=n+10
            if(allocated(obj%leaf) )then
                do i=1,size(obj%leaf)
                    if(obj%leaf(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_LEAF )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%LEAF(domain_idx)%Density(elem_idx) = Density
                            endif
                        enddo
                    else
                        obj%leaf(i)%Density = Density*eyes(obj%leaf(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif

    if(present(root) )then
        if(root)then
            n=n+100
            if(allocated(obj%root) )then
                do i=1,size(obj%root)
                    if(obj%root(i)%femdomain%empty() )then
                        cycle
                    elseif(present(ElementList) )then
                        do j=1, size(ElementList,1)
                            if(ElementList(j,1) == obj%TYPE_ROOT )then
                                domain_idx = ElementList(j,2)  
                                elem_idx = ElementList(j,3) 
                                obj%ROOT(domain_idx)%Density(elem_idx) = Density
                            endif
                        enddo
                    else
                        obj%root(i)%Density = Density*eyes(obj%root(i)%femdomain%ne())
                    endif
                enddo
            endif
        endif
    endif


    if(n==0)then
        call obj%setDensity(Density=Density,stem=.true.,root=.true.,leaf=.true.,&
            ElementList=ElementList)
    endif
    
end subroutine
! ################################################################


! ################################################################
function getEigenModeSoybean(obj, ground_level,penalty,debug,Frequency,EbOM_Algorithm,num_mode) result(EigenVectors)
    class(Soybean_),target,intent(inout) :: obj
    real(real64),intent(in) :: ground_level
    real(real64),optional,intent(in) :: penalty
    logical,optional,intent(in) :: debug
    real(real64),allocatable,intent(inout) :: Frequency(:)
    character(*),optional,intent(in) :: EbOM_Algorithm
    !integer(int32),optional,intent(in) :: num_mode
    
    integer(int32),optional,intent(in) :: num_mode
    integer(int32) :: num_freq

    type(FEMDomainp_),allocatable :: FEMDomainPointers(:)
    type(FEMSolver_) :: solver
    type(Math_) :: math

    real(real64),allocatable :: EigenVectors(:,:),buf(:,:),buf_vec(:)

    integer(int32) :: stem_id, leaf_id, root_id,DomainID,ElementID,i,n
    integer(int32) :: myStemID, yourStemID, myLeafID,myRootID, yourRootID
    integer(int32),allocatable :: FixBoundary(:)
    integer(int32) :: nn_domains,EbOM_Algorithm_id
    real(real64) :: vec_norm
    real(real64),allocatable :: all_frequency(:),All_EigenVectors(:,:)

    num_freq = input(default=10,option=num_mode)

    EbOM_Algorithm_id = FEMDomain_Overset_GPP
    if(present(EbOM_Algorithm) )then
        if(EbOM_Algorithm=="P2P")then
            EbOM_Algorithm_id=FEMDomain_Overset_P2P
        elseif(EbOM_Algorithm=="GPP")then
            EbOM_Algorithm_id=FEMDomain_Overset_P2P
        endif
    endif

    ! linear elasticity with infinitesimal strain theory
    n = obj%numStem() + obj%numLeaf() + obj%numRoot() 
    allocate(FEMDomainPointers(n) )

    !(1) >> compute overset
    ! For stems
    if(allocated(obj%stem2stem) )then
        if(allocated(obj%stem) )then
            do myStemID = 1,size(obj%stem2stem,1)
                do yourStemID = 1, size(obj%stem2stem,2)
                    if(obj%stem2stem(myStemID,yourStemID)>=1 )then
                        ! connected
                        call obj%stem(myStemID)%femdomain%overset(&
                            FEMDomain=obj%stem(yourStemID)%femdomain,&
                            DomainID   = yourStemID    ,& 
                            MyDomainID = myStemID  ,&
                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
                        call obj%stem(yourStemID)%femdomain%overset(&
                            FEMDomain=obj%stem(myStemID)%femdomain,&
                            DomainID   = myStemID    ,& 
                            MyDomainID = yourStemID  ,&
                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
                    endif
                enddo
            enddo
        endif
    endif

    if(allocated(obj%leaf2stem) )then
        if(allocated(obj%leaf) .and. allocated(obj%stem) )then
            do myLeafID = 1,size(obj%leaf2stem,1)
                do yourStemID = 1, size(obj%leaf2stem,2)
                    if(obj%leaf2stem(myLeafID,yourStemID)>=1 )then
                        ! connected
                        call obj%leaf(myLeafID)%femdomain%overset(&
                            FEMDomain=obj%stem(yourStemID)%femdomain,&
                            DomainID   = yourStemID    ,& 
                            MyDomainID = obj%numStem() + myLeafID  , &
                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
                        !call obj%stem(yourStemID)%femdomain%overset(&
                        !    FEMDomain=obj%leaf(myLeafID)%femdomain,
                        !    DomainID =obj%numStem() + myLeafID,
                        !    MyDomainID= yourStemID,
                        !    algorithm=EbOM_Algorithm_id ) ! or "P2P"
                        
                    endif
                enddo
            enddo
        endif
    endif
    

    if(allocated(obj%root2stem) )then
        if(allocated(obj%stem) .and. allocated(obj%root) )then
            do myRootID = 1,size(obj%root2stem,1)
                do yourStemID = 1, size(obj%root2stem,2)
                    if(obj%root2stem(myRootID,yourStemID)>=1 )then
                        ! connected
                        call obj%root(myRootID)%femdomain%overset(&
                            FEMDomain=obj%stem(yourStemID)%femdomain,&
                            DomainID   = yourStemID    ,& 
                            MyDomainID = obj%numStem() +obj%numLeaf() + myRootID  , &
                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
                        call obj%stem(yourStemID)%femdomain%overset(&
                            FEMDomain=  obj%root(myRootID)%femdomain,&
                            DomainID   = obj%numStem() +obj%numLeaf() + myRootID    ,& 
                            MyDomainID =  yourStemID , &
                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
                    endif
                enddo
            enddo
        endif
    endif


    if(allocated(obj%root2root) )then
        if(allocated(obj%root) )then
            do myRootID = 1,size(obj%root2root,1)
                do yourrootID = 1, size(obj%root2root,2)
                    if(obj%root2root(myRootID,yourrootID)>=1 )then
                        ! connected
                        call obj%root(myRootID)%femdomain%overset(&
                            FEMDomain=obj%root(yourrootID)%femdomain,&
                            DomainID   = obj%numroot() +obj%numLeaf() + yourrootID    ,& 
                            MyDomainID = obj%numroot() +obj%numLeaf() + myRootID  , &
                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
                    
                        call obj%root(yourrootID)%femdomain%overset(&
                            FEMDomain=obj%root(myRootID)%femdomain,&
                            DomainID   = obj%numroot() +obj%numLeaf() + myRootID    ,& 
                            MyDomainID =  obj%numroot() +obj%numLeaf() + yourrootID , &
                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
                    endif
                enddo
            enddo
        endif
    endif


    if(present(debug) )then
        if(debug)then
            print *, "[ok] overset >> done."        
        endif
    endif



    call solver%init(NumDomain=obj%numStem() +obj%numLeaf() + obj%numRoot() )
    
    FEMDomainPointers = obj%getFEMDomainPointers()
    call solver%setDomain(FEMDomainPointers=FEMDomainPointers )
    
    if(present(debug) )then
        if(debug)then
            print *, "[ok] initSolver >> done."        
        endif
    endif

    call solver%setCRS(DOF=3,debug=debug)

    ! CRS ready!

    if( .not. obj%checkYoungModulus())then
        print *, "[ERROR] YoungModulus(:) is not ready."
        stop
    endif
    if( .not. obj%checkPoissonRatio())then
        print *, "[ERROR] PoissonRatio(:) is not ready."
        stop
    endif
    if( .not. obj%checkDensity())then
        print *, "[ERROR] Density(:) is not ready."
        stop
    endif


    if(present(debug) )then
        if(debug)then
            print *, "[ok] setCRS >> done."        
        endif
    endif
    
    !$OMP parallel 
    !$OMP do
    do DomainID=1,size(FEMDomainPointers)
        do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
               Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix(&
               ElementID=ElementID,&
               E=obj%getYoungModulus(DomainID=DomainID,ElementID=ElementID), &
               v=obj%getPoissonRatio(DomainID=DomainID,ElementID=ElementID)  ) )
        enddo
    enddo
    !$OMP end do
    !$OMP end parallel
    
    
    if(present(debug) )then
        if(debug)then
            print *, "[ok] set Matrix & vectors >> done."        
        endif
    endif
    
    call solver%setEbOM(penalty=input(default=10000000.0d0,option=penalty), DOF=3)

    if(present(debug) )then
        if(debug)then
            print *, "[ok] set EbOM >> done."        
        endif
    endif

    call solver%keepThisMatrixAs("A")
    !call solver%saveMatrix(name="A",CRS_as_dense=.true.,zero_or_nonzero=.true)
    call solver%zeros()
    
    ! mass matrix
    !$OMP parallel 
    !$OMP do
    do DomainID=1,size(FEMDomainPointers)
        do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
               Matrix=FEMDomainPointers(DomainID)%femdomainp%massMatrix(&
                ElementID=ElementID,&
                Density=obj%getDensity(DomainID=DomainID,ElementID=ElementID), &
                DOF=3 ) )
        enddo
    enddo
    !$OMP end do
    !$OMP end parallel
    call solver%keepThisMatrixAs("B")
    
    ! fix-boundary conditions
    nn_domains = 0
    do i=1,size(FEMDomainPointers)
        if(FEMDomainPointers(i)%FEMDomainp%z_min() <= ground_level )then
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-2 + nn_domains*3
            call solver%fix_eig(IDs=FixBoundary)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-1 + nn_domains*3
            call solver%fix_eig(IDs=FixBoundary)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-0 + nn_domains*3
            call solver%fix_eig(IDs=FixBoundary)
        endif
        nn_domains = nn_domains + FEMDomainPointers(i)%FEMDomainp%nn()
    enddo

    if(present(debug) )then
        if(debug)then
            print *, "[ok] FixBoundary >> done."        
        endif
    endif

    if(present(debug) )then
        solver%debug = debug
    endif



    call solver%eig(eigen_value=All_Frequency,eigen_vectors=All_EigenVectors)
    call solver%remove()

    if(All_Frequency(1)<=0.0d0 )then
        return
    endif

    ! simplify this part
    ! normalize EigenVectors
    do i=1,size(All_EigenVectors,2)
        vec_norm = norm(All_EigenVectors(:,i) )
        All_EigenVectors(:,i) = All_EigenVectors(:,i)/vec_norm
    enddo

    Frequency = zeros(num_freq)
    EigenVectors = zeros(size(All_EigenVectors,1),num_freq)

    do i=1,num_freq
        n = minvalID(All_Frequency)
        EigenVectors(:,i) = All_EigenVectors(:,n)
        Frequency(i)      = All_Frequency(n)
        All_Frequency(n) = maxval(All_Frequency) 
    enddo

    do i=1,size(Frequency)
        if(Frequency(i)<0.0d0)then
            Frequency(i)=0.0d0
        endif
    enddo
    Frequency = sqrt((Frequency))/(2.0d0*math%PI)



    if(present(debug) )then
        if(debug)then
            print *, "[ok] Solve >> done."        
        endif
    endif
    


end function
! ################################################################

subroutine resizeStemSoybean(this,StemID,InterNodeID,Length,Width)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: stemID,InterNodeID
    real(real64),optional,intent(in) :: Length,Width
    real(real64) :: current_length
    integer(int32) :: i,j,node_id

    node_id = 0
    do i=1,size(this%stem,1)
        if(this%stem(i)%stemID==StemID)then
            if(this%stem(i)%InterNodeID==InterNodeID)then
               node_id = i 
            endif
        endif
    enddo
    if(node_id==0)then
        print *, "resizeStemSoybean 404 Not Found."
        return
    endif
    
    call this%stem(node_id)%grow(length=Length,Width=Width)
    call this%update()



end subroutine


subroutine rotateStemSoybean(this,StemID,InterNodeID,Angles)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: stemID,InterNodeID
    real(real64),intent(in) :: Angles(1:3)
    real(real64) :: current_length
    integer(int32) :: i,j,node_id

    do i=1,size(this%stem,1)
        if(this%stem(i)%stemID==StemID)then
            if(this%stem(i)%InterNodeID==InterNodeID)then
                node_id = i
            endif
        endif
    enddo
    

    if(node_id==0)then
        print *, "resizeStemSoybean 404 Not Found."
        return
    endif
    
    call this%stem(node_id)%femdomain%rotate(x=Angles(1),y=Angles(2),z=Angles(3),deg=.true. )
    call this%update()



end subroutine


function searchStemSoybean(this,StemID,InterNodeID) result(node_id)
    class(Soybean_),intent(in) :: this
    integer(int32),intent(in) :: stemID,InterNodeID
    real(real64) :: current_length
    integer(int32) :: i,j,node_id

    node_id = -404

    if(StemID==0)then
        if(1 <=InterNodeID .and. InterNodeID <= size(this%NodeID_MainStem) )then
            node_id = this%NodeID_MainStem(InterNodeID)
        else
            return
        endif
    else
        if(.not. allocated(this%NodeID_Branch)  )then
            return
        endif
        if(1 <=StemID .and. StemID <= size(this%NodeID_Branch) )then
            if(allocated(this%NodeID_Branch(StemID)%ID ) )then
                node_id = this%NodeID_Branch(StemID)%ID(InterNodeID)
            endif
        endif
    endif
    
!    do i=1,size(this%stem,1)
!        if(this%stem(i)%stemID==StemID)then
!            if(this%stem(i)%InterNodeID==InterNodeID)then
!                node_id = i
!            endif
!        endif
!    enddo
    
end function

function searchPetioleSoybean(this,StemID,InterNodeID,PetioleID) result(node_id)
    class(Soybean_),intent(in) :: this
    integer(int32),intent(in) :: stemID,InterNodeID,PetioleID
    real(real64) :: current_length
    integer(int32) :: i,j,node_id,n

    node_id = -404

    node_id = this%searchStem(StemID=StemID,InterNodeID=InterNodeID)

    if(node_id < 0)then
        return
    endif



    n = 0
    do i=1,size(this%stem2stem,1)
        if(this%stem2stem(i,node_id)==1 &
            .and. this%stem(i)%StemID==-1 )then
            n = n + 1
            if(n==PetioleID)then
                node_id = i
                return
            endif
        endif
    enddo

    node_id = -404404

    
end function





function searchLeafSoybean(this,StemID,InterNodeID,PetioleID,LeafID) result(leaf_id)
    class(Soybean_),intent(in) :: this
    integer(int32),intent(in) :: stemID,InterNodeID,PetioleID,LeafID
    real(real64) :: current_length
    integer(int32) :: i,j,node_id,n,petiole_id,leaf_id

    leaf_id    = -404404404
    petiole_id = -404404
    node_id    = -404

    petiole_id = this%searchPetiole(&
        StemID=StemID,& ! main=0, branch=1,2 ...
        InterNodeID=InterNodeID,& ! 1,2,3...
        PetioleID=PetioleID &
    )

    if(petiole_id<0)then
        leaf_id = -404
        return
    endif

    do i=1,size(this%leaf2stem,1)
        if(this%leaf2stem(i,petiole_id)==1 )then
            if(this%leaf(i)%leafID==LeafID )then
                leaf_id =  i
            endif
        endif
    enddo    
    
end function

! #############################################################################
subroutine resizePetioleSoybean(this,StemID,InterNodeID,PetioleID,Length,Width)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: stemID,InterNodeID,PetioleID
    real(real64),optional,intent(in) :: Length,Width
    real(real64) :: current_length
    integer(int32) :: i,j,node_id

    node_id = this%searchPetiole(StemID=StemID,InterNodeID=InterNodeID,PetioleID=PetioleID) 

    if(node_id<1)then
        print *, "resizePetioleSoybean 404 Not Found."
        return
    endif

    call this%stem(node_id)%grow(length=Length,Width=Width)
    call this%update()

end subroutine
! #############################################################################


! #############################################################################
subroutine rotatePetioleSoybean(this,StemID,InterNodeID,PetioleID,Angles)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: stemID,InterNodeID,PetioleID
    real(real64),intent(in) :: Angles(1:3)
    real(real64) :: current_length
    integer(int32) :: i,j,node_id

    node_id = this%searchPetiole(StemID=StemID,InterNodeID=InterNodeID,PetioleID=PetioleID)
    
    if(node_id<1)then
        print *, "rotatePetioleSoybean 404 Not Found."
        return
    endif
    
    call this%stem(node_id)%femdomain%rotate(x=Angles(1),y=Angles(2),z=Angles(3),deg=.true. )
    call this%update()



end subroutine
! #########################################################

subroutine resizeLeafSoybean(this,StemID,InterNodeID,PetioleID,LeafID,Length,Width)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: stemID,InterNodeID,PetioleID,LeafID
    real(real64),optional,intent(in) :: Length,Width
    real(real64) :: current_length
    integer(int32) :: i,j,leaf_id

    leaf_id = this%searchLeaf(StemID=StemID,InterNodeID=InterNodeID,PetioleID=PetioleID,&
        LeafID=LeafID) 

    if(leaf_id<1)then
        print *, "resizeLeafSoybean 404 Not Found."
        return
    endif


    call this%leaf(leaf_id)%resize(length=Length,Width=Width)
    
    
    call this%update()

end subroutine

! #########################################################
function maxStemIDSoybean(this) result(ret)
    class(Soybean_),intent(in) :: this
    integer(int32) :: ret,i,buf

    ret = 0
    do i=1, size(this%Stem,1)
        buf = this%maxInterNodeID(StemID=i)
        if(buf >=1)then
            ret = ret + 1
        else
            return
        endif
    enddo

end function
! #########################################################




! #########################################################
function maxInterNodeIDSoybean(this,StemID) result(ret)
    class(Soybean_),intent(in) :: this
    integer(int32),intent(in) :: StemID
    integer(int32) :: ret,i,buf

    ret = 0
    if(StemID==0)then
        if(allocated(this%NodeID_MainStem) )then
            ret = size(this%NodeID_MainStem)
        else
            ret = 0
        endif
    else
        if(allocated(this%NodeID_Branch) )then
            ret = size(this%NodeID_Branch(StemID)%ID )
        else
            ret = 0
        endif
    endif

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
function maxPetioleIDSoybean(this,StemID,InterNodeID) result(ret)
    class(Soybean_),intent(in) :: this
    integer(int32),intent(in) :: StemID,InterNodeID
    integer(int32) :: ret,i,buf,PerioleID

    ret = 0
    do i=1, size(this%Stem,1)
        buf = this%searchPetiole(StemID=StemID,InterNodeID=InterNodeID,PetioleID=i)
        if(buf >=1)then
            ret = ret + 1
        else
            return
        endif
    enddo

end function
! #########################################################




! #########################################################
function maxleafIDSoybean(this,StemID,InterNodeID,PetioleID) result(ret)
    class(Soybean_),intent(in) :: this
    integer(int32),intent(in) :: StemID,InterNodeID,PetioleID
    integer(int32) :: ret,i,buf,PerioleID

    ret = 0
    do i=1, size(this%Leaf,1)
        buf = this%searchLeaf(StemID=StemID,InterNodeID=InterNodeID,PetioleID=PetioleID,&
            LeafID=i)
        if(buf >=1)then
            ret = ret + 1
        else
            return
        endif
    enddo

end function
! #########################################################

! ################################################################

subroutine growStemSoybean(this,StemID,InterNodeID,dt)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: stemID,InterNodeID
    real(real64),intent(in) :: dt
    real(real64) :: current_length
    integer(int32) :: i,j,node_id

    node_id = 0
    do i=1,size(this%stem,1)
        if(this%stem(i)%stemID==StemID)then
            if(this%stem(i)%InterNodeID==InterNodeID)then
               node_id = i 
            endif
        endif
    enddo
    if(node_id==0)then
        print *, "resizeStemSoybean 404 Not Found."
        return
    endif
    

    call this%stem(node_id)%grow(dt)
    call this%update()



end subroutine
! #############################################
subroutine setFinalInternodeLengthSoybean(this,Length,StemID)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: StemID
    real(real64),intent(in)   :: Length(:)

    if(.not.allocated(this%InterNodeInfo) )then
        allocate(this%InterNodeInfo(0:this%MaxBranchNum) )
    endif

    this%InterNodeInfo(StemID)%FinalInterNodeLength = Length

end subroutine

! #############################################
subroutine setFinalPetioleLengthSoybean(this,Length,StemID)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: StemID
    real(real64),intent(in)   :: Length(:)

    if(.not.allocated(this%InterNodeInfo) )then
        allocate(this%InterNodeInfo(0:this%MaxBranchNum) )
    endif

    this%InterNodeInfo(StemID)%FinalPetioleLength = Length

end subroutine

! #############################################
subroutine setFinalLeafLengthSoybean(this,Length,StemID)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: StemID
    real(real64),intent(in)   :: Length(:)

    if(.not.allocated(this%InterNodeInfo) )then
        allocate(this%InterNodeInfo(0:this%MaxBranchNum) )
    endif

    this%InterNodeInfo(StemID)%FinalLeafLength = Length

end subroutine

! #############################################
subroutine setFinalLeafWidthSoybean(this,Width,StemID)
    class(Soybean_),intent(inout) :: this
    integer(int32),intent(in) :: StemID
    real(real64),intent(in)   :: Width(:)

    if(.not.allocated(this%InterNodeInfo) )then
        allocate(this%InterNodeInfo(0:this%MaxBranchNum) )
    endif

    this%InterNodeInfo(StemID)%FinalLeafWidth = Width

end subroutine

! ################################################################

function getYoungModulusFieldSoybean(this) result(YoungModulus)
    class(Soybean_),intent(inout) :: this
    real(real64),allocatable :: YoungModulus(:)
    integer(int32),allocatable :: ElementList(:,:)
    integer(int32) :: TYPE_IDX, DOMAIN_IDX, ELEMENT_IDX, i

    ElementList = this%getElementList()
    YoungModulus = zeros(size(ElementList,1))

    do i=1,size(ElementList,1)
        TYPE_IDX = ElementList(i,1)
        DOMAIN_IDX = ElementList(i,2)
        ELEMENT_IDX = ElementList(i,3)
        if(TYPE_IDX == this%TYPE_STEM)then
            YoungModulus(i) = this%stem(DOMAIN_IDX)%YoungModulus(ELEMENT_IDX)
        elseif(TYPE_IDX == this%TYPE_LEAF)then
            YoungModulus(i) = this%LEAF(DOMAIN_IDX)%YoungModulus(ELEMENT_IDX)
        elseif(TYPE_IDX == this%TYPE_ROOT)then
            YoungModulus(i) = this%ROOT(DOMAIN_IDX)%YoungModulus(ELEMENT_IDX)
        endif
    enddo
    
end function

! ################################################################

! ################################################################
function getPoissonRatioFieldSoybean(this) result(PoissonRatio)
    class(Soybean_),intent(inout) :: this
    real(real64),allocatable :: PoissonRatio(:)
    integer(int32),allocatable :: ElementList(:,:)
    integer(int32) :: TYPE_IDX, DOMAIN_IDX, ELEMENT_IDX, i

    ElementList = this%getElementList()
    PoissonRatio = zeros(size(ElementList,1))

    do i=1,size(ElementList,1)
        TYPE_IDX = ElementList(i,1)
        DOMAIN_IDX = ElementList(i,2)
        ELEMENT_IDX = ElementList(i,3)
        if(TYPE_IDX == this%TYPE_STEM)then
            PoissonRatio(i) = this%stem(DOMAIN_IDX)%PoissonRatio(ELEMENT_IDX)
        elseif(TYPE_IDX == this%TYPE_LEAF)then
            PoissonRatio(i) = this%LEAF(DOMAIN_IDX)%PoissonRatio(ELEMENT_IDX)
        elseif(TYPE_IDX == this%TYPE_ROOT)then
            PoissonRatio(i) = this%ROOT(DOMAIN_IDX)%PoissonRatio(ELEMENT_IDX)
        endif
    enddo
    
end function

! ################################################################

! ################################################################
function getDensityFieldSoybean(this) result(Density)
    class(Soybean_),intent(inout) :: this
    real(real64),allocatable :: Density(:)
    integer(int32),allocatable :: ElementList(:,:)
    integer(int32) :: TYPE_IDX, DOMAIN_IDX, ELEMENT_IDX, i

    ElementList = this%getElementList()
    Density = zeros(size(ElementList,1))

    do i=1,size(ElementList,1)
        TYPE_IDX = ElementList(i,1)
        DOMAIN_IDX = ElementList(i,2)
        ELEMENT_IDX = ElementList(i,3)
        if(TYPE_IDX == this%TYPE_STEM)then
            Density(i) = this%stem(DOMAIN_IDX)%Density(ELEMENT_IDX)
        elseif(TYPE_IDX == this%TYPE_LEAF)then
            Density(i) = this%LEAF(DOMAIN_IDX)%Density(ELEMENT_IDX)
        elseif(TYPE_IDX == this%TYPE_ROOT)then
            Density(i) = this%ROOT(DOMAIN_IDX)%Density(ELEMENT_IDX)
        endif
    enddo
    
end function
! #####################################################################

function getElementListSoybean(this,x_min,x_max,y_min,y_max,z_min,z_max,debug) result(ElementList)
    class(Soybean_),intent(inout) :: this
    integer(int32),allocatable :: ElementList(:,:)
    integer(int32),allocatable :: obj_type(:),obj_idx(:),elem_idx(:)
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
    logical,optional,intent(in) :: debug
    logical :: do_debug
    integer(int32) :: idx,n,m

    do_debug = input(default=.false.,option=debug)

    !ElementList(idx, [ObjType, ObjID, ElementID] )
    allocate(elem_idx(0) )
    allocate(obj_type(0) )
    allocate(obj_idx(0) )

    if(allocated(this%stem) )then
        do idx=1,size(this%stem)
            if(this%stem(idx)%femdomain%empty() )cycle
            m = size(elem_idx)
            elem_idx = &
                elem_idx // this%stem(idx)%femdomain%mesh%getElementList(&
                xmin=x_min,xmax=x_max,ymin=y_min,ymax=y_max,zmin=z_min,zmax=z_max)

            obj_idx = obj_idx // idx*int(eyes( size(elem_idx)- m))
        enddo

        if(do_debug)then
            print *, "[o] STEM"
        endif
    else
        if(do_debug)then
            print *, "NO STEM"
        endif
    endif
    
    ! debug

    
    obj_type = obj_type // this%TYPE_STEM*int(eyes(size(elem_idx)))

    if(allocated(this%leaf) )then
        do idx=1,size(this%leaf)
            if(this%leaf(idx)%femdomain%empty() )cycle
            m = size(elem_idx)
            elem_idx = &
                elem_idx // this%leaf(idx)%femdomain%mesh%getElementList(&
                xmin=x_min,xmax=x_max,ymin=y_min,ymax=y_max,zmin=z_min,zmax=z_max)
            obj_idx = obj_idx // idx*int(eyes( size(elem_idx)- m))
        enddo

        if(do_debug)then
            print *, "[o] LEAF"
        endif
    else
        if(do_debug)then
            print *, "NO LEAF"
        endif
    endif

    n = size(obj_type)
    obj_type = obj_type // this%TYPE_LEAF*int(eyes(size(elem_idx)-n))
    

    if(allocated(this%root) )then
        do idx=1,size(this%root)
            if(this%root(idx)%femdomain%empty() )cycle
            m = size(elem_idx)
            elem_idx = &
                elem_idx // this%root(idx)%femdomain%mesh%getElementList(&
                xmin=x_min,xmax=x_max,ymin=y_min,ymax=y_max,zmin=z_min,zmax=z_max)
            obj_idx = obj_idx // idx*int(eyes( size(elem_idx)- m))
        enddo

        if(do_debug)then
            print *, "[o] ROOT"
        endif
    else
        if(do_debug)then
            print *, "NO ROOT"
        endif
    endif
    n = size(obj_type)
    obj_type = obj_type // this%TYPE_ROOT*int(eyes(size(elem_idx)-n))
    
    
    ElementList = zeros( size(elem_idx),3 )
    ElementList(:,1) = obj_type
    ElementList(:,2) = obj_idx
    ElementList(:,3) = elem_idx

end function

! ################################################################


! ################################################################
function getStressFieldSoybean(this,displacement,i,j,option) result(StressField)
    class(Soybean_),intent(inout) :: this
    real(real64),intent(in) :: displacement(:)
    integer(int32),optional,intent(in) :: i,j
    character(*),optional,intent(in) :: option

    real(real64),allocatable :: StressField(:)
    integer(int32) :: ii,jj, n, obj_idx

    StressField = zeros(0)
    n = 1
    if(allocated(this%stem) )then
        do obj_idx=1,size(this%stem)
            if(this%stem(obj_idx)%femdomain%mesh%empty() ) cycle
            StressField = StressField // &
                this%stem(obj_idx)%femdomain%getElementCauchyStress(&
                displacement=displacement(n:n+this%stem(obj_idx)%femdomain%nn()&
                    *this%stem(obj_idx)%femdomain%nd()-1 ),&
                E = this%stem(obj_idx)%YoungModulus(:),&
                v = this%stem(obj_idx)%PoissonRatio(:) ,i=i,j=j,option=option)
                n = n + this%stem(obj_idx)%femdomain%nn()&
                *this%stem(obj_idx)%femdomain%nd()
        enddo
    endif


    if(allocated(this%leaf) )then
        do obj_idx=1,size(this%leaf)
            if(this%leaf(obj_idx)%femdomain%mesh%empty() ) cycle
            StressField = StressField // &
                this%leaf(obj_idx)%femdomain%getElementCauchyStress(&
                displacement=displacement(n:n+this%leaf(obj_idx)%femdomain%nn()&
                *this%leaf(obj_idx)%femdomain%nd()-1 ),&
                E = this%leaf(obj_idx)%YoungModulus(:),&
                v = this%leaf(obj_idx)%PoissonRatio(:) ,i=i,j=j,option=option)
                n = n + this%leaf(obj_idx)%femdomain%nn()&
                *this%leaf(obj_idx)%femdomain%nd()
        enddo
    endif

    if(allocated(this%root) )then
        do obj_idx=1,size(this%root)
            if(this%root(obj_idx)%femdomain%mesh%empty() ) cycle
            StressField = StressField // &
                this%root(obj_idx)%femdomain%getElementCauchyStress(&
                displacement=displacement(n:n+this%root(obj_idx)%femdomain%nn()&
                *this%root(obj_idx)%femdomain%nd()-1 ),&
                E = this%root(obj_idx)%YoungModulus(:),&
                v = this%root(obj_idx)%PoissonRatio(:) ,i=i,j=j,option=option)
                n = n + this%root(obj_idx)%femdomain%nn()&
                *this%root(obj_idx)%femdomain%nd()
        enddo
    endif

end function
! ################################################################

subroutine export_eigSoybean(this,name,Frequency,ModeVectors,stress_type)
    class(Soybean_),intent(inout) :: this
    character(*),intent(in) :: Name
    character(*),optional,intent(in) :: stress_type
    real(real64),intent(in) :: Frequency(:),ModeVectors(:,:)
    real(real64),allocatable :: displacement(:),stress(:)
    integer(int32) :: i,j 
    type(IO_) :: f

    call f%open(name + ".csv","w")
    call f%write("# Mode, Eigenfrequency (Hz)")
    do i=1,10
        displacement = ModeVectors(:,i)
        do j=1,36
            call this%deform(displacement =  cos(radian(j*10.0d0) ) * displacement )

            if(present(stress_type) )then
                stress = this%getStressField(Displacement=cos(radian(j*10.0d0) ) * displacement,option=stress_type)
                call this%vtk(name + zfill(i,3)+"_"+zfill(j,4),single_file = .true.,scalar_field=stress)
            else
                call this%vtk(name + zfill(i,3)+"_"+zfill(j,4),single_file = .true.)
            endif
            call this%deform(displacement = -cos(radian(j*10.0d0) ) *  displacement)
        enddo
        write(f%fh,*) str(i) +" , " , Frequency(i)
    enddo
    call f%close()
    
end subroutine

! ################################################################



end module  