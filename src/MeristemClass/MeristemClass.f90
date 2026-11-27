module MeristemClass
   use, intrinsic :: iso_fortran_env
   use LeafClass
   use EnvironmentClass
   implicit none

    !integer(int32),public :: PF_Meristem_SHAPE_CYLINDER     = 2
    !integer(int32),public :: PF_Meristem_SHAPE_HYPERBOLOID  = 3
   
   integer(int32),public :: PF_MERISTEM_TYPE_SHOOT = 0
   integer(int32),public :: PF_MERISTEM_TYPE_ROOT  = 1
    
   type :: Meristem_
      type(FEMDomain_)    ::  FEMDomain
      type(Stem_),allocatable :: stem(:)
      type(LeafSet_),allocatable :: leafset(:) ! petioles and leaves
      type(Meristem_),allocatable :: branch_meristem(:)
      
      integer(int32) :: num_leaf_per_leafset = 3
      integer(int32) :: num_leafset_per_stem = 1
      integer(int32),allocatable :: leafset2stem(:,:)

      ! type(root_),allocatable :: root(:)
      
      integer(int32),allocatable :: stem2stem(:,:)
      integer(int32),allocatable :: leaf2stem(:,:)

      integer(int32)      :: cross_section_shape = 1 ! PF_Meristem_SHAPE_RECTANGULAR
      
      real(real64)             ::  rot_x = 0.0d0
      real(real64)             ::  rot_y = 0.0d0
      real(real64)             ::  rot_z = 0.0d0
      real(real64)             ::  disp_x = 0.0d0
      real(real64)             ::  disp_y = 0.0d0
      real(real64)             ::  disp_z = 0.0d0
      integer(int32)           ::  EdgeNodeID(4)
      integer(int32)           ::  EdgeElemID(4)
      
      integer(int32), allocatable  :: I_planeNodeID(:)
      integer(int32), allocatable  :: I_planeElementID(:)
      integer(int32), allocatable  :: II_planeNodeID(:)
      integer(int32), allocatable  :: II_planeElementID(:)
      integer(int32)  :: A_PointNodeID
      integer(int32)  :: B_PointNodeID
      integer(int32)  :: C_PointNodeID
      integer(int32)  :: D_PointNodeID

      integer(int32)  :: A_PointElementID
      integer(int32)  :: B_PointElementID
      integer(int32)  :: xnum = 10
      integer(int32)  :: ynum = 10
      integer(int32)  :: znum = 10

      ! branch
      integer(int32) :: stem_idx ! the branch is branched from stem with stem_idx
      integer(int32) :: leafset_idx ! the branch is branched from leafset with leafset_idx
      

      ! meristem parameters
      integer(int32)  :: num_shoot_stem_node = 2
      real(real64) :: slight_overlap_epsilon = dble(1.0e-3)
      real(real64) :: top_meristem_aspect_ratio = 0.50d0 ! if length/width is greater than this value, internodes are divided.
      real(real64) :: leaf_aspect_ratio = 0.50d0 ! width/length
      real(real64) :: leaf_max_angle_deg = 90.0d0!
      real(real64) :: leaf_curvature = 0.0d0
      
      integer(int32) :: max_branch_num = 0
      integer(int32) :: apical_dominance_num = 2

      ! position in a whole structure (single plant)
      integer(int32) :: MeristemID = -1
      integer(int32) :: InterNodeID = -1

      logical :: already_grown = .false.

      ! physical parameter
      real(real64), allocatable :: DryDensity(:)  ! element-wise
      real(real64), allocatable :: WaterContent(:)! element-wise

      ! For deformation analysis
      real(real64), allocatable :: YoungModulus(:)! element-wise
      real(real64), allocatable :: PoissonRatio(:)! element-wise
      real(real64), allocatable :: CrossSectionalYoungModulus(:) !element-wise
      real(real64), allocatable :: Density(:)     ! element-wise
      real(real64), allocatable :: CarbonDiffusionCoefficient(:) ! element-wise
      real(real64), allocatable :: Stress(:, :, :)     ! Gauss point-wise
      real(real64), allocatable :: Displacement(:, :) ! node-wise, three dimensional
      real(real64), allocatable :: BoundaryTractionForce(:, :) ! node-wise, three dimensional
      real(real64), allocatable :: BoundaryDisplacement(:, :) ! node-wise, three dimensional

      integer(int32)             ::  Division

      ! growth parameters
      ! >>>>>>>>>>>
      real(real64) :: K_L ! Maximum value for length of a single internode (m)
      real(real64) :: K_R ! Maximum value for radius of a single internode (m)
      real(real64) :: T_L ! time constant for longitudinal growth of a single internode (m)
      real(real64) :: T_R ! time constant for cross-sectional growth of a single internode (m)
      real(real64) :: temp_time_limit ! max. stem generation speed (day/node)

      integer(int32) :: species = 1  ! soybean

      real(real64),allocatable :: leafset_params(:,:)
      real(real64),allocatable :: leafset_init_params(:)
      real(real64) :: size_coeff(1) = [10.0d0]
      real(real64) :: stem_size_coeff(1) = [10.0d0]

      ! phenotype parameters
      real(real64) :: leaf_directional_angle = 120.0d0 ! 
      real(real64) :: last_leaf_direction = 0.0d0
      real(real64) :: base_petiole_length_ratio = 3.0d0 ! (primary petiole length)/(n-th petiole length(n>1))

       ! <<<<<<<<<<<
      logical :: material_is_set

      type(Meristem_), pointer ::  pMeristem => null()



      ! physiological factor
      real(real64) :: R_d = 1.0d0 ! 暗呼吸速度, mincro-mol/m-2/s
      real(real64) :: default_CarbonDiffusionCoefficient = 0.0010d0 ! ソースの拡散係数 mincro-mol/m^2/m/s

      real(real64) :: average_drydensity = 0.30d0 ! dry bulk density, g/cm^3 = t/m^3
      real(real64) :: temp_range(1:2) = [18.0d0, 35.0d0] ! temperature range for growth

      ! current temperature
      real(real64) :: temperature = 28.0d0 ! deg. Celsius temperature
      

      ! photosynthesis simulation
      real(real64),allocatable :: Photosynthate_n(:)


      ! default values for aging of leaves in terms of photosynthesis
      ! 0 ~ 10 days (lighting hour : 8hr/day) : 100 %
      ! 10 ~ 40 days (lighting hour : 8hr/day): 100 % -> 0 %
      ! 30 days~ (lighting hour: 8hr/day): 0 %
      real(real64) :: stem_aging_params(1:2) = [30.0d0, 50.0d0]*60.0d0*60.0d0*24.0d0
      real(real64) :: peti_aging_params(1:2) = [30.0d0, 50.0d0]*60.0d0*60.0d0*24.0d0
      real(real64) :: leaf_aging_params(1:2) = [30.0d0, 50.0d0]*60.0d0*60.0d0*24.0d0

   contains
         procedure, public :: Init => initMeristem
         procedure,public :: update  => updateMeristem
         procedure,public :: grow_internode  => grow_internodeMeristem
         
         procedure,public :: meristem_division => meristem_division_MeristemC
         procedure,public :: update_leafset_params => update_leafset_params_MeristemC

         procedure,public :: set_branch => set_branch_MeristemC
         procedure,public :: setPoints => setPoints_MeristemClass

         procedure,public :: grow => grow_MeristemClass
         procedure,public :: trial_grow => trial_grow_MeristemClass
         
         ![UNDER DEVELOPMENT]
         procedure,public :: getPhotoSynthesis => getPhotoSynthesis_meristem
         

         !procedure, pass :: change_length_or_width_Meristem
         !procedure, pass :: grow_by_pressure_Meristem
         !generic, public :: grow => grow_by_pressure_Meristem
         !generic, public :: change_length_or_width => change_length_or_width_Meristem

         procedure, public :: resize => resizeMeristem
         procedure, public :: move => moveMeristem
         procedure, public :: rotate => rotateMeristem
         procedure, public :: connect => connectMeristem

         procedure, public :: numStemAndPeti => numStemAndPeti_Meristem
         procedure, public :: numLeaf => numLeaf_Meristem


         ! check condition
         procedure, public :: empty => emptyMeristem
         procedure, public :: getCoordinate => getCoordinateMeristem
         procedure, public :: getLength => getLengthMeristem
         procedure, public :: getAngles => getAngles_MeristemClass
         procedure, public :: getWidth => getWidthMeristem
         procedure, public :: getVolume => getVolumeMeristem
         procedure, public :: getVolumePerElement => getVolumePerElement_Meristem
         procedure, public :: get_all_femdomain => get_all_femdomain_MeristemClass
         procedure, public :: get_all_femdomain_ptr => get_all_femdomain_ptr_Meristem

         ! get stat
         procedure, public :: get_mainstem_length => get_mainstem_length_Meristem
         procedure, public :: get_mainstem_num_node => get_mainstem_num_node_Meristem
         procedure, public :: get_num_branch => get_num_branch_Meristem

         
         ! stats
         procedure, public :: getLeafArea => getLeafArea_Meristem
         procedure, public :: getNumberOfInternode => getNumberOfInternode_Meristem
         procedure, public :: getNumberOfBranch => getNumberOfBranch_Meristem
         
         
         procedure, public :: getStem => get_all_stem_Meristem
         procedure, public :: getPeti => get_all_peti_Meristem
         procedure, public :: getLeaf => get_all_leaves_Meristem
         procedure, public :: get_all_leaves => get_all_leaves_Meristem

         procedure, public :: get_stem_femdomain_ptr => get_stem_femdomain_ptr_Meristem
         procedure, public :: get_peti_femdomain_ptr => get_peti_femdomain_ptr_Meristem
         procedure, public :: get_leaf_femdomain_ptr => get_leaf_femdomain_ptr_Meristem
         
         
         procedure, public :: getLeafsetAngles => getLeafsetAngles_MeristemClass
         

         procedure, public :: ne => ne_MeristemClass ! number of FE-elements
         procedure, public :: nn => nn_MeristemClass ! number of FE-nodes

         !procedure, public :: FullyExpanded => FullyExpandedMeristem
         procedure, public :: gmsh => gmshMeristem
         procedure, public :: msh => mshMeristem
         procedure, public :: vtk => vtkMeristem
         procedure, public :: stl => stlMeristem
         procedure, public :: ply => plyMeristem
         

         !procedure, public :: export => exportMeristem
         procedure, public :: getBiomass => getBiomassMeristem
         procedure, public :: getPPFD => getPPFDMeristem
         procedure, public :: getPhotoSynthesisSpeedPerElem =>getPhotoSynthesisSpeedPerElemMer
         procedure, public :: getRespirationSpeedPerElem    => getRespirationSpeedPerElemMer
         procedure, public :: getHeight => getHeightMeristem

         procedure, public :: fall_old_leaves => fall_old_leaves_MeristemClass
         ! simulation
         procedure, public :: set_material => set_material_Meristem
         procedure, public :: sync => syncMeristem
         !procedure, public :: nn => nnMeristem
         !procedure, public :: ne => neMeristem
         procedure, public :: remove => removeMeristem
   end type

   interface operator(//)
      module procedure append_Meristem_thisect_vector
   end interface

   interface assignment(=)
      module procedure assign_meristemclass
   end interface

   interface ne
      module procedure ne_stem_vec,ne_leaf_vec
   end interface ne

contains

! ########################################
subroutine initMeristem(this, MERISTEM_TYPE, params, num_internode,dt,species)
   class(Meristem_), intent(inout) :: this
   integer(int32) :: MERISTEM_TYPE ! 0: shoot, 1: root
   character(*),optional,intent(in) :: species
   integer(int32),optional,intent(in) :: num_internode !
   real(real64),intent(in) :: dt
   real(real64),intent(in) :: params(:)

   type(IO_) :: Meristemconf, f
   character(200) :: fn, conf, line
   integer(int32), allocatable :: buf(:)
   real(real64) :: center_coord(1:3), dist_val
   type(FEMDomain_),allocatable :: femdomains(:)
   integer(int32) :: id, rmc, n, node_id, node_id2, elemid, blcount, i, j, stem_idx,m
   real(real64) :: loc(3),xx,yy,zz,theta,r,height,alpha,beta,radius,length
   real(real64),allocatable :: this_params(:)
   logical :: debug = .false.

   real(real64) :: K_L ! max length(m)
   real(real64) :: K_R ! max radius(m)
   real(real64) :: T_L ! Time constant for length (s)
   real(real64) :: T_R ! Time constant for radius (s)
   real(real64) :: temp_time_limit  ! max. stem generation speed (day/node)

   !real(real64) :: K_pL ! maximum size of each petiole internodes (m) 
   !real(real64) :: K_pR ! maximum size of each petiole radius (m) 
   !real(real64) :: K_lL ! maximum length of each leaf  (m) 
   !real(real64) :: T_pL ! Delay time (time constant) of each petiole internodes (s)
   !real(real64) :: T_pR ! Delay time (time constant) of each petiole radius (s)
   !real(real64) :: T_lL ! Delay time (time constant) for length of each leaf (s)

   
   this%leafset_init_params = params

   if(present(species))then
      this%species = species
   endif

   !this%my_time = 0.0d0
   this%material_is_set = .false.

   ! メッシュを生成
   this%K_L = params(1) ! max length(m)
   this%K_R = params(2) ! max radius(m)
   this%T_L = params(3) ! Time constant for length (s)
   this%T_R = params(4) ! Time constant for radius (s)
   this%temp_time_limit = params(5) !  max. stem generation speed (day/node)


   
   length = this%K_L
   radius = this%K_R
   
   if(MERISTEM_TYPE==PF_MERISTEM_TYPE_SHOOT)then
      ! cylindrical shape
      ! Wedding-cake model
      
      allocate(this%stem(this%num_shoot_stem_node))
      this%stem2stem = int(zeros(size(this%stem),size(this%stem)))
      height = 0.0d0
      do stem_idx=1,size(this%stem)
         this%stem(stem_idx)%CROSS_SECTION_SHAPE = PF_STEM_SHAPE_CYLINDER
         call this%stem(stem_idx)%init(x_num=this%xnum, y_num=this%ynum, z_num=this%znum)
         ! radius ratio: alpha
         ! length ratio: beta
         call this%stem(stem_idx)%move(z=-this%stem(stem_idx)%FEMDomain%zmin())
         alpha = dble(size(this%stem)-stem_idx+1)/dble(size(this%stem))
         beta  = dble(size(this%stem)-stem_idx+1)*dble(size(this%stem)-stem_idx+1)&
            /dble((dot_product([(i,i=1,size(this%stem))],[(i,i=1,size(this%stem))])))
         
         call this%stem(stem_idx)%resize(&
               x=2.0d0*radius*alpha,&
               y=2.0d0*radius*alpha,&
               z=length*beta &
            )
         
         if(stem_idx>=2)then
            call this%stem(stem_idx)%move(z=this%stem(stem_idx-1)%FEMDomain%zmax()&
               - this%slight_overlap_epsilon*length)
         endif

      enddo

      do stem_idx=size(this%stem),2,-1
         call this%stem(stem_idx)%connect("=>", this%stem(stem_idx-1))
         this%stem2stem(stem_idx, stem_idx-1) = 1
      enddo

      ! reset shape and create initial meristems
      do stem_idx=1,size(this%stem)
         call this%grow_internode(idx=stem_idx,params=params(:),dt=dt,&
            size_factor_function=default_size_factor_function)
      enddo
      !do leafset_idx=1,size(this%leafset)
      !   call this%leafset(leafset_idx)%grow_peti_and_leaf(params=params(6:11),dt=dt)
      !enddo

      ! set leafsets
      if(allocated(this%leafset))then
         deallocate(this%leafset)
      endif
      allocate(this%leafset(size(this%stem)))
      allocate(this%leafset_params(size(this%stem),size(params)))
      this%leafset2stem = int(zeros(this%num_leafset_per_stem*size(this%stem),size(this%stem)))
      
      do i=1,size(this%leafset_params,1)
         this%leafset_params(i,:) = params(:)
      enddo

      do stem_idx=1,size(this%stem) ! no leaves in last stem
         call this%grow_internode(idx=stem_idx,params=params,dt=dt,&
            size_factor_function=default_size_factor_function)
         do i=1,this%num_leafset_per_stem
            n = this%num_leafset_per_stem*(stem_idx-1) + i
            this%last_leaf_direction = this%last_leaf_direction + this%leaf_directional_angle
            this%leafset(n)%leaf_aspect_ratio = this%leaf_aspect_ratio
            this%leafset(n)%peti_x_num = this%xnum
            this%leafset(n)%peti_y_num = this%ynum
            this%leafset(n)%peti_z_num = this%znum
            this_params = this%leafset_params(n,6:11)
            call this%leafset(n)%init( &
               num_leaf=this%num_leaf_per_leafset,&
               params=this_params,&
               species=this%species,&
               direction=radian(this%last_leaf_direction),&
               dt=dt,&
               base_petiole_length_ratio=this%base_petiole_length_ratio)
            
            !call this%leafset(n)%rotate(x = radian(90.0d0))

            call this%leafset(n)%peti(1)%connect("=>", this%stem(stem_idx))
            this%leafset2stem( n,stem_idx ) = 1
         enddo
      enddo
      call this%update()
      
    else
         ! cylindrical shape
         call this%FEMdomain%create(meshtype="Cylinder", x_num=this%xnum, y_num=this%ynum, z_num=this%znum, &
            x_len=radius, y_len=radius, z_len=length)
        
         ! reshape
         do node_id=1,this%femdomain%nn()
            xx = this%femdomain%mesh%nodcoord(node_id,1)
            yy = this%femdomain%mesh%nodcoord(node_id,2)
            zz = this%femdomain%mesh%nodcoord(node_id,3)
            theta = zz/length ! theta \in [0.0, 1.0]
            
        
            ! Meristem has catenary shape (Fujiwara et al, 2021, Development)
            ! https://doi.org/10.1242/dev.196253
            r = sqrt(theta+0.10d0)
            xx = xx * r/radius
            yy = yy * r/radius
        
            this%femdomain%mesh%nodcoord(node_id,1) = xx
            this%femdomain%mesh%nodcoord(node_id,2) = yy
            
            !call this%femdomain%rotate(x=radian(180.0d0))
            
        enddo 
        call this%femdomain%vtk("test_root")
        ! initialize physical parameters
       this%DryDensity = zeros(this%FEMDomain%ne())
       this%watercontent = zeros(this%FEMDomain%ne())

       !if (present(config)) then
       !   this%DryDensity(:) = freal(Meristemconf%parse(conf, key1="drydensity"))
       !   this%watercontent(:) = freal(Meristemconf%parse(conf, key1="watercontent"))
       !end if

       ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
       this%I_planeNodeID = this%FEMdomain%mesh%getNodeList(zmax=0.0d0)
       this%I_planeElementID = this%FEMdomain%mesh%getElementList(zmax=0.0d0)

       ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
       this%II_planeNodeID = this%FEMdomain%mesh%getNodeList(zmin=length)
       this%II_planeElementID = this%FEMdomain%mesh%getElementList(zmin=length)

       center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:), 1)) &
                         /size(this%I_planeNodeID)
       center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:), 2)) &
                         /size(this%I_planeNodeID)
       center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:), 3)) &
                         /size(this%I_planeNodeID)
       dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(1), :) - center_coord)
       this%A_PointNodeID = this%I_planeNodeID(1)

       do i = 2, size(this%I_planeNodeID)
          if (norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i), :) - center_coord) < dist_val) then
             this%A_PointNodeID = this%I_planeNodeID(i)
             dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i), :) - center_coord)
          end if
       end do


       center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:), 1)) &
                         /size(this%I_planeNodeID)
       center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:), 2)) &
                         /size(this%I_planeNodeID)
       center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:), 3)) &
                         /size(this%I_planeNodeID)

         dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(1), :) - center_coord)
         this%B_PointNodeID = this%II_planeNodeID(1)

       do i = 2, size(this%II_planeNodeID)
          if (norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i), :) - center_coord) < dist_val) then
             this%B_PointNodeID = this%II_planeNodeID(i)
             dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i), :) - center_coord)
          end if
       end do

       center_coord(1) = maxval(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:), 1))

       center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:), 2)) &
                         /size(this%I_planeNodeID)

       center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:), 3)) &
                         /size(this%I_planeNodeID)

       dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(1), :) - center_coord)
       this%C_PointNodeID = this%I_planeNodeID(1)

       do i = 2, size(this%I_planeNodeID)
          if (norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i), :) - center_coord) < dist_val) then
             this%C_PointNodeID = this%I_planeNodeID(i)
             dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i), :) - center_coord)
          end if
       end do

       center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:), 1)) &
                         /size(this%II_planeNodeID)
       center_coord(2) = maxval(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:), 2))
       center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:), 3)) &
                         /size(this%II_planeNodeID)

       dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(1), :) - center_coord)
       this%D_PointNodeID = this%II_planeNodeID(1)
       do i = 2, size(this%II_planeNodeID)
          if (norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i), :) - center_coord) < dist_val) then
             this%D_PointNodeID = this%II_planeNodeID(i)
             dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i), :) - center_coord)
          end if
       end do


       buf = this%FEMDomain%mesh%getElementList( &
             xmin= - radius/dble(this%xnum)*3.0d0, &
             xmax=   radius/dble(this%xnum)*3.0d0, &
             ymin= - radius/dble(this%ynum)*3.0d0, &
             ymax=   radius/dble(this%ynum)*3.0d0, &
             zmax=0.0d0)
       !this%A_PointElementID = buf(1)
       this%A_PointElementID = median(buf)

       buf = this%FEMDomain%mesh%getElementList( &
             xmin= - radius/dble(this%xnum)*3.0d0, &
             xmax=   radius/dble(this%xnum)*3.0d0, &
             ymin= - radius/dble(this%ynum)*3.0d0, &
             ymax=   radius/dble(this%ynum)*3.0d0, &
             zmin=length)

       !this%B_PointElementID = buf(1)
       this%B_PointElementID = median(buf)
       if (debug) print *, this%A_PointNodeID
       if (debug) print *, this%B_PointNodeID
       if (debug) print *, this%A_PointElementID
       if (debug) print *, this%B_PointElementID

       call this%FEMDomain%rotate(x=radian(180.0d0))

   endif


end subroutine
! ########################################
!   subroutine resize(this, x, y, z)
!      class(Meristem_), intent(inout) :: this
!      real(real64), optional, intent(in) :: x, y, z
!
!      call this%femdomain%resize(x, y, z)
!
!   end subroutine

!! ########################################
!   subroutine exportMeristem(this, FileName, MeristemID)
!      class(Meristem_), intent(in)::this
!      character(*), intent(in) :: FileName
!      integer(int32), optional, intent(inout) :: MeristemID
!      real(real64) :: radius
!
!      radius = this%K_R
!
!      open (13, file=FileName)
!      write (13, '(A)') "//+"
!      write (13, '(A)') 'SetFactory("OpenCASCADE");'
!      write (13, *) "Cylinder(", input(default=1, option=MeristemID), ") = {", &
!         this%center_bottom(1), ",", this%center_bottom(2), ",", this%center_bottom(3), ",", &
!         this%center_top(1) - this%center_bottom(1), ",", this%center_top(2) - this%center_bottom(2), ",", &
!         this%center_top(3) - this%center_bottom(3), ",", &
!         radius, ", 2*Pi};"
!      close (13)
!      MeristemID = MeristemID + 1
!
!   end subroutine
!! ########################################

! ########################################
function getAngles_MeristemClass(this) result(ret)
   class(Meristem_),intent(in) :: this
   real(real64) :: ret(1:3)

   ret(1) = this%rot_x
   ret(2) = this%rot_y
   ret(3) = this%rot_z

end function
! ########################################

! ########################################
subroutine rotateMeristem(this, x, y, z, reset)
      class(Meristem_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      logical, optional, intent(in) :: reset
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      if (present(reset)) then
         print *, "[ERROR] > rotateMeristem :: meristem%femdomain is regacy. It will be deleted."
         stop
!         if (reset .eqv. .true.) then
!            call this%femdomain%rotate(-this%rot_x, -this%rot_y, -this%rot_z)
!            this%rot_x = 0.0d0
!            this%rot_y = 0.0d0
!            this%rot_z = 0.0d0
!         end if
      end if

      
      if(.not. this%femdomain%mesh%empty())then
         print *, "[ERROR] > rotateMeristem :: meristem%femdomain is regacy. It will be deleted."
         stop
!         origin1 = this%getCoordinate("A")
!         call this%femdomain%rotate(x, y, z)
!         this%rot_x = this%rot_x + input(default=0.0d0, option=x)
!         this%rot_y = this%rot_y + input(default=0.0d0, option=y)
!         this%rot_z = this%rot_z + input(default=0.0d0, option=z)
!         origin2 = this%getCoordinate("A")
!         disp = origin1
!         disp(:) = origin1(:) - origin2(:)
!         call this%femdomain%move(x=disp(1), y=disp(2), z=disp(3))
      endif
      
      !if(allocated(this%leafset))then
      call rotate_all_femdomain_Meristem(this,x=x,y=y,z=z,reset=reset)
      !endif

   end subroutine
! ########################################

! ########################################
subroutine moveMeristem(this, x, y, z, reset)
      class(Meristem_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      logical, optional, intent(in) :: reset

      integer(int32) :: stem_idx,leafset_idx
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      if(.not.this%femdomain%empty())then
         print *, "[CAUTION] > moveMeristem :: meristem%femdomain is regacy. It will be deleted."
!         if (present(reset)) then
!            if (reset .eqv. .true.) then
!               call this%femdomain%move(-this%disp_x, -this%disp_y, -this%disp_z)
!               this%disp_x = 0.0d0
!               this%disp_y = 0.0d0
!               this%disp_z = 0.0d0
!            end if
!         end if
!
!         call this%femdomain%move(x, y, z)
!         this%disp_x = this%disp_x + input(default=0.0d0, option=x)
!         this%disp_y = this%disp_y + input(default=0.0d0, option=y)
!         this%disp_z = this%disp_z + input(default=0.0d0, option=z)
      endif

      if(allocated(this%stem))then
         if(present(reset))then
            print *, "[ERROR] :: moveMeristem :: reset option is regacy"
            stop
         else
            do stem_idx=1,size(this%stem)
               call this%stem(stem_idx)%move(x=x,y=y,z=z)
            enddo
         endif
      endif


      if(allocated(this%leafset))then
         if(present(reset))then
            print *, "[ERROR] :: moveMeristem :: reset option is regacy"
            stop
         else
            do leafset_idx=1,size(this%leafset)
               call this%leafset(leafset_idx)%move(x=x,y=y,z=z)
            enddo
         endif
      endif

      

      this%disp_x = this%disp_x + input(default=0.0d0, option=x)
      this%disp_y = this%disp_y + input(default=0.0d0, option=y)
      this%disp_z = this%disp_z + input(default=0.0d0, option=z)

   end subroutine
! ########################################

   subroutine connectMeristem(this, direct, Meristem)
      class(Meristem_), intent(inout) :: this, Meristem
      character(2), intent(in) :: direct
      real(real64), allocatable :: x1(:), x2(:), disp(:)

      if (direct == "->" .or. direct == "=>") then
         ! move this to connect Meristem (Meristem is not moved.)
         x1 = this%getCoordinate("A")
         x2 = Meristem%getCoordinate("B")
         disp = x2 - x1
         call this%move(x=disp(1), y=disp(2), z=disp(3))
      end if

      if (direct == "<-" .or. direct == "<=") then
         ! move this to connect Meristem (Meristem is not moved.)
         x1 = Meristem%getCoordinate("A")
         x2 = this%getCoordinate("B")
         disp = x2 - x1
         call Meristem%move(x=disp(1), y=disp(2), z=disp(3))
      end if
   end subroutine

! ########################################
   function getCoordinateMeristem(this, nodetype) result(ret)
      class(Meristem_), intent(in) :: this
      character(*), intent(in) :: nodetype
      real(real64), allocatable :: ret(:)
      integer(int32) :: dimnum, n, i

      dimnum = size(this%femdomain%mesh%nodcoord, 2)

      allocate (ret(dimnum))
      ret(:) = 0.0d0
      if (nodetype == "A" .or. nodetype == "a") then

         
         if(this%femdomain%mesh%empty())then

            ret = this%stem(1)%femdomain%mesh%nodcoord(this%stem(1)%A_PointNodeID, :)
         else
            ! 20220701 this may be correct
            ret = this%femdomain%mesh%nodcoord(this%A_PointNodeID, :)
         endif
         return

         n = size(this%I_planeNodeID)
         if (n == 0) then
            print *, "ERROR >> getCoordinateMeristem >> size(this%I_planeNodeID) = 0"
         end if
         if (.not. allocated(this%I_planeNodeID)) then

            print *, "ERROR >> getCoordinateMeristem >> .not. allocated(this%I_planeNodeID) "

         end if
         do i = 1, n
            ret(:) = ret(:) + this%femdomain%mesh%nodcoord(this%I_planeNodeID(i), :)
         end do
         ret(:) = 1.0d0/dble(n)*ret(:)

         !ret = this%femdomain%mesh%nodcoord(this%A_PointNodeID,:)
      end if

      if (nodetype == "B" .or. nodetype == "b") then
         !ret = this%femdomain%mesh%nodcoord(this%B_PointNodeID,:)

         ! 20220701 this may be correct
         ret = this%femdomain%mesh%nodcoord(this%B_PointNodeID, :)
         return

         n = size(this%II_planeNodeID)
         if (n == 0) then
            print *, "ERROR >> getCoordinateMeristem >> size(this%II_planeNodeID) = 0"
         end if
         if (.not. allocated(this%I_planeNodeID)) then

            print *, "ERROR >> getCoordinateMeristem >> .not. allocated(this%II_planeNodeID) "

         end if
         do i = 1, n
            ret(:) = ret(:) + this%femdomain%mesh%nodcoord(this%II_planeNodeID(i), :)
         end do
         ret(:) = 1.0d0/dble(n)*ret(:)
      end if

   end function
! ########################################

   subroutine gmshMeristem(this, name)
      class(Meristem_), intent(inout) :: this
      character(*), intent(in) ::name
      if (this%femdomain%mesh%empty()) then
         return
      end if

      call this%femdomain%gmsh(Name=name)
   end subroutine

   subroutine mshMeristem(this, name)
      class(Meristem_), intent(inout) :: this
      character(*), intent(in) ::name
      if (this%femdomain%mesh%empty()) then
         return
      end if

      call this%femdomain%msh(Name=name)
   end subroutine
! ##############################################
   subroutine vtkMeristem(this, name, field_name,single_file,&
      scalar_field, vector_field, tensor_field)
      class(Meristem_), intent(inout) :: this
      character(*), intent(in) ::name
      character(*), optional, intent(in) ::field_name
      logical,optional,intent(in) :: single_file
      
      real(real64), optional, intent(in) :: scalar_field(:)
      real(real64), optional, intent(in) :: vector_field(:, :)
      real(real64), optional, intent(in) :: tensor_field(:, :, :)

      type(FEMDomain_) :: femdomain
      integer(int32) :: i,j
      

      if (.not. this%femdomain%mesh%empty()) then
         call this%femdomain%vtk(Name=name, field=field_name)
         return
      end if



      if (present(single_file)) then
         if (single_file) then
            ! export mesh for a single file
            femdomain = this%get_all_femdomain()
            !if (allocated(this%stem)) then
            !   do i = 1, size(this%stem)
            !      if (.not. this%stem(i)%femdomain%empty()) then
            !         femdomain = femdomain + this%stem(i)%femdomain
            !      end if
            !   end do
            !end if
            !
            !if (allocated(this%leafset)) then
            !   do i = 1, size(this%leafset)
            !      if(this%leafset(i)%is_empty())cycle
            !      do j=1, size(this%leafset(i)%peti)
            !         femdomain = femdomain + this%leafset(i)%peti(j)%femdomain
            !      enddo
            !      do j=1, size(this%leafset(i)%leaf)
            !         femdomain = femdomain + this%leafset(i)%leaf(j)%femdomain
            !      enddo
            !   end do
            !end if
!
            !if(allocated(this%branch_meristem))then
            !   do i=1,size(this%branch_meristem)
            !      femdomain = femdomain + this%branch_meristem(i)%get_all_femdomain()
            !   enddo
            !endif
            



            !if (allocated(this%leaf)) then
            !   do i = 1, size(this%leaf)
            !      if (.not. this%leaf(i)%femdomain%empty()) then
            !         femdomain = femdomain + this%leaf(i)%femdomain
            !      end if
            !   end do
            !end if

!            if (allocated(this%root)) then
!               do i = 1, size(this%root)
!                  if (.not. this%root(i)%femdomain%empty()) then
!                     femdomain = femdomain + this%root(i)%femdomain
!                  end if
!               end do
!            end if

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


      if(allocated(this%stem))then
         do i=1,size(this%stem)
            call this%stem(i)%vtk(Name=name+"_stem_"+str(i))
         enddo
      endif

      
      
   end subroutine
! ##############################################

   subroutine stlMeristem(this, name)
      class(Meristem_), intent(inout) :: this
      character(*), intent(in) ::name
      if (this%femdomain%mesh%empty()) then
         return
      end if

      call this%femdomain%ply(Name=name)
   end subroutine

! ########################################
   subroutine plyMeristem(this, name)
      class(Meristem_), intent(inout) :: this
      character(*), intent(in) ::name
      if (this%femdomain%mesh%empty()) then
         return
      end if

      call this%femdomain%ply(Name=name)
   end subroutine

! ########################################
   subroutine resizeMeristem(this, x, y, z)
      class(Meristem_), optional, intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      origin1 = this%getCoordinate("A")
      call this%femdomain%resize(x_len=x, y_len=y, z_len=z)
      origin2 = this%getCoordinate("A")
      disp = origin1 - origin2
      call this%move(x=disp(1), y=disp(2), z=disp(3))
   end subroutine
! ########################################

! ########################################
!   recursive subroutine change_length_or_width_Meristem(this, length, length_rate, Width, width_rate, dt)
!      class(Meristem_), intent(inout) :: this
!      real(real64), optional, intent(in) :: length, length_rate, width_rate, Width, dt
!      real(real64) :: new_width, new_length
!      real(real64) :: length_r, width_r, l_0, w_0, clength
!      real(real64), allocatable :: origin(:), top(:), n1(:), coord(:), center(:), vert(:)
!      integer(int32) :: i
!
!      if (this%already_grown) then
!         ! ignore growth for this
!         return
!      end if
!
!      if (present(dt)) then
!         ! logistic curve
!         ! automatic growth
!         if (this%femdomain%empty()) then
!            return
!         end if
!         this%my_time = this%my_time + dt
!         ! growth curve: logistic function
!         new_Length = this%final_length &
!                      /(1.0d0 + &
!                        (this%final_length/this%initial_length - 1.0d0) &
!                        *exp(-this%length_growth_ratio*this%my_time))
!
!         new_Width = this%final_Width &
!                     /(1.0d0 + &
!                       (this%final_Width/this%initial_Width - 1.0d0) &
!                       *exp(-this%Width_growth_ratio*this%my_time))
!         call this%change_length_or_width(Length=new_Length, Width=new_Width)
!         return
!      end if
!
!      origin = this%getCoordinate("A")
!      top = this%getCoordinate("B")
!      l_0 = sqrt(dot_product(top - origin, top - origin))
!      n1 = origin
!      n1 = top - origin
!      n1 = 1.0d0/norm(n1)*n1
!      coord = origin
!
!      ! length-ratio = new length / old length
!      if (present(length)) then
!         length_r = length/l_0
!      elseif (present(length_rate)) then
!         length_r = length_rate
!      else
!         length_r = 1.0d0
!      end if
!
!      if (present(Width)) then
!         width_r = Width/this%getWidth()
!      else
!         width_r = input(default=1.0d0, option=width_rate)
!      end if
!
!      ! enlong & fatten
!      do i = 1, this%femdomain%nn()
!         coord(:) = this%femdomain%mesh%nodcoord(i, :) - origin(:)
!         center = coord
!         clength = dot_product(coord, n1)
!         center(:) = clength*n1(:)
!         vert = coord - center
!         ! origin -> center -> current coordinate
!         coord(:) = length_r*center(:) + width_r*vert(:)
!         this%femdomain%mesh%nodcoord(i, :) = origin(:) + coord(:)
!      end do
!
!   end subroutine
! ########################################

! ########################################
   subroutine rescaleMeristem(this, x, y, z)
      class(Meristem_), optional, intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      origin1 = this%getCoordinate("A")
      call this%femdomain%resize(x_rate=x, y_rate=y, z_rate=z)
      origin2 = this%getCoordinate("A")
      disp = origin1 - origin2
      call this%move(x=disp(1), y=disp(2), z=disp(3))
   end subroutine
! ########################################

   function getLengthMeristem(this) result(ret)
      class(Meristem_), intent(in) :: this
      real(real64) :: ret

      if (this%femdomain%mesh%empty()) then
         ret = 0.0d0
      else
         ret = norm( &
               this%femdomain%mesh%nodcoord(this%A_PointNodeID, :) &
               - this%femdomain%mesh%nodcoord(this%B_PointNodeID, :))
      end if

   end function

   function getWidthMeristem(this) result(ret)
      class(Meristem_), intent(in) :: this
      real(real64) :: ret

      if (this%femdomain%mesh%empty()) then
         ret = 0.0d0
      else
         ret = 2.0d0*norm( &
               this%femdomain%mesh%nodcoord(this%C_PointNodeID, :) &
               - this%femdomain%mesh%nodcoord(this%A_PointNodeID, :))
      end if

   end function

!   function getVolumeMeristem(this) result(ret)
!      class(Meristem_), intent(in) :: this
!      real(real64) :: ret
!      integer(int32) :: i, j
!
!      ret = 0.0d0
!      if (this%femdomain%mesh%empty()) then
!         return
!      end if
!
!      do i = 1, this%femdomain%ne()
!         ret = ret + this%femdomain%getVolume(elem=i)
!      end do
!
!   end function
! ####################################################################

! ####################################################################
   function getBiomassMeristem(this) result(ret) ! unit: g
      class(Meristem_), intent(in) :: this
      real(real64) :: ret
      integer(int32) :: i, j

      ret = this%getVolume()*100.0d0*100.0d0*100.0d0*this%average_drydensity
      !ret = 0.0d0
      !if (this%femdomain%mesh%empty()) then
      !   return
      !end if
      !
      !do i = 1, this%femdomain%ne()
      !   ret = ret + this%femdomain%getVolume(elem=i)*this%drydensity(i)
      !end do

   end function
! ####################################################################

! ########################################
   function emptyMeristem(this) result(Meristem_is_empty)
      class(Meristem_), intent(in) :: this
      logical :: Meristem_is_empty

      Meristem_is_empty = this%femdomain%empty() ! true 
      Meristem_is_empty = Meristem_is_empty .and. (.not. allocated(this%leafset))
      Meristem_is_empty = Meristem_is_empty .and. (.not. allocated(this%stem))

   end function
! ########################################

   subroutine syncMeristem(this, from, mpid)
      class(Meristem_), intent(inout) :: this
      integer(int32), intent(in) :: from
      type(MPI_), intent(inout) :: mpid

      call this%FEMDomain%sync(from=from, mpid=mpid)
      !call mpid%bcast(from=from, val=this%init_radius) !
      !call mpid%bcast(from=from, val=this%init_length) !
      !call mpid%bcast(from=from, val=this%width) !
      !call mpid%bcast(from=from, val=this%MaxThickness) !
      !call mpid%bcast(from=from, val=this%Maxlength) !
      !call mpid%bcast(from=from, val=this%Maxwidth) !
      !call mpid%bcast(from=from, val=this%maxdiameter) !
      !call mpid%bcast(from=from, val=this%mindiameter) !
      !call mpid%bcast(from=from, val=this%minlength) !
      call mpid%bcast(from=from, val=this%rot_x) ! = 0.0d0
      call mpid%bcast(from=from, val=this%rot_y) ! = 0.0d0
      call mpid%bcast(from=from, val=this%rot_z) ! = 0.0d0
      call mpid%bcast(from=from, val=this%disp_x) ! = 0.0d0
      call mpid%bcast(from=from, val=this%disp_y) ! = 0.0d0
      call mpid%bcast(from=from, val=this%disp_z) ! = 0.0d0
      !call mpid%BcastMPIRealVecFixedSize(from=from, val=this%center_bottom) !(3)
      !call mpid%BcastMPIRealVecFixedSize(from=from, val=this%center_top) !(3)
      !call mpid%BcastMPIRealVecFixedSize(from=from, val=this%radius_bottom) !(3)
      !call mpid%BcastMPIRealVecFixedSize(from=from, val=this%radius_top) !(3)
      !call mpid%BcastMPIRealVecFixedSize(from=from, val=this%outer_normal_bottom) !(3)
      !call mpid%BcastMPIRealVecFixedSize(from=from, val=this%outer_normal_top) !(3)

      call mpid%BcastMPIIntVecFixedSize(from=from, val=this%EdgeNodeID)!(4)
      call mpid%BcastMPIIntVecFixedSize(from=from, val=this%EdgeElemID)!(4)
      call mpid%bcast(from=from, val=this%I_planeNodeID)!(:)
      call mpid%bcast(from=from, val=this%I_planeElementID)!(:)
      call mpid%bcast(from=from, val=this%II_planeNodeID)!(:)
      call mpid%bcast(from=from, val=this%II_planeElementID)!(:)
      call mpid%bcast(from=from, val=this%A_PointNodeID)!
      call mpid%bcast(from=from, val=this%B_PointNodeID)!
      call mpid%bcast(from=from, val=this%A_PointElementID)!
      call mpid%bcast(from=from, val=this%B_PointElementID)!
      call mpid%bcast(from=from, val=this%xnum)! = 10
      call mpid%bcast(from=from, val=this%ynum)! = 10
      call mpid%bcast(from=from, val=this%znum)! = 10

      ! physical parameter
      call mpid%bcast(from=from, val=this%DryDensity)!(:)  ! element-wise
      call mpid%bcast(from=from, val=this%WaterContent)!(:)! element-wise

      ! For deformation analysis
      call mpid%bcast(from=from, val=this%YoungModulus)!(:)! element-wise
      call mpid%bcast(from=from, val=this%CarbonDiffusionCoefficient)
      call mpid%bcast(from=from, val=this%PoissonRatio)!(:)! element-wise
      call mpid%bcast(from=from, val=this%Density)!(:)     ! element-wise
      call mpid%bcast(from=from, val=this%Stress)!(:,:,:)     ! Gauss point-wise
      call mpid%bcast(from=from, val=this%Displacement)!(:,:) ! node-wise, three dimensional

      call mpid%bcast(from=from, val=this%BoundaryTractionForce)!(:,:) ! node-wise, three dimensional
      call mpid%bcast(from=from, val=this%BoundaryDisplacement)!(:,:) ! node-wise, three dimensional

      call mpid%bcast(from=from, val=this%Division)

      !type(Meristem_),pointer ::  pMeristem

   end subroutine
! #########################################################

   subroutine syncMeristemVector(this, from, mpid)
      type(Meristem_), allocatable, intent(inout) :: this(:)
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

!! ########################################
!   function FullyExpandedMeristem(this, threshold) result(ret_expanded)
!      class(Meristem_), optional, intent(inout) :: this
!      real(real64), intent(in) :: threshold
!      logical :: ret_expanded
!      real(real64) :: length, full_length
!
!      if (this%getLength()/this%final_length > threshold) then
!         ret_expanded = .true.
!      else
!         ret_expanded = .false.
!      end if
!
!   end function
!! ########################################

   subroutine removeMeristem(this)
      class(Meristem_), intent(inout) :: this

      call this%FEMDomain%remove()
      !this%Thickness = 0.0d0
      !this%length = 0.0d0
      !this%width = 0.0d0
      !this%MaxThickness = 0.0d0
      !this%Maxlength = 0.0d0
      !this%Maxwidth = 0.0d0
      !this%center_bottom = 0.0d0
      !this%center_top = 0.0d0
      !this%radius_bottom = 0.0d0
      !this%radius_top = 0.0d0
      !this%outer_normal_bottom = 0.0d0
      !this%outer_normal_top = 0.0d0
      this%rot_x = 0.0d0
      this%rot_y = 0.0d0
      this%rot_z = 0.0d0
      this%disp_x = 0.0d0
      this%disp_y = 0.0d0
      this%disp_z = 0.0d0

      this%EdgeNodeID = 0
      this%EdgeElemID = 0
      !this%maxdiameter = 0.0d0
      !this%mindiameter = 0.0d0
      !this%minlength = 0.0d0
      if (allocated(this%I_planeNodeID)) deallocate (this%I_planeNodeID)! (:)
      if (allocated(this%I_planeElementID)) deallocate (this%I_planeElementID)! (:)
      if (allocated(this%II_planeNodeID)) deallocate (this%II_planeNodeID)! (:)
      if (allocated(this%II_planeElementID)) deallocate (this%II_planeElementID)! (:)
      this%A_PointNodeID = 0
      this%B_PointNodeID = 0
      this%C_PointNodeID = 0
      this%D_PointNodeID = 0

      this%A_PointElementID = 0
      this%B_PointElementID = 0
      this%xnum = 10
      this%ynum = 10
      this%znum = 10

      ! position in a whole structure (single plant)
      this%MeristemID = -1
      this%InterNodeID = -1
      this%already_grown = .false.

      ! physical parameter
      if (allocated(this%DryDensity)) deallocate (this%DryDensity)! (:)  ! element-wise
      if (allocated(this%WaterContent)) deallocate (this%WaterContent)! (:)! element-wise

      ! For deformation analysis
      if (allocated(this%YoungModulus)) deallocate (this%YoungModulus)! (:)! element-wise
      if (allocated(this%PoissonRatio)) deallocate (this%PoissonRatio)! (:)! element-wise
      if (allocated(this%Density)) deallocate (this%Density)! (:)     ! element-wise
      if (allocated(this%CarbonDiffusionCoefficient)) deallocate (this%CarbonDiffusionCoefficient)! (:)     ! element-wise

      if (allocated(this%Stress)) deallocate (this%Stress)! (:,:,:)     ! Gauss point-wise
      if (allocated(this%Displacement)) deallocate (this%Displacement)! (:,:) ! node-wise, three dimensional

      if (allocated(this%BoundaryTractionForce)) deallocate (this%BoundaryTractionForce)! (:,:) ! node-wise, three dimensional
      if (allocated(this%BoundaryDisplacement)) deallocate (this%BoundaryDisplacement)! (:,:) ! node-wise, three dimensional

      this%Division = 0

      ! growth parameters
      !this%my_time = 0.0d0

      !this%initial_width = 0.0010d0 ! 1.0 mm
      !this%initial_length = 0.0010d0 ! 1.0 mm
      !this%final_width = 0.0040d0   ! 4.0 mm
      !this%final_length = 0.040d0   ! 40.0 mm
      !this%width_growth_ratio = 1.0d0/4.0d0   !
      !this%length_growth_ratio = 1.0d0/4.0d0   !

      if (associated(this%pMeristem)) nullify (this%pMeristem)

   end subroutine

!   function nnMeristem(this) result(ret)
!      class(Meristem_), intent(in) :: this
!      integer(int32) :: ret
!
!      ret = this%femdomain%nn()
!   end function
!
!   function neMeristem(this) result(ret)
!      class(Meristem_), intent(in) :: this
!      integer(int32) :: ret
!
!      ret = this%femdomain%ne()
!   end function

! ############################################################
   function append_Meristem_thisect_vector(arg1, arg2) result(ret)
      type(Meristem_), allocatable, intent(in) :: arg1(:), arg2(:)
      type(Meristem_), allocatable :: ret(:)

      if (.not. allocated(arg1)) then
         if (.not. allocated(arg2)) then
            return
         else
            ret = arg2
         end if
      else
         if (.not. allocated(arg2)) then
            ret = arg1
            return
         else
            allocate (ret(size(arg1) + size(arg2)))
            ret(1:size(arg1)) = arg1(:)
            ret(size(arg1) + 1:) = arg2(:)
         end if
      end if

   end function
! ############################################################

! ############################################################
   subroutine set_material_Meristem(this, YoungModulus, PoissonRatio, side_stiffness_ratio)
      class(Meristem_), intent(inout) :: this
      real(real64), intent(in) :: YoungModulus, PoissonRatio, side_stiffness_ratio! kPa, [dimensionless],[dimensionless(Ez/Ex=Ez/Ey)],

      this%YoungModulus = YoungModulus*ones(this%femdomain%ne())
      this%PoissonRatio = PoissonRatio*ones(this%femdomain%ne())
      this%CrossSectionalYoungModulus = this%YoungModulus*side_stiffness_ratio
      this%material_is_set = .true.

   end subroutine
! ############################################################

!   subroutine grow_by_pressure_Meristem(this, pressure)
!      class(Meristem_), intent(inout) :: this
!      real(real64), intent(in) :: pressure ! kPa
!
!      real(real64), allocatable :: displ(:), sigma(:, :), tr_sigma(:), E_G(:), v(:), &
!                                   pressure_vec(:), rot_angles(:)
!
!      type(FEMSolver_) :: solver
!      integer(int32), allocatable   :: FixBoundary(:)
!      integer(int32)   :: i
!
!      call solver%init(NumDomain=1)
!      call solver%setDomain(FEMDomain=this%femdomain, DomainID=1)
!      call solver%setCRS(DOF=3)
!      E_G = zeros(6)
!      v = zeros(6)
!
!      rot_angles = radian([-this%rot_x, -this%rot_y, -this%rot_z])
!
!      pressure_vec = pressure*ones(this%femdomain%ne())
!
!      !$OMP parallel
!      !$OMP do
!      do i = 1, this%femdomain%ne()
!         E_G(1) = this%CrossSectionalYoungModulus(i)
!         E_G(2) = this%CrossSectionalYoungModulus(i)
!         E_G(3) = this%YoungModulus(i)
!         E_G(4) = 2.0d0*(1.0d0 + this%PoissonRatio(i))*E_G(1) ! really?
!         E_G(5) = 2.0d0*(1.0d0 + this%PoissonRatio(i))*E_G(2) ! really?
!         E_G(6) = 2.0d0*(1.0d0 + this%PoissonRatio(i))*E_G(3) ! really?
!         v(:) = this%PoissonRatio(i)
!         call solver%setMatrix(DomainID=1, ElementID=i, DOF=3, &
!                               Matrix=this%femdomain%StiffnessMatrix(ElementID=i, E=E_G, v=v, rot_angles=rot_angles))
!         call solver%setVector(DomainID=1, ElementID=i, DOF=3, &
!                               Vector=this%femdomain%PressureVector( &
!                               ElementID=i, &
!                               Pressure=pressure_vec(i) &
!                               ) &
!                               )
!      end do
!      !$OMP end do
!      !$OMP end parallel
!
!      print *, "matrices imported."
!      ! disp. boundary
!      FixBoundary = this%femdomain%select(z_max=this%femdomain%z_min())*3 - 2
!      call solver%fix(DomainID=1, IDs=FixBoundary, FixValue=0.0d0)
!      FixBoundary = this%femdomain%select(z_max=this%femdomain%z_min())*3 - 1
!      call solver%fix(DomainID=1, IDs=FixBoundary, FixValue=0.0d0)
!      FixBoundary = this%femdomain%select(z_max=this%femdomain%z_min())*3 - 0
!      call solver%fix(DomainID=1, IDs=FixBoundary, FixValue=0.0d0)
!
!      print *, "b.c. imported."
!
!      ! solve
!      solver%debug = .true.
!      solver%relative_er = dble(1.0e-4)
!      solver%er0 = dble(1.0e-4)
!      solver%itrmax = 1000
!
!      displ = solver%solve()
!      call this%femdomain%deform(disp=displ)
!
!    !!compute cell-averaged mean stress
!    !!trace(sigma)
!      !tr_sigma = zeros(cube%ne() )
!      !do i_i=1,cube%ne()
!      !    sigma = zeros(3,3)
!      !    sigma = cube%stressMatrix(ElementID=i_i,&
!      !        disp=reshape(displ,cube%nn(),cube%nd() ),&
!      !        E=100.0d0, v=0.40d0)
!      !    tr_sigma(i_i) = trace(sigma)/3.0d0
!      !enddo
!
!      ! x = X + u
!      this%femdomain%mesh%Nodcoord(:, :) = &
!         this%femdomain%mesh%Nodcoord(:, :) + reshape(displ, this%femdomain%nn(), this%femdomain%nd())
!
!    !! show result
!      !call cube%vtk("result_pressure_15",scalar=tr_sigma)
!
!      call solver%remove()
!
!   end subroutine
! ########################################
   recursive subroutine updateMeristem(this)
      class(Meristem_), intent(inout) :: this
      !integer(int32), optional, intent(in) :: stem_id, root_id, leaf_id
      !real(real64), optional, intent(in) :: overset_margin
      integer(int32) :: i, j, this_stem_id, next_stem_id, A_id, B_id, itr_tol, itr, k, kk
      integer(int32) :: this_leaf_id, next_leaf_id
      integer(int32) :: this_root_id, next_root_id, InterNodeID, PetioleID, StemID, LeafID
      real(real64) :: x_A(3), x_B(3), diff(3), error, last_error, mgn, overset_m, error_tol,&
         original_position(1:3),disp(1:3),origin_of_branch(1:3),old_origin_of_branch(1:3)
      type(Meristem_) :: meristem
      !logical, optional, intent(in) :: debug

      original_position = this%stem(1)%femdomain%mesh%nodcoord(1,3)


      ! update connectivity
      if (.not. allocated(this%stem2stem)) then
         print *, "updateMeristem >> ERROR :: .not. allocated(this%stem2stem )"
         return
      end if

      error_tol = dble(1.0e-14)

      ! margin between subdomains
      overset_m = input(default=0.03d0, option=this%slight_overlap_epsilon)

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
      if (maxval(this%stem2stem) /= 0) then

         do
            itr = itr + 1
            error = 0.0d0
            do i = 1, size(this%stem2stem, 1)
               do j = 1, size(this%stem2stem, 2)
                  this_stem_id = j
                  next_stem_id = i
                  if (this%stem2stem(i, j) /= 0 .and. i /= j) then
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
            !if (present(debug)) then
            !   if (debug) then
            !      print *, "Meristem % update s2s >> error :: ", error
            !   end if
            !end if
            if (itr > itr_tol) then
               print *, "Meristem % update s2s >> ERROR :: not converged for stem"
               stop
            end if

            if (abs(error) + abs(last_error) < error_tol) exit
            last_error = error
         end do
      end if

!      if (allocated(this%root2root)) then
!         ! root to root
!         last_error = 1.0d0
!         do
!            itr = itr + 1
!            error = 0.0d0
!            do i = 1, size(this%root2root, 1)
!               do j = 1, size(this%root2root, 2)
!                  this_root_id = j
!                  next_root_id = i
!                  if (next_root_id == 1) then
!                     cycle
!                  end if
!                  if (this%root2root(i, j) /= 0 .and. i /= j) then
!                     ! this_root_id ===>>> next_root_id, connected!
!                     !x_B(:) = this%root(this_root_id)%getCoordinate("B")
!                     !x_A(:) = this%root(next_root_id)%getCoordinate("A")
!
!                     ! Overset分食い込ませる
!                     x_B(:) = (1.0d0 - overset_m)*this%root(this_root_id)%getCoordinate("B") &
!                              + overset_m*this%root(this_root_id)%getCoordinate("A")
!                     ! Overset分食い込ませる
!                     x_A(:) = (1.0d0 - overset_m)*this%root(next_root_id)%getCoordinate("A") &
!                              + overset_m*this%root(next_root_id)%getCoordinate("B")
!
!                     diff(:) = x_B(:) - x_A(:)
!                     error = error + dot_product(diff, diff)
!                     call this%root(next_root_id)%move(x=diff(1), y=diff(2), z=diff(3))
!                  end if
!               end do
!            end do
!            if (present(debug)) then
!               if (debug) then
!                  print *, "Meristem % update r2r >> error :: ", error
!               end if
!            end if
!            if (itr > itr_tol) then
!               print *, "Meristem % update r2r >> ERROR :: not converged"
!               stop
!            end if
!
!            if (abs(error) + abs(last_error) < error_tol) exit
!            last_error = error
!         end do
!      end if

      ! leaf to stem
!      if(allocated(this%leaf))then
!         last_error = 1.0d0
!         do
!            itr = itr + 1
!            error = 0.0d0
!            do i = 1, size(this%leaf2stem, 1)
!               do j = 1, size(this%leaf2stem, 2)
!                  this_stem_id = j
!                  next_leaf_id = i
!                  if (this%leaf2stem(i, j) == 1) then
!                     ! this_stem_id ===>>> next_leaf_id, connected!
!                     !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
!                     !x_A(:) = this%leaf(next_leaf_id)%getCoordinate("A")
!
!                     ! Overset分食い込ませる
!                     x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("B") &
!                              + overset_m*this%stem(this_stem_id)%getCoordinate("A")
!                     ! Overset分食い込ませる
!                     x_A(:) = this%leaf(next_leaf_id)%getCoordinate("A")
!
!                     diff(:) = x_B(:) - x_A(:)
!                     error = error + dot_product(diff, diff)
!                     call this%leaf(next_leaf_id)%move(x=diff(1), y=diff(2), z=diff(3))
!                  end if
!               end do
!            end do
!            if (present(debug)) then
!               if (debug) then
!                  print *, "Meristem % update l2s >> error :: ", error
!               end if
!            end if
!            if (itr > itr_tol) then
!               print *, "Meristem % update l2s  >> ERROR :: not converged"
!               stop
!            end if
!
!            if (abs(error) - abs(last_error) < error_tol) exit
!            last_error = error
!         end do
!      endif
!
      if(allocated(this%leafset))then
         last_error = 1.0d0
         do
            itr = itr + 1
            error = 0.0d0
            do i = 1, size(this%leafset2stem, 1)
               do j = 1, size(this%leafset2stem, 2)
                  this_stem_id = j
                  next_leaf_id = i
                  if (this%leafset2stem(i, j) == 1) then
                     ! this_stem_id ===>>> next_leaf_id, connected!
                     !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
                     !x_A(:) = this%leaf(next_leaf_id)%getCoordinate("A")

                     ! Overset分食い込ませる
                     x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("B") &
                              + overset_m*this%stem(this_stem_id)%getCoordinate("A")
                     ! Overset分食い込ませる
                     x_A(:) = this%leafset(next_leaf_id)%peti(1)%getCoordinate("A")

                     diff(:) = x_B(:) - x_A(:)
                     error = error + dot_product(diff, diff)
                     call this%leafset(next_leaf_id)%peti(1)%move(x=diff(1), y=diff(2), z=diff(3))
                     call this%leafset(next_leaf_id)%update()
                  end if
               end do
            end do
            !if (present(debug)) then
            !   if (debug) then
            !      print *, "Meristem % update l2s >> error :: ", error
            !   end if
            !end if
            if (itr > itr_tol) then
               print *, "Meristem % update l2s  >> ERROR :: not converged for leafset"
               stop
            end if

            if (abs(error) - abs(last_error) < error_tol) exit
            last_error = error
         end do
      endif

      ! move branch
      if(allocated(this%branch_meristem))then
         do i=1,size(this%branch_meristem)
            if(.not. allocated(this%branch_meristem(i)%stem)) cycle
            origin_of_branch = this%stem(this%branch_meristem(i)%stem_idx)%getCoordinate("A")
            old_origin_of_branch = this%branch_meristem(i)%stem(1)%getCoordinate("A")

            call this%branch_meristem(i)%move(&
                  x=-old_origin_of_branch(1)+origin_of_branch(1),&
                  y=-old_origin_of_branch(2)+origin_of_branch(2),&
                  z=-old_origin_of_branch(3)+origin_of_branch(3) &
               )
            call this%branch_meristem(i)%update()
         enddo
      endif

      ! offset displacement
      !if ( norm(this%stem(1)%femdomain%mesh%nodcoord(1,3) - original_position) > error_tol)then
      !   disp = this%stem(1)%femdomain%mesh%nodcoord(1,3) - original_position
      !   call this%move(x=-disp(1),y=-disp(2),z=-disp(3))
      !endif

   end subroutine
! ########################################



! ########################################
subroutine grow_internodeMeristem(this,idx,params,dt,no_division,effective_ratio,size_factor_function)
   class(Meristem_),intent(inout) :: this
   
   interface
      function size_factor_function(node_idx,coeff) result(size_factor)
         use iso_fortran_env
         implicit none
         integer(int32), intent(in) :: node_idx
         real(real64),intent(in) :: coeff(:)
         real(real64) :: size_factor
      end function
   end interface

   integer(int32),intent(in)   :: idx
   real(real64),intent(in) :: dt,params(:)
   real(real64),optional,intent(in) ::effective_ratio
   logical,optional,intent(in) :: no_division

   real(real64) :: K_L ! 2.00d0 ! max length
   real(real64) :: K_R ! 0.40d0 ! max radius
   real(real64) :: T_L ! 60.0d0*60.0d0*24.0d0*20.0d0 ! Time constant for length
   real(real64) :: T_R ! 60.0d0*60.0d0*24.0d0*10.0d0 ! Time constant for radius
   real(real64) :: temp_time_limit ! max. stem generation speed (sec)
   
   real(real64) :: ex_ratio(1:3),max_width,t,xyz(1:3),dx,dy,dz,temp_dt,temp_coeff
   integer(int32) :: leafset_idx,i,peti_idx,leaf_idx,rot_x,rot_y,rot_z

   if(present(effective_ratio))then
      if(effective_ratio > 1.0d0 .or. effective_ratio < 0.0d0)then
         print *, "[ERROR] >> grow_internodeMeristem >> effective_ratio > 1 or < 0",effective_ratio
         stop
      endif
   endif

   K_L = params(1)*size_factor_function(node_idx=idx,coeff=this%stem_size_coeff)
   K_R = params(2)
   T_L = params(3)
   T_R = params(4)
   temp_time_limit = params(5)

   
   
   t = this%stem(idx)%my_time + dt

   temp_coeff = default_temp_response_function(temp=this%temperature,temp_range=this%temp_range)

   if(present(effective_ratio))then
      xyz = abs(this%stem(idx)%getSize())
      
      !call this%stem(idx)%resize( &
      !      x = xyz(1)+(2.0d0*K_R*(1.0d0 - exp(-t/T_R))-xyz(1))*abs(effective_ratio) ,&
      !      y = xyz(2)+(2.0d0*K_R*(1.0d0 - exp(-t/T_R))-xyz(2))*abs(effective_ratio) ,&
      !      z = xyz(3)+(K_L*(1.0d0 - exp(-t/T_L))-xyz(3))*abs(effective_ratio)  &
      !   )

      dx = 0.50d0*( &
         1.0d0/T_R*2.0d0*K_R*(exp(- (t   ) /T_R)) &
       + 1.0d0/T_R*2.0d0*K_R*(exp(- (t+dt) /T_R)) &
         )
      dy = 0.50d0*( &
         1.0d0/T_R*2.0d0*K_R*(exp(- (t   ) /T_R)) &
       + 1.0d0/T_R*2.0d0*K_R*(exp(- (t+dt) /T_R)) &
         )
      dz = 0.50d0*( &
         1.0d0/T_L*2.0d0*K_L*(exp(- (t   ) /T_L)) &
       + 1.0d0/T_L*2.0d0*K_L*(exp(- (t+dt) /T_L)) &
         )
      
      ! Trapezoidal rule method
      
      call this%stem(idx)%resize( &
            x = xyz(1)+temp_coeff*dx*dt*(abs(effective_ratio))**(1.0d0/3.0d0) ,&
            y = xyz(2)+temp_coeff*dy*dt*(abs(effective_ratio))**(1.0d0/3.0d0) ,&
            z = xyz(3)+temp_coeff*dz*dt*(abs(effective_ratio))**(1.0d0/3.0d0)  &
         )
      !print *, "before",xyz
      !print *, "after ",this%stem(idx)%getSize()
      
   else
      !call this%stem(idx)%resize( &
      !      x = 2.0d0*K_R*(1.0d0 - exp(-t/T_R)) ,&
      !      y = 2.0d0*K_R*(1.0d0 - exp(-t/T_R)) ,&
      !      z = K_L*(1.0d0 - exp(-t/T_L))  &
      !   )

      dx = 0.50d0*( &
         1.0d0/T_R*2.0d0*K_R*(exp(- (t   ) /T_R)) &
       + 1.0d0/T_R*2.0d0*K_R*(exp(- (t+dt) /T_R)) &
         )
      dy = 0.50d0*( &
         1.0d0/T_R*2.0d0*K_R*(exp(- (t   ) /T_R)) &
       + 1.0d0/T_R*2.0d0*K_R*(exp(- (t+dt) /T_R)) &
         )
      dz = 0.50d0*( &
         1.0d0/T_L*2.0d0*K_L*(exp(- (t   ) /T_L)) &
       + 1.0d0/T_L*2.0d0*K_L*(exp(- (t+dt) /T_L)) &
         )
         
      call this%stem(idx)%resize( &
            x = xyz(1)+temp_coeff*dx*dt ,&
            y = xyz(2)+temp_coeff*dy*dt ,&
            z = xyz(3)+temp_coeff*dz*dt  &
      )

   endif

   if(.not. input(default=.false.,option=no_division))then
      if(idx==size(this%stem) )then
         ! ここに積算温度を考慮して入れる．
         ! ここに積算温度を考慮して入れる．
         ! ここに積算温度を考慮して入れる．
         
         ! 温度調整時間(temp_time, 単位：時間)
         temp_dt = default_temp_response_function(temp=this%temperature,temp_range=this%temp_range) * dt
         this%stem(idx)%temp_time = this%stem(idx)%temp_time + temp_dt
         if(this%stem(idx)%temp_time >= this%temp_time_limit)then
            call this%meristem_division(params=params,dt=dt)
         endif
         ! ここに積算温度を考慮して入れる．
         ! ここに積算温度を考慮して入れる．
         ! ここに積算温度を考慮して入れる．


      endif
   endif
   this%stem(idx)%my_time = this%stem(idx)%my_time + dt

   call this%update()
   ! update leaves
   !if(allocated(this%leafset2stem))then
   !   do i=1,size(this%leafset2stem,1)
   !      if(this%leafset2stem(i,idx)==1)then
   !         if(this%leafset(i)%is_empty()) cycle
   !
   !         do peti_idx=1,size(this%leafset(i)%peti)
   !            call this%leafset(i)%grow_peti_and_leaf(params=params(6:11),dt=dt)
   !            call this%update()   
   !         enddo
   !         
   !      endif
   !   enddo
   !endif



end subroutine

! ######################################################
!> meristem division
!> divide a meristem internode into two meristem internodes
subroutine meristem_division_MeristemC(this,params,dt)
   class(Meristem_),intent(inout) :: this
   real(real64),intent(in) :: params(:)
   real(real64),intent(in) :: dt
   integer(int32) :: idx
   type(Stem_),allocatable ::  new_stem(:)
   type(Leafset_),allocatable :: new_leafset(:)
   real(real64),allocatable :: leafset_params(:,:)
   integer(int32),allocatable :: s2s_buf(:,:),leafset2stem_buf(:,:)
   integer(int32) :: old_leafset_size,i,stem_idx
   real(real64) :: length, radius,xyz(1:3)
   

   idx = size(this%stem)
   allocate(new_stem(1))
   new_stem(1)%CROSS_SECTION_SHAPE = this%stem(idx)%CROSS_SECTION_SHAPE 

   length = this%stem(idx)%getlength()

   xyz = abs(this%stem(idx)%getSize())

   radius = average(xyz(1:2))/4.0d0

   ! divide
   call this%stem(idx)%resize( &
         z=length*(1.0d0-this%top_meristem_aspect_ratio) &
      )

   ! create new internode
   ! with the half size
   call new_stem(1)%init(x_num=this%xnum,y_num=this%ynum,z_num=this%znum)
   call new_stem(1)%resize(&
         x=2.0d0*radius,&
         y=2.0d0*radius,&
         z=length/2.0d0 &
      )

   this%stem = this%stem // new_stem

   idx = size(this%stem)
   call this%stem(idx)%connect("=>", this%stem(idx-1))

   s2s_buf = this%stem2stem
   this%stem2stem = int(zeros(size(this%stem),size(this%stem)))
   this%stem2stem(1:size(s2s_buf,1),1:size(s2s_buf,1)) = s2s_buf(:,:)
   this%stem2stem(idx, idx-1) = 1


   ! add leafset 
   !>(1) extend this%leafset2stem array (connectivity matrix)
   if(allocated(this%leafset2stem))then
      leafset2stem_buf = this%leafset2stem
      ! stem + 1, leafset + n (n:this%num_leafset_per_stem)
      this%leafset2stem = zeros( size(leafset2stem_buf,1)+this%num_leafset_per_stem,&
          size(leafset2stem_buf,2)+1)
      this%leafset2stem(1:size(leafset2stem_buf,1),1:size(leafset2stem_buf,2)) &
         = leafset2stem_buf(:,:)
   else
      allocate(this%leafset2stem(this%num_leafset_per_stem,size(this%stem) ))
   endif

   !>(2) create new leafsets
   allocate(new_leafset(this%num_leafset_per_stem))
   allocate(leafset_params(this%num_leafset_per_stem,size(params)))

   do i=1,size(leafset_params,1)
      leafset_params(i,:) = params(:)
   enddo

   stem_idx = size(this%stem)
   do i=1,size(new_leafset)
      new_leafset(i)%leaf_aspect_ratio = this%leaf_aspect_ratio
      call new_leafset(i)%init(&
         num_leaf=this%num_leaf_per_leafset,&
         params= leafset_params(i,6:11),&
         species=this%species,&
         direction = radian(this%last_leaf_direction + this%leaf_directional_angle), &
         dt=dt)
      this%last_leaf_direction = this%last_leaf_direction + this%leaf_directional_angle
      
   enddo
   
   old_leafset_size = size(this%leafset)
   this%leafset = this%leafset // new_leafset
   this%leafset_params = this%leafset_params .v. leafset_params

   do i=old_leafset_size+1,size(this%leafset)
      !print *, allocated(this%leafset), size(this%leafset),old_leafset_size+1, size(this%leafset)
      !print *, i
      if(this%leafset(i)%is_empty())cycle
      !call this%leafset(i)%peti(1)%connect("=>", this%stem(stem_idx-1))
      !this%leafset2stem( i,stem_idx-1 ) = 1
      call this%leafset(i)%peti(1)%connect("=>", this%stem(stem_idx))
      this%leafset2stem( i,stem_idx ) = 1
   enddo

   


   call this%update()
   

end subroutine
! ######################################################


! ######################################################
function getHeightMeristem(this) result(height)
   class(Meristem_),intent(in) :: this
   real(real64) :: height
   type(FEMdomain_) :: domain
   
   domain = this%get_all_femdomain()
   height = domain%z_len()

end function
! ######################################################


! ######################################################
function getVolumeMeristem(this) result(vol)
   class(Meristem_),intent(in) :: this
   real(real64) :: vol
   integer(int32) :: i,j
   
   vol = 0.0d0
   !print *, "GetVolumeMeristem [1-1]"
   do i=1,size(this%stem)
      if(this%stem(i)%femdomain%empty())cycle
      vol = vol + this%stem(i)%femdomain%getVolume()
   enddo
   !print *, "GetVolumeMeristem [1-2]"
   do i=1,size(this%leafset)
      if(this%leafset(i)%is_empty())cycle
      do j=1,size(this%leafset(i)%peti)
         if(this%leafset(i)%peti(j)%femdomain%empty())cycle
         vol = vol + this%leafset(i)%peti(j)%femdomain%getVolume()
      enddo
      do j=1,size(this%leafset(i)%leaf)
         if(this%leafset(i)%leaf(j)%femdomain%empty())cycle
         vol = vol + this%leafset(i)%leaf(j)%femdomain%getVolume()
      enddo
   enddo
   !print *, "GetVolumeMeristem [1-3]"
   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         if(.not.allocated(this%branch_meristem(i)%stem)) cycle
         vol = vol + this%branch_meristem(i)%getVolume()
      enddo
   endif

end function
! ######################################################





! ######################################################
subroutine set_branch_MeristemC(this,stem_idx,leafset_idx,params,dt)
   class(Meristem_),intent(inout) :: this
   integer(int32),intent(in) :: stem_idx,leafset_idx
   real(real64),intent(in) :: params(:),dt
   type(Meristem_),allocatable :: branch_meristem_buf(:)
   real(real64) :: base_angle_x,base_angle_z,leafset_angles(1:3)
   integer(int32) :: branch_idx

   if(stem_idx >= size(this%stem)) then
      print *, "[Caution] >> set_branch_MeristemC >> stem_idx >= size(this%stem)"
      return
   endif
   ! Creating branch at a leaf axil of a given stem idx. (not for LATERAL ROOTS)
   ! The brancing angle is a half of the petiole angle.
   if(.not.allocated(this%branch_meristem))then
      allocate(this%branch_meristem(1))
   else
      ! extend array
      branch_meristem_buf = this%branch_meristem
      deallocate(this%branch_meristem)
      allocate(this%branch_meristem(size(branch_meristem_buf)+1))
      this%branch_meristem(1:size(branch_meristem_buf,1)) = branch_meristem_buf(:)
      deallocate(branch_meristem_buf)
   endif
   branch_idx = size(this%branch_meristem)

   this%branch_meristem(branch_idx)%xnum = this%xnum
   this%branch_meristem(branch_idx)%ynum = this%ynum
   this%branch_meristem(branch_idx)%znum = this%znum
   this%branch_meristem(branch_idx)%temp_range = this%temp_range
   this%branch_meristem(branch_idx)%temperature = this%temperature

   call this%branch_meristem(branch_idx)%init(meristem_type=PF_MERISTEM_TYPE_SHOOT,params=params,dt=dt)
   
   ! set position of the base of branch
   ! 
   leafset_angles = this%getLeafsetAngles(stem_idx=stem_idx,leafset_idx=leafset_idx)
   base_angle_x = 0.50d0*leafset_angles(1) ! half of the angle of leafset from its vertical positon.
   base_angle_z = leafset_angles(3) ! same direction as leafset
   !print *, base_angle_x
   
   call this%branch_meristem(branch_idx)%stem(1)%rotate(x=base_angle_x,z=base_angle_z)
   

   ! connect
   call this%branch_meristem(branch_idx)%stem(1)%connect("=>", this%stem(stem_idx))
   this%branch_meristem(branch_idx)%stem_idx = stem_idx
   this%branch_meristem(branch_idx)%leafset_idx = leafset_idx
   
end subroutine
! ######################################################

! ######################################################
subroutine grow_MeristemClass(this,params,dt,no_division,source_limit,effective_ratio,debug_mode)
   class(Meristem_),intent(inout) :: this
   type(Meristem_) :: trial_this

   real(real64),intent(in) :: params(:),dt
   real(real64),optional,intent(in) :: source_limit,effective_ratio
   
   logical,optional,intent(in) :: no_division

   logical,optional,intent(in) :: debug_mode
   logical :: run_as_debug_mode

   real(real64) :: base_angle_x, base_angle_z,leafset_angles(1:3),loc_effective_ratio,dv,trial_source_requirement
   real(real64),allocatable :: this_params(:)
   integer(int32) :: i,j,leafset_idx,stem_idx


   ! update morphological parameters (this%leafset_params)
   ! considering each leaf locations
   call this%update_leafset_params(size_factor_function=default_size_factor_function)

   !loc_effective_ratio = 1.0d0
   loc_effective_ratio = 0.50d0

   run_as_debug_mode = input(default=.False.,option=debug_mode)

   
   ! if there is no source limit
   if(present(source_limit))then
      print *, "[1] growth simulation started."
      !print *, "[1-1]"
      trial_this = this
      !print *, "[1-2]"
      call this%grow(params=params,dt=dt,no_division=no_division)
      !print *, "[1-3]"
      dv = this%getVolume()
      !print *, "[1-4]"
      this = trial_this
      !print *, "[1-5]"


      
      dv = dv - this%getVolume() ! m^3
      !print *, "[1-6]"

      trial_source_requirement = dv*this%average_drydensity ! m^3 x t/m^3 = t
      trial_source_requirement = trial_source_requirement*1000.0d0*1000.0d0 ! t -> g

      !print *, "[source_limit, trial_source_requirement] = "
      !print *, source_limit, trial_source_requirement

      if(source_limit < 0.0d0)then
         ! no NAR
         loc_effective_ratio = 0.0d0
      elseif(source_limit < trial_source_requirement )then
         ! if source_limit = 100 and trial_source_requirement = 150, 
         ! loc_effective_ratio should be 1 - 100/150 = 
         ! source limit
         !print *, "[source-limitation :: ACTIVE]"
         loc_effective_ratio = source_limit/trial_source_requirement ! 100/150
      else
         !print *, "[source-limitation :: IN-ACTIVE]"
         loc_effective_ratio = 1.0d0 ! 100%
      endif
      
      !print *, "loc_effective_ratio",loc_effective_ratio
      
   endif

   

   if(present(effective_ratio))then
      loc_effective_ratio = effective_ratio
   endif
   
   if(run_as_debug_mode)then
      print *, "[debug] grow_MeristemClass >> source limit done, grow_internode started."
   endif

   do i=1,size(this%stem)
      call this%grow_internode(idx=i,params=params,dt=dt,no_division=no_division,&
         effective_ratio=loc_effective_ratio,size_factor_function=default_size_factor_function)
      !call this%grow_internode(idx=i,params=params,dt=dt,no_division=no_division)
   enddo


   if(run_as_debug_mode)then
      print *, "[debug] grow_MeristemClass >> grow_internode done, leafset(i)%grow_peti_and_leaf started."
   endif

   if(size(this%leafset_params,1)/=size(this%leafset))then
      print *, "[ERROR] >> grow_MeristemClass >> size(leafset_params,1)/=size(this%leafset)"
      stop
   endif

   if(allocated(this%leafset))then
      do i=1,size(this%leafset)
         if(this%leafset(i)%is_empty()) cycle
         this%leafset(i)%leaf_aspect_ratio= this%leaf_aspect_ratio
         this%leafset(i)%max_angle_deg = this%leaf_max_angle_deg
         this%leafset(i)%leaf_curvature = this%leaf_curvature
         this_params = this%leafset_params(i,6:11)
         call this%leafset(i)%grow_peti_and_leaf(params=this_params,dt=dt,&
            base_petiole_length_ratio=this%base_petiole_length_ratio,&
            effective_ratio=loc_effective_ratio,debug_mode=debug_mode)
      enddo
   endif

   
   if(run_as_debug_mode)then
      print *, "[debug] grow_MeristemClass >> leafset(i)%grow_peti_and_leaf done, branch_meristem(i)%grow started."
   endif


   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         this%branch_meristem(i)%temp_range = this%temp_range
         this%branch_meristem(i)%temperature = this%temperature
         !print *, "vol of 1st stem in 1st branch:before",i,this%branch_meristem(i)%stem(1)%getVolume(),&
         !   this%branch_meristem(i)%stem(1)%my_time
         !print *, "vol of last stem in 1st branch:before",i,&
         !   this%branch_meristem(i)%stem(size(this%branch_meristem(i)%stem))%getVolume(),&
         !   this%branch_meristem(i)%stem(size(this%branch_meristem(i)%stem))%my_time
         this%branch_meristem(i)%max_branch_num = 0
         call this%branch_meristem(i)%grow(params=params,dt=dt,no_division=no_division,&
            effective_ratio=loc_effective_ratio)
         !call this%branch_meristem(i)%grow(params=params,dt=dt,no_division=no_division)
         
         ! my_timeは増加しているがvolumeは減っている．
         !print *, "vol of 1st stem in 1st branch:after",i,this%branch_meristem(i)%stem(1)%getVolume(),&
         !   this%branch_meristem(i)%stem(1)%my_time
         !print *, "vol of last stem in 1st branch:after",i,&
         !   this%branch_meristem(i)%stem(size(this%branch_meristem(i)%stem))%getVolume(),&
         !   this%branch_meristem(i)%stem(size(this%branch_meristem(i)%stem))%my_time

         ! set position of the base of branch
         leafset_idx = this%branch_meristem(i)%leafset_idx
         stem_idx = this%branch_meristem(i)%stem_idx
         leafset_angles = this%getLeafsetAngles(stem_idx=stem_idx,leafset_idx=leafset_idx)
         base_angle_x = 0.50d0*leafset_angles(1) ! half of the angle of leafset from its vertical positon.
         base_angle_z = leafset_angles(3) ! same direction as leafset   
         !print *, base_angle_x
         do j=1,size(this%branch_meristem(i)%stem)
            call this%branch_meristem(i)%stem(j)%rotate(x=base_angle_x,z=base_angle_z,reset=.true.)
            base_angle_x = base_angle_x/2.0d0 ! gradually reaches vertical position
         enddo
         call this%branch_meristem(i)%update()
      enddo
   endif


   if(run_as_debug_mode)then
      print *, "[debug] grow_MeristemClass >> branch_meristem(i)%grow done, %branch_check started."
   endif

   call this%update()

   ! branch check
   ! if the current branch number is smaller than the max. br. num, try to set a new branch.
   if(this%max_branch_num > 0)then
      if(allocated(this%branch_meristem))then
         ! Already at least 1 branch exists.
         if(size(this%branch_meristem)+2 == size(this%stem)-this%apical_dominance_num )then
            if(size(this%branch_meristem)<=this%max_branch_num)then
               ! ex. size(stem) = 4
               ! size(this%branch_meristem) = 1
               ! apical_dominance_num = 2, then
               ! 1 + 1 == 4 - 2 -> .true.
               print *, "branch_idx",size(this%branch_meristem)+1,size(this%branch_meristem),size(this%stem)
               call this%set_branch(stem_idx=size(this%branch_meristem)+1,&
                  leafset_idx=1,params=params,dt=dt)
            endif
         endif
      else

         if(size(this%stem)-this%apical_dominance_num >= 3)then
            print *, "branch_idx",size(this%stem)-this%apical_dominance_num
            call this%set_branch(stem_idx=size(this%stem)-this%apical_dominance_num,&
               leafset_idx=1,params=params,dt=dt)
         endif

      endif
   endif


   if(run_as_debug_mode)then
      print *, "[debug] grow_MeristemClass >> all done."
   endif
   
end subroutine
! ######################################################

function get_all_femdomain_MeristemClass(this) result(femdomain)
   class(Meristem_),intent(in) :: this
   type(FEMDomain_) :: femdomain
   integer(int32) :: i, j
   type(stem_),allocatable :: stem(:), peti(:)
   type(leaf_),allocatable :: leaf(:)


   ! stem -> peti -> leaf
   stem = this%getstem()
   peti = this%getpeti()
   leaf = this%getleaf()

   do i=1,size(stem)
      femdomain = femdomain + stem(i)%femdomain
   enddo
   do i=1,size(peti)
      femdomain = femdomain + peti(i)%femdomain
   enddo
   do i=1,size(leaf)
      femdomain = femdomain + leaf(i)%femdomain
   enddo


!   if (allocated(this%stem)) then
!      do i = 1, size(this%stem)
!         if (.not. this%stem(i)%femdomain%empty()) then
!            femdomain = femdomain + this%stem(i)%femdomain
!         end if
!      end do
!   end if
!   
!   if (allocated(this%leafset)) then
!      do i = 1, size(this%leafset)
!         if(this%leafset(i)%is_empty())cycle
!         do j=1, size(this%leafset(i)%peti)
!            femdomain = femdomain + this%leafset(i)%peti(j)%femdomain
!         enddo
!         do j=1, size(this%leafset(i)%leaf)
!            femdomain = femdomain + this%leafset(i)%leaf(j)%femdomain
!         enddo
!      end do
!   end if
!
!   if(allocated(this%branch_meristem))then
!      do i=1,size(this%branch_meristem)
!         femdomain = femdomain + this%branch_meristem(i)%get_all_femdomain()
!      enddo
!   endif

end function



recursive function get_all_femdomain_ptr_Meristem(this) result(femdomain_ptr)
   class(Meristem_),target,intent(in) :: this
   type(FEMDomainp_),allocatable :: femdomain_ptr(:),stem_ptr(:),peti_ptr(:),leaf_ptr(:)
   integer(int32) :: i, j, itr
   
   ! stem -> peti -> leaf
   stem_ptr = this%get_stem_femdomain_ptr()
   peti_ptr = this%get_peti_femdomain_ptr()
   leaf_ptr = this%get_leaf_femdomain_ptr()

   allocate(femdomain_ptr( size(stem_ptr)+size(peti_ptr)+size(leaf_ptr) ))
   itr = 0
   do i=1,size(stem_ptr)
      itr = itr + 1
      femdomain_ptr(itr) = stem_ptr(i)
   enddo
   do i=1,size(peti_ptr)
      itr = itr + 1
      femdomain_ptr(itr) = peti_ptr(i)
   enddo
   do i=1,size(leaf_ptr)
      itr = itr + 1
      femdomain_ptr(itr) = leaf_ptr(i)
   enddo


!   if (allocated(this%stem)) then
!      do i = 1, size(this%stem)
!         if (.not. this%stem(i)%femdomain%empty()) then
!            femdomain = femdomain + this%stem(i)%femdomain
!         end if
!      end do
!   end if
!   
!   if (allocated(this%leafset)) then
!      do i = 1, size(this%leafset)
!         if(this%leafset(i)%is_empty())cycle
!         do j=1, size(this%leafset(i)%peti)
!            femdomain = femdomain + this%leafset(i)%peti(j)%femdomain
!         enddo
!         do j=1, size(this%leafset(i)%leaf)
!            femdomain = femdomain + this%leafset(i)%leaf(j)%femdomain
!         enddo
!      end do
!   end if
!
!   if(allocated(this%branch_meristem))then
!      do i=1,size(this%branch_meristem)
!         femdomain = femdomain + this%branch_meristem(i)%get_all_femdomain()
!      enddo
!   endif

end function


! ############################################################
function getLeafsetAngles_MeristemClass(this,stem_idx,leafset_idx) result(angles)
   class(Meristem_),intent(in) :: this
   integer(int32),intent(in) :: stem_idx, leafset_idx
   real(real64) :: angles(1:3)
   integer(int32) :: i,j,current_leafset_idx

   if(leafset_idx > sum(this%leafset2stem(:,stem_idx)))then
      print *, "[ERROR] :: getLeafsetAngles_MeristemClass >> leafset idx should be smaller than"
      print *, sum(this%leafset2stem(:,stem_idx))," but leafset idx of ",leafset_idx, "are requested."
      stop
   endif
   
   angles(:) = 0.0d0
   current_leafset_idx = 0 
   if(allocated(this%leafset))then
      do i=1,size(this%leafset2stem,1)
         if(this%leafset2stem(i,stem_idx) ==1)then
            current_leafset_idx = current_leafset_idx + 1
            if(current_leafset_idx==leafset_idx)then
               angles(:) = [&
                  this%leafset(i)%peti(1)%rot_x,&
                  this%leafset(i)%peti(1)%rot_y,&
                  this%leafset(i)%peti(1)%rot_z &
               ]
               return
            endif
         endif
      enddo
   endif
   
   

end function
! ##############################################


! ##############################################
recursive function ne_MeristemClass(this) result(number_of_elements)
   class(Meristem_),intent(in) :: this
   integer(int32) :: number_of_elements,i

   number_of_elements = 0
   if(allocated(this%stem))then
      do i=1,size(this%stem)
         number_of_elements = number_of_elements + this%stem(i)%femdomain%ne()
      enddo
   endif

   if(allocated(this%leafset))then
      do i=1,size(this%leafset)
         number_of_elements = number_of_elements + this%leafset(i)%ne()
      enddo
   endif
   
   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         number_of_elements = number_of_elements + this%branch_meristem(i)%ne()
      enddo
   endif

end function
! ##############################################



! ##############################################
recursive function nn_MeristemClass(this) result(number_of_nodes)
   class(Meristem_),intent(in) :: this
   integer(int32) :: number_of_nodes,i

   number_of_nodes = 0
   if(allocated(this%stem))then
      do i=1,size(this%stem)
         number_of_nodes = number_of_nodes + this%stem(i)%femdomain%nn()
      enddo
   endif

   if(allocated(this%leafset))then
      do i=1,size(this%leafset)
         number_of_nodes = number_of_nodes + this%leafset(i)%nn()
      enddo
   endif
   
   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         number_of_nodes = number_of_nodes + this%branch_meristem(i)%nn()
      enddo
   endif

end function
! ##############################################

function getPhotoSynthesis_meristem(this, env, dt, Transparency) result(elem_photosynthesis)
      class(Meristem_), intent(inout) :: this
      class(Environment_), intent(in):: env
      real(real64), intent(in) :: dt ! sec.
      !type(Meristem_), optional, intent(inout) :: soybean_canopy(:)
      type(Leaf_), allocatable :: leaf(:)
      real(real64), optional, intent(in) :: Transparency
      real(real64), allocatable :: elem_ppfd(:), elem_volume(:), &
                                   elem_photospeed(:), elem_photosynthesis(:)

      if (.not. allocated(this%Photosynthate_n)) then
         this%Photosynthate_n = zeros(this%nn())
      end if

      !elem_volume = this%getVolumePerElement()
      !if (present(soybean_canopy)) then
!
      !   leaf = this%getIntersectLeaf(soybeans=soybean_canopy)
      !   elem_ppfd = this%getPPFD( &
      !               Light=env%Light, &
      !               Transparency=input(default=0.10d0, option=Transparency), &
      !               leaf=leaf)
      !else
         elem_ppfd = this%getPPFD( &
                     Light=env%Light, &
                     Transparency=input(default=0.10d0, option=Transparency))

      !end if

      elem_photospeed = this%getPhotoSynthesisSpeedPerElem( &
                        Air=env%Air, dt=dt, &
                        ppfd=elem_ppfd)

      elem_photosynthesis = elem_photospeed  ! *elem_volume

   end function
! #####################################################################
   function getVolumePerElement_Meristem(this) result(volume)
      class(Meristem_), intent(in) :: this
      integer(int32) :: i, j, elem_id
      real(real64), allocatable :: volume(:)
      type(FEMDomain_) :: all_femdomain

      all_femdomain = this%get_all_femdomain()
      allocate(volume(this%ne()))
      do elem_id=1,this%ne()
         volume(elem_id) = all_femdomain%getVolume(elem=elem_id)
      enddo

   end function
! ############################################################################


! ############################################################################
   function getPPFDMeristem(this, light, Transparency, num_threads) result(ppfd)
      class(Meristem_), intent(inout) :: this
      type(Light_), intent(in)    :: light
      real(real64), optional, intent(in) :: Transparency
      integer(int32), optional, intent(in) :: num_threads
      ! leaf of other plants
      !type(Leaf_), optional, intent(inout) :: leaf(:)

      real(real64), allocatable :: ppfd(:), NumberOfElement(:), NumberOfPoint(:),&
         x_A(:),x_B(:),x_C(:),x_D(:)
      real(real64), allocatable :: nodcoord(:, :), radius_vec(:), elem_cosins(:)
      integer(int32), allocatable :: leaf_pass_num(:),leaf_ne_list(:)
      real(real64) ::thickness, center_x(3), xmin(3), xmax(3), radius, radius_tr, coord(3), Transparency_val
      real(real64) :: zmin, leaf_cosine_val,theta_AB,theta_CD
      integer(int32) :: from, to, i, n, j, k, l, element_id

      logical :: inside, upside
      type(Math_) :: math
      type(Leaf_),allocatable :: all_leaves(:)
      type(Stem_),allocatable :: all_stem(:), all_peti(:)

      ! compute cosin
      !elem_cosins = this%getLeafCosValue(light)


      ! rotate meristem
      !> [here!]

      
      call this%rotate(z=radian(180.0d0 - light%angles(1)))

      ! debug MEMLEAK <-


      call this%rotate(x=radian(90.0d0 - light%angles(2)))
      
!      if (present(leaf)) then
!         do i = 1, size(leaf)
!            call leaf(i)%femdomain%rotate(z=radian(180.0d0 - light%angles(1)))
!            call leaf(i)%femdomain%rotate(x=radian(90.0d0 - light%angles(2)))
!         end do
!      end if
      !本当にあってる？？
      ! after this, rotate this back again

      
      Transparency_val = input(default=0.30d0, option=Transparency)

      ! ppfdが通過した葉の積算長さで減衰するモデル
      
      ppfd = zeros(this%ne())
      elem_cosins = zeros(this%ne())
      leaf_pass_num = int(zeros(this%ne()))

      !i = sum(NumberOfElement(1:sum(this%numStemAndPeti()))+1)

      ! soybeanclassでは，stem, leaf, rootオブジェクトごとにまとめていた．
      ! しかし，MeristemClassでは異なり，stem -> leafset -> branchの順に
      ! 全要素を数え上げている．

      ! getStem(),getPeti(),getLeaf()などとすれば，同じことが可能．
      
      !return
      ! debug MEMLEAK <-
      
      !NumberOfElement = this%ne() ???
      all_stem = this%getStem()
      all_peti = this%getPeti()
      all_leaves = this%getLeaf()
      NumberOfElement = int(zeros(size(all_stem)+size(all_peti)+size(all_leaves)))
      n=0
      do i=1,size(all_stem)
         n=n+1
         NumberOfElement(n) = all_stem(i)%femdomain%ne()
      enddo
      do i=1,size(all_peti)
         n=n+1
         NumberOfElement(n) = all_peti(i)%femdomain%ne()
      enddo
      do i=1,size(all_leaves)
         n=n+1
         NumberOfElement(n) = all_leaves(i)%femdomain%ne()
      enddo

      i = ne(all_stem) + ne(all_peti) + 1
      j = ne(all_stem) + ne(all_peti) + ne(all_leaves)
      
      ! trial values
      ! set maxPPFD for all leaf elements
      ppfd(i:j) = light%maxPPFD
      
      

      !n = sum(NumberOfElement(1:sum(this%numStemAndPeti())))
      !from = n
!      if (present(num_threads)) then
!         call omp_set_num_threads(num_threads)
!      end if

!      ! leaf of other plants
!      if (present(leaf)) then
!         !$OMP parallel do default(shared), private(j,k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
!         do i = 1, size(this%leaf)
!            if (.not. this%leaf(i)%femdomain%empty()) then
!               !print *, i, "/", this%numLeaf()
!               !$OMP parallel do default(shared), private(k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
!               do j = 1, this%leaf(i)%femdomain%ne()
!                  ! 中心座標
!                  center_x = this%leaf(i)%femdomain%centerPosition(ElementID=j)
!                  ! 枚数のみカウント
!                  ! 1枚あたりthicknessだけ距離加算
!
!                  !$OMP parallel do default(shared), private(inside,upside,radius_vec,radius_tr,zmin,n,element_id)
!                  do k = 1, size(leaf)
!                     if (.not. leaf(k)%femdomain%empty()) then
!
!                        inside = .false.
!                        radius_vec = zeros(leaf(k)%femdomain%nn())
!                        radius_vec = (leaf(k)%femdomain%mesh%nodcoord(:, 1) - center_x(1))**2 &
!                                     + (leaf(k)%femdomain%mesh%nodcoord(:, 2) - center_x(2))**2
!
!                        radius_tr = minval(radius_vec)
!
!                        if (radius_tr < radius*radius) then
!                           zmin = leaf(k)%femdomain%mesh%nodcoord(minvalID(radius_vec), 3)
!                           inside = .true.
!                        end if
!                        !あるいは，zmin,xmax,ymin,ymaxの正負で場合分けできるのでは？
!                        !>>なぜか失敗
!                        !upside = (center_x(3) < zmin )
!                        if (inside .eqv. .true.) then
!
!                           if (center_x(3) <= zmin) then
!                              !print *, center_x(3) , zmin
!
!                              n = this%numStemAndPeti() + (i - 1)
!
!                              element_id = sum(NumberOfElement(1:n)) + j
!                              leaf_pass_num(element_id) = leaf_pass_num(element_id) + 1
!                           end if
!                        end if
!                     end if
!                  end do
!                  !$OMP end parallel do
!               end do
!               !$OMP end parallel do
!            end if
!         end do
!         !$OMP end parallel do
!      end if
      all_leaves = this%get_all_leaves()

      allocate(leaf_ne_list(size(all_leaves)))
      do i=1,size(all_leaves)
         if (all_leaves(i)%femdomain%empty()) then
            leaf_ne_list(i) = 0
         else
            leaf_ne_list(i) = all_leaves(i)%femdomain%ne()
         endif
      enddo

      ! debug MEMLEAK <-
      

      !$OMP parallel private(j,k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id,leaf_cosine_val,radius,&
      !$OMP   x_A,x_B,x_C,x_D)
      !$OMP do reduction(+:leaf_pass_num, elem_cosins)
      do i = 1, size(all_leaves)
         if (all_leaves(i)%femdomain%empty()) then
            cycle
         else
            !print *, i, "/", this%numLeaf()
            !!$OMP parallel do default(shared), private(k,inside,upside,center_x,radius_vec,radius_tr,zmin,n,element_id)
            
            ! cosine value
            ! computed by the angle theta 
            x_A = all_leaves(i)%getCoordinate("A")
            x_B = all_leaves(i)%getCoordinate("B")
            x_C = all_leaves(i)%getCoordinate("C")
            x_D = all_leaves(i)%getCoordinate("D")
            
            
            if(sqrt(dot_product(x_A(1:2)-x_B(1:2),x_A(1:2)-x_B(1:2)))==0.0d0)then
               leaf_cosine_val = 0.0d0
            else
               theta_AB = atan2( abs(x_A(3)-x_B(3)),sqrt(dot_product(x_A(1:2)-x_B(1:2),x_A(1:2)-x_B(1:2))) )
               theta_CD = atan2( abs(x_C(3)-x_D(3)),sqrt(dot_product(x_C(1:2)-x_D(1:2),x_C(1:2)-x_D(1:2))) )
               leaf_cosine_val = abs(cos( average([theta_AB,theta_CD])))
            endif
            
            do j = 1, all_leaves(i)%femdomain%ne()
               ! 中心座標
               center_x = all_leaves(i)%femdomain%centerPosition(ElementID=j)
               
               ! distance from the edges of this elements
               radius_vec = (all_leaves(i)%femdomain%mesh%nodcoord(&
                  all_leaves(i)%femdomain%mesh%elemnod(j,:),1)-center_x(1))**2 &
                  + (all_leaves(i)%femdomain%mesh%nodcoord(&
                  all_leaves(i)%femdomain%mesh%elemnod(j,:),2)-center_x(2))**2 
               radius = sqrt(maxval(radius_vec))

               ! 枚数のみカウント
               ! 1枚あたりthicknessだけ距離加算
                !!$OMP parallel do default(shared), private(inside,upside,radius_vec,radius_tr,zmin,n,element_id)
               if(i==1)then
                  element_id = ne(all_stem) + ne(all_peti) + j
               else
                  element_id = ne(all_stem) + ne(all_peti) + sum(leaf_ne_list(:i-1)) + j
               endif
               elem_cosins(element_id)   = leaf_cosine_val
               do k = 1, size(all_leaves)

                  if (i == k) cycle
                  if (all_leaves(k)%femdomain%empty()) then
                     cycle
                  else

                     ! lay-tracing: 内外判定
                     inside = .false.

                     ! distance from the shadow of the leaves
                     radius_vec = zeros(all_leaves(k)%femdomain%nn())
                     radius_vec = (all_leaves(k)%femdomain%mesh%nodcoord(:, 1) - center_x(1))**2 &
                                  + (all_leaves(k)%femdomain%mesh%nodcoord(:, 2) - center_x(2))**2

                     radius_tr = sqrt(minval(radius_vec))
                     
                     

                     if (radius_tr < radius) then
                        zmin = all_leaves(k)%femdomain%mesh%nodcoord(minvalID(radius_vec), 3)
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
                           !n = sum(this%numStemAndPeti()) + (i - 1)
                           !element_id = sum(NumberOfElement(1:n)) + j
                           if(i==1)then
                              element_id = ne(all_stem) + ne(all_peti) + j
                           else
                              element_id = ne(all_stem) + ne(all_peti) + sum(leaf_ne_list(:i-1)) + j
                           endif
                           
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
         ppfd(i) = ppfd(i)*(Transparency_val**leaf_pass_num(i))
      end do

      
      ppfd(:) = ppfd(:)*elem_cosins(:)

      ! get back
      call this%rotate(x=-radian(90.0d0 - light%angles(2)))
      call this%rotate(z=-radian(180.0d0 - light%angles(1)))

      !if (present(leaf)) then
      !   do i = 1, size(leaf)
      !      call leaf(i)%femdomain%rotate(x=-radian(90.0d0 - light%angles(2)))
      !      call leaf(i)%femdomain%rotate(z=-radian(180.0d0 - light%angles(1)))
      !   end do
      !end if

   end function
! ############################################################################


! ################################################################
   subroutine rotate_all_femdomain_Meristem(this, x, y, z,reset)
      type(Meristem_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      type(FEMDomain_) :: domain
      logical,optional,intent(in) :: reset

      
      
      ! get points
      domain = this%get_all_femdomain()

      if (present(reset)) then
         if (reset .eqv. .true.) then
            call domain%rotate(-this%rot_x, -this%rot_y, -this%rot_z)
            this%rot_x = 0.0d0
            this%rot_y = 0.0d0
            this%rot_z = 0.0d0
         end if
      end if
      

      ! rotate points
      call domain%rotate(x=x, y=y, z=z)

      ! set points
      call this%setPoints(domain%mesh%nodcoord)

      

   end subroutine
! ################################################################

! ################################################################
recursive subroutine setPoints_MeristemClass(this,points)
   class(Meristem_),intent(inout) :: this
   type(FEMDomainp_),allocatable :: femdomain_ptr(:)
   real(real64),intent(in) :: points(:,:)
   
   integer(int32) :: from_idx,to_idx
   integer(int32) :: i,j



   ! for stem:
   from_idx = 0
   to_idx = 0
   
   femdomain_ptr = this%get_all_femdomain_ptr()
   do i=1,size(femdomain_ptr)
      if(.not.associated(femdomain_ptr(i)%femdomainp)) cycle
      from_idx = from_idx + 1
      to_idx = from_idx -1 + femdomain_ptr(i)%femdomainp%nn()
      femdomain_ptr(i)%femdomainp%mesh%nodcoord(:,:) = points(from_idx:to_idx,:)
      from_idx = to_idx
   enddo

end subroutine
! ################################################################


! ################################################################
function get_all_leaves_Meristem(this) result(ret_leaf)
   class(Meristem_),intent(in) :: this
   integer(int32) :: num_leaf
   type(Leaf_),allocatable :: ret_leaf(:)
   integer(int32) :: i,j,k
   
   
   num_leaf = 0
   if (allocated(this%leafset)) then
      do i = 1, size(this%leafset)
         if(this%leafset(i)%is_empty())cycle
         num_leaf = num_leaf + size(this%leafset(i)%leaf)
      end do
   end if

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         if(allocated(this%branch_meristem(i)%leafset))then
            do j=1,size(this%branch_meristem(i)%leafset)
               !do k=1,size(this%branch_meristem(i)%leafset(j)%leaf)
                  num_leaf = num_leaf + size(this%branch_meristem(i)%leafset(j)%leaf)
               !enddo
            enddo
         endif
      enddo
   endif

   allocate(ret_leaf(num_leaf))
   
   num_leaf = 0

   if (allocated(this%leafset)) then
      do i = 1, size(this%leafset)
         if(this%leafset(i)%is_empty())cycle
         do j=1,size(this%leafset(i)%leaf)
            num_leaf = num_leaf + 1
            ret_leaf(num_leaf) = this%leafset(i)%leaf(j)
         enddo
      end do
   end if

   ! [Caution!]
   ! only direct branches from the mainstem are considered.
   ! for higher-order branches (branches from branches), we need additional implementation.
   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         if(allocated(this%branch_meristem(i)%leafset))then
            do j=1,size(this%branch_meristem(i)%leafset)
               do k=1,size(this%branch_meristem(i)%leafset(j)%leaf)
                  num_leaf = num_leaf + 1
                  ret_leaf(num_leaf) = this%branch_meristem(i)%leafset(j)%leaf(k)
               enddo
            enddo
         endif
      enddo
   endif



end function
! ################################################################



! ################################################################
recursive function get_leaf_femdomain_ptr_Meristem(this) result(ret_leaf)
   class(Meristem_),target,intent(in) :: this
   integer(int32) :: num_leaf
   type(FEMDomainp_),allocatable :: ret_leaf(:)
   integer(int32) :: i,j
   
   
   num_leaf = 0
   if (allocated(this%leafset)) then
      do i = 1, size(this%leafset)
         if(this%leafset(i)%is_empty())cycle
         num_leaf = num_leaf + size(this%leafset(i)%leaf)
      end do
   end if

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         num_leaf = num_leaf + size(this%branch_meristem(i)%get_all_leaves())
      enddo
   endif

   allocate(ret_leaf(num_leaf))
   
   num_leaf = 0
   if (allocated(this%leafset)) then
      do i = 1, size(this%leafset)
         if(this%leafset(i)%is_empty())cycle
         do j=1,size(this%leafset(i)%leaf)
            num_leaf = num_leaf + 1
            ret_leaf(num_leaf)%femdomainp => this%leafset(i)%leaf(j)%femdomain
         enddo
      end do
   end if

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         ret_leaf =  ret_leaf // this%branch_meristem(i)%get_leaf_femdomain_ptr()
      enddo
   endif


end function
! ################################################################



! ################################################################
recursive function get_all_stem_Meristem(this) result(ret_stem)
   class(Meristem_),intent(in) :: this
   integer(int32) :: num_stem
   type(stem_),allocatable :: ret_stem(:)
   integer(int32) :: i, j
   
   
   ! [Caution!]
   ! only direct branches from the mainstem are considered.
   ! for higher-order branches (branches from branches), we need additional implementation.

   num_stem = size(this%stem)
   !if (allocated(this%leafset)) then
   !   do i = 1, size(this%leafset)
   !      if(this%leafset(i)%is_empty())cycle
   !      num_stem = num_stem + size(this%leafset(i)%leaf)
   !   end do
   !end if

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         num_stem = num_stem + size(this%branch_meristem(i)%stem)
      enddo
   endif

   allocate(ret_stem(num_stem))
   ret_stem(1:size(this%stem)) = this%stem

   num_stem = size(this%stem)

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         do j=1,size(this%branch_meristem(i)%stem)
            num_stem = num_stem + 1
            ret_stem(num_stem) =  this%branch_meristem(i)%stem(j)
         enddo
      enddo
   endif

   !ret_stem = this%stem // ret_stem

end function
! ################################################################


! ################################################################
recursive function get_stem_femdomain_ptr_Meristem(this) result(ret_stem)
   class(Meristem_),target,intent(in) :: this
   integer(int32) :: num_stem
   type(femdomainp_),allocatable :: ret_stem(:),this_ret_stem(:)
   integer(int32) :: i, j
      
   ! get FEM-domain pointers
   allocate(ret_stem(size(this%stem)))
   do i=1,size(this%stem)
      ret_stem(i)%femdomainp => this%stem(i)%femdomain 
   enddo

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         ret_stem =  ret_stem // this%branch_meristem(i)%get_stem_femdomain_ptr()
      enddo
   endif

end function
! ################################################################




! ################################################################
function get_all_peti_Meristem(this) result(ret_peti)
   class(Meristem_),intent(in) :: this
   integer(int32) :: num_peti
   type(stem_),allocatable :: ret_peti(:)
   integer(int32) :: i,j,k
   
   ! [Caution!]
   ! only direct branches from the mainstem are considered.
   ! for higher-order branches (branches from branches), we need additional implementation.
   
   num_peti = 0
   if (allocated(this%leafset)) then
      do i = 1, size(this%leafset)
         if(this%leafset(i)%is_empty())cycle
         num_peti = num_peti + size(this%leafset(i)%peti)
      end do
   end if

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         if(allocated(this%branch_meristem(i)%leafset))then
            do j=1,size(this%branch_meristem(i)%leafset)
               num_peti = num_peti + size(this%branch_meristem(i)%leafset(j)%peti)
            enddo
         endif
      enddo
   endif

   allocate(ret_peti(num_peti))
   
   num_peti = 0


   if (allocated(this%leafset)) then
      do i = 1, size(this%leafset)
         if(this%leafset(i)%is_empty())cycle
         do j=1,size(this%leafset(i)%peti)
            num_peti = num_peti + 1
            ret_peti(num_peti) = this%leafset(i)%peti(j)
         enddo
      end do
   end if

!   if(allocated(this%branch_meristem))then
!      do i=1,size(this%branch_meristem)
!         ret_peti =  ret_peti // this%branch_meristem(i)%getPeti()
!      enddo
!   endif
   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         if(allocated(this%branch_meristem(i)%leafset))then
            do j=1,size(this%branch_meristem(i)%leafset)
               do k=1,size(this%branch_meristem(i)%leafset(j)%peti)
                  num_peti = num_peti + 1
                  ret_peti(num_peti) = this%branch_meristem(i)%leafset(j)%peti(k)
               enddo
            enddo
         endif
      enddo
   endif


end function
! ################################################################



! ################################################################
recursive function get_peti_femdomain_ptr_Meristem(this) result(ret_peti)
   class(Meristem_),target,intent(in) :: this
   integer(int32) :: num_peti
   type(femdomainp_),allocatable :: ret_peti(:)
   integer(int32) :: i,j
   
   
   num_peti = 0
   if (allocated(this%leafset)) then
      do i = 1, size(this%leafset)
         if(this%leafset(i)%is_empty())cycle
         num_peti = num_peti + size(this%leafset(i)%peti)
      end do
   end if

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         num_peti = num_peti + size(this%branch_meristem(i)%getPeti())
      enddo
   endif

   allocate(ret_peti(num_peti))
   
   num_peti = 0


   if (allocated(this%leafset)) then
      do i = 1, size(this%leafset)
         if(this%leafset(i)%is_empty())cycle
         do j=1,size(this%leafset(i)%peti)
            num_peti = num_peti + 1
            ret_peti(num_peti)%femdomainp => this%leafset(i)%peti(j)%femdomain
         enddo
      end do
   end if

   if(allocated(this%branch_meristem))then
      do i=1,size(this%branch_meristem)
         ret_peti =  ret_peti // this%branch_meristem(i)%get_peti_femdomain_ptr()
      enddo
   endif

end function
! ################################################################



! [作業中！]
! ############################################################################
   function getPhotoSynthesisSpeedPerElemMer(this, air, dt, ppfd) result(photosynthesis)

      ! unit:: g/elem

      class(Meristem_), intent(inout) :: this
      !type(Light_),optional, intent(in)    :: light
      type(Air_), intent(in)    :: Air
      real(real64), intent(in) :: dt
      ! real(real64), optional, intent(in) :: Transparency, Resolution, ppfd(:)
      real(real64), intent(in) :: ppfd(:)

      real(real64), allocatable :: photosynthesis(:), Speed_PV(:)

      !integer(int32), allocatable :: NumberOfElement(:)
      integer(int32) :: i, j, offset, elem_id,leafset_idx,branch_idx

      type(IO_) :: debug
      

      if (.not. allocated(this%Photosynthate_n)) then
         this%Photosynthate_n = zeros(this%nn())
      end if

      photosynthesis = zeros(this%ne())

      !NumberOfElement = this%ne()
      offset = ne(this%getStem()) + ne(this%getPeti())

      

      !if (.not. present(ppfd)) then

      !   call this%laytracing(light=light, Transparency=Transparency, Resolution=Resolution)
      
      ! stem
      
      !else
      elem_id = offset
      if(allocated(this%leafset))then
         do leafset_idx = 1, size(this%leafset)
            do i=1,size(this%leafset(leafset_idx)%leaf)
               if (this%leafset(leafset_idx)%leaf(i)%femdomain%empty()) cycle

               do j = 1, this%leafset(leafset_idx)%leaf(i)%femdomain%ne()
                  elem_id = elem_id + 1
                  this%leafset(leafset_idx)%leaf(i)%ppfd(j) = ppfd(elem_id) !- photosynthesis(elem_id)
               end do

            enddo
         end do
      endif

      if(allocated(this%branch_meristem))then
         do branch_idx=1,size(this%branch_meristem)
            if(allocated(this%branch_meristem(branch_idx)%leafset))then
               do leafset_idx=1,size(this%branch_meristem(branch_idx)%leafset)
                  do i=1,size(this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf)
                     if (this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%femdomain%empty()) cycle
                     do j = 1, this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%femdomain%ne()
                        elem_id = elem_id + 1
                        this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%ppfd(j) = ppfd(elem_id) !- photosynthesis(elem_id)
                     end do
                  enddo
               enddo
            endif
         enddo
      endif


      !end if



      !call debug%open("debug.txt","w")
      
      elem_id = offset
      if(allocated(this%leafset))then
         do leafset_idx = 1, size(this%leafset)
            if(.not.allocated(this%leafset(leafset_idx)%leaf))cycle

            do i = 1, size(this%leafset(leafset_idx)%leaf)

               if (.not. this%leafset(leafset_idx)%leaf(i)%femdomain%empty()) then
                  ! for each leaves, compute photosynthesis ratio (g/m^3)
                  this%leafset(leafset_idx)%leaf(i)%aging_params = this%leaf_aging_params
                  Speed_PV = &
                     this%leafset(leafset_idx)%leaf(i)%getPhotoSynthesisSpeedPerVolume(dt=dt, air=air)

                  if(this%leafset(leafset_idx)%leaf(i)%femdomain%ne() /= size(Speed_PV))then
                     print *, "[ERROR] getPhotoSynthesisSpeedPerElement "
                     print *, ">> this%leafset(leafset_idx)%leaf(i)%femdomain%ne()/= size(Speed_PV)"
                     stop
                  endif

                  do j = 1, this%leafset(leafset_idx)%leaf(i)%femdomain%ne()
                     elem_id = elem_id + 1
                     photosynthesis(elem_id) = Speed_PV(j) &
                        *this%leafset(leafset_idx)%leaf(i)%femdomain%getVolume(elem=j)

                     !print *,"PV, V", Speed_PV(j),this%leafset(leafset_idx)%leaf(i)%femdomain%getVolume(elem=j)
!                     write(debug%fh,*) this%leafset(leafset_idx)%leaf(i)%femdomain%getVolume(elem=j), Speed_PV(j)
                  end do
               end if
            end do
         enddo
      endif

      if(allocated(this%branch_meristem))then
         do branch_idx=1,size(this%branch_meristem)
            if(allocated(this%branch_meristem(branch_idx)%leafset))then
               do leafset_idx=1,size(this%branch_meristem(branch_idx)%leafset)
                  do i=1,size(this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf)
                     if (this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%femdomain%empty()) cycle
                     ! for each leaves, compute photosynthesis ratio (g/m^3)
                     this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%aging_params &
                        = this%leaf_aging_params

                     Speed_PV = &
                        this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%getPhotoSynthesisSpeedPerVolume(&
                           dt=dt, air=air)

                     do j = 1, this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%femdomain%ne()
                        elem_id = elem_id + 1

                        photosynthesis(elem_id) = Speed_PV(j) &
                           *this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%femdomain%getVolume(elem=j)

                        ! print *,"PV, V", Speed_PV(j),this%leafset(leafset_idx)%leaf(i)%femdomain%getVolume(elem=j)
!                        write(debug%fh,*) this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)&
!                           %femdomain%getVolume(elem=j), Speed_PV(j)
                     end do
                     !do j = 1, this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%femdomain%ne()
                     !   elem_id = elem_id + 1
                     !   this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(i)%ppfd(j) = ppfd(elem_id) !- photosynthesis(elem_id)
                     !end do
                  enddo
               enddo
            endif
         enddo
      endif

      !call debug%close()

   end function
! ############################################################################

! ############################################################################
function getRespirationSpeedPerElemMer(this, air, dt) result(respiration)

   ! unit:: g/elem
   class(Meristem_), intent(inout) :: this
   !type(Light_),optional, intent(in)    :: light
   type(Air_), intent(in)    :: Air
   real(real64), intent(in) :: dt
   real(real64), allocatable :: respiration(:), Speed_PV(:)
   type(Stem_),allocatable :: Stem(:),Peti(:)
   !integer(int32), allocatable :: NumberOfElement(:)
   integer(int32) :: i, j, offset, elem_id,leafset_idx,branch_idx
   type(IO_) :: debug
   
   ! stem

   respiration = zeros(this%ne())
   elem_id = 0

   stem = this%getStem()
   peti = this%getPeti()
   ! stem
   do i=1,size(stem)
      if(stem(i)%femdomain%empty()) cycle
      stem(i)%aging_params = this%stem_aging_params
      Speed_PV = stem(i)%getRespirationSpeedPerVolume(dt=dt,air=air)
      do j=1,size(Speed_PV)
         elem_id = elem_id + 1
         respiration(elem_id) = Speed_PV(j)*stem(i)%femdomain%getVolume(elem=j)
      enddo
   enddo

   ! peti
   do i=1,size(peti)
      if(peti(i)%femdomain%empty()) cycle
      peti(i)%aging_params = this%peti_aging_params
      Speed_PV = peti(i)%getRespirationSpeedPerVolume(dt=dt,air=air)
      do j=1,size(Speed_PV)
         elem_id = elem_id + 1
         respiration(elem_id) = Speed_PV(j)*peti(i)%femdomain%getVolume(elem=j)
      enddo
   enddo



end function
! ############################################################################


! ########################################
   recursive function numStemAndPeti_Meristem(this) result(ret)
      class(Meristem_), intent(in) :: this
      integer(int32) :: ret(1:2), indernode_idx, branch_idx, leafset_idx,loc_peti_idx

      ret = [0,0]

      ! count stem (/= peti)
      if(allocated(this%stem))then
         do indernode_idx = 1, size(this%stem)
            if (this%stem(indernode_idx)%femdomain%Mesh%empty() ) then
               cycle
            else
               ret(1) = ret(1) + 1
            end if
         end do
      endif

      if(allocated(this%branch_meristem))then
         do branch_idx = 1,size(this%branch_meristem)
            ret(:) = ret(:) + this%branch_meristem(branch_idx)%numStemAndPeti()
         enddo
      endif

      if(allocated(this%leafset))then
         do leafset_idx = 1,size(this%leafset)
            if(allocated(this%leafset(leafset_idx)%peti))then
               do loc_peti_idx=1,size(this%leafset(leafset_idx)%peti)
                  if(this%leafset(leafset_idx)%peti(loc_peti_idx)%femdomain%Mesh%empty())then
                     cycle
                  else
                     ret(2) = ret(2) + 1
                  endif
               enddo
            endif
         enddo
      endif

   end function
! ########################################




! ########################################
   recursive function numLeaf_Meristem(this) result(ret)
      class(Meristem_), intent(in) :: this
      integer(int32) :: ret, leafset_idx, loc_leaf_idx

      ret = 0

      if(allocated(this%leafset))then
         do leafset_idx = 1,size(this%leafset)
            if(allocated(this%leafset(leafset_idx)%leaf))then
               do loc_leaf_idx=1,size(this%leafset(leafset_idx)%leaf)
                  if(this%leafset(leafset_idx)%leaf(loc_leaf_idx)%femdomain%Mesh%empty())then
                     cycle
                  else
                     ret = ret + 1
                  endif
               enddo
            endif
         enddo
      endif

   end function
! ########################################

!function number_of_elements_per_femdomains_Mer(this) result(ne_vec)
!   class(Meristem_),intent(in) :: this
!   integer(int32),allocatable :: ne_vec(:)
!
!   allocate(ne_vec(0))
!   if(allocated(this%stem))then
!      do i=1,size(this%stem)
!         ne_vec = ne_vec // [this%stem(i)%femdomain%ne()]
!      enddo
!   endif
!
!   if(allocated(this%leafset))then
!      do i=1,size(this%leafset)
!         ne_vec = ne_vec // this%leafset(i)%number_of_elements_per_femdomains()
!      enddo
!   endif
!   
!   if(allocated(this%branch_meristem))then
!      do i=1,size(this%branch_meristem)
!         ne_vec = ne_vec // this%branch_meristem(i)%number_of_elements_per_femdomains()
!      enddo
!   endif
!
!end function


! #####################################################
function ne_stem_vec(stem_vector) result(ne_val)
   type(stem_),intent(in) :: stem_vector(:)
   integer(int32) :: ne_val, i

   ne_val = 0
   do i=1,size(stem_vector)
      ne_val = ne_val + stem_vector(i)%femdomain%ne()
   enddo

end function
! #####################################################


! #####################################################
function ne_leaf_vec(leaf_vector) result(ne_val)
   type(leaf_),intent(in) :: leaf_vector(:)
   integer(int32) :: ne_val, i

   ne_val = 0
   do i=1,size(leaf_vector)
      ne_val = ne_val + leaf_vector(i)%femdomain%ne()
   enddo

end function
! #####################################################

subroutine fall_old_leaves_MeristemClass(this,elem_photospeed)
   class(Meristem_),intent(inout) :: this
   real(real64),intent(in) :: elem_photospeed(:)
   integer(int32) :: i, leaf_idx, elem_idx,leafset_idx,n,m,branch_idx,j,k,l
   integer(int32),allocatable :: should_be_removed(:), removed_leafset(:)
   type(Leaf_),allocatable :: leaf_buf(:)
   type(Leafset_),allocatable :: leafset_buf(:)
   integer(int32),allocatable :: leafset2stem_buf(:,:)

   ! remove leaves
   elem_idx = ne(this%getStem())+ne(this%getPeti())
   if(allocated(this%leafset))then
      do leafset_idx=1,size(this%leafset)
         do leaf_idx=1,size(this%leafset(leafset_idx)%leaf)
            n = elem_idx + 1
            m = elem_idx + this%leafset(leafset_idx)%leaf(leaf_idx)%ne()
            if(sum(elem_photospeed(n:m) )<=0.0d0 )then
               ! remove this leaf since the leaf is merely respiring.
               call this%leafset(leafset_idx)%leaf(leaf_idx)%femdomain%remove()
            endif
            elem_idx = elem_idx + this%leafset(leafset_idx)%leaf(leaf_idx)%ne()
         enddo
      enddo
   endif

   if(allocated(this%branch_meristem))then
      do branch_idx=1,size(this%branch_meristem)
         if(allocated(this%branch_meristem(branch_idx)%leafset))then
            do leafset_idx=1,size(this%branch_meristem(branch_idx)%leafset)
               do leaf_idx=1,size(this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf)
                  n = elem_idx + 1
                  m = elem_idx + &
                  this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(leaf_idx)%ne()
                  if(sum(elem_photospeed(n:m) )<=0.0d0 )then
                     ! remove this leaf since the leaf is merely respiring.
                     call this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(leaf_idx)%femdomain%remove()
                  endif
                  elem_idx = elem_idx + this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(leaf_idx)%ne()
               enddo
            enddo
         endif
      enddo
   endif

   ! deallocate removed leaves
   if(allocated(this%leafset))then
      removed_leafset = int(zeros(size(this%leafset)))
      
      do leafset_idx=1,size(this%leafset)
         
         should_be_removed = int(zeros(size(this%leafset(leafset_idx)%leaf)))
         do leaf_idx=1,size(this%leafset(leafset_idx)%leaf)
            if(this%leafset(leafset_idx)%leaf(leaf_idx)%femdomain%empty())then
               should_be_removed(leaf_idx) = 1
            endif
         enddo
         if(sum(should_be_removed) == 0)then
            cycle
         else
            if(allocated(leaf_buf)) deallocate(leaf_buf)
            if(size(should_be_removed)==sum(should_be_removed))then
               removed_leafset(leafset_idx) = 1
               cycle
            endif
            allocate(leaf_buf(size(should_be_removed)-sum(should_be_removed)))
            j = 0
            do leaf_idx=1,size(this%leafset(leafset_idx)%leaf)
               if(should_be_removed(leaf_idx)==0)then
                  j = j + 1
                  leaf_buf(j) = this%leafset(leafset_idx)%leaf(leaf_idx)
               else
                  cycle
               endif
            enddo
            deallocate(this%leafset(leafset_idx)%leaf)
            this%leafset(leafset_idx)%leaf = leaf_buf
         endif
      enddo

      if(sum(removed_leafset)/=0)then
         if(sum(removed_leafset) == size(removed_leafset))then
            deallocate(this%leafset)
         else
            if(allocated(leafset_buf))then
               deallocate(leafset_buf)
            endif
            allocate(leafset_buf(size(removed_leafset)-sum(removed_leafset)))
            leafset2stem_buf = zeros(size(removed_leafset)-sum(removed_leafset),size(this%leafset2stem,2))

            j = 0
            do i=1,size(this%leafset) 
               if(removed_leafset(i)==0)then
                  j = j + 1
                  leafset_buf(j) = this%leafset(i)
                  leafset2stem_buf(j,:) = this%leafset2stem(i,:)
               endif
            enddo
            this%leafset = leafset_buf
            this%leafset2stem = leafset2stem_buf

            
         endif
         
      endif

      
   endif

   if(allocated(this%branch_meristem))then
      do branch_idx=1,size(this%branch_meristem)
         if(allocated(this%branch_meristem(branch_idx)%leafset))then
            removed_leafset = int(zeros(size(this%branch_meristem(branch_idx)%leafset)))
            do leafset_idx=1,size(this%branch_meristem(branch_idx)%leafset)

               should_be_removed = int(zeros(size(this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf)))
               do leaf_idx=1,size(this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf)
                  if(this%branch_meristem(branch_idx)%leafset(leafset_idx)%&
                     leaf(leaf_idx)%femdomain%empty())then
                     should_be_removed(leaf_idx) = 1
                  endif
               enddo


               if(sum(should_be_removed) == 0)then
                  cycle
               else
                  if(size(should_be_removed)==sum(should_be_removed))then
                     removed_leafset(leafset_idx) = 1
                     cycle
                  endif

                  if(allocated(leaf_buf)) deallocate(leaf_buf)
                  allocate(leaf_buf(size(should_be_removed)-sum(should_be_removed)))
                  j = 0
                  do leaf_idx=1,size(this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf)
                     if(should_be_removed(leaf_idx)==0)then
                        j = j + 1
                        leaf_buf(j) = this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf(leaf_idx)
                     else
                        cycle
                     endif
                  enddo
                  deallocate(this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf)
                  this%branch_meristem(branch_idx)%leafset(leafset_idx)%leaf = leaf_buf
               endif
            enddo


            if(sum(removed_leafset)/=0)then
               if(sum(removed_leafset) == size(removed_leafset))then
                  deallocate(this%branch_meristem(branch_idx)%leafset)
               else
                  if(allocated(leafset_buf))then
                     deallocate(leafset_buf)
                  endif
                  allocate(leafset_buf(size(removed_leafset)-sum(removed_leafset)))
                  leafset2stem_buf = zeros(size(removed_leafset)-sum(removed_leafset),&
                     size(this%branch_meristem(branch_idx)%leafset2stem,2))
                  j = 0
                  do i=1,size(this%branch_meristem(branch_idx)%leafset) 
                     if(removed_leafset(i)==0)then
                        j = j + 1
                        leafset_buf(j) = this%branch_meristem(branch_idx)%leafset(i)
                        leafset2stem_buf(j,:) = this%branch_meristem(branch_idx)%leafset2stem(i,:)
                     endif
                  enddo
                  this%branch_meristem(branch_idx)%leafset = leafset_buf
                  this%branch_meristem(branch_idx)%leafset2stem = leafset2stem_buf
               endif

            endif


         endif
      enddo
   endif



end subroutine
! #####################################################

! #####################################################
function getLeafArea_Meristem(this) result(LeafArea)
   class(Meristem_),intent(in) :: this
   real(real64) :: LeafArea
   type(Leaf_),allocatable :: leaf(:)
   integer(int32) :: i


   LeafArea = 0.0d0
   leaf = this%getLeaf()
   do i=1,size(leaf)
      LeafArea = LeafArea + leaf(i)%getLeafArea()
   enddo

end function
! #####################################################


! #####################################################
function getNumberOfInternode_Meristem(this) result(num_internode)
   class(Meristem_),intent(in) :: this
   integer(int32) :: num_internode
   
   num_internode = size(this%get_stem_femdomain_ptr())

end function
! #####################################################


! #####################################################
function getNumberOfBranch_Meristem(this) result(num_branch)
   class(Meristem_),intent(in) :: this
   integer(int32) :: num_branch
   
   if(allocated(this%branch_meristem))then
      num_branch = size(this%branch_meristem)
   else
      num_branch = 0
   endif

end function
! #####################################################




! ######################################################
function trial_grow_MeristemClass(this,params,dt,no_division) result(trial_growth)
   class(Meristem_),intent(in) :: this
   type(Meristem_) :: trial_growth

   real(real64),intent(in) :: params(:),dt
   
   logical,optional,intent(in) :: no_division
   real(real64) :: base_angle_x, base_angle_z,leafset_angles(1:3)
   real(real64),allocatable :: this_params(:)
   integer(int32) :: i,j,leafset_idx,stem_idx



   trial_growth = this

   call trial_growth%update_leafset_params(size_factor_function=default_size_factor_function)


   do i=1,size(trial_growth%stem)
      call trial_growth%grow_internode(idx=i,params=params,dt=dt,no_division=no_division,&
         size_factor_function=default_size_factor_function)
      !call trial_growth%grow_internode(idx=i,params=params,dt=dt,no_division=no_division)
   enddo

   if(allocated(trial_growth%leafset))then
      do i=1,size(trial_growth%leafset)
         if(trial_growth%leafset(i)%is_empty()) cycle
         trial_growth%leafset(i)%leaf_aspect_ratio= trial_growth%leaf_aspect_ratio
         trial_growth%leafset(i)%max_angle_deg = trial_growth%leaf_max_angle_deg
         trial_growth%leafset(i)%leaf_curvature = trial_growth%leaf_curvature
         this_params = this%leafset_params(i,6:11)
         call trial_growth%leafset(i)%grow_peti_and_leaf(params=this_params,dt=dt,&
            base_petiole_length_ratio=trial_growth%base_petiole_length_ratio)
      enddo
   endif


   if(allocated(trial_growth%branch_meristem))then
      do i=1,size(trial_growth%branch_meristem)
         call trial_growth%branch_meristem(i)%grow(params=params,dt=dt,no_division=no_division)
         
         ! set position of the base of branch
         leafset_idx = trial_growth%branch_meristem(i)%leafset_idx
         stem_idx = trial_growth%branch_meristem(i)%stem_idx
         leafset_angles = trial_growth%getLeafsetAngles(stem_idx=stem_idx,leafset_idx=leafset_idx)
         base_angle_x = 0.50d0*leafset_angles(1) ! half of the angle of leafset from its vertical positon.
         base_angle_z = leafset_angles(3) ! same direction as leafset   
         !print *, base_angle_x
         do j=1,size(trial_growth%branch_meristem(i)%stem)
            call trial_growth%branch_meristem(i)%stem(j)%rotate(x=base_angle_x,z=base_angle_z,reset=.true.)
            base_angle_x = base_angle_x/2.0d0 ! gradually reaches vertical position
         enddo
         call trial_growth%branch_meristem(i)%update()
      enddo
   endif

   call trial_growth%update()


   
end function
! ######################################################

subroutine assign_meristemclass(b,a)
   type(Meristem_),intent(inout) :: b
   type(Meristem_),intent(in) :: a
   
   b%FEMDomain = a%FEMDomain
   if(allocated(a%stem))then
      b%stem = a%stem !(:)
   endif
   if(allocated(a%leafset))then
      b%leafset = a%leafset !(:) ! petioles and leaves
   endif
   if(allocated(a%branch_meristem))then
      b%branch_meristem = a%branch_meristem !(:)
   endif
      
   b%num_leaf_per_leafset = a% num_leaf_per_leafset ! = 3
   b%num_leafset_per_stem = a% num_leafset_per_stem ! = 1
   b%leafset2stem = a% leafset2stem !(:,:)

   !if(allocated(a%type))then
   !   ! type(root_),allocatable :: root(:)
   !endif
      
   if(allocated(a%stem2stem))then
      b%stem2stem = a%stem2stem!(:,:)
   endif
   if(allocated(a%leaf2stem))then
      b%leaf2stem = a%leaf2stem!(:,:)
   endif

   b%cross_section_shape = a%cross_section_shape! = 1 ! PF_Meristem_SHAPE_RECTANGULAR
      
   b% rot_x = a% rot_x! = 0.0d0
   b% rot_y = a% rot_y! = 0.0d0
   b% rot_z = a% rot_z! = 0.0d0
   b% disp_x = a% disp_x! = 0.0d0
   b% disp_y = a% disp_y! = 0.0d0
   b% disp_z = a% disp_z! = 0.0d0
   b% EdgeNodeID = a% EdgeNodeID!(4)
   b% EdgeElemID = a% EdgeElemID!(4)
      
   if(allocated(a%I_planeNodeID))then
      b%I_planeNodeID = a%I_planeNodeID!(:)
   endif
   if(allocated(a%I_planeElementID))then
      b%I_planeElementID = a%I_planeElementID!(:)
   endif
   if(allocated(a%II_planeNodeID))then
      b%II_planeNodeID = a%II_planeNodeID!(:)
   endif
   if(allocated(a%II_planeElementID))then
      b%II_planeElementID = a%II_planeElementID!(:)
   endif
   b%A_PointNodeID = a%A_PointNodeID!
   b%B_PointNodeID = a%B_PointNodeID!
   b%C_PointNodeID = a%C_PointNodeID!
   b%D_PointNodeID = a%D_PointNodeID!

   b%A_PointElementID = a%A_PointElementID!
   b%B_PointElementID = a%B_PointElementID!
   b%xnum = a%xnum! = 10
   b%ynum = a%ynum! = 10
   b%znum = a%znum! = 10

      ! branch
   b%stem_idx = a%stem_idx! ! the branch is branched from stem with stem_idx
   b%leafset_idx = a%leafset_idx! ! the branch is branched from leafset with leafset_idx
      

      ! meristem parameters
   b%num_shoot_stem_node = a%num_shoot_stem_node! = 2
   b%slight_overlap_epsilon = a%slight_overlap_epsilon! = dble(1.0e-3)
   b%top_meristem_aspect_ratio = a%top_meristem_aspect_ratio! = 0.50d0 ! if length/width is greater than this value, internodes are divided.
   b%leaf_aspect_ratio = a%leaf_aspect_ratio! = 0.50d0 ! width/length
   b%leaf_max_angle_deg = a%leaf_max_angle_deg! = 90.0d0!
   b%leaf_curvature = a%leaf_curvature! = 0.0d0

      ! position in a whole structure (single plant)
   b%MeristemID = a%MeristemID! = -1
   b%InterNodeID = a%InterNodeID! = -1

   b%already_grown = a%already_grown! = .false.

      ! physical parameter
   if(allocated(a%DryDensity))then
      b%DryDensity = a%DryDensity!(:)  ! element-wise
   endif
   if(allocated(a%WaterContent))then
      b%WaterContent = a%WaterContent!(:)! element-wise
   endif

      ! For deformation analysis
   if(allocated(a%YoungModulus))then
      b%YoungModulus = a%YoungModulus!(:)! element-wise
   endif
   if(allocated(a%PoissonRatio))then
      b%PoissonRatio = a%PoissonRatio!(:)! element-wise
   endif
   if(allocated(a%CrossSectionalYoungModulus))then
      b%CrossSectionalYoungModulus = a%CrossSectionalYoungModulus!(:) !element-wise
   endif
   if(allocated(a%Density))then
      b%Density = a%Density!(:)     ! element-wise
   endif
   if(allocated(a%CarbonDiffusionCoefficient))then
      b%CarbonDiffusionCoefficient = a%CarbonDiffusionCoefficient!(:) ! element-wise
   endif
   if(allocated(a%Stress))then
      b%Stress = a%Stress!(:, :, :)     ! Gauss point-wise
   endif
   if(allocated(a%Displacement))then
      b%Displacement = a%Displacement!(:, :) ! node-wise, three dimensional
   endif
   if(allocated(a%BoundaryTractionForce))then
      b%BoundaryTractionForce = a%BoundaryTractionForce!(:, :) ! node-wise, three dimensional
   endif
   if(allocated(a%BoundaryDisplacement))then
      b%BoundaryDisplacement = a%BoundaryDisplacement!(:, :) ! node-wise, three dimensional
   endif

   b% Division = a% Division!

      ! growth parameters
      ! >>>>>>>>>>>
   b%K_L = a%K_L! ! Maximum value for length of a single internode (m)
   b%K_R = a%K_R! ! Maximum value for radius of a single internode (m)
   b%T_L = a%T_L! ! time constant for longitudinal growth of a single internode (m)
   b%T_R = a%T_R! ! time constant for cross-sectional growth of a single internode (m)
   b%temp_time_limit = a%temp_time_limit! max. stem generation speed (day/node)

   b%species = a%species! = 1  ! soybean

      ! phenotype parameters
   b%leaf_directional_angle = a%leaf_directional_angle! = 120.0d0 ! 
   b%last_leaf_direction = a%last_leaf_direction! = 0.0d0
   b%base_petiole_length_ratio = a%base_petiole_length_ratio! = 3.0d0 ! (primary petiole length)/(n-th petiole length(n>1))

       ! <<<<<<<<<<<
   b%material_is_set = a%material_is_set!

   if(associated(a% pMeristem))then
      b% pMeristem = a% pMeristem
   endif

      ! physiological factor
   b%R_d = a%R_d! = 1.0d0 ! 暗呼吸速度, mincro-mol/m-2/s
   b%default_CarbonDiffusionCoefficient = a%default_CarbonDiffusionCoefficient! = 0.0010d0 ! ソースの拡散係数 mincro-mol/m^2/m/s

   b%average_drydensity = a%average_drydensity! = 0.30d0 ! dry bulk density, g/cm^3 = t/m^3

      ! photosynthesis simulation
   if(allocated(a%Photosynthate_n))then
      b%Photosynthate_n = a%Photosynthate_n!(:)
   endif


      ! default values for aging of leaves in terms of photosynthesis
      ! 0 ~ 10 days (lighting hour : 8hr/day) : 100 %
      ! 10 ~ 40 days (lighting hour : 8hr/day): 100 % -> 0 %
      ! 30 days~ (lighting hour: 8hr/day): 0 %
   b%stem_aging_params = a%stem_aging_params!(1:2) = [30.0d0, 50.0d0]*60.0d0*60.0d0*24.0d0
   b%peti_aging_params = a%peti_aging_params!(1:2) = [30.0d0, 50.0d0]*60.0d0*60.0d0*24.0d0
   b%leaf_aging_params = a%leaf_aging_params!(1:2) = [30.0d0, 50.0d0]*60.0d0*60.0d0*24.0d0


end subroutine


! #####################################################################
subroutine update_leafset_params_MeristemC(this,size_factor_function)
   class(Meristem_),intent(inout) :: this
   integer(int32) :: leafset_idx

   interface
      function size_factor_function(node_idx,coeff) result(size_factor)
         use iso_fortran_env
         implicit none
         integer(int32), intent(in) :: node_idx
         real(real64),intent(in) :: coeff(:)
         real(real64) :: size_factor
      end function
   end interface
   
   ! Maximum value vary in size depending on the node.
   ! These sizes are preliminary set by the values
   ! on the initial leaves.
   ! This subroutine takes into account the fact that 
   ! the sizes of the leaves and petioles vary from node to node.
   do leafset_idx=1,size(this%leafset)
      ! (6:8) are the parameters for shape
      ! (9:11) are time constants
      !print *, "sf",size_factor_function(node_idx=leafset_idx,coeff=this%size_coeff(:))
      this%leafset_params(leafset_idx,6:8) = size_factor_function(&
         node_idx=leafset_idx,coeff=this%size_coeff(:)) &
          * this%leafset_init_params(6:8)
   enddo


end subroutine
! #####################################################################



! #####################################################################
function default_size_factor_function(node_idx,coeff) result(size_factor) 
   ! size_factor = 1 :: size of primary leaves
   ! size_factor = 2 :: double size of primary leaves
   integer(int32),intent(in) :: node_idx ! node idx
   real(real64),intent(in) :: coeff(:)
   real(real64) :: size_factor

   size_factor = 1.0d0 !+ &
      !1.50d0*max(dble(node_idx)*(coeff(1)*2.0d0 - dble(node_idx))/(coeff(1)*coeff(1)),0.0d0)

end function
! #####################################################################


! >>  to be implemented.
! #####################################################################
function get_mainstem_length_Meristem(this) result(ret)
   class(Meristem_),intent(in) :: this
   real(real64)  :: ret
   integer(int32) :: i


   ret = 0.0d0
   do i=1,size(this%stem)
      ret = ret + this%stem(i)%getLength()
   enddo
   

end function
! #####################################################################

! #####################################################################
function get_mainstem_num_node_Meristem(this) result(ret)
   class(Meristem_),intent(in) :: this
   integer(real64) :: ret
   integer(int32) :: i

   ret = size(this%stem)

end function
! #####################################################################


! #####################################################################
function get_num_branch_Meristem(this) result(ret)
   class(Meristem_),intent(in) :: this
   integer(int32) ::  ret

   ret = size(this%branch_meristem)

end function
! #####################################################################



! #####################################################################
function default_temp_response_function(temp_range,temp,gain) result(ret)
   real(real64),intent(in) :: temp, temp_range(1:2)
   real(real64),optional,intent(in) :: gain
   real(real64) :: ret, opt_temp, current_gain,sigma,x,b
   

   ! 1
   !              _________
   !             /         \
   !            /           \
   ! 0 ________/             \_________
   !             |          |
   !           optimial temperature range
   !                          
   
   ! 	quinque-linear function
   !current_gain = input(default=1.50d0,option=gain)
   !ret = sigmoid(temp,[current_gain,temp_range(1)])-sigmoid(temp,[current_gain,temp_range(2)])
   !return

   sigma = (maxval(temp_range)-minval(temp_range))*0.5d0
   x = temp
   b = average(temp_range)
   ret = exp(  - ((x-b)**2)/2.0d0/sigma/sigma )



end function
! #####################################################################


end module