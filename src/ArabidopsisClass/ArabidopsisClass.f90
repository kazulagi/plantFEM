module ArabidopsisClass
   use LeafClass
   use StemClass
   use PodClass
   use RootClass
   implicit none

   type :: Arabidopsis_internode_info_
      real(real64), allocatable :: FinalInterNodeLength(:)
      real(real64), allocatable :: FinalPetioleLength(:)
      real(real64), allocatable :: FinalLeafLength(:)
      real(real64), allocatable :: FinalLeafWidth(:)
   end type

   type :: Arabidopsis_NodeID_Branch_
      integer(int32), allocatable :: ID(:)
   contains
      !procedure, public :: sync => syncArabidopsis_NodeID_Branch
   end type


   type :: Arabidopsis_

      ! >>> new implementation
      real(real64),allocatable :: ms_internode_length(:)
      real(real64),allocatable :: ms_internode_radius(:)
      real(real64),allocatable :: ms_internode_angle(:)
      real(real64),allocatable :: ms_leaf_length(:)
      real(real64),allocatable :: ms_leaf_width(:)
      real(real64),allocatable :: ms_leaf_thickness(:)

      real(real64),allocatable :: ms_pod_length(:)
      real(real64),allocatable :: ms_pod_radius(:)
      real(real64),allocatable :: ms_pod_angle(:)
      integer(int32),allocatable :: ms_pod_stem_idx(:)


      ! >>> regady implementation

      integer(int32) :: TYPE_STEM = 1
      integer(int32) :: TYPE_LEAF = 2
      integer(int32) :: TYPE_ROOT = 3
      ! 節-節点データ構造
      type(Mesh_) :: struct
      integer(int32), allocatable :: leaf2stem(:, :)
      integer(int32), allocatable :: pod2stem(:, :)
      integer(int32), allocatable :: stem2stem(:, :)
      integer(int32), allocatable :: root2stem(:, :)
      integer(int32), allocatable :: root2root(:, :)

      real(real64)   :: mainstem_length
      real(real64)   :: mainstem_width
      integer(int32) :: mainstem_node

      real(real64)   :: mainroot_length
      real(real64)   :: mainroot_width
      integer(int32) :: mainroot_node

      integer(int32) :: num_branch_root
      integer(int32) :: num_branch_root_node

      real(real64) :: ms_angle_ave = 0.0d0
      real(real64) :: ms_angle_sig = 0.0d0

      integer(int32), allocatable :: Leaf_From(:)

      !real(real64),allocatable :: leaf_Length(:)
      !real(real64),allocatable :: leaf_Width(:)

      real(real64), allocatable :: leaf_curvature(:)

      real(real64), allocatable :: leaf_thickness_ave(:)
      real(real64), allocatable :: leaf_thickness_sig(:)

      real(real64), allocatable :: leaf_angle_ave_x(:)
      real(real64), allocatable :: leaf_angle_sig_x(:)
      real(real64), allocatable :: leaf_angle_ave_z(:)
      real(real64), allocatable :: leaf_angle_sig_z(:)

      real(real64), allocatable :: leaf_length_ave(:)
      real(real64), allocatable :: leaf_length_sig(:)
      real(real64), allocatable :: leaf_width_ave(:)
      real(real64), allocatable :: leaf_width_sig(:)

      integer(int32) :: num_leaf
      integer(int32) :: num_stem
      integer(int32) :: num_pod
      integer(int32) :: num_root

      ! 器官オブジェクト配列
      !type(FEMDomain_), allocatable :: leaf_list(:)
      !type(FEMDomain_), allocatable :: stem_list(:)
      !type(FEMDomain_), allocatable :: root_list(:)

      character(:), allocatable :: LeafSurfaceData
      type(Leaf_), allocatable :: Leaf(:)
      type(Stem_), allocatable :: Stem(:)
      type(Pod_), allocatable  :: Pod(:)
      type(Root_), allocatable :: Root(:)

      integer(int32), allocatable :: NodeID_MainStem(:)
      type(Arabidopsis_NodeID_Branch_), allocatable :: NodeID_Branch(:)

      logical ::  inLoop = .false.
      real(real64) :: hours = 0.0d0

      ! growth simulation
      real(real64) :: FullyExpanded_stem_threshold = 0.10d0
      integer(int32) :: MaxBranchNum = 20
      type(Arabidopsis_internode_info_), allocatable :: InterNodeInfo(:)
      real(real64) :: default_Leaf_growth_ratio = 1.0d0/3.0d0
      real(real64) :: default_Stem_growth_ratio = 1.0d0/3.0d0
      integer(int32), allocatable :: MainStem_num_branch(:)
      real(real64) :: apical_dominance_distance = 1.0d0

      ! シミュレータ
      type(ContactMechanics_) :: contact
      real(real64) :: Gravity_acceralation = 9.810d0
      real(real64) :: PenaltyParameter = 100000.0d0
      logical :: GaussPointProjection = .false.

      ! setting
      integer(int32) :: stem_division(1:3) = [5, 5, 20]
      integer(int32) :: leaf_division(1:3) = [4, 2, 10] ! [width, thickness, length]
      integer(int32) :: pod_division(1:3) = [4, 4, 10] ! [radius1, radius2, length]

      ! only for arabi @ 2025.10.14


      ! k : rosette_limit
      ! 1 to k-th internodes are the rosette
      ! (k+1)th to n-th internodes are stalks
      integer(int32) :: ms_rosette_limit

      ! k : leaf_limit
      ! 1 to k-th internodes are the leaf
      ! (k+1)th to n-th internodes are flower stalks 
      integer(int32) :: ms_leaf_limit
      

   contains
      procedure, public :: init => createArabidopsis
      procedure, public :: create => createArabidopsis
      procedure, public :: AddBranch => AddBranchArabidopsis

      !procedure, public :: msh => mshArabidopsis
      procedure, public :: vtk => vtkArabidopsis
      !procedure, public :: stl => stlArabidopsis
      !procedure, public :: json => jsonArabidopsis
      procedure, public :: update => updateArabidopsis

      ! Editor
!      procedure, public :: remove => removeArabidopsis
!      procedure, public :: rotate => rotateArabidopsis
!      procedure, public :: move => moveArabidopsis

      procedure, public :: getElementList => getElementListArabidopsis
      ! Info
      !procedure,public :: properties => propertiesArabidopsis
      ! number of subdomain
      procedure, public :: ns => nsArabidopsis
      ! number of element
      procedure, public :: ne => neArabidopsis
      ! number of points
      procedure, public :: nn => nnArabidopsis
      ! MemSize
      procedure, public :: checkMemoryRequirement => checkMemoryRequirementArabidopsis
      ! number of stem, leaf ...etc.
      procedure, public :: numleaf => numleafArabidopsis
      procedure, public :: numstem => numstemArabidopsis
      procedure, public :: numroot => numrootArabidopsis
      ! get pointers
      procedure, public :: getFEMDomainPointers => getFEMDomainPointersArabidopsis
      ! check data
      procedure, public :: checkYoungModulus => checkYoungModulusArabidopsis
      procedure, public :: checkPoissonRatio => checkPoissonRatioArabidopsis
      procedure, public :: checkDensity => checkDensityArabidopsis
      ! get data
      procedure, public :: getYoungModulus => getYoungModulusArabidopsis
      procedure, public :: getPoissonRatio => getPoissonRatioArabidopsis
      procedure, public :: getDensity => getDensityArabidopsis

      procedure, public :: getYoungModulusField => getYoungModulusFieldArabidopsis
      procedure, public :: getPoissonRatioField => getPoissonRatioFieldArabidopsis
      procedure, public :: getDensityField => getDensityFieldArabidopsis
      procedure, public :: getStressField => getStressFieldArabidopsis

      ! alternative setters
      procedure, public :: setYoungModulus => setYoungModulusArabidopsis
      procedure, public :: setPoissonRatio => setPoissonRatioArabidopsis
      procedure, public :: setDensity => setDensityArabidopsis

      ! simulator
      procedure, public :: getEigenMode => getEigenModeArabidopsis
      procedure, public :: getDisplacement => getDisplacementArabidopsis
      procedure, public :: deform => deformArabidopsis

      ! export eigen modes and frequency
      procedure, public :: export_eig => export_eigArabidopsis

   end type
contains

! #############################################################
   subroutine createArabidopsis(this, config, debug)
      class(Arabidopsis_), intent(inout) :: this
      character(*), intent(in) :: config
      character(:), allocatable :: line
      logical, optional, intent(in) :: debug
      logical :: debug_log
      type(IO_) :: Arabidopsisconfig
      type(Random_) :: random
      integer(int32)::i, n, j, k, num_leaf, num_stem_node, num_branch, cpid,num_pod,&
         branch_idx
      real(real64) :: x_A(1:3)

      
      ! >> regacy mode
      debug_log = input(default=.false., option=debug)
      cpid = 0

      if (debug_log) then
         cpid = cpid + 1
         call print("createArabidopsis #"+str(cpid))
      end if

      
      this%ms_internode_length = Arabidopsisconfig%parse_json(config,to_list("Mainstem","internode_length")) .as. real64_vector() 
      this%ms_internode_radius = Arabidopsisconfig%parse_json(config,to_list("Mainstem","internode_radius")) .as. real64_vector() 
      this%ms_internode_angle  = Arabidopsisconfig%parse_json(config,to_list("Mainstem","internode_angle" )) .as. real64_vector() 

      this%ms_leaf_length     = Arabidopsisconfig%parse_json(config,to_list("Mainstem","leaf_length")) .as. real64_vector() 
      this%ms_leaf_width      = Arabidopsisconfig%parse_json(config,to_list("Mainstem","leaf_width")) .as. real64_vector() 
      this%ms_leaf_thickness  = Arabidopsisconfig%parse_json(config,to_list("Mainstem","leaf_thickness" )) .as. real64_vector() 

      this%ms_pod_radius  = Arabidopsisconfig%parse_json(config,to_list("Mainstem","pod_radius" )) .as. real64_vector() 
      this%ms_pod_length  = Arabidopsisconfig%parse_json(config,to_list("Mainstem","pod_length" )) .as. real64_vector() 
      this%ms_pod_angle   = Arabidopsisconfig%parse_json(config,to_list("Mainstem","pod_angle"  )) .as. real64_vector() 

      this%ms_pod_stem_idx   = Arabidopsisconfig%parse_json(config,to_list("Mainstem","pod_stem_idx"  )) .as. int32_vector() 

      
      
      this%Leaf_From = 0.0d0*this%ms_leaf_length
      do i=1,size(this%Leaf_From)
         this%Leaf_From(i) = i
      enddo

      
      !this%mainstem_length = freal(Arabidopsisconfig%parse(config, key1="Mainstem", key2="Length"))
      !this%mainstem_width = freal(Arabidopsisconfig%parse(config, key1="Mainstem", key2="Width"))
      !this%mainstem_node = fint(Arabidopsisconfig%parse(config, key1="Mainstem", key2="Node"))
      !this%ms_angle_ave = freal(Arabidopsisconfig%parse(config, key1="Mainstem", key2="ms_angle_ave"))
      !this%ms_angle_sig = freal(Arabidopsisconfig%parse(config, key1="Mainstem", key2="ms_angle_sig"))

      this%mainstem_node = size(this%ms_internode_length)
      if (debug_log) then
         cpid = cpid + 1
         call print("createArabidopsis #"+str(cpid))
      end if



      ! k : rosette_limit
      ! 1 to k-th internodes are the rosette
      ! (k+1)th to n-th internodes are stalks

      !this%ms_rosette_limit = fint(Arabidopsisconfig%parse(config, key1="Mainstem", key2="rosette_limit"))

      ! k : leaf_limit
      ! 1 to k-th internodes are the leaf
      ! (k+1)th to n-th internodes are flower stalks 

      !this%ms_leaf_limit    = fint(Arabidopsisconfig%parse(config, key1="Mainstem", key2="leaf_limit"))
      

      
      ! get number of leaf
      this%num_leaf = size(this%ms_leaf_length)
      this%num_pod = size(this%ms_pod_length)
      
      

      if (debug_log) then
         cpid = cpid + 1
         call print("createArabidopsis #"+str(cpid))
      end if

      allocate (this%leaf_curvature(this%num_leaf))
      allocate (this%leaf_thickness_ave(this%num_leaf))
      allocate (this%leaf_thickness_sig(this%num_leaf))
      allocate (this%leaf_angle_ave_x(this%num_leaf))
      allocate (this%leaf_angle_sig_x(this%num_leaf))
      allocate (this%leaf_angle_ave_z(this%num_leaf))
      allocate (this%leaf_angle_sig_z(this%num_leaf))
      allocate (this%leaf_length_ave(this%num_leaf))
      allocate (this%leaf_length_sig(this%num_leaf))
      allocate (this%leaf_width_ave(this%num_leaf))
      allocate (this%leaf_width_sig(this%num_leaf))
      !allocate (this%leaf_From(this%num_leaf))
      !allocate(this%leaf_Length(this%num_leaf))
      !allocate(this%leaf_Width(this%num_leaf))

      if (debug_log) then
         cpid = cpid + 1
         call print("createArabidopsis #"+str(cpid))
      end if

      if (debug_log) then
         cpid = cpid + 1
         call print("createArabidopsis #"+str(cpid))
      end if

      if (debug_log) then
         cpid = cpid + 1
         call print("createArabidopsis #"+str(cpid))
      end if
      this%num_stem = size(this%ms_internode_length)
      !this%num_root =this%num_branch_root_node + this%mainroot_node

      !allocate (this%stem_list(this%num_stem))
      !allocate (this%leaf_list(this%num_leaf))
      !allocate (this%pod_list(this%num_leaf))
      
      !allocate(this%root_list(this%num_root))

      
      allocate (this%stem(this%num_stem))
      allocate (this%leaf(this%num_leaf))
      allocate (this%pod(this%num_pod))
      !allocate(this%root(this%num_root))

      this%leaf2stem = zeros(this%num_leaf, this%num_stem)
      this%stem2stem = zeros(this%num_stem, this%num_stem)
      this%pod2stem = zeros(this%num_pod, this%num_stem)
      !this%root2stem = zeros( this%num_root , this%num_stem)
      !this%root2root = zeros( this%num_root , this%num_root )

      if (debug_log) then
         cpid = cpid + 1
         call print("createArabidopsis #"+str(cpid))
      end if


      
      


      ! set mainstem
      do i = 1, this%mainstem_node
         this%stem(i)%CROSS_SECTION_SHAPE = PF_STEM_SHAPE_CYLINDER
         call this%stem(i)%init( &
            x_num=this%stem_division(1), &
            y_num=this%stem_division(2), &
            z_num=this%stem_division(3)  &
            )

         call this%stem(i)%resize( &
            x=this%ms_internode_radius(i)*2.0d0, &
            y=this%ms_internode_radius(i)*2.0d0, &
            z=this%ms_internode_length(i) &
            )
         call this%stem(i)%rotate( &
            x=radian(this%ms_internode_angle(i)), &
            y=0.0d0, &
            z=radian(random%random()*360.0d0) &
            )
      end do


      do i = 1, this%mainstem_node - 1
         call this%stem(i + 1)%connect("=>", this%stem(i))
         this%stem2stem(i + 1, i) = 1
      end do

      if (debug_log) then
         cpid = cpid + 1
         call print("createArabidopsis #"+str(cpid))
      end if



      !set leaf
      num_leaf = 0
      do i = 1, this%num_leaf
         num_leaf = num_leaf + 1

         call this%leaf(num_leaf)%init(species=PF_Arabidopsis, &
                                       x_num=this%leaf_division(1), &
                                       y_num=this%leaf_division(2), &
                                       z_num=this%leaf_division(3) &
                                       )
         call this%leaf(num_leaf)%femdomain%resize( &
            y=this%ms_leaf_thickness(num_leaf), &
            z=this%ms_leaf_length(num_leaf), &
            x=this%ms_leaf_width(num_leaf) &
            )
         !call this%leaf(num_leaf)%curve(curvature=this%leaf_curvature(i))

         call this%leaf(num_leaf)%femdomain%rotate( &
               x=radian(90.0d0), &
               y=0.0d0, &
               z=radian(360.0d0*random%random()) &
            )
         call this%leaf(num_leaf)%connect("=>", this%stem(this%Leaf_From(i)))
         this%leaf2stem(num_leaf, this%Leaf_From(i)) = 1
      end do

      !set pod
      num_pod = 0
      do i = 1, this%num_pod
         num_pod = num_pod + 1
         call this%pod(num_pod)%init(species=PF_Arabidopsis, &
                                       x_num=this%pod_division(1), &
                                       y_num=this%pod_division(2), &
                                       z_num=this%pod_division(3) &
                                    )
         call this%pod(num_pod)%femdomain%resize( &
            z=this%ms_pod_length(num_pod), &
            y=this%ms_pod_radius(num_pod)*2.0d0, &
            x=this%ms_pod_radius(num_pod)*2.0d0 &
            )
         !call this%pod(num_pod)%curve(curvature=this%pod_curvature(i))

         call this%pod(num_pod)%femdomain%rotate( &
               x=radian(180.0d0-this%ms_pod_angle(num_pod)), &
               y=0.0d0, &
               z=radian(360.0d0*random%random()) &
            )
         call this%pod(num_pod)%connect("=>", this%stem(this%ms_pod_stem_idx(i)))
         this%pod2stem(num_pod, this%ms_pod_stem_idx(i)) = 1
      end do


      call this%update()

      ! add branch
      num_branch = fint(Arabidopsisconfig%parse_json(config,to_list("Mainstem","num_branch")))

      do branch_idx=1,num_branch
         call this%addBranch(idx=branch_idx,config=config)
      enddo

   end subroutine
! #############################################################


subroutine AddBranchArabidopsis(this,idx,config)
   class(Arabidopsis_),intent(inout) :: this
   integer(int32),intent(in) :: idx
   character(*), intent(in) :: config
   type(IO_) :: f

   real(real64),allocatable :: br_internode_length(:)
   real(real64),allocatable :: br_internode_radius(:)
   real(real64),allocatable :: br_internode_angle(:)
   real(real64),allocatable :: br_leaf_length(:)
   real(real64),allocatable :: br_leaf_width(:)
   real(real64),allocatable :: br_leaf_thickness(:)

   real(real64),allocatable :: br_pod_length(:)
   real(real64),allocatable :: br_pod_radius(:)
   real(real64),allocatable :: br_pod_angle(:)
   integer(int32),allocatable :: br_pod_stem_idx(:)
   integer(int32),allocatable :: leaf_From(:),old_stem2stem(:,:),old_leaf2stem(:,:),old_pod2stem(:,:)

   type(Stem_),allocatable :: stem(:),old_stem(:)
   type(Leaf_),allocatable :: leaf(:),old_leaf(:)
   type(Pod_),allocatable  :: pod(:),old_pod(:)
   integer(int32) :: stem_idx, i, num_stem, from_Idx,num_leaf,num_pod,leaf_idx,pod_idx
   type(Random_) :: random

   from_Idx            = fint(f%parse_json(config,to_list("Branch#"+str(idx),"From")))
   br_internode_length = f%parse_json(config,to_list("Branch#"+str(idx),"internode_length")) .as. real64_vector() 
   br_internode_radius = f%parse_json(config,to_list("Branch#"+str(idx),"internode_radius")) .as. real64_vector() 
   br_internode_angle  = f%parse_json(config,to_list("Branch#"+str(idx),"internode_angle" )) .as. real64_vector() 
   br_leaf_length      = f%parse_json(config,to_list("Branch#"+str(idx),"leaf_length")) .as. real64_vector() 
   br_leaf_width       = f%parse_json(config,to_list("Branch#"+str(idx),"leaf_width")) .as. real64_vector() 
   br_leaf_thickness   = f%parse_json(config,to_list("Branch#"+str(idx),"leaf_thickness" )) .as. real64_vector() 
   br_pod_radius       = f%parse_json(config,to_list("Branch#"+str(idx),"pod_radius" )) .as. real64_vector() 
   br_pod_length       = f%parse_json(config,to_list("Branch#"+str(idx),"pod_length" )) .as. real64_vector() 
   br_pod_angle        = f%parse_json(config,to_list("Branch#"+str(idx),"pod_angle"  )) .as. real64_vector() 
   br_pod_stem_idx     = f%parse_json(config,to_list("Branch#"+str(idx),"pod_stem_idx"  )) .as. int32_vector() 


   this%Leaf_From = 0.0d0*br_leaf_length
   do i=1,size(this%Leaf_From)
      this%Leaf_From(i) = i
   enddo


!   allocate (this%leaf_curvature(this%num_leaf))
!   allocate (this%leaf_thickness_ave(this%num_leaf))
!   allocate (this%leaf_thickness_sig(this%num_leaf))
!   allocate (this%leaf_angle_ave_x(this%num_leaf))
!   allocate (this%leaf_angle_sig_x(this%num_leaf))
!   allocate (this%leaf_angle_ave_z(this%num_leaf))
!   allocate (this%leaf_angle_sig_z(this%num_leaf))
!   allocate (this%leaf_length_ave(this%num_leaf))
!   allocate (this%leaf_length_sig(this%num_leaf))
!   allocate (this%leaf_width_ave(this%num_leaf))
!   allocate (this%leaf_width_sig(this%num_leaf))

   allocate(stem(size(br_internode_length)))
   do i = 1, size(br_internode_length)
      stem(i)%CROSS_SECTION_SHAPE = PF_STEM_SHAPE_CYLINDER
      call stem(i)%init( &
         x_num=this%stem_division(1), &
         y_num=this%stem_division(2), &
         z_num=this%stem_division(3)  &
         )
      call stem(i)%resize( &
         x=br_internode_radius(i)*2.0d0, &
         y=br_internode_radius(i)*2.0d0, &
         z=br_internode_length(i) &
         )
      call stem(i)%rotate( &
         x=radian(br_internode_angle(i)), &
         y=0.0d0, &
         z=radian(random%random()*360.0d0) &
         )
   end do

   num_stem = size(this%stem)
   
   old_stem = this%stem(:)
   deallocate(this%stem)
   allocate(this%stem(size(old_stem)+size(stem)))
   this%stem(1:size(old_stem))  = old_stem(:)
   this%stem(size(old_stem)+1:) = stem(:)
   
   old_stem2stem = this%stem2stem 
   this%stem2stem = int(zeros(size(this%stem),size(this%stem)))
   this%stem2stem(1:size(old_stem2stem,1),1:size(old_stem2stem,2)) = old_stem2stem(:,:)
   
   call this%stem(num_stem + 1)%connect("=>", this%stem(from_Idx))
   this%stem2stem(num_stem + 1, from_Idx) = 1

   do i = num_stem+1, num_stem+size(br_internode_length)-1
      call this%stem(i + 1)%connect("=>", this%stem(i))
      this%stem2stem(i + 1, i) = 1
   end do


   !set leaf
   num_leaf = size(br_leaf_length) 
   allocate(leaf(size(br_leaf_length)))
   do leaf_idx = 1, num_leaf
      call leaf(leaf_idx)%init(species=PF_Arabidopsis, &
                                    x_num=this%leaf_division(1), &
                                    y_num=this%leaf_division(2), &
                                    z_num=this%leaf_division(3) &
                                    )
      call leaf(leaf_idx)%femdomain%resize( &
         y=br_leaf_thickness(num_leaf), &
         z=br_leaf_length(num_leaf), &
         x=br_leaf_width(num_leaf) &
         )
      !call leaf(leaf_idx)%curve(curvature=this%leaf_curvature(i))
      call leaf(leaf_idx)%femdomain%rotate( &
            x=radian(90.0d0), &
            y=0.0d0, &
            z=radian(360.0d0*random%random()) &
         )
   end do

   

   ! extend leaf object list
   num_leaf = size(this%leaf)
   old_leaf = this%leaf(:)
   deallocate(this%leaf)
   allocate(this%leaf(size(old_leaf)+size(leaf)))
   this%leaf(1:size(old_leaf))  = old_leaf(:)
   this%leaf(size(old_leaf)+1:) = leaf(:)
   
   ! connect leaf to stem
   old_leaf2stem = this%leaf2stem 
   this%leaf2stem = int(zeros(size(this%leaf),size(this%stem)))
   this%leaf2stem(1:size(old_leaf2stem,1),1:size(old_leaf2stem,2)) = old_leaf2stem(:,:)

   do leaf_idx=1,size(leaf)!stem_idx=size(old_stem)+1,size(old_stem)+size(leaf)
      ! leaves are connected to the stem from its bottom
      call this%leaf(size(old_leaf)+leaf_idx)%connect("=>", this%stem(size(old_stem)+leaf_idx))
      this%leaf2stem(size(old_leaf)+leaf_idx, size(old_stem)+leaf_idx) = 1
   enddo
   call this%update()


   !set pod
   num_pod = size(br_pod_length)
   allocate(pod(num_pod))
   do pod_idx = 1, num_pod
      call pod(pod_idx)%init(species=PF_Arabidopsis, &
                                    x_num=this%pod_division(1), &
                                    y_num=this%pod_division(2), &
                                    z_num=this%pod_division(3) &
                                 )
      call pod(pod_idx)%femdomain%resize( &
         z=br_pod_length(pod_idx), &
         y=br_pod_radius(pod_idx)*2.0d0, &
         x=br_pod_radius(pod_idx)*2.0d0 &
         )
      !call pod(pod_idx)%curve(curvature=this%pod_curvature(i))
      call pod(pod_idx)%femdomain%rotate( &
            x=radian(180.0d0-br_pod_angle(pod_idx)), &
            y=0.0d0, &
            z=radian(360.0d0*random%random()) &
         )
   end do


   ! extend pod object list
   num_pod = size(this%pod)
   old_pod = this%pod(:)
   deallocate(this%pod)
   allocate(this%pod(size(old_pod)+size(pod)))
   this%pod(1:size(old_pod))  = old_pod(:)
   this%pod(size(old_pod)+1:) = pod(:)
   

   ! connect pod to stem
   old_pod2stem = this%pod2stem 
   this%pod2stem = int(zeros(size(this%pod),size(this%stem)))
   this%pod2stem(1:size(old_pod2stem,1),1:size(old_pod2stem,2)) = old_pod2stem(:,:)
   
   do pod_idx=1,size(pod)
      call this%pod(size(old_pod)+pod_idx)%connect("=>", this%stem(size(old_stem)+br_pod_stem_idx(pod_idx)))
      this%pod2stem(size(old_pod)+pod_idx, size(old_stem)+br_pod_stem_idx(pod_idx)) = 1
   enddo

   call this%update()

end subroutine

! ########################################
   subroutine mshArabidopsis(this, name, num_threads)
      class(Arabidopsis_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32), optional, intent(in) :: num_threads
      integer(int32) :: i, n

      n = input(default=1, option=num_threads)
      !$OMP parallel num_threads(n) private(i)
      !$OMP do
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%stem(i)%msh(name=name//"_stem"//str(i))
         end if
      end do
      !$OMP end do
      !$OMP end parallel

      !$OMP parallel num_threads(n) private(i)
      !$OMP do
      do i = 1, size(this%root)
         if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%root(i)%msh(name=name//"_root"//str(i))
         end if
      end do
      !$OMP end do
      !$OMP end parallel

      !$OMP parallel num_threads(n) private(i)
      !$OMP do
      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%leaf(i)%msh(name=name//"_leaf"//str(i))
         end if
      end do
      !$OMP end do
      !$OMP end parallel

   end subroutine
! ########################################

! ########################################
   subroutine vtkArabidopsis(this, name, num_threads, single_file, &
                             scalar_field, vector_field, tensor_field, field_name)
      class(Arabidopsis_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32), optional, intent(in) :: num_threads
      logical, optional, intent(in) :: single_file
      character(*), optional, intent(in) :: field_name
      integer(int32) :: i, n
      type(FEMDomain_) :: femdomain
      real(real64), optional, intent(in) :: scalar_field(:)
      real(real64), optional, intent(in) :: vector_field(:, :)
      real(real64), optional, intent(in) :: tensor_field(:, :, :)

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

            if (allocated(this%pod)) then
               do i = 1, size(this%pod)
                  if (.not. this%pod(i)%femdomain%empty()) then
                     femdomain = femdomain + this%pod(i)%femdomain
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
      if (allocated(this%stem)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%stem)
            if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%stem(i)%vtk(name=name//"_stem"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

      if (allocated(this%root)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%root)
            if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%root(i)%vtk(name=name//"_root"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

      if (allocated(this%leaf)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%leaf)
            if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%leaf(i)%vtk(name=name//"_leaf"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

      if (allocated(this%pod)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%pod)
            if (this%pod(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%pod(i)%vtk(name=name//"_pod"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

   end subroutine
! ########################################

! ########################################
   subroutine jsonArabidopsis(this, name)
      class(Arabidopsis_), intent(inout) :: this
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
      call f%write('"num_leaf":'//str(countnum)//',')
      call f%write('"return_Arabidopsis":0')
      call f%write("}")
      call f%close()
   end subroutine
! ########################################

! ########################################
   subroutine stlArabidopsis(this, name, num_threads, single_file)
      class(Arabidopsis_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32), optional, intent(in) :: num_threads
      integer(int32) :: i, n
      logical, optional, intent(in) :: single_file
      logical :: save_as_single_file
      n = input(default=1, option=num_threads)
      save_as_single_file = input(default=.false., option=single_file)

      if (save_as_single_file) then
         call execute_command_line("echo ' ' > "//name//".stl")
      end if

      !$OMP parallel num_threads(n) private(i)
      !$OMP do
      do i = 1, size(this%stem)
         if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%stem(i)%stl(name=name//"_stem"//str(i))
            if (save_as_single_file) then
               call execute_command_line("cat "//name//"_stem"//str(i)//".stl >> "//name//".stl")
               call execute_command_line("rm "//name//"_stem"//str(i)//".stl")
            end if
         end if
      end do
      !$OMP end do
      !$OMP end parallel

      !$OMP parallel num_threads(n) private(i)
      !$OMP do
      do i = 1, size(this%root)
         if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%root(i)%stl(name=name//"_root"//str(i))
            if (save_as_single_file) then
               call execute_command_line("cat "//name//"_root"//str(i)//".stl >> "//name//".stl")
               call execute_command_line("rm "//name//"_root"//str(i)//".stl")
            end if
         end if
      end do
      !$OMP end do
      !$OMP end parallel

      !$OMP parallel num_threads(n) private(i)
      !$OMP do
      do i = 1, size(this%leaf)
         if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
            call this%leaf(i)%stl(name=name//"_leaf"//str(i))
            if (save_as_single_file) then
               call execute_command_line("cat "//name//"_leaf"//str(i)//".stl >> "//name//".stl")
               call execute_command_line("rm "//name//"_leaf"//str(i)//".stl")
            end if
         end if
      end do
      !$OMP end do
      !$OMP end parallel

   end subroutine
! ########################################

! ########################################
   subroutine moveArabidopsis(this, x, y, z)
      class(Arabidopsis_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      integer(int32) :: i
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%stem(i)%move(x=x, y=y, z=z)
            end if
         end do
      end if
      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%root(i)%move(x=x, y=y, z=z)
            end if
         end do
      end if
      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%leaf(i)%move(x=x, y=y, z=z)
            end if
         end do
      end if

   end subroutine
! ########################################

! ########################################
   subroutine rotateArabidopsis(this, x, y, z)
      class(Arabidopsis_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      integer(int32) :: i

      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%stem(i)%rotate(x=x, y=y, z=z)
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (this%leaf(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%leaf(i)%rotate(x=x, y=y, z=z)
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (this%root(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%root(i)%rotate(x=x, y=y, z=z)
            end if
         end do
      end if

      call this%update()

   end subroutine
! ########################################

! ########################################
   recursive subroutine updateArabidopsis(this, stem_id, root_id, leaf_id, overset_margin, debug)
      class(Arabidopsis_), intent(inout) :: this
      integer(int32), optional, intent(in) :: stem_id, root_id, leaf_id
      real(real64), optional, intent(in) :: overset_margin
      integer(int32) :: i, j, this_stem_id, next_stem_id, A_id, B_id, itr_tol, itr, k, kk
      integer(int32) :: this_leaf_id, next_leaf_id, this_pod_id, next_pod_id
      integer(int32) :: this_root_id, next_root_id, InterNodeID, PetioleID, StemID, LeafID, PodID
      real(real64) :: x_A(3), x_B(3), diff(3), error, last_error, mgn, overset_m, error_tol
      logical, optional, intent(in) :: debug

!    if(this%default_Leaf_growth_ratio > 0.0d0)then
!        do i=1,size(this%leaf)
!            if(this%leaf(i)%empty() ) cycle
!            this%leaf(i)%length_growth_ratio = this%default_Leaf_growth_ratio
!            this%leaf(i)%Width_growth_ratio = this%default_Leaf_growth_ratio
!        enddo
!    endif
!
!
!    if(this%default_stem_growth_ratio > 0.0d0)then
!        do i=1,size(this%stem)
!            if(this%stem(i)%empty() ) cycle
!            this%stem(i)%length_growth_ratio = this%default_stem_growth_ratio
!            this%stem(i)%Width_growth_ratio = this%default_stem_growth_ratio
!        enddo
!    endif

      ! if Arabidopsis_internode_info_ is active
      ! update parameters
!    if(allocated(this%InterNodeInfo) )then
!        do i=0,this%MaxBranchNum
!
!            if(allocated(this%InterNodeInfo(i)%FinalInterNodeLength ) )then
!                do j=1,this%maxInterNodeID(StemID=i)
!                    InterNodeID = this%searchStem(StemID=i,InterNodeID=j)
!                    if(size(this%InterNodeInfo(i)%FinalInterNodeLength) < j ) then
!                        print *, "ERROR :: updateArabidopsis >> "
!                        print *, "size(this%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
!                        stop
!                    endif
!                    if(InterNodeID<1)then
!                        cycle
!                    endif
!                    this%stem(InterNodeID)%final_length = this%InterNodeInfo(i)%FinalInterNodeLength(j)
!                enddo
!            endif
!
!            if(allocated(this%InterNodeInfo(i)%FinalPetioleLength) )then
!                do j=1,this%maxInterNodeID(StemID=i)
!                    do k=1,this%maxPetioleID(StemID=i,InterNodeID=j)
!                        if(size(this%InterNodeInfo(i)%FinalPetioleLength) < j ) then
!                            print *, "ERROR :: updateArabidopsis >> "
!                            print *, "size(this%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
!                            stop
!                        endif
!
!                        PetioleID = this%searchPetiole(StemID=i,InterNodeID=j,PetioleID=k)
!
!                        this%stem(PetioleID)%final_length = this%InterNodeInfo(i)%FinalPetioleLength(j)
!                    enddo
!                enddo
!            endif
!
!                if(allocated(this%InterNodeInfo(i)%FinalLeafLength) )then
!                    do j=1,this%maxInterNodeID(StemID=i)
!                        do k=1,this%maxPetioleID(StemID=i,InterNodeID=j)
!                            do kk = 1, this%maxleafID(StemID=i,InterNodeID=j,PetioleID=k)
!                                if(size(this%InterNodeInfo(i)%FinalLeafLength) < j ) then
!                                    print *, "ERROR :: updateArabidopsis >> "
!                                    print *, "size(this%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
!                                    stop
!                                endif
!                                LeafID = this%searchleaf(StemID=i,InterNodeID=j,PetioleID=k,LeafID=kk)
!                                this%leaf(LeafID)%final_length = this%InterNodeInfo(i)%FinalLeafLength(j)
!                            enddo
!                        enddo
!                    enddo
!                endif
!
!
!                if(allocated(this%InterNodeInfo(i)%FinalLeafWidth) )then
!                    do j=1,this%maxInterNodeID(StemID=i)
!                        do k=1,this%maxPetioleID(StemID=i,InterNodeID=j)
!                            do kk = 1, this%maxleafID(StemID=i,InterNodeID=j,PetioleID=k)
!                                if(size(this%InterNodeInfo(i)%FinalLeafWidth) < j ) then
!                                    print *, "ERROR :: updateArabidopsis >> "
!                                    print *, "size(this%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
!                                    stop
!                                endif
!                                LeafID = this%searchleaf(StemID=i,InterNodeID=j,PetioleID=k,LeafID=kk)
!                                this%leaf(LeafID)%final_Width = this%InterNodeInfo(i)%FinalLeafWidth(j)
!                            enddo
!                        enddo
!                    enddo
!                endif
!
!        enddo
!    endif

      ! update connectivity
      if (.not. allocated(this%stem2stem)) then
         print *, "updateArabidopsis >> ERROR :: .not. allocated(this%stem2stem )"
         return
      end if

      error_tol = dble(1.0e-14)

      ! margin between subdomains
      overset_m = input(default=0.03d0, option=overset_margin)

      itr_tol = 100
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
            if (present(debug)) then
               if (debug) then
                  print *, "Arabidopsis % update s2s >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "Arabidopsis % update s2s >> ERROR :: not converged"
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
                  print *, "Arabidopsis % update r2s >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "Arabidopsis % update r2s  >> ERROR :: not converged"
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
                  print *, "Arabidopsis % update r2r >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "Arabidopsis % update r2r >> ERROR :: not converged"
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
               print *, "Arabidopsis % update l2s >> error :: ", error
            end if
         end if
         if (itr > itr_tol) then
            print *, "Arabidopsis % update l2s  >> ERROR :: not converged"
            stop
         end if

         if (abs(error) + abs(last_error) < error_tol) exit
         last_error = error
      end do

      ! Pod to stem
      last_error = 1.0d0
      do
         itr = itr + 1
         error = 0.0d0
         do i = 1, size(this%pod2stem, 1)
            do j = 1, size(this%pod2stem, 2)
               this_stem_id = j
               next_pod_id = i
               if (this%pod2stem(i, j) == 1) then
                  ! this_stem_id ===>>> next_pod_id, connected!
                  !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
                  !x_A(:) = this%pod(next_pod_id)%getCoordinate("A")

                  ! Overset分食い込ませる
                  x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("B") &
                           + overset_m*this%stem(this_stem_id)%getCoordinate("A")
                  ! Overset分食い込ませる
                  x_A(:) = this%pod(next_pod_id)%getCoordinate("A")

                  diff(:) = x_B(:) - x_A(:)
                  error = error + dot_product(diff, diff)
                  call this%pod(next_pod_id)%move(x=diff(1), y=diff(2), z=diff(3))
               end if
            end do
         end do
         if (present(debug)) then
            if (debug) then
               print *, "Arabidopsis % update p2s >> error :: ", error
            end if
         end if
         if (itr > itr_tol) then
            print *, "Arabidopsis % update p2s  >> ERROR :: not converged"
            stop
         end if

         if (abs(error) + abs(last_error) < error_tol) exit
         last_error = error
      end do

   end subroutine
! ########################################

! ########################################
!recursive subroutine updateArabidopsis(this,debug)
!    class(Arabidopsis_),intent(inout) :: this
!    integer(int32) :: i,j,this_stem_id,next_stem_id,A_id,B_id,itr_tol,itr
!    integer(int32) :: this_leaf_id,next_leaf_id
!    integer(int32) :: this_root_id,next_root_id
!    real(real64) :: x_A(3),x_B(3),diff(3),error,last_error
!    logical,optional,intent(in) :: debug
!
!
!    ! update connectivity
!    if(.not. allocated(this%stem2stem ))then
!        print *, "updateArabidopsis >> ERROR :: .not. allocated(this%stem2stem )"
!        return
!    endif
!
!    itr_tol = 100
!    itr=0
!
!    ! stem to stem
!    last_error = 1.0d0
!    do
!        itr=itr+1
!        error = 0.0d0
!        do i=1, size(this%stem2stem,1)
!            do j=1, size(this%stem2stem,2)
!                this_stem_id = j
!                next_stem_id = i
!                if(this%stem2stem(i,j)/=0 .and. i /= j)then
!                    ! this_stem_id ===>>> next_stem_id, connected!
!                    x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
!                    x_A(:) = this%stem(next_stem_id)%getCoordinate("A")
!                    diff(:) = x_B(:) - x_A(:)
!                    error = error + dot_product(diff,diff)
!                    call this%stem(next_stem_id)%move(x=diff(1),y=diff(2),z=diff(3) )
!                endif
!            enddo
!        enddo
!        if(present(debug) )then
!            if(debug)then
!                print *, "Arabidopsis % update >> error :: ",error
!            endif
!        endif
!        if(itr > itr_tol) then
!            print *, "Arabidopsis % update >> ERROR :: not converged"
!            stop
!        endif
!
!        if( abs(error) + abs(last_error) == 0.0d0) exit
!        last_error = error
!    enddo
!
!    ! leaf to stem
!    last_error = 1.0d0
!    do
!        itr=itr+1
!        error = 0.0d0
!        do i=1, size(this%leaf2stem,1)
!            do j=1, size(this%leaf2stem,2)
!                this_stem_id = j
!                next_leaf_id = i
!                if(this%leaf2stem(i,j)==1)then
!                    ! this_stem_id ===>>> next_leaf_id, connected!
!                    x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
!                    x_A(:) = this%leaf(next_leaf_id)%getCoordinate("A")
!                    diff(:) = x_B(:) - x_A(:)
!                    error = error + dot_product(diff,diff)
!                    call this%leaf(next_leaf_id)%move(x=diff(1),y=diff(2),z=diff(3) )
!                endif
!            enddo
!        enddo
!        if(present(debug) )then
!            if(debug)then
!                print *, "Arabidopsis % update >> error :: ",error
!            endif
!        endif
!
!        if(itr > itr_tol) then
!            print *, "Arabidopsis % update >> ERROR :: not converged"
!            stop
!        endif
!
!        if( abs(error) + abs(last_error) == 0.0d0) exit
!        last_error = error
!    enddo
!
!
!    return
!
!
!    ! root to root
!    last_error = 1.0d0
!    do
!        itr=itr+1
!        error = 0.0d0
!        do i=1, size(this%root2root,1)
!            do j=1, size(this%root2root,2)
!                this_root_id = j
!                next_root_id = i
!                if(this%root2root(i,j)/=0 .and. i /= j)then
!                    ! this_root_id ===>>> next_root_id, connected!
!                    x_B(:) = this%root(this_root_id)%getCoordinate("B")
!                    x_A(:) = this%root(next_root_id)%getCoordinate("A")
!                    diff(:) = x_B(:) - x_A(:)
!                    error = error + dot_product(diff,diff)
!                    call this%root(next_root_id)%move(x=diff(1),y=diff(2),z=diff(3) )
!                endif
!            enddo
!        enddo
!        if(present(debug) )then
!            if(debug)then
!                print *, "Arabidopsis % update >> error :: ",error
!            endif
!        endif
!        if(itr > itr_tol) then
!            print *, "Arabidopsis % update >> ERROR :: not converged"
!            stop
!        endif
!
!        if( abs(error) + abs(last_error) == 0.0d0) exit
!        last_error = error
!    enddo
!
!
!
!end subroutine
! ########################################

   subroutine removeArabidopsis(this, root)
      class(Arabidopsis_), intent(inout) :: this
      logical, optional, intent(in) :: root

      if (present(root)) then
         if (root) then

            if (allocated(this%Root)) deallocate (this%Root)
            !if (allocated(this%rootYoungModulus) ) deallocate(this%rootYoungModulus)
            !if (allocated(this%rootPoissonRatio) ) deallocate(this%rootPoissonRatio)
            !if (allocated(this%rootDensity) ) deallocate(this%rootDensity)

            if (allocated(this%root2stem)) deallocate (this%root2stem)
            if (allocated(this%root2root)) deallocate (this%root2root)
            !if (allocated(this%root_list) ) deallocate(this%root_list)

            !if (allocated(this%root_angle) ) deallocate(this%root_angle)
            !this%rootconfig=" "
            !this%Num_Of_Root = 0

         end if
         return
      end if

!    this%leaf_angle_ave(:)=0.0d0
!    this%leaf_angle_sig(:)=0.0d0
!    this%leaf_length_ave(:)=0.0d0
!    this%leaf_length_sig(:)=0.0d0
!    this%leaf_width_ave(:)=0.0d0
!    this%leaf_width_sig(:)=0.0d0
!    this%leaf_thickness_ave(:)=0.0d0
!    this%leaf_thickness_sig(:)=0.0d0
!
!    this%Stage="" ! VE, CV, V1,V2, ..., R1, R2, ..., R8
!    this%name=""
!    this%stage_id=0
!    this%dt=0.0d0
!    call this%Seed%remove()
!    if (allocated(this%NodeSystem) ) deallocate(this%NodeSystem)
!    if (allocated(this%RootSystem) ) deallocate(this%RootSystem)

      if (allocated(this%Stem)) deallocate (this%Stem)
      if (allocated(this%Leaf)) deallocate (this%Leaf)
      if (allocated(this%Root)) deallocate (this%Root)

      ! material info
!    if (allocated(this%stemYoungModulus) ) deallocate(this%stemYoungModulus)
!    if (allocated(this%leafYoungModulus) ) deallocate(this%leafYoungModulus)
!    if (allocated(this%rootYoungModulus) ) deallocate(this%rootYoungModulus)
!
!    if (allocated(this%stemPoissonRatio) ) deallocate(this%stemPoissonRatio)
!    if (allocated(this%leafPoissonRatio) ) deallocate(this%leafPoissonRatio)
!    if (allocated(this%rootPoissonRatio) ) deallocate(this%rootPoissonRatio)
!
!    if (allocated(this%stemDensity) ) deallocate(this%stemDensity)
!    if (allocated(this%leafDensity) ) deallocate(this%leafDensity)
!    if (allocated(this%rootDensity) ) deallocate(this%rootDensity)

!    if(allocated(this%NodeID_MainStem)) deallocate(this%NodeID_MainStem)
!    if(allocated(this%NodeID_Branch)) deallocate(this%NodeID_Branch)
      ! 節-節点データ構造
      call this%struct%remove(all=.true.)
      if (allocated(this%leaf2stem)) deallocate (this%leaf2stem)
      if (allocated(this%stem2stem)) deallocate (this%stem2stem)
      if (allocated(this%root2stem)) deallocate (this%root2stem)
      if (allocated(this%root2root)) deallocate (this%root2root)

      ! シミュレータ
      call this%contact%remove()
!    this%time=0.0d0
!    this%seed_length=0.0d0
!    this%seed_width=0.0d0
!    this%seed_height=0.0d0
!    if (allocated(this%stem_angle) ) deallocate(this%stem_angle)
!    if (allocated(this%root_angle) ) deallocate(this%root_angle)
!    if (allocated(this%leaf_angle) ) deallocate(this%leaf_angle)
!
!    this%stemconfig=" "
!    this%rootconfig=" "
!    this%leafconfig=" "

      this%TYPE_STEM = 1
      this%TYPE_LEAF = 2
      this%TYPE_ROOT = 3
      ! 節-節点データ構造
      call this%struct%remove()
      if (allocated(this%leaf2stem)) deallocate (this%leaf2stem)! (:,:)
      if (allocated(this%stem2stem)) deallocate (this%stem2stem)! (:,:)
      if (allocated(this%root2stem)) deallocate (this%root2stem)! (:,:)
      if (allocated(this%root2root)) deallocate (this%root2root)! (:,:)

      this%mainstem_length = 0.0d0
      this%mainstem_width = 0.0d0
      this%mainstem_node = 0

      this%mainroot_length = 0.0d0
      this%mainroot_width = 0.0d0
      this%mainroot_node = 0

      this%num_branch_root = 0
      this%num_branch_root_node = 0

      this%ms_angle_ave = 0.0d0
      this%ms_angle_sig = 0.0d0

      if (allocated(this%Leaf_From)) deallocate (this%Leaf_From)! (:)

      !real(real64),allocatable :: leaf_Length(:)
      !real(real64),allocatable :: leaf_Width(:)

      if (allocated(this%leaf_curvature)) deallocate (this%leaf_curvature)! (:)

      if (allocated(this%leaf_thickness_ave)) deallocate (this%leaf_thickness_ave)! (:)
      if (allocated(this%leaf_thickness_sig)) deallocate (this%leaf_thickness_sig)! (:)

      if (allocated(this%leaf_angle_ave_x)) deallocate (this%leaf_angle_ave_x)! (:)
      if (allocated(this%leaf_angle_sig_x)) deallocate (this%leaf_angle_sig_x)! (:)
      if (allocated(this%leaf_angle_ave_z)) deallocate (this%leaf_angle_ave_z)! (:)
      if (allocated(this%leaf_angle_sig_z)) deallocate (this%leaf_angle_sig_z)! (:)

      if (allocated(this%leaf_length_ave)) deallocate (this%leaf_length_ave)! (:)
      if (allocated(this%leaf_length_sig)) deallocate (this%leaf_length_sig)! (:)
      if (allocated(this%leaf_width_ave)) deallocate (this%leaf_width_ave)! (:)
      if (allocated(this%leaf_width_sig)) deallocate (this%leaf_width_sig)! (:)

      this%num_leaf = 0
      this%num_stem = 0
      this%num_root = 0

      !! 器官オブジェクト配列
      !if (allocated(this%leaf_list)) deallocate (this%leaf_list)! (:)
      !if (allocated(this%stem_list)) deallocate (this%stem_list)! (:)
      !if (allocated(this%root_list)) deallocate (this%root_list)! (:)

      this%LeafSurfaceData = ""
      if (allocated(this%Leaf)) deallocate (this%Leaf)! (:)
      if (allocated(this%Stem)) deallocate (this%Stem)! (:)
      if (allocated(this%Root)) deallocate (this%Root)! (:)

      if (allocated(this%NodeID_MainStem)) deallocate (this%NodeID_MainStem)! (:)
      if (allocated(this%NodeID_Branch)) deallocate (this%NodeID_Branch)! (:)

      this%inLoop = .false.
      this%hours = 0.0d0

      ! growth simulation
      this%FullyExpanded_stem_threshold = 0.10d0
      this%MaxBranchNum = 20
      if (allocated(this%InterNodeInfo)) deallocate (this%InterNodeInfo)! (:)
      this%default_Leaf_growth_ratio = 1.0d0/3.0d0
      this%default_Stem_growth_ratio = 1.0d0/3.0d0
      if (allocated(this%MainStem_num_branch)) deallocate (this%MainStem_num_branch)! (:)
      this%apical_dominance_distance = 1.0d0

      ! シミュレータ
      call this%contact%remove()
      this%Gravity_acceralation = 9.810d0
      this%PenaltyParameter = 100000.0d0
      this%GaussPointProjection = .false.

      ! setting
      this%stem_division(1:3) = [10, 10, 10]
      this%leaf_division(1:3) = [10, 10, 10]

   end subroutine
! ################################################################
   subroutine checkMemoryRequirementArabidopsis(this)
      class(Arabidopsis_), intent(in) :: this
      real(real64) :: re_val
      integer(int64) :: val

      print *, "===================================="
      print *, "checking Memory (RAM) Requirement..."
      print *, "------------------------------------"
      print *, "| thisect type                     | Arabidopsis"
      print *, "| Number of points                | "+str(this%nn())
      print *, "| Degree of freedom | Deformation | "+str(this%nn()*3)
      print *, "|                   | ModeAnalysis| ", dble(this%nn())*3*dble(this%nn())*3
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

! ##################################################################
   function nsArabidopsis(this) result(ret)
      class(Arabidopsis_), intent(in) :: this
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

! ##################################################################
   pure function nnArabidopsis(this) result(ret)
      class(Arabidopsis_), intent(in) :: this
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
   function neArabidopsis(this) result(ret)
      class(Arabidopsis_), intent(in) :: this
      integer(int32) :: ret, i

      ! get number of element
      ret = 0
      if (allocated(this%stem)) then
         do i = 1, size(this%stem)
            if (.not. this%stem(i)%femdomain%mesh%empty()) then
               ret = ret + this%stem(i)%femdomain%ne()
            end if
         end do
      end if

      if (allocated(this%leaf)) then
         do i = 1, size(this%leaf)
            if (.not. this%leaf(i)%femdomain%mesh%empty()) then
               ret = ret + this%leaf(i)%femdomain%ne()
            end if
         end do
      end if

      if (allocated(this%root)) then
         do i = 1, size(this%root)
            if (.not. this%root(i)%femdomain%mesh%empty()) then
               ret = ret + this%root(i)%femdomain%ne()
            end if
         end do
      end if

   end function
! ##################################################################

! ################################################################
   recursive subroutine setYoungModulusArabidopsis(this, YoungModulus, stem, root, leaf, ElementList)
      class(Arabidopsis_), intent(inout) :: this
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
         call this%setYoungModulus(YoungModulus=YoungModulus, stem=.true., root=.true., leaf=.true. &
                                   , ElementList=ElementList)
      end if

   end subroutine
! ################################################################

! ################################################################
   recursive subroutine setPoissonRatioArabidopsis(this, PoissonRatio, stem, root, leaf)
      class(Arabidopsis_), intent(inout) :: this
      logical, optional, intent(in) :: stem, root, leaf
      real(real64), intent(in) :: PoissonRatio
      integer(int32) :: i, n

      n = 0
      if (present(stem)) then
         if (stem) then
            n = n + 1
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (this%stem(i)%femdomain%empty()) then
                     cycle
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
                  else
                     this%root(i)%PoissonRatio = PoissonRatio*eyes(this%root(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (n == 0) then
         call this%setPoissonRatio(PoissonRatio=PoissonRatio, stem=.true., root=.true., leaf=.true. &
                                   )
      end if

   end subroutine
! ################################################################

! ################################################################
   recursive subroutine setDensityArabidopsis(this, Density, stem, root, leaf)
      class(Arabidopsis_), intent(inout) :: this
      logical, optional, intent(in) :: stem, root, leaf
      real(real64), intent(in) :: Density
      integer(int32) :: i, n

      n = 0
      if (present(stem)) then
         if (stem) then
            n = n + 1
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (this%stem(i)%femdomain%empty()) then
                     cycle
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
                  else
                     this%root(i)%Density = Density*eyes(this%root(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (n == 0) then
         call this%setDensity(Density=Density, stem=.true., root=.true., leaf=.true. &
                              )
      end if

   end subroutine
! ################################################################

! ################################################################
   function getEigenModeArabidopsis(this, ground_level, penalty, debug, Frequency, EbOM_Algorithm, num_mode) result(EigenVectors)
      class(Arabidopsis_), target, intent(inout) :: this
      real(real64), intent(in) :: ground_level
      real(real64), optional, intent(in) :: penalty
      logical, optional, intent(in) :: debug
      real(real64), allocatable, intent(inout) :: Frequency(:)
      character(*), optional, intent(in) :: EbOM_Algorithm

      integer(int32), optional, intent(in) :: num_mode
      integer(int32) :: num_freq
      !integer(int32),optional,intent(in) :: num_mode

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

      num_freq = input(default=10, option=num_mode)

      EbOM_Algorithm_id = FEMDomain_Overset_GPP
      if (present(EbOM_Algorithm)) then
         if (EbOM_Algorithm == "P2P") then
            EbOM_Algorithm_id = FEMDomain_Overset_P2P
         elseif (EbOM_Algorithm == "GPP") then
            EbOM_Algorithm_id = FEMDomain_Overset_P2P
         end if
      end if

      ! linear elasticity with infinitesimal strain theory
      n = this%numStem() + this%numLeaf() + this%numRoot()

      allocate (FEMDomainPointers(n))
      ! ORDER
      ! [STEM] => [LEAF] => [ROOT]
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
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%stem(myStemID)%femdomain, &
                        DomainID=myStemID, &
                        MyDomainID=yourStemID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
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
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%leaf(myLeafID)%femdomain, &
                        DomainID=this%numStem() + myLeafID, &
                        MyDomainID=yourStemID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
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
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%root(myRootID)%femdomain, &
                        DomainID=this%numStem() + this%numLeaf() + myRootID, &
                        MyDomainID=yourStemID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
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
                        algorithm=EbOM_Algorithm_id) ! or "P2P"

                     call this%root(yourrootID)%femdomain%overset( &
                        FEMDomain=this%root(myRootID)%femdomain, &
                        DomainID=this%numroot() + this%numLeaf() + myRootID, &
                        MyDomainID=this%numroot() + this%numLeaf() + yourrootID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
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
      if (.not. this%checkDensity()) then
         print *, "[ERROR] Density(:) is not ready."
         stop
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] setCRS >> done."
         end if
      end if

      !$OMP parallel
      !$OMP do
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix( &
                                  ElementID=ElementID, &
                                  E=this%getYoungModulus(DomainID=DomainID, ElementID=ElementID), &
                                  v=this%getPoissonRatio(DomainID=DomainID, ElementID=ElementID)))
         end do
      end do
      !$OMP end do
      !$OMP end parallel

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
      !$OMP parallel
      !$OMP do
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%massMatrix( &
                                  ElementID=ElementID, &
                                  Density=this%getDensity(DomainID=DomainID, ElementID=ElementID), &
                                  DOF=3))
         end do
      end do
      !$OMP end do
      !$OMP end parallel
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
      call solver%remove()

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

! ########################################
   function numleafArabidopsis(this) result(ret)
      class(Arabidopsis_), intent(in) :: this
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
   function numStemArabidopsis(this) result(ret)
      class(Arabidopsis_), intent(in) :: this
      integer(int32) :: ret, i

      ret = 0
      if (.not. allocated(this%Stem)) then
         return
      end if

      do i = 1, size(this%Stem)
         if (this%Stem(i)%femdomain%Mesh%empty() .eqv. .false.) then
            ret = ret + 1
         end if
      end do

   end function
! ########################################

! ########################################
   function numRootArabidopsis(this) result(ret)
      class(Arabidopsis_), intent(in) :: this
      integer(int32) :: ret, i

      ret = 0
      if (.not. allocated(this%Root)) then
         return
      end if

      do i = 1, size(this%Root)
         if (this%Root(i)%femdomain%Mesh%empty() .eqv. .false.) then
            ret = ret + 1
         end if
      end do

   end function
! ########################################

! ################################################################
   function getFEMDomainPointersArabidopsis(this) result(FEMDomainPointers)
      class(Arabidopsis_), target, intent(in) :: this
      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)
      integer(int32) :: num_FEMDomain, i, n

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

   end function
! ################################################################

! ################################################################
   function checkYoungModulusArabidopsis(this) result(all_young_modulus_is_set)
      class(Arabidopsis_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusArabidopsis >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusArabidopsis >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusArabidopsis >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

   end function
! ################################################################

! ################################################################
   function checkPoissonRatioArabidopsis(this) result(all_young_modulus_is_set)
      class(Arabidopsis_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioArabidopsis >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioArabidopsis >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioArabidopsis >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

   end function
! ################################################################

! ################################################################
   function checkDensityArabidopsis(this) result(all_young_modulus_is_set)
      class(Arabidopsis_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensityArabidopsis >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%Density)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensityArabidopsis >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%Density)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensityArabidopsis >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%Density)"
            return
         end if
      end do

   end function
! ################################################################

   function getYoungModulusFieldArabidopsis(this) result(YoungModulus)
      class(Arabidopsis_), intent(inout) :: this
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
   function getPoissonRatioFieldArabidopsis(this) result(PoissonRatio)
      class(Arabidopsis_), intent(inout) :: this
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
   function getDensityFieldArabidopsis(this) result(Density)
      class(Arabidopsis_), intent(inout) :: this
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

! ################################################################

   function getYoungModulusArabidopsis(this, DomainID, ElementID) result(YoungModulus)
      class(Arabidopsis_), intent(in) :: this
      integer(int32), optional, intent(in) :: DomainID, ElementID
      real(real64) :: YoungModulus
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot()) then
         print *, "ERROR :: getYoungModulusArabidopsis >>  DomainID exceeds max_domain_size"
         return
      end if

      ! default >> search @ all domains
      ! order: stem -> leaf -> root
      if (1 <= DomainID &
          .and. DomainID <= this%numStem()) then
         n = DomainID - 0
         YoungModulus = this%stem(n)%YoungModulus(ElementID)
         return
      elseif (this%numStem() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf()) then
         n = DomainID - this%numStem()
         YoungModulus = this%leaf(n)%YoungModulus(ElementID)
         return
      elseif (this%numStem() + this%numLeaf() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot()) then
         n = DomainID - (this%numStem() + this%numLeaf())
         YoungModulus = this%root(n)%YoungModulus(ElementID)
         return
      else
         print *, "[ERROR] >> getYoungModulusArabidopsis >> Invalid DomainID", DomainID
         return
      end if

   end function
! ################################################################

! ################################################################
   function getPoissonRatioArabidopsis(this, DomainID, ElementID) result(PoissonRatio)
      class(Arabidopsis_), intent(in) :: this
      integer(int32), intent(in) :: DomainID, ElementID
      real(real64) :: PoissonRatio
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot()) then
         print *, "ERROR :: getPoissonRatioArabidopsis >>  DomainID exceeds max_domain_size"
         return
      end if

      ! default >> search @ all domains
      ! order: stem -> leaf -> root
      if (1 <= DomainID &
          .and. DomainID <= this%numStem()) then
         n = DomainID - 0
         PoissonRatio = this%stem(n)%PoissonRatio(ElementID)
         return
      elseif (this%numStem() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf()) then
         n = DomainID - this%numStem()
         PoissonRatio = this%leaf(n)%PoissonRatio(ElementID)
         return
      elseif (this%numStem() + this%numLeaf() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot()) then
         n = DomainID - (this%numStem() + this%numLeaf())
         PoissonRatio = this%root(n)%PoissonRatio(ElementID)
         return
      else
         print *, "[ERROR] >> getPoissonRatioArabidopsis >> Invalid DomainID", DomainID
         return
      end if

   end function
! ################################################################

! ################################################################
   function getDensityArabidopsis(this, DomainID, ElementID) result(Density)
      class(Arabidopsis_), intent(in) :: this
      integer(int32), intent(in) :: DomainID, ElementID
      real(real64) :: Density
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot()) then
         print *, "ERROR :: getDensityArabidopsis >>  DomainID exceeds max_domain_size"
         return
      end if

      ! default >> search @ all domains
      ! order: stem -> leaf -> root
      if (1 <= DomainID &
          .and. DomainID <= this%numStem()) then
         n = DomainID - 0
         Density = this%stem(n)%Density(ElementID)
         return
      elseif (this%numStem() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf()) then
         n = DomainID - this%numStem()
         Density = this%leaf(n)%Density(ElementID)
         return
      elseif (this%numStem() + this%numLeaf() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot()) then
         n = DomainID - (this%numStem() + this%numLeaf())
         Density = this%root(n)%Density(ElementID)
         return
      else
         print *, "[ERROR] >> getDensityArabidopsis >> Invalid DomainID", DomainID
         return
      end if

   end function
! ################################################################

! #############################################################

   subroutine deformArabidopsis(this, displacement)
      class(Arabidopsis_), target, intent(inout) :: this

      ! deform Arabidopsis by displacement
      real(real64), optional, intent(in) :: displacement(:)

      type(FEMDomainp_), allocatable :: domainsp(:)
      integer(int32), allocatable :: contactList(:, :)
      integer(int32) :: i, j, numDomain, stemDomain, leafDomain, rootDomain, from, to, nd, nn
      real(real64) :: penalty, GLevel

      if (present(displacement)) then
         if (size(displacement) /= this%nn()*3) then
            print *, "ERROR :: deformArabidopsis >> &
&            size(displacement) should be (this%numStem() &
&            + this%numLeaf() + this%numRoot())*3"
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

   end subroutine
! #####################################################################

   function getElementListArabidopsis(this, x_min, x_max, y_min, y_max, z_min, z_max, debug) result(ElementList)
      class(Arabidopsis_), intent(inout) :: this
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
! ###########################################################################

! ################################################################
   function getDisplacementArabidopsis(this, ground_level, penalty, debug, EbOM_Algorithm) result(displacement)
      class(Arabidopsis_), target, intent(inout) :: this
      real(real64), intent(in) :: ground_level
      real(real64), optional, intent(in) :: penalty
      logical, optional, intent(in) :: debug
      real(real64), allocatable :: Frequency(:)
      character(*), optional, intent(in) :: EbOM_Algorithm
      !integer(int32),optional,intent(in) :: num_mode

      real(real64), allocatable :: displacement(:)

      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)
      type(FEMSolver_) :: solver
      type(Math_) :: math

      real(real64), allocatable :: EigenVectors(:, :), buf(:, :), buf_vec(:)

      integer(int32) :: stem_id, leaf_id, root_id, DomainID, ElementID, i, n
      integer(int32) :: myStemID, yourStemID, myLeafID, myRootID, yourRootID
      integer(int32), allocatable :: FixBoundary(:)
      integer(int32) :: nn_domains, EbOM_Algorithm_id
      real(real64) :: vec_norm

      EbOM_Algorithm_id = FEMDomain_Overset_GPP
      if (present(EbOM_Algorithm)) then
         if (EbOM_Algorithm == "P2P") then
            EbOM_Algorithm_id = FEMDomain_Overset_P2P
         elseif (EbOM_Algorithm == "GPP") then
            EbOM_Algorithm_id = FEMDomain_Overset_P2P
         end if
      end if

      ! linear elasticity with infinitesimal strain theory
      n = this%numStem() + this%numLeaf() + this%numRoot()

      allocate (FEMDomainPointers(n))
      ! ORDER
      ! [STEM] => [LEAF] => [ROOT]
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
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%stem(myStemID)%femdomain, &
                        DomainID=myStemID, &
                        MyDomainID=yourStemID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
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
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%leaf(myLeafID)%femdomain, &
                        DomainID=this%numStem() + myLeafID, &
                        MyDomainID=yourStemID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
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
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%root(myRootID)%femdomain, &
                        DomainID=this%numStem() + this%numLeaf() + myRootID, &
                        MyDomainID=yourStemID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
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
                        algorithm=EbOM_Algorithm_id) ! or "P2P"

                     call this%root(yourrootID)%femdomain%overset( &
                        FEMDomain=this%root(myRootID)%femdomain, &
                        DomainID=this%numroot() + this%numLeaf() + myRootID, &
                        MyDomainID=this%numroot() + this%numLeaf() + yourrootID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
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
      if (.not. this%checkDensity()) then
         print *, "[ERROR] Density(:) is not ready."
         stop
      end if

      if (present(debug)) then
         if (debug) then
            print *, "[ok] setCRS >> done."
         end if
      end if

      !$OMP parallel
      !$OMP do
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix( &
                                  ElementID=ElementID, &
                                  E=this%getYoungModulus(DomainID=DomainID, ElementID=ElementID), &
                                  v=this%getPoissonRatio(DomainID=DomainID, ElementID=ElementID)))
            call solver%setVector(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Vector=FEMDomainPointers(DomainID)%femdomainp%massVector( &
                                  ElementID=ElementID, &
                                  Density=this%getDensity(DomainID=DomainID, ElementID=ElementID), &
                                  Accel=[0.0d0, 0.0d0, -this%Gravity_acceralation], &
                                  DOF=3))
         end do
      end do
      !$OMP end do
      !$OMP end parallel

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

      ! mass matrix
      !$OMP parallel
      !$OMP do
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=3, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%massMatrix( &
                                  ElementID=ElementID, &
                                  Density=this%getDensity(DomainID=DomainID, ElementID=ElementID), &
                                  DOF=3))
         end do
      end do
      !$OMP end do
      !$OMP end parallel

      ! fix-boundary conditions
      nn_domains = 0
      do i = 1, size(FEMDomainPointers)
         if (FEMDomainPointers(i)%FEMDomainp%z_min() <= ground_level) then
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 2 !+ nn_domains*3
            call solver%fix(DomainID=i, IDs=FixBoundary, FixValue=0.0d0)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 1 !+ nn_domains*3
            call solver%fix(DomainID=i, IDs=FixBoundary, FixValue=0.0d0)
            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max=ground_level)*3 - 0 !+ nn_domains*3
            call solver%fix(DomainID=i, IDs=FixBoundary, FixValue=0.0d0)
         end if
         !nn_domains = nn_domains + FEMDomainPointers(i)%FEMDomainp%nn()
      end do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] FixBoundary >> done."
         end if
      end if

      if (present(debug)) then
         solver%debug = debug
      end if

      displacement = solver%solve(algorithm="BiCGSTAB")
      call solver%remove()

      if (present(debug)) then
         if (debug) then
            print *, "[ok] Solve >> done."
         end if
      end if

   end function

! ################################################################
   function getStressFieldArabidopsis(this, displacement, i, j, option) result(StressField)
      class(Arabidopsis_), intent(inout) :: this
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

   subroutine export_eigArabidopsis(this, name, Frequency, ModeVectors, stress_type)
      class(Arabidopsis_), intent(inout) :: this
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



end module ArabidopsisClass
