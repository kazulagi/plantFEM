module MaizeClass
   use LeafClass
   use StemClass
   use RootClass
   use EarClass
   use PanicleClass
   implicit none

   type :: Maize_internode_info_
      real(real64), allocatable :: FinalInterNodeLength(:)
      real(real64), allocatable :: FinalPetioleLength(:)
      real(real64), allocatable :: FinalLeafLength(:)
      real(real64), allocatable :: FinalLeafWidth(:)
   end type

   type :: Maize_NodeID_Branch_
      integer(int32), allocatable :: ID(:)
   contains
      !procedure, public :: sync => syncMaize_NodeID_Branch
   end type

   type :: Maize_
      integer(int32) :: TYPE_STEM = 1
      integer(int32) :: TYPE_LEAF = 2
      integer(int32) :: TYPE_ROOT = 3
      integer(int32) :: TYPE_EAR = 4
      integer(int32) :: TYPE_PANICLE = 5
      ! 節-節点データ構造
      type(Mesh_) :: struct
      integer(int32), allocatable :: leaf2stem(:, :)
      integer(int32), allocatable :: stem2stem(:, :)
      integer(int32), allocatable :: ear2stem(:, :)
      integer(int32), allocatable :: panicle2stem(:, :)
      integer(int32), allocatable :: root2stem(:, :)
      integer(int32), allocatable :: root2root(:, :)

      real(real64)   :: mainstem_length
      real(real64)   :: mainstem_width
      real(real64)   :: mainstem_bottom_width
      real(real64)   :: mainstem_top_width
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
      integer(int32) :: num_ear = 1
      integer(int32) :: num_panicle = 1
      integer(int32) :: num_root

      ! 器官オブジェクト配列
      type(FEMDomain_), allocatable :: leaf_list(:)
      type(FEMDomain_), allocatable :: stem_list(:)
      type(FEMDomain_), allocatable :: root_list(:)

      character(:), allocatable :: LeafSurfaceData
      type(Leaf_), allocatable :: Leaf(:)
      type(Stem_), allocatable :: Stem(:)
      type(Ear_), allocatable :: Ear(:)
      type(Panicle_), allocatable :: Panicle(:)
      type(Root_), allocatable :: Root(:)

      integer(int32), allocatable :: NodeID_MainStem(:)
      type(maize_NodeID_Branch_), allocatable :: NodeID_Branch(:)

      logical ::  inLoop = .false.
      real(real64) :: hours = 0.0d0

      ! growth simulation
      real(real64) :: FullyExpanded_stem_threshold = 0.10d0
      integer(int32) :: MaxBranchNum = 20
      type(maize_internode_info_), allocatable :: InterNodeInfo(:)
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
      integer(int32) :: stem_division(1:3) = [10, 10, 10]
      integer(int32) :: leaf_division(1:3) = [10, 10, 10]
      integer(int32) :: ear_division(1:3) = [10, 10, 10]
      integer(int32) :: panicle_division(1:3) = [10, 10, 10]

   contains
      procedure :: init => createMaize
      procedure :: create => createMaize
      procedure, public :: msh => mshMaize
      procedure, public :: vtk => vtkMaize
      procedure, public :: stl => stlMaize
      procedure, public :: json => jsonMaize
      procedure, public :: update => updateMaize

      ! Editor
      procedure, public :: remove => removeMaize
      procedure, public :: rotate => rotateMaize
      procedure, public :: move => moveMaize

      procedure, public :: getElementList => getElementListMaize
      ! Info
      !procedure,public :: properties => propertiesMaize
      ! number of subdomain
      procedure, public :: ns => nsMaize
      ! number of element
      procedure, public :: ne => neMaize
      ! number of points
      procedure, public :: nn => nnMaize
      ! range of pointIDs for [Organ type, ID]
      procedure, public :: nn_range => nn_rangeMaize

      ! MemSize
      procedure, public :: checkMemoryRequirement => checkMemoryRequirementMaize
      ! number of stem, leaf ...etc.
      procedure, public :: numleaf => numleafMaize
      procedure, public :: numstem => numstemMaize
      procedure, public :: numroot => numrootMaize
      procedure, public :: numEar => numEarMaize
      procedure, public :: numPanicle => numPanicleMaize
      ! get pointers
      procedure, public :: getFEMDomainPointers => getFEMDomainPointersMaize
      ! check data
      procedure, public :: checkYoungModulus => checkYoungModulusMaize
      procedure, public :: checkPoissonRatio => checkPoissonRatioMaize
      procedure, public :: checkDensity => checkDensityMaize
      ! get data
      procedure, public :: getYoungModulus => getYoungModulusMaize
      procedure, public :: getPoissonRatio => getPoissonRatioMaize
      procedure, public :: getDensity => getDensityMaize
      procedure, public :: getVertices => getVerticesMaize

      procedure, public :: getYoungModulusField => getYoungModulusFieldMaize
      procedure, public :: getPoissonRatioField => getPoissonRatioFieldMaize
      procedure, public :: getDensityField => getDensityFieldMaize
      procedure, public :: getStressField => getStressFieldMaize

      ! alternative setters
      procedure, public :: setYoungModulus => setYoungModulusMaize
      procedure, public :: setPoissonRatio => setPoissonRatioMaize
      procedure, public :: setDensity => setDensityMaize

      ! simulator
      procedure, public :: getEigenMode => getEigenModeMaize
      procedure, public :: getDisplacement => getDisplacementMaize
      procedure, public :: deform => deformMaize

      ! export eigen modes and frequency
      procedure, public :: export_eig => export_eigMaize

      ! editor
      procedure, public :: resizeLeaf => resizeLeafMaize
   end type
contains

! #############################################################
   subroutine createMaize(this, config, debug)
      class(Maize_), intent(inout) :: this
      character(*), intent(in) :: config
      character(:), allocatable :: line
      logical, optional, intent(in) :: debug
      logical :: debug_log
      type(IO_) :: Maizeconfig
      type(Random_) :: random
      integer(int32)::i, n, j, k, num_leaf, num_stem_node, num_branch_branch, cpid
      real(real64) :: x_A(1:3)
      real(real64), allocatable ::  Leaf_angle_z(:)

      debug_log = input(default=.false., option=debug)
      cpid = 0

      if (debug_log) then
         cpid = cpid + 1
         call print("createMaize #"+str(cpid))
      end if
      this%mainstem_length = freal(Maizeconfig%parse(config, key1="Mainstem", key2="Length"))
      this%mainstem_width = freal(Maizeconfig%parse(config, key1="Mainstem", key2="Width"))
      this%mainstem_bottom_width = freal(Maizeconfig%parse(config, key1="Mainstem", key2="BottomWidth"))
      this%mainstem_top_width = freal(Maizeconfig%parse(config, key1="Mainstem", key2="TopWidth"))
      this%mainstem_node = fint(Maizeconfig%parse(config, key1="Mainstem", key2="Node"))
      this%ms_angle_ave = freal(Maizeconfig%parse(config, key1="Mainstem", key2="ms_angle_ave"))
      this%ms_angle_sig = freal(Maizeconfig%parse(config, key1="Mainstem", key2="ms_angle_sig"))

      if (debug_log) then
         cpid = cpid + 1
         call print("createMaize #"+str(cpid))
      end if

      ! get number of leaf
      this%num_leaf = 1
      do
         if (this%num_leaf == this%mainstem_node) then
            this%num_leaf = this%num_leaf - 1
            exit
         end if

         line = Maizeconfig%parse(config, key1="Leaf_"//str(this%num_leaf)//"_", key2="From")

         if (len(trim(line)) == 0) then
            this%num_leaf = this%num_leaf - 1
            exit
         else
            this%num_leaf = this%num_leaf + 1
            cycle
         end if

      end do

      if (debug_log) then
         cpid = cpid + 1
         call print("createMaize #"+str(cpid))
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
      allocate (this%leaf_From(this%num_leaf))
      !allocate(this%leaf_Length(this%num_leaf))
      !allocate(this%leaf_Width(this%num_leaf))

      if (debug_log) then
         cpid = cpid + 1
         call print("createMaize #"+str(cpid))
      end if

      do i = 1, this%num_leaf
         this%leaf_From(i) = fint(Maizeconfig%parse( &
                                  config, key1="Leaf_"//str(i)//"_", key2="From"))
         !this%leaf_Length(i)= freal(Maizeconfig%parse(&
         !    config,key1="Leaf_"//str(i)//"_",key2="Length"))
         !this%leaf_Width(i)= freal(Maizeconfig%parse(&
         !    config,key1="Leaf_"//str(i)//"_",key2="Width"))
         this%leaf_curvature(i) = freal(Maizeconfig%parse( &
                                        config, key1="Leaf_"//str(i)//"_", key2="leaf_curvature"))
         this%leaf_thickness_ave(i) = freal(Maizeconfig%parse( &
                                            config, key1="Leaf_"//str(i)//"_", key2="leaf_thickness_ave"))
         this%leaf_thickness_sig(i) = freal(Maizeconfig%parse( &
                                            config, key1="Leaf_"//str(i)//"_", key2="leaf_thickness_sig"))
         this%leaf_angle_ave_x(i) = freal(Maizeconfig%parse( &
                                          config, key1="Leaf_"//str(i)//"_", key2="leaf_angle_ave_x"))
         this%leaf_angle_sig_x(i) = freal(Maizeconfig%parse( &
                                          config, key1="Leaf_"//str(i)//"_", key2="leaf_angle_sig_x"))
         this%leaf_angle_ave_z(i) = freal(Maizeconfig%parse( &
                                          config, key1="Leaf_"//str(i)//"_", key2="leaf_angle_ave_z"))
         this%leaf_angle_sig_z(i) = freal(Maizeconfig%parse( &
                                          config, key1="Leaf_"//str(i)//"_", key2="leaf_angle_sig_z"))
         this%leaf_length_ave(i) = freal(Maizeconfig%parse( &
                                         config, key1="Leaf_"//str(i)//"_", key2="leaf_length_ave"))
         this%leaf_length_sig(i) = freal(Maizeconfig%parse( &
                                         config, key1="Leaf_"//str(i)//"_", key2="leaf_length_sig"))
         this%leaf_width_ave(i) = freal(Maizeconfig%parse( &
                                        config, key1="Leaf_"//str(i)//"_", key2="leaf_width_ave"))
         this%leaf_width_sig(i) = freal(Maizeconfig%parse( &
                                        config, key1="Leaf_"//str(i)//"_", key2="leaf_width_sig"))
      end do

      if (debug_log) then
         cpid = cpid + 1
         call print("createMaize #"+str(cpid))
      end if
!    this%mainroot_length = freal(Maizeconfig%parse(config,key1="Mainroot",key2="Length"))
!    this%mainroot_width = freal(Maizeconfig%parse(config,key1="Mainroot",key2="Width"))
!    this%mainroot_node = fint(Maizeconfig%parse(config,key1="Mainroot",key2="Node"))

      ! get number of branch && number of node
!    this%num_branch_root=1
!    this%num_branch_root_node=0
!    do
!        line = Maizeconfig%parse(config,key1="Branchroot_"//str(this%num_branch_root)//"_",key2="Node" )
!        if(len(trim(line))==0)then
!            this%num_branch_root = this%num_branch_root -1
!            exit
!        else
!            this%num_branch_root = this%num_branch_root  + 1
!            this%num_branch_root_node = this%num_branch_root_node + fint(line)
!            cycle
!        endif
!    enddo

      if (debug_log) then
         cpid = cpid + 1
         call print("createMaize #"+str(cpid))
      end if
      this%num_stem = this%mainstem_node
      !this%num_root =this%num_branch_root_node + this%mainroot_node

      allocate (this%leaf_list(this%num_leaf))
      allocate (this%stem_list(this%num_stem))
      !allocate(this%root_list(this%num_root))

      allocate (this%leaf(this%num_leaf))
      allocate (this%stem(this%num_stem))
      
      ! Change Crosssectional shape from RECT to CYLINDER
      do i=1,size(this%stem)
         this%stem(i)%cross_section_shape = PF_STEM_SHAPE_CYLINDER
      enddo
      !allocate(this%root(this%num_root))

      this%leaf2stem = zeros(this%num_leaf, this%num_stem)
      this%stem2stem = zeros(this%num_stem, this%num_stem)
      this%ear2stem = zeros(this%num_ear, this%num_stem)
      this%panicle2stem = zeros(this%num_panicle, this%num_stem)
      !this%root2stem = zeros( this%num_root , this%num_stem)
      !this%root2root = zeros( this%num_root , this%num_root )

      if (debug_log) then
         cpid = cpid + 1
         call print("createMaize #"+str(cpid))
      end if

      ! set mainstem
      Leaf_angle_z = zeros(this%mainstem_node)
      do i = 1, this%mainstem_node

         call this%stem(i)%init( &
            x_num=this%stem_division(1), &
            y_num=this%stem_division(2), &
            z_num=this%stem_division(3) &
            )
         if (this%mainstem_bottom_width /= 0.0d0 .and. this%mainstem_top_width /= 0) then
            this%mainstem_width = (this%mainstem_top_width - this%mainstem_bottom_width)/this%mainstem_node &
                                  *(i - 1) + this%mainstem_bottom_width
         end if
         call this%stem(i)%resize( &
            x=this%mainstem_width, &
            y=this%mainstem_width, &
            z=this%mainstem_length/dble(this%mainstem_node) &
            )
         call this%stem(i)%rotate( &
            x=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)), &
            y=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)), &
            z=radian(random%gauss(mu=this%ms_angle_ave, sigma=this%ms_angle_sig)) &
            )
      end do

      do i = 1, this%mainstem_node - 1
         call this%stem(i + 1)%connect("=>", this%stem(i))
         this%stem2stem(i + 1, i) = 1
      end do

      if (debug_log) then
         cpid = cpid + 1
         call print("createMaize #"+str(cpid))
      end if

      !set leaf
      num_leaf = 0
      do i = 1, this%num_leaf
         ! 1葉/1節
         ! add leaves

         num_leaf = num_leaf + 1

         call this%leaf(num_leaf)%init(species=PF_MAIZE, &
                                       x_num=this%leaf_division(1), &
                                       y_num=this%leaf_division(2), &
                                       z_num=this%leaf_division(3) &
                                       )
         call this%leaf(num_leaf)%femdomain%resize( &
            y=random%gauss(mu=this%leaf_thickness_ave(i), sigma=this%leaf_thickness_sig(i)), &
            z=random%gauss(mu=this%leaf_length_ave(i), sigma=this%leaf_length_sig(i)), &
            x=random%gauss(mu=this%leaf_width_ave(i), sigma=this%leaf_width_sig(i)) &
            )
         call this%leaf(num_leaf)%curve(curvature=this%leaf_curvature(i))

         call this%leaf(num_leaf)%femdomain%rotate( &
            x=radian(random%gauss(mu=this%leaf_angle_ave_x(i), sigma=this%leaf_angle_sig_x(i))), &
            y=0.0d0, &
            z=radian(random%gauss(mu=this%leaf_angle_ave_z(i), sigma=this%leaf_angle_sig_z(i))) &
            )
         Leaf_angle_z(this%Leaf_From(i)) = radian(random%gauss(mu=this%leaf_angle_ave_z(i), sigma=this%leaf_angle_sig_z(i)))
         call this%leaf(num_leaf)%connect("=>", this%stem(this%Leaf_From(i)))
         this%leaf2stem(num_leaf, this%Leaf_From(i)) = 1
      end do

      ! set panicle

      ! set panicles
      ! get number of panicles
      this%num_panicle = 1
      do
         if (this%num_panicle == this%mainstem_node) then
            this%num_panicle = this%num_panicle - 1
            exit
         end if

         line = Maizeconfig%parse(config, key1="Panicle_"//str(this%num_panicle)//"_", key2="From")

         if (len(trim(line)) == 0) then
            this%num_panicle = this%num_panicle - 1
            exit
         else
            this%num_panicle = this%num_panicle + 1
            cycle
         end if

      end do

      allocate (this%panicle(this%num_panicle))
      this%panicle2stem = zeros(this%num_panicle, size(this%stem2stem, 1))
      do i = 1, this%num_panicle
         call this%panicle(i)%init( &
            x_num=this%panicle_division(1), &
            y_num=this%panicle_division(2), &
            z_num=this%panicle_division(3), &
            Length=freal(Maizeconfig%parse(config, key1="Panicle_"//str(i)//"_", key2="Length")), &
            Width=freal(Maizeconfig%parse(config, key1="Panicle_"//str(i)//"_", key2="Width")), &
            Node=fint(Maizeconfig%parse(config, key1="Panicle_"//str(i)//"_", key2="Node")), &
            shape_factor=freal(Maizeconfig%parse(config, key1="Panicle_"//str(i)//"_", key2="shape_factor")) & ! optional, then, small
            )
         n = fint(Maizeconfig%parse(config, key1="Panicle_"//str(i)//"_", key2="From"))
         this%panicle2stem(i, n) = 1

         ! こいつを実装する．
         call this%panicle(i)%connect("=>", this%stem(n))

      end do

      ! set ears
      ! get number of ears
      this%num_ear = 1
      do
         if (this%num_ear == this%mainstem_node) then
            this%num_ear = this%num_ear - 1
            exit
         end if

         line = Maizeconfig%parse(config, key1="Ear_"//str(this%num_ear)//"_", key2="From")

         if (len(trim(line)) == 0) then
            this%num_ear = this%num_ear - 1
            exit
         else
            this%num_ear = this%num_ear + 1
            cycle
         end if

      end do

      allocate (this%ear(this%num_ear))
      this%ear2stem = zeros(this%num_ear, size(this%stem2stem, 1))
      do i = 1, this%num_ear
         n = fint(Maizeconfig%parse(config, key1="Ear_"//str(i)//"_", key2="From"))

         call this%ear(i)%init( &
            x_num=this%ear_division(1), &
            y_num=this%ear_division(2), &
            z_num=this%ear_division(3), &
            Length=freal(Maizeconfig%parse(config, key1="Ear_"//str(i)//"_", key2="Length")), &
            Width=freal(Maizeconfig%parse(config, key1="Ear_"//str(i)//"_", key2="Width")), &
            Angle=freal(Maizeconfig%parse(config, key1="Ear_"//str(i)//"_", key2="Angle")), & ! deg.
            Leaf_angle_z=Leaf_angle_z(n) &
            )
         this%ear2stem(i, n) = 1

         ! こいつを実装する．
         call this%ear(i)%connect("=>", this%stem(n))

      end do

      call this%update()

   end subroutine
! #############################################################

! ########################################
   subroutine mshMaize(this, name, num_threads)
      class(Maize_), intent(inout) :: this
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
   subroutine vtkMaize(this, name, num_threads, single_file, &
                       scalar_field, vector_field, tensor_field, field_name)
      class(Maize_), intent(inout) :: this
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

            if (allocated(this%Ear)) then
               do i = 1, size(this%Ear)
                  if (.not. this%Ear(i)%femdomain%empty()) then
                     femdomain = femdomain + this%Ear(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%Panicle)) then
               do i = 1, size(this%Panicle)
                  if (.not. this%Panicle(i)%femdomain%empty()) then
                     femdomain = femdomain + this%Panicle(i)%femdomain
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

      if (allocated(this%ear)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%ear)
            if (this%ear(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%ear(i)%vtk(name=name//"_ear"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

      if (allocated(this%panicle)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%panicle)
            if (this%panicle(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%panicle(i)%vtk(name=name//"_panicle"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

   end subroutine
! ########################################

! ########################################
   subroutine jsonMaize(this, name)
      class(Maize_), intent(inout) :: this
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
      call f%write('"return_Maize":0')
      call f%write("}")
      call f%close()
   end subroutine
! ########################################

! ########################################
   subroutine stlMaize(this, name, num_threads, single_file)
      class(Maize_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32), optional, intent(in) :: num_threads
      logical, optional, intent(in) :: single_file
      integer(int32) :: i, n
      type(FEMDomain_) :: femdomain

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

            if (allocated(this%Ear)) then
               do i = 1, size(this%Ear)
                  if (.not. this%Ear(i)%femdomain%empty()) then
                     femdomain = femdomain + this%Ear(i)%femdomain
                  end if
               end do
            end if

            if (allocated(this%Panicle)) then
               do i = 1, size(this%Panicle)
                  if (.not. this%Panicle(i)%femdomain%empty()) then
                     femdomain = femdomain + this%Panicle(i)%femdomain
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

      n = input(default=1, option=num_threads)
      if (allocated(this%stem)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%stem)
            if (this%stem(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%stem(i)%stl(name=name//"_stem"//str(i))
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
               call this%root(i)%stl(name=name//"_root"//str(i))
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
               call this%leaf(i)%stl(name=name//"_leaf"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

      if (allocated(this%ear)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%ear)
            if (this%ear(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%ear(i)%stl(name=name//"_ear"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

      if (allocated(this%panicle)) then
         !$OMP parallel num_threads(n) private(i)
         !$OMP do
         do i = 1, size(this%panicle)
            if (this%panicle(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%panicle(i)%stl(name=name//"_panicle"//str(i))
            end if
         end do
         !$OMP end do
         !$OMP end parallel
      end if

   end subroutine
! ########################################

! ########################################
   subroutine moveMaize(this, x, y, z)
      class(Maize_), intent(inout) :: this
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

      if (allocated(this%Ear)) then
         do i = 1, size(this%Ear)
            if (this%Ear(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%Ear(i)%move(x=x, y=y, z=z)
            end if
         end do
      end if

      if (allocated(this%panicle)) then
         do i = 1, size(this%panicle)
            if (this%panicle(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%panicle(i)%move(x=x, y=y, z=z)
            end if
         end do
      end if

   end subroutine
! ########################################

! ########################################
   subroutine rotateMaize(this, x, y, z)
      class(Maize_), intent(inout) :: this
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

      if (allocated(this%Ear)) then
         do i = 1, size(this%Ear)
            if (this%Ear(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%Ear(i)%rotate(x=x, y=y, z=z)
            end if
         end do
      end if

      if (allocated(this%panicle)) then
         do i = 1, size(this%panicle)
            if (this%panicle(i)%femdomain%mesh%empty() .eqv. .false.) then
               call this%panicle(i)%rotate(x=x, y=y, z=z)
            end if
         end do
      end if
   end subroutine
! ########################################

! ########################################
   recursive subroutine updateMaize(this, stem_id, root_id, leaf_id, overset_margin, debug)
      class(Maize_), intent(inout) :: this
      integer(int32), optional, intent(in) :: stem_id, root_id, leaf_id
      real(real64), optional, intent(in) :: overset_margin
      integer(int32) :: i, j, this_stem_id, next_stem_id, A_id, B_id, itr_tol, itr, k, kk
      integer(int32) :: this_leaf_id, next_leaf_id
      integer(int32) :: this_root_id, next_root_id, InterNodeID, PetioleID, StemID, LeafID
      real(real64) :: x_A(3), x_B(3), diff(3), error, last_error, mgn, overset_m, error_tol
      logical, optional, intent(in) :: debug
      integer(int32) :: next_ear_id, next_panicle_id

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

      ! if Maize_internode_info_ is active
      ! update parameters
!    if(allocated(this%InterNodeInfo) )then
!        do i=0,this%MaxBranchNum
!
!            if(allocated(this%InterNodeInfo(i)%FinalInterNodeLength ) )then
!                do j=1,this%maxInterNodeID(StemID=i)
!                    InterNodeID = this%searchStem(StemID=i,InterNodeID=j)
!                    if(size(this%InterNodeInfo(i)%FinalInterNodeLength) < j ) then
!                        print *, "ERROR :: updateMaize >> "
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
!                            print *, "ERROR :: updateMaize >> "
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
!                                    print *, "ERROR :: updateMaize >> "
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
!                                    print *, "ERROR :: updateMaize >> "
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
         print *, "updateMaize >> ERROR :: .not. allocated(this%stem2stem )"
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
                  print *, "Maize % update s2s >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "Maize % update s2s >> ERROR :: not converged"
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
                  print *, "Maize % update r2s >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "Maize % update r2s  >> ERROR :: not converged"
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
                  print *, "Maize % update r2r >> error :: ", error
               end if
            end if
            if (itr > itr_tol) then
               print *, "Maize % update r2r >> ERROR :: not converged"
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
               print *, "Maize % update l2s >> error :: ", error
            end if
         end if
         if (itr > itr_tol) then
            print *, "Maize % update l2s  >> ERROR :: not converged"
            stop
         end if

         if (abs(error) + abs(last_error) < error_tol) exit
         last_error = error
      end do

      ! ear to stem
      last_error = 1.0d0
      do
         itr = itr + 1
         error = 0.0d0
         do i = 1, size(this%ear2stem, 1)
            do j = 1, size(this%ear2stem, 2)
               this_stem_id = j
               next_ear_id = i
               if (this%ear2stem(i, j) == 1) then
                  ! this_stem_id ===>>> next_ear_id, connected!
                  !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
                  !x_A(:) = this%ear(next_ear_id)%getCoordinate("A")

                  ! Overset分食い込ませる
                  x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("B") &
                           + overset_m*this%stem(this_stem_id)%getCoordinate("A")
                  ! Overset分食い込ませる
                  x_A(:) = this%ear(next_ear_id)%getCoordinate("A")

                  diff(:) = x_B(:) - x_A(:)
                  error = error + dot_product(diff, diff)
                  call this%ear(next_ear_id)%move(x=diff(1), y=diff(2), z=diff(3))
               end if
            end do
         end do

         if (present(debug)) then
            if (debug) then
               print *, "Maize % update l2s >> error :: ", error
            end if
         end if
         if (itr > itr_tol) then
            print *, "Maize % update l2s  >> ERROR :: not converged"
            stop
         end if

         if (abs(error) + abs(last_error) < error_tol) exit
         last_error = error
      end do

      ! panicle to stem
      last_error = 1.0d0
      do
         itr = itr + 1
         error = 0.0d0
         do i = 1, size(this%panicle2stem, 1)
            do j = 1, size(this%panicle2stem, 2)
               this_stem_id = j
               next_panicle_id = i
               if (this%panicle2stem(i, j) == 1) then
                  ! this_stem_id ===>>> next_panicle_id, connected!
                  !x_B(:) = this%stem(this_stem_id)%getCoordinate("B")
                  !x_A(:) = this%panicle(next_panicle_id)%getCoordinate("A")

                  ! Overset分食い込ませる
                  x_B(:) = (1.0d0 - overset_m)*this%stem(this_stem_id)%getCoordinate("B") &
                           + overset_m*this%stem(this_stem_id)%getCoordinate("A")
                  ! Overset分食い込ませる
                  x_A(:) = this%panicle(next_panicle_id)%getCoordinate("A")

                  diff(:) = x_B(:) - x_A(:)
                  error = error + dot_product(diff, diff)
                  call this%panicle(next_panicle_id)%move(x=diff(1), y=diff(2), z=diff(3))
               end if
            end do
         end do

         if (present(debug)) then
            if (debug) then
               print *, "Maize % update l2s >> error :: ", error
            end if
         end if
         if (itr > itr_tol) then
            print *, "Maize % update l2s  >> ERROR :: not converged"
            stop
         end if

         if (abs(error) + abs(last_error) < error_tol) exit
         last_error = error
      end do

   end subroutine
! ########################################

! ########################################
!recursive subroutine updateMaize(this,debug)
!    class(Maize_),intent(inout) :: this
!    integer(int32) :: i,j,this_stem_id,next_stem_id,A_id,B_id,itr_tol,itr
!    integer(int32) :: this_leaf_id,next_leaf_id
!    integer(int32) :: this_root_id,next_root_id
!    real(real64) :: x_A(3),x_B(3),diff(3),error,last_error
!    logical,optional,intent(in) :: debug
!
!
!    ! update connectivity
!    if(.not. allocated(this%stem2stem ))then
!        print *, "updateMaize >> ERROR :: .not. allocated(this%stem2stem )"
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
!                print *, "Maize % update >> error :: ",error
!            endif
!        endif
!        if(itr > itr_tol) then
!            print *, "Maize % update >> ERROR :: not converged"
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
!                print *, "Maize % update >> error :: ",error
!            endif
!        endif
!
!        if(itr > itr_tol) then
!            print *, "Maize % update >> ERROR :: not converged"
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
!                print *, "Maize % update >> error :: ",error
!            endif
!        endif
!        if(itr > itr_tol) then
!            print *, "Maize % update >> ERROR :: not converged"
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

   subroutine removeMaize(this, root)
      class(Maize_), intent(inout) :: this
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
      if (allocated(this%Ear)) deallocate (this%Ear)
      if (allocated(this%Panicle)) deallocate (this%Panicle)

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
      if (allocated(this%Ear2stem)) deallocate (this%Ear2stem)
      if (allocated(this%Panicle2stem)) deallocate (this%Panicle2stem)
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
      this%TYPE_EAR = 4
      this%TYPE_PANICLE = 5
      ! 節-節点データ構造
      call this%struct%remove()
      if (allocated(this%leaf2stem)) deallocate (this%leaf2stem)! (:,:)
      if (allocated(this%stem2stem)) deallocate (this%stem2stem)! (:,:)
      if (allocated(this%ear2stem)) deallocate (this%ear2stem)! (:,:)
      if (allocated(this%panicle2stem)) deallocate (this%panicle2stem)! (:,:)
      if (allocated(this%root2stem)) deallocate (this%root2stem)! (:,:)
      if (allocated(this%root2root)) deallocate (this%root2root)! (:,:)

      this%mainstem_length = 0.0d0
      this%mainstem_width = 0.0d0
      this%mainstem_bottom_width = 0.0d0
      this%mainstem_top_width = 0.0d0
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
      this%num_ear = 1
      this%num_panicle = 1
      this%num_root = 0

      ! 器官オブジェクト配列
      if (allocated(this%leaf_list)) deallocate (this%leaf_list)! (:)
      if (allocated(this%stem_list)) deallocate (this%stem_list)! (:)
      if (allocated(this%root_list)) deallocate (this%root_list)! (:)

      this%LeafSurfaceData = ""
      if (allocated(this%Leaf)) deallocate (this%Leaf)! (:)
      if (allocated(this%Stem)) deallocate (this%Stem)! (:)
      if (allocated(this%Ear)) deallocate (this%Ear)! (:)
      if (allocated(this%Panicle)) deallocate (this%Panicle)! (:)
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
      this%ear_division(1:3) = [10, 10, 10]
      this%panicle_division(1:3) = [10, 10, 10]

   end subroutine
! ################################################################
   subroutine checkMemoryRequirementMaize(this)
      class(Maize_), intent(in) :: this
      real(real64) :: re_val
      integer(int64) :: val

      print *, "===================================="
      print *, "checking Memory (RAM) Requirement..."
      print *, "------------------------------------"
      print *, "| thisect type                     | Maize"
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
   function nsMaize(this) result(ret)
      class(Maize_), intent(in) :: this
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

      do i = 1, size(this%Ear)
         if (.not. this%Ear(i)%femdomain%mesh%empty()) then
            ret = ret + 1
         end if
      end do

      do i = 1, size(this%Panicle)
         if (.not. this%Panicle(i)%femdomain%mesh%empty()) then
            ret = ret + 1
         end if
      end do
   end function

! ##################################################################

! ##################################################################
   pure function nnMaize(this) result(ret)
      class(Maize_), intent(in) :: this
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

      if (allocated(this%Ear)) then
         do i = 1, size(this%Ear)
            if (.not. this%Ear(i)%femdomain%mesh%empty()) then
               ret = ret + this%Ear(i)%femdomain%nn()
            end if
         end do
      end if

      if (allocated(this%Panicle)) then
         do i = 1, size(this%Panicle)
            if (.not. this%Panicle(i)%femdomain%mesh%empty()) then
               ret = ret + this%Panicle(i)%femdomain%nn()
            end if
         end do
      end if
   end function
! ##################################################################

! ##################################################################
   function nn_rangeMaize(this, organ_type, ID) result(ret)
      class(Maize_), intent(inout) :: this
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
      case ("Ear", "ear", "EAR")

         if (allocated(this%stem)) then
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
               do i = 1, size(this%root)
                  if (.not. this%root(i)%femdomain%mesh%empty()) then
                     offset = offset + this%root(i)%femdomain%nn()
                  end if
               end do
            end if
            if (allocated(this%ear)) then
               do i = 1, ID - 1
                  if (.not. this%ear(i)%femdomain%mesh%empty()) then
                     offset = offset + this%ear(i)%femdomain%nn()
                  end if
               end do
               ret(1) = offset + 1
               ret(2) = offset + this%ear(ID)%femdomain%nn()
            end if
         end if
      case ("Panicle", "panicle", "PANICLE")
         if (allocated(this%stem)) then
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
               do i = 1, size(this%root)
                  if (.not. this%root(i)%femdomain%mesh%empty()) then
                     offset = offset + this%root(i)%femdomain%nn()
                  end if
               end do
            end if
            if (allocated(this%ear)) then
               do i = 1, size(this%ear)
                  if (.not. this%ear(i)%femdomain%mesh%empty()) then
                     offset = offset + this%ear(i)%femdomain%nn()
                  end if
               end do
            end if
            if (allocated(this%panicle)) then
               do i = 1, ID - 1
                  if (.not. this%panicle(i)%femdomain%mesh%empty()) then
                     offset = offset + this%panicle(i)%femdomain%nn()
                  end if
               end do
               ret(1) = offset + 1
               ret(2) = offset + this%panicle(ID)%femdomain%nn()
            end if
         end if

      end select

   end function
! ##################################################################

! ##################################################################
   function neMaize(this) result(ret)
      class(Maize_), intent(in) :: this
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

      if (allocated(this%Ear)) then
         do i = 1, size(this%Ear)
            if (.not. this%Ear(i)%femdomain%mesh%empty()) then
               ret = ret + this%Ear(i)%femdomain%ne()
            end if
         end do
      end if

      if (allocated(this%Panicle)) then
         do i = 1, size(this%Panicle)
            if (.not. this%Panicle(i)%femdomain%mesh%empty()) then
               ret = ret + this%Panicle(i)%femdomain%ne()
            end if
         end do
      end if
   end function
! ##################################################################

! ################################################################
   recursive subroutine setYoungModulusMaize(this, YoungModulus, stem, root, leaf, ear, panicle, ElementList)
      class(Maize_), intent(inout) :: this
      logical, optional, intent(in) :: stem, root, leaf, ear, panicle

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

      if (present(Ear)) then
         if (Ear) then
            n = n + 100
            if (allocated(this%Ear)) then
               do i = 1, size(this%Ear)
                  if (this%Ear(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_EAR) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%EAR(domain_idx)%YoungModulus(elem_idx) = YoungModulus
                        end if
                     end do
                  else
                     this%Ear(i)%YoungModulus = YoungModulus*eyes(this%Ear(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(panicle)) then
         if (panicle) then
            n = n + 100
            if (allocated(this%panicle)) then
               do i = 1, size(this%panicle)
                  if (this%panicle(i)%femdomain%empty()) then
                     cycle
                  elseif (present(ElementList)) then
                     do j = 1, size(ElementList, 1)
                        if (ElementList(j, 1) == this%TYPE_PANICLE) then
                           domain_idx = ElementList(j, 2)
                           elem_idx = ElementList(j, 3)
                           this%PANICLE(domain_idx)%YoungModulus(elem_idx) = YoungModulus
                        end if
                     end do
                  else
                     this%panicle(i)%YoungModulus = YoungModulus*eyes(this%panicle(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (n == 0) then
         call this%setYoungModulus(YoungModulus=YoungModulus, stem=.true., root=.true., leaf=.true. &
                                   , Ear=.true., Panicle=.true., ElementList=ElementList)
      end if

   end subroutine
! ################################################################

! ################################################################
   recursive subroutine setPoissonRatioMaize(this, PoissonRatio, stem, root, leaf, ear, panicle)
      class(Maize_), intent(inout) :: this
      logical, optional, intent(in) :: stem, root, leaf, ear, panicle
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

      if (present(Ear)) then
         if (Ear) then
            n = n + 100
            if (allocated(this%Ear)) then
               do i = 1, size(this%Ear)
                  if (this%Ear(i)%femdomain%empty()) then
                     cycle
                  else
                     this%Ear(i)%PoissonRatio = PoissonRatio*eyes(this%Ear(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(panicle)) then
         if (panicle) then
            n = n + 100
            if (allocated(this%panicle)) then
               do i = 1, size(this%panicle)
                  if (this%panicle(i)%femdomain%empty()) then
                     cycle
                  else
                     this%panicle(i)%PoissonRatio = PoissonRatio*eyes(this%panicle(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (n == 0) then
         call this%setPoissonRatio(PoissonRatio=PoissonRatio, stem=.true., root=.true., leaf=.true. &
                                   , Ear=.true., Panicle=.true.)
      end if

   end subroutine
! ################################################################

! ################################################################
   recursive subroutine setDensityMaize(this, Density, stem, root, leaf, ear, panicle)
      class(Maize_), intent(inout) :: this
      logical, optional, intent(in) :: stem, root, leaf, ear, panicle
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

      if (present(Ear)) then
         if (Ear) then
            n = n + 100
            if (allocated(this%Ear)) then
               do i = 1, size(this%Ear)
                  if (this%Ear(i)%femdomain%empty()) then
                     cycle
                  else
                     this%Ear(i)%Density = Density*eyes(this%Ear(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (present(panicle)) then
         if (panicle) then
            n = n + 100
            if (allocated(this%panicle)) then
               do i = 1, size(this%panicle)
                  if (this%panicle(i)%femdomain%empty()) then
                     cycle
                  else
                     this%panicle(i)%Density = Density*eyes(this%panicle(i)%femdomain%ne())
                  end if
               end do
            end if
         end if
      end if

      if (n == 0) then
         call this%setDensity(Density=Density, stem=.true., root=.true., leaf=.true. &
                              , Ear=.true., Panicle=.true.)
      end if

   end subroutine
! ################################################################

! ################################################################
   function getEigenModeMaize(this, ground_level, penalty, debug, Frequency, EbOM_Algorithm, num_mode) result(EigenVectors)
      class(Maize_), target, intent(inout) :: this
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

      integer(int32) :: myEarID, myPanicleID
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
      n = this%numStem() + this%numLeaf() + this%numRoot() + this%numEar() + this%numPanicle()

      allocate (FEMDomainPointers(n))
      ! ORDER
      ! [STEM] => [LEAF] => [ROOT] => [Ear] => [PANICLE]
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

      if (allocated(this%Ear2stem)) then
         if (allocated(this%Ear) .and. allocated(this%stem)) then
            do myEarID = 1, size(this%Ear2stem, 1)
               do yourStemID = 1, size(this%Ear2stem, 2)
                  if (this%Ear2stem(myEarID, yourStemID) >= 1) then
                     ! connected
                     call this%Ear(myEarID)%femdomain%overset( &
                        FEMDomain=this%stem(yourStemID)%femdomain, &
                        DomainID=yourStemID, &
                        MyDomainID=this%numStem() + this%numLeaf() + this%numRoot() + myEarID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%Ear(myEarID)%femdomain, &
                        DomainID=this%numStem() + this%numLeaf() + this%numRoot() + myEarID, &
                        MyDomainID=yourStemID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"

                  end if
               end do
            end do
         end if
      end if

      if (allocated(this%Panicle2stem)) then
         if (allocated(this%Panicle) .and. allocated(this%stem)) then
            do myPanicleID = 1, size(this%Panicle2stem, 1)
               do yourStemID = 1, size(this%Panicle2stem, 2)
                  if (this%Panicle2stem(myPanicleID, yourStemID) >= 1) then
                     ! connected
                     call this%Panicle(myPanicleID)%femdomain%overset( &
                        FEMDomain=this%stem(yourStemID)%femdomain, &
                        DomainID=yourStemID, &
                        MyDomainID=this%numStem() + this%numLeaf() + this%numRoot() + &
                        this%numEar() + myPanicleID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"

                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%Panicle(myPanicleID)%femdomain, &
                        DomainID=this%numStem() + this%numLeaf() + this%numRoot() + &
                        this%numEar() + myPanicleID, &
                        MyDomainID=yourStemID, &
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

      call solver%init(NumDomain=this%numStem() + this%numLeaf() + this%numRoot() &
                       + this%numEar() + this%numPanicle())

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

!! ################################################################
!function getDisplacement_elastodynamic_maize(this, ground_level,penalty,debug,Frequency,EbOM_Algorithm,num_mode) result(EigenVectors)
!    class(Maize_),target,intent(inout) :: this
!    real(real64),intent(in) :: ground_level
!    real(real64),optional,intent(in) :: penalty
!    logical,optional,intent(in) :: debug
!    real(real64),allocatable,intent(inout) :: Frequency(:)
!    character(*),optional,intent(in) :: EbOM_Algorithm
!
!    integer(int32),optional,intent(in) :: num_mode
!    integer(int32) :: num_freq
!    !integer(int32),optional,intent(in) :: num_mode
!
!
!    type(FEMDomainp_),allocatable :: FEMDomainPointers(:)
!    type(FEMSolver_) :: solver
!    type(Math_) :: math
!
!    real(real64),allocatable :: EigenVectors(:,:),buf(:,:),buf_vec(:)
!
!    integer(int32) :: stem_id, leaf_id, root_id,DomainID,ElementID,i,n
!    integer(int32) :: myStemID, yourStemID, myLeafID,myRootID, yourRootID
!    integer(int32),allocatable :: FixBoundary(:)
!    integer(int32) :: nn_domains,EbOM_Algorithm_id
!    real(real64) :: vec_norm
!
!    integer(int32) :: myEarID, myPanicleID
!    real(real64),allocatable :: all_frequency(:),All_EigenVectors(:,:)
!
!    num_freq = input(default=10,option=num_mode)
!
!    EbOM_Algorithm_id = FEMDomain_Overset_GPP
!    if(present(EbOM_Algorithm) )then
!        if(EbOM_Algorithm=="P2P")then
!            EbOM_Algorithm_id=FEMDomain_Overset_P2P
!        elseif(EbOM_Algorithm=="GPP")then
!            EbOM_Algorithm_id=FEMDomain_Overset_P2P
!        endif
!    endif
!
!    ! linear elasticity with infinitesimal strain theory
!    n = this%numStem() + this%numLeaf() + this%numRoot() + this%numEar() + this%numPanicle()
!
!    allocate(FEMDomainPointers(n) )
!    ! ORDER
!    ! [STEM] => [LEAF] => [ROOT] => [Ear] => [PANICLE]
!    !(1) >> compute overset
!    ! For stems
!    if(allocated(this%stem2stem) )then
!        if(allocated(this%stem) )then
!            do myStemID = 1,size(this%stem2stem,1)
!                do yourStemID = 1, size(this%stem2stem,2)
!                    if(this%stem2stem(myStemID,yourStemID)>=1 )then
!                        ! connected
!                        call this%stem(myStemID)%femdomain%overset(&
!                            FEMDomain=this%stem(yourStemID)%femdomain,&
!                            DomainID   = yourStemID    ,&
!                            MyDomainID = myStemID  ,&
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                        call this%stem(yourStemID)%femdomain%overset(&
!                            FEMDomain=this%stem(myStemID)%femdomain,&
!                            DomainID   = myStemID    ,&
!                            MyDomainID = yourStemID  ,&
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                    endif
!                enddo
!            enddo
!        endif
!    endif
!
!    if(allocated(this%leaf2stem) )then
!        if(allocated(this%leaf) .and. allocated(this%stem) )then
!            do myLeafID = 1,size(this%leaf2stem,1)
!                do yourStemID = 1, size(this%leaf2stem,2)
!                    if(this%leaf2stem(myLeafID,yourStemID)>=1 )then
!                        ! connected
!                        call this%leaf(myLeafID)%femdomain%overset(&
!                            FEMDomain=this%stem(yourStemID)%femdomain,&
!                            DomainID   = yourStemID    ,&
!                            MyDomainID = this%numStem() + myLeafID  , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                        call this%stem(yourStemID)%femdomain%overset(&
!                            FEMDomain=this%leaf(myLeafID)%femdomain,&
!                            DomainID   = this%numStem() + myLeafID    ,&
!                            MyDomainID = yourStemID  , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                    endif
!                enddo
!            enddo
!        endif
!    endif
!
!
!
!
!    if(allocated(this%root2stem) )then
!        if(allocated(this%stem) .and. allocated(this%root) )then
!            do myRootID = 1,size(this%root2stem,1)
!                do yourStemID = 1, size(this%root2stem,2)
!                    if(this%root2stem(myRootID,yourStemID)>=1 )then
!                        ! connected
!                        call this%root(myRootID)%femdomain%overset(&
!                            FEMDomain=this%stem(yourStemID)%femdomain,&
!                            DomainID   = yourStemID    ,&
!                            MyDomainID = this%numStem() +this%numLeaf() + myRootID  , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                        call this%stem(yourStemID)%femdomain%overset(&
!                            FEMDomain=  this%root(myRootID)%femdomain,&
!                            DomainID   = this%numStem() +this%numLeaf() + myRootID    ,&
!                            MyDomainID =  yourStemID , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                    endif
!                enddo
!            enddo
!        endif
!    endif
!
!
!    if(allocated(this%root2root) )then
!        if(allocated(this%root) )then
!            do myRootID = 1,size(this%root2root,1)
!                do yourrootID = 1, size(this%root2root,2)
!                    if(this%root2root(myRootID,yourrootID)>=1 )then
!                        ! connected
!                        call this%root(myRootID)%femdomain%overset(&
!                            FEMDomain=this%root(yourrootID)%femdomain,&
!                            DomainID   = this%numroot() +this%numLeaf() + yourrootID    ,&
!                            MyDomainID = this%numroot() +this%numLeaf() + myRootID  , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!
!                        call this%root(yourrootID)%femdomain%overset(&
!                            FEMDomain=this%root(myRootID)%femdomain,&
!                            DomainID   = this%numroot() +this%numLeaf() + myRootID    ,&
!                            MyDomainID =  this%numroot() +this%numLeaf() + yourrootID , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                    endif
!                enddo
!            enddo
!        endif
!    endif
!
!
!    if(allocated(this%Ear2stem) )then
!        if(allocated(this%Ear) .and. allocated(this%stem) )then
!            do myEarID = 1,size(this%Ear2stem,1)
!                do yourStemID = 1, size(this%Ear2stem,2)
!                    if(this%Ear2stem(myEarID,yourStemID)>=1 )then
!                        ! connected
!                        call this%Ear(myEarID)%femdomain%overset(&
!                            FEMDomain=this%stem(yourStemID)%femdomain,&
!                            DomainID   = yourStemID    ,&
!                            MyDomainID = this%numStem() + this%numLeaf() + this%numRoot() + myEarID  , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                        call this%stem(yourStemID)%femdomain%overset(&
!                            FEMDomain=this%Ear(myEarID)%femdomain,&
!                            DomainID   = this%numStem() + this%numLeaf() + this%numRoot() + myEarID    ,&
!                            MyDomainID = yourStemID  , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!
!                    endif
!                enddo
!            enddo
!        endif
!    endif
!
!    if(allocated(this%Panicle2stem) )then
!        if(allocated(this%Panicle) .and. allocated(this%stem) )then
!            do myPanicleID = 1,size(this%Panicle2stem,1)
!                do yourStemID = 1, size(this%Panicle2stem,2)
!                    if(this%Panicle2stem(myPanicleID,yourStemID)>=1 )then
!                        ! connected
!                        call this%Panicle(myPanicleID)%femdomain%overset(&
!                            FEMDomain=this%stem(yourStemID)%femdomain,&
!                            DomainID   = yourStemID    ,&
!                            MyDomainID = this%numStem() + this%numLeaf() + this%numRoot() + &
!                                this%numEar() + myPanicleID  , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!
!                        call this%stem(yourStemID)%femdomain%overset(&
!                            FEMDomain=this%Panicle(myPanicleID)%femdomain,&
!                            DomainID   = this%numStem() + this%numLeaf() + this%numRoot() + &
!                            this%numEar() + myPanicleID    ,&
!                            MyDomainID = yourStemID  , &
!                            algorithm=EbOM_Algorithm_id ) ! or "P2P"
!                    endif
!                enddo
!            enddo
!        endif
!    endif
!
!
!
!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] overset >> done."
!        endif
!    endif
!
!
!
!    call solver%init(NumDomain=this%numStem() +this%numLeaf() + this%numRoot() &
!        + this%numEar() + this%numPanicle() )
!
!    FEMDomainPointers = this%getFEMDomainPointers()
!    call solver%setDomain(FEMDomainPointers=FEMDomainPointers )
!
!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] initSolver >> done."
!        endif
!    endif
!
!    call solver%setCRS(DOF=3,debug=debug)
!
!    ! CRS ready!
!
!    if( .not. this%checkYoungModulus())then
!        print *, "[ERROR] YoungModulus(:) is not ready."
!        stop
!    endif
!    if( .not. this%checkPoissonRatio())then
!        print *, "[ERROR] PoissonRatio(:) is not ready."
!        stop
!    endif
!    if( .not. this%checkDensity())then
!        print *, "[ERROR] Density(:) is not ready."
!        stop
!    endif
!
!
!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] setCRS >> done."
!        endif
!    endif
!
!    !$OMP parallel
!    !$OMP do
!    do DomainID=1,size(FEMDomainPointers)
!        do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
!            call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
!               Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix(&
!               ElementID=ElementID,&
!               E=this%getYoungModulus(DomainID=DomainID,ElementID=ElementID), &
!               v=this%getPoissonRatio(DomainID=DomainID,ElementID=ElementID)  ) )
!        enddo
!    enddo
!    !$OMP end do
!    !$OMP end parallel
!
!
!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] set Matrix & vectors >> done."
!        endif
!    endif
!
!    call solver%setEbOM(penalty=input(default=10000000.0d0,option=penalty), DOF=3)
!
!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] set EbOM >> done."
!        endif
!    endif
!    call solver%keepThisMatrixAs("A")
!    !call solver%saveMatrix(name="A",CRS_as_dense=.true.,zero_or_nonzero=.true)
!    call solver%zeros()
!
!    ! mass matrix
!    !$OMP parallel
!    !$OMP do
!    do DomainID=1,size(FEMDomainPointers)
!        do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
!            call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
!               Matrix=FEMDomainPointers(DomainID)%femdomainp%massMatrix(&
!                ElementID=ElementID,&
!                Density=this%getDensity(DomainID=DomainID,ElementID=ElementID), &
!                DOF=3 ) )
!        enddo
!    enddo
!    !$OMP end do
!    !$OMP end parallel
!    call solver%keepThisMatrixAs("B")
!
!    ! fix-boundary conditions
!    nn_domains = 0
!    do i=1,size(FEMDomainPointers)
!        if(FEMDomainPointers(i)%FEMDomainp%z_min() <= ground_level )then
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-2 + nn_domains*3
!            call solver%fix_eig(IDs=FixBoundary)
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-1 + nn_domains*3
!            call solver%fix_eig(IDs=FixBoundary)
!            FixBoundary = FEMDomainPointers(i)%FEMDomainp%select(z_max = ground_level )*3-0 + nn_domains*3
!            call solver%fix_eig(IDs=FixBoundary)
!        endif
!        nn_domains = nn_domains + FEMDomainPointers(i)%FEMDomainp%nn()
!    enddo
!
!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] FixBoundary >> done."
!        endif
!    endif
!
!    if(present(debug) )then
!        solver%debug = debug
!    endif
!
!
!
!    call solver%eig(eigen_value=All_Frequency,eigen_vectors=All_EigenVectors)
!    call solver%remove()
!
!    ! simplify this part
!    ! normalize EigenVectors
!    do i=1,size(All_EigenVectors,2)
!        vec_norm = norm(All_EigenVectors(:,i) )
!
!        All_EigenVectors(:,i) = All_EigenVectors(:,i)/vec_norm
!    enddo
!
!    Frequency = zeros(num_freq)
!    EigenVectors = zeros(size(All_EigenVectors,1),num_freq)
!
!    do i=1,num_freq
!        n = minvalID(All_Frequency)
!        EigenVectors(:,i) = All_EigenVectors(:,n)
!        Frequency(i)      = All_Frequency(n)
!        All_Frequency(n) = maxval(All_Frequency)
!    enddo
!
!    do i=1,size(Frequency)
!        if(Frequency(i)<0.0d0)then
!            Frequency(i)=0.0d0
!        endif
!    enddo
!    Frequency = sqrt((Frequency))/(2.0d0*math%PI)
!
!
!
!
!    if(present(debug) )then
!        if(debug)then
!            print *, "[ok] Solve >> done."
!        endif
!    endif
!
!
!
!end function
!! ################################################################

! ########################################
   function numleafMaize(this) result(ret)
      class(Maize_), intent(in) :: this
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
   function numStemMaize(this) result(ret)
      class(Maize_), intent(in) :: this
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
   function numRootMaize(this) result(ret)
      class(Maize_), intent(in) :: this
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

! ########################################
   function numEarMaize(this) result(ret)
      class(Maize_), intent(in) :: this
      integer(int32) :: ret, i

      ret = 0
      if (.not. allocated(this%Ear)) then
         return
      end if

      do i = 1, size(this%Ear)
         if (this%Ear(i)%femdomain%Mesh%empty() .eqv. .false.) then
            ret = ret + 1
         end if
      end do

   end function
! ########################################

! ########################################
   function numPanicleMaize(this) result(ret)
      class(Maize_), intent(in) :: this
      integer(int32) :: ret, i

      ret = 0
      if (.not. allocated(this%Panicle)) then
         return
      end if

      do i = 1, size(this%Panicle)
         if (this%Panicle(i)%femdomain%Mesh%empty() .eqv. .false.) then
            ret = ret + 1
         end if
      end do

   end function
! ########################################

! ################################################################
   function getFEMDomainPointersMaize(this) result(FEMDomainPointers)
      class(Maize_), target, intent(in) :: this
      type(FEMDomainp_), allocatable :: FEMDomainPointers(:)
      integer(int32) :: num_FEMDomain, i, n

      num_FEMDomain = this%numStem() + this%numLeaf() + this%numRoot() &
                      + this%numEar() + this%numPanicle()
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

      do i = 1, this%numEar()
         if (.not. this%Ear(i)%femdomain%empty()) then
            n = n + 1
            FEMDomainPointers(n)%femdomainp => this%Ear(i)%femdomain
         end if
      end do

      do i = 1, this%numPanicle()
         if (.not. this%Panicle(i)%femdomain%empty()) then
            n = n + 1
            FEMDomainPointers(n)%femdomainp => this%Panicle(i)%femdomain
         end if
      end do
   end function
! ################################################################

! ################################################################
   function checkYoungModulusMaize(this) result(all_young_modulus_is_set)
      class(Maize_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusMaize >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusMaize >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusMaize >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

      do i = 1, this%numEar()
         if (.not. allocated(this%Ear(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusMaize >> Young Modulus is not set"
            print *, "@ Ear ID:", i
            print *, "check it by: allocated(this%Ear("+str(i) + ")%YoungModulus)"
            return
         end if
      end do

      do i = 1, this%numPanicle()
         if (.not. allocated(this%Panicle(i)%YoungModulus)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkYoungModulusMaize >> Young Modulus is not set"
            print *, "@ Panicle ID:", i
            print *, "check it by: allocated(this%Panicle("+str(i) + ")%YoungModulus)"
            return
         end if
      end do
   end function
! ################################################################

! ################################################################
   function checkPoissonRatioMaize(this) result(all_young_modulus_is_set)
      class(Maize_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioMaize >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioMaize >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioMaize >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

      do i = 1, this%numEar()
         if (.not. allocated(this%Ear(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioMaize >> Young Modulus is not set"
            print *, "@ Ear ID:", i
            print *, "check it by: allocated(this%Ear("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do

      do i = 1, this%numPanicle()
         if (.not. allocated(this%Panicle(i)%PoissonRatio)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkPoissonRatioMaize >> Young Modulus is not set"
            print *, "@ Panicle ID:", i
            print *, "check it by: allocated(this%Panicle("+str(i) + ")%PoissonRatio)"
            return
         end if
      end do
   end function
! ################################################################

! ################################################################
   function checkDensityMaize(this) result(all_young_modulus_is_set)
      class(Maize_), intent(in) :: this
      logical :: all_young_modulus_is_set
      integer(int32) :: i
      ! order: stem -> leaf -> root

      all_young_modulus_is_set = .true.
      do i = 1, this%numStem()
         if (.not. allocated(this%stem(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensityMaize >> Young Modulus is not set"
            print *, "@ Stem ID:", i
            print *, "check it by: allocated(this%stem("+str(i) + ")%Density)"
            return
         end if
      end do

      do i = 1, this%numLeaf()
         if (.not. allocated(this%Leaf(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensityMaize >> Young Modulus is not set"
            print *, "@ Leaf ID:", i
            print *, "check it by: allocated(this%Leaf("+str(i) + ")%Density)"
            return
         end if
      end do

      do i = 1, this%numRoot()
         if (.not. allocated(this%Root(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensityMaize >> Young Modulus is not set"
            print *, "@ Root ID:", i
            print *, "check it by: allocated(this%Root("+str(i) + ")%Density)"
            return
         end if
      end do

      do i = 1, this%numEar()
         if (.not. allocated(this%Ear(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensityMaize >> Young Modulus is not set"
            print *, "@ Ear ID:", i
            print *, "check it by: allocated(this%Ear("+str(i) + ")%Density)"
            return
         end if
      end do

      do i = 1, this%numPanicle()
         if (.not. allocated(this%Panicle(i)%Density)) then
            all_young_modulus_is_set = .false.
            print *, "[!Warning!] checkDensityMaize >> Young Modulus is not set"
            print *, "@ Panicle ID:", i
            print *, "check it by: allocated(this%Panicle("+str(i) + ")%Density)"
            return
         end if
      end do

   end function
! ################################################################

   function getYoungModulusFieldMaize(this) result(YoungModulus)
      class(Maize_), intent(inout) :: this
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
         elseif (TYPE_IDX == this%TYPE_EAR) then
            YoungModulus(i) = this%EAR(DOMAIN_IDX)%YoungModulus(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_PANICLE) then
            YoungModulus(i) = this%PANICLE(DOMAIN_IDX)%YoungModulus(ELEMENT_IDX)
         end if
      end do

   end function

! ################################################################

! ################################################################
   function getPoissonRatioFieldMaize(this) result(PoissonRatio)
      class(Maize_), intent(inout) :: this
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
         elseif (TYPE_IDX == this%TYPE_EAR) then
            PoissonRatio(i) = this%EAR(DOMAIN_IDX)%PoissonRatio(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_PANICLE) then
            PoissonRatio(i) = this%PANICLE(DOMAIN_IDX)%PoissonRatio(ELEMENT_IDX)
         end if
      end do

   end function

! ################################################################

! ################################################################
   function getDensityFieldMaize(this) result(Density)
      class(Maize_), intent(inout) :: this
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
         elseif (TYPE_IDX == this%TYPE_EAR) then
            Density(i) = this%EAR(DOMAIN_IDX)%Density(ELEMENT_IDX)
         elseif (TYPE_IDX == this%TYPE_PANICLE) then
            Density(i) = this%PANICLE(DOMAIN_IDX)%Density(ELEMENT_IDX)
         end if
      end do

   end function

! ################################################################

   function getYoungModulusMaize(this, DomainID, ElementID) result(YoungModulus)
      class(Maize_), intent(in) :: this
      integer(int32), optional, intent(in) :: DomainID, ElementID
      real(real64) :: YoungModulus
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot() &
          + this%numEar() + this%numPanicle()) then
         print *, "ERROR :: getYoungModulusMaize >>  DomainID exceeds max_domain_size"
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
      elseif (this%numStem() + this%numLeaf() + this%numRoot() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot() + this%numEar()) then
         n = DomainID - (this%numStem() + this%numLeaf() + this%numRoot())
         YoungModulus = this%ear(n)%YoungModulus(ElementID)
         return

      elseif (this%numStem() + this%numLeaf() + this%numRoot() + this%numEar() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot() &
              + this%numEar() + this%numPanicle()) then

         n = DomainID - (this%numStem() + this%numLeaf() + this%numRoot() + this%numEar())
         YoungModulus = this%panicle(n)%YoungModulus(ElementID)
         return
      else
         print *, "[ERROR] >> getYoungModulusMaize >> Invalid DomainID", DomainID
         return
      end if

   end function
! ################################################################

! ################################################################
   function getPoissonRatioMaize(this, DomainID, ElementID) result(PoissonRatio)
      class(Maize_), intent(in) :: this
      integer(int32), intent(in) :: DomainID, ElementID
      real(real64) :: PoissonRatio
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot() &
          + this%numEar() + this%numPanicle()) then
         print *, "ERROR :: getPoissonRatioMaize >>  DomainID exceeds max_domain_size"
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
      elseif (this%numStem() + this%numLeaf() + this%numRoot() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot() + this%numEar()) then
         n = DomainID - (this%numStem() + this%numLeaf() + this%numRoot())
         PoissonRatio = this%ear(n)%PoissonRatio(ElementID)
         return

      elseif (this%numStem() + this%numLeaf() + this%numRoot() + this%numEar() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot() &
              + this%numEar() + this%numPanicle()) then

         n = DomainID - (this%numStem() + this%numLeaf() + this%numRoot() + this%numEar())
         PoissonRatio = this%panicle(n)%PoissonRatio(ElementID)
         return
      else
         print *, "[ERROR] >> getPoissonRatioMaize >> Invalid DomainID", DomainID
         return
      end if

   end function
! ################################################################

! ################################################################
   function getDensityMaize(this, DomainID, ElementID) result(Density)
      class(Maize_), intent(in) :: this
      integer(int32), intent(in) :: DomainID, ElementID
      real(real64) :: Density
      integer(int32) :: i, n

      if (DomainID > this%numStem() + this%numLeaf() + this%numRoot() &
          + this%numEar() + this%numPanicle()) then
         print *, "ERROR :: getDensityMaize >>  DomainID exceeds max_domain_size"
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
      elseif (this%numStem() + this%numLeaf() + this%numRoot() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot() + this%numEar()) then
         n = DomainID - (this%numStem() + this%numLeaf() + this%numRoot())
         Density = this%ear(n)%Density(ElementID)
         return

      elseif (this%numStem() + this%numLeaf() + this%numRoot() + this%numEar() + 1 <= DomainID &
              .and. DomainID <= this%numStem() + this%numLeaf() + this%numRoot() &
              + this%numEar() + this%numPanicle()) then

         n = DomainID - (this%numStem() + this%numLeaf() + this%numRoot() + this%numEar())
         Density = this%panicle(n)%Density(ElementID)
         return
      else
         print *, "[ERROR] >> getDensityMaize >> Invalid DomainID", DomainID
         return
      end if

   end function
! ################################################################

! ################################################################
   subroutine getVerticesMaize(this, Vertices, VertexIDs)
      class(Maize_), intent(inout) :: this
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

      if (allocated(this%ear)) then
         !$OMP parallel do private(nn_range,this_vertices,this_vertexIDs)
         do i = 1, size(this%ear)
            if (.not. this%ear(i)%femdomain%Mesh%empty()) then
               call this%ear(i)%femdomain%getVertices(this_vertices, this_vertexIDs)
               nn_range = this%nn_range("ear", i)
               Vertices(3*(nn_range(1) - 1) + 1:3*(nn_range(1) - 1) + size(this_vertices)) = this_vertices(:)
               VertexIDs(nn_range(1):nn_range(1) - 1 + size(this_vertexIDs)) = nn_range(1) - 1 + this_vertexIDs(:)
               deallocate (this_vertices)
               deallocate (this_vertexIDs)
            end if
         end do
         !$OMP end parallel do
      end if

      if (allocated(this%panicle)) then
         !$OMP parallel do private(nn_range,this_vertices,this_vertexIDs)
         do i = 1, size(this%panicle)
            if (.not. this%panicle(i)%femdomain%Mesh%empty()) then
               call this%panicle(i)%femdomain%getVertices(this_vertices, this_vertexIDs)
               nn_range = this%nn_range("panicle", i)
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

   end subroutine getVerticesMaize
! ################################################################

! #############################################################

   subroutine deformMaize(this, displacement)
      class(Maize_), target, intent(inout) :: this

      ! deform Maize by displacement
      real(real64), optional, intent(in) :: displacement(:)

      type(FEMDomainp_), allocatable :: domainsp(:)
      integer(int32), allocatable :: contactList(:, :)
      integer(int32) :: i, j, numDomain, stemDomain, leafDomain, rootDomain, from, to, nd, nn
      real(real64) :: penalty, GLevel

      if (present(displacement)) then
         if (size(displacement) /= this%nn()*3) then
            print *, "ERROR :: deformMaize >> size(displacement) should be (this%numStem() + this%numLeaf() + this%numRoot())*3"
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

         if (allocated(this%ear)) then
            do i = 1, size(this%ear)
               if (.not. this%ear(i)%femdomain%Mesh%empty()) then
                  nn = this%ear(i)%femdomain%nn()
                  nd = this%ear(i)%femdomain%nd()

                  to = from + this%ear(i)%femdomain%nn()*this%ear(i)%femdomain%nd() - 1

                  this%ear(i)%femdomain%mesh%nodcoord(:, :) = &
                     this%ear(i)%femdomain%mesh%nodcoord(:, :) + &
                     reshape(displacement(from:to), nn, nd)

                  from = to + 1
               end if
            end do
         end if

         if (allocated(this%panicle)) then
            do i = 1, size(this%panicle)
               if (.not. this%panicle(i)%femdomain%Mesh%empty()) then
                  nn = this%panicle(i)%femdomain%nn()
                  nd = this%panicle(i)%femdomain%nd()

                  to = from + this%panicle(i)%femdomain%nn()*this%panicle(i)%femdomain%nd() - 1

                  this%panicle(i)%femdomain%mesh%nodcoord(:, :) = &
                     this%panicle(i)%femdomain%mesh%nodcoord(:, :) + &
                     reshape(displacement(from:to), nn, nd)

                  from = to + 1
               end if
            end do
         end if

         return
      end if

   end subroutine
! #####################################################################

   function getElementListMaize(this, x_min, x_max, y_min, y_max, z_min, z_max, debug) result(ElementList)
      class(Maize_), intent(inout) :: this
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

      if (allocated(this%Ear)) then
         do idx = 1, size(this%Ear)
            if (this%Ear(idx)%femdomain%empty()) cycle
            m = size(elem_idx)
            elem_idx = &
               elem_idx//this%Ear(idx)%femdomain%mesh%getElementList( &
               xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, zmin=z_min, zmax=z_max)
            obj_idx = obj_idx//idx*int(eyes(size(elem_idx) - m))
         end do

         if (do_debug) then
            print *, "[o] EAR"
         end if
      else
         if (do_debug) then
            print *, "NO EAR"
         end if
      end if
      n = size(obj_type)
      obj_type = obj_type//this%TYPE_EAR*int(eyes(size(elem_idx) - n))

      if (allocated(this%Panicle)) then
         do idx = 1, size(this%Panicle)
            if (this%Panicle(idx)%femdomain%empty()) cycle
            m = size(elem_idx)
            elem_idx = &
               elem_idx//this%Panicle(idx)%femdomain%mesh%getElementList( &
               xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, zmin=z_min, zmax=z_max)
            obj_idx = obj_idx//idx*int(eyes(size(elem_idx) - m))
         end do

         if (do_debug) then
            print *, "[o] PANICLE"
         end if
      else
         if (do_debug) then
            print *, "NO PANICLE"
         end if
      end if
      n = size(obj_type)
      obj_type = obj_type//this%TYPE_PANICLE*int(eyes(size(elem_idx) - n))

      ElementList = zeros(size(elem_idx), 3)
      ElementList(:, 1) = obj_type
      ElementList(:, 2) = obj_idx
      ElementList(:, 3) = elem_idx

   end function
! ###########################################################################

! ################################################################
   function getDisplacementMaize(this, ground_level, penalty, debug, EbOM_Algorithm) result(displacement)
      class(Maize_), target, intent(inout) :: this
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

      integer(int32) :: myEarID, myPanicleID

      EbOM_Algorithm_id = FEMDomain_Overset_GPP
      if (present(EbOM_Algorithm)) then
         if (EbOM_Algorithm == "P2P") then
            EbOM_Algorithm_id = FEMDomain_Overset_P2P
         elseif (EbOM_Algorithm == "GPP") then
            EbOM_Algorithm_id = FEMDomain_Overset_P2P
         end if
      end if

      ! linear elasticity with infinitesimal strain theory
      n = this%numStem() + this%numLeaf() + this%numRoot() + this%numEar() + this%numPanicle()

      allocate (FEMDomainPointers(n))
      ! ORDER
      ! [STEM] => [LEAF] => [ROOT] => [Ear] => [PANICLE]
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

      if (allocated(this%Ear2stem)) then
         if (allocated(this%Ear) .and. allocated(this%stem)) then
            do myEarID = 1, size(this%Ear2stem, 1)
               do yourStemID = 1, size(this%Ear2stem, 2)
                  if (this%Ear2stem(myEarID, yourStemID) >= 1) then
                     ! connected
                     call this%Ear(myEarID)%femdomain%overset( &
                        FEMDomain=this%stem(yourStemID)%femdomain, &
                        DomainID=yourStemID, &
                        MyDomainID=this%numStem() + this%numLeaf() + this%numRoot() + myEarID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"
                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%Ear(myEarID)%femdomain, &
                        DomainID=this%numStem() + this%numLeaf() + this%numRoot() + myEarID, &
                        MyDomainID=yourStemID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"

                  end if
               end do
            end do
         end if
      end if

      if (allocated(this%Panicle2stem)) then
         if (allocated(this%Panicle) .and. allocated(this%stem)) then
            do myPanicleID = 1, size(this%Panicle2stem, 1)
               do yourStemID = 1, size(this%Panicle2stem, 2)
                  if (this%Panicle2stem(myPanicleID, yourStemID) >= 1) then
                     ! connected
                     call this%Panicle(myPanicleID)%femdomain%overset( &
                        FEMDomain=this%stem(yourStemID)%femdomain, &
                        DomainID=yourStemID, &
                        MyDomainID=this%numStem() + this%numLeaf() + this%numRoot() + &
                        this%numEar() + myPanicleID, &
                        algorithm=EbOM_Algorithm_id) ! or "P2P"

                     call this%stem(yourStemID)%femdomain%overset( &
                        FEMDomain=this%Panicle(myPanicleID)%femdomain, &
                        DomainID=this%numStem() + this%numLeaf() + this%numRoot() + &
                        this%numEar() + myPanicleID, &
                        MyDomainID=yourStemID, &
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

      call solver%init(NumDomain=this%numStem() + this%numLeaf() + this%numRoot() &
                       + this%numEar() + this%numPanicle())

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
   function getStressFieldMaize(this, displacement, i, j, option) result(StressField)
      class(Maize_), intent(inout) :: this
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

      if (allocated(this%Ear)) then
         do obj_idx = 1, size(this%Ear)
            if (this%Ear(obj_idx)%femdomain%mesh%empty()) cycle
            StressField = StressField// &
                          this%Ear(obj_idx)%femdomain%getElementCauchyStress( &
                          displacement=displacement(n:n + this%Ear(obj_idx)%femdomain%nn() &
                                                    *this%Ear(obj_idx)%femdomain%nd() - 1), &
                          E=this%Ear(obj_idx)%YoungModulus(:), &
                          v=this%Ear(obj_idx)%PoissonRatio(:), i=i, j=j, option=option)
            n = n + this%Ear(obj_idx)%femdomain%nn() &
                *this%Ear(obj_idx)%femdomain%nd()
         end do
      end if

      if (allocated(this%panicle)) then
         do obj_idx = 1, size(this%panicle)
            if (this%panicle(obj_idx)%femdomain%mesh%empty()) cycle
            StressField = StressField// &
                          this%panicle(obj_idx)%femdomain%getElementCauchyStress( &
                          displacement=displacement(n:n + this%panicle(obj_idx)%femdomain%nn() &
                                                    *this%panicle(obj_idx)%femdomain%nd() - 1), &
                          E=this%panicle(obj_idx)%YoungModulus(:), &
                          v=this%panicle(obj_idx)%PoissonRatio(:), i=i, j=j, option=option)
            n = n + this%panicle(obj_idx)%femdomain%nn() &
                *this%panicle(obj_idx)%femdomain%nd()
         end do
      end if

   end function
! ################################################################

   subroutine export_eigMaize(this, name, Frequency, ModeVectors, stress_type)
      class(Maize_), intent(inout) :: this
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
   subroutine resizeLeafMaize(this, LeafID, Length)
      class(Maize_), intent(inout) :: this
      integer(int32), intent(in) :: LeafID
      real(real64), intent(in) :: Length

      call this%leaf(leafID)%resize(Length=Length)
      call this%update()

   end subroutine
! ################################################################

end module MaizeClass
