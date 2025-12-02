module MeristemClass
   use, intrinsic :: iso_fortran_env
   use LeafClass
   implicit none

    !integer(int32),public :: PF_Meristem_SHAPE_CYLINDER     = 2
    !integer(int32),public :: PF_Meristem_SHAPE_HYPERBOLOID  = 3
   
   integer(int32),public :: PF_MERISTEM_TYPE_SHOOT = 0
   integer(int32),public :: PF_MERISTEM_TYPE_ROOT  = 1
    
   type :: Meristem_
      type(FEMDomain_)    ::  FEMDomain
      type(Stem_),allocatable :: stem(:)
      type(LeafSet_),allocatable :: leafset(:) ! petioles and leaves
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
      type(Meristem_),allocatable :: branch_meristem(:)

      ! meristem parameters
      integer(int32)  :: num_shoot_stem_node = 2
      real(real64) :: slight_overlap_epsilon = dble(1.0e-3)
      real(real64) :: top_meristem_aspect_ratio = 0.70d0 ! if length/width is greater than this value, internodes are divided.

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
      real(real64) :: L_limit ! threshold for internode division

      integer(int32) :: species = 1  ! 
      real(real64) :: leaf_directional_angle = 120.0d0 ! 
      real(real64) :: last_leaf_direction = 0.0d0

       ! <<<<<<<<<<<
      logical :: material_is_set

      type(Meristem_), pointer ::  pMeristem

      ! physiological factor
      real(real64) :: R_d = 1.0d0 ! 暗呼吸速度, mincro-mol/m-2/s
      real(real64) :: default_CarbonDiffusionCoefficient = 0.0010d0 ! ソースの拡散係数 mincro-mol/m^2/m/s

   contains
         procedure, public :: Init => initMeristem
         procedure, public :: rotate => rotateMeristem
         procedure,public :: update  => updateMeristem
         procedure,public :: grow_internode  => grow_internodeMeristem
         
         procedure,public :: meristem_division => meristem_division_MeristemC

         procedure,public :: set_branch => set_branch_MeristemC



         !procedure, pass :: change_length_or_width_Meristem
         !procedure, pass :: grow_by_pressure_Meristem
         !generic, public :: grow => grow_by_pressure_Meristem
         !generic, public :: change_length_or_width => change_length_or_width_Meristem

         procedure, public :: resize => resizeMeristem
         procedure, public :: move => moveMeristem
         procedure, public :: connect => connectMeristem

         ! check condition
         procedure, public :: empty => emptyMeristem
         procedure, public :: getCoordinate => getCoordinateMeristem
         procedure, public :: getLength => getLengthMeristem
         procedure, public :: getAngles => getAngles_MeristemClass
         procedure, public :: getWidth => getWidthMeristem
         procedure, public :: getVolume => getVolumeMeristem

         !procedure, public :: FullyExpanded => FullyExpandedMeristem
         procedure, public :: gmsh => gmshMeristem
         procedure, public :: msh => mshMeristem
         procedure, public :: vtk => vtkMeristem
         procedure, public :: stl => stlMeristem
         procedure, public :: ply => plyMeristem
         !procedure, public :: export => exportMeristem
         procedure, public :: getBiomass => getBiomassMeristem
         procedure, public :: getHeight => getHeightMeristem
         ! simulation
         procedure, public :: set_material => set_material_Meristem
         procedure, public :: sync => syncMeristem
         procedure, public :: nn => nnMeristem
         procedure, public :: ne => neMeristem
         procedure, public :: remove => removeMeristem
   end type

   interface operator(//)
      module procedure append_Meristem_thisect_vector
   end interface

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
   
   logical :: debug = .false.

   real(real64) :: K_L ! max length(m)
   real(real64) :: K_R ! max radius(m)
   real(real64) :: T_L ! Time constant for length (s)
   real(real64) :: T_R ! Time constant for radius (s)
   real(real64) :: L_limit  ! threshold length on subdivision of the top meristem

   !real(real64) :: K_pL ! maximum size of each petiole internodes (m) 
   !real(real64) :: K_pR ! maximum size of each petiole radius (m) 
   !real(real64) :: K_lL ! maximum length of each leaf  (m) 
   !real(real64) :: T_pL ! Delay time (time constant) of each petiole internodes (s)
   !real(real64) :: T_pR ! Delay time (time constant) of each petiole radius (s)
   !real(real64) :: T_lL ! Delay time (time constant) for length of each leaf (s)

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
   this%L_limit = params(5) ! threshold length on subdivision of the top meristem


   
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
         call this%stem(stem_idx)%init()
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
         call this%grow_internode(idx=stem_idx,params=params(:),dt=dt)
      enddo
      !do leafset_idx=1,size(this%leafset)
      !   call this%leafset(leafset_idx)%grow_peti_and_leaf(params=params(6:11),dt=dt)
      !enddo

      ! set leafsets
      if(allocated(this%leafset))then
         deallocate(this%leafset)
      endif
      allocate(this%leafset(size(this%stem)))
      this%leafset2stem = int(zeros(this%num_leafset_per_stem*size(this%stem),size(this%stem)))
      
      do stem_idx=1,size(this%stem) ! no leaves in last stem
         call this%grow_internode(idx=stem_idx,params=params,dt=dt)
         do i=1,this%num_leafset_per_stem
            n = this%num_leafset_per_stem*(stem_idx-1) + i
            this%last_leaf_direction = this%last_leaf_direction + this%leaf_directional_angle
            call this%leafset(n)%init( &
               num_leaf=this%num_leaf_per_leafset,&
               params=params(6:11),&
               species=this%species,&
               direction=radian(this%last_leaf_direction),&
               dt=dt)
            
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
   recursive subroutine rotateMeristem(this, x, y, z, reset)
      class(Meristem_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      logical, optional, intent(in) :: reset
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      if (present(reset)) then
         if (reset .eqv. .true.) then
            call this%femdomain%rotate(-this%rot_x, -this%rot_y, -this%rot_z)
            this%rot_x = 0.0d0
            this%rot_y = 0.0d0
            this%rot_z = 0.0d0
         end if
      end if

      origin1 = this%getCoordinate("A")
      call this%femdomain%rotate(x, y, z)
      this%rot_x = this%rot_x + input(default=0.0d0, option=x)
      this%rot_y = this%rot_y + input(default=0.0d0, option=y)
      this%rot_z = this%rot_z + input(default=0.0d0, option=z)
      origin2 = this%getCoordinate("A")
      disp = origin1
      disp(:) = origin1(:) - origin2(:)
      call this%femdomain%move(x=disp(1), y=disp(2), z=disp(3))

   end subroutine
! ########################################

! ########################################
   recursive subroutine moveMeristem(this, x, y, z, reset)
      class(Meristem_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      logical, optional, intent(in) :: reset
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      if (present(reset)) then
         if (reset .eqv. .true.) then
            call this%femdomain%move(-this%disp_x, -this%disp_y, -this%disp_z)
            this%disp_x = 0.0d0
            this%disp_y = 0.0d0
            this%disp_z = 0.0d0
         end if
      end if

      call this%femdomain%move(x, y, z)
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

         ! 20220701 this may be correct
         ret = this%femdomain%mesh%nodcoord(this%A_PointNodeID, :)
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
            if (allocated(this%stem)) then
               do i = 1, size(this%stem)
                  if (.not. this%stem(i)%femdomain%empty()) then
                     femdomain = femdomain + this%stem(i)%femdomain
                  end if
               end do
            end if
            
            if (allocated(this%leafset)) then
               do i = 1, size(this%leafset)
                  if(this%leafset(i)%is_empty())cycle
                  do j=1, size(this%leafset(i)%peti)
                     femdomain = femdomain + this%leafset(i)%peti(j)%femdomain
                  enddo
                  do j=1, size(this%leafset(i)%leaf)
                     femdomain = femdomain + this%leafset(i)%leaf(j)%femdomain
                  enddo
               end do
            end if
            



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
   function getBiomassMeristem(this) result(ret)
      class(Meristem_), intent(in) :: this
      real(real64) :: ret
      integer(int32) :: i, j

      ret = 0.0d0
      if (this%femdomain%mesh%empty()) then
         return
      end if

      do i = 1, this%femdomain%ne()
         ret = ret + this%femdomain%getVolume(elem=i)*this%drydensity(i)
      end do

   end function
! ####################################################################

! ########################################
   function emptyMeristem(this) result(Meristem_is_empty)
      class(Meristem_), intent(in) :: this
      logical :: Meristem_is_empty

      Meristem_is_empty = this%femdomain%empty()

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

   function nnMeristem(this) result(ret)
      class(Meristem_), intent(in) :: this
      integer(int32) :: ret

      ret = this%femdomain%nn()
   end function

   function neMeristem(this) result(ret)
      class(Meristem_), intent(in) :: this
      integer(int32) :: ret

      ret = this%femdomain%ne()
   end function

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
      real(real64) :: x_A(3), x_B(3), diff(3), error, last_error, mgn, overset_m, error_tol,original_position(1:3),disp(1:3)
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


      ! offset displacement
      !if ( norm(this%stem(1)%femdomain%mesh%nodcoord(1,3) - original_position) > error_tol)then
      !   disp = this%stem(1)%femdomain%mesh%nodcoord(1,3) - original_position
      !   call this%move(x=-disp(1),y=-disp(2),z=-disp(3))
      !endif

   end subroutine
! ########################################

subroutine grow_merisemClass(this,dt,temperature,reproductive_stage)
   class(Meristem_),intent(inout) :: this
   real(real64),intent(in) :: dt, temperature
   logical,optional,intent(in) :: reproductive_stage
   logical :: is_in_reproductive_stage

   is_in_reproductive_stage = input(default=.false.,option=reproductive_stage)

!   do j_j=1,size(this%stem)
!      call this%grow_internode(idx=j_j,no_division=is_in_reproductive_stage)
!   enddo

end subroutine



! ########################################
subroutine grow_internodeMeristem(this,idx,params,dt,no_division)
   class(Meristem_),intent(inout) :: this
   integer(int32),intent(in)   :: idx
   real(real64),intent(in) :: dt,params(:)
   logical,optional,intent(in) :: no_division

   real(real64) :: K_L ! 2.00d0 ! max length
   real(real64) :: K_R ! 0.40d0 ! max radius
   real(real64) :: T_L ! 60.0d0*60.0d0*24.0d0*20.0d0 ! Time constant for length
   real(real64) :: T_R ! 60.0d0*60.0d0*24.0d0*10.0d0 ! Time constant for radius
   real(real64) :: L_limit ! 0.50d0 ! threshold length on subdivision of the top meristem
   
   real(real64) :: ex_ratio(1:3),max_width,t
   integer(int32) :: leafset_idx,i,peti_idx,leaf_idx
   

   K_L = params(1)
   K_R = params(2)
   T_L = params(3)
   T_R = params(4)
   L_limit = params(5)

   t = this%stem(idx)%my_time + dt
   call this%stem(idx)%resize( &
         x = 2.0d0*K_R*(1.0d0 - exp(-t/T_R)) ,&
         y = 2.0d0*K_R*(1.0d0 - exp(-t/T_R)) ,&
         z = K_L*(1.0d0 - exp(-t/T_L))  &
      )

   
   
   

   if(.not. input(default=.false.,option=no_division))then
      if(idx==size(this%stem) )then
         if(this%stem(idx)%getLength() >= L_limit)then
            call this%meristem_division(params=params,dt=dt)
         endif

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

   call this%update()


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
   integer(int32),allocatable :: s2s_buf(:,:),leafset2stem_buf(:,:)
   integer(int32) :: old_leafset_size,i,stem_idx
   real(real64) :: length, radius
   

   idx = size(this%stem)
   allocate(new_stem(1))
   new_stem(1)%CROSS_SECTION_SHAPE = this%stem(idx)%CROSS_SECTION_SHAPE 

   length = this%stem(idx)%getlength()
   radius = this%stem(idx)%getWidth()/2.0d0

   ! divide
   call this%stem(idx)%resize( &
         z=length*(1.0d0-this%top_meristem_aspect_ratio) &
      )

   ! create new internode
   ! with the half size
   call new_stem(1)%init()
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
      this%leafset2stem = zeros( size(leafset2stem_buf,1)+this%num_leafset_per_stem,&
          size(leafset2stem_buf,2)+this%num_leafset_per_stem)
      this%leafset2stem(1:size(leafset2stem_buf,1),1:size(leafset2stem_buf,2)) &
         = leafset2stem_buf(:,:)
   else
      allocate(this%leafset2stem(this%num_leafset_per_stem,size(this%stem)))
   endif

   !>(2) create new leafsets
   allocate(new_leafset(this%num_leafset_per_stem))
   stem_idx = size(this%stem)
   do i=1,size(new_leafset)
      call new_leafset(i)%init(&
         num_leaf=this%num_leaf_per_leafset,&
         params=params(6:11),&
         species=this%species,&
         direction = radian(this%last_leaf_direction + this%leaf_directional_angle), &
         dt=dt)
      this%last_leaf_direction = this%last_leaf_direction + this%leaf_directional_angle
      
   enddo
   
   old_leafset_size = size(this%leafset)
   this%leafset = this%leafset // new_leafset

   do i=old_leafset_size+1,size(this%leafset)
      print *, allocated(this%leafset), size(this%leafset),old_leafset_size+1, size(this%leafset)
      print *, i
      if(this%leafset(i)%is_empty())cycle
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
   
   height = this%stem(size(this%stem))%femdomain%zmax() - this%stem(1)%femdomain%zmin()

end function
! ######################################################


! ######################################################
function getVolumeMeristem(this) result(vol)
   class(Meristem_),intent(in) :: this
   real(real64) :: vol
   integer(int32) :: i,j
   
   vol = 0.0d0
   do i=1,size(this%stem)
      vol = vol + this%stem(i)%getVolume()
   enddo
   do i=1,size(this%leafset)
      if(this%leafset(i)%is_empty())cycle
      do j=1,size(this%leafset(i)%peti)
         vol = vol + this%leafset(i)%peti(j)%getVolume()
      enddo
      do j=1,size(this%leafset(i)%leaf)
         vol = vol + this%leafset(i)%leaf(j)%getVolume()
      enddo
   enddo

end function
! ######################################################


! ######################################################
subroutine set_branch_MeristemC(this,stem_idx)
   class(Meristem_),intent(inout) :: this
   integer(int32),intent(in) :: stem_idx

   ! Creating branch at a leaf axil of a given stem idx.
   ! The brancing angle is a half of the petiole angle.
   


end subroutine
! ######################################################

end module