module SoilClass
   use, intrinsic :: iso_fortran_env
   use fem
   use FEMDomainClass
   use FertilizerClass
   use BoringClass
   use DigitalElevationModelClass
   use EarthClass
   implicit none

   ! N-value to Vs
   !今井恒夫、殿内啓司：N 値と S 波速度の関係およびその
   !利用例、基礎工、Vol.16、No.6、pp.70～76、1982
   integer(int32), parameter :: PF_N2Vs_Imai = 1
   !太田裕、後藤典俊：S 波速度を他の土質諸指標から推定
   integer(int32), parameter :: PF_N2Vs_OhtaGoto = 2
   !する試み、物理探鉱、第29巻、第4号、pp.31～41、1976
   !公益社団法人 日本道路協会：道路橋示方書・同解説、
   !Ⅴ耐震設計編、pp.69、2017
   integer(int32), parameter :: PF_N2Vs_JAPANROAD_1 = 3
   integer(int32), parameter :: PF_N2Vs_JAPANROAD_2 = 4


   type :: Soil_
      type(FEMDomain_) :: FEMDomain
      type(Boring_), allocatable :: Boring(:)
      type(LinearSolver_) :: solver

      real(real64), allocatable :: disp(:, :)
      ! soil parameters
      real(real64), allocatable :: YoungModulus(:)
      real(real64), allocatable :: PoissonRatio(:)
      real(real64), allocatable :: Density(:)
      real(real64), allocatable :: VoidRatio(:)
      real(real64), allocatable :: Cohesion(:)
      real(real64), allocatable :: FrictionAngle(:)

      real(real64) :: depth
      real(real64) :: length
      real(real64) :: width
      integer(int32) :: num_x
      integer(int32) :: num_y
      integer(int32) :: num_z
      real(real64) :: x, y, z ! center coordinate

      ! soil property
      character(:),allocatable :: config
      real(real64),allocatable :: lambda(:)
      real(real64),allocatable :: kappa(:)
      real(real64),allocatable :: e0(:)
      real(real64),allocatable :: P0(:)
      real(real64),allocatable :: Py(:)

      ! ================
      ! Nutorient
      !------------
      real(real64) :: N_kg = 0.0d0
      real(real64) :: P_kg = 0.0d0
      real(real64) :: K_kg = 0.0d0
      real(real64) :: Ca_kg = 0.0d0
      real(real64) :: Mg_kg = 0.0d0
      real(real64) :: S_kg = 0.0d0
      !------------
      real(real64) :: Fe_kg = 0.0d0
      real(real64) :: Mn_kg = 0.0d0
      real(real64) :: B_kg = 0.0d0
      real(real64) :: Zn_kg = 0.0d0
      real(real64) :: Mo_kg = 0.0d0
      real(real64) :: Cu_kg = 0.0d0
      real(real64) :: Cl_kg = 0.0d0
      ! ================

      ! ================
      ! Soil phyisical parameters
      real(real64) :: C_N_ratio
      real(real64) :: EC
      ! ================

      ! ================
      
      ! ================

   contains
      procedure, pass :: initSoil
      procedure, pass :: init_by_latlon_Soil
      generic :: init => initSoil, init_by_latlon_Soil
      generic :: create => initSoil, init_by_latlon_Soil
      generic :: new => initSoil, init_by_latlon_Soil

      procedure,public :: nn => nn_Soil
      procedure,public :: ne => ne_Soil
      procedure,public :: nne => nne_Soil
      procedure,public :: nd => nd_Soil

      procedure :: import => importSoil
      procedure :: resize => resizeSoil
      procedure :: rotate => rotateSoil
      procedure :: move => moveSoil
      procedure :: gmsh => gmshSoil
      procedure :: msh => mshSoil
      procedure :: vtk => vtkSoil
      procedure :: deform => deformSoil
      procedure :: PreFlightCheck => PreFlightCheckSoil
      procedure :: fertilize => fertilizeSoil
      procedure :: diagnosis => diagnosisSoil
      procedure :: export => exportSoil
      procedure :: getNvalue => getNvalueSoil
      procedure :: convertNvalue2Vs => convertNvalue2VsSoil

      procedure,public :: GL => Ground_level_of_Soil

      ! Soil's emperical mechanical parameters & models
      procedure,public :: setSoilType => setSoilType_SoilClass
      procedure,public :: JGS_coeff_subgrade_react => JGS_coeff_subgrade_react_Soil
      procedure,public :: JGS_subgrade_displacement => JGS_subgrade_displacement_Soil
      procedure,public :: raining => raining_Soil
      procedure,public :: cultivate => cultivate_Soil
      procedure,public :: updateVoidRatio => updateVoidRatio_Soil
      ! 
      ! MPI
      procedure :: sync => syncSoil
   end type

contains
! ################################################################

! ################################################################
   subroutine importSoil(obj, name, boring, dem, x_num, y_num, z_num, radius, depth)
      class(Soil_), intent(inout)::obj
      character(*), optional, intent(in) :: name
      type(Boring_), optional, intent(in) :: Boring(:)
      type(DigitalElevationModel_), optional, intent(in) :: dem
      integer(int32), optional, intent(in) :: x_num, y_num, z_num
      real(real64), optional, intent(in) :: radius, depth
      real(real64) :: radius_val, xlen, ylen, zlen, def_interval
      real(real64) :: r_tr, original_z, new_z, depth_val, bottom_z
      integer(int32) :: xnum, ynum, znum, DOF, i, j

      !depth_val = input(default=-1.0d0,option=-abs(depth)

      ! Boring core sampling data
      if (present(Boring)) then
         obj%Boring = Boring
      end if

      if (present(dem)) then
         DOF = dem%NumberOfPoint()
         xnum = input(default=int(dble(DOF)**(1.0d0/3.0d0)), option=x_num)
         ynum = input(default=int(dble(DOF)**(1.0d0/3.0d0)), option=y_num)
         znum = input(default=int(dble(DOF)**(1.0d0/3.0d0)), option=z_num)

         xlen = maxval(dem%x) - minval(dem%x)
         ylen = maxval(dem%y) - minval(dem%y)
         zlen = maxval(dem%z) - minval(dem%z)
         ! create mesh
         call obj%FEMDomain%create("Cube3D", x_num=xnum, y_num=ynum, z_num=znum)
         call obj%FEMDomain%resize(x=xlen)
         call obj%FEMDomain%resize(y=ylen)
         call obj%FEMDomain%resize(z=minval(dem%z))

         call obj%FEMDomain%move(x=minval(dem%x))
         call obj%FEMDomain%move(y=minval(dem%y))
         !call obj%FEMDomain%move(z=minval(dem%z)  )

         ! modify mesh
         def_interval = maxval([xlen/dble(xnum), ylen/dble(ynum), zlen/dble(znum)])
         radius_val = input(default=def_interval, option=radius)
         do i = 1, dem%NumberOfPoint()
            do j = obj%femdomain%nn() - 2*(xnum + 1)*(ynum + 1), obj%femdomain%nn()
               r_tr = (dem%x(i) - obj%femdomain%mesh%nodcoord(j, 1))**2
               r_tr = r_tr + (dem%y(i) - obj%femdomain%mesh%nodcoord(j, 2))**2
               r_tr = sqrt(r_tr)
               if (r_tr <= radius_val) then
                  ! change coordinate
                  !original_z = obj%femdomain%mesh%nodcoord(j,3)
                  ! height original_z:->
                  !new_z = original_z/zlen * dem%z(i) * &
                  !    (radius_val - r_tr)*(radius_val - r_tr)/radius_val/radius_val
                  obj%femdomain%mesh%nodcoord(j, 3) = dem%z(i)
               end if
            end do
         end do

         do i = 1, (xnum + 1)*(ynum + 1)
            do j = 1, znum
               obj%femdomain%mesh%nodcoord((xnum + 1)*(ynum + 1)*(j - 1) + i, 3) = &
                  obj%femdomain%mesh%nodcoord((xnum + 1)*(ynum + 1)*znum + i, 3)*dble(j)/dble(znum + 1)
            end do
         end do
         bottom_z = minval(obj%femdomain%mesh%nodcoord(:, 3))
         obj%femdomain%mesh%nodcoord(1:(xnum + 1)*(ynum + 1), 3) = bottom_z

         obj%YoungModulus = zeros(obj%femdomain%ne())
         obj%PoissonRatio = zeros(obj%femdomain%ne())
         obj%Density = zeros(obj%femdomain%ne())
         obj%VoidRatio = zeros(obj%femdomain%ne())
         obj%Cohesion = zeros(obj%femdomain%ne())
         obj%FrictionAngle = zeros(obj%femdomain%ne())
      end if

      if (present(name)) then
         if (index(name, ".vtk") /= 0) then
            call obj%femdomain%import(file=name)
            obj%YoungModulus = zeros(obj%femdomain%ne())
            obj%PoissonRatio = zeros(obj%femdomain%ne())
            obj%Density = zeros(obj%femdomain%ne())
            obj%VoidRatio = zeros(obj%femdomain%ne())
            obj%Cohesion = zeros(obj%femdomain%ne())
            obj%FrictionAngle = zeros(obj%femdomain%ne())
         else
            print *, "ERROR :: importSoil >> only vtk ASCII format is readable."
         end if
      end if

   end subroutine
! ################################################################
   subroutine init_by_latlon_Soil(this, latitude, longitude, depth, division)
      class(Soil_), intent(inout)::this
      real(real64), intent(in) :: latitude(1:4), longitude(1:4), depth
      integer(int32), intent(in) :: division(1:3)
      real(real64), allocatable :: xy(:, :), XXYY(:, :), dxdxi(:, :), F(:, :), coord(:)
      real(real64) :: r, theta, alpha, beta, theta_1, theta_2, theta_3, rot_mat_1(2, 2), &
                      rot_mat_2(2, 2)
      integer(int32) :: elemnod(1, 4), i, j
      type(Math_) :: math

      xy = zeros(size(latitude), 2)
      do i = 2, size(latitude)
         xy(i, 1:2) = to_Cartesian(longitude=longitude(i), latitude=latitude(i), origin=[latitude(1), longitude(1)])
      end do

      call this%femdomain%create(meshtype="Cube3D", x_num=division(1), &
                                 y_num=division(2), z_num=division(3))
      call this%femdomain%resize(x=1.0d0, y=1.0d0, z=abs(depth))
      call this%femdomain%move(x=this%femdomain%xmin(), y=this%femdomain%ymin(), z=-abs(depth))

      coord = zeros(2)
      XXYY = zeros(4, 2)

      XXYY(1, 1:2) = [0.0d0, 0.0d0]
      XXYY(2, 1:2) = [1.0d0, 0.0d0]
      XXYY(3, 1:2) = [1.0d0, 1.0d0]
      XXYY(4, 1:2) = [0.0d0, 1.0d0]

      rot_mat_1(1:2, 1) = xy(2, 1:2)
      rot_mat_1(1:2, 2) = xy(3, 1:2) - xy(2, 1:2)

      rot_mat_2(1:2, 1) = xy(3, 1:2) - xy(4, 1:2)
      rot_mat_2(1:2, 2) = xy(4, 1:2)

      do i = 1, this%femdomain%nn()
         coord = this%femdomain%mesh%nodcoord(i, 1:2)
         r = norm(coord)
         if (coord(1) == 0.0d0) then
            theta = Math%PI/2.0d0
         else
            theta = atan(coord(2)/coord(1))
         end if

         if (theta <= Math%PI/4.0d0) then
            this%femdomain%mesh%nodcoord(i, 1:2) = matmul(rot_mat_1, coord)
         else
            this%femdomain%mesh%nodcoord(i, 1:2) = matmul(rot_mat_2, coord)
         end if
      end do

   end subroutine

! ################################################################
   subroutine initSoil(obj, config, x_num, y_num, z_num)
      class(Soil_), intent(inout)::obj
      character(*), optional, intent(in) :: config
      integer(int32), optional, intent(in) :: x_num, y_num, z_num
      character(:), allocatable :: fn, conf, line
      real(real64) :: MaxThickness, Maxwidth, loc(3), vec(3), rot(3), zaxis(3), meshloc(3), meshvec(3)
      integer(int32) :: i, j, k, blcount, id, rmc, n, node_id, node_id2, elemid
      type(IO_) :: soilconf

      obj%YoungModulus = zeros(obj%femdomain%ne())
      obj%PoissonRatio = zeros(obj%femdomain%ne())
      obj%Density = zeros(obj%femdomain%ne())
      obj%VoidRatio = zeros(obj%femdomain%ne())
      obj%Cohesion = zeros(obj%femdomain%ne())
      obj%FrictionAngle = zeros(obj%femdomain%ne())

      ! 節を生成するためのスクリプトを開く
      if (.not. present(config) .or. index(config, ".json") == 0) then
         ! デフォルトの設定を生成
         print *, "New Soil-configuration >> soilconfig.json"
         call soilconf%open("soilconfig.json")
         write (soilconf%fh, *) '{'
         write (soilconf%fh, *) '   "type": "soil",'
         write (soilconf%fh, *) '   "length": 1.00,'
         write (soilconf%fh, *) '   "width" : 1.00,'
         write (soilconf%fh, *) '   "depth" : 0.40,'
         write (soilconf%fh, *) '   "num_x": 10,'
         write (soilconf%fh, *) '   "num_y": 10,'
         write (soilconf%fh, *) '   "num_z":  4'
         write (soilconf%fh, *) '}'
         conf = "soilconfig.json"
         call soilconf%close()
      else
         conf = config
      end if

      call soilconf%open(conf)
      blcount = 0
      do
         read (soilconf%fh, '(a)') line
         print *, line
         if (adjustl(line) == "{") then
            blcount = 1
            cycle
         end if
         if (adjustl(line) == "}") then
            exit
         end if

         if (blcount == 1) then

            if (index(line, "type") /= 0 .and. index(line, "soil") == 0) then
               print *, "ERROR: This config-file is not for Soil"
               return
            end if

            if (index(line, "length") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) obj%length
            end if

            if (index(line, "width") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) obj%width
            end if

            if (index(line, "depth") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) obj%depth
            end if

            if (index(line, "num_y") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) obj%num_y
            end if

            if (index(line, "num_z") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) obj%num_z
            end if

            if (index(line, "num_x") /= 0) then
               ! 生育ステージ
               rmc = index(line, ",")
               ! カンマがあれば除く
               if (rmc /= 0) then
                  line(rmc:rmc) = " "
               end if
               id = index(line, ":")
               read (line(id + 1:), *) obj%num_x
            end if

            cycle

         end if

      end do
      call soilconf%close()

      if (present(x_num)) then
         obj%num_x = x_num
      end if

      if (present(y_num)) then
         obj%num_y = y_num
      end if

      if (present(z_num)) then
         obj%num_z = z_num
      end if

      call obj%FEMdomain%create(meshtype="rectangular3D", x_num=obj%num_x, &
                                y_num=obj%num_y, z_num=obj%num_z, &
                                x_len=obj%length, y_len=obj%width, z_len=obj%depth)

      call obj%femdomain%move(x=-obj%length/2.0d0, &
                              y=-obj%width/2.0d0, z=-obj%depth)

   end subroutine
! ################################################################

! ################################################################
   subroutine fertilizeSoil(obj, Fertilizer, N_kg, P_kg, K_kg, Ca_kg, Mg_kg, S_kg, Fe_kg, &
                            Mn_kg, B_kg, Zn_kg, Mo_kg, Cu_kg, Cl_kg)

      class(Soil_), intent(inout)::obj
      type(Fertilizer_), optional, intent(in) :: Fertilizer

      ! ================
      real(real64), optional, intent(in) :: N_kg
      real(real64), optional, intent(in) :: P_kg
      real(real64), optional, intent(in) :: K_kg
      real(real64), optional, intent(in) :: Ca_kg
      real(real64), optional, intent(in) :: Mg_kg
      real(real64), optional, intent(in) :: S_kg
      ! ================
      real(real64), optional, intent(in) :: Fe_kg
      real(real64), optional, intent(in) :: Mn_kg
      real(real64), optional, intent(in) :: B_kg
      real(real64), optional, intent(in) :: Zn_kg
      real(real64), optional, intent(in) :: Mo_kg
      real(real64), optional, intent(in) :: Cu_kg
      real(real64), optional, intent(in) :: Cl_kg
      ! ================

      if (present(Fertilizer)) then
         obj%N_kg = obj%N_kg + Fertilizer%N_kg
         obj%P_kg = obj%P_kg + Fertilizer%P_kg
         obj%K_kg = obj%K_kg + Fertilizer%K_kg
         obj%Ca_kg = obj%Ca_kg + Fertilizer%Ca_kg
         obj%Mg_kg = obj%Mg_kg + Fertilizer%Mg_kg
         obj%S_kg = obj%S_kg + Fertilizer%S_kg
         obj%Fe_kg = obj%Fe_kg + Fertilizer%Fe_kg
         obj%Mn_kg = obj%Mn_kg + Fertilizer%Mn_kg
         obj%B_kg = obj%B_kg + Fertilizer%B_kg
         obj%Zn_kg = obj%Zn_kg + Fertilizer%Zn_kg
         obj%Mo_kg = obj%Mo_kg + Fertilizer%Mo_kg
         obj%Cu_kg = obj%Cu_kg + Fertilizer%Cu_kg
         obj%Cl_kg = obj%Cl_kg + Fertilizer%Cl_kg
         return
      end if

      obj%N_kg = input(default=0.0d0, option=N_kg)
      obj%P_kg = input(default=0.0d0, option=P_kg)
      obj%K_kg = input(default=0.0d0, option=K_kg)
      obj%Ca_kg = input(default=0.0d0, option=Ca_kg)
      obj%Mg_kg = input(default=0.0d0, option=Mg_kg)
      obj%S_kg = input(default=0.0d0, option=S_kg)
      obj%Fe_kg = input(default=0.0d0, option=Fe_kg)
      obj%Mn_kg = input(default=0.0d0, option=Mn_kg)
      obj%B_kg = input(default=0.0d0, option=B_kg)
      obj%Zn_kg = input(default=0.0d0, option=Zn_kg)
      obj%Mo_kg = input(default=0.0d0, option=Mo_kg)
      obj%Cu_kg = input(default=0.0d0, option=Cu_kg)
      obj%Cl_kg = input(default=0.0d0, option=Cl_kg)

   end subroutine
! ################################################################

! ################################################################
   subroutine exportSoil(obj, FileName, format, objID)
      class(Soil_), intent(inout)::obj
      integer(int32), optional, intent(inout) :: objID
      character(*), intent(in)::FileName
      character(*), optional, intent(in) :: format

      if (present(format)) then
         if (format == ".geo" .or. format == "geo") then
            open (15, file=FileName)
            write (15, '(A)') "//+"
            write (15, '(A)') 'SetFactory("OpenCASCADE");'
            write (15, *) "Box(", input(default=1, option=objID), ") = {", &
               obj%x, ",", obj%y, ",", obj%z, ",", &
               obj%width, ",", obj%length, ",", obj%depth, "};"
            close (15)
            objID = objID + 1
         end if
      end if
   end subroutine
! ################################################################

   subroutine diagnosisSoil(obj, FileName)
      class(Soil_), intent(inout) :: obj
      character(*), optional, intent(in)::FileName

      print *, "======================="
      print *, "Soil diagnosis"
      print *, "-----------------------"
      print *, "Total area ", adjustl(fstring(obj%width*obj%length))//" (cm^2)"
      print *, "Total area ", adjustl(fstring(obj%width/100.0d0*obj%length/100.0d0))//" (m^2)"
      print *, "Total area ", adjustl(fstring(obj%width/100.0d0*obj%length/100.0d0/100.0d0))//" (a)"
      print *, "Total area ", adjustl(fstring(obj%width/100.0d0*obj%length/100.0d0/100.0d0/100.0d0))//" (ha)"
      print *, "Total N  ", adjustl(fstring(obj%N_kg))//" (kg)"
      print *, "Total P  ", adjustl(fstring(obj%P_kg))//" (kg)"
      print *, "Total K  ", adjustl(fstring(obj%K_kg))//" (kg)"
      print *, "Total Ca ", adjustl(fstring(obj%Ca_kg))//" (kg)"
      print *, "Total Mg ", adjustl(fstring(obj%Mg_kg))//" (kg)"
      print *, "Total S  ", adjustl(fstring(obj%S_kg))//" (kg)"
      print *, "Total Fe ", adjustl(fstring(obj%Fe_kg))//" (kg)"
      print *, "Total Mn ", adjustl(fstring(obj%Mn_kg))//" (kg)"
      print *, "Total B  ", adjustl(fstring(obj%B_kg))//" (kg)"
      print *, "Total Zn ", adjustl(fstring(obj%Zn_kg))//" (kg)"
      print *, "Total Mo ", adjustl(fstring(obj%Mo_kg))//" (kg)"
      print *, "Total Cu ", adjustl(fstring(obj%Cu_kg))//" (kg)"
      print *, "Total Cl ", adjustl(fstring(obj%Cl_kg))//" (kg)"
      print *, "-----------------------"
      print *, "Total N  ", adjustl(fstring(obj%N_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total P  ", adjustl(fstring(obj%P_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total K  ", adjustl(fstring(obj%K_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total Ca ", adjustl(fstring(obj%Ca_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total Mg ", adjustl(fstring(obj%Mg_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total S  ", adjustl(fstring(obj%S_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total Fe ", adjustl(fstring(obj%Fe_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total Mn ", adjustl(fstring(obj%Mn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total B  ", adjustl(fstring(obj%B_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total Zn ", adjustl(fstring(obj%Zn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total Mo ", adjustl(fstring(obj%Mo_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total Cu ", adjustl(fstring(obj%Cu_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "Total Cl ", adjustl(fstring(obj%Cl_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0))//" (kg/10a)"
      print *, "-----------------------"
      print *, "Total N  ", adjustl(fstring(obj%N_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total P  ", adjustl(fstring(obj%P_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total K  ", adjustl(fstring(obj%K_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total Ca ", adjustl(fstring(obj%Ca_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total Mg ", adjustl(fstring(obj%Mg_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total S  ", adjustl(fstring(obj%S_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total Fe ", adjustl(fstring(obj%Fe_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total Mn ", adjustl(fstring(obj%Mn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total B  ", adjustl(fstring(obj%B_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total Zn ", adjustl(fstring(obj%Zn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total Mo ", adjustl(fstring(obj%Mo_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total Cu ", adjustl(fstring(obj%Cu_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "Total Cl ", adjustl(fstring(obj%Cl_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0))//" (kg/ha)"
      print *, "======================="

      if (present(FileName)) then
         open (16, file=FileName)

         write (16, *) "======================="
         write (16, *) "Soil diagnosis"
         write (16, *) "-----------------------"
         write (16, *) "Total N  (kg)", obj%N_kg
         write (16, *) "Total P  (kg)", obj%P_kg
         write (16, *) "Total K  (kg)", obj%K_kg
         write (16, *) "Total Ca (kg)", obj%Ca_kg
         write (16, *) "Total Mg (kg)", obj%Mg_kg
         write (16, *) "Total S  (kg)", obj%S_kg
         write (16, *) "Total Fe (kg)", obj%Fe_kg
         write (16, *) "Total Mn (kg)", obj%Mn_kg
         write (16, *) "Total B  (kg)", obj%B_kg
         write (16, *) "Total Zn (kg)", obj%Zn_kg
         write (16, *) "Total Mo (kg)", obj%Mo_kg
         write (16, *) "Total Cu (kg)", obj%Cu_kg
         write (16, *) "Total Cl (kg)", obj%Cl_kg
         write (16, *) "-----------------------"
         write (16, *) "Total N  (kg/10a)", obj%N_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total P  (kg/10a)", obj%P_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total K  (kg/10a)", obj%K_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total Ca (kg/10a)", obj%Ca_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total Mg (kg/10a)", obj%Mg_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total S  (kg/10a)", obj%S_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total Fe (kg/10a)", obj%Fe_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total Mn (kg/10a)", obj%Mn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total B  (kg/10a)", obj%B_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total Zn (kg/10a)", obj%Zn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total Mo (kg/10a)", obj%Mo_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total Cu (kg/10a)", obj%Cu_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "Total Cl (kg/10a)", obj%Cl_kg/(obj%width/100.0d0)/(obj%length/100.0d0)
         write (16, *) "-----------------------"
         write (16, *) "Total N  (kg/ha)", obj%N_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total P  (kg/ha)", obj%P_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total K  (kg/ha)", obj%K_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total Ca (kg/ha)", obj%Ca_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total Mg (kg/ha)", obj%Mg_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total S  (kg/ha)", obj%S_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total Fe (kg/ha)", obj%Fe_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total Mn (kg/ha)", obj%Mn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total B  (kg/ha)", obj%B_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total Zn (kg/ha)", obj%Zn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total Mo (kg/ha)", obj%Mo_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total Cu (kg/ha)", obj%Cu_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "Total Cl (kg/ha)", obj%Cl_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0
         write (16, *) "======================="
         close (16)
      end if
   end subroutine

! ########################################
   subroutine gmshSoil(obj, name)
      class(Soil_), intent(inout) :: obj
      character(*), intent(in) :: name

      call obj%femdomain%gmsh(Name=name)

   end subroutine
! ########################################

! ########################################
   subroutine vtkSoil(obj, name, scalar, vector, tensor, field, ElementType)
      class(Soil_), intent(inout) :: obj
      character(*), intent(in) :: name
      character(*), optional, intent(in) :: field
      real(real64), optional, intent(in) :: scalar(:), vector(:, :), tensor(:, :, :)
      integer(int32), optional, intent(in) :: ElementType

      call obj%femdomain%vtk(name=name, scalar=scalar, vector=vector, tensor=tensor, &
                             field=field, ElementType=ElementType)

   end subroutine
! ########################################

! ########################################
   subroutine resizeSoil(obj, x, y, z)
      class(Soil_), intent(inout) :: obj
      real(real64), optional, intent(in) :: x, y, z

      call obj%femdomain%resize(x=x, y=y, z=z)

   end subroutine
! ########################################

! ########################################
   subroutine rotateSoil(obj, x, y, z)
      class(Soil_), intent(inout) :: obj
      real(real64), optional, intent(in) :: x, y, z

      call obj%femdomain%rotate(x=x, y=y, z=z)

   end subroutine
! ########################################

! ########################################
   subroutine moveSoil(obj, x, y, z)
      class(Soil_), intent(inout) :: obj
      real(real64), optional, intent(in) :: x, y, z

      call obj%femdomain%move(x=x, y=y, z=z)

   end subroutine
! ########################################

! ########################################
   subroutine mshSoil(obj, name)
      class(Soil_), intent(inout) :: obj
      character(*), intent(in) :: name

      call obj%femdomain%msh(Name=name)

   end subroutine
! ########################################

! ########################################
   subroutine PreFlightCheckSoil(obj)
      class(Soil_), target, intent(inout) :: obj
      integer(int32) :: caution

      caution = 0
      print *, "PreFlightCheckList :: SoilClass >> started."
      if (obj%femdomain%mesh%empty()) then
         print *, "[CAUTION] Mesh is empty."
         caution = caution + 1
      else
         print *, "[ok] Mesh is ready."
      end if
      if (.not. allocated(obj%YoungModulus)) then
         print *, "[CAUTION] soil % YoungModulus is not allocated."
         caution = caution + 1
      else
         print *, "[ok] soil % YoungModulus is allocated."
         if (minval(obj%YoungModulus) <= 0.0d0) then
            print *, "[CAUTION] soil % YoungModulus <= 0"
            caution = caution + 1
         end if
      end if
      if (.not. allocated(obj%PoissonRatio)) then
         print *, "[CAUTION] soil % PoissonRatio is not allocated."
         caution = caution + 1
      else
         print *, "[ok] soil % PoissonRatio is allocated."
         if (minval(obj%PoissonRatio) <= 0.0d0 .or. maxval(obj%PoissonRatio) > 1.0d0) then
            print *, "[CAUTION] soil % PoissonRatio <= 0 or soil % PoissonRatio > 1"
            caution = caution + 1
         end if
      end if
      if (.not. allocated(obj%Density)) then
         print *, "[CAUTION] soil % Density is not allocated."
         caution = caution + 1
      else
         print *, "[ok] soil % Density is allocated."
         if (minval(obj%Density) <= 0.0d0) then
            print *, "[CAUTION] soil % Density <= 0"
            caution = caution + 1
         end if
      end if
      if (.not. allocated(obj%VoidRatio)) then
         print *, "[CAUTION] soil % VoidRatio is not allocated."
         caution = caution + 1
      else
         print *, "[ok] soil % VoidRatio is allocated."
         if (minval(obj%VoidRatio) == 0.0d0) then
            print *, "[CAUTION] soil % VoidRatio = 0"
            caution = caution + 1
         end if
      end if
      if (.not. allocated(obj%Cohesion)) then
         print *, "[CAUTION] soil % Cohesion is not allocated."
         caution = caution + 1
      else
         print *, "[ok] soil % Cohesion is allocated."
      end if
      if (.not. allocated(obj%FrictionAngle)) then
         print *, "[CAUTION] soil % FrictionAngle is not allocated."
         caution = caution + 1
      else
         print *, "[ok] soil % FrictionAngle is allocated."
      end if

      if (caution == 0) then
         print *, "[ok] PreFlightCheckListSoil successfully done!"
      else
         print *, "[Caution] Total ", caution, "events were found."
      end if
   end subroutine
! ########################################

! ########################################
   subroutine deformSoil(obj, disp, x_min, x_max, y_min, y_max, z_min, z_max, BCRangeError)
      class(Soil_), target, intent(inout) :: obj
      real(real64), optional, intent(in) :: disp(3)
      real(real64), optional, intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max, BCRangeError
      type(FEMDomainp_), allocatable :: domainsp(:)
      integer(int32), allocatable :: contactList(:, :)
      integer(int32) :: i, j, numDomain, stemDomain, leafDomain, rootDomain
      real(real64) :: penalty, displacement(3), GLevel, error
      type(LinearSolver_) :: solver
      integer(int32) :: ElementID
      integer(int32), allocatable :: FixBoundary(:), DomainIDs(:)
      real(real64), allocatable :: A_ij(:, :), x_i(:), b_i(:) ! A x = b

      type(IO_) :: f

      error = input(default=dble(1.0e-7), option=BCRangeError)

      call solver%init(NumberOfNode=[obj%femdomain%nn()], DOF=3)

      ! create Elemental Matrices and Vectors
      do ElementID = 1, obj%femdomain%ne()

         ! For 1st element, create stiffness matrix
         A_ij = obj%femdomain%StiffnessMatrix(ElementID=ElementID, &
                                              E=obj%YoungModulus(ElementID), &
                                              v=obj%PoissonRatio(ElementID))
         b_i = obj%femdomain%MassVector( &
               ElementID=ElementID, &
               DOF=obj%femdomain%nd(), &
               Density=obj%Density(ElementID), &
               Accel=(/0.0d0, 0.0d0, -9.80d0/) &
               )
         DomainIDs = int(zeros(size(obj%femdomain%connectivity(ElementID=ElementID))))
         DomainIDs(:) = 1
         ! assemble them
         call solver%assemble( &
            connectivity=obj%femdomain%connectivity(ElementID=ElementID), &
            DOF=obj%femdomain%nd(), &
            eMatrix=A_ij, &
            DomainIDs=DomainIDs)
         call solver%assemble( &
            connectivity=obj%femdomain%connectivity(ElementID=ElementID), &
            DOF=obj%femdomain%nd(), &
            eVector=b_i, &
            DomainIDs=DomainIDs)
      end do

      ! set roler boundary in the sides
      ! x-direction
      call solver%prepareFix()

      FixBoundary = obj%femdomain%select(x_max=minval(obj%femdomain%mesh%nodcoord(:, 1))*(1.0d0 - error))
      !print *, size(FixBoundary)
      if (allocated(FixBoundary)) then
         do i = 1, size(FixBoundary)
            call solver%fix(nodeid=FixBoundary(i), DOF=3, EntryID=1, entryvalue=0.0d0, row_DomainID=1)
         end do
      end if

      FixBoundary = obj%femdomain%select(x_min=maxval(obj%femdomain%mesh%nodcoord(:, 1))*(1.0d0 - error))
      !print *, size(FixBoundary)
      if (allocated(FixBoundary)) then
         do i = 1, size(FixBoundary)
            call solver%fix(nodeid=FixBoundary(i), DOF=3, EntryID=1, entryvalue=0.0d0, row_DomainID=1)
         end do
      end if

      !y-direction
      FixBoundary = obj%femdomain%select(y_max=minval(obj%femdomain%mesh%nodcoord(:, 2))*(1.0d0 - error))
      !print *, size(FixBoundary)
      if (allocated(FixBoundary)) then
         do i = 1, size(FixBoundary)
            call solver%fix(nodeid=FixBoundary(i), DOF=3, EntryID=2, entryvalue=0.0d0, row_DomainID=1)
         end do
      end if

      FixBoundary = obj%femdomain%select(y_min=maxval(obj%femdomain%mesh%nodcoord(:, 2))*(1.0d0 - error))
      !print *, size(FixBoundary)
      if (allocated(FixBoundary)) then
         do i = 1, size(FixBoundary)
            call solver%fix(nodeid=FixBoundary(i), DOF=3, EntryID=2, entryvalue=0.0d0, row_DomainID=1)
         end do
      end if

      !z-direction
      FixBoundary = obj%femdomain%select(z_max=minval(obj%femdomain%mesh%nodcoord(:, 3))*(1.0d0 - error))
      !print *, size(FixBoundary)
      if (allocated(FixBoundary)) then
         do i = 1, size(FixBoundary)
            call solver%fix(nodeid=FixBoundary(i), DOF=3, EntryID=3, entryvalue=0.0d0, row_DomainID=1)
         end do
      end if

      ! fix deformation >> Dirichlet Boundary
      FixBoundary = obj%femdomain%select(x_min=x_min, x_max=x_max, &
                                         y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
      if (allocated(FixBoundary) .and. present(disp)) then
         do i = 1, size(FixBoundary)
            call solver%fix(nodeid=FixBoundary(i), DOF=3, EntryID=1, entryvalue=disp(1), row_DomainID=1)
            call solver%fix(nodeid=FixBoundary(i), DOF=3, EntryID=2, entryvalue=disp(2), row_DomainID=1)
            call solver%fix(nodeid=FixBoundary(i), DOF=3, EntryID=3, entryvalue=disp(3), row_DomainID=1)
         end do
      end if

      ! solve > get displacement
      call solver%solve("BiCGSTAB")

      obj%solver = solver

      ! update mesh
      obj%femdomain%mesh%nodcoord(:, :) = obj%femdomain%mesh%nodcoord(:, :) &
                                          + reshape(solver%x, obj%femdomain%nn(), obj%femdomain%nd())
      obj%disp = reshape(solver%x, obj%femdomain%nn(), obj%femdomain%nd())

   end subroutine

   subroutine syncSoil(obj, from, mpid)
      class(Soil_), intent(inout) :: obj
      integer(int32), intent(in) :: from
      type(MPI_), intent(inout) :: mpid

      type(FEMDomain_) :: FEMDomain

      !type(Boring_),allocatable :: Boring(:)
      !type(LinearSolver_) :: solver

      call mpid%Bcast(from=from, val=obj%disp) !(:,:)
      ! soil parameters
      call mpid%bcast(from=from, val=obj%YoungModulus)!(:)
      call mpid%bcast(from=from, val=obj%PoissonRatio)!(:)
      call mpid%bcast(from=from, val=obj%Density)!(:)
      call mpid%bcast(from=from, val=obj%VoidRatio)!(:)
      call mpid%bcast(from=from, val=obj%Cohesion)!(:)
      call mpid%bcast(from=from, val=obj%FrictionAngle)!(:)

      call mpid%bcast(from=from, val=obj%depth)!
      call mpid%bcast(from=from, val=obj%length)!
      call mpid%bcast(from=from, val=obj%width)!
      call mpid%bcast(from=from, val=obj%num_x)
      call mpid%bcast(from=from, val=obj%num_y)
      call mpid%bcast(from=from, val=obj%num_z)
      call mpid%bcast(from=from, val=obj%x)!,y,z ! center coordinate

      ! soil property

      ! ================
      ! Nutorient
      !------------
      call mpid%bcast(from=from, val=obj%N_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%P_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%K_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%Ca_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%Mg_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%S_kg)! = 0.0d0
      !------------
      call mpid%bcast(from=from, val=obj%Fe_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%Mn_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%B_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%Zn_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%Mo_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%Cu_kg)! = 0.0d0
      call mpid%bcast(from=from, val=obj%Cl_kg)! = 0.0d0
      ! ================

      ! ================
      ! Soil phyisical parameters
      call mpid%bcast(from=from, val=obj%C_N_ratio)!
      call mpid%bcast(from=from, val=obj%EC)!
      ! ================

   end subroutine
! ##############################################################
   function getNvalueSoil(obj, borings, VoronoiRatio, MovingAverageFilter, Delaunay) result(Nvalue)
      class(Soil_), intent(inout) :: obj
      type(Boring_), optional, intent(in) :: borings(:)
      real(real64), optional, intent(in) :: VoronoiRatio
      logical, optional, intent(in) :: MovingAverageFilter, Delaunay
      real(real64), allocatable :: Nvalue(:), boring_position(:, :), dist_xy(:), Nvals(:), w(:)
      real(real64) :: elem_position(3), min_dist, err, min_w_value, sum_dist, sum_w, &
                      Nval_tr, Vratio, xmin, xmax, ymin, ymax
      integer(int32) :: i, j, n
      type(Mesh_) :: mesh
      type(FEMDomain_) :: FEMDomain
      real(real64), allocatable :: dxi_dx(:, :), dxi_dx_inv(:, :), buf(:, :)
      real(real64) :: xi(2), depth, eNvalue(3)
      integer(int32) :: NearestBoringID(4), eNodeID(3), k
    !!! Algorithm for interpolation
      ! if Vratio=0.0 >> Distance-Weighted avarage
      ! if Vratio=1.0 >> Voronoi diagram
      ! if Vratio=0.5 >> Average

      if (present(Delaunay)) then
         if (Delaunay) then
            print *, "[Caution!] getNvalueSoil >> Some bugs exist!!"

            !(*) Delaunay Trianglular
            Nvalue = zeros(obj%femdomain%ne())
            n = size(borings) + 4
            mesh%nodcoord = zeros(n, 2)

            ! set nodes
            xmin = 1.10d0*minval(obj%femdomain%mesh%NodCoord(:, 1))
            xmax = 1.10d0*maxval(obj%femdomain%mesh%NodCoord(:, 1))
            ymin = 1.10d0*minval(obj%femdomain%mesh%NodCoord(:, 2))
            ymax = 1.10d0*maxval(obj%femdomain%mesh%NodCoord(:, 2))
            mesh%nodcoord(1, :) = [xmin, ymin]
            mesh%nodcoord(2, :) = [xmax, ymin]
            mesh%nodcoord(3, :) = [xmax, ymax]
            mesh%nodcoord(4, :) = [xmin, ymax]
            n = 0
            do i = 5, mesh%nn()
               n = n + 1
               mesh%nodcoord(i, 1) = borings(n)%x
               mesh%nodcoord(i, 2) = borings(n)%y
            end do

            !call mesh%convert2Dto3D()
            femdomain%mesh = mesh

            ! mesh Delaunay 2D
            call femdomain%meshing()
            mesh = femdomain%mesh
            call femdomain%mesh%convertTriangleToRectangular()
            call femdomain%mesh%convert2Dto3D()
            call femdomain%vtk("bmesh")

            buf = mesh%nodcoord
            mesh%nodcoord = zeros(size(buf, 1), 3)
            mesh%nodcoord(:, 1:2) = buf(:, 1:2)

            dxi_dx = zeros(2, 2)
            !call femdomain%vtk("mesh")
            !stop

            do i = 1, 4
               NearestBoringID(i) = mesh%getNearestNodeID( &
                                    x=mesh%nodcoord(i, 1), &
                                    y=mesh%nodcoord(i, 2), &
                                    exceptlist=[1, 2, 3, 4] &
                                    )
            end do
            ! for each element
            do i = 1, obj%femdomain%ne()
               elem_position = obj%femdomain%centerPosition(i)
               ! check IN/OUT of triangle
               do j = 1, size(mesh%elemnod, 1)
                  dxi_dx(1, 1) = mesh%nodcoord(mesh%elemnod(j, 2), 1) &
                                 - mesh%nodcoord(mesh%elemnod(j, 1), 1)
                  dxi_dx(2, 1) = mesh%nodcoord(mesh%elemnod(j, 2), 2) &
                                 - mesh%nodcoord(mesh%elemnod(j, 1), 2)
                  dxi_dx(1, 2) = mesh%nodcoord(mesh%elemnod(j, 3), 1) &
                                 - mesh%nodcoord(mesh%elemnod(j, 1), 1)
                  dxi_dx(2, 2) = mesh%nodcoord(mesh%elemnod(j, 3), 2) &
                                 - mesh%nodcoord(mesh%elemnod(j, 1), 2)
                  call inverse_rank_2(dxi_dx, dxi_dx_inv)
                  xi(1:2) = matmul(dxi_dx_inv, elem_position(1:2))
                  if (0.0d0 <= xi(1) .and. xi(1) <= 1.0d0) then
                     if (0.0d0 <= xi(2) .and. xi(2) <= 1.0d0) then
                        if (xi(1) + xi(2) <= 1.0d0) then
                           depth = elem_position(3)
                           eNodeID(1) = mesh%elemnod(j, 1)
                           eNodeID(2) = mesh%elemnod(j, 2)
                           eNodeID(3) = mesh%elemnod(j, 3)

                           do k = 1, 3
                              if (eNodeID(k) <= 4) then
                                 eNvalue(k) = borings(NearestBoringID(eNodeID(k)))%getN(depth=depth)
                              else
                                 n = eNodeID(k) - 4
                                 eNvalue(k) = borings(n)%getN(depth=depth)
                              end if
                           end do
                           Nvalue(i) = eNvalue(1) &
                                       + xi(1)*(eNvalue(2) - eNvalue(1)) &
                                       + xi(2)*(eNvalue(3) - eNvalue(1))
                           exit
                        end if
                     end if
                  end if
               end do
            end do
            return
         end if
      end if

      ! Interpolate by Voronoi subdivision
      ! and apply Gaussian filter
      Vratio = input(default=1.0d0, option=VoronoiRatio)
      err = dble(1.0e-13)
      if (obj%femdomain%empty()) then
         print *, "[ERROR] >> getNvalueSoil >> object not initialized."
         print *, "call soil % init()"
         return
      end if
      boring_position = zeros(size(borings), 2)
      dist_xy = zeros(size(borings))
      Nvals = zeros(size(borings))
      w = zeros(size(borings))
      do i = 1, size(borings)
         boring_position(i, 1) = borings(i)%x
         boring_position(i, 2) = borings(i)%y
      end do
      if (present(borings)) then
         Nvalue = zeros(obj%femdomain%ne())
         ! Element-wise
         ! interporate values from boring data
         do i = 1, obj%femdomain%ne() ! for each element
            ! find closest boring
            elem_position = obj%femdomain%centerPosition(i)
            dist_xy(:) = (boring_position(:, 1) - elem_position(1)) &
                         *(boring_position(:, 1) - elem_position(1)) + &
                         (boring_position(:, 2) - elem_position(2)) &
                         *(boring_position(:, 2) - elem_position(2))
            dist_xy = sqrt(dist_xy)
            n = minvalID(dist_xy)
            Nval_tr = borings(n)%getN(depth=elem_position(3))
            !dist_xy = dist_xy*dist_xy
            min_dist = minval(dist_xy)
            sum_dist = sum(dist_xy)
            ! 距離の比による補間
            w(:) = sum_dist/dist_xy(:)
            sum_w = sum(w)
            w = w/sum_w
            sum_w = sum(w)
            w = w/sum_w
            do n = 1, size(borings)
               Nvals(n) = w(n)*borings(n)%getN(depth=elem_position(3))
            end do
            Nvalue(i) = (1.0d0 - VRatio)*sum(Nvals) + VRatio*Nval_tr
         end do
      end if

      if (present(MovingAverageFilter)) then
         if (MovingAverageFilter) then
            ! gaussina filter
            Nvalue = obj%FEMDomain%MovingAverageFilter(inScalarField=Nvalue, ignore_top_and_bottom=.true.)
         end if
      end if

   end function
! ##############################################################
   pure function convertNvalue2VsSoil(obj, Nvalue, Formula, H, Yg, St) result(Vs)
      class(Soil_), intent(in) :: obj
      real(real64), intent(in) :: Nvalue(:)
      integer(int32), intent(in) :: Formula
      ! for Ohta-Goto
      real(real64), optional, intent(in) :: H(:), Yg(:), St(:)
      real(real64), allocatable :: Vs(:)
      ! https://www.zenchiren.or.jp/e-Forum/2019/PDF/2019_105.pdf
      Vs = zeros(size(Nvalue))
      if (Formula == PF_N2Vs_Imai) then
         Vs(:) = 97.0d0*(Nvalue(:)**0.314)
      elseif (Formula == PF_N2Vs_OhtaGoto) then
         Vs(:) = 68.79d0*(Nvalue(:)**0.717)*H(:)**0.199*Yg(:)*St(:)
      elseif (Formula == PF_N2Vs_JAPANROAD_1) then
         Vs(:) = 80.0d0*(Nvalue(:)**0.33333)
      elseif (Formula == PF_N2Vs_JAPANROAD_2) then
         Vs(:) = 100.0d0*(Nvalue(:)**0.33333)

      end if

   end function

! ##############################################################

! ##############################################################
function JGS_coeff_subgrade_react_Soil(this,B,Is) result(k)
   class(Soil_),intent(inout) :: this
   real(real64),intent(in) :: B ! width of foundation loaded on the soil (Unit: m)
   real(real64),optional,intent(in) :: Is
   integer(int32),allocatable  :: elementlist(:)
   real(real64) :: k,E,v

   if(.not. allocated(this%YoungModulus))then
      print *, "[ERROR] >> JGS_coeff_subgrade_react_Soil >> .not. allocated(this%YoungModulus)"
      stop
   endif
   if(.not. allocated(this%PoissonRatio))then
      print *, "[ERROR] >> JGS_coeff_subgrade_react_Soil >> .not. allocated(this%PoissonRatio)"
      stop
   endif

   ! Compute a coefficient of subgrade reaction of soil by Ishihara's formula (Kenji Ishihara, Soil Mehcanics, 1988, pp. 216-219.)
   elementlist = this%FEMDomain%getElementList(xmin=this%FEMDomain%x_min(),zmin=this%FEMDomain%z_max()-1.0d0)
   E  = average(this%YoungModulus(elementlist) ) ! Average of Young's modulus above G.L. -1.0m
   v  = average(this%PoissonRatio(elementlist) ) ! Average of Poisson's ratio above G.L. -1.0m
   
   k = 1.0d0/(input(default=0.785d0,option=Is)*(1.0d0-v*v) )*E/B

end function
! ##############################################################


! ##############################################################
function JGS_subgrade_displacement_Soil(this,area,weight,Is) result(u)
   class(Soil_),intent(inout) :: this
   real(real64),intent(in) :: area  ! area of foundation loaded on the soil (Unit: m^2)
   real(real64),intent(in) :: weight ! weight of foundation loaded on the soil (Unit: t)
   real(real64),optional,intent(in) :: Is ! optional
   real(real64) :: u

   u = weight*1000.0d0*9.80d0/area/this%JGS_coeff_subgrade_react(B=sqrt(area)) ! m

end function
! ##############################################################


! ##############################################################
subroutine raining_Soil(this,precipitation_mm)
   class(Soil_),intent(inout) :: this
   real(real64),intent(in) :: precipitation_mm

   ! update Young's modulus by precipitation
   

end subroutine
! ##############################################################



! ##############################################################
subroutine cultivate_Soil(this,depth,VoidRatio)
   class(Soil_),intent(inout) :: this
   real(real64),intent(in) :: depth,VoidRatio
   real(real64),allocatable :: volumetric_strain(:)

   ! update Young's modulus by precipitation
   call this%updateVoidRatio(range=to_range(z_min=this%GL()-depth),VoidRatio=VoidRatio)

   if(.not. allocated(this%P0))then
      print *, "[ERROR] :: this%P0 is not set."
      stop
   endif
   if(.not. allocated(this%lambda))then
      print *, "[ERROR] :: this%lambda is not set."
      stop
   endif
   if(.not. allocated(this%kappa))then
      print *, "[ERROR] :: this%kappa is not set."
      stop
   endif
   if(.not. allocated(this%e0))then
      print *, "[ERROR] :: this%e0 is not set."
      stop
   endif
   if(.not. allocated(this%Py))then
      print *, "[ERROR] :: this%Py is not set."
      stop
   endif
   if(.not. allocated(this%Density))then
      print *, "[ERROR] :: this%Density is not set."
      stop
   endif
   if(.not. allocated(this%PoissonRatio))then
      print *, "[ERROR] :: this%PoissonRatio is not set."
      stop
   endif
   
   ! 間隙比からヤング率を更新
   ! ln f-ln P 関係を利用
   ! f = f0*(P/P_0)^(-lambda)
   !this%VoidRatio( this%FEMDomain%getElementList(x_min=this%FEMDomain%x_min(),&
   !   z_min=this%FEMDomain%z_max()-depth) ) = VoidRatio
   volumetric_strain  = (this%VoidRatio - this%e0)/this%e0
   this%YoungModulus = -this%P0*1.0d0/this%lambda*(1+volumetric_strain)**(-1.0d0-1.0d0/this%lambda)


end subroutine
! ##############################################################



! ##############################################################
subroutine updateVoidRatio_Soil(this,range,VoidRatio)
   class(Soil_),intent(inout) :: this
   type(Range_),intent(in) :: range
   real(real64),intent(in) :: VoidRatio

   if (.not. allocated(this%VoidRatio) )then
      this%VoidRatio = zeros(this%ne())
   endif

   this%VoidRatio(this%FEMDomain%getElementList(&
         xmin=range%x_range(1),&
         xmax=range%x_range(2),&
         ymin=range%y_range(1),&
         ymax=range%y_range(2),&
         zmin=range%z_range(1),&
         zmax=range%z_range(2) &
      ) ) = VoidRatio

   
end subroutine
! ##############################################################


! ##############################################################
function Ground_level_of_Soil(this) result(ret)
   class(Soil_),intent(in) :: this
   real(real64) :: ret

   ret = this%FEMDomain%zmax()

end function
! ##############################################################


! ##############################################################
subroutine setSoilType_SoilClass(this,config)
   class(Soil_),intent(inout) :: this
   character(*),intent(in) :: config
   type(IO_) :: f

   this%config = config

   this%lambda = freal(f%parse_json(config,to_list("lnf_lnP","lambda")) )*ones(this%ne())
   this%kappa  = freal(f%parse_json(config,to_list("lnf_lnP","kappa")) )*ones(this%ne())
   this%e0     = freal(f%parse_json(config,to_list("lnf_lnP","e0")) )*ones(this%ne())
   this%P0     = freal(f%parse_json(config,to_list("lnf_lnP","P0")) )*ones(this%ne())
   this%Py     = freal(f%parse_json(config,to_list("lnf_lnP","Py")) )*ones(this%ne())

   this%VoidRatio = this%e0*ones(this%ne())
   this%Density      = freal(f%parse_json(config,to_list("MechanicalProperties","Density")) )*ones(this%ne())
   this%PoissonRatio = freal(f%parse_json(config,to_list("MechanicalProperties","PoissonRatio")) )*ones(this%ne())
   
   !this%YoungModulus = - this%P0(:)*1.0d0/this%kappa(:)*(1.0d0+this)

end subroutine
! ##############################################################

! ##############################################################
function nn_Soil(this) result(ret)
   class(Soil_),intent(in) :: this
   integer(int32) :: ret

   ret = this%FEMDomain%nn()

end function
! ##############################################################


! ##############################################################
function ne_Soil(this) result(ret)
   class(Soil_),intent(in) :: this
   integer(int32) :: ret

   ret = this%FEMDomain%ne()

end function
! ##############################################################



! ##############################################################
function nne_Soil(this) result(ret)
   class(Soil_),intent(in) :: this
   integer(int32) :: ret

   ret = this%FEMDomain%nne()

end function
! ##############################################################


! ##############################################################
function nd_Soil(this) result(ret)
   class(Soil_),intent(in) :: this
   integer(int32) :: ret

   ret = this%FEMDomain%nd()

end function
! ##############################################################





end module
