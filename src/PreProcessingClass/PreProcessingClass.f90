module PreprocessingClass
   use, intrinsic :: iso_fortran_env
   use std
   use FEMDomainClass
   use PostProcessingClass

   implicit none

   type :: PreProcessing_
      type(FEMDomain_) :: FEMDomain
      type(FEMDomain_), pointer :: pFEMDomain
      character(:), allocatable   :: PictureName
      character(:), allocatable   :: RGBDataName, PixcelSizeDataName
      integer(int32)         :: PixcelSize(2), num_of_pixcel
      integer(int32)         :: ColorRGB(3)
   contains
      !procedure :: set                => setPreprocessing
      procedure :: getScfFromImage => getScfFromImagePreProcessing
      procedure :: Init => InitializePrePro
      procedure :: finalize => finalizePrePro
      procedure :: ImportPictureName => ImportPictureName
      procedure :: importPixcelAsNode => importPixcelAsNodePreProcessing
      procedure :: ShowName => ShowPictureName
      procedure :: ShowPixcelSize => ShowPixcelSize
      procedure :: GetPixcelSize => GetPixcelSize
      procedure :: GetAllPointCloud => GetAllPointCloud
      procedure :: SetColor => SetColor
      procedure :: ShowColor => ShowColor
      procedure :: GetPixcelByRGB => GetPixcelByRGB
      procedure :: GetSurfaceNode => GetPixcelSurfaceNode
      procedure :: modifySuefaceNode => modifySuefaceNodePrepro
      procedure :: AssembleSurfaceElement => AssembleSurfaceElement
      procedure :: ReduceSize => ReduceSize
      procedure :: ExportGeoFile => ExportGeoFile
      procedure :: ConvertGeo2Msh => ConvertGeo2Msh
      procedure :: ConvertGeo2Inp => ConvertGeo2Inp
      procedure :: ConvertGeo2Mesh => ConvertGeo2Mesh
      procedure :: ConvertMsh2Scf => ConvertMsh2Scf
      procedure :: ConvertMesh2Scf => ConvertMesh2Scf
      procedure :: ConvertGeo2VTK => ConvertGeo2VTK
      procedure :: Export => ExportPreProcessing
      procedure :: ExportAsLodgingSim => ExportAsLodgingSimProcessing
      procedure :: import => importPreProcessing
      procedure :: Reverse => ReversePreProcessing
      procedure :: SetDataType => SetDataTypeFEMDomain
      procedure :: SetSolver => SetSolverPreProcessing
      procedure :: SetUp => SetUpPreprocessing
      procedure :: SetScale => SetScalePreProcessing
      procedure :: SetBC => SetBoundaryConditionPrePro
      procedure :: removeBC => removeBoundaryConditionPrePro
      procedure :: SetSizeOfBC => SetSizeOfBCPrePrecessing
      procedure :: SetMatPara => SetMatParaPreProcessing
      procedure :: SetMatID => SetMatIDPreProcessing
      procedure :: ShowBC => ShowBCPrePrecessing
      procedure :: Convert3Dto2D => Convert3Dto2D
      procedure :: Convert2Dto3D => Convert2Dto3D
      procedure :: SetControlPara => SetControlParaPrePro
      procedure :: getSkelton => getSkeltonPreProcessing
      procedure :: Boolean => BooleanModifyerPreProcessing
      procedure :: setEntity => setEntityPreProcessing
      procedure :: showMesh => showMeshPreProcessing
      procedure :: meshing => meshingPreProcessing

   end type

contains

! #########################################################
   subroutine getScfFromImagePreProcessing(obj, project, ElemType, MPIData, R, G, B, scalex, scaley, &
                                           Soilfile, sR, SG, sB, SolverName)
      class(PreProcessing_), intent(inout) :: obj
      class(MPI_), intent(inout)           :: MPIData

      type(Dictionary_)       :: InfileList, DBoundlist, NBoundlist, Materialist
      type(PreProcessing_)    :: leaf, soil
      character(*), intent(in) :: project, elemtype, SolverName
      character(*), optional, intent(in) :: Soilfile
      integer(int32), intent(in) :: R, G, B
      integer(int32), optional, intent(in) :: sR, SG, sB
      real(real64), intent(in) :: scalex, scaley
      real(real64) :: Dbound_val, Nbound_val, xratio, yratio
      character(:), allocatable         :: name, name1, name2, name3, name4, str_id, &
                                           sname, dirichlet, neumann, materials, parameters
      integer(int32) :: NumOfImages, i, id, num_d, num_n, DBoundRGB(3), Dbound_xyz, NBoundRGB(3), Nbound_xyz
      integer(int32) :: NumOfMaterial, NumOfparameter, matid, MaterialRGB(3)
      real(real64), allocatable :: matpara(:)

      if (ElemType /= "LinearRectangularGp4") then
         print *, "ERROR :: now only LinearRectangularGp4 is available."
         return
      end if

      ! get paths for Image lists
      open (50, file=project//"filenamelist.txt")
      read (50, *) NumOfImages
      call InfileList%Init(NumOfImages)
      do i = 1, NumOfImages
         read (50, '(A)') name
         call InfileList%Input(i, name)
      end do
      close (50)
      call MPIData%createStack(total=NumOfImages)

      ! get boundary information list
      open (60, file=project//"boundcondlist.txt")
      read (60, *) dirichlet
      read (60, *) num_d
      call DBoundlist%Init(num_d)
      do i = 1, num_d
         read (60, '(A)') name
         read (60, *) DBoundRGB(1:3)
         read (60, *) Dbound_xyz, Dbound_val
         call DBoundlist%Input(i, content=name)
         call DBoundlist%Input(i, intlist=DBoundRGB)
         call DBoundlist%Input(i, IntValue=Dbound_xyz)
         call DBoundlist%Input(i, RealValue=Dbound_val)
      end do
      read (60, *) neumann
      read (60, *) num_n
      do i = 1, num_n
         read (60, '(A)') name
         read (60, *) NBoundRGB(1:3)
         read (60, *) Nbound_xyz, Nbound_val
         call NBoundlist%Input(i, content=name)
         call NBoundlist%Input(i, Intlist=NBoundRGB)
         call NBoundlist%Input(i, IntValue=Nbound_xyz)
         call NBoundlist%Input(i, RealValue=Nbound_val)
      end do
      close (60)

      ! get paths for material information lists
      open (70, file=project//"materialist.txt")
      read (70, '(A)') materials
      read (70, *) NumOfMaterial
      read (70, '(A)') parameters
      read (70, *) NumOfparameter
      allocate (matpara(NumOfparameter))

      call Materialist%Init(NumOfMaterial)
      do i = 1, NumOfMaterial
         read (70, '(A)') name
         read (70, *) MaterialRGB(1:3)
         read (70, *) matpara(1:NumOfparameter)
         call materialist%Input(i, content=name)
         call materialist%Input(i, Intlist=materialRGB)
         call materialist%Input(i, Realist=matpara)
         !    call Materialist%Input(i, name )
      end do
      close (70)

      do i = 1, size(MPIData%LocalStack)
         id = MPIData%LocalStack(i)
         str_id = adjustl(fstring(id))
         name = InfileList%get(MPIData%LocalStack(i))
         print *, "MyRank", MPIData%MyRank, "|", name

         ! Get Pixcel
         call leaf%ImportPictureName(name)
         call leaf%GetPixcelSize(MPIData)
         call leaf%SetColor(R, G, B)
         call leaf%GetPixcelByRGB(MPIData, err=5, onlycoord=.true.)
         ! Get Outline
         call leaf%GetSurfaceNode(MPIData)
         call leaf%AssembleSurfaceElement(MPIData, dim=2, threshold=5, DelRange=5)

         ! Convert SurfaceNod to .geo
         call leaf%ExportGeoFile(MPIData, Name=project//"mesh"//str_id//".geo")

         ! Run Gmsh to convert .geo to .msh
         call leaf%ConvertGeo2Msh(MPIData, Name=project//"mesh"//str_id//".geo")
         call leaf%ConvertGeo2Inp(MPIData, Name=project//"mesh"//str_id//".geo")
         call leaf%ConvertGeo2Mesh(MPIData, Name=project//"mesh"//str_id//".geo")

         ! Convert .msh to .scf

         call leaf%ConvertMesh2Scf(MPIData, ElementType=ElemType, &
                                   Name=project//"mesh"//str_id//".mesh")
         call leaf%FEMDomain%checkconnectivity(fix=.true.)

         !call leaf%Convert3Dto2D()

         call leaf%SetSolver(InSolverType=SolverName)
         call leaf%SetUp(NoFacetMode=.true.)

         call leaf%SetMatPara(materialist=materialist, simple=.true., MaterialID=1)

         call leaf%setBC(MPIData=MPIData, dirichlet=.true., Boundinfo=DBoundlist)
         call leaf%setBC(MPIData=MPIData, neumann=.true., Boundinfo=NBoundlist)
         call leaf%SetControlPara(OptionalItrTol=100, OptionalTimestep=100, OptionalSimMode=1)

         !call leaf%Export(Name=project//"root"//str_id//".geo")

         ! get soil mesh
         if (present(Soilfile)) then
            sname = Soilfile
            call soil%ImportPictureName(sname)
            call soil%GetPixcelSize(MPIData)
            call soil%SetColor(sR, sG, sB)
            call soil%GetPixcelByRGB(MPIData, err=5, onlycoord=.true.)

            ! Get Outline (simple outline)
            ! see soil as a box
            call soil%GetSurfaceNode(MPIData, box=.true.)
            call soil%modifySuefaceNode(Mesh=leaf%FEMDomain%Mesh, boolean="diff")

            ! Convert SurfaceNod to .geo
            call soil%ExportGeoFile(MPIData, Name=project//"soil"//str_id//".geo")

            ! Run Gmsh to convert .geo to .msh
            call soil%ConvertGeo2Msh(MPIData, Name=project//"soil"//str_id//".geo")
            call soil%ConvertGeo2Inp(MPIData, Name=project//"soil"//str_id//".geo")
            call soil%ConvertGeo2Mesh(MPIData, Name=project//"soil"//str_id//".geo")
            ! Convert .msh to .scf
            call soil%ConvertMesh2Scf(MPIData, ElementType=ElemType, &
                                      Name=project//"soil"//str_id//".mesh")

            call soil%FEMDomain%checkconnectivity(fix=.true.)

            call soil%SetSolver(InSolverType=SolverName)
            call soil%SetUp(NoFacetMode=.true.)

            call soil%SetMatPara(materialist=materialist, simple=.true., MaterialID=2)

            ! setup boundary conditions
            call soil%setBC(MPIData=MPIData, dirichlet=.true., Boundinfo=DBoundlist)
            call soil%setBC(MPIData=MPIData, Neumann=.true., Boundinfo=NBoundlist)

            call soil%SetControlPara(OptionalItrTol=100, OptionalTimestep=100, OptionalSimMode=1)

            !call soil%Export(Name=project//"soil"//str_id//".geo")

         end if
         xratio = scalex/leaf%PixcelSize(1)
         yratio = scaley/leaf%PixcelSize(2)
         call soil%SetScale(xratio=xratio, yratio=yratio)
         call leaf%SetScale(xratio=xratio, yratio=yratio)
         call soil%Reverse()
         call leaf%Reverse()
         call leaf%Export(with=soil, Name=project//"rootandsoil"//str_id//".scf", regacy=.true.)

         ! destructor
         call leaf%finalize()
         call soil%finalize()

      end do

      ! Export Object
      !call leaf%FEMDomain%GmshPlotVector(Name="Tutorial/InputData/grass_leaf",step=0,&
      !    withMsh=.true.,FieldName="DispBound",NodeWize=.true.,onlyDirichlet=.true.)

   end subroutine
! #########################################################

! #########################################################
   subroutine InitializePrePro(obj, Default)
      class(PreProcessing_), intent(inout)::obj
      logical, optional, intent(in)::Default

      call obj%FEMDomain%Init(Default)
   end subroutine
! #########################################################

! #########################################################
   subroutine finalizePrePro(obj)
      class(PreProcessing_), intent(inout)::obj

      call obj%FEMDomain%delete()

      obj%PictureName = ""
      obj%RGBDataName = ""
      obj%PixcelSizeDataName = ""
      obj%PixcelSize(:) = 0
      obj%num_of_pixcel = 0
      obj%ColorRGB(:) = 0
   end subroutine
! #########################################################

! #########################################################
   subroutine ImportPictureName(obj, Name)
      class(PreProcessing_)::obj
      character(*), intent(in)::Name
      obj%PictureName = name
   end subroutine
! #########################################################

! #########################################################
   subroutine ShowPictureName(obj)
      class(PreProcessing_)::obj
      print *, obj%PictureName
   end subroutine
! #########################################################

! #########################################################
   subroutine ShowPixcelSize(obj)
      class(PreProcessing_)::obj
      character*20       :: pix_x
      character*20       :: pix_y
      write (pix_x, *) obj%PixcelSize(1)
      write (pix_y, *) obj%PixcelSize(2)

      print *, "Pixcel size is :: ", adjustl(pix_x), " x ", &
         adjustl(pix_y)
   end subroutine
! #########################################################

! #########################################################
   subroutine GetPixcelSize(obj, MPIData, Name)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      character(*), optional, intent(in)   :: Name
      character(:), allocatable       :: pid
      character(:), allocatable      :: python_script
      character(:), allocatable      :: python_buffer
      character(:), allocatable      :: command
      integer(int32)              :: fh

      call MPIData%GetInfo()

      pid = adjustl(fstring(MPIData%MyRank))
      python_script = "GetPixcelSize_pid_"//pid//".py"
      python_buffer = "GetPixcelSize_pid_"//pid//".txt"

      if (present(Name)) then
         python_script = Name//"GetPixcelSize_pid_"//pid//".py"
         python_buffer = Name//"GetPixcelSize_pid_"//pid//".txt"
      end if

      obj%PixcelSizeDataName = python_buffer
      !print *, python_script

      ! using python script
      ! python imaging library is to be installed.
      fh = MPIData%MyRank + 100

      open (fh, file=python_script, status="replace")
      command = "from PIL import Image"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "import sys"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "import os"
      write (fh, '(A)') adjustl(obj%PictureName)

      ! open file
      command = 'img_in = Image.open("'//obj%PictureName//'")'
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)
      command = 'python_buffer = open("'//python_buffer//'","w")'
      write (fh, '(A)') adjustl(obj%PictureName)
      !print *, command
      ! get pixcel size
      command = "rgb_im = img_in.convert('RGB')"
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "size = rgb_im.size"
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "print( str(size[0]), ' ',str(size[1])  ) "
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)

      ! write size
      command = "python_buffer.write( str(size[0]))"
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "python_buffer.write('\n')"
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "python_buffer.write( str(size[1]))"
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)

      ! close
      command = "img_in.close()"
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "python_buffer.close()"
      !print *, command
      write (fh, '(A)') adjustl(obj%PictureName)
      close (fh)

      command = "python3 "//python_script
      !print *, obj%PictureName
      call execute_command_line(obj%PictureName)

      ! get pixcel size
      open (fh, file=python_buffer, status="old")
      read (fh, *) obj%PixcelSize(1)
      read (fh, *) obj%PixcelSize(2)
      close (fh)

   end subroutine
! #########################################################

! #########################################################
   subroutine GetAllPointCloud(obj, MPIData, Name)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      character(*), optional, intent(in)   :: Name
      character*20       :: pid
      character*200      :: python_script
      character*200      :: python_buffer
      character*200      :: command
      integer(int32)              :: fh

      call MPIData%GetInfo()
      write (pid, *) MPIData%MyRank

      python_script = "GetAllPointCloud_pid_"//pid//".py"
      python_buffer = "GetAllPointCloud_pid_"//pid//".txt"
      if (present(Name)) then
         python_script = Name//"GetAllPointCloud_pid_"//pid//".py"
         python_buffer = Name//"GetAllPointCloud_pid_"//pid//".txt"
      end if

      print *, python_script

      ! using python script
      ! python imaging library is to be installed.
      fh = MPIData%MyRank + 10
      open (fh, file=python_script, status="replace")
      command = "from PIL import Image"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "import sys"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "import os"
      write (fh, '(A)') adjustl(obj%PictureName)

      ! open file
      command = 'img_in = Image.open("'//obj%PictureName//'")'
      write (fh, '(A)') adjustl(obj%PictureName)
      command = 'python_buffer = open("'//python_buffer//'","w")'
      write (fh, '(A)') adjustl(obj%PictureName)

      ! get pixcel size
      command = "rgb_im = img_in.convert('RGB')"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "size = rgb_im.size"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "print( str(size[0]), ' ',str(size[1])  ) "
      write (fh, '(A)') adjustl(obj%PictureName)

      ! get rgb pixcel coordinates
      command = "width,height =img_in.size"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "for i in range(width):"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "for j in range(height):"
      write (fh, '(A)') "   "//adjustl(obj%PictureName)
      command = "R,G,B=rgb_im.getpixel((i,j))"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)
      command = "python_buffer.write(str(i)+'\t')"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)
      command = "python_buffer.write(str(j)+'\t')"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)
      command = "python_buffer.write(str(R)+'\t')"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)
      command = "python_buffer.write(str(G)+'\t')"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)
      command = "python_buffer.write(str(B)+'\n')"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)

      ! close
      command = "img_in.close()"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "python_buffer.close()"
      write (fh, '(A)') adjustl(obj%PictureName)
      close (fh)

      command = "python3 "//python_script
      print *, obj%PictureName
      call execute_command_line(obj%PictureName)

   end subroutine
! #########################################################

! #########################################################
   subroutine GetPixcelByRGB(obj, MPIData, err, onlycoord, Name)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      integer(int32), optional, intent(in)        :: err
      logical, optional, intent(in)        :: onlycoord
      character(*), optional, intent(in)   :: Name
      character(:), allocatable :: pid
      character(:), allocatable :: Red, Green, Blue
      character(:), allocatable :: er
      character(:), allocatable :: python_script
      character(:), allocatable :: python_buffer
      character(:), allocatable :: python_buffer_size
      character(:), allocatable :: command
      integer(int32)              :: fh, error, sizeofpc, i

      if (present(err)) then
         error = err
      else
         error = 0
      end if

      call MPIData%GetInfo()
      write (pid, *) MPIData%MyRank
      write (Red, *) obj%ColorRGB(1)
      write (Green, *) obj%ColorRGB(2)
      write (Blue, *) obj%ColorRGB(3)
      write (er, *) error

      python_script = "GetPixcelByRGB_pid_"//pid//".py"
      python_buffer = "GetPixcelByRGB_pid_"//pid//".txt"
      python_buffer_size = "GetPixcelByRGB_size_pid_"//pid//".txt"
      obj%RGBDataName = python_buffer
      if (present(Name)) then
         python_script = Name//"GetPixcelByRGB_pid_"//pid//".py"
         python_buffer = Name//"GetPixcelByRGB_pid_"//pid//".txt"
         python_buffer_size = Name//"GetPixcelByRGB_size_pid_"//pid//".txt"
      end if
      print *, python_script

      ! using python script
      ! python imaging library is to be installed.
      fh = MPIData%MyRank + 10
      open (fh, file=python_script, status="replace")
      command = "from PIL import Image"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "import sys"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "import os"
      write (fh, '(A)') adjustl(obj%PictureName)

      ! open file
      command = 'img_in = Image.open("'//obj%PictureName//'")'
      write (fh, '(A)') adjustl(obj%PictureName)
      command = 'python_buffer = open("'//python_buffer//'","w")'
      write (fh, '(A)') adjustl(obj%PictureName)
      command = 'python_buffer_size = open("'//python_buffer_size//'","w")'
      write (fh, '(A)') adjustl(obj%PictureName)

      ! get pixcel size
      command = "rgb_im = img_in.convert('RGB')"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "size = rgb_im.size"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "print( str(size[0]), ' ',str(size[1])  ) "
      write (fh, '(A)') adjustl(obj%PictureName)

      ! get rgb pixcel coordinates

      command = "itr = 0"
      write (fh, '(A)') adjustl(obj%PictureName)

      command = "width,height =img_in.size"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "for i in range(width):"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "for j in range(height):"
      write (fh, '(A)') "   "//adjustl(obj%PictureName)
      command = "R,G,B=rgb_im.getpixel((i,j))"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)
      command = "er=abs(R-"//adjustl(Red)// &
                ")+abs(G-"//adjustl(Green)// &
                ")+abs(B-"//adjustl(Blue)//")"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)
      command = "if er <= "//adjustl(er)//" :"
      write (fh, '(A)') "       "//adjustl(obj%PictureName)

      command = "python_buffer.write(str(i)+'\t')"
      write (fh, '(A)') "           "//adjustl(obj%PictureName)
      command = "itr=itr+1"
      write (fh, '(A)') "           "//adjustl(obj%PictureName)
      if (onlycoord .eqv. .true.) then
         command = "python_buffer.write(str(j)+'\n')"
         write (fh, '(A)') "           "//adjustl(obj%PictureName)
      else
         command = "python_buffer.write(str(j)+'\t')"
         write (fh, '(A)') "           "//adjustl(obj%PictureName)
         command = "python_buffer.write(str(R)+'\t')"
         write (fh, '(A)') "           "//adjustl(obj%PictureName)
         command = "python_buffer.write(str(G)+'\t')"
         write (fh, '(A)') "           "//adjustl(obj%PictureName)
         command = "python_buffer.write(str(B)+'\n')"
         write (fh, '(A)') "           "//adjustl(obj%PictureName)
      end if

      ! close
      command = "python_buffer_size.write(str(itr)+'\n')"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "img_in.close()"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "python_buffer.close()"
      write (fh, '(A)') adjustl(obj%PictureName)
      command = "python_buffer_size.close()"
      write (fh, '(A)') adjustl(obj%PictureName)
      close (fh)

      command = "python3 "//python_script
      print *, obj%PictureName
      call execute_command_line(obj%PictureName)

      open (fh, file=python_buffer_size, status="old")
      read (fh, *) sizeofpc
      close (fh)

      allocate (obj%FEMDomain%Mesh%NodCoord(sizeofpc, 3))
      obj%FEMDomain%Mesh%NodCoord(:, 3) = 0.0d0
      open (fh, file=python_buffer, status="old")
      if (sizeofpc == 0) then
         print *, "ERROR :: GetPixcelByRGB >> no such color"
         stop
      end if
      do i = 1, sizeofpc
         read (fh, *) obj%FEMDomain%Mesh%NodCoord(i, 1:2)
      end do
      obj%FEMDomain%Mesh%NodCoord(:, 2) = -1.0d0*obj%FEMDomain%Mesh%NodCoord(:, 2)
      close (fh)

   end subroutine
! #########################################################

! #########################################################
   subroutine SetColor(obj, Red, Green, Blue)
      class(PreProcessing_), intent(inout):: obj
      integer(int32), intent(in) :: Red, Green, Blue

      obj%ColorRGB(1) = Red
      obj%ColorRGB(2) = Green
      obj%ColorRGB(3) = Blue

   end subroutine
! #########################################################

! #########################################################
   subroutine ShowColor(obj)
      class(PreProcessing_), intent(inout):: obj

      print *, "Object Name is : ", obj%PictureName
      print *, "Red   : ", obj%ColorRGB(1)
      print *, "Green : ", obj%ColorRGB(2)
      print *, "Blue  : ", obj%ColorRGB(3)

   end subroutine
! #########################################################

! #########################################################
   subroutine GetPixcelSurfaceNode(obj, MPIData, r, NumOfMaxNod, Name, convex, division, box)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      character(*), optional, intent(in)   :: Name
      integer(int32), optional, intent(in)        :: r, NumOfMaxNod, division
      logical, optional, intent(in)        :: convex, box

      character(:), allocatable   :: python_buffer
      character(:), allocatable    :: pid

      integer(int32), allocatable :: KilledPixcel(:)

      integer(int32) :: i, j, k, n, node_id, check_node_id, point_count, fh, MaxNod, dv, itr
      real(real64) :: x_real, y_real, z_real, xmin, xmax, ymin, ymax, dly, dlx, xwidth, ywidth, xm, ym
      real(real64) :: x_real_tr, y_real_tr, z_real_tr, diff_real, max_r, x_tr, y_tr, ymax_tr, ymin_tr
      real(real64), allocatable :: buffer(:, :), NodCoordDiv(:, :), xmaxval(:), ymaxval(:), xminmval(:), yminmval(:)
      ! in case of box
      if (present(box)) then
         if (box .eqv. .true.) then
            print *, "Notice :: obj%GetSurfaceNode as Box"
            xm = minval(obj%FEMDomain%Mesh%NodCoord(:, 1))
            ym = minval(obj%FEMDomain%Mesh%NodCoord(:, 2))
            dv = input(default=10, option=division)
            xwidth = maxval(obj%FEMDomain%Mesh%NodCoord(:, 1)) - minval(obj%FEMDomain%Mesh%NodCoord(:, 1))
            ywidth = maxval(obj%FEMDomain%Mesh%NodCoord(:, 2)) - minval(obj%FEMDomain%Mesh%NodCoord(:, 2))
            dlx = xwidth/dble(dv)
            dly = ywidth/dble(dv)
            xmax = maxval(obj%FEMDomain%Mesh%NodCoord(:, 1))
            ymax = maxval(obj%FEMDomain%Mesh%NodCoord(:, 2))
            xmin = minval(obj%FEMDomain%Mesh%NodCoord(:, 1))
            ymin = minval(obj%FEMDomain%Mesh%NodCoord(:, 2))
            allocate (buffer(4*dv, 2))
            buffer(:, :) = 0.0d0

            do i = 1, dv
               buffer(i, 1) = dlx*dble(i - 1) + xm
               buffer(i, 2) = ymin
            end do

            do i = 1, dv
               buffer(i + dv, 1) = xmax
               buffer(i + dv, 2) = dly*dble(i - 1) + ym
            end do

            do i = 1, dv
               buffer(i + dv*2, 1) = xmax - dlx*dble(i - 1)
               buffer(i + dv*2, 2) = ymax
            end do

            do i = 1, dv
               buffer(i + dv*3, 1) = xmin
               buffer(i + dv*3, 2) = ymax - dly*dble(i - 1)
            end do

            deallocate (obj%FEMDomain%Mesh%NodCoord)
            allocate (obj%FEMDomain%Mesh%NodCoord(size(buffer, 1), size(buffer, 2)))
            do i = 1, size(buffer, 1)
               obj%FEMDomain%Mesh%NodCoord(i, :) = buffer(size(buffer, 1) - i + 1, :)
            end do
            return
         end if
      end if

      ! in case of convex
      ! in case of convex
      !if(present(convex) )then
      !    if(convex .eqv. .true.)then
      !        xm=minval(obj%FEMDomain%Mesh%NodCoord(:,1))
      !        ym=minval(obj%FEMDomain%Mesh%NodCoord(:,2))
      !        ! get discrete points with interval by division
      !        dv = input(default=10,option=division)
      !        xwidth = maxval(obj%FEMDomain%Mesh%NodCoord(:,1))-minval(obj%FEMDomain%Mesh%NodCoord(:,1))
      !        ywidth = maxval(obj%FEMDomain%Mesh%NodCoord(:,2))-minval(obj%FEMDomain%Mesh%NodCoord(:,2))
      !        dlx = xwidth/dble(dv)
      !        dly = ywidth/dble(dv)
      !        allocate(buffer(dv*4,2) )
      !        allocate(xmaxval(dv) )
      !        allocate(xmaxval(dv) )
      !        allocate(yminval(dv) )
      !        allocate(yminval(dv) )
!
      !        ! get x-max values
      !        do i=1,dv
      !            ymin=dble(i-1)*ywidth+ym
      !            ymax=dble(i)*ywidth+ym
      !            itr=1
      !            do j=1,size(obj%FEMDomain%Mesh%NodCoord,1)
      !                y_tr=obj%FEMDomain%Mesh%NodCoord(j,2)
      !                if(ymin <= y_tr .and. y_tr <= ymax  )then
      !                    if(itr==1)then
      !                        itr=itr+1
      !                        xmax_tr=obj%FEMDomain%Mesh%NodCoord(j,1)
      !                        xmin_tr=obj%FEMDomain%Mesh%NodCoord(j,1)
      !                    else
      !                        if(xmax_tr<=obj%FEMDomain%Mesh%NodCoord(j,1) )then
      !                            xmax_tr=obj%FEMDomain%Mesh%NodCoord(j,1) ! update y-max
      !                            xmaxval(i)=xmax_tr
      !                        endif
      !                        if(xmin_tr>=obj%FEMDomain%Mesh%NodCoord(j,1) )then
      !                            xmin_tr=obj%FEMDomain%Mesh%NodCoord(j,1) ! update y-max
      !                            xminval(i)=xmin_tr
      !                        endif
      !                    endif
      !                else
      !                    cycle
      !                endif
      !            enddo
      !        enddo
!
      !        do i=1,dv
      !            buffer(i,2)=ymaxval(i)
      !        enddo
!
      !        do i=1,dv
      !            buffer(i+dv*2,dv*3)=yminval(i)
      !        enddo
      !
      !        do i=1,dv
      !            buffer(i,2)=ymaxval(i)
      !        enddo
!
      !        do i=1,dv
      !            buffer(i+dv*2,dv*3)=yminval(i)
      !        enddo
      !
!
      !    endif
      !endif
      ! in case of convex
      ! in case of convex

      if (present(r)) then
         max_r = r
      else
         max_r = sqrt(2.10d0)
      end if

      if (present(NumOfMaxNod)) then
         MaxNod = NumOfMaxNod
      else
         MaxNod = 7
      end if

      n = size(obj%FEMDomain%Mesh%NodCoord, 1)
      allocate (KilledPixcel(n))
      KilledPixcel(:) = 0

      ! remove isolated pixcel and surrounded nodes
      do i = 1, n
         point_count = 0
         do j = 1, n

            check_node_id = j
            x_real = obj%FEMDomain%Mesh%NodCoord(i, 1)
            y_real = obj%FEMDomain%Mesh%NodCoord(i, 2)
            z_real = obj%FEMDomain%Mesh%NodCoord(i, 3)
            x_real_tr = obj%FEMDomain%Mesh%NodCoord(j, 1)
            y_real_tr = obj%FEMDomain%Mesh%NodCoord(j, 2)
            z_real_tr = obj%FEMDomain%Mesh%NodCoord(j, 3)

            diff_real = (x_real - x_real_tr)*(x_real - x_real_tr) + &
                        (y_real - y_real_tr)*(y_real - y_real_tr) + &
                        (z_real - z_real_tr)*(z_real - z_real_tr)

            diff_real = dsqrt(diff_real)

            if (diff_real < max_r) then
               point_count = point_count + 1
            end if

            if (point_count > MaxNod) then
               KilledPixcel(i) = 1
               exit
            end if
         end do
         if (point_count == 0) then
            KilledPixcel(i) = 1
         end if
      end do

      call MPIData%GetInfo()
      write (pid, *) MPIData%MyRank
      fh = MPIData%MyRank + 10
      python_buffer = "GetSurface_pid_"//pid//".txt"
      if (present(Name)) then
         python_buffer = Name//"GetSurface_pid_"//pid//".txt"
      end if

      open (fh, file=python_buffer, status="replace")
      do i = 1, n
         if (KilledPixcel(i) == 0) then
            write (fh, *) obj%FEMDomain%Mesh%NodCoord(i, 1:2)
         else
            cycle
         end if
      end do
      close (fh)

      do i = 1, n
         point_count = point_count + KilledPixcel(i)
      end do
      allocate (buffer(n - point_count, 3))
      point_count = 0

      do i = 1, n
         if (KilledPixcel(i) == 0) then
            point_count = point_count + 1
            if (size(obj%FEMDomain%Mesh%NodCoord, 1) < i) then
               print *, "ERROR"
               exit
            end if
            if (size(buffer, 1) < point_count) then
               print *, "ERROR"
               exit
            end if
            buffer(point_count, :) = obj%FEMDomain%Mesh%NodCoord(i, :)
         else
            cycle
         end if
      end do
      deallocate (obj%FEMDomain%Mesh%NodCoord)
      allocate (obj%FEMDomain%Mesh%NodCoord(size(buffer, 1), size(buffer, 2)))

      obj%FEMDomain%Mesh%NodCoord(:, :) = buffer(:, :)

      ! remove clossing
      call unwindLine(obj%FEMDomain%Mesh%NodCoord)

   end subroutine
! #########################################################

! #########################################################
   subroutine AssembleSurfaceElement(obj, MPIData, dim, threshold, DelRange, Name)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      integer(int32), optional, intent(in)        :: dim, threshold, DelRange
      character(*), optional, intent(in)   :: Name
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: pid
      real(real64), allocatable     :: buffer(:, :), r_value(:), line_segment_len(:)
      integer(int32), allocatable     :: checked(:), line_segment(:, :), kill(:)
      real(real64)                 :: x(3), x_tr(3), r_tr, r_ref, a1(2), a2(2), b1(2), b2(2), c1, c2, c3
      real(real64) :: bufvec(2), middle(2)
      integer(int32) :: i, j, k, n, trial_num, id_tr, fh, r_threshold, drange, nn, ys_solved, m

      if (present(threshold)) then
         r_threshold = threshold
      else
         r_threshold = 5
      end if

      if (present(DelRange)) then
         drange = DelRange
      else
         drange = 5
      end if

      if (present(dim) .and. dim /= 2) then
         call MPIData%End()
         print *, "AssembleSurfaceElement :: >> only 2-D is available."
         stop
      end if

      n = size(obj%FEMDomain%Mesh%NodCoord, 1)
      allocate (buffer(n, 3))
      buffer(:, :) = 0.0d0
      allocate (checked(n), r_value(n))
      checked(:) = 0

      buffer(1, :) = obj%FEMDomain%Mesh%NodCoord(1, :)
      checked(1) = 1

      do i = 1, n - 1
         ! get buffer(i+1,:)
         trial_num = 0
         do j = 1, n
            if (checked(j) == 1) then
               cycle
            end if
            trial_num = trial_num + 1
            x(:) = buffer(i, :)
            x_tr(:) = obj%FEMDomain%Mesh%NodCoord(j, :)
            r_tr = dsqrt(dot_product(x - x_tr, x - x_tr))
            if (trial_num == 1) then
               r_ref = r_tr
               id_tr = j
            else
               if (r_ref > r_tr) then
                  id_tr = j
                  r_ref = r_tr
               else
                  cycle
               end if
            end if
         end do
         buffer(i + 1, :) = obj%FEMDomain%Mesh%NodCoord(id_tr, :)
         checked(id_tr) = 1
      end do

      ! remove unnatural pixcel
      checked(:) = 0
      do i = 1, size(buffer, 1) - 1
         x(:) = buffer(i, :)
         x_tr(:) = buffer(i + 1, :)
         r_tr = dsqrt(dot_product(x - x_tr, x - x_tr))
         if (r_tr > dble(r_threshold)) then
            checked(i) = 1
            do k = 1, drange
               if (i + k <= size(buffer, 1)) then
                  checked(i + k) = 1
               end if
               if (i - k >= 1) then
                  checked(i - k) = 1
               end if

            end do
            print *, i, i + 1
         end if
      end do

      if (maxval(checked) /= 0) then
         deallocate (obj%FEMDomain%Mesh%NodCoord)
         allocate (obj%FEMDomain%Mesh%NodCoord(n - sum(checked), 3))

         k = 0
         do i = 1, n
            if (checked(i) /= 0) then
               cycle
            else
               k = k + 1
               obj%FEMDomain%Mesh%NodCoord(k, 1:2) = buffer(i, 1:2)

            end if
         end do
      else
         obj%FEMDomain%Mesh%NodCoord(:, :) = buffer(:, :)
      end if

      write (pid, *) MPIData%MyRank
      fh = MPIData%MyRank + 10
      python_buffer = "GetSurface_pid_"//pid//".txt"
      if (present(Name)) then
         python_buffer = Name//"GetSurface_pid_"//pid//".txt"
      end if

      open (fh, file=python_buffer, status="replace")
      do i = 1, size(obj%FEMDomain%Mesh%NodCoord, 1)
         write (fh, *) obj%FEMDomain%Mesh%NodCoord(i, 1:2)
      end do
      close (fh)

      ! modifier to remove invalid surface nodes
      ! remove solitary island
      ! for 2-D cases
      ! get median of line segments
      n = size(obj%FEMDomain%Mesh%NodCoord, 1)
      allocate (line_segment(n, 3), line_segment_len(n))
      line_segment(:, :) = 0
      do i = 1, n - 1
         line_segment(i, 1) = i
         line_segment(i, 2) = i + 1
      end do
      line_segment(n, 1) = n
      line_segment(n, 2) = 1

      do i = 1, n
         a1(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(i, 1), 1:2)
         a2(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(i, 2), 1:2)
         line_segment_len(i) = dsqrt(dot_product(a2 - a1, a2 - a1))
      end do

      ! write a operation to remove invalid nodes.

      ! remove crossing surface

      do i = 1, n - 2
         a1(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(i, 1), 1:2)
         a2(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(i, 2), 1:2) - a1(1:2)
         ys_solved = 0
         do j = i + 2, n
            b1(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(j, 1), 1:2) - a1(1:2)
            b2(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(j, 2), 1:2) - a1(1:2)
            ! detect crossing by cross product
            c1 = (a2(1)*b1(2) - a2(2)*b1(1))*(a2(1)*b2(2) - a2(2)*b2(1))
            if (c1 > 0.0d0) then
               cycle
            else
               b1(1:2) = b1(1:2) + a1(1:2)
               b2(1:2) = b2(1:2) + a1(1:2)
               a2(1:2) = a2(1:2) + a1(1:2)

               b2(1:2) = b2(1:2) - b1(1:2)
               a1(1:2) = a1(1:2) - b1(1:2)
               a2(1:2) = a2(1:2) - b1(1:2)
               c2 = (b2(1)*a1(2) - b2(2)*a1(1))*(b2(1)*a2(2) - b2(2)*a2(1))
               if (c2 > 0.0d0) then
                  cycle
               else
                  ! crossed

                  nn = line_segment(j, 1)
                  k = i

                  do
                     if (line_segment(k, 2) >= line_segment(nn, 1)) then
                        ys_solved = 1
                        exit
                     end if
                     if (abs(line_segment(k, 2) - line_segment(nn, 1)) <= 1) then
                        ys_solved = 1
                        exit
                     end if

                     print *, "surface-line segments are Crossed >> modification ", line_segment(k, 2), " to ", line_segment(nn, 1)

                     bufvec(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(k, 2), 1:2)
                     obj%FEMDomain%Mesh%NodCoord(line_segment(k, 2), 1:2) = &
                        obj%FEMDomain%Mesh%NodCoord(line_segment(nn, 1), 1:2)
                     obj%FEMDomain%Mesh%NodCoord(line_segment(nn, 1), 1:2) = bufvec(1:2)

                     if (line_segment(nn, 1) - line_segment(k, 2) <= 1) then
                        ys_solved = 1
                        exit
                     end if

                     nn = nn - 1
                     k = k + 1

                  end do
               end if
            end if

            if (ys_solved == 1) then
               exit
            end if
         end do
      end do

      ! reverse
      nn = size(obj%FEMDomain%Mesh%NodCoord, 1)
      do i = 1, size(obj%FEMDomain%Mesh%NodCoord, 1)
         if (nn <= i) then
            exit
         end if
         bufvec(1:2) = obj%FEMDomain%Mesh%NodCoord(i, 1:2)
         obj%FEMDomain%Mesh%NodCoord(i, 1:2) = obj%FEMDomain%Mesh%NodCoord(nn, 1:2)
         obj%FEMDomain%Mesh%NodCoord(nn, 1:2) = bufvec(1:2)
         nn = nn - 1
      end do

      ! kill invalid nodes
      nn = size(obj%FEMDomain%Mesh%NodCoord, 1)
      allocate (kill(size(obj%FEMDomain%Mesh%NodCoord, 1)))
      kill(:) = 0
      do i = 1, nn - 1
         a1(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(i, 1), 1:2) !11
         a2(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(i, 2), 1:2) !9
         b1(1:2) = obj%FEMDomain%Mesh%NodCoord(line_segment(i + 1, 2), 1:2) !10
         middle(1:2) = 0.50d0*a2(1:2) + 0.50d0*a1(1:2) !11-9
         if (dot_product(middle - b1, middle - b1) == 0.0d0) then
            ! invalid node
            kill(line_segment(i + 1, 2)) = 1
            print *, line_segment(i, 2), " will be killed."
         end if
      end do

      if (allocated(buffer)) then
         deallocate (buffer)
      end if

      allocate (buffer(nn - sum(kill), 2))
      k = 0
      do i = 1, nn
         if (kill(i) == 1) then
            cycle
         else
            k = k + 1
            buffer(k, 1:2) = obj%FEMDomain%Mesh%NodCoord(i, 1:2)
         end if
      end do
      deallocate (obj%FEMDomain%Mesh%NodCoord)
      n = size(buffer, 1)
      m = size(buffer, 2)
      allocate (obj%FEMDomain%Mesh%NodCoord(n, m))
      do i = 1, n
         obj%FEMDomain%Mesh%NodCoord(i, 1:2) = buffer(i, 1:2)
      end do

   end subroutine
! #########################################################

! #########################################################
   subroutine ReduceSize(obj, MPIData, interval, Name, auto, curvetol)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      character(*), optional, intent(in)    :: Name
      integer(int32), optional, intent(in) :: interval
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: pid
      real(real64), allocatable:: buffer(:, :)
      integer(int32), allocatable:: chosen(:), kill(:)
      logical, optional, intent(in) :: auto
      integer(int32) :: i, j, k, n, m, fh, itr, id1, id2, id3, killcount, killcount_b
      real(real64), optional, intent(in) :: curvetol
      real(real64) :: x1(2), x2(2), x3(2), x4(2), dp1, dp2, tol

      tol = input(default=0.010d0, option=curvetol)
      if (present(auto)) then
         if (auto .eqv. .true.) then

            n = size(obj%FEMDomain%Mesh%NodCoord, 1)
            m = size(obj%FEMDomain%Mesh%NodCoord, 2)
            allocate (kill(n))
            kill(:) = 0
            do i = 1, n - 2
               if (kill(i) == 1) then
                  cycle
               end if
               do
                  x1(1:2) = obj%FEMDomain%Mesh%NodCoord(i, 1:2)
                  x2(1:2) = obj%FEMDomain%Mesh%NodCoord(i + 1, 1:2)
                  x3(1:2) = obj%FEMDomain%Mesh%NodCoord(i + 2, 1:2)
                  x4(1:2) = 0.50d0*x1(1:2) + 0.50d0*x2(1:2)
                  dp1 = dot_product(x3 - x4, x3 - x4)
                  dp2 = dot_product(x1 - x4, x1 - x4)
                  if (dp1/dp2 < tol) then
                     kill(i + 1) = 1
                     cycle
                  else
                     exit
                  end if
               end do
            end do

            ! kill nodes
            allocate (buffer(n - sum(kill), 2))
            itr = 1
            do i = 1, n
               if (kill(i) == 1) then
                  buffer(itr, 1:2) = obj%FEMDomain%Mesh%NodCoord(i, 1:2)
                  itr = itr + 1
               else
                  cycle
               end if
            end do
            deallocate (obj%FEMDomain%Mesh%NodCoord)
            allocate (obj%FEMDomain%Mesh%NodCoord(size(buffer, 1), size(buffer, 2)))
            obj%FEMDomain%Mesh%NodCoord(:, :) = buffer(:, :)
            return
         end if
      end if

      n = size(obj%FEMDomain%Mesh%NodCoord, 1)
      m = size(obj%FEMDomain%Mesh%NodCoord, 2)
      allocate (chosen(n))
      chosen(:) = 0
      k = 0
      do i = 1, n
         k = k + 1
         if (k == interval) then
            chosen(i) = 1
            k = 0
         else
            cycle
         end if
      end do

      allocate (buffer(sum(chosen), 3))

      k = 0
      do i = 1, n
         if (chosen(i) == 1) then
            k = k + 1
            buffer(k, :) = obj%FEMDomain%Mesh%NodCoord(i, :)
         end if
      end do

      deallocate (obj%FEMDomain%Mesh%NodCoord)

      allocate (obj%FEMDomain%Mesh%NodCoord(size(buffer, 1), size(buffer, 2)))
      obj%FEMDomain%Mesh%NodCoord(:, :) = buffer(:, :)

      write (pid, *) MPIData%MyRank
      fh = MPIData%MyRank + 10
      python_buffer = "GetSurface_pid_"//pid//".txt"

      if (present(Name)) then
         python_buffer = Name//"GetSurface_pid_"//pid//".txt"
      end if
      open (fh, file=python_buffer, status="replace")
      do i = 1, size(obj%FEMDomain%Mesh%NodCoord, 1)
         write (fh, *) obj%FEMDomain%Mesh%NodCoord(i, 1:2)
      end do
      close (fh)

   end subroutine
! #########################################################

! #########################################################
   subroutine ExportGeoFile(obj, MPIData, Name)
      class(PreProcessing_), intent(inout):: obj
      character(*), optional, intent(in)    :: Name
      class(MPI_), intent(inout)          :: MPIData
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: pid
      integer(int32) :: i, j, k, n, fh, xsize
      real(real64) :: x_p, y_p

      write (pid, *) MPIData%MyRank
      fh = MPIData%MyRank + 10
      python_buffer = "GetSurface_pid_"//pid//".geo"

      if (present(Name)) then
         python_buffer = name

      end if

      xsize = size(obj%FEMDomain%Mesh%NodCoord, 1)
      open (fh, file=python_buffer)
      do i = 1, xsize

         x_p = obj%FEMDomain%Mesh%NodCoord(i, 1)
         y_p = obj%FEMDomain%Mesh%NodCoord(i, 2)
         write (fh, *) "Point(", i, ") = {", x_p, ",", y_p, ",", "0, 1.0};"
      end do

      write (fh, *) "Line(1) = {", xsize, ",1};"
      do i = 2, xsize
         write (fh, *) "Line(", i, ") = {", i - 1, ",", i, "};"
      end do
      write (fh, *) "Line Loop(", xsize + 1, ") = {"
      do i = 1, xsize - 1
         write (fh, *) i, ","
      end do
      write (fh, *) xsize, "};"
      write (fh, *) "Plane Surface(", xsize + 2, ") = {", xsize + 1, "};"
      flush (fh)
      close (fh)
      !print *, "Done !!"
      !print *, "Generating mesh..."
      !call execute_command_line("gmsh.exe pm2.geo -2 -algo del2d -clmin 40")
   end subroutine
! #########################################################

! #########################################################
   subroutine ConvertGeo2Msh(obj, MPIData, Name, clmin, clmax)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      character(*), optional, intent(in)    :: Name
      real(real64), optional, intent(in) :: clmin, clmax
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: command
      character(:), allocatable    :: pid
      integer(int32) :: i, j, k, n, fh
      real(real64) :: cmin, cmax

      write (pid, *) MPIData%MyRank

      cmin = input(default=100.0d0, option=clmin)
      cmax = input(default=100000.0d0, option=clmax)
      fh = MPIData%MyRank + 10
      python_buffer = " "
      command = " "
      python_buffer = "GetSurface_pid_"//pid//".geo"
      if (present(Name)) then
         python_buffer = name
      end if
      command = "gmsh "//python_buffer//" -2 -algo del2d -clmin "//fstring(cmin) &
                //" -clmax "//fstring(cmax)

      writE (*, '(A)') obj%PictureName

      call execute_command_line(command)

      !call execute_command_line("sh ./MakeMesh.sh")

   end subroutine
! #########################################################

! #########################################################
   subroutine ConvertGeo2VTK(obj, MPIData, Name, clmin, clmax)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      character(*), optional, intent(in)    :: Name
      real(real64), optional, intent(in) :: clmin, clmax
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: command
      character(:), allocatable  :: pid
      integer(int32) :: i, j, k, n, fh
      real(real64) :: cmin, cmax

      write (pid, *) MPIData%MyRank

      cmin = input(default=100.0d0, option=clmin)
      cmax = input(default=100000.0d0, option=clmax)
      fh = MPIData%MyRank + 10
      python_buffer = " "
      command = " "
      python_buffer = "GetSurface_pid_"//pid//".geo"
      if (present(Name)) then
         python_buffer = name
      end if
      command = "gmsh "//python_buffer//" -2 -algo del2d -clmin "//fstring(cmin) &
                //" -clmax "//fstring(cmax)//" -format vtk"

      writE (*, '(A)') obj%PictureName

      call execute_command_line(command)

      !call execute_command_line("sh ./MakeMesh.sh")

   end subroutine
! #########################################################

! #########################################################
   subroutine ConvertGeo2Inp(obj, MPIData, Name, clmin, clmax)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      character(*), optional, intent(in)    :: Name
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: command
      character(:), allocatable    :: pid
      integer(int32) :: i, j, k, n, fh
      real(real64) :: cmin, cmax
      real(real64), optional, intent(in) :: clmin, clmax

      write (pid, *) MPIData%MyRank
      cmin = input(default=100.0d0, option=clmin)
      cmax = input(default=100000.0d0, option=clmax)

      fh = MPIData%MyRank + 10
      python_buffer = " "
      command = " "
      python_buffer = "GetSurface_pid_"//pid//".geo"
      if (present(Name)) then
         python_buffer = name
      end if
      !command="gmsh "//python_buffer//" -2 -algo del2d -clmin 100 -clmax 100000 -format inp"
      command = "gmsh "//python_buffer//" -2 -algo del2d -clmin "//fstring(cmin)// &
                " -clmax "//fstring(cmax)//" -format inp"

      writE (*, '(A)') obj%PictureName

      call execute_command_line(obj%PictureName)
      !call execute_command_line("sh ./MakeMesh.sh")

   end subroutine
! #########################################################

! #########################################################
   subroutine ConvertGeo2Mesh(obj, MPIData, SizePara, Name, clmin, clmax)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      integer(int32), optional, intent(in) :: SizePara
      character(*), optional, intent(in)    :: Name
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: command
      character(:), allocatable    :: pid, a
      integer(int32) :: i, j, k, n, fh, sp
      real(real64) :: cmin, cmax
      real(real64), optional, intent(in) :: clmin, clmax

      cmin = input(default=100.0d0, option=clmin)
      cmax = input(default=100000.0d0, option=clmax)

      if (present(SizePara)) then
         sp = SizePara
      else
         sp = 100
      end if
      write (a, *) sp

      write (pid, *) MPIData%MyRank

      fh = MPIData%MyRank + 10
      python_buffer = " "
      command = " "
      python_buffer = "GetSurface_pid_"//pid//".geo"
      if (present(Name)) then
         python_buffer = name
      end if
      command = "gmsh "//python_buffer//" -2 -algo del2d -clmin "//fstring(cmin) &
                //" -clmax "//fstring(cmax)//" -format mesh"

      writE (*, '(A)') obj%PictureName

      call execute_command_line(command)

      !call execute_command_line("./MakeMesh.sh")

   end subroutine
! #########################################################

! #########################################################
   subroutine ConvertMsh2Scf(obj, MPIData, ElementType, Name)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), intent(inout)          :: MPIData
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: command, infile, outfile
      character(*), optional, intent(in) :: ElementType
      character(*), optional, intent(in)    :: Name
      character(:), allocatable ::  pid
      character(:), allocatable ::  MeshFormat
      character(:), allocatable ::  EndMeshFormat
      character(:), allocatable ::   Nodes
      character(:), allocatable ::   EndNodes, Elements
      character(:), allocatable ::   EndElements
      integer(int32), allocatable :: elem1(:), surface_nod(:)
      integer(int32) :: i, j, k, n, n1, n2, fh, a, nm, mm, nod_num, nn, elem_num, surf_num
      integer(int32) :: elem_num_all, n3, n4, n5, n6, n7, elemnod_num, startfrom
      real(real64) :: re1, re2

      ! ======================================================
      ! deallocate all
      if (allocated(obj%FEMDomain%Mesh%NodCoord)) then
         deallocate (obj%FEMDomain%Mesh%NodCoord)
      end if

      if (allocated(obj%FEMDomain%Mesh%ElemNod)) then
         deallocate (obj%FEMDomain%Mesh%ElemNod)
      end if
      ! ======================================================

      ! ======================================================
      write (pid, *) MPIData%MyRank
      fh = MPIData%MyRank + 10
      infile = "GetSurface_pid_"//pid//".msh"
      outfile = "GetSurface_pid_"//pid//".scf"

      if (present(Name)) then
         infile = Name//"GetSurface_pid_"//pid//".msh"
         outfile = Name//"GetSurface_pid_"//pid//".scf"
      end if

      open (fh, file=infile, status="old")
      print *, "Opening ", infile
      ! ======================================================

      ! ======================================================
      !read file to get nod and elem number
      read (fh, *) MeshFormat
      read (fh, *) re1, nm, mm
      read (fh, *) EndMeshFormat
      read (fh, *) Nodes
      if (nodes /= "$Nodes") then
         if (nodes == "$Entit") then
            do
               read (fh, *) nodes
               if (nodes == "$Nodes") then
                  print *, "Read entities"
                  exit
               end if
            end do
         else
            stop "ERROR: invalid location:$Nodes"
         end if
      end if
      ! ======================================================
      ! Number of nodes
      read (fh, *) nod_num
      !print *,"nod_number",nod_num
      if (allocated(obj%FEMDomain%Mesh%NodCoord)) then
         deallocate (obj%FEMDomain%Mesh%NodCoord)
      end if
      allocate (obj%FEMDomain%Mesh%NodCoord(nod_num, 3))
      do i = 1, nod_num
         read (fh, *) a
         if (i /= a) then
            stop "ERROR :: msh2scf"
         end if
      end do
      read (fh, *) EndNodes

      if (EndNodes /= "$EndNodes") then
         stop "ERROR: invalid location:$EndNodes"
      end if
      ! ======================================================

      ! ======================================================
      read (fh, *) Elements
      if (Elements /= "$Elements") then
         stop "ERROR: invalid location: $Elements"
      end if

      read (fh, *) elem_num
      if (present(ElementType)) then
         if (ElementType == "LinearRectangularGp4") then
            elemnod_num = 4
         elseif (ElementType == "LinearHexahedralGp8") then
            elemnod_num = 8
         else
            print *, "PreProcessingClass.f90  >> Element : ", ElementType, "is not defined."
            return
         end if
      else
         ! default
         elemnod_num = 4
      end if

      startfrom = 0
      k = 0
      do i = 1, elem_num
         read (fh, *) n1, n2, n3
         if (n2 == 3 .and. n3 == 2) then
            if (startfrom == 0) then
               startfrom = i
            end if
            k = k + 1
         end if
      end do
      allocate (obj%FEMDomain%Mesh%ElemNod(k, elemnod_num))
      allocate (elem1(elemnod_num))

      read (fh, *) EndElements
      if (EndElements /= "$EndElements") then
         stop "ERROR: invalid location: $EndElements"
      end if
      close (fh)
      ! ========================================================

      ! ======================================================
      write (pid, *) MPIData%MyRank
      fh = MPIData%MyRank + 10
      infile = "GetSurface_pid_"//pid//".msh"
      outfile = "GetSurface_pid_"//pid//".scf"
      if (present(Name)) then

         infile = Name//"GetSurface_pid_"//pid//".msh"
         outfile = Name//"GetSurface_pid_"//pid//".scf"

      end if

      print *, "File Information is imported."
      open (fh, file=infile, status="old")

      ! ======================================================

      ! ======================================================
      !read file to get nod and elem number
      read (fh, *) MeshFormat
      read (fh, *) re1, nm, mm
      read (fh, *) EndMeshFormat
      read (fh, *) Nodes
      if (nodes /= "$Nodes") then
         stop "ERROR: invalid location:$Nodes"
      end if

      ! ======================================================
      ! Number of nodes
      read (fh, *) nod_num
      !print *,"nod_number",nod_num
      if (allocated(obj%FEMDomain%Mesh%NodCoord)) then
         deallocate (obj%FEMDomain%Mesh%NodCoord)
      end if
      allocate (obj%FEMDomain%Mesh%NodCoord(nod_num, 3))
      do i = 1, nod_num
         read (fh, *) n2, obj%FEMDomain%Mesh%NodCoord(i, :)
      end do
      read (fh, *) EndNodes

      if (EndNodes /= "$EndNodes") then
         stop "ERROR: invalid location:$EndNodes"
      end if
      ! ======================================================

      ! ======================================================
      read (fh, *) Elements
      if (Elements /= "$Elements") then
         stop "ERROR: invalid location: $Elements"
      end if

      read (fh, *) elem_num
      if (present(ElementType)) then
         if (ElementType == "LinearRectangularGp4") then
            elemnod_num = 4
         elseif (ElementType == "LinearHexahedralGp8") then
            elemnod_num = 8
         else
            print *, "PreProcessingClass.f90  >> Element : ", ElementType, "is not defined."
            return
         end if
      else
         ! default
         elemnod_num = 4
      end if

      k = 0
      do i = 1, elem_num
         if (startfrom > i) then
            read (fh, *) n1, n2, n3
            cycle
         end if

         if (i <= size(obj%FEMDomain%Mesh%ElemNod, 1) + startfrom - 1) then
            k = k + 1
            read (fh, *) n1, n2, n3, n4, n5, obj%FEMDomain%Mesh%ElemNod(k, 1:elemnod_num)
            cycle
         end if

         if (size(obj%FEMDomain%Mesh%ElemNod, 1) + startfrom >= i) then
            read (fh, *) n1
         end if
      end do

      read (fh, *) EndElements
      if (EndElements /= "$EndElements") then
         stop "ERROR: invalid location: $EndElements"
      end if
      close (fh)

      ! ========================================================

      ! Setup FEMDomain

   end subroutine
! #########################################################

! #########################################################
   subroutine ConvertMesh2Scf(obj, MPIData, ElementType, Name)
      class(PreProcessing_), intent(inout):: obj
      class(MPI_), optional, intent(inout)          :: MPIData
      type(Mesh_) :: tobj
      character(*), optional, intent(in) :: Name
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: command, infile, outfile
      character(*), optional, intent(in) :: ElementType
      character(:), allocatable ::  pid
      character(:), allocatable ::  MeshFormat
      character(:), allocatable ::  EndMeshFormat
      character(:), allocatable ::   Nodes
      character(:), allocatable ::   EndNodes, Elements
      character(:), allocatable ::   EndElements
      integer(int32), allocatable :: elem1(:), surface_nod(:), triangle(:, :), devide_line(:, :), buffer(:, :)
      integer(int32) :: i, j, k, n, n1, n2, fh, a, nm, mm, nod_num, nn, elem_num, surf_num, l, numnum
      integer(int32) :: elem_num_all, n3, n4, n5, n6, n7, elemnod_num, startfrom, node1, node2, tr1, tr2
      real(real64) :: re1, re2

      print *, "ConvertMesh2Scf >>>> only for 2D"
      obj%FEMDomain%Mesh%ElemType = "LinearRectangularGp4"
      ! ======================================================
      ! deallocate all
      if (allocated(obj%FEMDomain%Mesh%NodCoord)) then
         deallocate (obj%FEMDomain%Mesh%NodCoord)
      end if

      if (allocated(obj%FEMDomain%Mesh%ElemNod)) then
         deallocate (obj%FEMDomain%Mesh%ElemNod)
      end if
      ! ======================================================

      ! ======================================================
      numnum = input(default=1, option=MPIData%MyRank)
      write (pid, *) numnum
      fh = input(default=1, option=MPIData%MyRank + 10)
      infile = "GetSurface_pid_"//pid//".mesh"
      outfile = "GetSurface_pid_"//pid//".scf"
      if (present(Name)) then
         infile = Name
         outfile = Name//".scf"

      end if

      open (fh, file=infile, status="old")
      print *, "Opening ", infile
      ! ======================================================

      ! ======================================================
      !read file to get nod and elem number
      read (fh, *) MeshFormat
      if (MeshFormat /= "MeshVersionFormatted") then
         print *, "ConvertMesh2Scf ERROR :: ", MeshFormat
      end if
      read (fh, *) MeshFormat
      read (fh, *) mm
      do
         MeshFormat = " "
         read (fh, *) MeshFormat
         if (MeshFormat == "Vertices") then
            print *, "ConvertMesh2Scf reading ", MeshFormat
            ! ======================================================
            ! Number of nodes
            read (fh, *) nod_num
            !print *,"nod_number",nod_num
            if (allocated(obj%FEMDomain%Mesh%NodCoord)) then
               deallocate (obj%FEMDomain%Mesh%NodCoord)
            end if
            allocate (obj%FEMDomain%Mesh%NodCoord(nod_num, 2))
            obj%FEMDomain%Mesh%NodCoord(:, :) = 0.0d0
            do i = 1, nod_num
               read (fh, *) obj%FEMDomain%Mesh%NodCoord(i, 1:2)
            end do

            cycle
            ! ======================================================
         elseif (MeshFormat == "Quadrilaterals") then
            ! ======================================================
            print *, "ConvertMesh2Scf reading ", MeshFormat
            read (fh, *) elem_num
            allocate (obj%FEMDomain%Mesh%ElemNod(elem_num, 4))
            obj%FEMDomain%Mesh%ElemNod(:, :) = -1
            do i = 1, elem_num
               read (fh, *) obj%FEMDomain%Mesh%ElemNod(i, 1:4)
            end do
            exit
            ! ======================================================
         elseif (MeshFormat == "Triangles") then

            ! ======================================================
            print *, "ConvertMesh2Scf reading ", MeshFormat
            read (fh, *) mm
            allocate (tobj%ElemNod(mm, 3))

            do i = 1, mm
               read (fh, *) tobj%ElemNod(i, 1:3)
            end do
            ! ======================================================
         else
            print *, "ConvertMesh2Scf Skipped", MeshFormat
            if (MeshFormat == "End") then
               exit
            end if
            read (fh, *) mm
            do i = 1, mm
               read (fh, *) n
            end do
         end if

      end do

      ! ======================================================
      !
      !if(allocated(obj%FEMDomain%Mesh%ElemMat) )then
      !    deallocate(obj%FEMDomain%Mesh%ElemMat)
      !endif
      !allocate(obj%FEMDomain%Mesh%ElemMat(elem_num))
      !obj%FEMDomain%Mesh%ElemMat(:)=1
      ! ======================================================

      ! convert triangle
      if (.not. allocated(tobj%ElemNod)) then
         print *, "No triangles"
         return
      end if

      allocate (tobj%NodCoord(size(obj%FEMDomain%Mesh%NodCoord, 1), 2))
      tobj%NodCoord(:, 1:2) = obj%FEMDomain%Mesh%NodCoord(:, 1:2)

      call tobj%convertMeshType(option="convertTriangleToRectangular")

      if (allocated(obj%FEMDomain%Mesh%ElemNod)) then
         print *, "triangular and rectangurar => ignore triangular"
         return
      else
         print *, "triangular => converted."
         allocate (obj%FEMDomain%Mesh%ElemNod(sizE(tobj%ElemNod, 1), 1:4))
         deallocate (obj%FEMDomain%Mesh%NodCoord)
         allocate (obj%FEMDomain%Mesh%NodCoord(size(tobj%NodCoord, 1), size(tobj%NodCoord, 2)))
         obj%FEMDomain%Mesh%NodCoord(:, :) = tobj%NodCoord(:, :)
         obj%FEMDomain%Mesh%ElemNod(:, :) = tobj%ElemNod(:, :)

      end if

      print *, "surface information is updated"
      call obj%FEMDomain%Mesh%GetSurface()

      return

      do i = 1, size(devide_line, 1)
         do j = 1, 3
            if (i == 1) then
               node1 = triangle(i, 1)
               node2 = triangle(i, 2)
            elseif (i == 2) then
               node1 = triangle(i, 2)
               node2 = triangle(i, 3)
            else
               node1 = triangle(i, 3)
               node2 = triangle(i, 1)
            end if
            do k = 1, size(obj%FEMDomain%Mesh%ElemNod, 1)
               do l = 1, size(obj%FEMDomain%Mesh%ElemNod, 2)
                  if (l == 1) then
                     tr1 = obj%FEMDomain%Mesh%ElemNod(k, 1)
                     tr2 = obj%FEMDomain%Mesh%ElemNod(k, 2)
                  elseif (l == 2) then
                     tr1 = obj%FEMDomain%Mesh%ElemNod(k, 2)
                     tr2 = obj%FEMDomain%Mesh%ElemNod(k, 3)
                  elseif (l == 3) then
                     tr1 = obj%FEMDomain%Mesh%ElemNod(k, 3)
                     tr2 = obj%FEMDomain%Mesh%ElemNod(k, 4)
                  elseif (l == 4) then
                     tr1 = obj%FEMDomain%Mesh%ElemNod(k, 4)
                     tr2 = obj%FEMDomain%Mesh%ElemNod(k, 1)
                  else
                     stop "ERROR :: ConvertMesh2Scf"
                  end if
                  if (node2 == tr1 .and. node1 == tr2) then
                     devide_line(i, j) = 1
                  end if
               end do
            end do
         end do
      end do

   end subroutine
! #########################################################

! #########################################################
   subroutine ExportPreProcessing(obj, MPIData, FileName, MeshDimension, Name, regacy, with)
      class(PreProcessing_), intent(inout):: obj
      class(PreProcessing_), optional, intent(inout):: with
      class(MPI_), optional, intent(inout) :: MPIData
      character(*), optional, intent(in)  :: FileName
      character(*), optional, intent(in)   :: Name
      integer(int32), optional, intent(in) :: MeshDimension
      logical, optional, intent(in)::regacy
      character(:), allocatable   :: python_buffer
      character(:), allocatable   :: command
      character(:), allocatable    :: pid
      integer(int32) :: i, j, k, n, fh

      fh = 11
      if (present(MPIData)) then
         write (pid, *) MPIData%MyRank
         fh = MPIData%MyRank + 120
         if (present(Name)) then
            python_buffer = Name//"GetSurface_pid_"//pid
         else
            python_buffer = "GetSurface_pid_"//pid

         end if
      elseif (present(FileName)) then
         if (present(Name)) then
            python_buffer = Name//FileName
         else
            python_buffer = FileName
         end if
      else
         if (present(Name)) then
            python_buffer = Name
         else
            python_buffer = "NoName"
         end if
      end if

      call obj%FEMDomain%Export(OptionalProjectName=python_buffer, FileHandle=fh, Name=Name, regacy=regacy &
                                , with=with%FEMDomain)

   end subroutine
! #########################################################

!##################################################
   subroutine SetSolverPreProcessing(obj, inSolverType)
      class(PreProcessing_), intent(inout)::obj
      character(*), optional, intent(in) :: inSolverType
      character(:), allocatable ::sn

      if (present(inSolverType)) then
         sn = inSolverType
      else
         sn = "Default"
      end if

      Obj%FEMDomain%SolverType = sn

   end subroutine
!##################################################

!##################################################
   subroutine SetDataTypeFEMDomain(obj, inDType)
      class(PreProcessing_), intent(inout)::obj
      character(*), optional, intent(in) :: inDType
      character(:), allocatable :: sn

      sn = ""

      if (.not. present(inDType)) then
         sn = "FEMDomain"
      else
         sn = inDType
      end if

      call obj%FEMDomain%SetDataType(sn)

   end subroutine

!##################################################

!##################################################
   subroutine SetUpPreprocessing(obj, DataType, SolverType, NoFacetMode, MatPara)
      class(PreProcessing_), intent(inout)::obj
      character(*), optional, intent(in) :: DataType
      character(*), optional, intent(in) :: SolverType
      logical, optional, intent(in) :: NoFacetMode
      real(real64), allocatable, optional, intent(inout)::MatPara(:, :)
      real(real64), allocatable::MatParaDef(:, :)
      character(:), allocatable :: sn

      if (present(DataType)) then
         call obj%SetDataType(DataType)
      else
         call obj%SetDataType()
      end if

      call obj%FEMDomain%Mesh%Init(NoFacetMode=NoFacetMode)
      if (.not. present(MatPara)) then
         allocate (MatParaDef(1, 1))
         MatParaDef(:, :) = 1.0d0
         call obj%FEMDomain%MaterialProp%Init(MaterialParameters=MatParaDef)
      else
         call obj%FEMDomain%MaterialProp%Init(MaterialParameters=MatPara)
      end if

   end subroutine
!##################################################

!##################################################
   subroutine SetScalePreProcessing(obj, scalex, scaley, scalez &
                                    , picscalex, picscaley, picscalez, xratio, yratio)
      class(PreProcessing_), intent(inout)::obj
      real(real64), optional, intent(in)::scalex, scaley, scalez
      real(real64), optional, intent(in)::picscalex, picscaley, picscalez, xratio, yratio
      real(real64) :: lx, ly, lz
      integer(int32) :: i

      if (present(xratio)) then
         do i = 1, size(obj%FEMDomain%Mesh%NodCoord, 1)
            obj%FEMDomain%Mesh%NodCoord(i, 1) = xratio*obj%FEMDomain%Mesh%NodCoord(i, 1)
         end do
      end if
      if (present(yratio)) then
         do i = 1, size(obj%FEMDomain%Mesh%NodCoord, 1)
            obj%FEMDomain%Mesh%NodCoord(i, 2) = yratio*obj%FEMDomain%Mesh%NodCoord(i, 2)
         end do
      end if
      if (present(yratio) .or. present(xratio)) then
         return
      end if

      if (present(scalex)) then
         if (scalex == 0.0d0) then
            stop "ERROR :: SetScalePreProcessing >> scalex==0.0d0"
            return
         else
            lx = maxval(obj%FEMDomain%Mesh%NodCoord(:, 1)) - &
                 minval(obj%FEMDomain%Mesh%NodCoord(:, 1))
            if (lx == 0.0d0) then
               stop "ERROR :: SetScalePreProcessing >> lx==0.0d0"
            end if
            obj%FEMDomain%Mesh%NodCoord(:, 1) = &
               obj%FEMDomain%Mesh%NodCoord(:, 1)/lx*scalex
         end if
      end if

      if (present(scaley)) then
         if (scaley == 0.0d0) then
            stop "ERROR :: SetScalePreProcessing >> scaley==0.0d0"
            return
         else
            ly = maxval(obj%FEMDomain%Mesh%NodCoord(:, 2)) - &
                 minval(obj%FEMDomain%Mesh%NodCoord(:, 2))
            if (ly == 0.0d0) then
               stop "ERROR :: SetScalePreProcessing >> ly==0.0d0"
            end if
            obj%FEMDomain%Mesh%NodCoord(:, 2) = &
               obj%FEMDomain%Mesh%NodCoord(:, 2)/ly*scaley
         end if
      end if
      if (present(scalez)) then
         if (scalez == 0.0d0) then
            stop "ERROR :: SetScalePreProcessing >> scalez==0.0d0"
            return
         else
            lz = maxval(obj%FEMDomain%Mesh%NodCoord(:, 3)) - &
                 minval(obj%FEMDomain%Mesh%NodCoord(:, 3))
            if (lz == 0.0d0) then
               stop "ERROR :: SetScalePreProcessing >> lz==0.0d0"
            end if
            obj%FEMDomain%Mesh%NodCoord(:, 3) = &
               obj%FEMDomain%Mesh%NodCoord(:, 3)/lz*scalez
         end if
      end if

      do i = 1, size(obj%FEMDomain%Mesh%NodCoord, 1)
         write (123, *) obj%FEMDomain%Mesh%NodCoord(i, :)
      end do

   end subroutine
!##################################################

!##################################################
   subroutine SetBoundaryConditionPrePro(obj, Dirichlet, Neumann, Initial, xmin, xmax, ymin, ymax, zmin, zmax, &
                                         tmin, tmax, val, val_id, NumOfValPerNod, BoundInfo, MPIData)
      class(PreProcessing_), intent(inout)::obj
      type(preprocessing_) :: DBC
      class(Dictionary_), optional, intent(in) :: BoundInfo
      class(MPI_), optional, intent(inout) :: MPIData
      real(real64), optional, intent(in)::xmin, xmax
      real(real64), optional, intent(in)::ymin, ymax
      real(real64) :: x_min, x_max
      real(real64) :: y_min, y_max
      real(real64), optional, intent(in)::zmin, zmax
      real(real64), optional, intent(in)::tmin, tmax
      logical, optional, intent(in)::Dirichlet, Neumann, Initial
      integer(int32), optional, intent(in)::NumOfValPerNod, val_id
      real(real64), optional, intent(in)::val
      integer(int32) :: i, j, n, k, l

      if (present(BoundInfo)) then
         if (.not. allocated(BoundInfo%pages)) then
            return
         end if

         do i = 1, BoundInfo%sizeof()
            ! list of boundary conditions is given as figures
            print *, "now dbc"
            if (.not. present(MPIData)) then
               print *, "ERROR :: setBC :: MPIData should be imported."
               return
            end if
            print *, BoundInfo%content(i)

            call DBC%ImportPictureName(BoundInfo%content(i))
            call DBC%GetPixcelSize(MPIData, name="DBC")
            call DBC%SetColor(BoundInfo%IntList(i, 1), &
                              BoundInfo%IntList(i, 2), BoundInfo%IntList(i, 3))

            call DBC%GetPixcelByRGB(MPIData, err=5, onlycoord=.true.)

            x_min = minval(DBC%FEMDomain%Mesh%NodCoord(:, 1))
            x_max = maxval(DBC%FEMDomain%Mesh%NodCoord(:, 1))
            y_min = minval(DBC%FEMDomain%Mesh%NodCoord(:, 2))
            y_max = maxval(DBC%FEMDomain%Mesh%NodCoord(:, 2))

            ! debug
            !print *, minval(obj%FEMDomain%Mesh%NodCoord(:,1)),&
            !maxval(obj%FEMDomain%Mesh%NodCoord(:,1)),&
            !minval(obj%FEMDomain%Mesh%NodCoord(:,2)),&
            !maxval(obj%FEMDomain%Mesh%NodCoord(:,2))
            n = size(obj%FEMDomain%Mesh%NodCoord, 2)
            call obj%setBC(Dirichlet=.true., xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, &
                           val_id=BoundInfo%intvalue(i), val=BoundInfo%realvalue(i), NumOfValPerNod=n)

            print *, "boundary condition ID : ", i
            call DBC%finalize()
         end do
         return
      end if

      if (present(Dirichlet)) then
         if (Dirichlet .eqv. .true.) then

            call AddDBoundCondition(obj%FEMDomain, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax &
                                , zmin=zmin, zmax=zmax, tmin=tmin, tmax=tmax, val=val, val_id=val_id, NumOfValPerNod=NumOfValPerNod)
            print *, "boundary conditions are added."
            if (.not. allocated(obj%FEMDomain%Boundary%DBoundNum)) then
               n = size(obj%FEMDomain%Boundary%DBoundNodID, 2)
               allocate (obj%FEMDomain%Boundary%DBoundNum(n))
               print *, "caution .not. allocated(obj%FEMDomain%Boundary%DBoundNum "
            end if
            do i = 1, size(obj%FEMDomain%Boundary%DBoundNum)
               k = countif(Array=obj%FEMDomain%Boundary%DBoundNodID(:, i), Value=-1, notEqual=.true.)
               obj%FEMDomain%Boundary%DBoundNum(i) = k
            end do

         end if
      end if

      if (present(Neumann)) then
         if (Neumann .eqv. .true.) then

            call AddNBoundCondition(obj%FEMDomain, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax &
                                , zmin=zmin, zmax=zmax, tmin=tmin, tmax=tmax, val=val, val_id=val_id, NumOfValPerNod=NumOfValPerNod)

            return
         end if
      end if

      if (present(Initial)) then
         if (Initial .eqv. .true.) then

            call AddTBoundCondition(obj%FEMDomain, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax &
                                , zmin=zmin, zmax=zmax, tmin=tmin, tmax=tmax, val=val, val_id=val_id, NumOfValPerNod=NumOfValPerNod)

            return
         end if
      end if

   end subroutine
!##################################################

!##################################################
   subroutine SetSizeOfBCPrePrecessing(obj, Dirichlet, Neumann, Initial, NumOfValue)
      class(PreProcessing_), intent(inout)::obj

      logical, optional, intent(in)::Dirichlet, Neumann, Initial
      integer(int32), optional, intent(in)::NumOfValue
      logical :: DB, NB, IC
      integer(int32) :: coord_dim

      if (.not. present(Dirichlet)) then
         DB = .false.
      else
         DB = Dirichlet
      end if

      if (.not. present(Neumann)) then
         NB = .false.
      else
         NB = Neumann
      end if

      if (.not. present(Initial)) then
         IC = .false.
      else
         IC = Initial
      end if

      coord_dim = size(obj%FEMDomain%Mesh%NodCoord, 2)
      if (DB .eqv. .true.) then
         call obj%FEMDomain%InitDBC(NumOfValue)
      elseif (NB .eqv. .true.) then
         call obj%FEMDomain%InitNBC(NumOfValue)
      elseif (IC .eqv. .true.) then
         call obj%FEMDomain%InitTBC(NumOfValue)
      else
         return
      end if

   end subroutine
!##################################################

!##################################################
   subroutine ShowBCPrePrecessing(obj, Dirichlet, Neumann, Initial)
      class(PreProcessing_), intent(inout)::obj

      logical, optional, intent(in)::Dirichlet, Neumann, Initial

      integer(int32) :: n, m1, m2, i

      if (Dirichlet .eqv. .true.) then
         n = size(obj%FEMDomain%Boundary%DBoundNum)
         m1 = size(obj%FEMDomain%Boundary%DBoundNodID, 1)
         m2 = size(obj%FEMDomain%Boundary%DBoundVal, 1)
         print *, "Number of boundary conditions are :: ", n
         print *, "obj%FEMDomain%Boundary%DBoundNodID"
         do i = 1, m1
            print *, obj%FEMDomain%Boundary%DBoundNodID(i, :)
         end do
         do i = 1, m2
            print *, obj%FEMDomain%Boundary%DBoundVal(i, :)
         end do

      end if

   end subroutine

!##################################################

!##################################################
   subroutine Convert3Dto2D(obj)
      class(PreProcessing_), intent(inout)::obj
      real(real64), allocatable::buffer(:, :)
      integer(int32) :: i, n, m

      n = size(obj%FEMDomain%Mesh%NodCoord, 1)
      m = size(obj%FEMDomain%Mesh%NodCoord, 2)

      allocate (buffer(n, 2))

      do i = 1, n
         buffer(i, 1:2) = obj%FEMDomain%Mesh%NodCoord(i, 1:2)
      end do
      deallocate (obj%FEMDomain%Mesh%NodCoord)
      allocate (obj%FEMDomain%Mesh%NodCoord(n, 2))
      obj%FEMDomain%Mesh%NodCoord(:, :) = buffer(:, :)
      deallocate (buffer)
      if (allocated(obj%FEMDomain%Mesh%ElemMat)) then
         if (size(obj%FEMDomain%Mesh%ElemMat) /= size(obj%FEMDomain%Mesh%ElemNod, 1)) then
            deallocate (obj%FEMDomain%Mesh%ElemMat)
         end if
      end if
      if (.not. allocated(obj%FEMDomain%Mesh%ElemMat)) then
         n = size(obj%FEMDomain%Mesh%ElemNod, 1)
         allocate (obj%FEMDomain%Mesh%ElemMat(n))
         obj%FEMDomain%Mesh%ElemMat(:) = 1
      end if
   end subroutine
!##################################################

!##################################################
   subroutine Convert2Dto3D(obj, Thickness, division)
      class(PreProcessing_), intent(inout)::obj
      real(real64), allocatable::buffer(:, :)
      real(real64), optional, intent(in)::Thickness
      integer(int32), optional, intent(in)::division
      real(real64) :: Tn
      integer(int32) :: i, j, n, m, NumOfLayer, numnod

      ! only for linear elements

      if (present(Thickness)) then
         if (Thickness == 0.0d0) then
            print *, "ERROR :: Convert2Dto3D >> Thickness = 0"
            return
         else
            Tn = Thickness
         end if
      else
         Tn = 1.0d0
      end if

      if (present(division)) then
         if (division == 0) then
            print *, "ERROR :: Convert2Dto3D >> division = 0"
            return
         end if
         NumOfLayer = division
      else
         NumOfLayer = 1
      end if

      numnod = size(obj%FEMDomain%Mesh%NodCoord, 1)
      n = size(obj%FEMDomain%Mesh%NodCoord, 1)
      m = size(obj%FEMDomain%Mesh%NodCoord, 2)

      allocate (buffer(n*(NumOfLayer + 1), 3))

      do j = 1, NumOfLayer + 1
         do i = 1, n
            buffer(n*(j - 1) + i, 1:2) = obj%FEMDomain%Mesh%NodCoord(i, 1:2)
            buffer(n*(j - 1) + i, 3) = Tn/dble(NumOfLayer)*dble(j - 1)
         end do
      end do

      deallocate (obj%FEMDomain%Mesh%NodCoord)
      allocate (obj%FEMDomain%Mesh%NodCoord(size(buffer, 1), size(buffer, 2)))
      obj%FEMDomain%Mesh%NodCoord(:, :) = buffer(:, :)
      deallocate (buffer)

      ! ElemNod

      if (.not. allocated(obj%FEMDomain%Mesh%ElemNod)) then
         print *, "Caution :: Convert2Dto3D >> ElemNod is not allocated = 0"
         return
      end if
      n = size(obj%FEMDomain%Mesh%ElemNod, 1)
      m = size(obj%FEMDomain%Mesh%ElemNod, 2)

      allocate (buffer(n*NumOfLayer, m*2))

      do j = 1, NumOfLayer
         do i = 1, n
            buffer(n*(j - 1) + i, 1:m) = obj%FEMDomain%Mesh%ElemNod(i, 1:m) + numnod*(j - 1)
            buffer(n*(j - 1) + i, m + 1:2*m) = obj%FEMDomain%Mesh%ElemNod(i, 1:m) + numnod*(j)
         end do
      end do

      deallocate (obj%FEMDomain%Mesh%ElemNod)
      allocate (obj%FEMDomain%Mesh%ElemNod(size(buffer, 1), size(buffer, 2)))
      obj%FEMDomain%Mesh%ElemNod(:, :) = buffer(:, :)
      deallocate (buffer)

      ! ElemMat

      if (.not. allocated(obj%FEMDomain%Mesh%ElemMat)) then
         print *, "Caution :: Convert2Dto3D >> ElemMat is not allocated = 0"
         return
      end if

      allocate (buffer(n*NumOfLayer, 1))

      do j = 1, NumOfLayer
         do i = 1, n
            buffer(n*(j - 1) + i, 1) = obj%FEMDomain%Mesh%ElemMat(i)
         end do
      end do

      deallocate (obj%FEMDomain%Mesh%ElemMat)
      allocate (obj%FEMDomain%Mesh%ElemMat(size(buffer, 1)))
      obj%FEMDomain%Mesh%ElemMat(:) = buffer(:, 1)
      deallocate (buffer)

   end subroutine
!##################################################

!##################################################
   subroutine SetControlParaPrePro(obj, OptionalTol, OptionalItrTol, OptionalTimestep, OptionalSimMode)
      class(PreProcessing_), intent(inout)::obj
      real(real64), optional, intent(in)::OptionalTol
      integer(int32), optional, intent(in)::OptionalSimMode, OptionalItrTol, OptionalTimestep

      call SetControlPara(obj%FEMDomain%ControlPara, OptionalTol, OptionalItrTol, OptionalTimestep, OptionalSimMode)
   end subroutine
!##################################################

!##################################################
   subroutine SetMatParaPreProcessing(obj, MaterialID, ParameterID, Val, materialist, simple)
      class(PreProcessing_), intent(inout)::obj
      class(Dictionary_), optional, intent(inout) :: materialist
      integer(int32), optional, intent(in)::MaterialID, ParameterID
      real(real64), optional, intent(in)::Val
      logical, optional, intent(in) :: simple
      integer(int32) ::i, n, m, p(2), mm

      if (present(materialist)) then
         ! import material information from list
         print *, "total ", materialist%sizeof(), " materials are imported."
         n = materialist%sizeof()
         m = size(materialist%pages(1)%Realist)
         if (allocated(obj%FEMDomain%MaterialProp%MatPara)) then
            deallocate (obj%FEMDomain%MaterialProp%MatPara)
         end if
         allocate (obj%FEMDomain%MaterialProp%MatPara(n, m))
         do i = 1, materialist%sizeof()
            obj%FEMDomain%MaterialProp%MatPara(i, :) = materialist%pages(i)%Realist(:)
         end do
      end if

      if (present(simple)) then
         if (simple .eqv. .true.) then
            if (.not. allocated(obj%FEMDomain%Mesh%ElemMat)) then
               n = size(obj%FEMDomain%Mesh%ElemNod, 1)
               allocate (obj%FEMDomain%Mesh%ElemMat(n))
            else
               deallocate (obj%FEMDomain%Mesh%ElemMat)
               n = size(obj%FEMDomain%Mesh%ElemNod, 1)
               allocate (obj%FEMDomain%Mesh%ElemMat(n))
            end if
            obj%FEMDomain%Mesh%ElemMat(:) = input(default=1, option=MaterialID)
         end if
         return
      end if

      if (.not. allocated(obj%FEMDomain%MaterialProp%MatPara)) then
         allocate (obj%FEMDomain%MaterialProp%MatPara(MaterialID, ParameterID))
         obj%FEMDomain%MaterialProp%MatPara(MaterialID, ParameterID) = val
         return
      else
         mm = size(obj%FEMDomain%MaterialProp%MatPara, 1)
         if (size(obj%FEMDomain%MaterialProp%MatPara, 1) < MaterialID) then
            do i = 1, MaterialID - size(obj%FEMDomain%MaterialProp%MatPara, 1)
               call extendArray(obj%FEMDomain%MaterialProp%MatPara, extend1stColumn=.true.)
               obj%FEMDomain%MaterialProp%MatPara(i + mm, :) = 0.0d0
            end do
         end if

         mm = size(obj%FEMDomain%MaterialProp%MatPara, 2)
         if (size(obj%FEMDomain%MaterialProp%MatPara, 2) < ParameterID) then
            do i = 1, ParameterID - size(obj%FEMDomain%MaterialProp%MatPara, 2)
               call extendArray(obj%FEMDomain%MaterialProp%MatPara, extend2ndColumn=.true.)
               obj%FEMDomain%MaterialProp%MatPara(:, i + mm) = 0.0d0
            end do
         end if
         obj%FEMDomain%MaterialProp%MatPara(MaterialID, ParameterID) = val
      end if

!    if(.not.allocated(obj%FEMDomain%MaterialProp%MatPara) )then
!        allocate(obj%FEMDomain%MaterialProp%MatPara(1,1) )
!        obj%FEMDomain%MaterialProp%MatPara(1,1)=0.0d0
!    endif
!
!    if(present(NumOfMaterial) )then
!        if(NumOfMaterial > size(obj%FEMDomain%MaterialProp%MatPara,1) )then
!            mm=size(obj%FEMDomain%MaterialProp%MatPara,1)
!            do i=1,NumOfMaterial - mm
!                call extendArray(obj%FEMDomain%MaterialProp%MatPara,extend1stColumn=.true.)
!
!
!                if(present(val) )then
!                    obj%FEMDomain%MaterialProp%MatPara(mm+i,:)=val
!                endif
!
!                n=size(obj%FEMDomain%MaterialProp%MatPara,2)
!
!                if(present(AllVal) )then
!                    m=size(AllVal)
!                    p(1)=n
!                    p(2)=m
!                    obj%FEMDomain%MaterialProp%MatPara(mm+i,1: minval(p) )=AllVal(1: minval(p) )
!                endif
!
!            enddo
!        endif
!    endif
!
!
!    if(present(NumOfPara) )then
!        if(NumOfPara > size(obj%FEMDomain%MaterialProp%MatPara,2) )then
!            mm=size(obj%FEMDomain%MaterialProp%MatPara,2)
!            do i=1,NumOfPara - mm
!
!                call extendArray(obj%FEMDomain%MaterialProp%MatPara,extend2ndColumn=.true.)
!                if(present(val) )then
!                    obj%FEMDomain%MaterialProp%MatPara(:,mm+i)=val
!                endif
!                n=size(obj%FEMDomain%MaterialProp%MatPara,1)
!                if(present(AllVal) )then
!                    m=size(AllVal)
!                    p(1)=n
!                    p(2)=m
!                    obj%FEMDomain%MaterialProp%MatPara(1: minval(p) , mm+i)=AllVal(1: minval(p) )
!                endif
!            enddo
!        endif
!    endif

   end subroutine
!##################################################

!##################################################
   subroutine SetMatIDPreProcessing(obj, xmin, xmax, ymin, ymax, zmin, zmax, &
                                    tmin, tmax, MaterialID)
      class(PreProcessing_), intent(inout)::obj
      real(real64), optional, intent(in)::xmin, xmax
      real(real64), optional, intent(in)::ymin, ymax
      real(real64), optional, intent(in)::zmin, zmax
      real(real64), optional, intent(in)::tmin, tmax
      integer(int32), optional, intent(in)::MaterialID
      integer(int32) :: i, j, n

      ! Now implementing
      call AddMaterialID(obj%FEMDomain, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax &
                         , zmin=zmin, zmax=zmax, tmin=tmin, tmax=tmax, MaterialID=MaterialID)

   end subroutine
!##################################################

!##################################################
   subroutine getSkeltonPreProcessing(obj)
      class(PreProcessing_), intent(inout)::obj

      call obj%FEMDomain%MeltingSkelton()

   end subroutine
!##################################################

!##################################################
   subroutine setEntityPreProcessing(obj, Circle, Rectangle, Plane, Cylinder, Box, &
                                     Radius, XSize, YSize, ZSize, Xloc, Yloc, Zloc)
      class(PreProcessing_), intent(inout)::obj
      logical, optional, intent(in) :: Circle, Rectangle, Plane, Cylinder, Box
      real(real64), optional, intent(in) :: Radius, XSize, YSize, ZSize, Xloc, Yloc, Zloc
      integer(int32) :: i

      if (present(Circle)) then
         if (Circle .eqv. .true.) then
            print *, "Under construction"
            return
         end if
      end if

      if (present(Rectangle)) then
         if (Rectangle .eqv. .true.) then
            if (.not. present(Xsize)) then
               print *, "Error :: setEntity >> Please import XSize"
               return
            end if
            if (.not. present(Ysize)) then
               print *, "Error :: setEntity >> Please import YSize"
               return
            end if

            if (allocated(obj%FEMDomain%Mesh%NodCoord)) then
               deallocate (obj%FEMDomain%Mesh%NodCoord)
            end if
            if (allocated(obj%FEMDomain%Mesh%ElemNod)) then
               deallocate (obj%FEMDomain%Mesh%ElemNod)
            end if
            if (allocated(obj%FEMDomain%Mesh%ElemMat)) then
               deallocate (obj%FEMDomain%Mesh%ElemMat)
            end if
            allocate (obj%FEMDomain%Mesh%NodCoord(4, 2))
            obj%FEMDomain%Mesh%NodCoord(1, 1) = 0.0d0; obj%FEMDomain%Mesh%NodCoord(1, 2) = 0.0d0
            obj%FEMDomain%Mesh%NodCoord(2, 1) = Xsize; obj%FEMDomain%Mesh%NodCoord(2, 2) = 0.0d0
            obj%FEMDomain%Mesh%NodCoord(3, 1) = Xsize; obj%FEMDomain%Mesh%NodCoord(3, 2) = ysize
            obj%FEMDomain%Mesh%NodCoord(4, 1) = 0.0d0; obj%FEMDomain%Mesh%NodCoord(4, 2) = ysize

            allocate (obj%FEMDomain%Mesh%ElemNod(1, 4))
            do i = 1, 4
               obj%FEMDomain%Mesh%ElemNod(1, i) = i
            end do
            allocate (obj%FEMDomain%Mesh%ElemMat(1))
            obj%FEMDomain%Mesh%ElemMat(1) = 1

            if (present(Xloc)) then
               obj%FEMDomain%Mesh%NodCoord(:, 1) = obj%FEMDomain%Mesh%NodCoord(:, 1) + Xloc
            end if
            if (present(Yloc)) then
               obj%FEMDomain%Mesh%NodCoord(:, 2) = obj%FEMDomain%Mesh%NodCoord(:, 2) + Yloc
            end if
            return

         end if
      end if
      if (present(Plane)) then
         if (Plane .eqv. .true.) then
            print *, "Under construction"
            return
         end if
      end if
      if (present(Cylinder)) then
         if (Cylinder .eqv. .true.) then
            print *, "Under construction"
            return
         end if
      end if
      if (present(Box)) then
         if (Box .eqv. .true.) then
            if (.not. present(Xsize)) then
               print *, "Error :: setEntity >> Please import XSize"
               return
            end if
            if (.not. present(Ysize)) then
               print *, "Error :: setEntity >> Please import YSize"
               return
            end if
            if (.not. present(Zsize)) then
               print *, "Error :: setEntity >> Please import YSize"
               return
            end if

            if (allocated(obj%FEMDomain%Mesh%NodCoord)) then
               deallocate (obj%FEMDomain%Mesh%NodCoord)
            end if
            if (allocated(obj%FEMDomain%Mesh%ElemNod)) then
               deallocate (obj%FEMDomain%Mesh%ElemNod)
            end if
            if (allocated(obj%FEMDomain%Mesh%ElemMat)) then
               deallocate (obj%FEMDomain%Mesh%ElemMat)
            end if

            allocate (obj%FEMDomain%Mesh%NodCoord(8, 3))
            obj%FEMDomain%Mesh%NodCoord(1, 1) = 0.0d0;
            obj%FEMDomain%Mesh%NodCoord(1, 2) = 0.0d0;
            obj%FEMDomain%Mesh%NodCoord(1, 3) = 0.0d0;
            
            obj%FEMDomain%Mesh%NodCoord(2, 1) = Xsize;
            obj%FEMDomain%Mesh%NodCoord(2, 2) = 0.0d0;
            obj%FEMDomain%Mesh%NodCoord(2, 3) = 0.0d0;
            
            obj%FEMDomain%Mesh%NodCoord(3, 1) = Xsize;
            obj%FEMDomain%Mesh%NodCoord(3, 2) = ysize;
            obj%FEMDomain%Mesh%NodCoord(3, 3) = 0.0d0;
            
            obj%FEMDomain%Mesh%NodCoord(4, 1) = 0.0d0;
            obj%FEMDomain%Mesh%NodCoord(4, 2) = ysize;
            obj%FEMDomain%Mesh%NodCoord(4, 3) = 0.0d0;
            
            obj%FEMDomain%Mesh%NodCoord(5, 1) = 0.0d0;
            obj%FEMDomain%Mesh%NodCoord(5, 2) = 0.0d0;
            obj%FEMDomain%Mesh%NodCoord(5, 3) = ZSize;
            
            obj%FEMDomain%Mesh%NodCoord(6, 1) = Xsize;
            obj%FEMDomain%Mesh%NodCoord(6, 2) = 0.0d0;
            obj%FEMDomain%Mesh%NodCoord(6, 3) = ZSize;
            
            obj%FEMDomain%Mesh%NodCoord(7, 1) = Xsize;
            obj%FEMDomain%Mesh%NodCoord(7, 2) = ysize;
            obj%FEMDomain%Mesh%NodCoord(7, 3) = ZSize;
            
            obj%FEMDomain%Mesh%NodCoord(8, 1) = 0.0d0;
            obj%FEMDomain%Mesh%NodCoord(8, 2) = ysize;
            obj%FEMDomain%Mesh%NodCoord(8, 3) = ZSize;
            
            allocate (obj%FEMDomain%Mesh%ElemNod(1, 8))

            obj%FEMDomain%Mesh%ElemNod(1, 1) = 1
            obj%FEMDomain%Mesh%ElemNod(1, 2) = 2
            obj%FEMDomain%Mesh%ElemNod(1, 3) = 3
            obj%FEMDomain%Mesh%ElemNod(1, 4) = 4
            obj%FEMDomain%Mesh%ElemNod(1, 5) = 5
            obj%FEMDomain%Mesh%ElemNod(1, 6) = 6
            obj%FEMDomain%Mesh%ElemNod(1, 7) = 7
            obj%FEMDomain%Mesh%ElemNod(1, 8) = 8

            allocate (obj%FEMDomain%Mesh%ElemMat(1))
            obj%FEMDomain%Mesh%ElemMat(1) = 1

            if (present(Xloc)) then
               obj%FEMDomain%Mesh%NodCoord(:, 1) = obj%FEMDomain%Mesh%NodCoord(:, 1) + Xloc
            end if
            if (present(Yloc)) then
               obj%FEMDomain%Mesh%NodCoord(:, 2) = obj%FEMDomain%Mesh%NodCoord(:, 2) + Yloc
            end if
            if (present(Zloc)) then
               obj%FEMDomain%Mesh%NodCoord(:, 3) = obj%FEMDomain%Mesh%NodCoord(:, 3) + Zloc
            end if

            return
         end if
      end if

   end subroutine
!##################################################

!##################################################
   subroutine BooleanModifyerPreProcessing(obj, ModObj, XDiv, Ydic, Zdiv)
      class(PreProcessing_), intent(inout)::obj
      class(PreProcessing_), intent(inout)::ModObj
      integer(int32), optional, intent(in) :: XDiv, Ydic, Zdiv
      real(real64) :: ground_level
      real(real64), allocatable::RSInterface(:, :), xmin, xmax, ymin, ymax, NodCoord(:, :)
      integer(int32) :: ground_surface_id, n, m, itr, k, i, j, buf(2)
      integer(int32) :: NodeNum, DimNum, ElemNum, ElemNodNum, startnode, endnode, newnodenum
      integer(int32), allocatable::RSIElemID(:), RSINodeID(:), RSIElemNod(:, :), AvailFE(:)
      integer(int32), allocatable::OldNodID(:), OldtoNewNodID(:), countnode(:, :)

      ! ###### This is for 4-node elements or 8-node box element
      if (.not. allocated(obj%FEMDomain%Mesh%NodCoord)) then
         print *, "Boolean :: >> Error Please import obj%FEMDomain%Mesh%NodCoord"
         return
      end if
      if (.not. allocated(Modobj%FEMDomain%Mesh%NodCoord)) then
         print *, "Boolean :: >> Error Please import Modobj%FEMDomain%Mesh%NodCoord"
         return
      end if

      if (size(obj%FEMDomain%Mesh%NodCoord, 2) == 2) then
         ! if rectangle => ok
         if (size(obj%FEMDomain%Mesh%ElemNod, 2) == 4) then
            ! ### only structural is supported ###
            print *, "Boolean :: ### only structural is supported ###"
            xmin = minval(obj%FEMDomain%Mesh%NodCoord(:, 1))
            xmax = maxval(obj%FEMDomain%Mesh%NodCoord(:, 1))
            ymin = minval(obj%FEMDomain%Mesh%NodCoord(:, 2))
            ymax = maxval(obj%FEMDomain%Mesh%NodCoord(:, 2))
            ground_level = maxval(obj%FEMDomain%Mesh%NodCoord(:, 2))

            NodeNum = size(obj%FEMDomain%Mesh%NodCoord, 1)
            DimNum = size(obj%FEMDomain%Mesh%NodCoord, 2)
            ElemNum = size(obj%FEMDomain%Mesh%ElemNod, 1)
            ElemNodNum = size(obj%FEMDomain%Mesh%ElemNod, 2)
            allocate (RSIElemID(ElemNum), RSINodeID(NodeNum))
            allocate (OldNodID(NodeNum), OldtoNewNodID(NodeNum))
            OldNodID(:) = 0
            OldtoNewNodID(:) = 0
            RSIElemID(:) = 1
            RSINodeID(:) = -1
            if (.not. allocated(ModObj%FEMDomain%Mesh%FacetElemNod)) then
               call GetSurface(ModObj%FEMDomain%Mesh)
            end if

            call Obj%FEMDomain%Mesh%Copy(ModObj%FEMDomain%Mesh, Minimum=.true.)
            ! call showArray(-Obj%FEMDomain%Mesh%NodCoord,Obj%FEMDomain%Mesh%FacetElemNod,FileHandle=224)

            n = size(ModObj%FEMDomain%Mesh%FacetElemNod, 1)
            m = size(ModObj%FEMDomain%Mesh%FacetElemNod, 2)
            allocate (AvailFE(n))
            allocate (countnode(size(ModObj%FEMDomain%Mesh%NodCoord, 1), 2))
            countnode(:, :) = 0
            AvailFE(:) = 0
            itr = 0
            do i = 1, n
               do j = 1, m
                  if (Modobj%FEMDomain%Mesh%NodCoord(ModObj%FEMDomain%Mesh%FacetElemNod(i, j), 2) <= ground_level) then
                     ! utilize
                 countnode(ModObj%FEMDomain%Mesh%FacetElemNod(i, j), 1) = countnode(ModObj%FEMDomain%Mesh%FacetElemNod(i, j), 1) + 1
                     countnode(ModObj%FEMDomain%Mesh%FacetElemNod(i, j), 2) = i
                     AvailFE(i) = 1
                     itr = itr + 1
                     exit
                  end if
               end do
            end do

            k = 0
            do i = 1, size(AvailFE)
               if (k == 0 .and. AvailFE(i) == 1) then
                  cycle
               end if

               if (k == 0 .and. AvailFE(i) == 0) then
                  startnode = i
                  k = 1
               end if
               if (k == 1 .and. AvailFE(i) == 0) then
                  cycle
               end if
               if (k == 1 .and. AvailFE(i) == 1) then
                  endnode = i
                  exit
               end if

            end do

            buf(1) = startnode
            buf(2) = endnode
            n = size(Obj%FEMDomain%Mesh%FacetElemNod, 1)
            m = size(Obj%FEMDomain%Mesh%NodCoord, 2)
            allocate (NodCoord(itr + 4, m))

            NodCoord(:, :) = 0.0d0
            itr = 0
            do i = 1, n

               if (i > minval(buf)) then
                  exit
               end if
               if (AvailFE(i) == 1) then
                  itr = itr + 1
                  NodCoord(itr, :) = Obj%FEMDomain%Mesh%NodCoord(Obj%FEMDomain%Mesh%FacetElemNod(i, 1), :)
               end if

            end do

            NodCoord(1 + itr, 1) = xmax; NodCoord(1 + itr, 2) = ymax; 
            NodCoord(2 + itr, 1) = xmax; NodCoord(2 + itr, 2) = ymin; 
            NodCoord(3 + itr, 1) = xmin; NodCoord(3 + itr, 2) = ymin; 
            NodCoord(4 + itr, 1) = xmin; NodCoord(4 + itr, 2) = ymax; 
            itr = itr + 4

            do i = 1, n

               if (i < maxval(buf)) then
                  cycle
               end if

               if (AvailFE(i) == 1) then
                  itr = itr + 1
                  NodCoord(itr, :) = Obj%FEMDomain%Mesh%NodCoord(Obj%FEMDomain%Mesh%FacetElemNod(i, 1), :)
               end if

               if (itr > n) then
                  exit
               end if
            end do

            deallocate (Obj%FEMDomain%Mesh%NodCoord)
            allocate (Obj%FEMDomain%Mesh%NodCoord(size(NodCoord, 1), size(NodCoord, 2)))
            do i = 1, size(NodCoord, 1)
               Obj%FEMDomain%Mesh%NodCoord(i, :) = NodCoord(i, :)
            end do

            !call showArray(NodCoord,FileHandle=226)
            !print *," "
            !print *, startnode,endnode
            return

            if (allocated(obj%FEMDomain%Mesh%ElemNod)) then
               deallocate (obj%FEMDomain%Mesh%ElemNod)
            end if

            allocate (obj%FEMDomain%Mesh%FacetElemNod(itr + 4, m))

            ! FacetElements of under-gournd part of root domain is copyed to soil domain
            itr = 0
            do i = 1, n
               if (AvailFE(i) == 1) then
                  itr = itr + 1
                  obj%FEMDomain%Mesh%FacetElemNod(itr, :) = Modobj%FEMDomain%Mesh%FacetElemNod(i, :)
                  do j = 1, m
                     OldNodID(Modobj%FEMDomain%Mesh%FacetElemNod(i, j)) = 1
                  end do
               end if
            end do

            ! list up old to new

            itr = 0
            do i = 1, NodeNum
               if (OldNodID(i) == 1) then
                  itr = itr + 1
                  OldtoNewNodID(i) = itr
               end if
            end do

            do i = 1, size(obj%FEMDomain%Mesh%FacetElemNod, 1)
               do j = 1, size(obj%FEMDomain%Mesh%FacetElemNod, 2)
                  obj%FEMDomain%Mesh%FacetElemNod(i, j) = OldtoNewNodID(obj%FEMDomain%Mesh%FacetElemNod(i, j))
                  if (obj%FEMDomain%Mesh%FacetElemNod(i, j) == 0) then
                     stop "BooleanModifyerPreProcessing :: ERROR :: OldtoNewNodID is wrong "
                  end if
               end do
            end do

            NewNodeNum = maxval(OldtoNewNodID)
            if (allocated(obj%FEMDomain%Mesh%NodCoord)) then
               deallocate (obj%FEMDomain%Mesh%NodCoord)
            end if
            allocate (obj%FEMDomain%Mesh%NodCoord(NewNodeNum, DimNum))
            itr = 0
            do i = 1, NodeNum
               if (OldNodID(i) == 1) then
                  itr = itr + 1
                  obj%FEMDomain%Mesh%NodCoord(itr, :) = Modobj%FEMDomain%Mesh%NodCoord(i, :)
               end if
            end do
            allocate (countnode(NewNodeNum, 2))
            countnode(:, :) = 0

            ! NodCoord, FacetElemNod are imported.
            ! Find edge of SurfaceNod
            ! Sort
            itr = 0
            do i = 1, size(obj%FEMDomain%Mesh%FacetElemNod, 1)
               do j = 1, size(obj%FEMDomain%Mesh%FacetElemNod, 2)
                  countnode(obj%FEMDomain%Mesh%FacetElemNod(i, j), 1) = &
                     countnode(obj%FEMDomain%Mesh%FacetElemNod(i, j), 1) + 1
                  k = countnode(obj%FEMDomain%Mesh%FacetElemNod(i, j), 2)
                  if (k < j) then
                     countnode(obj%FEMDomain%Mesh%FacetElemNod(i, j), 2) = j
                  end if

               end do
            end do

            if (maxval(countnode(:, 1)) == 3) then
               print *, "Boolean >> ERROR :: Same node appears 3 times "
               stop
            elseif (minval(countnode(:, 1)) == 2) then
               print *, "Boolean >> ERROR :: minval(countnode)==2 "
               stop
            else
               print *, "Boolean >> ERROR :: minval(countnode)<=0"
               stop
            end if

            itr = 0
            do i = 1, size(countnode, 1)
               if (countnode(i, 1) == 1) then
                  itr = itr + 1
                  if (countnode(i, 2) == 1) then
                     startnode = i
                  elseif (countnode(i, 2) == 2) then
                     endnode = i
                  else
                     stop "Boolean >> ERROR :: countnode(i,2) /= (1 or 2) "
                  end if
               end if
            end do

            if (itr /= 2) then
               print *, "Boolean >> ERROR :: itr /= 2"
               stop
            end if

            ! あとはSurfaceNodを出力するだけ
            open (1233, file="test.txt")
            do i = 1, size(obj%FEMDomain%Mesh%FacetElemNod, 1)
               n = obj%FEMDomain%Mesh%FacetElemNod(i, 1)
               write (1233, *) obj%FEMDomain%Mesh%NodCoord(i, :)
               do j = 1, size(obj%FEMDomain%Mesh%FacetElemNod, 1)

               end do
            end do

            if (allocated(obj%FEMDomain%Mesh%ElemMat)) then
               if (size(obj%FEMDomain%Mesh%ElemMat) /= size(obj%FEMDomain%Mesh%ElemNod, 1)) then
                  deallocate (obj%FEMDomain%Mesh%ElemMat)
               end if
            end if
            if (.not. allocated(obj%FEMDomain%Mesh%ElemMat)) then
               n = size(obj%FEMDomain%Mesh%ElemNod, 1)
               allocate (obj%FEMDomain%Mesh%ElemMat(n))
               obj%FEMDomain%Mesh%ElemMat(:) = 1
            end if

            !do i=1,NodeNum
            !    if(obj%FEMDomain%Mesh%NodCoord(i,2) <= ground_level )then
            !        RSINodeID(i)=1
            !    endif
            !enddo
!
            !itr=0
            !do i=1,ElemNum
            !    do j=1,ElemNodNum
            !        if(obj%FEMDomain%Mesh%ElemNod(i,j) == -1 )then
            !            RSIElemID(:)=-1
            !            itr=itr+1
            !            exit
            !        endif
            !    enddo
            !enddo
            !allocate(RSIElemNod(ElemNum-itr,ElemNodNum) )
            !do i=1,ElemNum
            !    if(RSIElemNod(i) == 1 )then
            !        RSIElemNod(i,:)=obj%FEMDomain%Mesh%ElemNod(i,:)
            !    endif
            !enddo

            return
         else
            print *, "Boolean :: >> Error :: only rectangle is supported"
            return
         end if

      elseif (size(obj%FEMDomain%Mesh%NodCoord, 2) == 3) then
         ! if box => ok
         if (size(obj%FEMDomain%Mesh%ElemNod) == 8) then
            call Modobj%FEMDomain%Mesh%GetSurface()

            stop "Now implementing box"
         else
            print *, "Boolean :: >> Error :: only box is supported"
            return
         end if
      else
         print *, "Boolean :: >> Error size(obj%FEMDomain%Mesh%NodCoord,2) should be 2 or 3"
         return
      end if

   end subroutine
!##################################################

   subroutine ReversePreProcessing(obj)
      class(PreProcessing_), intent(inout)::obj

      obj%FEMDomain%Mesh%NodCoord(:, :) = -obj%FEMDomain%Mesh%NodCoord(:, :)
   end subroutine
!##################################################

!##################################################
   subroutine showMeshPreProcessing(obj, Step, Name, withNeumannBC, withDirichletBC, withMaterial)
      class(PreProcessing_), intent(inout)::obj
      character(*), optional, intent(in):: Name
      integer(int32), optional, intent(in):: Step
      logical, optional, intent(in)::withNeumannBC, withDirichletBC, withMaterial

      integer(int32) :: stp

      if (present(Step)) then
         stp = step
      else
         stp = 0
      end if

      call GmshPlotMesh(obj%FEMDomain, OptionalStep=stp, Name=name, withNeumannBC=withNeumannBC, &
                        withDirichletBC=withDirichletBC, withMaterial=withMaterial)

   end subroutine
!##################################################

!##################################################
   subroutine meshingPreProcessing(obj)
      class(PreProcessing_), intent(inout)::obj

      call obj%FEMDomain%meshing()
   end subroutine
!##################################################

!##################################################
   subroutine importPixcelAsNodePreProcessing(obj, interval)
      class(PreProcessing_), intent(inout)::Obj
      integer(int32), optional, intent(in):: interval
      integer(int32) ::i, j, n, k, l, m
      real(real64), allocatable :: random(:)
      real(real64), allocatable :: NewNodCoord(:, :)
      !integer(int32) :: fh1,fh2,xsize,ysize,final_size,interval_,i,j,k,xpixcel,ypixcel

      m = size(obj%FEMDomain%Mesh%NodCoord, 1)
      n = input(default=1, option=interval)
      k = size(obj%FEMDomain%Mesh%NodCoord, 1)
      allocate (random(k/n))
      call random_number(random)
      allocate (NewNodCoord(k/n, 2))

      random(:) = random(:)*dble(m)

      do i = 1, size(NewNodCoord, 1)
         NewNodCoord(i, 1:2) = obj%FEMDomain%Mesh%NodCoord(int(random(i)), 1:2)
      end do
      deallocate (obj%FEMDomain%Mesh%NodCoord)
      allocate (obj%FEMDomain%Mesh%NodCoord(size(NewNodCoord, 1), size(NewNodCoord, 2)))
      obj%FEMDomain%Mesh%NodCoord(:, :) = NewNodCoord(:, :)

      return
!    fh1=10
!    open(fh1,file=trim(obj%PixcelSizeDataName),status="old" )
!
!    read(fh1,*) xsize
!    read(fh1,*) ysize
!    close(fh1)
!
!    fh2=10
!    open(fh2,file=trim(obj%RGBDataName),status="old" )
!    if(allocated(obj%FEMDomain%Mesh%NodCoord) )then
!        deallocate(obj%FEMDomain%Mesh%NodCoord)
!    endif
!    interval_=input(default=1,option=interval)
!    final_size=xsize*ysize/interval_-1
!    allocate(obj%FEMDomain%Mesh%NodCoord(final_size,2 ) )
!    k=1
!    j=1
!    do i=1,xsize*ysize
!        print *, i,"/",xsize*ysize
!        if(j>final_size)then
!            exit
!        endif
!        read(fh2,*) xpixcel,ypixcel
!        obj%FEMDomain%Mesh%NodCoord(j,1)=dble(xpixcel)
!        obj%FEMDomain%Mesh%NodCoord(j,2)=dble(ypixcel)
!        k=k+1
!        if(k==interval_)then
!            k=1
!            j=j+1
!        endif
!    enddo
!    close(fh2)

   end subroutine
!##################################################

!##################################################
   subroutine removeBoundaryConditionPrePro(obj, Dirichlet, Neumann, Initial)
      class(PreProcessing_), intent(inout)::obj
      logical, optional, intent(in)::Dirichlet, Neumann, Initial
      integer(int32) :: i, j, n

      if (present(Dirichlet)) then
         if (Dirichlet .eqv. .true.) then

            call removeDBoundCondition(obj%FEMDomain)
            return

         end if
      end if

      if (present(Neumann)) then
         if (Neumann .eqv. .true.) then

            call removeNBoundCondition(obj%FEMDomain)

            return
         end if
      end if

      if (present(Initial)) then
         if (Initial .eqv. .true.) then

            call removeTBoundCondition(obj%FEMDomain)

            return
         end if
      end if
   end subroutine
!##################################################

!##################################################
   subroutine importPreProcessing(obj, Name, FileHandle, Mesh)
      class(PreProcessing_), intent(inout)::obj
      type(Mesh_), optional, intent(in)::Mesh
      character(*), optional, intent(in)::Name
      integer(int32), optional, intent(in)::FileHandle

      if (present(Mesh)) then
         call obj%FEMDomain%import(Mesh=Mesh)
         return
      end if
      call obj%FEMDomain%import(OptionalProjectName=Name, FileHandle=FileHandle)

   end subroutine
!##################################################

!##################################################
   subroutine modifySuefaceNodePrepro(obj, Mesh, boolean)
      class(PreProcessing_), intent(inout) :: obj
      class(Mesh_), intent(inout) :: Mesh
      character(*), intent(in) :: boolean
      real(real64), allocatable :: surfacenod_m(:, :), surfacenod(:, :), buffer(:, :), surf_nod_buffer(:, :)
      integer(int32) :: i, j, n, itr, cross1, cross2, cross3, cross4, end1, end2, cases
      integer(int32), allocatable :: in_out(:), in_out_m(:)
      integer(int32) :: s(4), s_m(4), non
      real(real64) :: xmax, ymax, xmin, ymin, x_tr, y_tr, direct, direct_m
      real(real64) :: xmax_m, ymax_m, xmin_m, ymin_m, end1_m, end2_m, xe1, xe2, ye1, ye2

      if (boolean == "diff" .or. boolean == "Diff") then
         cross1 = 0
         ! only for 2D
         if (.not. allocated(Mesh%SurfaceLine2D)) then
            call Mesh%GetSurface()
         end if

         ! Only for box-shaped soil and roots:
         ! It should be boolean operation
         n = size(Mesh%SurfaceLine2D)
         xmin = minval(obj%FEMDomain%Mesh%NodCoord(:, 1))
         ymin = minval(obj%FEMDomain%Mesh%NodCoord(:, 2))
         xmax = maxval(obj%FEMDomain%Mesh%NodCoord(:, 1))
         ymax = maxval(obj%FEMDomain%Mesh%NodCoord(:, 2))
         xmin_m = minval(Mesh%NodCoord(:, 1))
         ymin_m = minval(Mesh%NodCoord(:, 2))
         xmax_m = maxval(Mesh%NodCoord(:, 1))
         ymax_m = maxval(Mesh%NodCoord(:, 2))

         allocate (in_out(size(Mesh%SurfaceLine2D, 1)))
         in_out(:) = 0
         itr = 0
         end1 = 1
         end2 = n
         do i = 1, n
            x_tr = Mesh%NodCoord(Mesh%SurfaceLine2D(i), 1)
            y_tr = Mesh%NodCoord(Mesh%SurfaceLine2D(i), 2)
            if (xmin <= x_tr .and. x_tr <= xmax) then
               if (ymin <= y_tr .and. y_tr <= ymax) then
                  ! in
                  itr = itr + 1
                  in_out(i) = 1
               else
                  ! out
                  in_out(i) = 0
                  if (ymin > y_tr) then
                     cross1 = 1
                  else
                     cross1 = 3
                  end if
               end if
            else
               !out
               in_out(i) = 0
               cross1 = 2
               if (xmin > x_tr) then
                  cross1 = 4
               else
                  cross1 = 2
               end if
            end if
         end do
         allocate (surfacenod(itr, 2))

         ! only for roots surrounded by soils
         ! detect two ends
         do i = 1, n - 1
            if (in_out(i) == 0 .and. in_out(i + 1) == 1) then
               end1 = i
            end if
            if (in_out(i) == 1 .and. in_out(i + 1) == 0) then
               end2 = i
            end if
         end do

         itr = 0
         do i = 1, n
            x_tr = Mesh%NodCoord(Mesh%SurfaceLine2D(i), 1)
            y_tr = Mesh%NodCoord(Mesh%SurfaceLine2D(i), 2)
            if (xmin <= x_tr .and. x_tr <= xmax) then
               if (ymin <= y_tr .and. y_tr <= ymax) then
                  itr = itr + 1
                  surfacenod(itr, 1) = x_tr
                  surfacenod(itr, 2) = y_tr
               else
                  cycle
               end if
            else
               cycle
            end if
         end do

         ! remove overlapped soil surface
         !allocate( in_out_m(size(obj%FEMDomain%Mesh%NodCoord,1) ))
         !in_out_m(:)=0
         !itr=0
         !do i=1,size(in_out_m)
         !    x_tr=obj%FEMDomain%Mesh%NodCoord( i ,1)
         !    y_tr=obj%FEMDomain%Mesh%NodCoord( i ,2)
         !    if( xmin_m <= x_tr .and. x_tr<= xmax_m )then
         !        if( ymin_m <= y_tr .and. y_tr<= ymax_m )then
         !            ! in
         !            itr=itr+1
         !            in_out_m(i)=0
         !        else
         !            ! out
         !            in_out_m(i)=1
         !
         !        endif
         !    else
         !        !out
         !        in_out_m(i)=1
         !
         !    endif
         !enddo
         !allocate(surfacenod_m(itr,2 ))
         ! only for roots surrounded by soils
         ! detect two ends
         !do i=1,n-1
         !    if(in_out_m(i)==0 .and. in_out_m(i+1)==1 )then
         !        end1_m=i
         !    endif
         !    if(in_out_m(i)==1 .and. in_out_m(i+1)==0 )then
         !        end2_m=i
         !    endif
         !enddo
!
         !itr=0
         !do i=1,n
         !    x_tr=obj%FEMDomain%Mesh%NodCoord( i ,1)
         !    y_tr=obj%FEMDomain%Mesh%NodCoord( i ,2)
         !    if( xmin_m <= x_tr .and. x_tr<= xmax_m )then
         !        if( ymin_m <= y_tr .and. y_tr<= ymax_m )then
         !            itr=itr+1
         !            surfacenod_m(itr,1)=x_tr
         !            surfacenod_m(itr,2)=y_tr
         !        else
         !            cycle
         !        endif
         !    else
         !        cycle
         !    endif
         !enddo

         ! add soil surface and root surface
         n = 4 + sum(in_out)
         allocate (buffer(size(obj%FEMDomain%Mesh%NodCoord, 1), 2))
         buffer(:, 1:2) = obj%FEMDomain%Mesh%NodCoord(:, 1:2)
         deallocate (obj%FEMDomain%Mesh%NodCoord)
         allocate (obj%FEMDomain%Mesh%NodCoord(n, 2))

         ! get subdomain of root domain which is in soil domain
         ! end1 to end2
         allocate (surf_nod_buffer(sum(in_out), 2))
         itr = 0
         i = end1 - 1

         do
            itr = itr + 1
            i = i + 1
            if (i > size(Mesh%SurfaceLine2D, 1)) then
               i = 1
            end if

            if (in_out(i) == 0) then
               itr = itr - 1
               cycle
            end if

            surf_nod_buffer(itr, 1:2) = Mesh%NodCoord(Mesh%SurfaceLine2D(i), 1:2)
            if (itr >= size(surf_nod_buffer, 1)) then
               exit
            end if
         end do

         ! add soil surface (box-shaped)

         xe1 = Mesh%NodCoord(Mesh%SurfaceLine2D(end1), 1)
         xe2 = Mesh%NodCoord(Mesh%SurfaceLine2D(end2), 1)
         ye1 = Mesh%NodCoord(Mesh%SurfaceLine2D(end1), 2)
         ye2 = Mesh%NodCoord(Mesh%SurfaceLine2D(end2), 2)

         ! get anti-clockwize surface-line
         non = size(surf_nod_buffer, 1)
         if (cross1 == 1) then
            ! head is down-ward
            print *, "head is down-ward"

            if (xe1 < xe2) then
               do i = 1, non
                  obj%FEMDomain%Mesh%NodCoord(i, 1:2) = surf_nod_buffer(i, 1:2)
               end do
            else
               do i = 1, non
                  obj%FEMDomain%Mesh%NodCoord(i, 1:2) = surf_nod_buffer(non - i + 1, 1:2)
               end do
            end if
            obj%FEMDomain%Mesh%NodCoord(non + 1, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 2, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 3, 1) = xmin
            obj%FEMDomain%Mesh%NodCoord(non + 4, 1) = xmin

            obj%FEMDomain%Mesh%NodCoord(non + 1, 2) = ymin
            obj%FEMDomain%Mesh%NodCoord(non + 2, 2) = ymax
            obj%FEMDomain%Mesh%NodCoord(non + 3, 2) = ymax
            obj%FEMDomain%Mesh%NodCoord(non + 4, 2) = ymin
         elseif (cross1 == 2) then
            ! head is right-side
            print *, "head is right-side"

            if (ye1 < ye2) then
               do i = 1, non
                  obj%FEMDomain%Mesh%NodCoord(i, 1:2) = surf_nod_buffer(i, 1:2)
               end do
            else
               do i = 1, non
                  obj%FEMDomain%Mesh%NodCoord(i, 1:2) = surf_nod_buffer(non - i + 1, 1:2)
               end do
            end if
            obj%FEMDomain%Mesh%NodCoord(non + 1, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 2, 1) = xmin
            obj%FEMDomain%Mesh%NodCoord(non + 3, 1) = xmin
            obj%FEMDomain%Mesh%NodCoord(non + 4, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 1, 2) = ymax
            obj%FEMDomain%Mesh%NodCoord(non + 2, 2) = ymax
            obj%FEMDomain%Mesh%NodCoord(non + 3, 2) = ymin
            obj%FEMDomain%Mesh%NodCoord(non + 4, 2) = ymin
         elseif (cross1 == 3) then
            ! head is upper-side
            print *, "head is upper-side"

            if (xe1 > xe2) then
               do i = 1, non
                  obj%FEMDomain%Mesh%NodCoord(i, 1:2) = surf_nod_buffer(i, 1:2)
               end do
            else
               do i = 1, non
                  obj%FEMDomain%Mesh%NodCoord(i, 1:2) = surf_nod_buffer(non - i + 1, 1:2)
               end do
            end if
            obj%FEMDomain%Mesh%NodCoord(non + 1, 1) = xmin
            obj%FEMDomain%Mesh%NodCoord(non + 2, 1) = xmin
            obj%FEMDomain%Mesh%NodCoord(non + 3, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 4, 1) = xmax

            obj%FEMDomain%Mesh%NodCoord(non + 1, 2) = ymax
            obj%FEMDomain%Mesh%NodCoord(non + 2, 2) = ymin
            obj%FEMDomain%Mesh%NodCoord(non + 3, 2) = ymin
            obj%FEMDomain%Mesh%NodCoord(non + 4, 2) = ymax
         elseif (cross1 == 4) then
            ! head is upper-side
            print *, " head is upper-side"

            if (ye1 > ye2) then
               do i = 1, non
                  obj%FEMDomain%Mesh%NodCoord(i, 1:2) = surf_nod_buffer(i, 1:2)
               end do
            else
               do i = 1, non
                  obj%FEMDomain%Mesh%NodCoord(i, 1:2) = surf_nod_buffer(non - i + 1, 1:2)
               end do
            end if
            obj%FEMDomain%Mesh%NodCoord(non + 1, 1) = xmin
            obj%FEMDomain%Mesh%NodCoord(non + 2, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 3, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 4, 1) = xmin

            obj%FEMDomain%Mesh%NodCoord(non + 1, 2) = ymin
            obj%FEMDomain%Mesh%NodCoord(non + 2, 2) = ymin
            obj%FEMDomain%Mesh%NodCoord(non + 3, 2) = ymax
            obj%FEMDomain%Mesh%NodCoord(non + 4, 2) = ymax
         else
            ! inside
            print *, "none of them"
            print *, cross1

            obj%FEMDomain%Mesh%NodCoord(1:non, 1:2) = surf_nod_buffer(1:non, 1:2)
            obj%FEMDomain%Mesh%NodCoord(non + 1, 1) = xmin
            obj%FEMDomain%Mesh%NodCoord(non + 2, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 3, 1) = xmax
            obj%FEMDomain%Mesh%NodCoord(non + 4, 1) = xmin

            obj%FEMDomain%Mesh%NodCoord(non + 1, 2) = ymin
            obj%FEMDomain%Mesh%NodCoord(non + 2, 2) = ymin
            obj%FEMDomain%Mesh%NodCoord(non + 3, 2) = ymax
            obj%FEMDomain%Mesh%NodCoord(non + 4, 2) = ymax
         end if

         return

!        do i=1,size(Mesh%SurfaceLine2D,1)
!            if(Mesh%NodCoord( Mesh%SurfaceLine2D(i),1)==maxval( Mesh%NodCoord(:,1) ) )then
!                s(1)=i
!            elseif(Mesh%NodCoord( Mesh%SurfaceLine2D(i) ,2)==maxval( Mesh%NodCoord(:,2) ) )then
!                s(2)=i
!            elseif(Mesh%NodCoord( Mesh%SurfaceLine2D(i) ,1)==minval( Mesh%NodCoord(:,1) ) )then
!                s(3)=i
!            elseif(Mesh%NodCoord( Mesh%SurfaceLine2D(i) ,2)==minval( Mesh%NodCoord(:,2) ) )then
!                s(4)=i
!            else
!                cycle
!            endif
!        enddo
!
!        do i=1,size(obj%FEMDomain%Mesh%NodCoord,1)
!            if(obj%FEMDomain%Mesh%NodCoord(i,1)==maxval( obj%FEMDomain%Mesh%NodCoord(:,1) ) )then
!                s_m(1)=i
!            elseif(obj%FEMDomain%Mesh%NodCoord(i,2)==maxval( obj%FEMDomain%Mesh%NodCoord(:,2) ) )then
!                s_m(2)=i
!            elseif(obj%FEMDomain%Mesh%NodCoord(i,1)==minval( obj%FEMDomain%Mesh%NodCoord(:,1) ) )then
!                s_m(3)=i
!            elseif(obj%FEMDomain%Mesh%NodCoord(i,2)==minval( obj%FEMDomain%Mesh%NodCoord(:,2) ) )then
!                s_m(4)=i
!            else
!                cycle
!            endif
!        enddo
!
!        direct=dble(s(4)-s(3))/abs(s(4)-s(3) )+dble(s(3)-s(2))/abs(s(3)-s(2) )&
!            +dble(s(2)-s(1))/abs(s(2)-s(1) )+dble(s(1)-s(4))/abs(s(1)-s(4) )
!        direct_m=dble(s_m(4)-s_m(3))/abs(s_m(4)-s_m(3) )+dble(s_m(3)-s_m(2))/abs(s_m(3)-s_m(2) )&
!            +dble(s_m(2)-s_m(1))/abs(s_m(2)-s_m(1) )+dble(s_m(1)-s_m(4))/abs(s_m(1)-s_m(4) )
!
!
!
!        if(direct * direct_m <= 0.0d0)then
!            print *, "opposite direction"
!            do i=1,size(buffer,1)
!                buffer(i,1:2)=obj%FEMDomain%Mesh%NodCoord( size(buffer,1)-i+1    ,1:2)
!            enddo
!        else
!            print *, "same direction"
!            buffer(:,1:2)=obj%FEMDomain%Mesh%NodCoord(:,1:2)
!        endif
!

         !scall showarray(obj%FEMDomain%Mesh%NodCoord)
         !call showarray(Mesh%NodCoord)
!        i=end1_m
!        itr=0
!        do
!            itr=itr+1
!            i=i+1
!            if(i>size(buffer,1) )then
!                i=1
!            endif
!
!            if(in_out_m(i)==0 )then
!                itr=itr-1
!                cycle
!            endif
!
!            obj%FEMDomain%Mesh%NodCoord(itr,1:2)=buffer(i,1:2)
!            if(itr>=sum(in_out_m) )then
!                exit
!            endif
!        enddo
!        itr=itr+1
!        obj%FEMDomain%Mesh%NodCoord(itr,1:2)=Mesh%NodCoord(Mesh%SurfaceLine2D(end1),1:2)
!        i=end1

      end if

   end subroutine
!##################################################

! #########################################################
   subroutine ExportAsLodgingSimProcessing(obj, soil, Name, penalypara, displacement)
      class(PreProcessing_), intent(inout):: obj
      type(PreProcessing_), intent(inout):: soil
      real(real64), optional, intent(in)::penalypara, displacement
      character(*), intent(in) :: Name
      real(real64) :: x_max, x_min, y_max, y_min
      integer(int32) :: number_of_node, fh, i
      integer(int32), allocatable :: top_root_list(:)
      integer(int32), allocatable :: rightside_soil_list(:)
      integer(int32), allocatable :: leftside_soil_list(:)
      integer(int32), allocatable :: bottom_soil_list(:)
      integer(int32) :: total_x_cond, total_y_cond
      ! Export As Lodging Simulator 25 file.

      ! bug exists

      ! give ux=0, uy=0 at toproot.
      y_max = maxval(obj%FEMDomain%Mesh%NodCoord(:, 2))
      number_of_node = countif(Array=obj%FEMDomain%Mesh%NodCoord(:, 2), Equal=.true., Value=y_max)
      allocate (top_root_list(number_of_node))
      top_root_list(:) = getif(Array=obj%FEMDomain%Mesh%NodCoord(:, 2), Value=y_max)

      ! give ux=0, uy=0 at right-side of soil.
      x_max = maxval(Soil%FEMDomain%Mesh%NodCoord(:, 1))
      number_of_node = countif(Array=Soil%FEMDomain%Mesh%NodCoord(:, 1), Equal=.true., Value=x_max)
      allocate (rightside_soil_list(number_of_node))
      rightside_soil_list(:) = 0
      rightside_soil_list(:) = getif(Array=Soil%FEMDomain%Mesh%NodCoord(:, 1), Value=x_max)
      rightside_soil_list(:) = rightside_soil_list(:) + size(obj%FEMDomain%Mesh%NodCoord, 1)

      ! give ux=0, uy=0 at left-side of soil.
      x_min = minval(Soil%FEMDomain%Mesh%NodCoord(:, 1))
      number_of_node = countif(Array=Soil%FEMDomain%Mesh%NodCoord(:, 1), Equal=.true., Value=x_min)
      allocate (leftside_soil_list(number_of_node))
      leftside_soil_list(:) = 0
      leftside_soil_list(:) = getif(Array=Soil%FEMDomain%Mesh%NodCoord(:, 1), Value=x_min)
      leftside_soil_list(:) = leftside_soil_list(:) + size(obj%FEMDomain%Mesh%NodCoord, 1)

      ! give ux=0, uy=0 at the bottom of soil.
      y_min = minval(Soil%FEMDomain%Mesh%NodCoord(:, 2))
      number_of_node = countif(Array=Soil%FEMDomain%Mesh%NodCoord(:, 2), Equal=.true., Value=y_min)
      allocate (bottom_soil_list(number_of_node))
      bottom_soil_list(:) = 0
      bottom_soil_list(:) = getif(Array=Soil%FEMDomain%Mesh%NodCoord(:, 2), Value=y_min)
      bottom_soil_list(:) = bottom_soil_list(:) + size(obj%FEMDomain%Mesh%NodCoord, 1)

      total_x_cond = size(top_root_list) + size(rightside_soil_list) + size(leftside_soil_list) + size(bottom_soil_list)
      total_y_cond = size(top_root_list) + size(rightside_soil_list) + size(leftside_soil_list) + size(bottom_soil_list)

      ! export info
      fh = 22
      open (fh, file=Name)
      write (fh, *) 2
      write (fh, *) 1, size(obj%FEMDomain%Mesh%NodCoord, 1)
      write (fh, *) size(obj%FEMDomain%Mesh%NodCoord, 1) + 1, &
         size(obj%FEMDomain%Mesh%NodCoord, 1) + size(soil%FEMDomain%Mesh%NodCoord, 1)
      write (fh, *) size(obj%FEMDomain%Mesh%ElemNod, 1)
      write (fh, *) size(soil%FEMDomain%Mesh%ElemNod, 1)
      write (fh, *) size(obj%FEMDomain%Mesh%NodCoord, 1) + size(soil%FEMDomain%Mesh%NodCoord, 1)
      call showArray(Mat=obj%FEMDomain%Mesh%NodCoord, FileHandle=fh)
      call showArray(Mat=Soil%FEMDomain%Mesh%NodCoord, FileHandle=fh)
      write (fh, *) size(obj%FEMDomain%Mesh%ElemNod, 1) + size(soil%FEMDomain%Mesh%ElemNod, 1), &
         size(soil%FEMDomain%Mesh%ElemNod, 2)
      call showArray(Mat=obj%FEMDomain%Mesh%ElemNod, FileHandle=fh)
      call showArray(Mat=Soil%FEMDomain%Mesh%ElemNod, FileHandle=fh, Add=size(obj%FEMDomain%Mesh%NodCoord, 1))
      call showArray(Mat=obj%FEMDomain%Mesh%ElemMat, FileHandle=fh)
      Soil%FEMDomain%Mesh%ElemMat(:) = 4
      call showArray(Mat=Soil%FEMDomain%Mesh%ElemMat, FileHandle=fh)
      write (fh, *) 4
      write (fh, *) "6038.93994      0.349999994       0.00000000       1.00000002E+20  0.777999997       0.00000000"
      write (fh, *) "6038.93994      0.349999994       0.00000000       1.00000002E+20  0.777999997       0.00000000"
      write (fh, *) "60000.0000      0.349999994       0.00000000       1.00000002E+20  0.777999997       0.00000000"
      write (fh, *) "60000.0000      0.349999994       0.00000000       1.00000002E+20  0.777999997       0.00000000"
      write (fh, *) total_x_cond, total_y_cond
      call showArray(Mat=top_root_list, FileHandle=fh)
      call showArray(Mat=rightside_soil_list, FileHandle=fh)
      call showArray(Mat=leftside_soil_list, FileHandle=fh)
      call showArray(Mat=bottom_soil_list, FileHandle=fh)
      do i = 1, size(top_root_list)
         write (fh, *) 0.0d0
      end do
      do i = 1, size(rightside_soil_list)
         write (fh, *) 0.0d0
      end do
      do i = 1, size(leftside_soil_list)
         write (fh, *) 0.0d0
      end do
      do i = 1, size(bottom_soil_list)
         write (fh, *) 0.0d0
      end do
      call showArray(Mat=top_root_list, FileHandle=fh)
      call showArray(Mat=rightside_soil_list, FileHandle=fh)
      call showArray(Mat=leftside_soil_list, FileHandle=fh)
      call showArray(Mat=bottom_soil_list, FileHandle=fh)
      do i = 1, size(top_root_list)
         write (fh, *) - 1.0d0
      end do
      do i = 1, size(rightside_soil_list)
         write (fh, *) 0.0d0
      end do
      do i = 1, size(leftside_soil_list)
         write (fh, *) 0.0d0
      end do
      do i = 1, size(bottom_soil_list)
         write (fh, *) 0.0d0
      end do
      write (fh, *) 0
      call obj%FEMDomain%Mesh%getSurface()
      call Soil%FEMDomain%Mesh%getSurface()
      write (fh, *) size(obj%FEMDomain%Mesh%SurfaceLine2D) + size(Soil%FEMDomain%Mesh%SurfaceLine2D)
      call showArray(Mat=obj%FEMDomain%Mesh%SurfaceLine2D, FileHandle=fh)
      call showArray(Mat=Soil%FEMDomain%Mesh%SurfaceLine2D, FileHandle=fh, Add=size(obj%FEMDomain%Mesh%NodCoord, 1))
      write (fh, *) 1, size(obj%FEMDomain%Mesh%SurfaceLine2D)
      write (fh, *) size(obj%FEMDomain%Mesh%SurfaceLine2D) + 1, &
         size(obj%FEMDomain%Mesh%SurfaceLine2D) + size(Soil%FEMDomain%Mesh%SurfaceLine2D)
      write (fh, *) "0.1000000000000E-01   0.1000000000000E-01"
      write (fh, *) "1  1"
      write (fh, *) 1, size(obj%FEMDomain%Mesh%SurfaceLine2D) + size(Soil%FEMDomain%Mesh%SurfaceLine2D), 1
      write (fh, *) 1
      write (fh, *) "0.5000000000000E+05   0.5000000000000E+05   0.2402100000000E+01   0.5404000000000E+00"
      write (fh, *) "1  800  1"
      close (fh)

   end subroutine ExportAsLodgingSimProcessing
! #########################################################

end module

