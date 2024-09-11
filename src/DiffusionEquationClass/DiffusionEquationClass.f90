module DiffusionEquationClass
   use, intrinsic :: iso_fortran_env
   use FEMDomainClass
   use FEMSolverClass
   use PostProcessingClass
   use LinearSolverClass
   implicit none

   type:: DiffusionEq_
      ! For single-domain problem
      type(FEMDomain_), pointer ::FEMDomain
      type(FEMDomainp_), allocatable :: FEMDomains
      type(FEMSolver_) :: solver

      real(real64), allocatable ::UnknownValue(:, :)
      real(real64), allocatable ::UnknownVec(:)
      real(real64), allocatable ::UnknownValueInit(:, :)
      real(real64), allocatable ::UnknownValueRate(:, :)
      real(real64), allocatable ::DiffusionMat(:, :, :)
      real(real64), allocatable ::Divergence(:, :)
      real(real64), allocatable ::Flowvector(:, :)
      real(real64), allocatable ::FluxVector3D(:, :)

      real(real64), allocatable :: CRS_RHS_n(:)

      real(real64), allocatable ::Permiability(:)  ! directly give parameter #1
      real(real64)             ::dt
      integer(int32)           :: step
      logical :: explicit = .false.
   contains
      procedure :: Setup => SetupDiffusionEq

      procedure, pass :: SolveDiffusionEq
      generic :: solve => SolveDiffusionEq
      procedure, public :: getDiffusionField => Solve_oneline_DiffusionEq

      procedure :: Update => UpdateDiffusionEq

      procedure :: GetMat => GetDiffusionMat
      procedure :: GetRHS => GetFlowvector
      procedure :: GetInitVal => GetUnknownValue
      procedure :: Display => DisplayDiffusionEq
      procedure :: import => importDiffusionEq
      procedure :: export => exportDiffusionEq
      procedure :: deploy => deployDiffusionEq

      procedure :: save => saveDiffusionEq
      procedure :: open => openDiffusionEq
      procedure :: remove => removeDiffusionEq
      procedure :: updateByRK4 => updateByRK4DiffusionEq

      procedure :: check_stability_condition => check_stability_conditionDiffusionEq

   end type

contains

! #######################################################################
   subroutine updateByRK4DiffusionEq(obj)
      class(DiffusionEq_), intent(inout) :: obj

      ! update unknown by 4th order Runge-Kutta method.

      integer(int32) :: nod_num, dim_num, elemnod_num, elem_num
      integer(int32) :: i, j, k, l, m
      real(real64) :: diff_coeff
      real(real64), allocatable::DiffMat(:, :), MassMat(:, :), Cvec(:), Flux(:), FluxA(:), &
                                  k1(:), k2(:), k3(:), k4(:), MassMat_inv(:, :), vec(:), rkvec(:), DiffMatA(:, :), MassMatA(:, :), &
                                  UnknownValue_n(:, :)

      nod_num = size(obj%FEMDomain%Mesh%NodCoord, 1)
      elem_num = size(obj%FEMDomain%Mesh%ElemNod, 1)
      elemnod_num = size(obj%FEMDomain%Mesh%ElemNod, 2)
      dim_num = size(obj%FEMDomain%Mesh%NodCoord, 2)

      allocate (Cvec(elemnod_num), Flux(elemnod_num), FluxA(elemnod_num))
      allocate (k1(elemnod_num))
      allocate (k2(elemnod_num))
      allocate (k3(elemnod_num))
      allocate (k4(elemnod_num))
      allocate (vec(elemnod_num))
      allocate (rkvec(elemnod_num))
      allocate (DiffMatA(elemnod_num, elemnod_num))
      allocate (MassMatA(elemnod_num, elemnod_num))

      if (.not. allocated(obj%FlowVector)) allocate (obj%FlowVector(elem_num, elemnod_num))
      if (.not. allocated(obj%UnknownValue)) allocate (obj%UnknownValue(elem_num, elemnod_num))
      if (.not. allocated(obj%DiffusionMat)) allocate (obj%DiffusionMat(elem_num, elemnod_num, elemnod_num))
      if (.not. allocated(obj%FluxVector3D)) allocate (obj%FluxVector3D(elem_num, elemnod_num))
      obj%FlowVector(:, :) = 0.0d0
      obj%FluxVector3D(:, :) = 0.0d0
      obj%DiffusionMat(:, :, :) = 0.0d0

      ! apply dirichlet B.C.
      do i = 1, size(obj%FEMDomain%Mesh%ElemNod, 1)
         do j = 1, size(obj%FEMDomain%Mesh%ElemNod, 2)
            do k = 1, size(obj%FEMDomain%Boundary%DBoundNodID, 1)
               if (obj%FEMDomain%Boundary%DBoundNodID(k, 1) == obj%FEMDomain%Mesh%ElemNod(i, j)) then
                  obj%UnknownValue(i, j) = obj%FEMDomain%Boundary%DBoundVal(k, 1)
               end if
            end do
         end do
      end do

      UnknownValue_n = obj%UnknownValue

      obj%FEMDomain%ShapeFunction%ElemType = obj%FEMDomain%Mesh%ElemType
      call SetShapeFuncType(obj%FEMDomain%ShapeFunction)

      !call showArraySize(obj%FluxVector3D)
      !call showArraySize(Flux)

      do i = 1, elem_num
         Cvec(:) = UnknownValue_n(i, :)
         vec(:) = UnknownValue_n(i, :)

         DiffMatA(:, :) = 0.0d0
         MassMatA(:, :) = 0.0d0
         FluxA(:) = 0.0d0

         do j = 1, obj%FEMDomain%ShapeFunction%NumOfGp

            diff_coeff = obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i), 1)

            ! allow direct-import of Permiability
            if (allocated(obj%Permiability)) then
               if (size(obj%Permiability) /= elem_num) then
                  print *, "ERROR :: FiniteDeform :: size(obj%Permiability/=elem_num)"
               else
                  diff_coeff = obj%Permiability(i)
               end if
            end if

            call getAllShapeFunc(obj%FEMDomain%ShapeFunction, elem_id=i, nod_coord=obj%FEMDomain%Mesh%NodCoord, &
                                 elem_nod=obj%FEMDomain%Mesh%ElemNod, OptionalGpID=j)
            call getElemDiffusionMatrix(obj%FEMDomain%ShapeFunction, diff_coeff, DiffMat)

            !if(obj%step==2) stop

            call getElemFluxVec(obj%FEMDomain%ShapeFunction, diff_coeff, Flux, Cvec)
            call getElemMassMatrix(obj%FEMDomain%ShapeFunction, MassMat)

            MassMatA(:, :) = MassMatA(:, :) + MassMat(:, :)
            DiffMatA(:, :) = DiffMatA(:, :) + DiffMat(:, :)
            FluxA(:) = FluxA(:) + Flux(:)

            !do k=1,size(DiffMat,1)
            !    obj%DiffusionMat(i,k,:)=obj%DiffusionMat(i,k,:)- obj%dt/2.0d0* DiffMat(k,:)&
            !    +MassMat(k,:)
            !enddo

            !
            !obj%FluxVector3D(i,:)=obj%FluxVector3D(i,:) + Flux(:)
         end do

         ! 4th-order RK!
         vec(:) = matmul(DiffMatA, Cvec) - FluxA(:)
         call gauss_jordan_pv(MassMatA, k1, vec, size(vec))

         rkvec(:) = Cvec(:) + obj%dt*0.50d0*k1(:)
         vec(:) = matmul(DiffMatA, rkvec) - FluxA(:)
         call gauss_jordan_pv(MassMatA, k2, vec, size(vec))

         rkvec(:) = Cvec(:) + obj%dt*0.50d0*k2(:)
         vec(:) = matmul(DiffMatA, rkvec) - FluxA(:)
         call gauss_jordan_pv(MassMatA, k3, vec, size(vec))

         rkvec(:) = Cvec(:) + obj%dt*0.50d0*k3(:)
         vec(:) = matmul(DiffMatA, rkvec) - FluxA(:)
         call gauss_jordan_pv(MassMatA, k4, vec, size(vec))

         !call print("UnknownValue_n(i,:)")
         !call print(UnknownValue_n(i,:))
         !call print("obj%UnknownValue(i,:)")
         !call print(obj%UnknownValue(i,:))
         !stop

         obj%UnknownValue(i, :) = UnknownValue_n(i, :) &
                                  + obj%dt/6.0d0*(k1 + 2.0d0*k2 + 2.0d0*k3 + k4)

      end do

   end subroutine
! #######################################################################

! #######################################################################
   subroutine deployDiffusionEq(obj, FEMDomain)
      class(DiffusionEq_), intent(inout) :: obj
      type(FEMDomain_), target, intent(in) :: FEMDomain

      if (associated(obj%FEMDomain)) then
         nullify (obj%FEMDomain)
      end if
      obj%FEMDomain => FEMDomain

   end subroutine
! #######################################################################

! #######################################################################
   subroutine removeDiffusionEq(obj)
      class(DiffusionEq_), intent(inout) :: obj
      if (associated(obj%FEMDomain)) nullify (obj%FEMDomain)
      if (allocated(obj%UnknownValue)) deallocate (obj%UnknownValue)
      if (allocated(obj%UnknownVec)) deallocate (obj%UnknownVec)
      if (allocated(obj%UnknownValueInit)) deallocate (obj%UnknownValueInit)
      if (allocated(obj%UnknownValueRate)) deallocate (obj%UnknownValueRate)
      if (allocated(obj%DiffusionMat)) deallocate (obj%DiffusionMat)
      if (allocated(obj%Divergence)) deallocate (obj%Divergence)
      if (allocated(obj%Flowvector)) deallocate (obj%Flowvector)
      if (allocated(obj%FluxVector3D)) deallocate (obj%FluxVector3D)

      if (allocated(obj%Permiability)) deallocate (obj%Permiability)
      obj%dt = 1.0d0
      obj%step = 0
   end subroutine
! #######################################################################

! #######################################################################
   subroutine linkDiffusionEq(obj, FEMDomain)
      class(DiffusionEq_), intent(inout) :: obj
      type(FEMDomain_), target, intent(in) :: FEMDomain

      if (associated(obj%FEMDomain)) then
         nullify (obj%FEMDomain)
         obj%FEMDomain => FEMDOmain
      end if

   end subroutine
! #######################################################################

! #######################################################################
   subroutine openDiffusionEq(obj, path, name)
      class(DiffusionEq_), intent(inout) :: obj
      character(*), intent(in) :: path
      character(*), optional, intent(in) :: name
      character(200) :: pathi
      type(IO_) :: f
      integer(int32) :: n

      if (present(name)) then
         pathi = path
         !if( index(path, "/", back=.true.) == len(path) )then
         !        n=index(path, "/", back=.true.)
         !        pathi(n:n)= " "
         !endif

         call f%open(pathi//"/"//name, "/"//"DiffusionEq", ".prop")
      else
         pathi = path
         !if( index(path, "/", back=.true.) == len(path) )then
         !        n=index(path, "/", back=.true.)
         !        pathi(n:n)= " "
         !endif
         call f%open(pathi//"/DiffusionEq", "/DiffusionEq", ".prop")
      end if

      ! write smt at here!

      call openArray(f%fh, obj%UnknownValue)
      call openArray(f%fh, obj%UnknownVec)
      call openArray(f%fh, obj%UnknownValueInit)
      call openArray(f%fh, obj%UnknownValueRate)
      call openArray(f%fh, obj%DiffusionMat)
      call openArray(f%fh, obj%Divergence)
      call openArray(f%fh, obj%Flowvector)
      call openArray(f%fh, obj%FluxVector3D)

      call openArray(f%fh, obj%Permiability)  ! directly give parameter #1
      read (f%fh, *) obj%dt
      read (f%fh, *) obj%step

      call f%close()

   end subroutine

! #######################################################################
   subroutine saveDiffusionEq(obj, path, name)
      class(DiffusionEq_), intent(inout) :: obj
      character(*), intent(in) :: path
      character(*), optional, intent(in) :: name
      character(200) :: pathi
      type(IO_) :: f
      integer(int32) :: n

      if (present(name)) then
         pathi = path
         !if( index(path, "/", back=.true.) == len(path) )then
         !        n=index(path, "/", back=.true.)
         !        pathi(n:n)= " "
         !endif

         call execute_command_line("mkdir -p "//pathi)
         call execute_command_line("mkdir -p "//pathi//"/"//name)
         call f%open(pathi//"/"//name, "/"//"DiffusionEq", ".prop")
      else
         pathi = path
         !if( index(path, "/", back=.true.) == len(path) )then
         !        n=index(path, "/", back=.true.)
         !        pathi(n:n)= " "
         !endif
         call execute_command_line("mkdir -p "//pathi)
         call execute_command_line("mkdir -p "//pathi//"/DiffusionEq")
         call f%open(pathi//"/DiffusionEq", "/DiffusionEq", ".prop")
      end if

      ! write smt at here!

      call writeArray(f%fh, obj%UnknownValue)
      call writeArray(f%fh, obj%UnknownVec)
      call writeArray(f%fh, obj%UnknownValueInit)
      call writeArray(f%fh, obj%UnknownValueRate)
      call writeArray(f%fh, obj%DiffusionMat)
      call writeArray(f%fh, obj%Divergence)
      call writeArray(f%fh, obj%Flowvector)
      call writeArray(f%fh, obj%FluxVector3D)

      call writeArray(f%fh, obj%Permiability)  ! directly give parameter #1
      write (f%fh, *) obj%dt
      write (f%fh, *) obj%step

      call f%close()

   end subroutine

! ###################################################
   subroutine importDiffusionEq(obj, Permiability)
      class(DiffusionEq_), intent(inout) :: obj
      real(real64), optional, intent(in) :: Permiability(:)
      integer(int32) :: i, j, n

      if (present(Permiability)) then
         if (allocated(obj%Permiability)) then
            deallocate (obj%Permiability)
         end if
         n = size(Permiability)
         allocate (obj%Permiability(n))
         obj%Permiability(:) = Permiability(:)
         return
      end if

   end subroutine importDiffusionEq
! ###################################################

! ###################################################
   subroutine exportDiffusionEq(obj, path, restart)
      class(DiffusionEq_), intent(inout) :: obj
      character(*), optional, intent(in) :: path
      logical, optional, intent(in) :: restart
      type(IO_) :: f

      if (present(restart)) then
         call execute_command_line("mkdir -p "//path//"/DiffusionEq")
         call f%open("./", path, "/DiffusionEq/DiffusionEq.res")

         if (associated(obj%FEMDomain)) then
            call obj%FEMDomain%export(path=path//"/FEMDomain", FileHandle=f%fh + 1, restart=.true.)
         end if

         write (f%fh, *) obj%UnknownValue(:, :)
         write (f%fh, *) obj%UnknownVec(:)
         write (f%fh, *) obj%UnknownValueInit(:, :)
         write (f%fh, *) obj%UnknownValueRate(:, :)
         write (f%fh, *) obj%DiffusionMat(:, :, :)
         write (f%fh, *) obj%Divergence(:, :)
         write (f%fh, *) obj%Flowvector(:, :)
         write (f%fh, *) obj%FluxVector3D(:, :)
         write (f%fh, *) obj%Permiability(:)
         write (f%fh, *) obj%dt
         write (f%fh, *) obj%step

         call f%close()
      end if

   end subroutine
! ###################################################

!######################## ImportData of DiffusionEq ########################
   subroutine ImportFEMDomainDiff(obj, OptionalFileFormat, OptionalProjectName)
      class(FEMDomain_), intent(inout)::obj
      character*4, optional, intent(in)::OptionalFileFormat
      character*70, optional, intent(in)::OptionalProjectName

      character*4::FileFormat
      character*70::ProjectName
      character*74 ::FileName
      integer(int32), allocatable::IntMat(:, :)
      real(real64), allocatable::RealMat(:, :)
      integer(int32) :: fh, i, j, NumOfDomain, n, m, DimNum
      character*70 Msg

      call DeallocateFEMDomain(obj)

      fh = 104
      if (present(OptionalFileFormat)) then
         FileFormat = OptionalFileFormat
      else
         FileFormat = ".scf"
      end if

      if (present(OptionalProjectName)) then
         ProjectName = OptionalProjectName
      else
         ProjectName = "untitled"
      end if

      FileName = ProjectName//FileFormat
      obj%FileName = FileName
      obj%FilePath = "./"
    !!print *, "Project : ",ProjectName
    !!print *, "is Exported as : ",FileFormat," format"
    !!print *, "File Name is : ",FileName

      open (fh, file=FileName, status="old")

      if (FileFormat == ".scf") then

         read (fh, *) NumOfDomain
         allocate (IntMat(NumOfDomain, 2))
         allocate (obj%Mesh%SubMeshNodFromTo(NumOfDomain, 3))
         allocate (obj%Mesh%SubMeshElemFromTo(NumOfDomain, 3))

         do i = 1, NumOfDomain
            obj%Mesh%SubMeshNodFromTo(i, 1) = i
            read (fh, *) obj%Mesh%SubMeshNodFromTo(i, 2), obj%Mesh%SubMeshNodFromTo(i, 3)
         end do

         do i = 1, NumOfDomain
            obj%Mesh%SubMeshElemFromTo(i, 1) = i
            read (fh, *) obj%Mesh%SubMeshElemFromTo(i, 3)
            if (i == 1) then
               obj%Mesh%SubMeshElemFromTo(i, 2) = 1
            else
               obj%Mesh%SubMeshElemFromTo(i, 2) = obj%Mesh%SubMeshElemFromTo(i - 1, 3) + 1
            end if
         end do

         read (fh, *) n, m
         DimNum = m
         allocate (obj%Mesh%NodCoord(n, m))
         call ImportArray(obj%Mesh%NodCoord, OptionalFileHandle=fh)

         read (fh, *) n, m
         read (fh, *) obj%Mesh%ElemType
         allocate (obj%Mesh%ElemNod(n, m))
         allocate (obj%Mesh%ElemMat(n))
         call ImportArray(obj%Mesh%ElemNod, OptionalFileHandle=fh)
         do i = 1, n
            read (fh, *) obj%Mesh%ElemMat(i)
         end do
         read (fh, *) n, m
         allocate (obj%MaterialProp%MatPara(n, m))
         call ImportArray(obj%MaterialProp%MatPara, OptionalFileHandle=fh)

         !######### Dirichlet boundary conditions #################
         DimNum = 1
         allocate (obj%Boundary%DBoundNum(DimNum))
         read (fh, *) obj%Boundary%DBoundNum(:)
         allocate (obj%Boundary%DBoundNodID(maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)))
         allocate (obj%Boundary%DBoundVal(maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)))

         obj%Boundary%DBoundNodID(:, :) = -1
         obj%Boundary%DBoundVal(:, :) = 0.0d0

         do i = 1, size(obj%Boundary%DBoundNum, 1)
            do j = 1, obj%Boundary%DBoundNum(i)
               read (fh, *) obj%Boundary%DBoundNodID(j, i)
            end do
            do j = 1, obj%Boundary%DBoundNum(i)
               read (fh, *) obj%Boundary%DBoundVal(j, i)
            end do
         end do
         !######### Dirichlet boundary conditions #################

         !######### Neumann boundary conditions #################
         read (fh, *) n
         allocate (obj%Boundary%NBoundNum(DimNum))
         allocate (obj%Boundary%NBoundNodID(n, size(obj%Boundary%NBoundNum)))
         allocate (obj%Boundary%NBoundVal(n, size(obj%Boundary%NBoundNum)))
         obj%Boundary%NBoundNodID(:, :) = -1
         obj%Boundary%NBoundVal(:, :) = 0.0d0

         obj%Boundary%NBoundNum(:) = n
         do i = 1, n
            read (fh, *) m
            obj%Boundary%NBoundNodID(i, :) = m
         end do

         do i = 1, n
            read (fh, *) obj%Boundary%NBoundVal(i, :)
         end do
         !######### Neumann boundary conditions #################

         !######### Initial conditions #################
         read (fh, *) n
         allocate (obj%Boundary%TBoundNodID(n, 1))
         allocate (obj%Boundary%TBoundVal(n, 1))
         allocate (obj%Boundary%TBoundNum(1))

         obj%Boundary%TBoundNum = n

         if (n /= 0) then
            if (n < 0) then
               print *, "ERROR :: number of initial conditions are to be zero"
            else
               do i = 1, n
                  read (fh, *) obj%Boundary%TBoundNodID(i, 1)
               end do
               do i = 1, n
                  read (fh, *) obj%Boundary%TBoundVal(i, 1)
               end do
            end if
         end if

         !######### Initial conditions #################
         read (fh, *) obj%ControlPara%ItrTol, obj%ControlPara%Timestep
         close (fh)
      else
        !!print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
      end if

   end subroutine ImportFEMDomainDiff
!######################## ImportData of DiffusionEq ########################

!######################## Solve DiffusionEq ########################
   subroutine SolveDiffusionEq(obj, Solvertype, restart)
      class(DiffusionEq_), intent(inout)::obj
      character(*), optional, intent(in)::Solvertype
      character*70 ::solver, defaultsolver

      real(real64), allocatable::Amat(:, :), bvec(:), xvec(:)
      real(real64)::val, er
      integer(int32) ::i, j, n, m, k, nodeid1, nodeid2, localid, itrmax
      logical, optional, intent(in) :: restart
      logical :: skip = .false.

      if (obj%explicit .eqv. .true.) then
         ! explicit solver is utilized for time-integral.
         ! Default Algorithm is RK-4
         call obj%updateByRK4()
         return
      end if

      if (present(restart)) then
         skip = .true.
      end if

      defaultsolver = "GaussJordan"

      if (present(SolverType)) then
         solver = Solvertype
      else
         solver = defaultsolver
      end if

      n = size(obj%FEMDomain%Mesh%NodCoord, 1)
      allocate (Amat(n, n), bvec(n), xvec(n))
      Amat(:, :) = 0.0d0
      bvec(:) = 0.0d0

      !===================================
      ! assemble matrix
      n = size(obj%FEMDomain%Mesh%ElemNod, 1)
      m = size(obj%FEMDomain%Mesh%ElemNod, 2)
      do i = 1, n
         do j = 1, m
            nodeid1 = obj%FEMDomain%Mesh%ElemNod(i, j)
            do k = 1, m
               nodeid2 = obj%FEMDomain%Mesh%ElemNod(i, k)
               Amat(nodeid1, nodeid2) = Amat(nodeid1, nodeid2) &
                                        + obj%DiffusionMat(i, j, k)
            end do
            bvec(nodeid1) = bvec(nodeid1) + obj%Flowvector(i, j)
         end do
      end do
      !===================================

      !===================================
      xvec(:) = 0.0d0
      ! set initial value for xvec
      !
      if (allocated(obj%FEMDomain%Boundary%TboundNodID)) then

         if (obj%step == 1 .and. skip .eqv. .false.) then
            do i = 1, size(obj%FEMDomain%Boundary%TboundNodID, 1)
               if (obj%FEMDomain%Boundary%TboundNodID(i, 1) >= 1) then
                  xvec(obj%FEMDomain%Boundary%TBoundNodID(i, 1)) = obj%FEMDomain%Boundary%TboundVal(i, 1)
               end if
            end do
         elseif (obj%step >= 2) then
            !=====================================
            ! Export Values to Element-by-Element form
            !n=size(obj%FEMDomain%Mesh%ElemNod,1)
            !m=size(obj%FEMDomain%Mesh%ElemNod,2)
            !do i=1,n
            !    do j=1,m
            !        nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            !        xvec(nodeid1)=obj%UnknownValue(i,j)
            !    enddo
            !enddo
            !=====================================
         else
            print *, "ERROR :: Diffusion%solve() >> not initialized by %setup()"
            stop
         end if
      end if

      !===================================
      ! show initial value
      !
      if (allocated(obj%FEMDomain%Boundary%TboundNodID)) then

         if (obj%step == 1 .and. skip .eqv. .false.) then
            n = size(obj%FEMDomain%Mesh%ElemNod, 1)
            m = size(obj%FEMDomain%Mesh%ElemNod, 2)
            do i = 1, n
               do j = 1, m
                  nodeid1 = obj%FEMDomain%Mesh%ElemNod(i, j)
                  obj%UnknownValue(i, j) = xvec(nodeid1)
               end do
            end do
            call obj%Display(DisplayMode="Gmsh", OptionalStep=0)

         elseif (obj%step >= 2) then
            !=====================================
            ! Export Values to Element-by-Element form
            !n=size(obj%FEMDomain%Mesh%ElemNod,1)
            !m=size(obj%FEMDomain%Mesh%ElemNod,2)
            !do i=1,n
            !    do j=1,m
            !        nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            !        xvec(nodeid1)=obj%UnknownValue(i,j)
            !    enddo
            !enddo
            !=====================================
         else
            print *, "ERROR :: Diffusion%solve() >> not initialized by %setup()"
            stop
         end if
      end if
      !===================================

      !===================================
      ! introduce D.B.C
      n = size(obj%FEMDomain%Boundary%DBoundNodID, 1)
      do i = 1, n
         nodeid1 = obj%FEMDomain%Boundary%DBoundNodID(i, 1)
         if (nodeid1 < 1) then
            cycle
         else
            val = obj%FEMDomain%Boundary%DBoundVal(i, 1)

            do j = 1, size(bvec)
               bvec(j) = bvec(j) - Amat(nodeid1, j)*val
            end do
            Amat(nodeid1, :) = 0.0d0
            Amat(:, nodeid1) = 0.0d0
            Amat(nodeid1, nodeid1) = 1.0d0
            bvec(nodeid1) = val
            xvec(nodeid1) = val
         end if
      end do
      !===================================

      itrmax = 1000
      er = 1.0e-15
      n = size(bvec)

      if (solver == "GaussJordan") then
         print *, "Solver type :: GaussJordan"
         call gauss_jordan_pv(Amat, xvec, bvec, size(xvec))
      elseif (solver == "BiCGSTAB") then
         print *, "Solver type :: BiCGSTAB"
         call bicgstab_Diffusion(Amat, bvec, xvec, size(xvec), itrmax, er, obj%FEMDomain%Boundary%DBoundNodID, &
                                 obj%FEMDomain%Boundary%DBoundVal)
      else
         print *, "Critical ERROR :: No solvers are selected in DiffusionEqCl"
         stop
      end if

      !=====================================
      ! Export Values to Element-by-Element form
      if (.not. allocated(obj%UnknownVec)) then
         allocate (obj%UnknownVec(size(xvec)))
      end if
      obj%UnknownVec(:) = xvec(:)
      n = size(obj%FEMDomain%Mesh%ElemNod, 1)
      m = size(obj%FEMDomain%Mesh%ElemNod, 2)
      do i = 1, n
         do j = 1, m
            nodeid1 = obj%FEMDomain%Mesh%ElemNod(i, j)
            obj%UnknownValue(i, j) = xvec(nodeid1)
         end do
      end do
      !=====================================

   end subroutine

!######################## Solve DiffusionEq ########################

!######################## Initialize DiffusionEq ########################
   subroutine UpdateDiffusionEq(obj, explicit)
      class(DiffusionEq_), intent(inout)::obj
      logical, optional, intent(in) :: explicit

      if (present(explicit)) then
         obj%explicit = explicit
      end if

      obj%step = obj%step + 1
      call UpdateUnknownValue(obj)

      if (obj%explicit .eqv. .true.) then
         return
      end if

      call GetDiffusionMat(obj)
      call GetFlowvector(obj)

   end subroutine
!######################## Initialize DiffusionEq ########################

!######################## Initialize DiffusionEq ########################
   subroutine SetupDiffusionEq(obj, explicit)
      class(DiffusionEq_), intent(inout)::obj
      logical, optional, intent(in) :: explicit

      if (present(explicit)) then
         obj%explicit = explicit
      end if

      obj%step = 1
      if (obj%dt == 0.0d0 .or. obj%dt /= obj%dt) then
         obj%dt = 1.0d0
      end if

      call GetUnknownValue(obj)

      call GetDivergence(obj)

      if (obj%explicit .eqv. .true.) then
         return
      end if

      call GetDiffusionMat(obj)
      call GetFlowvector(obj)

   end subroutine
!######################## Initialize DiffusionEq ########################

!######################## Assemble Element Matrix ##########################
   subroutine GetDiffusionMat(obj)
      class(DiffusionEq_), intent(inout)::obj

      integer(int32) :: nod_num, dim_num, elemnod_num, elem_num
      integer(int32) :: i, j, k
      real(real64) :: diff_coeff
      real(real64), allocatable::DiffMat(:, :), MassMat(:, :), Cvec(:), Flux(:)

      nod_num = size(obj%FEMDomain%Mesh%NodCoord, 1)
      elem_num = size(obj%FEMDomain%Mesh%ElemNod, 1)
      elemnod_num = size(obj%FEMDomain%Mesh%ElemNod, 2)
      dim_num = size(obj%FEMDomain%Mesh%NodCoord, 2)

      allocate (Cvec(elemnod_num), Flux(elemnod_num))

      if (.not. allocated(obj%FlowVector)) allocate (obj%FlowVector(elem_num, elemnod_num))
      if (.not. allocated(obj%UnknownValue)) allocate (obj%UnknownValue(elem_num, elemnod_num))
      if (.not. allocated(obj%DiffusionMat)) allocate (obj%DiffusionMat(elem_num, elemnod_num, elemnod_num))
      if (.not. allocated(obj%FluxVector3D)) allocate (obj%FluxVector3D(elem_num, elemnod_num))
      obj%FlowVector(:, :) = 0.0d0
      obj%FluxVector3D(:, :) = 0.0d0
      obj%DiffusionMat(:, :, :) = 0.0d0

      obj%FEMDomain%ShapeFunction%ElemType = obj%FEMDomain%Mesh%ElemType
      call SetShapeFuncType(obj%FEMDomain%ShapeFunction)

      !call showArraySize(obj%FluxVector3D)
      !call showArraySize(Flux)

      do i = 1, elem_num
         Cvec(:) = obj%UnknownValue(i, :)
         do j = 1, obj%FEMDomain%ShapeFunction%NumOfGp

            diff_coeff = obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i), 1)

            ! allow direct-import of Permiability
            if (allocated(obj%Permiability)) then
               if (size(obj%Permiability) /= elem_num) then
                  print *, "ERROR :: FiniteDeform :: size(obj%Permiability/=elem_num)"
               else
                  diff_coeff = obj%Permiability(i)
               end if
            end if

            call getAllShapeFunc(obj%FEMDomain%ShapeFunction, elem_id=i, nod_coord=obj%FEMDomain%Mesh%NodCoord, &
                                 elem_nod=obj%FEMDomain%Mesh%ElemNod, OptionalGpID=j)
            call getElemDiffusionMatrix(obj%FEMDomain%ShapeFunction, diff_coeff, DiffMat)
            call getElemFluxVec(obj%FEMDomain%ShapeFunction, diff_coeff, Flux, Cvec)
            call getElemMassMatrix(obj%FEMDomain%ShapeFunction, MassMat)

            do k = 1, size(DiffMat, 1)
               obj%DiffusionMat(i, k, :) = obj%DiffusionMat(i, k, :) - obj%dt/2.0d0*DiffMat(k, :) &
                                           + MassMat(k, :)
            end do

            obj%FluxVector3D(i, :) = obj%FluxVector3D(i, :) + Flux(:)

            DiffMat(:, :) = 0.0d0
            MassMat(:, :) = 0.0d0

         end do
      end do

   end subroutine
!######################## Assemble Element Matrix ##########################

!######################## Get Flow-vector ##########################
   subroutine GetFlowvector(obj)
      class(DiffusionEq_), intent(inout)::obj

      integer(int32) ::  i, j, k, n, m, num_of_nbc, node_id, num_of_elem, num_of_elemnod
      real(real64), allocatable::MassMat(:, :), RHSvector(:), diff_coeff, DiffMat(:, :)

      obj%Flowvector(:, :) = 0.0d0
      num_of_elem = size(obj%FEMDomain%Mesh%ElemNod, 1)
      num_of_elemnod = size(obj%FEMDomain%Mesh%ElemNod, 2)
      num_of_nbc = size(obj%FEMDomain%Boundary%NBoundNodID, 1)

      if (.not. allocated(obj%Flowvector)) then
         allocate (obj%Flowvector(size(obj%FEMDomain%Mesh%ElemNod, 1), size(obj%FEMDomain%Mesh%ElemNod, 2)))
      else
         if (size(obj%Flowvector, 1) /= num_of_elem .or. size(obj%Flowvector, 2) /= num_of_elemnod) then
            deallocate (obj%Flowvector)
            allocate (obj%Flowvector(size(obj%FEMDomain%Mesh%ElemNod, 1), size(obj%FEMDomain%Mesh%ElemNod, 2)))
         end if

      end if

      !get scalar value

      do i = 1, num_of_nbc
         node_id = obj%FEMDomain%Boundary%NBoundNodID(i, 1)
         n = 0
         if (node_id > 0) then
            do j = 1, num_of_elem
               do k = 1, num_of_elemnod
                  if (node_id == obj%FEMDomain%Mesh%ElemNod(j, k)) then
                     !obj%Flowvector(j,k) =  - obj%FEMDomain%Boundary%NBoundVal(i,1)
                     !
                     obj%Flowvector(j, k) = -obj%dt*obj%FEMDomain%Boundary%NBoundVal(i, 1)
                     !obj%Flowvector(j,k) = obj%Flowvector(j,k) - obj%dt * obj%FEMDomain%Boundary%NBoundVal(i,1)

                     n = 1
                     exit
                  end if
               end do
               if (n == 1) then
                  exit
               end if
            end do
         else
            cycle
         end if
      end do

      !Add time integration terms
      obj%FEMDomain%ShapeFunction%ElemType = obj%FEMDomain%Mesh%ElemType
      call SetShapeFuncType(obj%FEMDomain%ShapeFunction)
      allocate (RHSvector(num_of_elemnod))

      do i = 1, num_of_elem
         do j = 1, obj%FEMDomain%ShapeFunction%NumOfGp

            diff_coeff = obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i), 1)

            ! allow direct-import of Permiability
            if (allocated(obj%Permiability)) then
               if (size(obj%Permiability) /= num_of_elem) then
                  print *, "ERROR :: FiniteDeform :: size(obj%Permiability/=num_of_elem)"
               else
                  diff_coeff = obj%Permiability(i)
               end if
            end if

            call GetAllShapeFunc(obj%FEMDomain%ShapeFunction, elem_id=i, nod_coord=obj%FEMDomain%Mesh%NodCoord, &
                                 elem_nod=obj%FEMDomain%Mesh%ElemNod, OptionalGpID=j)
            call GetElemMassMatrix(obj%FEMDomain%ShapeFunction, MassMat)
            call getElemDiffusionMatrix(obj%FEMDomain%ShapeFunction, diff_coeff, DiffMat)
            !RHSvector(:)=obj%dt/2.0d0*obj%UnknownValueInit(i,:)!+obj%UnknownValueRate(i,:)
            RHSvector(:) = obj%UnknownValueInit(i, :)!+obj%UnknownValueRate(i,:)
            obj%Flowvector(i, :) = obj%Flowvector(i, :) + matmul(MassMat, RHSvector) + &
                                   obj%dt/2.0d0*matmul(DiffMat, RHSvector) !obj%Divergence(i,:)

            MassMat(:, :) = 0.0d0
            DiffMat(:, :) = 0.0d0

         end do
      end do

   end subroutine GetFlowvector
!######################## Get Flow-vector ##########################

!######################## Get UnknownValue ##########################
   subroutine GetUnknownValue(obj)
      class(DiffusionEq_), intent(inout)::obj

      integer(int32) :: i, j, k, n, m, nodeid
      real(real64) :: val

      n = size(obj%FEMDomain%Mesh%ElemNod, 1)
      m = size(obj%FEMDomain%Mesh%ElemNod, 2)
      if (.not. allocated(obj%UnknownValue)) then
         allocate (obj%UnknownValue(n, m))
      elseif (size(obj%UnknownValue, 1) /= n .or. size(obj%UnknownValue, 1) /= m) then
         deallocate (obj%UnknownValue)
         allocate (obj%UnknownValue(n, m))
      else
      end if
      obj%UnknownValue(:, :) = 0.0d0

      if (.not. allocated(obj%UnknownValueInit)) then
         allocate (obj%UnknownValueInit(n, m))
      elseif (size(obj%UnknownValueInit, 1) /= n .or. size(obj%UnknownValueInit, 1) /= m) then
         deallocate (obj%UnknownValueInit)
         allocate (obj%UnknownValueInit(n, m))
      else
      end if

      obj%UnknownValueInit(:, :) = 0.0d0

      if (allocated(obj%FEMDomain%Boundary%TBoundNum)) then
         if (obj%FEMDomain%Boundary%TBoundNum(1) /= 0) then
            do i = 1, obj%FEMDomain%Boundary%TBoundNum(1)
               nodeid = obj%FEMDomain%Boundary%TBoundNodID(i, 1)
               val = obj%FEMDomain%Boundary%TBoundVal(i, 1)
               do j = 1, size(obj%FEMDomain%Mesh%ElemNod, 1)
                  do k = 1, size(obj%FEMDomain%Mesh%ElemNod, 2)
                     if (obj%FEMDomain%Mesh%ElemNod(j, k) == nodeid) then
                        obj%UnknownValueInit(j, k) = val
                     end if
                  end do
               end do
            end do
         end if
      end if

      !===================================
      ! introduce D.B.C
      if (.not. allocated(obj%FEMDomain%Boundary%DBoundNodID)) then
         stop "No obj%FEMDomain%Boundary%DBoundNodID is installed"
      end if
      n = size(obj%FEMDomain%Boundary%DBoundNodID, 1)
      do i = 1, n
         nodeid = obj%FEMDomain%Boundary%DBoundNodID(i, 1)
         if (nodeid < 1) then
            cycle
         else
            val = obj%FEMDomain%Boundary%DBoundVal(i, 1)
            do j = 1, size(obj%FEMDomain%Mesh%ElemNod, 1)
               do k = 1, size(obj%FEMDomain%Mesh%ElemNod, 2)
                  if (obj%FEMDomain%Mesh%ElemNod(j, k) == nodeid) then
                     obj%UnknownValueInit(j, k) = val
                     obj%UnknownValue(j, k) = val
                  end if
               end do
            end do
         end if
      end do
      !===================================

      if (.not. allocated(obj%UnknownValueRate)) then
         allocate (obj%UnknownValueRate(n, m))
      elseif (size(obj%UnknownValueRate, 1) /= n .or. size(obj%UnknownValueRate, 1) /= m) then
         deallocate (obj%UnknownValueRate)
         allocate (obj%UnknownValueRate(n, m))
      else
      end if

      obj%UnknownValue(:, :) = obj%UnknownValueInit(:, :)
      obj%UnknownValueRate(:, :) = 0.0d0

   end subroutine GetUnknownValue
!######################## Get UnknownVector ##########################

!######################## Get UnknownValue ##########################
   subroutine GetDivergence(obj)
      class(DiffusionEq_), intent(inout)::obj

      integer(int32) :: i, j, k, n, m, nodeid
      real(real64) :: val

      n = size(obj%FEMDomain%Mesh%ElemNod, 1)
      m = size(obj%FEMDomain%Mesh%ElemNod, 2)
      if (.not. allocated(obj%Divergence)) then
         allocate (obj%Divergence(n, m))
      elseif (size(obj%Divergence, 1) /= n .or. size(obj%Divergence, 1) /= m) then
         deallocate (obj%Divergence)
         allocate (obj%Divergence(n, m))
      else
      end if
      obj%Divergence(:, :) = 0.0d0

   end subroutine
!######################## Get UnknownVector ##########################

!######################## Update UnknownVector ##########################
   subroutine UpdateUnknownValue(obj)
      class(DiffusionEq_), intent(inout)::obj

    !!call showArraySize(obj%UnknownValueRate)
      !call showArraySize(obj%UnknownValueRate)
      !call showArraySize(obj%UnknownValueRate)

      !obj%UnknownValueRate(:,:)=-obj%UnknownValueRate(:,:)+&
      !    2.0d0/obj%dt*(obj%UnknownValue(:,:)-obj%UnknownValueInit(:,:) )
      obj%UnknownValueInit(:, :) = obj%UnknownValue(:, :)

   end subroutine
!######################## Update UnknownVector ##########################

!################## Elemental Entities ##################
   subroutine GetElemDiffusionMatrix(obj, diff_coeff, DiffMat)
      class(ShapeFunction_), intent(inout)::obj
      real(real64), intent(in)::diff_coeff
      real(real64), allocatable, intent(inout)::DiffMat(:, :)
      integer(int32) :: i, j, n
      real(real64) :: signm_modifier
      n = size(obj%dNdgzi, 2)
      if (size(DiffMat, 1) /= n .or. size(DiffMat, 2) /= n) then
         if (allocated(DiffMat)) then
            deallocate (DiffMat)
         end if
         allocate (DiffMat(n, n))
      end if

      ! diff_coeff should be negative
      if (diff_coeff > 0.0d0) then
         signm_modifier = -1.0d0
      else
         signm_modifier = 1.0d0
      end if

      if (.not. allocated(DiffMat)) then
         allocate (DiffMat(n, n))
      end if

      DiffMat(:, :) = 0.0d0

      DiffMat(:, :) = matmul(transpose(matmul(obj%JmatInv, obj%dNdgzi)), &
                             matmul(obj%JmatInv, obj%dNdgzi))*signm_modifier*diff_coeff*det_mat(obj%JmatInv, size(obj%JmatInv, 1))
   end subroutine GetElemDiffusionMatrix
!################## Elemental Entities ##################

!################## Elemental Entities ##################
   subroutine getElemFluxVec(obj, diff_coeff, Flux, Cvec)
      class(ShapeFunction_), intent(inout)::obj
      real(real64), intent(in)::diff_coeff
      real(real64), intent(in)::Cvec(:)
      real(real64), allocatable, intent(inout)::Flux(:)
      integer(int32) :: i, j, n
      real(real64) :: signm_modifier
      n = size(obj%dNdgzi, 2)
      if (size(Flux, 1) /= n) then
         if (allocated(Flux)) then
            deallocate (Flux)
         end if
         allocate (Flux(n))
      end if

      ! diff_coeff should be negative
      if (diff_coeff > 0.0d0) then
         signm_modifier = -1.0d0
      else
         signm_modifier = 1.0d0
      end if

      Flux(:) = 0.0d0

      Flux(:) = signm_modifier*diff_coeff*det_mat(obj%Jmat, size(obj%Jmat, 1)) &
                *matmul(obj%JmatInv, matmul(obj%dNdgzi, cvec))

   end subroutine GetElemFluxVec
!################## Elemental Entities ##################

!################## Elemental Entities ##################
   subroutine GetElemMassMatrix(obj, MassMat)
      class(ShapeFunction_), intent(inout)::obj
      real(real64), allocatable, intent(inout)::MassMat(:, :)
      integer(int32) :: i, j, n

      n = size(obj%dNdgzi, 2)
      if (.not. allocated(MassMat)) allocate (MassMat(n, n))
      if (size(MassMat, 1) /= n .or. size(MassMat, 2) /= n) then
         if (allocated(MassMat)) then
            deallocate (MassMat)
         end if
         allocate (MassMat(n, n))
      end if

      MassMat(:, :) = 0.0d0
      MassMat(:, :) = diadic(obj%Nmat, obj%Nmat)*det_mat(obj%Jmat, size(obj%Jmat, 1))

   end subroutine GetElemMassMatrix
!################## Elemental Entities ##################

!################## Elemental Entities ##################
   subroutine DisplayDiffusionEq(obj, OptionalProjectName, DisplayMode, OptionalStep, Name)
      class(DiffusionEq_), intent(inout)::obj
      character(*), optional, intent(in) :: OptionalProjectName, DisplayMode, Name
      logical::withMsh
      integer(int32), optional, intent(in)::OptionalStep
      integer(int32) :: i, j, n, m, step

      if (.not. associated(obj%FEMDomain)) then
         return
      end if
      if (obj%FEMDomain%Mesh%empty() .eqv. .true.) then
         return
      end if

      if (present(OptionalStep)) then
         step = OptionalStep
      else
         step = 1

      end if

      if (step == 1) then
         withMsh = .true.
      else
         withMsh = .false.
      end if

      if (present(DisplayMode)) then
         if (DisplayMode == "Terminal" .or. DisplayMode == "terminal") then
            do i = 1, size(obj%UnknownValue, 1)
               print *, obj%UnknownValue(i, :)
            end do
         elseif (DisplayMode == "gmsh" .or. DisplayMode == "Gmsh") then
            call GmshPlotContour(obj%FEMDomain, gp_value=obj%UnknownValue, OptionalStep=step)
            call GmshPlotVector(obj%FEMDomain, Vector=obj%FluxVector3D, Name=obj%FEMDomain%FileName, &
                                FieldName="Flux", Step=step, ElementWize=.true., withMsh=withMsh)
         elseif (DisplayMode == "gnuplot" .or. DisplayMode == "Gnuplot") then
            call GnuplotPlotContour(obj%FEMDomain, obj%UnknownValue, OptionalStep=step)

         else
            print *, "Invalid DisplayMode >> DisplayDiffusionEq "
            print *, "DisplayMode '", DisplayMode, "' is not defined"
         end if
         return
      end if
   end subroutine
!################## Elemental Entities ##################

   function Solve_oneline_DiffusionEq(this, FEMDomains, DiffusionCoeff, Reaction, penalty, &
                                FixBoundary, FixValue, C_n, dt, RHS, Matrix, algorithm, AL_lambda, AL_tol, AL_alpha) result(C_field)
      class(DiffusionEq_), intent(inout) :: this
      type(FEMDomain_), intent(inout) :: FEMDomains(:)
      real(real64), intent(in) :: DiffusionCoeff(:), Reaction(:), penalty, FixValue(:), &
                                  C_n(:), dt

      integer(int32), intent(in) :: FixBoundary(:)
      real(real64), allocatable :: C_field(:), bvector(:), bvector_n(:), cell_centered_diag(:), &
                                   k1(:), k2(:), k3(:), k4(:), expA_Cn(:), bhat(:)
      real(real64), allocatable, optional, intent(inout) :: RHS(:)
      type(CRS_), optional, intent(inout) :: Matrix
      integer(int32), allocatable :: NumberOfElement(:), overset_coeff(:)
      character(*), optional, intent(in) :: algorithm

      ! >>>> variables for argumented Lagrangian method >>>>>>>>>
      real(real64), optional, intent(in) :: AL_lambda, AL_tol, AL_alpha
      ! size = number of projection point
      real(real64), allocatable :: lambda_N(:), lambda(:), gap(:)
      ! <<<< variables for argumented Lagrangian method <<<<<<<<<

      real(real64) :: h, epsilon
      integer(int32) :: total_ElementID, DomainID, ElementID, offset, i, itr, itrmax
      type(CRS_) :: Mmatrix, Kmatrix, Amatrix, Nmatrix
      type(IO_) :: f

      call this%solver%init(NumDomain=size(FEMDomains))
      call this%solver%setDomain(FEMDomains=FEMDomains)
      call this%solver%setCRS(DOF=1)

      offset = 0
      do DomainID = 1, size(FEMDomains)
         if (FEMDomains(DomainID)%empty()) cycle
         overset_coeff = FEMDomains(DomainID)%getNumberOfOversetForElement()
         overset_coeff = 1!overset_coeff + 1
         do ElementID = 1, femdomains(DomainID)%ne()
            call this%solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=1, &
                                       Matrix=femdomains(DomainID)%MassMatrix( &
                                       ElementID=ElementID, &
                                       Density=1.0d0/overset_coeff(ElementID), &
                                       DOF=1 &
                                       ))
         end do
         offset = offset + femdomains(DomainID)%ne()
      end do

      call this%solver%keepThisMatrixAs("M")
      call this%solver%zeros()

      offset = 0
      do DomainID = 1, size(FEMDomains)
         overset_coeff = FEMDomains(DomainID)%getNumberOfOversetForElement()
         overset_coeff = 1!overset_coeff + 1

         do ElementID = 1, FEMDomains(DomainID)%ne()
            call this%solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=1, &
                                       Matrix=FEMDomains(DomainID)%DiffusionMatrix( &
                                       ElementID=ElementID, &
                                       D=DiffusionCoeff(offset + ElementID)/overset_coeff(ElementID) &
                                       ))
            call this%solver%setVector(DomainID=DomainID, ElementID=ElementID, DOF=1, &
                                       Vector=FEMDomains(DomainID)%MassVector( &
                                       ElementID=ElementID, &
                                       DOF=1, &
                                       Density=Reaction(offset + ElementID)/overset_coeff(ElementID), &
                                       Accel=[1.0d0]/overset_coeff(ElementID) &
                                       ) &
                                       )
         end do
         offset = offset + femdomains(DomainID)%ne()
      end do

      !call this%solver%setEbOM(penalty=penalty, DOF=1)
      call this%solver%keepThisMatrixAs("K")
      bvector = this%solver%CRS_RHS
      call this%solver%zeros()

      call this%solver%setEbOM(penalty=1.0d0, DOF=1)
      call this%solver%keepThisMatrixAs("N")
      !call this%solver%zeros()

      Mmatrix = this%solver%getCRS("M")
      Kmatrix = this%solver%getCRS("K")
      Nmatrix = this%solver%getCRS("N")

      this%solver%CRS_RHS = bvector
      if (.not. allocated(this%CRS_RHS_n)) then
         this%CRS_RHS_n = this%solver%CRS_RHS
      end if

      if (present(AL_lambda)) then
         if (.not. present(AL_tol) .or. .not. present(AL_alpha)) then
            print *, "[ERROR] DIFFUSIONEQUATION>> .not.present(AL_tol) .or. .not.present(AL_alpha)"
            stop
         end if
         ! Argumented lagrangian method
         epsilon = penalty

         ! compute [M]^{-1}
         cell_centered_diag = Mmatrix%diag(cell_centered=.true.)
         ! compute [M]^{-1} [D]
         Amatrix = (-1.0d0)*(Kmatrix%divide_by(cell_centered_diag) + &
                             epsilon*Nmatrix%divide_by(cell_centered_diag))

         !call Amatrix%fix(idx=FixBoundary,val=FixValue,RHS=bvector)

         ! compute g(c)
         lambda = AL_lambda*eyes(this%solver%num_EbOFEM_projection_point())
         ! compute gap
         gap = this%solver%get_gap_function(x=C_n, DOF=1)
         ! lambda_{k+1} = lambda_{k} + epsilon*g
         lambda = lambda + epsilon*gap
         ! compute lambda_{i}*N_{i}
         lambda_N = this%solver%argumented_Lagrangian_RHS(lambda=lambda, DOF=1) ! size = nn()
         bvector = (this%solver%CRS_RHS + lambda_N)/cell_centered_diag*dt
         ! compute [exp([M]^{-1} [D]) ]{c}_{n}
         expA_Cn = Amatrix%tensor_exponential(x=C_n, dt=dt, itr_tol=1000000, tol=dble(1.0e-30))
         ! compute [exp([M]^{-1} [D]) ]{c}_{n}
         C_field = expA_Cn + bvector
         ! fix dirichlet boundary
         C_field(FixBoundary(:)) = FixValue(:)

         ! Argumented lagrangian loop
         do itr = 1, this%solver%itrmax
            print *, itr, norm(gap), maxval(abs(gap))

            ! solve
            ! [M]{dc/ct} - [D]{c} + lambda_{i}*{N}_{i} - {f} = 0
            ! {dc/ct} - [M]^{-1}[D]{c} + [M]^{-1}(lambda_{i}*{N}_{i}) - [M]^{-1}{f}= 0
            ! {c}_{n+1} = [exp([M]^{-1}[D])]{c}_{n} - [M]^{-1}(lambda_{i}*{N}_{i})dt + [M]^{-1}{f}dt

            ! compute gap
            gap = this%solver%get_gap_function(x=C_field, DOF=1)
            ! lambda_{k+1} = lambda_{k} + epsilon*g
            lambda = lambda + epsilon*gap
            epsilon = epsilon*AL_alpha
            ! compute lambda_{i}*[N]_{i}
            lambda_N = this%solver%argumented_Lagrangian_RHS(lambda=lambda, DOF=1)
            ! check convergence
            if (maxval(abs(gap)) < AL_tol) then
               exit
            end if

            ! compute [M]^{-1} [D]
            Amatrix = (-1.0d0)*(Kmatrix%divide_by(cell_centered_diag) + &
                                epsilon*Nmatrix%divide_by(cell_centered_diag))
            bvector = (this%solver%CRS_RHS + lambda_N)/cell_centered_diag*dt
            !call Amatrix%fix(idx=FixBoundary,RHS=bvector,val=FixValue)
            ! compute [exp([M]^{-1} [D]) ]{c}_{n}
            expA_Cn = Amatrix%tensor_exponential(x=C_n, dt=dt, itr_tol=1000000, tol=dble(1.0e-30))
            ! {c}_{n+1} = [exp([M]^{-1}[D])]{c}_{n} - [M]^{-1}(lambda_{i}*{N}_{i})dt + [M]^{-1}{f}dt
            C_field = expA_Cn + bvector
            ! fix Dirichlet boundary
            C_field(FixBoundary(:)) = FixValue(:)

         end do

      else
         Kmatrix = Kmatrix + penalty*Nmatrix
         if (present(algorithm)) then
            if (algorithm == "RK4") then
               ! solve by 4th order Runge-Kutta
               !(1) M => M-(cell-centered)
               cell_centered_diag = Mmatrix%diag(cell_centered=.true.)
               Amatrix = Kmatrix%divide_by(cell_centered_diag)

               bvector_n = this%CRS_RHS_n/cell_centered_diag
               bvector = this%solver%CRS_RHS/cell_centered_diag
               bhat = zeros(size(bvector))
               call Amatrix%fix(idx=FixBoundary, RHS=bhat, val=FixValue)
               !call Amatrix%fix(idx=FixBoundary,RHS=bvector,val=FixValue)
               ! {dc/dt} = [M^-1][K]{c} + [M^-1]{R}
               ! {dc/dt} = [A]{c} + {b}
               ! Ready for RK4
               bvector = bvector + bhat
               h = dt
               k1 = -1.0d0*Amatrix%matmul(C_n) + bvector
               k2 = -1.0d0*Amatrix%matmul(C_n + h/2*k1) + (bvector)
               k3 = -1.0d0*Amatrix%matmul(C_n + h/2*k2) + (bvector)
               k4 = -1.0d0*Amatrix%matmul(C_n + h*k3) + bvector
               C_field = C_n + h/6*(k1 + 2*k2 + 2*k3 + k4)
               C_field(FixBoundary(:)) = FixValue(:)

               if (present(RHS)) then
                  RHS = this%solver%CRS_RHS
               end if
               if (present(Matrix)) then
                  Matrix = this%solver%getCRS()
               end if
               call this%solver%remove()
               return
            elseif (algorithm == "ForwardEuler") then
               ! solve by 4th order Runge-Kutta
               !(1) M => M-(cell-centered)
               cell_centered_diag = Mmatrix%diag(cell_centered=.true.)
               !Amatrix = Mmatrix
               Amatrix = (-1.0d0)*Kmatrix%divide_by(cell_centered_diag)
               bvector = this%solver%CRS_RHS/cell_centered_diag*dt
               bhat = zeros(size(bvector))
               call Amatrix%fix(idx=FixBoundary, RHS=bhat, val=FixValue)
               ! {dc/dt} = [M^-1][K]{c} + [M^-1]{R}
               ! {dc/dt} = [A]{c} + {b}
               ! Ready for RK4

               C_field = C_n + dt*Amatrix%matmul(C_n) + bvector - bhat*dt

               C_field(FixBoundary(:)) = FixValue(:)

               if (present(RHS)) then
                  RHS = this%solver%CRS_RHS
               end if
               if (present(Matrix)) then
                  Matrix = this%solver%getCRS()
               end if
               call this%solver%remove()
               return
            end if
         end if
         ! sparse matrix
         ! exponential integral
         cell_centered_diag = Mmatrix%diag(cell_centered=.true.)
         Amatrix = (-1.0d0)*Kmatrix%divide_by(cell_centered_diag)
         bvector = this%solver%CRS_RHS/cell_centered_diag*dt

         !bhat = zeros(size(bvector) )
         !call Amatrix%fix(idx=FixBoundary,RHS=bhat,val=FixValue)
         !bvector = bvector - bhat
         !bvector(FixBoundary(:) ) = 0.0d0

         expA_Cn = Amatrix%tensor_exponential(x=C_n, dt=dt, itr_tol=1000000, tol=dble(1.0e-30), &
                                              fix_idx=FixBoundary, fix_val=FixValue)
         !bhat = 0.0d0

         call Amatrix%fix(idx=FixBoundary, RHS=bvector, val=FixValue*0.0d0)
         if (norm(bvector) /= 0.0d0) then
            call Amatrix%BICGSTAB(x=bhat, b=bvector, debug=.true., tol=dble(1.0e-8))
         else
            bhat = bvector
         end if

         C_field = expA_Cn - bhat
         C_field(FixBoundary(:)) = FixValue(:)
      end if

      if (present(RHS)) then
         RHS = this%solver%CRS_RHS
      end if
      if (present(Matrix)) then
         Matrix = this%solver%getCRS()
      end if

      call this%solver%remove()
   end function
! #######################################################

! #######################################################
   subroutine check_stability_conditionDiffusionEq(this, dt, dx, coefficient, passed)
      class(DiffusionEq_), intent(in) :: this
      real(real64), intent(in) :: dt, dx, coefficient
      logical :: passed
      !http://es.ris.ac.jp/~yoshizaki/lecture/2013_1st/SynopticMet/S_05.pdf
      print *, ">>>>>>>>>>>>>>>>>>>>>>>>"
      print *, ">>[checking stability condition...]>>"
      if (dt <= dx*dx/2.0d0/coefficient) then
         print *, "[ok] dt <= dxdx/2/a"
         print *, dt, "<=", dx*dx/2.0d0/coefficient
         passed = .true.
      else
         print *, "[WARNING] dt > dxdx/2/a"
         print *, dt, ">", dx*dx/2.0d0/coefficient
         passed = .false.
      end if
      print *, ">>>>>>>>>>>>>>>>>>>>>>>>"

   end subroutine

end module DiffusionEquationClass
