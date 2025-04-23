module TSFEMClass
   use WaveKernelClass
   use ArrayClass
   use MPIClass
   implicit none

   type :: TSFEM_
      type(WaveKernel_) :: WK
      type(FEMDomainp_) :: femdomain
      type(MPI_), pointer :: mpid => null()

      ! >>> boundary condition
      integer(int32), allocatable :: fix_idx(:)
      real(real64), allocatable :: f_n(:)
      ! <<< boundary condition

      ! >>> solutions >>>
      real(real64), allocatable :: u(:), v(:)
      real(real64), allocatable :: u_n(:), v_n(:)

      ! HPF
      real(real64), allocatable :: u_2(:), v_2(:)
      real(real64), allocatable :: u_1(:), v_1(:)
      real(real64), allocatable :: u_old(:), v_old(:)
      ! <<< solutions <<<

      ! >>> material parameters >>>
      real(real64), allocatable :: YoungModulus(:)
      real(real64), allocatable :: PoissonRatio(:)
      real(real64), allocatable :: Density(:)
      ! <<< material parameters <<<

      real(real64) :: relative_cutoff_frequency = 1.0d0
      real(real64) :: dt = 1.0d0/100.0d0
      real(real64) :: t = 0.0d0
      integer(int32) :: timestep = 0
      integer(int32) :: start_step = 1
      integer(int32) :: interval = 0

      integer(int32) :: itrmax = 50
      integer(int32) :: DOF
      real(real64)   :: tol = dble(1.0e-25)

      ! for debug
      !complex(real64),allocatable :: ab_weight_cos(:)
      !complex(real64),allocatable :: ab_weight_sinc(:)

      logical :: initialized = .false.
   contains
      procedure, public :: init => initTSFEM
      procedure, public :: DirichletBoundary => DirichletBoundaryTSFEM

      procedure, pass :: NeumannBoundaryTSFEM
      procedure, pass :: NeumannBoundary_multiple_ForceTSFEM
      generic, public :: NeumannBoundary => NeumannBoundary_multiple_ForceTSFEM, &
         NeumannBoundaryTSFEM

      procedure, public :: time => timeTSFEM
      !procedure, public :: update => updateTSFEM
      procedure, public :: save => saveTSFEM
      procedure, public :: movie => movieTSFEM

      ! << UNRECOMMENDED for many bugs>>
      procedure, public :: AbsorbingBoundary => AbsorbingBoundaryTSFEM
   end type
contains
! ##############################################
   subroutine initTSFEM(this, femdomain, Density, YoungModulus, PoissonRatio, DOF, mpid)
      class(TSFEM_), intent(inout) :: this
      type(FEMDomain_), target, intent(inout) :: femdomain
      real(real64), intent(in) :: Density, YoungModulus
      real(real64), optional, intent(in) :: PoissonRatio
      type(MPI_), target, optional, intent(in) :: mpid
      integer(int32), intent(in) :: DOF
      integer(int32) :: nn

      if (associated(this%mpid)) then
         nullify (this%mpid)
      end if

      if (present(mpid)) then
         this%mpid => mpid
      end if

      this%DOF = DOF
      this%femdomain%femdomainp => femdomain
      this%Density = Density*ones(femdomain%ne())
      this%YoungModulus = YoungModulus*ones(femdomain%ne())
      if (present(PoissonRatio)) then
         this%PoissonRatio = PoissonRatio*ones(femdomain%ne())
      end if

      this%u = zeros(femdomain%nn()*DOF)
      this%u_n = zeros(femdomain%nn()*DOF)
      this%v = zeros(femdomain%nn()*DOF)
      this%v_n = zeros(femdomain%nn()*DOF)
      this%v_n = zeros(femdomain%nn()*DOF)
      this%f_n = zeros(femdomain%nn()*DOF)

      if (DOF == 1) then

         call this%WK%init(FEMDomain=femdomain, &
                           YoungModulus=this%YoungModulus, &
                           Density=this%Density, &
                           DOF=DOF)
      else

         call this%WK%init(FEMDomain=femdomain, &
                           YoungModulus=this%YoungModulus, &
                           Density=this%Density, &
                           PoissonRatio=this%PoissonRatio, &
                           DOF=DOF)

      end if

      nn = this%femdomain%femdomainp%nn()

      this%initialized = .true.

   end subroutine initTSFEM
! ##############################################

! ##############################################
   recursive subroutine DirichletBoundaryTSFEM(this, NodeList, direction)
      class(TSFEM_), intent(inout) :: this
      integer(int32), intent(in) :: NodeList(:)
      integer(int32), allocatable :: LocalNodeList(:), NodeIDx(:)
      character(*), intent(in) :: direction
      integer(int32) :: DOF, i
      integer(int32), allocatable ::  idx(:)

      DOF = this%DOF

      if (associated(this%mpid)) then

         if (this%mpid%petot >= 2) then
            idx = this%femdomain%femdomainp%mpi_global_node_idx.cap.NodeList
            LocalNodeList = getIdx( &
                            vec=this%femdomain%femdomainp%mpi_global_node_idx, &
                            equal_to=idx)
            if (size(LocalNodeList) == 0) then
               return
            else
               NodeIDx = LocalNodeList
            end if
         else
            NodeIDx = NodeList
         end if
      else
         NodeIDx = NodeList
      end if

      if (.not. allocated(this%fix_idx)) then
         this%fix_idx = int(zeros(0))
      end if

      if (size(NodeIdx) == 0) then
         return
      end if

      if (index(direction, "x") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 1
      end if
      if (index(direction, "X") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 1
      end if
      if (index(direction, "1") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 1
      end if

      if (index(direction, "y") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 2
      end if
      if (index(direction, "Y") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 2
      end if
      if (index(direction, "2") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 2
      end if

      if (index(direction, "z") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 3
      end if
      if (index(direction, "Z") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 3
      end if
      if (index(direction, "3") /= 0) then
         this%fix_idx = this%fix_idx//(NodeIdx - 1)*DOF + 3
      end if

      call this%WK%OmegaSqMatrix%fix(idx=this%fix_idx)

   end subroutine DirichletBoundaryTSFEM
! ##############################################

! ##############################################
   recursive subroutine NeumannBoundaryTSFEM(this, NodeList, Force, direction, clear)
      class(TSFEM_), intent(inout) :: this
      integer(int32), intent(in) :: NodeList(:)
      character(*), intent(in) :: direction
      real(real64), intent(in) :: Force
      integer(int32) :: DOF, nn
      integer(int32), allocatable :: LocalNodeList(:), idx(:), NodeIdx(:)
      logical, optional, intent(in) :: clear

      nn = this%femdomain%femdomainp%nn()
      DOF = this%DOF

      if (.not. allocated(this%f_n)) then
         this%f_n = int(zeros(DOF*nn))
      end if

      if (associated(this%mpid)) then
         if (this%mpid%petot >= 2) then
            idx = this%femdomain%femdomainp%mpi_global_node_idx.cap.NodeList
            LocalNodeList = getIdx( &
                            vec=this%femdomain%femdomainp%mpi_global_node_idx, &
                            equal_to=idx)
            if (size(LocalNodeList) == 0) then
               return
            else
               NodeIdx = LocalNodeList
            end if
         else
            NodeIdx = NodeList
         end if
      else
         NodeIdx = NodeList
      end if

      if (present(clear)) then
         if (clear) then
            this%f_n(:) = 0.0d0
            return
         end if
      end if

      if (index(direction, "x") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 1) = Force
      end if
      if (index(direction, "X") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 1) = Force
      end if
      if (index(direction, "1") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 1) = Force
      end if

      if (index(direction, "y") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 2) = Force
      end if
      if (index(direction, "Y") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 2) = Force
      end if
      if (index(direction, "2") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 2) = Force
      end if

      if (index(direction, "z") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 3) = Force
      end if
      if (index(direction, "Z") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 3) = Force
      end if
      if (index(direction, "3") /= 0) then
         this%f_n((NodeIdx(:) - 1)*DOF + 3) = Force
      end if

   end subroutine NeumannBoundaryTSFEM
! ##############################################

! ##############################################
   recursive subroutine NeumannBoundary_multiple_ForceTSFEM(this, NodeList, Force, direction, clear)
      class(TSFEM_), intent(inout) :: this
      integer(int32), intent(in) :: NodeList(:)
      character(*), intent(in) :: direction
      real(real64), intent(in) :: Force(:)
      logical, optional, intent(in) :: clear
      real(real64) :: force_val
      integer(int32) :: i

      do i = 1, size(NodeList)
         force_val = Force(i)
         call this%NeumannBoundary(NodeList=NodeList(i:i), &
                                   Force=force_val, direction=direction, clear=clear)
      end do
   end subroutine NeumannBoundary_multiple_ForceTSFEM
! ##############################################

! ##############################################
   subroutine AbsorbingBoundaryTSFEM(this, NodeList, direction)
      class(TSFEM_), intent(inout) :: this
      integer(int32), intent(in) :: NodeList(:)
      character(*), intent(in) :: direction
      !real(real64),intent(in) :: beta
      integer(int32) :: DOF, nn
      real(real64) :: force(1:3)
      !>HPF
      real(real64) :: a, cutoff_frequency
      !complex(real64) :: beta_u, beta_v
      type(Math_) :: math

      ![[[CAUTION]]] EXPERIMENTAL, MANY CRITICAL BUGS EXIST.
      ![[[CAUTION]]] EXPERIMENTAL, MANY CRITICAL BUGS EXIST.
      ![[[CAUTION]]] EXPERIMENTAL, MANY CRITICAL BUGS EXIST.
      ![[[CAUTION]]] EXPERIMENTAL, MANY CRITICAL BUGS EXIST.
      ![[[CAUTION]]] EXPERIMENTAL, MANY CRITICAL BUGS EXIST.
      ![[[CAUTION]]] EXPERIMENTAL, MANY CRITICAL BUGS EXIST.

      nn = this%femdomain%femdomainp%nn()
      DOF = this%DOF

      ! HPF
      ! https://101010.fun/iot/rc-high-pass-filter.html
      cutoff_frequency = this%relative_cutoff_frequency*(1.0d0/this%dt)
      cutoff_frequency = cutoff_frequency*10.0d0
      a = 1.0d0/(2.0d0*math%pi*this%dt*cutoff_frequency + 1.0d0)

      if (.not. allocated(this%u_old)) then
         this%u_old = 0.0d0*this%u_n
      end if
      if (.not. allocated(this%v_old)) then
         this%v_old = 0.0d0*this%v_n
      end if

      if (.not. allocated(this%u_1)) then
         this%u_1 = 0.0d0*this%u_n
      end if
      if (.not. allocated(this%v_1)) then
         this%v_1 = 0.0d0*this%v_n
      end if
      if (.not. allocated(this%u_2)) then
         this%u_2 = 0.0d0*this%u_n
      end if
      if (.not. allocated(this%v_2)) then
         this%v_2 = 0.0d0*this%v_n
      end if
      ! caution
      ! 拡張性なし．1つのAB, 1方向しか対応できない．
      this%u_2((NodeList(:) - 1)*DOF + 1) = a*this%u_1((NodeList(:) - 1)*DOF + 1) &
                                            + a*(this%u_n((NodeList(:) - 1)*DOF + 1) - this%u_old((NodeList(:) - 1)*DOF + 1))
      this%u_1((NodeList(:) - 1)*DOF + 1) = this%u_2((NodeList(:) - 1)*DOF + 1)
      this%u_old((NodeList(:) - 1)*DOF + 1) = this%u_n((NodeList(:) - 1)*DOF + 1)
      this%u_n((NodeList(:) - 1)*DOF + 1) = this%u_2((NodeList(:) - 1)*DOF + 1)
      this%u((NodeList(:) - 1)*DOF + 1) = this%u_2((NodeList(:) - 1)*DOF + 1)

!    this%v_2((NodeList(:)-1)*DOF + 1) = a*this%v_1((NodeList(:)-1)*DOF + 1) &
!        + a*(this%v_n((NodeList(:)-1)*DOF + 1) - this%v_old((NodeList(:)-1)*DOF + 1) )
!    this%v_1   ((NodeList(:)-1)*DOF + 1) = this%v_2((NodeList(:)-1)*DOF + 1)
!    this%v_old ((NodeList(:)-1)*DOF + 1) = this%v_n((NodeList(:)-1)*DOF + 1)
!    this%v_n   ((NodeList(:)-1)*DOF + 1) = this%v_2((NodeList(:)-1)*DOF + 1)
!    this%v     ((NodeList(:)-1)*DOF + 1) = this%v_2((NodeList(:)-1)*DOF + 1)
!

      return

!    beta_u = sqrt(2.0d0)*cos(math%i*beta*(this%dt) + 0.250d0*math%pi   )
!    beta_v = sqrt(2.0d0)*cos(math%i*beta*(this%dt) - 0.250d0*math%pi   )

      if (index(direction, "x") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 1) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 1) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 1) = beta_v*this%v_n((NodeList(:)-1)*DOF + 1)
         !this%u((NodeList(:)-1)*DOF + 1) = beta_u*this%u((NodeList(:)-1)*DOF + 1)
         !this%v((NodeList(:)-1)*DOF + 1) = beta_v*this%v((NodeList(:)-1)*DOF + 1)
      end if
      if (index(direction, "X") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 1) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 1) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 1) = beta_v*this%v_n((NodeList(:)-1)*DOF + 1)
         !this%u((NodeList(:)-1)*DOF + 1) = beta_u*this%u((NodeList(:)-1)*DOF + 1)
         !this%v((NodeList(:)-1)*DOF + 1) = beta_v*this%v((NodeList(:)-1)*DOF + 1)
      end if
      if (index(direction, "1") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 1) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 1) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 1) = beta_v*this%v_n((NodeList(:)-1)*DOF + 1)
         !this%u((NodeList(:)-1)*DOF + 1) = beta_u*this%u((NodeList(:)-1)*DOF + 1)
         !this%v((NodeList(:)-1)*DOF + 1) = beta_v*this%v((NodeList(:)-1)*DOF + 1)
      end if

      if (index(direction, "y") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 2) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 2) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 2) = beta_v*this%v_n((NodeList(:)-1)*DOF + 2)
         !this%u((NodeList(:)-1)*DOF + 2) = beta_u*this%u((NodeList(:)-1)*DOF + 2)
         !this%v((NodeList(:)-1)*DOF + 2) = beta_v*this%v((NodeList(:)-1)*DOF + 2)
      end if
      if (index(direction, "Y") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 2) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 2) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 2) = beta_v*this%v_n((NodeList(:)-1)*DOF + 2)
         !this%u((NodeList(:)-1)*DOF + 2) = beta_u*this%u((NodeList(:)-1)*DOF + 2)
         !this%v((NodeList(:)-1)*DOF + 2) = beta_v*this%v((NodeList(:)-1)*DOF + 2)
      end if
      if (index(direction, "2") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 2) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 2) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 2) = beta_v*this%v_n((NodeList(:)-1)*DOF + 2)
         !this%u((NodeList(:)-1)*DOF + 2) = beta_u*this%u((NodeList(:)-1)*DOF + 2)
         !this%v((NodeList(:)-1)*DOF + 2) = beta_v*this%v((NodeList(:)-1)*DOF + 2)
      end if

      if (index(direction, "z") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 3) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 3) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 3) = beta_v*this%v_n((NodeList(:)-1)*DOF + 3)
         !this%u((NodeList(:)-1)*DOF + 3) = beta_u*this%u((NodeList(:)-1)*DOF + 3)
         !this%v((NodeList(:)-1)*DOF + 3) = beta_v*this%v((NodeList(:)-1)*DOF + 3)
      end if
      if (index(direction, "Z") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 3) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 3) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 3) = beta_v*this%v_n((NodeList(:)-1)*DOF + 3)
         !this%u((NodeList(:)-1)*DOF + 3) = beta_u*this%u((NodeList(:)-1)*DOF + 3)
         !this%v((NodeList(:)-1)*DOF + 3) = beta_v*this%v((NodeList(:)-1)*DOF + 3)
      end if
      if (index(direction, "3") /= 0) then
         !this%ab_weight_cos((NodeList(:)-1)*DOF + 3) = beta_u
         !this%ab_weight_sinc((NodeList(:)-1)*DOF + 3) = beta_v
         !this%v_n((NodeList(:)-1)*DOF + 3) = beta_v*this%v_n((NodeList(:)-1)*DOF + 3)
         !this%u((NodeList(:)-1)*DOF + 3) = beta_u*this%u((NodeList(:)-1)*DOF + 3)
         !this%v((NodeList(:)-1)*DOF + 3) = beta_v*this%v((NodeList(:)-1)*DOF + 3)
      end if

   end subroutine AbsorbingBoundaryTSFEM
! ##############################################

! ##############################################
   subroutine timeTSFEM(this, dt, t, timestep)
      class(TSFEM_), intent(inout) :: this
      real(real64), intent(in) :: dt, t
      integer(int32), intent(in) :: timestep

      this%dt = dt
      this%t = t
      this%timestep = timestep
      this%start_step = timestep + 1
   end subroutine
! ##############################################

! ##############################################
   subroutine TSFEM(this)
      class(TSFEM_), intent(inout) :: this
      real(real64) :: cutoff_frequency
      real(real64), allocatable :: f_v(:), f_u(:)

      this%WK%itrmax = this%itrmax
      this%WK%tol = this%tol
      cutoff_frequency = this%relative_cutoff_frequency*(1.0d0/this%dt)

      if (associated(this%mpid)) then
         if (this%mpid%petot >= 2) then
            call this%WK%getDisplacement_and_Velocity( &
               u_n=this%u_n, v_n=this%v_n, dt=this%dt, &
               fix_idx=this%fix_idx, cutoff_frequency=cutoff_frequency, &
               u=this%u, v=this%v, RHS=this%f_n, MPID=this%MPID, FEMDomain=this%femdomain%femdomainp)
         else
            call this%WK%getDisplacement_and_Velocity( &
               u_n=this%u_n, v_n=this%v_n, dt=this%dt, &
               fix_idx=this%fix_idx, cutoff_frequency=cutoff_frequency, &
               u=this%u, v=this%v, RHS=this%f_n)
         end if
      else
         call this%WK%getDisplacement_and_Velocity( &
            u_n=this%u_n, v_n=this%v_n, dt=this%dt, &
            fix_idx=this%fix_idx, cutoff_frequency=cutoff_frequency, &
            u=this%u, v=this%v, RHS=this%f_n)
      end if

      if (allocated(this%fix_idx)) then
         f_v = this%WK%Mmatrix_diag*this%v_n
         f_u = this%WK%OmegaSqMatrix%matmul(this%u_n)
         this%f_n(this%fix_idx) = f_v(this%fix_idx) !+ f_u(this%fix_idx)
      end if
      if (associated(this%mpid)) then
         call this%mpid%barrier()
      end if
      this%u_n = this%u
      this%v_n = this%v

      this%t = this%t + this%dt
      this%timestep = this%timestep + 1

!    if(associated(this%mpid) )then
!        call this%mpid%barrier()
!    endif

   end subroutine
! ##############################################

! ##############################################
   subroutine saveTSFEM(this, name)
      class(TSFEM_), intent(inout) :: this
      type(IO_) :: f
      character(*), intent(in) :: name
      real(real64), allocatable :: u_xyz(:, :), v_xyz(:, :)

      u_xyz = transpose(reshape(dble(this%u), [this%DOF, this%femdomain%femdomainp%nn()]))
      call this%femdomain%femdomainp%vtk(name=name + "_ux_"+zfill(this%timestep, 6) + ".vtk", &
                                         scalar=u_xyz(:, 1))
      call this%femdomain%femdomainp%vtk(name=name + "_uy_"+zfill(this%timestep, 6) + ".vtk", &
                                         scalar=u_xyz(:, 2))
      call this%femdomain%femdomainp%vtk(name=name + "_uz_"+zfill(this%timestep, 6) + ".vtk", &
                                         scalar=u_xyz(:, 3))
      deallocate (u_xyz)

      v_xyz = transpose(reshape(dble(this%u), [this%DOF, this%femdomain%femdomainp%nn()]))
      call this%femdomain%femdomainp%vtk(name=name + "_vx_"+zfill(this%timestep, 6) + ".vtk", &
                                         scalar=v_xyz(:, 1))
      call this%femdomain%femdomainp%vtk(name=name + "_vy_"+zfill(this%timestep, 6) + ".vtk", &
                                         scalar=v_xyz(:, 2))
      call this%femdomain%femdomainp%vtk(name=name + "_vz_"+zfill(this%timestep, 6) + ".vtk", &
                                         scalar=v_xyz(:, 3))
      deallocate (v_xyz)

   end subroutine
! ##############################################

! ##############################################
   subroutine movieTSFEM(this, name)
      class(TSFEM_), intent(inout) :: this
      type(IO_) :: f
      character(*), intent(in) :: name
      real(real64), allocatable :: u_xyz(:, :)

      call f%open(name + "_v.gp", "w")
      call f%write('set term gif enhanced animate optimize size 700, 480')
      call f%write('set output "'+name + '_v.gif"')
      call f%write('do for[i='+str(this%start_step) + ':'+str(this%timestep) + ':1]{')
      call f%write("filename = sprintf('"+name + "_v0%05d.tsv', i)")
      call f%write("dt="+str(this%dt))
      call f%write("plot_title = sprintf('time = %f sec.', i*dt)")
      call f%write('set title plot_title')
      call f%write("plot filename u 1:2 w l")
      call f%write('}')
      call f%write('set out')
      call f%write('reset')
      call f%close()

      call f%open(name + "_u.gp", "w")
      call f%write('set term gif enhanced animate optimize size 700, 480')
      call f%write('set output "'+name + '_u.gif"')
      call f%write('do for[i='+str(this%start_step) + ':'+str(this%timestep) + ':1]{')
      call f%write("filename = sprintf('"+name + "_u0%05d.tsv', i)")
      call f%write("dt="+str(this%dt))
      call f%write("plot_title = sprintf('time = %f sec.', i*dt)")
      call f%write('set title plot_title')
      call f%write("plot filename u 1:2 w l")
      call f%write('}')
      call f%write('set out')
      call f%write('reset')
      call f%close()

   end subroutine
! ##############################################

end module TSFEMClass
