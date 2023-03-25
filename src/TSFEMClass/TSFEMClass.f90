module TSFEMClass
    use WaveKernelClass
    use ArrayClass
    implicit none

    type :: TSFEM_
        type(WaveKernel_) :: WK
        type(FEMDomainp_) :: femdomain
        
        ! >>> boundary condition
        integer(int32),allocatable :: fix_idx(:)
        real(real64),allocatable :: f_n(:)
        ! <<< boundary condition

        ! >>> solutions >>>
        real(real64),allocatable :: u(:),v(:)
        real(real64),allocatable :: u_n(:),v_n(:)
        ! <<< solutions <<<

        ! >>> material parameters >>>
        real(real64),allocatable :: YoungModulus(:)
        real(real64),allocatable :: PoissonRatio(:)
        real(real64),allocatable :: Density(:)
        ! <<< material parameters <<<

        real(real64) :: relative_cutoff_frequency = 1.0d0
        real(real64) :: dt=1.0d0/100.0d0
        real(real64) :: t=0.0d0
        integer(int32) :: timestep = 0
        integer(int32) :: start_step=1
        integer(int32) :: interval=0

        integer(int32) :: itrmax = 30
        integer(int32) :: DOF
        real(real64)   :: tol    = dble(1.0e-25)

        logical :: initialized = .false.
    contains
        procedure,public :: init => initTSFEM
        procedure,public :: DirichletBoundary => DirichletBoundaryTSFEM
        procedure,public :: NeumannBoundary   => NeumannBoundaryTSFEM
        procedure,public :: AbsorbingBoundary   => AbsorbingBoundaryTSFEM
        procedure,public :: time   => timeTSFEM
        procedure,public :: update => updateTSFEM
        procedure,public :: save => saveTSFEM
        procedure,public :: movie => movieTSFEM
        
    end type
contains
! ##############################################
subroutine initTSFEM(this,femdomain,Density,YoungModulus,PoissonRatio,DOF)
    class(TSFEM_),intent(inout) :: this
    type(FEMDomain_),target,intent(inout) :: femdomain
    real(real64),intent(in) :: Density, YoungModulus
    real(real64),optional,intent(in) :: PoissonRatio
    integer(int32),intent(in) :: DOF

    this%DOF = DOF
    this%femdomain%femdomainp => femdomain
    this%Density      = Density     *ones(femdomain%ne() )
    this%YoungModulus = YoungModulus*ones(femdomain%ne() )
    if(present(PoissonRatio) )then
        this%PoissonRatio = PoissonRatio*ones(femdomain%ne() )
    endif
    

    this%u   = zeros( femdomain%nn()*DOF )
    this%u_n = zeros( femdomain%nn()*DOF )
    this%v   = zeros( femdomain%nn()*DOF )
    this%v_n = zeros( femdomain%nn()*DOF )
    this%v_n = zeros( femdomain%nn()*DOF )


    if(DOF==1)then
        call this%WK%init(FEMDomain=femdomain,&
            YoungModulus=this%YoungModulus,&
            Density=this%Density,&
            DOF=DOF)
    else
        call this%WK%init(FEMDomain=femdomain,&
            YoungModulus=this%YoungModulus,&
            Density=this%Density,&
            PoissonRatio=this%PoissonRatio,&
            DOF=DOF)
    endif

    

    this%initialized = .true.

end subroutine initTSFEM
! ##############################################



! ##############################################
subroutine DirichletBoundaryTSFEM(this,NodeList,direction)
    class(TSFEM_),intent(inout) :: this
    integer(int32),intent(in) :: NodeList(:)
    character(*),intent(in) :: direction
    integer(int32) :: DOF

    DOF = this%DOF

    if(.not.allocated(this%fix_idx) )then
        this%fix_idx = int(zeros(0))
    endif

    if(index(direction,"x")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 1
    endif
    if(index(direction,"X")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 1
    endif
    if(index(direction,"1")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 1
    endif


    if(index(direction,"y")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 2
    endif
    if(index(direction,"Y")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 2
    endif
    if(index(direction,"2")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 2
    endif

    if(index(direction,"z")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 3
    endif
    if(index(direction,"Z")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 3
    endif
    if(index(direction,"3")/=0 )then
        this%fix_idx = this%fix_idx // (NodeList-1)*DOF + 3
    endif
end subroutine DirichletBoundaryTSFEM
! ##############################################




! ##############################################
subroutine NeumannBoundaryTSFEM(this,NodeList,Force,direction,clear)
    class(TSFEM_),intent(inout) :: this
    integer(int32),intent(in) :: NodeList(:)
    character(*),intent(in) :: direction
    real(real64),intent(in) :: Force
    integer(int32) :: DOF,nn
    logical,optional,intent(in) :: clear


    nn  = this%femdomain%femdomainp%nn()
    DOF = this%DOF

    if(.not.allocated(this%f_n) )then
        this%f_n = int(zeros( DOF*nn ))
    endif

    if(present(clear) )then
        this%f_n(:) = 0.0d0    
        return
    endif



    if(index(direction,"x")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 1) = Force
    endif
    if(index(direction,"X")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 1) = Force
    endif
    if(index(direction,"1")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 1) = Force
    endif


    if(index(direction,"y")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 2) = Force
    endif
    if(index(direction,"Y")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 2) = Force
    endif
    if(index(direction,"2")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 2) = Force
    endif

    if(index(direction,"z")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 3) = Force
    endif
    if(index(direction,"Z")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 3) = Force
    endif
    if(index(direction,"3")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 3) = Force
    endif
    
end subroutine NeumannBoundaryTSFEM
! ##############################################


! ##############################################
subroutine AbsorbingBoundaryTSFEM(this,NodeList,direction,damper,spring)
    class(TSFEM_),intent(inout) :: this
    integer(int32),intent(in) :: NodeList(:)
    character(*),intent(in) :: direction
    real(real64),intent(in) :: damper,spring
    integer(int32) :: DOF,nn
    real(real64) :: force(1:3)
    

    nn  = this%femdomain%femdomainp%nn()
    DOF = this%DOF

    if(.not.allocated(this%f_n) )then
        this%f_n = int(zeros( DOF*nn ))
    endif




    if(index(direction,"x")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 1) = -spring*this%u_n((NodeList(:)-1)*DOF + 1)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 1)  
    endif
    if(index(direction,"X")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 1) = -spring*this%u_n((NodeList(:)-1)*DOF + 1)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 1)  
    endif
    if(index(direction,"1")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 1) = -spring*this%u_n((NodeList(:)-1)*DOF + 1)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 1)  
    endif


    if(index(direction,"y")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 2) = -spring*this%u_n((NodeList(:)-1)*DOF + 2)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 2)  
    endif
    if(index(direction,"Y")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 2) = -spring*this%u_n((NodeList(:)-1)*DOF + 2)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 2)  
    endif
    if(index(direction,"2")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 2) = -spring*this%u_n((NodeList(:)-1)*DOF + 2)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 2)  
    endif

    if(index(direction,"z")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 3) = -spring*this%u_n((NodeList(:)-1)*DOF + 3)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 3)  
    endif
    if(index(direction,"Z")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 3) = -spring*this%u_n((NodeList(:)-1)*DOF + 3)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 3)  
    endif
    if(index(direction,"3")/=0 )then
        this%f_n((NodeList(:)-1)*DOF + 3) = -spring*this%u_n((NodeList(:)-1)*DOF + 3)  &
            -damper*this%v_n((NodeList(:)-1)*DOF + 3)  
    endif
    
end subroutine AbsorbingBoundaryTSFEM
! ##############################################



! ##############################################
subroutine timeTSFEM(this,dt,t,timestep)
    class(TSFEM_),intent(inout) :: this
    real(real64),intent(in) :: dt,t
    integer(int32),intent(in) :: timestep

    this%dt = dt
    this%t  =  t
    this%timestep = timestep
    this%start_step = timestep+1
end subroutine
! ##############################################

! ##############################################
subroutine updateTSFEM(this)
    class(TSFEM_),intent(inout) :: this
    real(real64) :: cutoff_frequency

    this%WK%itrmax = this%itrmax
    this%WK%tol = this%tol
    cutoff_frequency  = this%relative_cutoff_frequency*(1.0d0/this%dt)

    if(allocated(this%fix_idx) )then
        this%u_n(this%fix_idx)=0.0d0
    endif
    call this%WK%getDisplacement_and_Velocity(&
        u_n=this%u_n,v_n=this%v_n,dt=this%dt,&
        fix_idx=this%fix_idx,cutoff_frequency=cutoff_frequency,&
        u=this%u,v=this%v,RHS=this%f_n)
!    this%u =   this%WK%getDisplacement(u_n=this%u_n, v_n=this%v_n,RHS=this%f_n,&
!        dt=this%dt,fix_idx=this%fix_idx,cutoff_frequency=cutoff_frequency)
!
!    this%v =   this%WK%getVelocity(u_n=this%u_n, v_n=this%v_n,RHS=this%f_n,&
!        dt=this%dt,fix_idx=this%fix_idx,cutoff_frequency=cutoff_frequency)

    if(allocated(this%fix_idx) )then
        this%u(this%fix_idx)=0.0d0
    endif
    this%u_n = this%u
    this%v_n = this%v

    this%t = this%t + this%dt
    this%timestep = this%timestep + 1

end subroutine
! ##############################################



! ##############################################
subroutine saveTSFEM(this,name)
    class(TSFEM_),intent(inout) :: this
    type(IO_) :: f
    character(*),intent(in) :: name
    real(real64),allocatable :: u_xyz(:,:)

    u_xyz = transpose(reshape(dble(this%v),[this%DOF,this%femdomain%femdomainp%nn() ]))
    call f%open(name+"_v"+zfill(this%timestep,6) +".tsv","w")
    call f%write( this%femdomain%femdomainp%mesh%nodcoord(:,1:this%DOF)&
         .h. u_xyz(:,1:this%DOF ) )
    call f%close()
    u_xyz = transpose(reshape(dble(this%u),[this%DOF,this%femdomain%femdomainp%nn() ]))
    call f%open(name+"_u"+zfill(this%timestep,6) +".tsv","w")
    call f%write( this%femdomain%femdomainp%mesh%nodcoord(:,1:this%DOF) &
        .h. u_xyz(:,1:this%DOF) )
    call f%close()

end subroutine
! ##############################################



! ##############################################
subroutine movieTSFEM(this,name)
    class(TSFEM_),intent(inout) :: this
    type(IO_) :: f
    character(*),intent(in) :: name
    real(real64),allocatable :: u_xyz(:,:)

    call f%open(name+"_v.gp","w")
    call f%write('set term gif enhanced animate optimize size 700, 480')
    call f%write('set output "'+name+'_v.gif"')
    call f%write('do for[i='+str(this%start_step)+':'+str(this%timestep)+':1]{')
    call f%write("filename = sprintf('"+name+"_v0%05d.tsv', i)")
    call f%write("dt="+str(this%dt) )
    call f%write("plot_title = sprintf('time = %f sec.', i*dt)")
    call f%write('set title plot_title')
    call f%write("plot filename u 1:2 w l")
    call f%write('}')
    call f%write('set out')
    call f%write('reset')
    call f%close()

    call f%open(name+"_u.gp","w")
    call f%write('set term gif enhanced animate optimize size 700, 480')
    call f%write('set output "'+name+'_u.gif"')
    call f%write('do for[i='+str(this%start_step)+':'+str(this%timestep)+':1]{')
    call f%write("filename = sprintf('"+name+"_u0%05d.tsv', i)")
    call f%write("dt="+str(this%dt) )
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