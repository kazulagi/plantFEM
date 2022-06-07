module ModalAnalysisClass
    use FEMSolverClass
    implicit none

    type :: ModalAnalysis_
        type(FEMSolver_) :: solver
        integer(int32),allocatable :: DomainNodeID(:,:)
        integer(int32),allocatable :: DomainElemID(:,:)
        real(real64),allocatable   :: YoungModulus(:)
        real(real64),allocatable   :: PoissonRatio(:)
        real(real64),allocatable   :: Density(:)

        real(real64),allocatable   :: EigenFrequency(:)
        real(real64),allocatable   :: EigenMode(:,:)
    contains
        procedure ::  init => initModalAnalysis 
        procedure ::  setMaterial => setMaterialModalAnalysis
        procedure ::  setBoundary => setBoundaryModalAnalysis
        procedure ::  solve => solveModalAnalysis
        procedure ::  vtk => vtkModalAnalysis
        procedure ::  remove => removeModalAnalysis
        procedure ::  destroy => removeModalAnalysis
    end type

contains

subroutine initModalAnalysis(this,domains) 
    class(ModalAnalysis_),intent(inout) :: this
    type(FEMDomain_),intent(in) :: domains(:)
    integer(int32),allocatable :: domainIDs(:)
    integer(int32) :: n,nn,ne,i
    
    n = size(domains)

    allocate(domainIDs(n) )
    !$OMP parallel do
    do i=1,n
        domainIDs(i) = i
    enddo
    !$OMP end parallel do

    call this%solver%init(NumDomain=n )
    call this%solver%setDomain(FEMDomains=domains(:),DomainIDs=DomainIDs)
    call this%solver%setCRS(DOF=domains(1)%nd() )
    
    this%DomainElemID = zeros( n,2 )
    this%DomainNodeID = zeros( n,2 )
    ne = 0
    do i=1,size(Domains)
        ne = ne + domains(i)%ne()
        if(i==1)then
            this%DomainElemID(i,1) = 1
            this%DomainElemID(i,2) = domains(i)%ne()

            this%DomainNodeID(i,1) = 1
            this%DomainNodeID(i,2) = domains(i)%nn()
        else
            this%DomainElemID(i,1) = this%DomainElemID(i-1,2) + 1
            this%DomainElemID(i,2) = this%DomainElemID(i-1,2) + domains(i)%ne()
            this%DomainNodeID(i,1) = this%DomainNodeID(i-1,2) + 1
            this%DomainNodeID(i,2) = this%DomainNodeID(i-1,2) + domains(i)%nn()
        endif
    enddo
    this%YoungModulus = zeros(ne)
    this%PoissonRatio = zeros(ne)
    this%Density      = zeros(ne)

end subroutine
! ###########################################################
subroutine setMaterialModalAnalysis(this,DomainID,Density,YoungModulus,PoissonRatio)
    class(ModalAnalysis_),intent(inout) :: this
    integer(int32),intent(in) :: DomainID
    real(real64),intent(in) :: Density(:),YoungModulus(:),PoissonRatio(:)

    if(size(Density)==1 )then
        this%Density(  this%DomainElemID(DomainID,1): this%DomainElemID(DomainID,2) ) = Density(1)
    else
        this%Density(  this%DomainElemID(DomainID,1): this%DomainElemID(DomainID,2) ) = Density(:)
    endif


    if(size(YoungModulus)==1 )then
        this%YoungModulus(  this%DomainElemID(DomainID,1): this%DomainElemID(DomainID,2) ) = YoungModulus(1)
    else
        this%YoungModulus(  this%DomainElemID(DomainID,1): this%DomainElemID(DomainID,2) ) = YoungModulus(:)
    endif

    if(size(PoissonRatio)==1 )then
        this%PoissonRatio(  this%DomainElemID(DomainID,1): this%DomainElemID(DomainID,2) ) = PoissonRatio(1)
    else
        this%PoissonRatio(  this%DomainElemID(DomainID,1): this%DomainElemID(DomainID,2) ) = PoissonRatio(:)
    endif
end subroutine
! ######################################################################

subroutine setBoundaryModalAnalysis(this,DomainID,NodeList)
    class(ModalAnalysis_),intent(inout) :: this
    integer(int32),intent(in) :: DomainID,NodeList(:)
    integer(int32),allocatable :: node_list_x(:)
    integer(int32),allocatable :: node_list_y(:)
    integer(int32),allocatable :: node_list_z(:)
    integer(int32),allocatable :: node_list(:)


    node_list = NodeList 
    node_list(:) = node_list(:) + this%DomainNodeID(DomainID,1) - 1
    node_list_x =(node_list(:)-1)*3+1
    call this%solver%fix_eig(IDs=node_list_x)
    node_list_y =(node_list(:)-1)*3+2
    call this%solver%fix_eig(IDs=node_list_y)
    node_list_z =(node_list(:)-1)*3+3
    call this%solver%fix_eig(IDs=node_list_z)

end subroutine

subroutine solveModalAnalysis(this,penalty,only_matrix)
    class(ModalAnalysis_),intent(inout) :: this
    real(real64),optional,intent(in) :: penalty
    logical,optional,intent(in) :: only_matrix
    integer(int32) :: i,DomainID,offset


    ! create matrices
    call this%solver%zeros()
    
    do DomainID=1,size(this%solver%femdomains )

        !$OMP parallel do private(offset)
        do i=1,this%solver%femdomains(DomainID)%femdomainp%ne()
            offset = this%DomainElemID(DomainID,1) - 1
            call this%solver%setMatrix(&
                DomainID=DomainID,&
                ElementID=i,&
                DOF=this%solver%femdomains(DomainID)%femdomainp%nd(),&
                Matrix=this%solver%femdomains(DomainID)%femdomainp%MassMatrix(ElementID=i,&
                    Density=this%density(i+offset),&
                DOF=this%solver%femdomains(DomainID)%femdomainp%nd()  ) &
                )
        enddo
        !$OMP end parallel do 
    enddo
    
    
    call this%solver%keepThisMatrixAs("B")
    call this%solver%zeros()

    print *, "Save Stiffness Matrix"
    
    do DomainID=1,size(this%solver%femdomains )
        !$OMP parallel do private(offset)
        do i=1,this%solver%femdomains(DomainID)%femdomainp%ne()
            offset = this%DomainElemID(DomainID,1) - 1
            call this%solver%setMatrix(&
                DomainID=DomainID,&
                ElementID=i,&
                DOF=this%solver%femdomains(DomainID)%femdomainp%nd(),&
                Matrix=this%solver%femdomains(DomainID)%femdomainp%StiffnessMatrix(ElementID=i  ,&
                E=this%YoungModulus(i+offset),&
                v=this%PoissonRatio(i+offset)  ) &
                )
        enddo
        !$OMP end parallel do 
    enddo
    
    print *, "[ok]Element-matrices done"
    if(size(this%solver%femdomains)/=1 )then
        if(.not.present(penalty) )then
            print *, "ERROR >> Multi-domain mode needs argument "
            print *, "Real(real64) :: penalty"
            stop
        endif
        call this%solver%setEbOM(penalty=penalty, DOF=this%solver%femdomains(1)%femdomainp%nd()) 
    endif
    
    call this%solver%keepThisMatrixAs("A")
    
    if(present(only_matrix) )then
        if(only_matrix)then
        return
        endif
    endif

    call this%solver%eig(eigen_value=this%EigenFrequency,eigen_vectors=this%EigenMode)
    

    ! read results
    this%EigenFrequency = sqrt(abs(this%EigenFrequency))/2.0d0/3.141590d0



end subroutine

! #################################################################
subroutine vtkModalAnalysis(this,name,num_mode,amp,stress_scale)
    class(ModalAnalysis_),intent(in) :: this
    integer(int32),intent(in) :: num_mode
    character(*),intent(in) :: name
    real(real64),intent(in) :: amp
    real(real64),optional,intent(in) :: stress_scale
    
    type(IO_) :: f
    real(real64) :: dt,t,st_scale
    real(real64),allocatable :: Mode_U(:),mode_U_total (:),&
        mode_Ut(:),YoungModulus(:),PoissonRatio(:)
    integer(int32) :: mode_id,step,DOF,DomainID,ElementID,i,j,offset
    
    st_scale = input(default=1.0d0,option=stress_scale)
    dt = 0.10d0
    DOF = this%solver%femdomains(1)%femdomainp%nd()
    ! num_mode modes
    do mode_id=1,num_mode
        ! get 
        mode_U_total = zeros(size(this%EigenMode,1))
        mode_U_total = this%EigenMode(:,mode_id)
        ! 

        do DomainID=1,size(this%solver%femdomains)

            mode_U = mode_U_total( DOF*(this%DomainNodeID(DomainID,1)-1)+1 : &
                DOF*this%DomainNodeID(DomainID,2)  )
            YoungModulus = this%YoungModulus( this%DomainElemID(DomainID,1) : &
                this%DomainElemID(DomainID,2)  )
            PoissonRatio = this%PoissonRatio( this%DomainElemID(DomainID,1) : &
                this%DomainElemID(DomainID,2)  )

            dt = 1.0d0/this%EigenFrequency(mode_id)/100.0d0
            do step=1,100
                t = dt * dble(step-1)
                mode_Ut = mode_U*cos( 2.0d0*3.140d0*this%EigenFrequency(mode_id)*t )

                this%solver%femdomains(DomainID)%femdomainp%mesh%nodcoord =&
                 this%solver%femdomains(DomainID)%femdomainp%mesh%nodcoord &
                +amp*reshape(mode_Ut,this%solver%femdomains(DomainID)%femdomainp%nn(),3 ) 

                call this%solver%femdomains(DomainID)%femdomainp%vtk&
                (name+"_Mode_"+str(mode_id)+"_Domain_"+str(DomainID)+"_I1_t_"+str(step),&
                scalar = 1.0d0/st_scale*this%solver%femdomains(DomainID)%femdomainp%getElementCauchyStress(&
                    option="I1", displacement=mode_Ut,E=YoungModulus,v=PoissonRatio &
                    ) )
                
                this%solver%femdomains(DomainID)%femdomainp%mesh%nodcoord =&
                 this%solver%femdomains(DomainID)%femdomainp%mesh%nodcoord &
                -amp*reshape(mode_Ut,this%solver%femdomains(DomainID)%femdomainp%nn(),3 ) 
            enddo
        enddo
    enddo
   
    print *, "Freq (Hz)"
    call print(this%EigenFrequency(1:num_mode) )

    call f%open(name + "_Freq.txt")
    write(f%fh,*) "# Freq (Hz)"
    call f%write(this%EigenFrequency(1:num_mode) )
    call f%close()

end subroutine
!###################################################
subroutine removeModalAnalysis(this)
    class(ModalAnalysis_),intent(inout) :: this

    call this%solver%remove()
    if(allocated(this%DomainNodeID) ) deallocate(this%DomainNodeID) !(:,:)
    if(allocated(this%DomainElemID) ) deallocate(this%DomainElemID) !(:,:)
    if(allocated(this%YoungModulus) ) deallocate(this%YoungModulus) !(:)
    if(allocated(this%PoissonRatio) ) deallocate(this%PoissonRatio) !(:)
    if(allocated(this%Density) ) deallocate(this%Density) !(:)

    if(allocated(this%EigenFrequency) ) deallocate(this%EigenFrequency)! (:)
    if(allocated(this%EigenMode) ) deallocate(this%EigenMode)! (:,:)

end subroutine

end module
