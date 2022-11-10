module FEMSolverClass
    !Linear soler for FEMDomainClass
    use iso_fortran_env
    use SparseClass
    use FEMDomainClass
    implicit none

    type :: M_Link_Item_
        integer(int32) :: rank_and_rowID_1(1:2) = [0,0]
        integer(int32) :: rank_and_rowID_2(1:2) = [0,0]
    end type


    type :: FEMSolver_
        type(FEMDomainp_),allocatable :: femdomains(:)  
        integer(int32),allocatable :: DomainIDs(:)

        real(real64),allocatable :: IfaceElemConnectivity(:,:)
        real(real64),allocatable :: IfaceElemDomainID(:,:)

        logical :: initialized = .false.
        logical :: InterfaceExist = .false.
        logical :: debug = .false.
        
        real(real64),allocatable :: CRS_val(:)
        integer(int32),allocatable :: CRS_Index_Col(:)
        integer(int32),allocatable :: CRS_Index_Row(:)
        real(real64),allocatable :: CRS_RHS(:)
        
        !> General Eigen Value Problem
        !> [A]{x} = (lambda)[B]{x}

        real(real64),allocatable   :: A_CRS_val(:)
        integer(int32),allocatable :: A_CRS_Index_Col(:)
        integer(int32),allocatable :: A_CRS_Index_Row(:)
        logical                    :: A_empty = .true.

        real(real64),allocatable   :: B_CRS_val(:)
        integer(int32),allocatable :: B_CRS_Index_Col(:)
        integer(int32),allocatable :: B_CRS_Index_Row(:)
        logical                    :: B_empty = .true.

        
        integer(int32),allocatable :: fix_eig_IDs(:)
        
        logical,allocatable :: fix_lin_exists(:)
        real(real64),allocatable :: fix_lin_exists_Values(:)
        
        !integer(int32),allocatable :: fix_lin_IDs(:)
        !real(real64),allocatable :: fix_lin_Values(:)
        
        real(real64),allocatable :: CRS_x(:)
        integer(int32),allocatable :: CRS_ID_Starts_From(:)

        ! dense matrix
        real(real64),allocatable :: A_dense(:,:)
        integer(int32),allocatable :: Num_nodes_in_Domains(:)

        ! with MPI
        type(MPI_),pointer :: MPI_target  => null()
        type(M_Link_Item_),allocatable :: Link_Table(:)

        integer(int32) :: LINK_TABLE_INIT_SIZE = 1000
        integer(int32) :: Link_num=0
        
        type(CRS_) :: ILU_MATRIX

        integer(int32) :: itrmax = 100000
        real(real64)   :: er0 = dble(1.0e-10)
        real(real64)   :: relative_er = dble(1.0e-10)

        logical        :: use_LOBPCG     = .true.
        integer(int32) :: LOBPCG_MAX_ITR = 100000
        real(real64)   :: LOBPCG_TOL     = dble(1.0e-6)
        integer(int32) :: LOBPCG_NUM_MODE= 5
    contains
        !(1) Initialize solver
        procedure,public ::  init => initFEMSolver

        !(2) set Domain info
        procedure,public ::  setDomain => setDomainFEMSolver
        procedure,public ::  setDomains=> setDomainFEMSolver

        !(3) setup Ax=b as CRS format
        procedure,pass :: setCRS_CRSobjFEMSolver
        procedure,pass :: setCRSFEMSolver
        procedure,public :: setRHS => setRHSFEMSolver
        generic ::  setCRS  => setCRSFEMSolver,setCRS_CRSobjFEMSolver
        procedure,public ::  getCRS  => getCRSFEMSolver

        !(4) set Ax=b as CRS format
        procedure,public ::  setValue  => setValueFEMSolver
        procedure,public ::  setValues => setValueFEMSolver
        procedure,public :: addMatrixValue => addMatrixValueFEMSolver

        procedure,public ::  setMatrix  => setMatrixFEMSolver
        procedure,public ::  setVector  => setVectorFEMSolver

        procedure,public ::  keepThisMatrixAs => keepThisMatrixAsFEMSolver

        !(5) fix x=\bar{x} (not implemented yet.)
        ! for Linear Solver
        procedure,public :: fix => fixFEMSolver

        !(5) fix x=\bar{x} (not implemented yet.)
        ! for eigen solver
        procedure,public :: fix_eig => fix_eigFEMSolver
        
        !(6) save matrix
        procedure,public :: saveMatrix => saveMatrixFEMSolver
        procedure,public :: loadMatrix => loadMatrixFEMSolver


        
        
        !(7-1) Modal analysis
        procedure,public :: eig => eigFEMSolver

        !(7-2) linear solver
        procedure,pass :: solveFEMSolver
        procedure,pass :: solveFEMSolver_UserDefinedLinearSolver
        procedure,pass :: solveFEMSolver_UserDefinedLinearSolverAsFunc
        generic :: solve => solveFEMSolver,solveFEMSolver_UserDefinedLinearSolver,&
            solveFEMSolver_UserDefinedLinearSolverAsFunc

        !(7-3) condition number
        procedure,public :: conditionNumber => conditionNumberFEMSolver

        !re-zero matrix
        procedure,public :: zeros => zerosFEMSolver
        ! M:diag matrix,  A*M^{-1}
        procedure,public :: matmulDiagMatrix => matmulDiagMatrixFEMSolver
        procedure,public :: diag =>diagFEMSolver
        
        ! others:
        ! Energy-based Overset Mesh (Tomobe et al., under review)
        procedure, public :: setEbOM => setEbOMFEMSolver

        ! FOR MPI 
        procedure, public :: MPI_link => MPI_linkFEMSolver
        procedure, public :: MPI_dot_product => MPI_dot_productFEMSolver
        procedure, public :: MPI_matmul => MPI_matmulFEMSolver
        procedure, public :: MPI_BICGSTAB => MPI_BICGSTABFEMSolver
        

        ! destractor
        procedure, public :: remove => removeFEMSolver

    end type

    interface reverseArray
        module procedure reverseArrayReal64
    end interface reverseArray
    interface reverseVector
        module procedure reverseVectorReal64
    end interface reverseVector
contains

recursive subroutine initFEMSolver(this,NumDomain,FEMDomains,DomainIDs,DOF,MPI_target)
    class(FEMSolver_),intent(inout) :: this
    ! two-way
    integer(int32),optional,intent(in) :: numDomain
    ! optional
    ! you can bypass solver%setDomain and solver%setCRS
    type(FEMDomain_),optional,intent(in) :: FEMDomains(:)
    integer(int32),optional,intent(in) :: DomainIDs(:), DOF
    

    ! useless
    type(MPI_),optional,target,intent(in) :: MPI_target
    integer(int32) :: i
    integeR(int32),allocatable :: default_DomainIDs(:)

    if(present(FEMDomains) )then
        if(present(DomainIDs) .and. present(DOF) )then
            ! bypass mode
            call this%init(NumDomain=size(FEMDomains) )
            call this%setDomain(FEMDomains=FEMDomains(:),DomainIDs=DomainIDs)
            call this%setCRS(DOF=DOF)
            return
        elseif( present(DOF) )then
            default_DomainIDs = zeros(size(FEMDomains) )
            do i=1,size(FEMDomains)
                default_DomainIDs(i) = i
            enddo

            call this%init(NumDomain=size(FEMDomains) )
            call this%setDomain(FEMDomains=FEMDomains(:),DomainIDs=default_DomainIDs)
            call this%setCRS(DOF=DOF)
            return
        else
            print *, "ERROR :: initFEMSolver >> "
            print *, "You are trying to use ByPass-mode,"
            print *, "which requires at least following two arguments"
            print *, "(1) type(FEMDomain_) :: FEMDomains(:)  "
            print *, "(2) Integer(int32)   :: DOF <DEGREE OF FREEDOM> " 
            print *, " "
            print *, "and, if you give original domain-ids,"
            print *, "(2) Integer(int32)   :: DomainIDs(:) "
            print *, "is also necessary."
            stop 
        endif
    endif


    if(.not. present(numDomain) )then
        print *, "ERROR :: initFEMSolver >> "
        print *, "Please input "
        print *, "integer(int32) :: numDomain "
        print *, "or  "
        print *, "type(FEMDomains_) :: femdomains(:) "
        print *, "integer(int32) :: DOF <DEGREE OF FREEDOM> "
        stop
    endif

    nullify(this%MPI_target)


    if(present(MPI_target) )then
        this%MPI_target => MPI_target
    endif


    this%initialized = .false.
    if(allocated(this%femdomains)) deallocate(this%femdomains)
    if(allocated( this%IfaceElemConnectivity)) then
        deallocate( this%IfaceElemConnectivity)
    endif! IfaceElemConnectivity(:,:)
    if(allocated( this%IfaceElemDomainID)) then
        deallocate( this%IfaceElemDomainID)
    endif! IfaceElemDomainID(:,:)

    this%initialized = .false.
    this%InterfaceExist = .false.
    
    if(allocated( this%CRS_val)) then
        deallocate( this%CRS_val)
    endif! CRS_val(:)
    if(allocated(this%CRS_Index_Col)) deallocate(this%CRS_Index_Col)!(:)
    if(allocated(this%CRS_Index_Row)) deallocate(this%CRS_Index_Row)!(:)
    if(allocated( this%CRS_RHS)) then
        deallocate( this%CRS_RHS)
    endif! CRS_RHS(:)
    
    !> General Eigen Value Problem
    !> [A]{x} = (lambda)[B]{x}

    if(allocated(this%A_CRS_val)) deallocate(this%A_CRS_val)!(:)
    if(allocated(this%A_CRS_Index_Col)) deallocate(this%A_CRS_Index_Col)!(:)
    if(allocated(this%A_CRS_Index_Row)) deallocate(this%A_CRS_Index_Row)!(:)
    this%A_empty = .true.

    if(allocated(this%B_CRS_val)) deallocate(this%B_CRS_val)!(:)
    if(allocated(this%B_CRS_Index_Col)) deallocate(this%B_CRS_Index_Col)!(:)
    if(allocated(this%B_CRS_Index_Row)) deallocate(this%B_CRS_Index_Row)!(:)
    this%B_empty = .true.
    
    if(allocated(this%fix_eig_IDs)) deallocate(this%fix_eig_IDs)!(:)
    
    if(allocated( this%CRS_x)) then
        deallocate( this%CRS_x)
    endif! CRS_x(:)
    if(allocated( this%CRS_ID_Starts_From)) then
        deallocate( this%CRS_ID_Starts_From)
    endif! CRS_ID_Starts_From(:)

    ! dense matrix
    if(allocated( this%A_dense)) then
        deallocate( this%A_dense)
    endif! A_dense(:,:)
    if(allocated(this%Num_nodes_in_Domains)) deallocate(this%Num_nodes_in_Domains)! Num_nodes_in_Domains(:)


    if(numDomain<=0)then
        print *, "ERROR :: Number of element should be greater than 1"
        return
    else
        if(allocated(this%femdomains) )then
            deallocate(this%femdomains)
        endif
        allocate(this%femdomains(numDomain))
        allocate(this%DomainIDs(numDomain))
        do i=1, numDomain
            this%DomainIDs(i) = i
        enddo
    endif

!    if(NumInterfaceElement==0)then
!        this%InterfaceExist=.false.
!    else
!        ! check if NumNodePerInterfaceElement exists
!        if(.not. present(NumNodePerInterfaceElement) )then
!            print *, "ERROR :: NumNodePerInterfaceElement should be present."
!            return
!        endif
!        this%InterfaceExist=.true.
!    endif

    this%initialized = .true.

end subroutine


recursive subroutine setDomainFEMSolver(this,FEMDomain,FEMDomains,FEMDomainPointers,DomainID,DomainIDs)
    class(FEMSolver_),intent(inout) :: this
    type(FEMDomain_),target,optional,intent(in) :: FEMDomain,FEMDomains(:)
    type(FEMDomainp_),target,optional,intent(in) :: FEMDomainPointers(:)
    integer(int32),optional,intent(in) :: DomainID,DomainIDs(:)
    integer(int32) :: i

    if(present(DomainID) )then
        if(DomainID<=0)then
            print *, "ERROR :: FEMSOlver%setDomain >> DomainID should be >=1"
        endif
    endif

    if(present(DomainIDs) )then
        if(minval(DomainIDs)<=0)then
            print *, "ERROR :: FEMSOlver%setDomain >> DomainID should be >=1"
        endif
    endif

!    if(.not. present(DomainID) .and. .not.present(DomainIDs) )then
!        print *, "ERROR :: DomainID or DomainIDs are to be passed."
!        stop
!    endif
!
!    if(.not. present(FEMDomain) .and. .not.present(FEMDomains) )then
!        print *, "ERROR :: FEMDomain or FEMDomains are to be passed."
!        stop
!    endif

    if(.not.this%initialized)then
        print *, "ERROR :: this%setDomain should be called after this%init()"
    endif

    if(present(FEMDomain) .and. present(DomainID) )then
        if(associated(this%FEMDomains(DomainID)%FEMDomainp ) )then
            nullify(this%FEMDomains(DomainID)%FEMDomainp)
        endif
        this%FEMDomains(DomainID)%FEMDomainp => FEMDomain
        
        return    
    endif

    if(present(FEMDomains) .and. present(DomainIDs) )then
        
        if(maxval(DomainIDs) > size(this%femdomains) )then
            print *, "ERROR :: setFEMDomains >> invalid domain IDs"
            stop
        endif


        do i=1,size(DomainIDs)
            call this%setDomain(&
                FEMDomain=FEMDomains(i),DomainID=DomainIDs(i) )
        enddo
        return

    endif


    if(present(FEMDomains) .and. .not.present(DomainIDs) )then
        ! see domain_ptr as id
        do i=1,size(this%DomainIDs)
            call this%setDomain(&
                FEMDomain=FEMDomains(i),DomainID=this%DomainIDs(i) )
        enddo
        return
    endif

    if(present(FEMDomainPointers) .and. .not.present(DomainIDs) )then
        ! see domain_ptr as id
        this%DomainIDs = int(zeros(size(FEMDomainPointers) ))
        do i=1,size(FEMDomainPointers)
            this%DomainIDs(i) = i
        enddo

        do i=1,size(FEMDomainPointers)
            call this%setDomain(&
                FEMDomain=FEMDomainPointers(i)%femdomainp,DomainID= i)
        enddo
        return
    endif
    print *, "ERROR >> setDomainFEMSolver >> invalid combinations for args"

end subroutine


subroutine setCRS_CRSobjFEMSolver(this,CRS)
    class(FEMSolver_),intent(inout) :: this
    type(CRS_),intent(in) :: CRS


    this%CRS_Index_Col = CRS%col_idx
    this%CRS_Index_Row = CRS%row_ptr
    this%CRS_Val = CRS%val

end subroutine 

subroutine setRHSFEMSolver(this,RHS)
    class(FEMSolver_),intent(inout) :: this
    real(real64),intent(in) :: RHS(:)

    this%CRS_RHS = RHS

end subroutine

subroutine setCRSFEMSolver(this,DOF,debug)
    class(FEMSolver_),intent(inout) :: this
    
    integer(int32),intent(in) :: DOF
    logical,optional,intent(in) :: debug    
    integer(int32) :: i,j,k,l,m,n,itr
    integer(int32) :: size_of_global_matrix,offset,node_id,row_id
    integer(int32) :: node_id_p, col_id, kk, ll,drow_offset,buf_1
    integer(int32),allocatable :: Num_nodes_in_Domains(:),num_entry_in_row(:)
    integer(int32),allocatable :: col_local(:),new_col_local(:)
    integer(int32),allocatable :: Overset_CRS_Index_Col(:)
    integer(int32),allocatable :: Overset_CRS_Index_Row(:)
    integer(int32),allocatable :: new_CRS_Index_Col(:)
    integer(int32),allocatable :: new_CRS_Index_Row(:)
    integer(int32) :: row_node_id,col_node_id, row, col,row_domain_id,col_domain_id,&
        row_DOF,col_DOF, k0
    integer(int32),allocatable :: num_nodes_of_domains(:)
    logical :: debug_mode_on = .true.
    integer(int32) :: DomainID
    type(FEMSolver_) :: single_domain_solver
    type(COO_)       :: COO

    
    if(present(debug) )then
        debug_mode_on = debug
    endif

    if(allocated(this%femdomains) .and. size(this%femdomains) >=2 )then
        ! check interface
        itr = 0
        
        if(debug_mode_on)then
            print *, "[ok] setCRS started."
        endif

        do domainID=1,size(this%femdomains)
            if(this%femdomains(domainID)%femdomainp%empty() )then
                cycle
            endif

            call single_domain_solver%init(NumDomain=1)
            call single_domain_solver%setDomain(FEMDomain=this%femdomains(domainID)%femdomainp,&
                DomainID=1 )
            call single_domain_solver%setCRS(DOF=DOF)
            
            
            itr = itr + 1
            
            if(itr==1)then
                this%CRS_val            = single_domain_solver%CRS_val
                this%CRS_Index_Col      = single_domain_solver%CRS_Index_Col 
                this%CRS_Index_Row      = single_domain_solver%CRS_Index_Row      
                this%CRS_RHS            = single_domain_solver%CRS_RHS            
            else
                n = size(this%CRS_Index_Row)-1
                single_domain_solver%CRS_Index_Col(:) = single_domain_solver%CRS_Index_Col(:) + n 
                this%CRS_Index_Col = this%CRS_Index_Col // single_domain_solver%CRS_Index_Col 
                
                
                m = this%CRS_Index_Row(size(this%CRS_Index_Row))-1
                single_domain_solver%CRS_Index_Row(:)    = single_domain_solver%CRS_Index_Row(:) + m
                    
                this%CRS_Index_Row      = this%CRS_Index_Row(1:n) // single_domain_solver%CRS_Index_Row

                this%CRS_RHS            = this%CRS_RHS       //single_domain_solver%CRS_RHS            
                this%CRS_val            = this%CRS_val       //single_domain_solver%CRS_val            
            endif
            call single_domain_solver%remove()


        enddo

        if(debug_mode_on)then
            print *, "[ok] setCRS >> CRS-allocation done."
        endif
        ! <debug> following values are not unnatural
        !print *, size(this%CRS_Index_Col), maxval(this%CRS_Index_Col),minval(this%CRS_Index_Col)
        !print *, size(this%CRS_Index_Row), maxval(this%CRS_Index_Row),minval(this%CRS_Index_Row)
        !print *, size(this%CRS_RHS), maxval(this%CRS_RHS),minval(this%CRS_RHS)
        !print *, size(this%CRS_Val), maxval(this%CRS_Val),minval(this%CRS_Val)
        !print *, size(this%CRS_Index_Col)/size(this%CRS_Index_Row)
        
        call COO%init( num_row = size(this%CRS_Index_Row) -1 )
        ! next :: interface:
        do domainID=1,size(this%femdomains)
            if(this%femdomains(domainID)%femdomainp%empty() )then
                cycle
            endif

            if(allocated(this%femdomains(DomainID)%femdomainp%OversetConnect))then
                ! (a) create interface connectivity
                
                
                
                if(allocated(num_nodes_of_domains) ) deallocate(num_nodes_of_domains)
                allocate(num_nodes_of_domains(1:size(this%femdomains) ) ) 
                num_nodes_of_domains(1:size(this%femdomains) ) = 0
                do i=1,size(this%femdomains)
                    if(associated(this%femdomains(i)%femdomainp )  )then
                        if( .not.this%femdomains(i)%femdomainp%empty() )then
                            num_nodes_of_domains(i) = this%femdomains(i)%femdomainp%nn()
                        endif
                    endif
                enddo
                
                ! Overset_CRS_Index_Col
                ! Overset_CRS_Index_Row
                do i=1,size(this%femdomains(DomainID)%femdomainp%OversetConnect)
                    if(.not.allocated(this%femdomains(DomainID)%femdomainp%OversetConnect(i)%InterConnect) )then
                        cycle
                    endif
                    do j=1,size(this%femdomains(DomainID)%femdomainp%OversetConnect(i)%InterConnect)
                        do k=1,size(this%femdomains(DomainID)%femdomainp%OversetConnect(i)%InterConnect)
                            row_node_id = this%femdomains(DomainID)%femdomainp%OversetConnect(i)%InterConnect(j)
                            col_node_id = this%femdomains(DomainID)%femdomainp%OversetConnect(i)%InterConnect(k)
                            row_domain_id = this%femdomains(DomainID)%femdomainp%OversetConnect(i)%DomainIDs12(j)
                            col_domain_id = this%femdomains(DomainID)%femdomainp%OversetConnect(i)%DomainIDs12(k)
                            do row_DOF=1, DOF
                                do col_DOF=1, DOF
                                    if(row_domain_id==1)then
                                        row = (row_node_id-1)*DOF + row_DOF
                                    else
                                        row = sum(num_nodes_of_domains(1:row_domain_id-1))*DOF + &
                                        (row_node_id-1)*DOF + row_DOF
                                    endif
                                    

                                    if(col_domain_id==1)then
                                        col = (col_node_id-1)*DOF + col_DOF
                                    else
                                        col = sum(num_nodes_of_domains(1:col_domain_id-1))*DOF + &
                                        (col_node_id-1)*DOF + col_DOF
                                    endif
                                    
                                    !col = sum(num_nodes_of_domains(0:col_domain_id-1))*DOF + &
                                    !    (col_node_id-1)*DOF + col_DOF
                                    ! [ok] num_nodes_of_domains
                                    ! [ok] row_domain_id
                                    
                                    call COO%add(row=row,col=col,val=0.0d0)
                                enddo
                            enddo
                        enddo
                    enddo
                enddo

                ! (b) and merge them to 
                ! this%CRS_Index_Col
                ! this%CRS_Index_Row
                do i=1,size(this%CRS_Index_Row)-1
                    do j=this%CRS_Index_row(i),this%CRS_Index_row(i+1) - 1
                        row = i
                        col = this%CRS_Index_Col(j)
                        call COO%add(row=row,col=col,val=0.0d0)
                    enddo
                enddo


                ! COO -> CRSにする作戦，完了．
            endif
        enddo


        if(present(debug) )then
            if(debug)then
                print *, "[Ready!] Interface Memory "
            endif
        endif
                
        do i=1, size(COO%row)
            if(.not.allocated(COO%row(i)%col ) )then
                print *, "[CAUTION] >> some of doamins are not overset"
                cycle
            endif
            call heapsort(n=size(COO%row(i)%col) ,array=COO%row(i)%col )
        enddo

        if(present(debug) )then
            if(debug)then
                print *, "[Ready!] Sort ok! "
            endif
        endif


        new_CRS_Index_Row = int(zeros( size(COO%row) + 1 ) )
        new_CRS_Index_Row(1) = 1
        do i=2, size(COO%row)+1
            if( .not. allocated(COO%row(i-1)%col ))then
                new_CRS_Index_Row(i) = new_CRS_Index_Row(i-1)
                cycle
            endif
            new_CRS_Index_Row(i) = new_CRS_Index_Row(i-1) + size(COO%row(i-1)%col)
        enddo

        if(present(debug) )then
            if(debug)then
                print *, "[Ready!] Row ok! "
            endif
        endif

        ! this is really heavy.
        k = 0

        ! first, count size:

        new_CRS_Index_Col = COO%getAllCol()
        

        !do i=1, size(COO%row)
        !    if(allocated(COO%row(i)%col ) )then
        !        k=k+1
        !        if(k==1)then
        !            new_CRS_Index_Col = COO%row(i)%col
        !        else
        !            new_CRS_Index_Col = new_CRS_Index_Col // COO%row(i)%col
        !        endif
        !    endif
        !enddo

        if(present(debug) )then
            if(debug)then
                print *, "[Ready!] Col ok! "
            endif
        endif

        call COO%remove()

        this%CRS_Val = zeros( size(new_CRS_Index_Col) ) 
        this%CRS_Index_Col = new_CRS_Index_Col
        this%CRS_Index_Row = new_CRS_Index_Row

        if(.not.allocated(this%CRS_ID_Starts_From))then
            allocate(this%CRS_ID_Starts_From(size(this%FEMDomains) ))
            this%CRS_ID_Starts_From(1) = 1
            do i=2,size(this%FEMDomains)
                if(associated(this%FEMDomains(i)%femdomainp ) )then
                    this%CRS_ID_Starts_From(i) = this%CRS_ID_Starts_From(i-1) + this%FEMDomains(i-1)%femdomainp%nn()*DOF
                else
                    this%CRS_ID_Starts_From(i) = this%CRS_ID_Starts_From(i-1) + 0
                endif
            enddo
        endif

        return
    else


        if(.not. allocated(this%CRS_val))then
            ! allocate CRS-formatted Matrix-vector (Ax = b)
            ![A]
            if(allocated(this%CRS_Index_Col ) ) deallocate(this%CRS_Index_Col)
            if(allocated(this%CRS_Index_Row ) ) deallocate(this%CRS_Index_Row)
            
            ![b]
            if(allocated(this%CRS_RHS ) ) deallocate(this%CRS_RHS)
            
            ![x]
            if(allocated(this%CRS_x ) ) deallocate(this%CRS_x)
            if(allocated(this%CRS_ID_Starts_From) ) deallocate(this%CRS_ID_Starts_From)
    
            !通し番号を振る
            !First, For Domains
            !DomainID -> NodeID -> DOF(x-y-z, etc.)
            ! count number of global unknowns
            size_of_global_matrix = 0
            Num_nodes_in_Domains = int(zeros(size(this%FEMDomains)) )
            do i=1,size(this%FEMDomains)
                if(associated(this%FEMDomains(i)%femdomainp ) )then
                    size_of_global_matrix = size_of_global_matrix &
                        + this%FEMDomains(i)%femdomainp%nn()*DOF
                    Num_nodes_in_Domains(i)=this%FEMDomains(i)%femdomainp%nn()
                endif
            enddo
    
            allocate(this%CRS_ID_Starts_From(size(this%FEMDomains) ))
            this%CRS_ID_Starts_From(1) = 1
            do i=2,size(this%FEMDomains)
                if(associated(this%FEMDomains(i)%femdomainp ) )then
                    this%CRS_ID_Starts_From(i) = this%CRS_ID_Starts_From(i-1) + this%FEMDomains(i-1)%femdomainp%nn()*DOF
                else
                    this%CRS_ID_Starts_From(i) = this%CRS_ID_Starts_From(i-1) + 0
                endif
            enddo
            !
            this%CRS_Index_Row = int(zeros(size_of_global_matrix+1))
            this%CRS_RHS       = int(zeros(size_of_global_matrix))
            this%CRS_x         = int(zeros(size_of_global_matrix))
    
            num_entry_in_row   = int(zeros(size_of_global_matrix))
            
    
            ! First, create CRS-Index-Row
            !print *, "! First, create CRS-Index-Row"
            !DomainID -> NodeID -> DOF(x-y-z, etc.)
            !CRSだが，重複を許し大目に見積もる
            ! 本当のCRS_INdex_Rowではない．あくまで，各Rowに最大いくつのcolumnが非ゼロとなりうるか．
            
            do i=1,size(Num_nodes_in_Domains)
                
                if(i==1)then
                    if(associated(this%FEMDomains(i)%femdomainp ))then
                        
                        offset=0
                        do j=1,this%FEMDomains(i)%femdomainp%ne()
                            do k=1,this%FEMDomains(i)%femdomainp%nne()
                                do l=1,DOF
                                    node_id = this%FEMDomains(i)%femdomainp%mesh%elemnod(j,k)
                                    this%CRS_Index_Row(DOF*Offset + DOF*(node_id-1)+l ) = &
                                    this%CRS_Index_Row(DOF*Offset + DOF*(node_id-1)+l ) &
                                        + (this%FEMDomains(i)%femdomainp%nne())*DOF
                                    !最大でも，この数までの未知数としか係数行列を持たない
                                enddo
                            enddo
                        enddo
                    endif
                    
                else
                    if(associated(this%FEMDomains(i)%femdomainp ))then
                        offset=sum(Num_nodes_in_Domains(1:i-1))
                        do j=1,this%FEMDomains(i)%femdomainp%ne()
                            do k=1,this%FEMDomains(i)%femdomainp%nne()
                                do l=1,DOF
                                    node_id = this%FEMDomains(i)%femdomainp%mesh%elemnod(j,k)
                                    this%CRS_Index_Row(DOF*Offset + DOF*(node_id-1)+l ) = &
                                    this%CRS_Index_Row(DOF*Offset + DOF*(node_id-1)+l ) &
                                        + (this%FEMDomains(i)%femdomainp%nne())*DOF
                                    !最大でも，この数までの未知数としか係数行列を持たない
                                enddo
                            enddo
                        enddo
                    endif
                    
                endif
            enddo
            
    
            !this%CRS_Index_Rowに，あと，Interfaceのconnectivityのぶんを足す（あとで）
    
    
            ! CRS-Index-colを作成
            !print *, "! CRS-Index-colをAllocate"
            this%CRS_Index_Col = int(zeros(sum(this%CRS_Index_Row(:)) ))
            this%CRS_Val = zeros(  sum(this%CRS_Index_Row(:)) )
    
            ! 本当のCRS_INdex_Rowにする．
            buf_1 = this%CRS_Index_Row(1)
            do i=2,size(this%CRS_Index_Row)
                this%CRS_Index_Row(i) =this%CRS_Index_Row(i-1)+ this%CRS_Index_Row(i)
            enddo
            do i=size(this%CRS_Index_Row),2,-1
                this%CRS_Index_Row(i) =this%CRS_Index_Row(i-1)+1
            enddo
            this%CRS_Index_Row(1)=1
    
            !print *, "! CRS-Index-colを作成"
            num_entry_in_row(:) = 0
            do i=1,size(Num_nodes_in_Domains)
    
                if(i==1)then
                    if(associated(this%FEMDomains(i)%femdomainp ))then
                        offset=0
                        do j=1,this%FEMDomains(i)%femdomainp%ne()
                            do k=1,this%FEMDomains(i)%femdomainp%nne()
                                do l=1,DOF
                                    node_id = this%FEMDomains(i)%femdomainp%mesh%elemnod(j,k)
                                    row_id = DOF*Offset+DOF*(node_id-1)+l
                                    do kk=1,this%FEMDomains(i)%femdomainp%nne()
                                        do ll=1,DOF
                                            node_id_p=this%FEMDomains(i)%femdomainp%mesh%elemnod(j,kk)
                                            col_id =DOF*Offset+DOF*(node_id_p-1)+ll
                                            num_entry_in_row(row_id) = num_entry_in_row(row_id) + 1
                                            drow_offset = this%CRS_Index_Row(row_id)-1
                                            
                                            this%CRS_Index_Col( drow_offset  &
                                            + num_entry_in_row(row_id)) = col_id
                                        enddo
                                    enddo
                                enddo
                            enddo
                        enddo
                    endif
    
                else
                    if(associated(this%FEMDomains(i)%femdomainp ))then
                        offset=sum(Num_nodes_in_Domains(1:i-1))
                        do j=1,this%FEMDomains(i)%femdomainp%ne()
                            do k=1,this%FEMDomains(i)%femdomainp%nne()
                                do l=1,DOF
                                    node_id = this%FEMDomains(i)%femdomainp%mesh%elemnod(j,k)
                                    row_id = DOF*Offset+DOF*(node_id-1)+l
                                    do kk=1,this%FEMDomains(i)%femdomainp%nne()
                                        do ll=1,DOF
                                            node_id_p=this%FEMDomains(i)%femdomainp%mesh%elemnod(j,kk)
                                            col_id =DOF*Offset+DOF*(node_id_p-1)+ll
                                            num_entry_in_row(row_id) = num_entry_in_row(row_id) + 1
                                            drow_offset = this%CRS_Index_Row(row_id)-1
                                            
                                            this%CRS_Index_Col( drow_offset  &
                                            + num_entry_in_row(row_id)) = col_id
                                        enddo
                                    enddo
                                enddo
                            enddo
                        enddo
                    endif
                    
                endif
            enddo
    
            ! crs_index_colに被っているものがあるので，それを省く
            !print *,"! crs_index_colに被っているものがあるので，それを省く"
            
            num_entry_in_row(:) = 0
            
            !$OMP parallel do default(shared) private(col_local,new_col_local)
            do i=1,size(this%CRS_Index_Row)-1

                col_local = int(zeros(this%CRS_Index_Row(i+1)-this%CRS_Index_Row(i)))
                col_local(1:this%CRS_Index_Row(i+1)-this%CRS_Index_Row(i) ) = &
                this%CRS_Index_Col(this%CRS_Index_Row(i):this%CRS_Index_Row(i+1)-1 )

                new_col_local = RemoveIF(col_local,equal_to=0)
                new_col_local = RemoveOverwrap(new_col_local)
                
                
                col_local(:)  = 0
                col_local(1:size(new_col_local) ) = new_col_local(:)
                this%CRS_Index_Col(this%CRS_Index_Row(i):this%CRS_Index_Row(i+1)-1 ) = &
                col_local(1:this%CRS_Index_Row(i+1)-this%CRS_Index_Row(i) ) 
                num_entry_in_row(i) = size(new_col_local)
            enddo
            !$OMP end parallel do
    
            !print *, "Final"
            
            this%CRS_Index_Col = RemoveIF(this%CRS_Index_Col,equal_to=0)
            
    
            buf_1 = num_entry_in_row(size(num_entry_in_row)  )
            do i=2,size(num_entry_in_row)
                num_entry_in_row(i) =num_entry_in_row(i-1)+ num_entry_in_row(i)
            enddo
            do i=size(num_entry_in_row),2,-1
                num_entry_in_row(i) =num_entry_in_row(i-1)+1
            enddo
            num_entry_in_row(1)=1
            
            this%CRS_Index_Row(1:size(num_entry_in_row )) = num_entry_in_row(:)
            this%CRS_Index_Row(size(num_entry_in_row )+1) = num_entry_in_row(size(num_entry_in_row) ) + buf_1
            
            !do i=1,size(num_entry_in_row)
            !    this%CRS_Index_Row(i) = sum(num_entry_in_row(1:i)) -num_entry_in_row(i) +  1
            !enddo
    
    
            ! then, this%CRS_Index_Row and this%CRS_Index_Col are filled.
            this%CRS_Val = zeros(size(this%CRS_Index_Col))
            
        endif
    
    endif


end subroutine
! #############################################################################

subroutine setMatrixFEMSolver(this,DomainID,ElementID,DOF,Matrix,as_Dense)
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: DomainID,ElementID,DOF
    real(real64),intent(in) :: Matrix(:,:)
    logical,optional,intent(in) :: as_Dense

    call this%setValue(DomainID=DomainID,ElementID=ElementID,DOF=DOF,Matrix=Matrix,as_dense=as_dense)

    
end subroutine

! #############################################################################

subroutine setVectorFEMSolver(this,DomainID,ElementID,DOF,Vector)
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: DomainID,ElementID,DOF
    real(real64),intent(in) :: Vector(:)

    call this%setValue(DomainID=DomainID,ElementID=ElementID,DOF=DOF,Vector=Vector)

    
end subroutine

! #############################################################################
subroutine addMatrixValueFEMSolver(this,row_id,col_id,SingleValue,as_Dense)
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: row_id,col_id
    real(real64),intent(in) :: SingleValue
    logical,optional,intent(in) :: as_Dense
    logical :: successfully_done = .false.
    integer(int32) :: i,j

    if(present(As_Dense) )then
        if(As_Dense)then
            this%A_dense(row_id,col_id) = SingleValue
            return
        endif
    endif

    if(.not. allocated(this%CRS_val) )then
        print *, "ERROR ::addMatrixValueFEMSolver >>  .not. allocated(this%CRS_val)"
        stop
    endif

    do i=this%CRS_Index_row(row_id),this%CRS_Index_row(row_id+1)-1
        if(this%CRS_Index_col(i) == col_id )then
            ! add
            this%CRS_Val(i) = this%CRS_Val(i) + SingleValue
            successfully_done = .true.
        endif
    enddo
    if(.not. successfully_done)then
        print *, "ERROR ::addMatrixValueFEMSolver >>  the address is not allocated in CRS"
        print *, "row",row_id
        print *, "col",col_id
        stop
    endif

end subroutine
! #############################################################################




! #############################################################################
subroutine setValueFEMSolver(this,DomainID,ElementID,DOF,Matrix,Vector,as_Dense)
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: DomainID,ElementID,DOF
    real(real64),optional,intent(in) :: Matrix(:,:),Vector(:)
    logical,optional,intent(in) :: as_Dense
    
    integer(int32) :: row_id, col_id, CRS_id,row_node_id,col_node_id
    integer(int32) :: i,j,ii,jj,eRow_id, eCol_id,k,id,nne,l_row_id,l_col_id
    integer(int32) :: g_row_id,g_col_id,g_node_id_row,g_node_id_col,offset
    integer(int32),allocatable ::  local_col_ids(:)

    if(present(as_Dense) )then
        if(as_Dense)then
            ! store as dense matrix
            
            if(.not.allocated(this%A_dense) )then
                this%Num_nodes_in_Domains = int(zeros(size(this%femdomains) ))
                do i=1,sizE(this%FEMDomains)
                    if(associated(this%femdomains(i)%femdomainp) )then
                        this%Num_nodes_in_Domains(i) = this%femdomains(i)%femdomainp%nn()
                    endif
                enddo
                this%A_dense = zeros(sum(this%Num_nodes_in_Domains)*DOF,sum(this%Num_nodes_in_Domains)*DOF )
            endif

            do i=1,this%femdomains(DomainID)%femdomainp%nne() ! row
                do j=1,DOF
                    do ii=1,this%femdomains(DomainID)%femdomainp%nne() ! col
                        do jj=1,DOF
                            nne = this%femdomains(DomainID)%femdomainp%nne()
                            
                            l_row_id = DOF*(i -1) + j
                            l_col_id = DOF*(ii-1) + jj

                            g_node_id_row=this%femdomains(DomainID)%femdomainp%mesh%elemnod(ElementID,i)
                            g_node_id_col=this%femdomains(DomainID)%femdomainp%mesh%elemnod(ElementID,ii)

                            if(DomainID==1)then
                                offset = 0
                            else
                                offset = sum(this%Num_nodes_in_Domains(1:DomainID-1))
                            endif

                            g_row_id = Offset*DOF + (g_node_id_row-1)*DOF  + j
                            g_col_id = Offset*DOF + (g_node_id_col-1)*DOF + jj
                            this%A_dense(g_row_id,g_col_id) =this%A_dense(g_row_id,g_col_id) + Matrix(l_row_id,l_col_id)
                        enddo
                    enddo
                enddo
            enddo
            return
        endif
    endif
    
    if(.not.allocated(this%CRS_val) )then
        call this%setCRS(DOF=DOF)
    endif

    ! bugなし?
    if(present(Matrix) )then
        do i=1,this%femdomains(DomainID)%femdomainp%nne()
            do j=1,DOF
                row_node_id=this%femdomains(DomainID)%femdomainp%mesh%elemnod(ElementID,i)
                row_id = this%CRS_ID_Starts_From(DomainID)-1+DOF*(row_node_id-1)+j
                
                do ii=1,this%femdomains(DomainID)%femdomainp%nne()
                    do jj=1,DOF
                        col_node_id=this%femdomains(DomainID)%femdomainp%mesh%elemnod(ElementID,ii)
                        col_id = this%CRS_ID_Starts_From(DomainID)-1+DOF*(col_node_id-1)+jj
                        local_col_ids = zeros(  this%CRS_Index_Row(row_id+1) - this%CRS_Index_Row(row_id)  )
                        local_col_ids(:) = this%CRS_Index_Col( this%CRS_Index_Row(row_id) : this%CRS_Index_Row(row_id+1)-1 )
                        
                        id = -1
                        do k=1,size(local_col_ids)
                            if(local_col_ids(k)==col_id )then
                                id = k
                                exit
                            endif
                        enddo
                        if(id==-1)then
                            print *, "[ERROR] :: No memory is allocated in CRS format for "
                            print *, "Domain  : ",DomainID
                            print *, "Element : ",ElementID
                            print *, "Nodes    : ",i,ii
                            print *, "Dims    : ",j,jj
                            stop
                        endif
                        CRS_id = this%CRS_Index_Row(row_id)-1 + id
                        eRow_id = DOF*(i-1) + j
                        eCol_id = DOF*(ii-1) + jj
                        !!$OMP atomic
                        this%CRS_val(CRS_id) = this%CRS_val(CRS_id) + Matrix(eRow_id,eCol_id)
                    enddo
                enddo
            enddo
        enddo
    endif

    if(present(Vector) )then
        do i=1,this%femdomains(DomainID)%femdomainp%nne()
            do j=1,DOF
                row_node_id=this%femdomains(DomainID)%femdomainp%mesh%elemnod(ElementID,i)
                row_id = this%CRS_ID_Starts_From(DomainID)-1+DOF*(row_node_id-1)+j
                eRow_id = DOF*(i-1) + j
                !!$OMP atomic
                this%CRS_RHS(Row_id) = this%CRS_RHS(Row_id) + Vector(eRow_id)
            enddo
        enddo
    endif

end subroutine
! ###################################################################
subroutine fixFEMSolver(this,DomainID,IDs,FixValue,FixValues)
    class(FEMSolver_),intent(inout) :: this

    real(real64),optional,intent(in) :: FixValue
    real(real64),optional,intent(in) :: FixValues(:)
    integer(int32),intent(in) :: DomainID
    integer(int32),intent(in) :: IDs(:)

    integer(int32),allocatable :: buf(:)
    integer(int32),allocatable :: buf_real(:)
    integer(int32) :: i, n

    ! fix unknowns for linear solvers
    ! fix IDs(:)-th amplitudes as zero (Fixed boundary)
    ! only create list
    if(.not.allocated(this%fix_lin_exists) )then
        n = size(this%CRS_RHS)
        allocate(this%fix_lin_exists( n ) )
        
        this%fix_lin_exists_values = zeros( n )
        this%fix_lin_exists(:) = .false.
        
        do i=1,size(IDs)
            if(IDs(i)< 1 ) cycle
            
            if(IDs(i) > size(this%fix_lin_exists) ) cycle
            if(IDs(i) <= 0 ) cycle
            
            this%fix_lin_exists( IDs(i)) = .true.
            if(present(FixValue) )then
                this%fix_lin_exists_values(IDs(i)) = FixValue
            elseif(present(FixValues) )then
                this%fix_lin_exists_values(IDs(i)) = FixValues(i)
            else
                this%fix_lin_exists_values( IDs(i) ) = 0.0d0
            endif

        enddo
    else

        do i=1,size(IDs)
            if(IDs(i)< 1 ) cycle
            this%fix_lin_exists( IDs(i) ) = .true.
            if(present(FixValue) )then
                this%fix_lin_exists_values( IDs(i) ) = FixValue
            elseif(present(FixValues) )then
                this%fix_lin_exists_values( IDs(i) ) = FixValues(i)
            else
                this%fix_lin_exists_values( IDs(i) ) = 0.0d0
            endif
        enddo
    endif
    
end subroutine

!
function diagFEMSolver(this,cell_centered) result(diag_vector)
    class(FEMSolver_),intent(in) :: this
    logical,optional,intent(in)  :: cell_centered
    real(real64),allocatable :: diag_vector(:)
    
    integer(int32) :: row,col,id

    if(present(cell_centered) )then
        if(cell_centered)then
            
        ! diagonal components of CRS matrix
        if(allocated(this%CRS_val) )then
            diag_vector = zeros(size(this%CRS_Index_row) -1 )
            do row=1,size(this%CRS_Index_row) -1
                do id=this%CRS_Index_row(row),this%CRS_Index_row(row+1)-1
                    diag_vector(row) = diag_vector(row) + this%CRS_val(id)
                enddo
            enddo
        endif
        endif
    endif

    ! diagonal components of CRS matrix
    if(allocated(this%CRS_val) )then
        diag_vector = zeros(size(this%CRS_Index_row) -1 )
        do row=1,size(this%CRS_Index_row) -1
            do id=this%CRS_Index_row(row),this%CRS_Index_row(row+1)-1
                col = this%CRS_Index_col(id)
                if(col==row)then
                    diag_vector(row) = this%CRS_val(id)
                endif
            enddo
        enddo
    endif

end function


! ####################################################

!
subroutine matmulDiagMatrixFEMSolver(this,diagMat)
    class(FEMSolver_),intent(inout) :: this
    real(real64),intent(in) :: diagMat(:)
    integer(int32) :: n
    integer(int32) :: row,col,id
    
    n = size(diagMat)
!> diag is diagonal component of n x n matrix

    ! diagonal components of CRS matrix
    if(allocated(this%CRS_val) )then
        do row=1,size(this%CRS_Index_row) -1
            do id=this%CRS_Index_row(row),this%CRS_Index_row(row+1)-1
                col = this%CRS_Index_col(id)
                if(col==row)then
                    this%CRS_val(id) = this%CRS_val(id)*diagMat(row)
                endif
            enddo
        enddo
    endif

end subroutine
! ###################################################################
subroutine loadMatrixFEMSolver(this,from)
    class(FEMSolver_),intent(inout) :: this
    character(1),intent(in) :: from

    ! overwrite this%CRS_*
    select case(from)
        case("A")
            this%CRS_val = this%A_CRS_val
            this%CRS_Index_Col = this%A_CRS_Index_Col
            this%CRS_Index_Row = this%A_CRS_Index_Row
        case("B")
            this%CRS_val = this%B_CRS_val
            this%CRS_Index_Col = this%B_CRS_Index_Col
            this%CRS_Index_Row = this%B_CRS_Index_Row
        case default
            print *, "ERROR :: loadMatrixFEMSolver >> from should be A or B."
            stop
    end select


end subroutine

! ###################################################################
subroutine saveMatrixFEMSolver(this,name,CRS_as_dense, if_dense_exists,zero_or_nonzero)
    class(FEMSolver_),intent(in) :: this
    character(*),intent(in) :: name
    logical,optional,intent(in) :: CRS_as_dense,if_dense_exists,zero_or_nonzero
    integer(int32) :: i,j,k,n
    real(real64),allocatable :: row_vector(:)
    type(IO_)::f

    if(present(if_dense_exists) )then
        if(if_dense_exists)then
            if(.not.allocated(this%A_dense) ) return
            call f%open(name+"_dense.csv","w")
            n = size(this%A_dense,1)
            
            do i=1,n
                do j=1,n-1
                    write(f%fh,'(A)',advance='no') str(this%A_dense(i,j) )+","
                enddo
                write(f%fh,'(A)',advance='yes') str(this%A_dense(i,n) )
            enddo
            call f%close()
        endif
        return
    endif

    if(.not.allocated(this%CRS_val) )then
        print *, "[ERROR] saveMatrixFEMSolver >> (.not.allocated(this%CRS_val) )"
        return
    endif

    if(present(CRS_as_dense) )then
        if(CRS_as_dense)then
            call f%open(name+"_dense.csv","w")
            n = size(this%CRS_Index_Row)-1
            do i=1,n
                row_vector = zeros(n)
                do j=this%CRS_Index_Row(i),this%CRS_Index_Row(i+1)-1
                    row_vector( this%CRS_Index_Col(j) ) = this%CRS_val(j)
                enddo
                do j=1,size(row_vector)-1
                    if(present(zero_or_nonzero) )then
                        if(zero_or_nonzero)then
                            if(row_vector(j) ==0.0d0)then
                                write(f%fh,'(A)',advance='no') "0"        
                            else
                                write(f%fh,'(A)',advance='no')  "*"        
                            endif
                            cycle        
                        endif
                    endif            
                    write(f%fh,'(A)',advance='no') str(row_vector(j) )+","
                enddo
                if(present(zero_or_nonzero) )then
                    if(zero_or_nonzero)then
                        if(row_vector(n) ==0.0d0)then
                            write(f%fh,'(A)',advance='yes') "0"        
                        else
                            write(f%fh,'(A)',advance='yes')  "*"        
                        endif
                        cycle        
                    endif
                endif            
                write(f%fh,'(A)',advance='yes') str(row_vector(n) )
            enddo
            call f%close()
        endif
    else

        call f%open(name+"_data.txt","w")
        call f%write(this%CRS_val)
        call f%close()
    
    
        call f%open(name+"_indices.txt","w")
        call f%write(this%CRS_Index_Col)
        call f%close()
    
        call f%open(name+"_indptr.txt","w")
        call f%write(this%CRS_Index_Row)
        call f%close()
        
    endif


end subroutine
! ###################################################################
subroutine zerosFEMSolver(this)
    class(FEMSolver_),intent(inout)::this

    this%CRS_val(:) = 0.0d0
    if(allocated(this%CRS_RHS) )then
        this%CRS_RHS(:) = 0.0d0
    endif
    
    if(allocated(this%A_dense) )then
        this%A_dense(:,:) = 0.0d0
    endif

end subroutine

! ###################################################################

!function eigFEMSolver(this,num_eigen,tol,eigen_value,as_dense) result(eig_vec)
!    class(FEMSolver_),intent(in)::this
!    real(real64),allocatable :: eig_vec(:,:),dense_mat(:,:)
!    real(real64),optional,allocatable,intent(inout) :: eigen_value(:)
!    real(real64),intent(in) :: tol
!    integer(int32),optional,intent(in) :: num_eigen
!    integer(int32) :: ndim
!    logical,optional,intent(in) :: as_dense
!    !> default =>> get eigen vectors of this%CRS
!    !> eigens(:,0) are eigen values
!    !> eigens(:,n) are n-th eigen vectors
!    if(present(as_Dense))then
!        if(as_Dense)then
!            call to_Dense(this%CRS_val,this%CRS_index_col,this%CRS_index_row,&
!                dense_mat)
!            dense_mat = 0.50d0*(dense_mat + transpose(dense_mat) )
!            call eigenValueAndVector(A=dense_mat,&
!                lambda=eigen_value,x=eig_vec,tol=tol) 
!            return
!        endif
!    endif
!
!    ndim = size(this%CRS_Index_Row) - 1
!
!    eigen_value = zeros(num_eigen)
!    eig_vec =  LOBPCG_sparse(&
!        A_val=this%CRS_val,&
!        A_col=this%CRS_index_col,&
!        A_rowptr=this%CRS_index_row,&
!        lambda_min=eigen_value,&
!        tolerance=tol)
!
!    
!end function

! ###################################################################
subroutine LanczosMethod(this,eigen_value,Eigen_vectors,max_itr)
    clasS(FEMSolver_),intent(inout) :: this
    real(real64),allocatable :: eigen_value(:),Eigen_vectors(:,:)
    real(real64),allocatable :: w(:)
    real(real64)::alpha,beta
    integer(int32),intent(in) :: max_itr
    integer(int32) :: num_dim
    integer(int32) :: i,j

    num_dim = size(this%CRS_index_row) - 1
    !http://www.slis.tsukuba.ac.jp/~fujisawa.makoto.fu/cgi-bin/wiki/index.php?%CF%A2%CE%A91%BC%A1%CA%FD%C4%F8%BC%B0%A1%A7Lanczos%CB%A1

    eigen_value   = zeros(num_dim)
    eigen_vectors = eyesMatrix(num_dim,num_dim)
    print *, "Lanczos method is not implemented."
    do i=1,max_itr

    enddo



end subroutine
! ###################################################################





recursive subroutine eigFEMSolver(this,num_eigen,eigen_value,eigen_vectors)
    ! solve Ku = \lambda M x by LAPACK
    clasS(FEMSolver_),intent(inout) :: this
    integer(int32),optional,intent(in)::num_eigen
    !logical,optional,intent(in) :: Lanczos



    !>>>>>>>>>>>>>> INPUT
    integer(int32) :: ITYPE = 1   ! A*x = (lambda)*B*x
    character(1) :: JOBZ  = 'V' ! Compute eigenvalues and eigenvectors.
    character(1) :: UPLO  = 'U' ! Upper triangles of A and B are stored;
    !<<<<<<<<<<<<<< INPUT

    integer(int32) :: N ! order of matrix
    real(real64),allocatable :: AP(:)
    real(real64),allocatable :: BP(:)
    real(real64),allocatable :: W(:)
    real(real64),allocatable :: Z(:,:),M(:)
    real(real64),allocatable :: WORK(:),ID(:)
    real(real64),allocatable,intent(inout) :: eigen_value(:)
    real(real64),allocatable,intent(inout) :: eigen_vectors(:,:)
    integer(int32),allocatable :: IWORK(:),IDS(:)
    integer(int32) :: LDZ
    integer(int32) :: LWORK
    integer(int32) :: LIWORK 
    integer(int32) :: INFO
    integer(int32) :: from,to,k,j,i
    integer(int32),allocatable :: new_id_from_old_id(:)
    real(real64),allocatable :: dense_mat(:,:)
    !logical :: use_lanczos
    type(IO_) :: f
    type(CRS_) :: crs

    !use_lanczos = .false.
    !if(present(Lanczos) )then
    !    use_lanczos = Lanczos
    !endif

    if(allocated(this%fix_eig_IDs) )then
        ! amplitudes are zero@ this%fix_eig_IDs
        ! remove from problem [A][U] = w[B][U]
        ! sort before it
        
        if(size(this%fix_eig_IDs)>=1 )then
            ! first, for [A]
            call heapsort(n=size(this%fix_eig_IDs),array=this%fix_eig_IDs)
            call reduce_crs_matrix(CRS_val=this%A_CRS_val,CRS_col=this%A_CRS_index_col,&
            CRS_rowptr=this%A_CRS_index_row,remove_IDs=this%fix_eig_IDs)
            call reduce_crs_matrix(CRS_val=this%B_CRS_val,CRS_col=this%B_CRS_index_col,&
            CRS_rowptr=this%B_CRS_index_row,remove_IDs=this%fix_eig_IDs)
        endif
    endif
    
    !>>>>>>>>>>>>>> INPUT
    N      = size(this%A_CRS_index_row) -1 
    LDZ    = input(default=N,option=num_eigen)
    LWORK  = 1 + 6*N + 2*N**2
    LIWORK = 3 + 5*N
    !<<<<<<<<<<<<<< INPUT

    if(this%use_LOBPCG)then
        print *, ">> Solver :: LOBPCG"
        call LOBPCG(&
            A=this%getCRS("A"),&
            B=this%getCRS("B"),&
            X=Z, lambda=W,&
            m=input(default=this%LOBPCG_NUM_MODE,option=num_eigen ),&
            MAX_ITR=this%LOBPCG_MAX_ITR,&
            TOL=this%LOBPCG_TOL,&
            debug=this%debug)

    else
        !>>>>>>>>>>>>>>  INPUT/OUTPUT
        AP = zeros(N*(N+1)/2 )
        BP = zeros(N*(N+1)/2 )
        ! Upper triangle matrix
        AP = UpperTriangularMatrix(CRS_val=this%A_CRS_val,CRS_col=this%A_CRS_index_col,&
            CRS_rowptr=this%A_CRS_index_row)
        BP = UpperTriangularMatrix(CRS_val=this%B_CRS_val,CRS_col=this%B_CRS_index_col,&
            CRS_rowptr=this%B_CRS_index_row)
        !<<<<<<<<<<<<<< INPUT/OUTPUT
        
        !>>>>>>>>>>>>>> INPUT
        N      = size(this%A_CRS_index_row) -1 
        LDZ    = input(default=N,option=num_eigen)
        LWORK  = 1 + 6*N + 2*N**2
        LIWORK = 3 + 5*N
        !<<<<<<<<<<<<<< INPUT

        !>>>>>>>>>>>>>>  OUTPUT
        W     = zeros(N )
        Z     = zeros(LDZ,N)
        WORK  = zeros(LWORK)
        IWORK = zeros(LIWORK)
        INFO  = 0
        !<<<<<<<<<<<<<< OUTPUT
        

        print *, ">> Solver :: LAPACK/DSPGVD"
        call DSPGVD (ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
        LWORK, IWORK, LIWORK, INFO)

    endif
    
    eigen_value = w
    if(allocated(this%fix_eig_IDs) )then    
        ! U(this%fix_eig_IDs(i),: ) = 0.0d0
        if(size(this%fix_eig_IDs)>=1 )then
            new_id_from_old_id = zeros(N)
            
            !new_id_from_old_id(Dirichlet境界を除いた固有ベクトルzのj番成分が，もとの何番目に対応するか)

            k = 0
            do j=1,this%fix_eig_IDs(1)-1
                k = k + 1
                new_id_from_old_id(k) = k
            enddo
            
            do i=2,size(this%fix_eig_IDs)
                from = this%fix_eig_IDs(i-1)+1
                to   = this%fix_eig_IDs(i)-1
                do j=from,to
                    k = k + 1
                    if(k > size(new_id_from_old_id) ) cycle
                    new_id_from_old_id(k) = j
                enddo
            enddo
            
            do j=this%fix_eig_IDs( size(this%fix_eig_IDs) )+1,N+size(this%fix_eig_IDs)
                k = k + 1
                if(k > size(new_id_from_old_id) ) cycle
                new_id_from_old_id(k) = j
            enddo
        
            
            eigen_vectors = zeros(size(Z,1)+size(this%fix_eig_IDs),size(Z,2) ) 
            do i=1,size(Z,2)
                do j=1,size(new_id_from_old_id,1)
                    eigen_vectors( new_id_from_old_id(j) ,i) = Z( j  ,i)
                enddo
            enddo
            
        else
            eigen_vectors = Z    
        endif
    else
        eigen_vectors = Z    
    endif

end subroutine
! ###################################################################

subroutine keepThisMatrixAsFEMSolver(this,As)
    class(FEMSolver_),intent(inout) :: this
    character(1),intent(in) :: As ! [A] or [B]

    if(As == "A")then
        this%A_CRS_Index_Col = this%CRS_Index_Col
        this%A_CRS_Index_Row = this%CRS_Index_Row
        this%A_CRS_val       = this%CRS_val
        this%A_empty         = .false. 
        return
    endif
    
    if(As == "B")then
        this%B_CRS_Index_Col = this%CRS_Index_Col
        this%B_CRS_Index_Row = this%CRS_Index_Row
        this%B_CRS_val       = this%CRS_val
        this%B_empty         = .false.
        return
    endif
    
    print *, "As = A or B"
    stop 

end subroutine

! ################################################################
subroutine fix_eigFEMSolver(this,IDs) 
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: IDs(:)
    integer(int32),allocatable :: buf(:)

    ! fix IDs(:)-th amplitudes as zero (Fixed boundary)
    ! only create list
    if(.not.allocated(this%fix_eig_IDs) )then
        this%fix_eig_IDs = IDs
    elseif(size(this%fix_eig_IDs)==0)then
        this%fix_eig_IDs = IDs
    else
        
        buf = this%fix_eig_IDs
        this%fix_eig_IDs = zeros(size(buf) + size(IDs) )
        this%fix_eig_IDs(1:size(buf) ) = buf(:)
        this%fix_eig_IDs(size(buf)+1:) = IDs(:)
    endif

    this%fix_eig_IDs = unique(this%fix_eig_IDs)
    


end subroutine
! #####################################################

! #####################################################
function solveFEMSolver_UserDefinedLinearSolver(this,LinearSolver,x0) result(x)
    class(FEMSolver_),intent(inout) :: this
    real(real64),optional,intent(in) :: x0(:)
    real(real64),allocatable :: x(:)

    ! CRS formatted Linear solver
    interface 
        subroutine LinearSolver(row_ptr,col_idx,val,rhs,x) 
            use iso_fortran_env
            implicit none
            real(real64),intent(in) :: val(:),rhs(:)
            real(real64),intent(inout) :: x(:)
            integer(int32),intent(in) :: row_ptr(:),col_idx(:)

        end subroutine
    end interface

    if(present(x0) )then
        x = x0 
    else
        x = zeros(size(this%CRS_index_row)-1 )
    endif

    ! 外部ソルバの利用(ただし，subroutineのみ)
    call LinearSolver(this%CRS_index_row,this%CRS_Index_Col,this%CRS_val,&
        this%CRS_RHS,x)

    this%CRS_x = x

end function

! #####################################################


! #####################################################
function solveFEMSolver_UserDefinedLinearSolverAsFunc(this,LinearSolver,x0) result(x)
    class(FEMSolver_),intent(inout) :: this
    real(real64),intent(in) :: x0(:)
    real(real64),allocatable :: x(:)

    ! CRS formatted Linear solver
    interface 
        function LinearSolver(row_ptr,col_idx,val,rhs,x0) result(x)
            use iso_fortran_env
            implicit none
            real(real64),intent(in) :: val(:),rhs(:)
            real(real64),intent(in) :: x0(:)
            real(real64),allocatable :: x(:)
            integer(int32),intent(in) :: row_ptr(:),col_idx(:)

        end function
    end interface
    
    ! 外部ソルバの利用(ただし，subroutineのみ)
    x = LinearSolver(this%CRS_index_row,this%CRS_Index_Col,this%CRS_val,&
        this%CRS_RHS,x0)

    this%CRS_x = x

end function

! #####################################################


! #####################################################
function solveFEMSolver(this,algorithm,preconditioning,x0) result(x)
    class(FEMSolver_),intent(inout) :: this
    real(real64),allocatable :: x(:),dense_mat(:,:),fix_value(:)
    real(real64),optional,intent(in) :: x0(:)
    integer(int32) :: i,j, ElementID,col,row_ptr,col_row_fix
    logical,allocatable :: need_fix(:)
    character(*),optional,intent(in) :: algorithm, preconditioning

    type(IO_) :: f

    
    if(allocated(this%fix_lin_exists) )then
        need_fix = this%fix_lin_exists
        fix_value= this%fix_lin_exists_values
        
        do i=1,size(this%CRS_Index_Row)-1 !すべての行
            do col = this%CRS_index_row(i) ,this%CRS_index_row(i+1)-1
                
                if( need_fix( this%CRS_index_col(col) ) )then
                    this%CRS_RHS( i )  = this%CRS_RHS( i ) &
                            - this%CRS_val( col )*fix_value( this%CRS_index_col(col)) ! 移項
                endif
            enddo
        enddo
    endif

    if(allocated(this%fix_lin_exists) )then
        
        ! 右辺ベクトルに強制値を導入        
        ! for each boundary conditioned-node
        do i=1,size(this%CRS_RHS)
            if(this%fix_lin_exists(i) )then
                this%CRS_RHS(i) = this%fix_lin_exists_values(i)
            endif
        enddo

        do i=1,size(this%CRS_Index_row)-1
            do j=this%CRS_Index_row(i),this%CRS_Index_row(i+1)-1
                if( this%fix_lin_exists(i) )then
                    this%CRS_val(j) = 0.0d0
                endif

                if( this%fix_lin_exists( this%CRS_Index_Col(j) ) )then
                    this%CRS_val(j) = 0.0d0
                endif
            enddo
        enddo

    

        do i=1,size(this%CRS_Index_row)-1
            do j=this%CRS_Index_row(i),this%CRS_Index_row(i+1)-1
                if( this%fix_lin_exists(i) .and. &
                    this%CRS_Index_Col(j) == i ) then
                    this%CRS_val(j) = 1.0d0
                else
                    cycle
                endif
                
            enddo
        enddo

    endif

    

    if(this%debug)then
        print *, "[ok] b.c. loaded"
    endif

    if(present(x0) )then
        x = x0
    else
        x = zeros(size(this%CRS_RHS))
    endif

    if(present(preconditioning))then
        if(preconditioning=="PointJacobi")then

            if(this%debug)then
                print *, "PBiCGSTAB (PointJacobi)"
            endif
            call JacobiPreconditionerCRS(val=this%CRS_val,row_ptr=this%CRS_index_row,&
                col_idx=this%CRS_Index_col,rhs=this%CRS_RHS)
            
        elseif(preconditioning=="incompleteLU" .or. preconditioning=="ILU") then

            if(this%debug)then
                print *, "PBiCGSTAB (ILU(0))"
            endif
            call bicgstab_CRS_ILU(this%CRS_val, this%CRS_index_row, this%CRS_index_col,&
                    x, this%CRS_RHS, this%itrmax, this%er0,this%relative_er,this%debug,&
                    this%ILU_MATRIX)
            return
        else
            print *, "[Warning!] :: FEMSolver :: invalid preconditioning"
            stop
        endif
    endif


    if(associated(this%mpi_target) )then
        call this%MPI_BICGSTAB(x)
    else
        if(present(algorithm) )then
            if(algorithm=="GaussJordan") then
                call gauss_jordan_crs(this%CRS_val, this%CRS_index_row, this%CRS_index_col,&
                x, this%CRS_RHS, size(this%CRS_RHS) )
                return
            else
                call  bicgstab_CRS_2(this%CRS_val, this%CRS_index_row, this%CRS_index_col,&
                    x, this%CRS_RHS, this%itrmax, this%er0,this%relative_er,this%debug)
                return
            endif
        else
            call  bicgstab_CRS_2(this%CRS_val, this%CRS_index_row, this%CRS_index_col,&
                x, this%CRS_RHS, this%itrmax, this%er0, this%relative_er, this%debug)
            return
        endif
    endif   
    

end function
! #####################################################
subroutine removeFEMSolver(this,only_matrices)
    class(FEMSolver_),intent(inout) :: this
    logical,optional,intent(in) :: only_matrices

 
    if(allocated( this%CRS_val)) deallocate(this%CRS_val)!(:)
    if(allocated( this%CRS_Index_Col)) deallocate(this%CRS_Index_Col)!(:)
    if(allocated( this%CRS_Index_Row)) deallocate(this%CRS_Index_Row)!(:)
    if(allocated( this%CRS_RHS)) deallocate(this%CRS_RHS)!(:)
    
    !> General Eigen Value Problem
    !> [A]{x} = (lambda)[B]{x}

    if(allocated( this%A_CRS_val)) deallocate(this%A_CRS_val)!(:)
    if(allocated( this%A_CRS_Index_Col)) deallocate(this%A_CRS_Index_Col)!(:)
    if(allocated( this%A_CRS_Index_Row)) deallocate(this%A_CRS_Index_Row)!(:)
    this%A_empty = .true.

    if(allocated( this%B_CRS_val)) deallocate(this%B_CRS_val)!(:)
    if(allocated( this%B_CRS_Index_Col)) deallocate(this%B_CRS_Index_Col)!(:)
    if(allocated( this%B_CRS_Index_Row)) deallocate(this%B_CRS_Index_Row)!(:)
    this%B_empty = .true.

    if(present(only_matrices) )then
        if(only_matrices)then
            return
        endif
    endif
    if(allocated( this%femdomains)) deallocate(this%femdomains)!(:)  
    if(allocated( this%DomainIDs)) deallocate(this%DomainIDs)!(:)

    if(allocated( this%IfaceElemConnectivity)) deallocate(this%IfaceElemConnectivity)!(:,:)
    if(allocated( this%IfaceElemDomainID)) deallocate(this%IfaceElemDomainID)!(:,:)

    this%initialized = .false.
    this%InterfaceExist = .false.
    this%debug = .false.
   

    
    if(allocated( this%fix_eig_IDs)) deallocate(this%fix_eig_IDs)!(:)
    
    if(allocated( this%fix_lin_exists)) deallocate(this%fix_lin_exists)!(:)
    if(allocated( this%fix_lin_exists_Values)) deallocate(this%fix_lin_exists_Values)!(:)
    
    ! if(allocated( this%fix_lin_IDs)) deallocate(this%fix_lin_IDs)!(:)
    ! if(allocated( this%fix_lin_Values)) deallocate(this%fix_lin_Values)!(:)
    
    if(allocated( this%CRS_x)) deallocate(this%CRS_x)!(:)
    if(allocated( this%CRS_ID_Starts_From)) deallocate(this%CRS_ID_Starts_From)!(:)

    ! dense matrix
    if(allocated( this%A_dense)) deallocate(this%A_dense)!(:,:)
    if(allocated( this%Num_nodes_in_Domains)) deallocate(this%Num_nodes_in_Domains)!(:)

    ! with MPI
    if(associated(this%MPI_target)) nullify(this%MPI_target)
    if(allocated(this%Link_Table)) deallocate(this%Link_Table)

    
    this%LINK_TABLE_INIT_SIZE = 1000
    this%Link_num=0
    this%itrmax = 100000
    this%er0 = dble(1.0e-10)
    this%relative_er = dble(1.0e-10)
    
end subroutine
! #####################################################
subroutine setEbOMFEMSolver(this,penalty,DOF)
    class(FEMSolver_),intent(inout) :: this

    real(real64),intent(in) :: penalty
    integer(int32),intent(in) :: DOF
    integer(int32) :: DomainID,ElementID,GaussPointID,myDomainID, pairDomainID
    integer(int32) :: i,j,k,m,n,row_id,col_id,row_Domain_id,col_domain_id
    real(real64) :: singleValue
    real(real64),allocatable :: A_ij(:,:),position(:)
    type(ShapeFunction_) :: sf

    ! it was
    ! do DomainID=1,2
    do DomainID=1,size(this%FEMDomains)
        do i = 1, this%FEMDomains(DomainID)%femdomainp&
            %numOversetElements()
            
            if( this%FEMDomains(DomainID)%femdomainp&
                %OversetConnect(i)%active )then

                ElementID    = this%FEMDomains(DomainID)%femdomainp&
                %OversetConnect(i)%ElementID
                GaussPointID = this%FEMDomains(DomainID)%femdomainp&
                %OversetConnect(i)%GaussPointID
                
                position = this%FEMDomains(DomainID)%femdomainp&
                    %OversetConnect(i)%position
                
                myDomainID   = this%FEMDomains(DomainID)%femdomainp&
                    %OversetConnect(i)%DomainIDs12(1)
                n = size(this%FEMDomains(DomainID)%femdomainp&
                    %OversetConnect(i)%DomainIDs12)
                pairDomainID = this%FEMDomains(DomainID)%femdomainp&
                    %OversetConnect(i)%DomainIDs12( n )
                
                if(GaussPointID>0)then
                    ! GPP
                    
                    sf = this%FEMDomains(myDomainID)%femdomainp%mesh%getShapeFunction(ElementID,GaussPointID)
                
                    sf%ElementID=ElementID

                    A_ij = penalty*this%FEMDomains(pairDomainID)%femdomainp%connectMatrix(&
                        position=position,&
                        DOF=DOF,&
                        shapefunction=sf) 
                else
                    ! P2P
                    A_ij = penalty*this%FEMDomains(pairDomainID)%femdomainp&
                    %connectMatrix(position=position,DOF=this%FEMDomains(DomainID)%femdomainp&
                    %nd() ) 
                endif
            
                

                ! assemble them 
                do j=1,size(this%FEMDomains(DomainID)%femdomainp&
                    %OversetConnect(i)%interConnect)
                    do k=1,size(this%FEMDomains(DomainID)%femdomainp&
                        %OversetConnect(i)%interConnect)
                        do m=1,DOF
                            do n = 1, DOF
                        
                                row_Domain_id = this%FEMDomains(DomainID)%femdomainp&
                                %OversetConnect(i)%DomainIDs12(j)
                                col_Domain_id = this%FEMDomains(DomainID)%femdomainp&
                                %OversetConnect(i)%DomainIDs12(k)

                                row_id = &
                                    + this%FEMDomains(DomainID)%femdomainp&
                                    %OversetConnect(i)%interConnect(j)
                                col_id =  &
                                    + this%FEMDomains(DomainID)%femdomainp&
                                    %OversetConnect(i)%interConnect(k)

                                row_id = this%CRS_ID_Starts_From(row_Domain_id) -1 + (row_id-1)*DOF + m
                                col_id = this%CRS_ID_Starts_From(col_Domain_id) -1 + (col_id-1)*DOF + n 

                                singleValue = A_ij( (j-1)*DOF + m , (k-1)*DOF + n )

                                call this%addMatrixValue(&
                                    row_id = row_id, &
                                    col_id = col_id, &
                                    singleValue = singleValue &
                                    )
                            enddo
                        enddo
                    enddo
                enddo
                !call obj%solver%assemble(&
                !    connectivity=InterConnect,&
                !    DOF=domain2%nd() ,&
                !    eMatrix=A_ij,&
                !    DomainIDs=this%FEMDomains(DomainID)%OversetConnect(i)%DomainIDs12)    
                

                
                !call solver%setMatrix(DomainID=DomainID,ElementID=ElementID,DOF=3,&
                !    Matrix=this%FEMDomains(DomainID)%StiffnessMatrix(ElementID=ElementID,E=300000.0d0, v=0.330d0),&
                !    )
            endif
        enddo
    enddo
    

end subroutine


! #####################################################
subroutine bicgstab_CRS_2(a, ptr_i, index_j, x, b, itrmax, er, relative_er,debug)
    integer(int32), intent(inout) :: ptr_i(:),index_j(:), itrmax
    real(real64), intent(inout) :: a(:), b(:), er
    real(real64),optional,intent(in) :: relative_er
    real(real64), intent(inout) :: x(:)
    logical,optional,intent(in) :: debug
    logical :: speak = .false.
    integer(int32) itr,i,j,n
    real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr,re_er0
    real(real64),allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:),pa(:),ax(:)
    
    er0 = er
    if(present(debug) )then
        speak = debug
    endif
    
    n=size(b)
    if(speak) print *, "BiCGSTAB STARTED >> DOF:", n
    allocate(r(n), r0(n), p(n), y(n), e(n), v(n))

    r(:) = b(:)
    if(speak) print *, "BiCGSTAB >> [1] initialize"

    call sub_crs_matvec(CRS_value=a,CRS_col=index_j,&
        CRS_row_ptr=ptr_i,old_vector=x,new_vector=ax)
    r = b - ax

    
    if(speak) print *, "BiCGSTAB >> [2] dp1"

    c1 = dot_product(r,r)
    !call omp_dot_product(r,r,c1)
    
    init_rr=c1
    if(speak) print *, "BiCGSTAB >> [2] init_rr",c1
    !if(speak) print *, "BiCGSTAB >>      |r|^2 = ",init_rr
    
    !if (c1 < er0) return

    p(:) = r(:)
    r0(:) = r(:)
    

    do itr = 1, itrmax   
        if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] initialize"
        c1 = dot_product(r0,r)
        !call omp_dot_product(r0,r,c1)
        
        y(:) = 0.0d0
        call sub_crs_matvec(CRS_value=a,CRS_col=index_j,&
        CRS_row_ptr=ptr_i,old_vector=p,new_vector=y)

        c2 = dot_product(r0,y)
        !call omp_dot_product(r0,y,c2)
        
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = 0.0d0
        call sub_crs_matvec(CRS_value=a,CRS_col=index_j,&
        CRS_row_ptr=ptr_i,old_vector=e,new_vector=v)
        
        if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] half"
        
        
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        !call omp_dot_product(e,v,ev)
        !call omp_dot_product(v,v,vv)
        
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
        c3 = ev / vv
        if(speak) print *, "BiCGSTAB >> c3 = ev/vv",c3
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)

        rr = dot_product(r,r)
        !call omp_dot_product(r,r,rr)
        
        if(itr==1)then
            re_er0 = rr
        endif

        if(speak)then
            print *, sqrt(rr)
        endif
        
        if(present(relative_er) )then
            if(sqrt(rr/re_er0)<relative_er )then
                exit
            endif
        endif
        !    write(*,*) 'itr, er =', itr,rr
        if (sqrt(rr) < er0) exit
        
        c1 = dot_product(r0,r)
        !call omp_dot_product(r0,r,c1)
        

        bet = c1 / (c2 * c3)
        if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
    enddo
end subroutine 
!===============================================================

! #####################################################
subroutine MPI_BiCGSTABFEMSolver(this,x)
    class(FEMSolver_),intent(inout) :: this
    !integer(int32), intent(inout) :: ptr_i(:),index_j(:), itrmax
    !real(real64), intent(inout) :: a(:), b(:), er
    !real(real64),optional,intent(in) :: relative_er
    ! MPI
    !type(M_Link_Item_),intent(in) :: link_table(:)
    !type(MPI_) :: mpi_target

    real(real64), allocatable, intent(inout) :: x(:)
    
    logical :: speak = .false.
    integer(int32) itr,i,j,n
    real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr,re_er0
    real(real64),allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:),pa(:),ax(:)
    

    er0 = this%er0
    speak = this%debug
    if(.not.allocated(this%CRS_RHS) )then
        print *, "[ERROR] >> bicgstab_CRS_MPIFEMSolver :: detected .not.allocated(this%CRS_RHS)"
        stop
    endif
    n=size(this%CRS_RHS)
    if(speak) print *, "BiCGSTAB STARTED >> DOF:", n

    if(.not.allocated(x) )then
        x = zeros(n)
    endif

    allocate(r(n), r0(n), p(n), y(n), e(n), v(n))

    r(:) = this%CRS_RHS(:)

    if(speak) print *, "BiCGSTAB >> [1] initialize"

    ! matrix-vector multiplication
    !ax = crs_matvec(CRS_value=a,CRS_col=index_j,&
    !    CRS_row_ptr=ptr_i,old_vector=x)
    ax = this%MPI_matmul(b=x)
    r = this%CRS_RHS - ax
    
    
    if(speak) print *, "BiCGSTAB >> [2] dp1"
    
    ! dot_product
    c1 = this%mpi_dot_product(r,r)

    init_rr=c1
    !if(speak) print *, "BiCGSTAB >>      |r|^2 = ",init_rr
    
    if (c1 < er0) return

    p(:) = r(:)
    r0(:) = r(:)

    do itr = 1, this%itrmax 
        if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] initialize"
        c1 = this%mpi_dot_product(r0,r)
        
        !y = crs_matvec(CRS_value=a,CRS_col=index_j,&
        !CRS_row_ptr=ptr_i,old_vector=p)
        y = this%MPI_matmul(b=p)

        c2 = this%mpi_dot_product(r0,y)

        alp = c1/c2
        
        e(:) = r(:) - alp * y(:)
        
        !v = crs_matvec(CRS_value=a,CRS_col=index_j,&
        !CRS_row_ptr=ptr_i,old_vector=e)
        v = this%MPI_matmul(b=e)

        if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] half"
        
        
        ev = this%mpi_dot_product(e,v)
        vv = this%mpi_dot_product(v,v)
        
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
        
        c3 = ev / vv
        
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        
        r(:) = e(:) - c3 * v(:)
        
        rr = this%mpi_dot_product(r,r)
        
        if(itr==1)then
            re_er0 = rr
        endif

        if(speak)then
            print *, sqrt(rr)
        endif
        if(sqrt(rr/re_er0)<this%relative_er )then
            exit
        endif
        !    write(*,*) 'itr, er =', itr,rr
        if (sqrt(rr) < er0) exit
        c1 = this%mpi_dot_product(r0,r)

        
        bet = c1 / (c2 * c3)
        
        if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
        
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
    enddo
end subroutine 
!===============================================================




!===================================================================================
subroutine gauss_jordan_crs(a0_val, a0_row_ptr,a0_col_idx, x, b, n)
    integer(int32), intent(in) :: n
    real(real64), intent(in) :: a0_val(n), b(n)
    integer(int32),intent(in) :: a0_row_ptr(:), a0_col_idx(:)
    real(real64), intent(inout) :: x(n)
    real(real64) :: a_ik
    integer(int32) i, j, k, m,nn, mm


    integer(int32) :: i_1,i_2,i_3,i_4
    
    real(real64) ar, am, t, a_val(n), w(n)
    
    nn = size(b)
  
    ! Gauss-Jordan method with CRS format
    !a(:,:)= a0(:,:)
    a_val = a0_val

    
    !x(:) = b(:)
    x(:) = b(:)

    do k = 1, n
        m = k
       
        !am = abs(a(k,k))
        am = abs(getCRSval(a_val, a0_row_ptr, a0_col_idx, row=k, col=k))

        !do i = k+1, n
        !    if (abs(a(i,k)) > am) then
        !        am = abs(a(i,k))
        !        m = i
        !    endif
        !enddo


        !do i = k+1, n
        !    a_ik = abs(getCRSval(a_val, a0_row_ptr, a0_col_idx, row=i, col=k))
        !    if (abs(a(i,k)) > am) then
        !        am = abs( a_ik )
        !        m = i
        !    endif
        !enddo


        if (am == 0.0d0)   stop  ' A is singular '

        print *, "Under construction :: gauss_jordan_crs"
        stop
       
!        if ( k /= m) then
!            w(k:n) = a(k, k:n)
!            a(k,k:n) = a(m, k:n) ! [*] a_val and pointers are to be reallocated.
!            a(m, k:n) =w(k:n)    ! [*] a_val and pointers are to be reallocated.
!            t = x(k)
!            x(k) = x(m)
!            x(m) = t
!        endif
!        
!
!       ! gauss_jordan
!       if (a(k, k) == 0.0d0)  stop  'devide by zero3gauss_jordan_pv'
!          ar = 1.0d0 / a(k,k)
!       a(k,k) = 1.0d0
!       a(k,k+1:n) = ar * a(k, k+1:n) ! [*] a_val and pointers are to be reallocated.
!       x(k) = ar * x(k)
!       do i= 1, n
!         if (i /= k) then
!           a(i, k+1:n) = a(i, K+1:n) - a(i,k) * a(k, k+1:n) ! [*] a_val and pointers are to be reallocated.
!           x(i) = x(i) - a(i,k) * x(k)
!           a(i,k) = 0.0d0 ! [*] a_val and pointers are to be reallocated.
!         endif
!       enddo
    enddo
    
   end subroutine 
!===========================================================================
  
function getCRSval(val, row_ptr, col_idx, row, col) result(ret)
    real(real64),intent(in) :: val(:)
    integer(int32),intent(in) :: row_ptr(:), col_idx(:),row,col
    integer(int32) :: j, k
    real(real64) :: ret

    ret=0.0d0
    
    do j = row_ptr(row), row_ptr(row+1)-1
        if( col_idx(j)==col )then
            ret = val(j) 
        endif
    enddo

end function
!===========================================================================


!===========================================================================
subroutine JacobiPreconditionerCRS(val,row_ptr,col_idx,rhs)
    real(real64),intent(inout) :: val(:),rhs(:)
    integer(int32),intent(inout) :: row_ptr(:),col_idx(:)
    real(real64) :: A_k_k, A_k_k_inv
    integer(int32) :: i,j

    !!$OMP parallel
    !!$OMP do 
    do i=1,size(row_ptr) - 1    
        A_k_k = getCRSval(val=val, row_ptr=row_ptr, col_idx=col_idx, row=i, col=i)
        if(A_k_k ==0.0d0) cycle
        A_k_k_inv = 1.0d0/A_k_k
        rhs(i) = rhs(i) * A_k_k_inv
        do j=row_ptr(i), row_ptr(i+1)-1
            !!$OMP atomic
            val(j) = val(j) * A_k_k_inv
        enddo
    enddo
    !!$OMP end do
    !!$OMP end parallel
end subroutine
!===========================================================================

!===========================================================================
!subroutine incompleteLUCRS(val,row_ptr,col_idx,rhs)
!    real(real64),intent(inout) :: val(:),rhs(:)
!    integer(int32),intent(inout) :: row_ptr(:),col_idx(:)
!    real(real64),allocatable :: diag_vec(:)
!    real(real64) :: A_k_k, A_k_k_inv,A_i_k, A_k_j
!    integer(int32) :: i,j,n,m,k,col,col_id,col_id_2,col_2
!    type(CRS_) :: crs
!    print *, "incompleteLUCRS >> bug exists"
!    stop
!
!    ! ILU(0)
!    call crs%init(val=val,row_ptr=row_ptr,col_idx=col_idx)
!    call crs%ILU(0,rhs=rhs)



!    do row=2,n
!        do col_id = row_ptr(row),row_ptr(row+1)-1
!            col = col_idx(col_id)
!            if(1<= col .and. col <= row - 1)then
!                val(col_id) = val(col_id) / diag_vec(row) 
!                A_i_k = val(col_id)
!            endif
!
!            do col_id_2 = row_ptr(row),row_ptr(row+1)-1
!                col_2 = col_idx(col_id_2)
!                if(col + 1<= col_2 .and. col_2 <= row - 1)then
!                    val(col_id_2) = val(col_id_2) - A_i_k*A_k_j
!                endif
!            enddo
!        enddo
!    enddo

!    !$OMP parallel
!    !$OMP do 
!    do k=1,size(row_ptr) - 1    
!        A_k_k = getCRSval(val=val, row_ptr=row_ptr, col_idx=col_idx, row=k, col=k)
!        A_k_k_inv = 1.0d0/A_k_k
!        
!        do i=1,size(row_ptr) - 1    
!            do n=row_ptr(i), row_ptr(i+1)-1
!                j = col_idx(n)
!                ! a_ij := val(j)
!                if(j > k )then
!                    A_i_k=0.0d0
!                    do m=row_ptr(i), row_ptr(i+1)-1
!                        col = col_idx(m)
!                        if(col==k )then
!                            A_i_k = val(col)
!                        else
!                            cycle
!                        endif
!                    enddo
!                    if(A_i_k==0.0d0)then
!                        cycle
!                    endif
!                    !A_i_k = getCRSval(val=val, row_ptr=row_ptr, col_idx=col_idx, row=i, col=k)
!                    A_k_j = 0.0d0
!                    do m=row_ptr(k), row_ptr(k+1)-1
!                        col = col_idx(m)
!                        if(col==j )then
!                            A_k_j = val(col)
!                        else
!                            cycle
!                        endif
!                    enddo
!                    if(A_k_j==0.0d0)then
!                        cycle
!                    endif
!                    
!                    !A_k_j = getCRSval(val=val, row_ptr=row_ptr, col_idx=col_idx, row=k, col=j)
!                    !!$OMP atomic
!                    val(j) = val(j) - A_i_k*A_k_k_inv*A_k_j
!                    
!                endif
!            enddo
!        enddo
!    enddo
!    !$OMP end do
!    !$OMP end parallel
!end subroutine
!===========================================================================
!
!subroutine incompleteCholesky(val,row_ptr,col_idx,rhs)
!    real(real64),intent(inout) :: val(:),rhs(:)
!    integer(int32),intent(inout) :: row_ptr(:),col_idx(:)
!    real(real64) :: A_k_k, A_k_k_inv,A_i_k, A_k_j
!    integer(int32) :: i,j,n,m,k,col
!	
!    n = size(a,1);
!
!	do k=1,n
!		!a(k,k) = sqrt(a(k,k))
!        a_k_k  = getCRSval(val=val, row_ptr=row_ptr, col_idx=col_idx, row=k, col=k)
!        
!		do i=(k+1),n
!		    if (a(i,k)/=0)then
!		        a(i,k) = a(i,k)/a(k,k)            
!		    endif
!		enddo
!
!		do j=(k+1),n
!		    do i=j,n
!		        if (a(i,j)/=0)then
!		            a(i,j) = a(i,j)-a(i,k)*a(j,k)  
!		        endif
!		    enddo
!		enddo
!	enddo
!
!    do i=1,n
!        do j=i+1,n
!            a(i,j) = 0
!        enddo
!    enddo   
!
!end subroutine
!

subroutine reverseVectorReal64(A)
    real(real64),intent(inout) :: A(:)
    real(real64) :: buf
    integer(int32) :: i,n

    n = size(A)
    do i=1,n/2
        buf = A(i)
        A(i) = A(n-i+1)
        A(n-i+1) = buf
    enddo

end subroutine

subroutine reverseArrayReal64(A)
    real(real64),intent(inout) :: A(:,:)
    real(real64),allocatable :: buf(:)
    integer(int32) :: i,n

    ! see columns as vectors, and reverse order
    n = size(A,1)
    buf = zeros(n)
    do i=1,n
        call reverseVectorReal64(A(i,:))
    enddo
    
end subroutine

function sortByIDreal64ColisVector(vectors, ID) result(new_vectors)
    real(real64),intent(in) :: vectors(:,:)
    integer(int32),intent(in) :: ID(:)
    real(real64),allocatable :: new_vectors(:,:)
    integer(int32) :: i
    new_vectors = zeros( size(vectors,1),size(vectors,2) )
    do i=1,size(ID)
        new_vectors(:,i) = vectors(:,ID(i) )
    enddo

end function

pure function getCRSFEMSolver(this,name) result(ret)
    class(FEMSolver_),intent(in) :: this
    character(*),optional,intent(in) :: name
    type(CRS_) :: ret

    if(present(name) )then
        select case(name)
            case("A","a","K","StiffnessMatrix")
                ret%col_idx = this%A_CRS_Index_Col
                ret%row_ptr = this%A_CRS_Index_Row
                ret%val     = this%A_CRS_val
            
            case("B","b","M","MassMatrix")
                ret%col_idx = this%B_CRS_Index_Col
                ret%row_ptr = this%B_CRS_Index_Row
                ret%val     = this%B_CRS_val
        end select
    else
        ret%col_idx = this%CRS_Index_Col
        ret%row_ptr = this%CRS_Index_Row
        ret%val     = this%CRS_val
    endif
end function


subroutine MPI_linkFEMSolver(this,rank_and_rowID_1,rank_and_rowID_2)
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: rank_and_rowID_1(1:2),rank_and_rowID_2(1:2)
    type(M_Link_Item_),allocatable :: buf_table(:) 
    integer(int32) :: i

    ! if not allocated, allocate
    if(.not.allocated(this%Link_Table) )then
        allocate(this%Link_Table(this%LINK_TABLE_INIT_SIZE) )
        this%Link_num=0
    endif

    ! if not related, return
    if(rank_and_rowID_1(1)/=this%MPI_target%myrank &
        .and. rank_and_rowID_2(1)/=this%MPI_target%myrank )then
        return
    endif

    ! related. but table is full
    if(this%Link_num >=size(this%Link_Table) )then
        buf_table = this%Link_Table
        deallocate(this%Link_Table)
        allocate(this%Link_Table(2*size(buf_table) )  )
        do i=1,size(buf_table)
            this%Link_Table(i) = buf_table(i)
        enddo
    endif

    ! related and buffer has enough space
    this%Link_num = this%Link_num + 1

    if(rank_and_rowID_1(1)==this%mpi_target%myrank )then
        this%Link_Table(this%Link_num)%rank_and_rowID_1 = rank_and_rowID_1
        this%Link_Table(this%Link_num)%rank_and_rowID_2 = rank_and_rowID_2
    elseif(rank_and_rowID_2(1)==this%mpi_target%myrank )then
        this%Link_Table(this%Link_num)%rank_and_rowID_1 = rank_and_rowID_2
        this%Link_Table(this%Link_num)%rank_and_rowID_2 = rank_and_rowID_1
    else
        ! do nothing
        return
    endif

end subroutine

function MPI_dot_productFEMSolver(this,a, b) result(dp)
    class(FEMSolver_),intent(in) :: this
    real(real64),intent(in) :: a(:),b(:)
    integer(int32),allocatable :: num_link(:)
    integer(int32) :: i,j,n,m
    real(real64) :: dp, my_dp,sendobj(1),recvobj(1)

    if(.not. associated(this%MPI_target) )then
        print *, "ERROR ::MPI_dot_productFEMSolver >> please associate this%mpi_target "
        print *, "this%mpi_target => your_mpi_object"
        stop
    endif

    n = size(a)
    num_link = int( eyes(n) )
    if(allocated(this%Link_Table) )then
        do i=1,this%Link_num
            
            m = this%Link_Table(i)%rank_and_rowID_1(2)
            if(this%Link_Table(i)%rank_and_rowID_2(1) > this%MPI_target%petot-1 ) cycle
            if(m > n ) cycle
            num_link(m) = num_link(m) + 1
        enddo
    endif

    my_dp = 0.0d0
    !$OMP parallel 
    !$OMP do reduction(+:my_dp)
    do i=1,n
        my_dp = my_dp + a(i)*b(i)/dble(num_link(i) )
    enddo
    !$OMP end do
    !$OMP end parallel


    sendobj(1) = my_dp
    recvobj(1) = 0.0d0
    call this%MPI_target%AllReduce(sendobj=sendobj,recvobj=recvobj,count=1,sum=.true.)
    dp = recvobj(1)

end function

function MPI_matmulFEMSolver(this,A,b) result(my_c)
    class(FEMSolver_),intent(in) :: this
    type(CRS_),optional,intent(in) :: A
    real(real64),intent(in) :: b(:)
    real(real64),allocatable :: sendbuf(:),recvbuf(:),my_c(:)
    integer(int32),allocatable :: send_recv_rank(:)
    integer(int32) :: i,j,n,cor_rank,my_row_id

    if(.not. associated(this%MPI_target) )then
        print *, "ERROR ::MPI_matmulFEMSolver >> please associate this%mpi_target "
        print *, "this%mpi_target => your_mpi_object"
        stop
    endif

!    ! create sendbuf and recvbuf 
!    sendbuf  = zeros(this%Link_num)
!    send_recv_rank = int(zeros(this%Link_num))
!    do i=1,this%Link_num
!        my_row_id  = this%Link_Table(i)%rank_and_rowID_1(2)
!        cor_rank   = this%Link_Table(i)%rank_and_rowID_2(1)
!        sendbuf(i)  = b_copy(my_row_id) 
!        send_recv_rank(i) = cor_rank
!    enddo
!    recvbuf  = zeros(this%Link_num)
!    
!    ! MPI COMMUNICATION
!    ! ISEND :: >>> NON-BLOCKING
!    call this%mpi_target%isend_irecv(sendobj=sendbuf,recvobj=recvbuf,send_recv_rank=send_recv_rank)
!
!
!    do i=1,this%Link_num
!        my_row_id  = this%Link_Table(i)%rank_and_rowID_1(2)
!        cor_rank   = this%Link_Table(i)%rank_and_rowID_2(1)
!        b_copy(my_row_id) = b_copy(my_row_id) + recvbuf(i)  
!    enddo
    if(present(A) )then
        my_c = A%matmul(b)
    else
        my_c = zeros(size(b) )
        call sub_crs_matvec(CRS_value=this%CRS_val,CRS_col=this%CRS_Index_Col,&
            CRS_row_ptr=this%CRS_Index_Row,old_vector=b,new_vector=my_c)
    endif
    
    ! create sendbuf and recvbuf 

    sendbuf  = zeros(this%Link_num)
    send_recv_rank = int(zeros(this%Link_num))
    recvbuf  = zeros(this%Link_num)

    do i=1,this%Link_num
        my_row_id  = this%Link_Table(i)%rank_and_rowID_1(2)
        cor_rank   = this%Link_Table(i)%rank_and_rowID_2(1)
        sendbuf(i)  = my_c(my_row_id) 
        send_recv_rank(i) = cor_rank
    enddo
    recvbuf  = zeros(this%Link_num)


    call this%mpi_target%isend_irecv(sendobj=sendbuf,recvobj=recvbuf,send_recv_rank=send_recv_rank)
    
    do i=1,this%Link_num
        my_row_id  = this%Link_Table(i)%rank_and_rowID_1(2)
        cor_rank   = this%Link_Table(i)%rank_and_rowID_2(1)
        my_c(my_row_id) = my_c(my_row_id) + recvbuf(i)  
    enddo



end function

! ################################################
function conditionNumberFEMSolver(this) result(RCOND)
    class(FEMSolver_),intent(in) :: this
    integer(int32) :: N, LDA, INFO
    real(real64),allocatable :: A(:,:)
    real(real64) :: ANORM, RCOND
    real(real64),allocatable :: WORK(:)
    integer(int32),allocatable :: IWORK(:)
    character(1) :: NORM
    type(CRS_) :: crs

    N = size(this%CRS_Index_Row)-1
    LDA = N
    CRS = this%getCRS()
    A   = CRS%to_Dense()
    NORM = "1"
    ANORM = sqrt(dot_product(this%CRS_val,this%CRS_val))

    allocate(WORK(4*N),IWORK(N) )
    call dgecon(NORM,N,A,LDA,ANORM,RCOND,WORK,IWORK,INFO)

    if(INFO <0)then
        print *, "ERROR >> DEGCON failed."
    endif

end function
! ################################################



end module 