module FEMSolverClass
    !Linear soler for FEMDomainClass
    use FEMDomainClass
    implicit none


    type :: FEMSolver_
        type(FEMDomainp_),allocatable :: femdomains(:)  
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
        real(real64),allocatable :: CRS_ID_Starts_From(:)

        ! dense matrix
        real(real64),allocatable :: A_dense(:,:)
        integer(int32),allocatable :: Num_nodes_in_Domains(:)

        

        integer(int32) :: itrmax = 100000
        real(real64)   :: er0 = dble(1.0e-10)
    contains
        !(1) Initialize solver
        procedure,public ::  init => initFEMSolver

        !(2) set Domain info
        procedure,public ::  setDomain => setDomainFEMSolver
        procedure,public ::  setDomains=> setDomainFEMSolver

        !(3) setup Ax=b as CRS format
        procedure,public ::  setCRS  => setCRSFEMSolver

        !(4) set Ax=b as CRS format
        procedure,public ::  setValue  => setValueFEMSolver
        procedure,public ::  setValues => setValueFEMSolver

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


        
        
        !(7-1) Modal analysis
        procedure,public :: eig => eigFEMSolver

        !(7-2) linear solver
        procedure,public :: solve => solveFEMSolver

        !re-zero matrix
        procedure,public :: zeros => zerosFEMSolver
        ! M:diag matrix,  A*M^{-1}
        procedure,public :: matmulDiagMatrix => matmulDiagMatrixFEMSolver

    end type
contains

subroutine initFEMSolver(this,NumDomain,NumInterfaceElement,NumNodePerInterfaceElement)
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: numDomain,NumInterfaceElement
    integer(int32),optional,intent(in) :: NumNodePerInterfaceElement

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
    endif

    if(NumInterfaceElement==0)then
        this%InterfaceExist=.false.
    else
        ! check if NumNodePerInterfaceElement exists
        if(.not. present(NumNodePerInterfaceElement) )then
            print *, "ERROR :: NumNodePerInterfaceElement should be present."
            return
        endif
        this%InterfaceExist=.true.
        this%IfaceElemConnectivity = int(zeros(NumInterfaceElement,NumNodePerInterfaceElement))
        this%IfaceElemDomainID     = int(zeros(NumInterfaceElement,NumNodePerInterfaceElement))
    endif

    this%initialized = .true.

end subroutine


recursive subroutine setDomainFEMSolver(this,FEMDomain,FEMDomains,DomainID,DomainIDs)
    class(FEMSolver_),intent(inout) :: this
    type(FEMDomain_),target,optional,intent(in) :: FEMDomain,FEMDomains(:)
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

    if(.not. present(DomainID) .and. .not.present(DomainIDs) )then
        print *, "ERROR :: DomainID or DomainIDs are to be passed."
        stop
    endif

    if(.not. present(FEMDomain) .and. .not.present(FEMDomains) )then
        print *, "ERROR :: FEMDomain or FEMDomains are to be passed."
        stop
    endif

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
        if(size(FEMDomains) == size(DomainIDs) )then
            do i=1,size(DomainIDs)
                call this%setDomain(FEMDomain=FEMDomains(i),DomainID=DomainIDs(i) )
            enddo
            return
        endif
    endif

    print *, "ERROR >> setDomainFEMSolver >> invalid combinations for args"

end subroutine


subroutine setCRSFEMSolver(this,DOF)
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: DOF
    
    integer(int32) :: i,j,k,l
    integer(int32) :: size_of_global_matrix,offset,node_id,row_id
    integer(int32) :: node_id_p, col_id, kk, ll,drow_offset,buf_1
    integer(int32),allocatable :: Num_nodes_in_Domains(:),num_entry_in_row(:)
    integer(int32),allocatable :: col_local(:),new_col_local(:)


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
                this%CRS_ID_Starts_From(i) = this%CRS_ID_Starts_From(i-1) + this%FEMDomains(i)%femdomainp%nn()*DOF
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
                print *,this%Num_nodes_in_Domains
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
subroutine fixFEMSolver(this,DomainID,IDs,FixValue)
    class(FEMSolver_),intent(inout) :: this

    real(real64),intent(in) :: FixValue
    integer(int32),intent(in) :: DomainID
    integer(int32),intent(in) :: IDs(:)

    integer(int32),allocatable :: buf(:)
    integer(int32),allocatable :: buf_real(:)
    integer(int32) :: i

    ! fix unknowns for linear solvers
    ! fix IDs(:)-th amplitudes as zero (Fixed boundary)
    ! only create list
    if(.not.allocated(this%fix_lin_exists) )then
        allocate(this%fix_lin_exists( size(this%CRS_RHS) ) )
        this%fix_lin_exists_values = zeros( size(this%CRS_RHS) )
        this%fix_lin_exists(:) = .false.
        do i=1,size(IDs)
            this%fix_lin_exists( IDs(i)) = .true.
            this%fix_lin_exists_values(IDs(i)) = FixValue
        enddo
    else

        do i=1,size(IDs)
            this%fix_lin_exists( IDs(i) ) = .true.
            this%fix_lin_exists_values( IDs(i) ) = FixValue
        enddo
    endif
    
end subroutine

!
function diag(this) result(diag_vector)
    class(FEMSolver_),intent(in) :: this
    real(real64),allocatable :: diag_vector(:)
    integer(int32) :: row,col,id

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
subroutine saveMatrixFEMSolver(this,name,CRS_as_dense, if_dense_exists)
    class(FEMSolver_),intent(in) :: this
    character(*),intent(in) :: name
    logical,optional,intent(in) :: CRS_as_dense,if_dense_exists
    integer(int32) :: i,j,k,n
    real(real64),allocatable :: row_vector(:)
    type(IO_)::f

    if(present(if_dense_exists) )then
        if(if_dense_exists)then
            if(.not.allocated(this%A_dense) ) return
            call f%open(name+"_dense.csv","w")
            n = size(this%A_dense,1)
            print *, n
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
                    write(f%fh,'(A)',advance='no') str(row_vector(j) )+","
                enddo
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


    !>>>>>>>>>>>>>> INPUT
    integer(int32) :: ITYPE = 1   ! A*x = (lambda)*B*x
    character(1) :: JOBZ  = 'V' ! Compute eigenvalues and eigenvectors.
    character(1) :: UPLO  = 'U' ! Upper triangles of A and B are stored;
    !<<<<<<<<<<<<<< INPUT

    integer(int32) :: N ! order of matrix
    real(real64),allocatable :: AP(:)
    real(real64),allocatable :: BP(:)
    real(real64),allocatable :: W(:)
    real(real64),allocatable :: Z(:,:)
    real(real64),allocatable :: WORK(:)
    real(real64),allocatable,intent(inout) :: eigen_value(:)
    real(real64),allocatable,intent(inout) :: eigen_vectors(:,:)
    integer(int32),allocatable :: IWORK(:)
    integer(int32) :: LDZ
    integer(int32) :: LWORK
    integer(int32) :: LIWORK 
    integer(int32) :: INFO
    integer(int32) :: from,to,k,j,i
    integer(int32),allocatable :: new_id_from_old_id(:)
    real(real64),allocatable :: dense_mat(:,:)
    type(IO_) :: f

    if(allocated(this%fix_eig_IDs) )then
        ! amplitudes are zero@ this%fix_eig_IDs
        ! remove from problem [A][U] = w[B][U]
        ! sort before it
        
        
        ! first, for [A]
        call heapsort(n=size(this%fix_eig_IDs),array=this%fix_eig_IDs)
        call reduce_crs_matrix(CRS_val=this%A_CRS_val,CRS_col=this%A_CRS_index_col,&
        CRS_rowptr=this%A_CRS_index_row,remove_IDs=this%fix_eig_IDs)
        call reduce_crs_matrix(CRS_val=this%B_CRS_val,CRS_col=this%B_CRS_index_col,&
        CRS_rowptr=this%B_CRS_index_row,remove_IDs=this%fix_eig_IDs)
    endif


    !>>>>>>>>>>>>>> INPUT
    N      = size(this%A_CRS_index_row) -1 
    LDZ    = input(default=N,option=num_eigen)
    LWORK  = 1 + 6*N + 2*N**2
    LIWORK = 3 + 5*N
    !<<<<<<<<<<<<<< INPUT
    

    !>>>>>>>>>>>>>>  INPUT/OUTPUT
    AP = zeros(N*(N+1)/2 )
    BP = zeros(N*(N+1)/2 )
    ! Upper triangle matrix
    AP = UpperTriangularMatrix(CRS_val=this%A_CRS_val,CRS_col=this%A_CRS_index_col,&
        CRS_rowptr=this%A_CRS_index_row)
    BP = UpperTriangularMatrix(CRS_val=this%B_CRS_val,CRS_col=this%B_CRS_index_col,&
        CRS_rowptr=this%B_CRS_index_row)
    !<<<<<<<<<<<<<< INPUT/OUTPUT


    !>>>>>>>>>>>>>>  OUTPUT
    W     = zeros(N )
    Z     = zeros(LDZ,N)
    WORK  = zeros(LWORK)
    IWORK = zeros(LIWORK)
    INFO  = 0
    !<<<<<<<<<<<<<< OUTPUT

    
    call DSPGVD (ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
    LWORK, IWORK, LIWORK, INFO)

    eigen_value = w
    
    if(allocated(this%fix_eig_IDs) )then    
        ! U(this%fix_eig_IDs(i),: ) = 0.0d0
        
        new_id_from_old_id = zeros(N)
        
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

        
        eigen_vectors = zeros(size(Z,1)+size(this%fix_eig_IDs),size(Z,1) ) 
        do i=1,size(Z,2)
            do j=1,size(new_id_from_old_id,1)
                eigen_vectors( new_id_from_old_id(j) ,i) = Z( j  ,i)
            enddo
        enddo
        
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
    else
        buf = this%fix_eig_IDs
        this%fix_eig_IDs = zeros(size(buf) + size(IDs) )
        this%fix_eig_IDs(1:size(buf) ) = buf(:)
        this%fix_eig_IDs(size(buf)+1:) = IDs(:)
    endif

    this%fix_eig_IDs = unique(this%fix_eig_IDs)
    


end subroutine


! #####################################################
function solveFEMSolver(this) result(x)
    class(FEMSolver_),intent(inout) :: this
    real(real64),allocatable :: x(:),dense_mat(:,:),fix_value(:)
    integer(int32) :: i,j, ElementID,col,row_ptr,col_row_fix
    logical,allocatable :: need_fix(:)

    type(IO_) :: f

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
    x = zeros(size(this%CRS_RHS))
    
    call  bicgstab_CRS_2(this%CRS_val, this%CRS_index_row, this%CRS_index_col,&
        x, this%CRS_RHS, this%itrmax, this%er0,this%debug)
    

end function
! #####################################################


! #####################################################
subroutine bicgstab_CRS_2(a, ptr_i, index_j, x, b, itrmax, er, debug)
    integer(int32), intent(inout) :: ptr_i(:),index_j(:), itrmax
    real(real64), intent(inout) :: a(:), b(:), er
    real(real64), intent(inout) :: x(:)
    logical,optional,intent(in) :: debug
    logical :: speak = .false.
    integer(int32) itr,i,j,n
    real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
    real(real64),allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:),pa(:),ax(:)
  
    er0 = er
    if(present(debug) )then
      speak = debug
    endif
  
    if(speak) print *, "BiCGSTAB STARTED >> DOF:", n
    n=size(b)
    allocate(r(n), r0(n), p(n), y(n), e(n), v(n))
    
    r(:) = b(:)
    if(speak) print *, "BiCGSTAB >> [1] initialize"
    
    ax = crs_matvec(CRS_value=a,CRS_col=index_j,&
        CRS_row_ptr=ptr_i,old_vector=x)
    r = b - ax
    
  
    if(speak) print *, "BiCGSTAB >> [2] dp1"
    
    c1 = dot_product(r,r)
      
    init_rr=c1
    
    if (c1 < er0) return
    
    p(:) = r(:)
    r0(:) = r(:)
    
    do itr = 1, itrmax   
      if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] initialize"
      c1 = dot_product(r0,r)
      
        y = crs_matvec(CRS_value=a,CRS_col=index_j,&
        CRS_row_ptr=ptr_i,old_vector=p)
        
      c2 = dot_product(r0,y)
      alp = c1/c2
      e(:) = r(:) - alp * y(:)
      v = crs_matvec(CRS_value=a,CRS_col=index_j,&
      CRS_row_ptr=ptr_i,old_vector=e)
    
      if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] half"
      
      
      ev = dot_product(e,v)
      vv = dot_product(v,v)
  
      if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
          c3 = ev / vv
      x(:) = x(:) + alp * p(:) + c3 * e(:)
      r(:) = e(:) - c3 * v(:)
      rr = dot_product(r,r)
      
      if(speak)then
        print *, rr/init_rr
      endif
      
      !    write(*,*) 'itr, er =', itr,rr
      if (rr/init_rr < er0) exit
      c1 = dot_product(r0,r)
      bet = c1 / (c2 * c3)
          if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
      p(:) = r(:) + bet * (p(:) -c3*y(:) )
    enddo
   end subroutine 
  !===============================================================
  
  
end module 