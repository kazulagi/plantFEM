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
        
        real(real64),allocatable :: CRS_x(:)
        real(real64),allocatable :: CRS_ID_Starts_From(:)

        ! dense matrix
        real(real64),allocatable :: A_dense(:,:)
        integer(int32),allocatable :: Num_nodes_in_Domains(:)
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
                        !$OMP atomic
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
                !$OMP atomic
                this%CRS_RHS(Row_id) = this%CRS_RHS(Row_id) + Vector(eRow_id)
            enddo
        enddo
    endif

end subroutine
! ###################################################################
pure function RemoveIF(vector,equal_to) result(new_vector)
    integeR(int32),intent(in) :: vector(:),equal_to
    integer(int32),allocatable :: new_vector(:)
    integer(int32) :: i, num_remove,new_id

    num_remove=0
    do i=1,size(vector)
        if(vector(i)==equal_to )then
            num_remove=num_remove+1
        endif
    enddo
    new_vector = int(zeros(size(vector,1)-num_remove ))
    new_id=0
    do i=1,size(vector)
        if(vector(i)==equal_to )then
            cycle
        else
            new_id = new_id+1
            new_vector(new_id) = vector(i)
        endif
    enddo
end function
! ###################################################################

! ###################################################################
function RemoveOverwrap(vector) result(new_vector)
    integeR(int32),intent(in) :: vector(:)
    integer(int32),allocatable :: new_vector(:),buf(:)
    integer(int32) :: i,j,null_flag,new_size,new_id

    buf = vector
    
    call heapsort(n=size(buf),array=buf)
    
    null_flag = minval(buf)-1
    do i=1,size(buf)-1
        if(buf(i)==null_flag ) cycle
        do j=i+1,size(buf)
            if(buf(j)==null_flag ) cycle
            ! if same, set null_flag
            if(buf(i)==buf(j) )then
                buf(j)=null_flag
            endif
        enddo
    enddo
    
    ! remove null_flaged values
    new_size = 0
    do i=1,size(buf)
        if(buf(i)/=null_flag )then
            new_size = new_size + 1
        endif
    enddo
    new_vector = int(zeros(new_size) )
    new_id = 0
    do i=1,size(buf)
        if(buf(i)/=null_flag )then
            new_id = new_id + 1
            new_vector(new_id) = buf(i)
        endif
    enddo

end function
! ###################################################################
subroutine fixFEMSolver(this,DomainID,NodeIDs,DOFs,to)
    class(FEMSolver_),intent(inout) :: this
    integer(int32),intent(in) :: DomainID,NodeIDs(:),DOFs(:)
    real(real64),intent(in) :: to

    !
end subroutine

function LOBPCG_sparse(A_val,A_col,A_rowptr,lambda_min,tolerance) result(eigen_vectors)
    real(real64),intent(in) :: A_val(:)
    integer(int32),intent(in)::A_col(:),A_rowptr(:)
    real(real64),allocatable :: eigen_vectors(:,:)
    real(real64),allocatable :: V(:,:),A_(:,:),xtemp(:,:),&
        lambda_and_x(:,:),lambda(:),bmin(:),x(:,:),r(:,:),x0s(:,:),p(:,:),w(:),&
        alpha,beta,gamma,XX(:),WW(:),PP(:),SA(:,:),SB(:,:),w_x_p(:,:),WW_XX_PP(:,:),&
        SB_inv(:,:),x_(:,:),lambda_mat(:,:),lambda_ids(:),Bm(:,:),residual(:),norms(:)
    integer(int32) :: num_eigen
    real(real64),optional,intent(in) :: tolerance 
    real(real64) :: tol=dble(1.0e-14)
    real(real64) :: mu,normval
    real(real64),intent(inout) :: lambda_min(:)
    type(Random_) :: random
    integer(int32) :: i,j,n,id,itr
    integer(int32) :: m

    tol = input(default=dble(1.0e-14),option=tolerance)
    num_eigen = size(lambda_min,1)
    n = size(A_rowptr,1) -1

    if(num_eigen*3 >= n)then
        print *, "ERROR :: num_eigen*3 should be < n"
        allocate(eigen_vectors(0,0))
        return
    endif
    !m = 3*num_eigen
    residual = zeros(num_eigen)
    !https://www.researchgate.net/figure/Algorithm-of-LOBPCG-method-for-matrix-A-Here-the-matrix-T-is-a-preconditioner_fig1_323863889
   !    !x0s = random%randn(n,3)
    !initial guess x0

    lambda_min = 0.0d0

    
    x = zeros(n,num_eigen)
    x = random%randn(n,num_eigen)
    

    x = x * 10.0d0
    do i=1,size(x,1)
        do j=1,size(x,2)
            if(abs(x(i,j) )<1.0d0 )then
                x(i,j) =x(i,j) / abs(x(i,j) )  
            endif
        enddo
    enddo
    itr=0
    
    do 
        itr=itr+1
        
        if(itr==1)then

            ! step 1 to make initial values
            if(num_eigen/=1)then
                xtemp = X
                call gram_real(n,num_eigen,Xtemp,X)
                
                ! [ok] X(:,:) :: ok!
                A_ = matmul(transpose(X),crs_matmul(A_val,A_col,A_rowptr,X))
                
                A_ = 0.50d0*A_ + 0.50d0*transpose(A_)

                ! [ok] A_(:,:) has correct size
                call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=tol*dble(1.0e-3) ) 
                
                
                lambda_mat = zeros(num_eigen,num_eigen)
                lambda_mat(:,:) = 0.0d0
                do i=1,size(lambda_mat,1)
                    lambda_mat(i,i) = lambda(i)
                enddo
                ! [ok] lambda_mat 

                ! [ok] A_ X = X \Lambda

                ! X1 = X0 B
                x = matmul(x,x_)
                ![ok] size and content of R 
                R = crs_matmul(A_val,A_col,A_rowptr,X) - matmul(X,lambda_mat)
            else
                mu = dot_product(x(:,1),crs_matvec(A_val,A_col,A_rowptr,x(:,1) ))/dot_product(x(:,1),x(:,1) )
                R = crs_matmul(A_val,A_col,A_rowptr,X) - mu*X
            endif

            ! 2m次元固有値問題
            V = zeros(n,2*num_eigen)
            V(:,1:num_eigen) = X
            V(:,num_eigen+1:) = R
            ! [ok]直交化
            xtemp = V
            call gram_real(n,2*num_eigen,xtemp,V)
            X=V(:,1:num_eigen)  
            R=V(:,num_eigen+1:) 
            

            A_ = matmul(transpose( V ),crs_matmul(A_val,A_col,A_rowptr,V ))
            ! try
                
            A_ = 0.50d0*A_ + 0.50d0*transpose(A_)
            ![ok] size(A_,1) & size(A_,2)

            !2m 次元固有値問題
            
            call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=dble(1.0e-14)) 
            
            ![ok] 下から m 個の固有値と固有ベクトルからなる行列:
            lambda_ids = linspace([1.0d0,dble(2*num_eigen) ], 2*num_eigen)
            call heapsort(n=2*num_eigen,array=lambda,val=lambda_ids)
            ![ok] sort
            
            lambda_mat = zeros(num_eigen,num_eigen)
            do i=1,num_eigen
                lambda_mat(i,i) = lambda(i)
            enddo
            
            ! [ok] check eigen values
            
            Bm = zeros(2*num_eigen,num_eigen)
            do i=1,num_eigen
                Bm(:,i) = x_(:, int(lambda_ids(i)) )
            enddo
            ! X2, R2, P2
            ! V = {X, R}
            X = matmul(V,Bm)

            !V = zeros(n,num_eigen*2 )
            V(:,1:num_eigen) =0.0d0
            
            V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            ! R=R1 orR=R2?
            ! 今はR1と仮定
            P = matmul(V,Bm)

            R = crs_matmul(A_val,A_col,A_rowptr, X) - matmul(X, lambda_mat)
            
            !V(:,1:num_eigen) =0.0d0
            !V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            ! R=R1 orR=R2?
            ! 今はR2と仮定
            !P = matmul(V,Bm)
            
            do i=1,num_eigen
                Residual(i) = norm(R(:,i))
            enddo
            call print("residual: " + str(maxval(Residual) ))
            cycle
            
        else
            V  = zeros(n,3*num_eigen) 
            
            V(:,1:num_eigen)  = x(:,:)
            V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            V(:,2*num_eigen+1:)  = p(:,:)
            


            xtemp = V
            call gram_real(n,3*num_eigen,xtemp,V)

            x(:,:) = V(:,1:num_eigen)              
            r(:,:) = V(:,num_eigen+1:2*num_eigen)  
            p(:,:) = V(:,2*num_eigen+1:)           
            

            A_ = matmul(transpose(V),crs_matmul(A_val,A_col,A_rowptr,V) )! try
            
            A_ = 0.50d0*A_ + 0.50d0*transpose(A_)
            
            x_ = zeros(3*num_eigen,3*num_eigen)
            
            call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=dble(1.0e-14)) 
            
            
            lambda_ids = linspace([1.0d0,dble(3*num_eigen) ],3*num_eigen)
            lambda_mat = zeros(num_eigen,num_eigen)

            call heapsort(n=3*num_eigen,array=lambda,val=lambda_ids)
            ![ok] sort of lambda
            
            do i=1,num_eigen
                lambda_mat(i,i) = lambda( i )
            enddo
            
            Bm = zeros(3*num_eigen,num_eigen)
            do i=1,num_eigen
                Bm(:,i) = x_(:, int(lambda_ids(i)) )
            enddo

            
            X = matmul(V,Bm)
            
            
            !V = zeros(n,3*num_eigen )
            V(:,1:num_eigen) = 0.0d0
            P = matmul(V,Bm)
            !V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            !V(:,2*num_eigen+1:)  = p(:,:)

            R = crs_matmul(A_val,A_col,A_rowptr, X) - matmul(X, Lambda_mat)
            
            do i=1,num_eigen
                Residual(i) = norm(R(:,i))
            enddo
            

            print *, "info: ",itr,maxval(residual),minval(residual)
            
            
            
            
            if(maxval(Residual) < tol)then
                exit
            endif
            
        endif
        !あとは収束判定
        if(itr==20000)then
            print *, "ERROR :: LOBPCG >> did not converge!"
            exit
        endif
    enddo
    print *, "residual:",maxval(residual)
    print *, "itr=",itr
    
   
    do i=1,num_eigen
        lambda_min(i) = lambda( i )
    enddo
    eigen_vectors = x



end function
!! ###################################################################


! ###################################################################
function LOBPCG_dense(A,B,lambda_min) result(eigen_vectors)
    real(real64),intent(in) :: A(:,:),B(:,:)
    real(real64),allocatable :: eigen_vectors(:,:)
    real(real64),allocatable :: V(:,:),A_(:,:),xtemp(:,:),&
        lambda_and_x(:,:),lambda(:),bmin(:),x(:,:),r(:,:),x0s(:,:),p(:,:),w(:),&
        alpha,beta,gamma,XX(:),WW(:),PP(:),SA(:,:),SB(:,:),w_x_p(:,:),WW_XX_PP(:,:),&
        SB_inv(:,:),x_(:,:),lambda_mat(:,:),lambda_ids(:),Bm(:,:),residual(:),norms(:)
    integer(int32) :: num_eigen

    real(real64) :: tol=dble(1.0e-14)
    real(real64) :: mu,normval
    real(real64),intent(inout) :: lambda_min(:)
    type(Random_) :: random
    integer(int32) :: i,j,n,id,itr
    integer(int32) :: m


    num_eigen = size(lambda_min,1)
    n = size(A,1)

    if(num_eigen*3 >= n)then
        print *, "ERROR :: num_eigen*3 should be < n"
        allocate(eigen_vectors(0,0))
        return
    endif
    !m = 3*num_eigen
    residual = zeros(num_eigen)
    !https://www.researchgate.net/figure/Algorithm-of-LOBPCG-method-for-matrix-A-Here-the-matrix-T-is-a-preconditioner_fig1_323863889
   !    !x0s = random%randn(n,3)
    !initial guess x0

    lambda_min = 0.0d0

    n = size(A,1)
    
    x = zeros(n,num_eigen)
    x = random%randn(n,num_eigen)
    

    x = x * 10.0d0
    do i=1,size(x,1)
        do j=1,size(x,2)
            if(abs(x(i,j) )<1.0d0 )then
                x(i,j) =x(i,j) / abs(x(i,j) )  
            endif
        enddo
    enddo
    itr=0
    
    do 
        itr=itr+1
        
        if(itr==1)then

            ! step 1 to make initial values
            if(num_eigen/=1)then
                xtemp = X
                call gram_real(n,num_eigen,Xtemp,X)
                
                ! [ok] X(:,:) :: ok!
                A_ = matmul(transpose(X),matmul(A,X))
                
                A_ = 0.50d0*A_ + 0.50d0*transpose(A_)

                ! [ok] A_(:,:) has correct size
                call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=tol) 
                
                
                lambda_mat = zeros(num_eigen,num_eigen)
                lambda_mat(:,:) = 0.0d0
                do i=1,size(lambda_mat,1)
                    lambda_mat(i,i) = lambda(i)
                enddo
                ! [ok] lambda_mat 

                ! [ok] A_ X = X \Lambda

                ! X1 = X0 B
                x = matmul(x,x_)
                ![ok] size and content of R 
                R = matmul(A,X) - matmul(X,lambda_mat)
            else
                mu = dot_product(x(:,1),matmul(A(:,:),x(:,1) ))/dot_product(x(:,1),x(:,1) )
                R = matmul(A,X) - mu*X
            endif

            ! 2m次元固有値問題
            V = zeros(n,2*num_eigen)
            V(:,1:num_eigen) = X
            V(:,num_eigen+1:) = R
            ! [ok]直交化
            xtemp = V
            call gram_real(n,2*num_eigen,xtemp,V)
            X=V(:,1:num_eigen)  
            R=V(:,num_eigen+1:) 
            

            A_ = matmul(transpose( V ),matmul(A,V ))
            ! try
                
            A_ = 0.50d0*A_ + 0.50d0*transpose(A_)
            ![ok] size(A_,1) & size(A_,2)

            !2m 次元固有値問題
            
            call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=tol) 
            
            ![ok] 下から m 個の固有値と固有ベクトルからなる行列:
            lambda_ids = linspace([1.0d0,dble(2*num_eigen) ], 2*num_eigen)
            call heapsort(n=2*num_eigen,array=lambda,val=lambda_ids)
            ![ok] sort
            
            lambda_mat = zeros(num_eigen,num_eigen)
            do i=1,num_eigen
                lambda_mat(i,i) = lambda(i)
            enddo
            
            ! [ok] check eigen values
            
            Bm = zeros(2*num_eigen,num_eigen)
            do i=1,num_eigen
                Bm(:,i) = x_(:, int(lambda_ids(i)) )
            enddo
            ! X2, R2, P2
            ! V = {X, R}
            X = matmul(V,Bm)

            !V = zeros(n,num_eigen*2 )
            V(:,1:num_eigen) =0.0d0
            
            V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            ! R=R1 orR=R2?
            ! 今はR1と仮定
            P = matmul(V,Bm)

            R = matmul(A, X) - matmul(X, lambda_mat)
            
            !V(:,1:num_eigen) =0.0d0
            !V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            ! R=R1 orR=R2?
            ! 今はR2と仮定
            !P = matmul(V,Bm)
            
            do i=1,num_eigen
                Residual(i) = norm(R(:,i))
            enddo
            call print("residual: " + str(maxval(Residual) ))
            cycle
            
        else
            V  = zeros(n,3*num_eigen) 
            
            V(:,1:num_eigen)  = x(:,:)
            V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            V(:,2*num_eigen+1:)  = p(:,:)
            


            xtemp = V
            call gram_real(n,3*num_eigen,xtemp,V)

            x(:,:) = V(:,1:num_eigen)              
            r(:,:) = V(:,num_eigen+1:2*num_eigen)  
            p(:,:) = V(:,2*num_eigen+1:)           
            

            A_ = matmul(transpose(V),matmul(A,V) )! try
            
            A_ = 0.50d0*A_ + 0.50d0*transpose(A_)
            
            x_ = zeros(3*num_eigen,3*num_eigen)
            
            call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=tol) 
            
            
            lambda_ids = linspace([1.0d0,dble(3*num_eigen) ],3*num_eigen)
            lambda_mat = zeros(num_eigen,num_eigen)

            call heapsort(n=3*num_eigen,array=lambda,val=lambda_ids)
            ![ok] sort of lambda
            
            do i=1,num_eigen
                lambda_mat(i,i) = lambda( i )
            enddo
            
            Bm = zeros(3*num_eigen,num_eigen)
            do i=1,num_eigen
                Bm(:,i) = x_(:, int(lambda_ids(i)) )
            enddo

            
            X = matmul(V,Bm)
            
            
            !V = zeros(n,3*num_eigen )
            V(:,1:num_eigen) = 0.0d0
            P = matmul(V,Bm)
            !V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            !V(:,2*num_eigen+1:)  = p(:,:)

            R = matmul(A, X) - matmul(X, Lambda_mat)
            
            do i=1,num_eigen
                Residual(i) = norm(R(:,i))
            enddo
            
            
            
            
            if(maxval(Residual) < tol)then
                exit
            endif
            
        endif
        !あとは収束判定
        if(itr==20000)then
            print *, "ERROR :: LOBPCG >> did not converge!"
            exit
        endif
    enddo
    print *, "residual:",maxval(residual)
    print *, "itr=",itr
    
   
    do i=1,num_eigen
        lambda_min(i) = lambda( i )
    enddo
    eigen_vectors = x



end function
!! ###################################################################

function LOBPCG_dense_single(A,B,lambda_min) result(eigen_vector)
    real(real64),intent(in) :: A(:,:),B(:,:)
    real(real64),allocatable :: eigen_vector(:)
    real(real64),allocatable :: x0(:),r0(:),p0(:),V(:,:),A_(:,:),xtemp(:,:),&
        lambda_and_x(:,:),lambda(:),bmin(:),x(:),r(:),x0s(:,:),p(:),w(:),&
        alpha,beta,gamma,XX(:),WW(:),PP(:),SA(:,:),SB(:,:),w_x_p(:,:),WW_XX_PP(:,:),&
        SB_inv(:,:),x_(:,:)
    real(real64) :: tol=dble(1.0e-10)
    real(real64) :: mu
    real(real64),intent(inout) :: lambda_min
    type(Random_) :: random
    integer(int32) :: i,j,n,id,itr
    integer(int32) :: m=3

    !https://www.researchgate.net/figure/Algorithm-of-LOBPCG-method-for-matrix-A-Here-the-matrix-T-is-a-preconditioner_fig1_323863889
   !    !x0s = random%randn(n,3)
    !initial guess x0
    n = size(A,1)

    lambda_min = 0.0d0

    n = size(A,1)
    xtemp = zeros(n,3)
    
    eigen_vector = zeros(n)
    !xtemp = x0s
    x = zeros(n)
    do i=1,n
        x(i) = 1.0d0
    enddo
    x = random%randn(n)
    
    
    x = x/norm(x)
    mu = dot_product(x,matmul(A,x ))/dot_product(x,x)
    

    r = matmul(A,x ) - mu*x
    !r = r/norm(r)
    p = zeros(n)

    V  = zeros(n,3 )
    V(:,1) = x(:)
    V(:,2) = r(:)
    V(:,3) = p(:)
    xtemp = V

    call gram_real(n,2,xtemp,v)
    x(:) = V(:,1) 
    r(:) = V(:,2) 
    
    V  = zeros(n,3 )
    V(:,1) = x(:)
    V(:,2) = r(:)
    V(:,3) = p(:)
    !

    itr=0
    do 
        itr=itr+1
        
        A_ = matmul(transpose(V),matmul(A,V) )
        A_ = 0.50d0*A_ + 0.50d0*transpose(A_)
        
        !lambda = eigenValue(A_,tol=tol)
        
        call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=tol) 

        ! 固有ベクトルを正規化
        
        do i=1,size(x_,2)
            x_(:,i) = x_(:,i)/norm(x_(:,i))
        enddo
        if(itr==1)then
            id = minvalID(lambda(1:2) )
        else
            id = minvalID(lambda(1:3) )
        endif
        print *, "---check---"
        
        ! 最小固有ベクトル
        bmin = x_(:,id)
        
        x = matmul(V,bmin)

        mu = lambda(id)
        r  = matmul(A,x) - mu*x(:)
        
        
        p = zeros(n)
        p(:) = V(:,2)*bmin(2) + V(:,3)*bmin(3)
        

        if(norm(r) <tol .and. norm(x)/=0.0d0)exit


        print *, "error",norm(r)
        
        ! 直交化
        V = zeros(n,3)
        V(:,1) = x(:)
        V(:,2) = r(:)
        V(:,3) = p(:)
        call gram_real(n,3,V,xtemp)
        x(:) = xtemp(:,1) 
        r(:) = xtemp(:,2) 
        p(:) = xtemp(:,3) 
        V(:,1) = x(:)
        V(:,2) = r(:)
        V(:,3) = p(:)
        
        !print *, dot_product(x,r)
        !print *, dot_product(r,p)
        !print *, dot_product(x,p)

        !x = x/norm(x)
        !r = r/norm(r)
        !p = p/norm(p)
        

        !call print(xtemp)
        !stop
        if(itr==10000)then
            print *, "ERROR :: LOBPCG >> did not converge!"
            exit
        endif
    enddo
    print *, "residual:",norm(r)
    print *, "itr=",itr
    
    lambda_min = lambda(id)
    eigen_vector = x



end function
!! ###################################################################
subroutine gram_real(m,n,mat_v,mat_v_out)
    ! cited from 
    !http://park.itc.u-tokyo.ac.jp/kato-yusuke-lab/nagai/note_141009_LOBPCG.pdf
    implicit none
    ! m : ベクトルの次元
    ! n : ベクトルの本数
    integer,intent(in)::m,n
    real(8),intent(in)::mat_v(1:m,1:n)
    real(8),intent(out)::mat_v_out(1:m,1:n)
    integer::i,j
    real(8)::v(1:m),nai,vi(1:m),viold(1:m)
    real(8)::norm
    mat_v_out = mat_v
    do i = 1,n
        viold = mat_v_out(:,i)
        do j = 1,i-1
            nai = dot_product(mat_v_out(1:m,j),viold(1:m))
            vi = viold - nai*mat_v_out(:,j)
            viold = vi
        end do
        norm = sqrt(dble(dot_product(viold,viold)))
        if(norm==0.0d0)then
            ! DEBUG Right?
            print *, "ERROR gram_real :: norm-zero"
            mat_v_out(j,j) =1.0d0
            !
            !stop
            
            !mat_v_out(:,j) = viold
        else
            mat_v_out(:,j) = viold/norm
        endif
    end do
    return
end subroutine gram_real

!function LOBPCG(A,B,num_eigens,err) result(eigens)
!    type(FEMSolver_),intent(in) :: A, B
!    complex(real64),allocatable :: eigens(:,:)
!    integer(int32),intent(in) :: num_eigens
!    real(real64),optional,intent(in) :: err
!    real(real64) :: error_tolerance
!    integer(int32) :: i,j,m,nx,nv
!    
!    ! Locally Optimal Block Preconditioned Conjugate Gradient (LOBPCG) 
!    ! https://arxiv.org/pdf/1704.07458.pdf
!
!
!    if(.not.allocated(A%CRS_val) )then
!        print *, "[ERROR] eigFEMSolver >> (.not.allocated(this%CRS_val) )"
!        return
!    endif
!
!
!    error_tolerance = input(default=dble(1.0e-14),option=err)
!
!    m = size(A%CRS_Index_Row)-1
!    nx= num_eigens
!    allocate(eigens(0:num_eigens,1:m))
!    !eigens(0,1:m) :: eigen values
!    !eigens(1:nx,1:m) :: eigen vectors from 1st to nx-th
!    eigens(:,:) = 0.0d0
!
!
!
!
!
!end function
! ###################################################################

!subroutine RayleighRitz(S,C,Theta)
!    complex(real64),intent(in) :: S(:,:)
!    complex(real64),allocatable,intent(out) :: C(:,:),Theta(:,:)
!    complex(real64),allocatable :: D(:,:)
!
!    !Rayleigh-Ritz procedure
!    D = matmul( transpose(S), )
!
!end subroutine
subroutine to_Dense(CRS_val,CRS_col,CRS_rowptr,DenseMatrix)

    real(real64),allocatable,intent(inout) :: DenseMatrix(:,:)
    real(real64),allocatable,intent(in) :: CRS_val(:)
    integer(int32),allocatable,intent(in) :: CRS_col(:),CRS_rowptr(:)

    integer(int32) :: nonzero_count,i,j,k,n

    n = size(CRS_rowptr) -1
    
    allocate(DenseMatrix(n,n) )
    DenseMatrix(:,:) = 0.0d0

    do i=1,size(CRS_rowptr)-1
        do j=CRS_rowptr(i) ,CRS_rowptr(i+1)-1
            
            DenseMatrix(  i ,CRS_col(j) ) = CRS_val(j)
        enddo
    enddo
end subroutine

subroutine to_CRS(DenseMatrix,CRS_val,CRS_col,CRS_rowptr) 
    real(real64),intent(in) :: DenseMatrix(:,:)
    real(real64),allocatable,intent(inout) :: CRS_val(:)
    integer(int32),allocatable,intent(inout) :: CRS_col(:),CRS_rowptr(:)

    integer(int32) :: nonzero_count,i,j,k,n

    nonzero_count = 0
    do i=1,size(DenseMatrix,1)
        do j=1,size(DenseMatrix,2)
            if(DenseMatrix(i,j)/=0.0d0 ) then
                nonzero_count = nonzero_count + 1
            endif
        enddo
    enddo
    CRS_val = zeros(nonzero_count)
    CRS_col = int(linspace([1.0d0,dble(nonzero_count)],nonzero_count))
    CRS_rowptr = int(zeros(size(DenseMatrix,1)+1) )
    
    nonzero_count = 0
    do i=1,size(DenseMatrix,1)
        do j=1,size(DenseMatrix,2)
            if(DenseMatrix(i,j)/=0.0d0 ) then
                nonzero_count = nonzero_count + 1
                CRS_val(nonzero_count) = DenseMatrix(i,j)
                CRS_col(nonzero_count) = j
                CRS_rowptr(i) = CRS_rowptr(i) + 1
            endif
        enddo
    enddo
    
    n = CRS_rowptr(1) 
    !CRS_rowptr(1) =0
    do i=1,size(CRS_rowptr)-1
        CRS_rowptr(i+1) = CRS_rowptr(i+1) + CRS_rowptr(i) 
    enddo
    do i=size(CRS_rowptr)-1,1,-1
        CRS_rowptr(i+1) = CRS_rowptr(i) 
    enddo
    do i=size(CRS_rowptr),1,-1
        CRS_rowptr(i) = CRS_rowptr(i) +1
    enddo
    CRS_rowptr(1) = 1
    
    
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


!
function crs_matmul(CRS_value,CRS_col,CRS_row_ptr,old_vectors) result(new_vectors)
    real(real64),intent(in)  :: CRS_value(:),Old_vectors(:,:)
    integeR(int32),intent(in):: CRS_col(:),CRS_row_ptr(:)

    real(real64),allocatable :: new_vectors(:,:)
    
    
    integer(int32) :: i, j, n,gid,lid,row,CRS_id,col,m
    !> x_i = A_ij b_j


    n = size(CRS_row_ptr)-1
    m = size(old_vectors,2)
    if(size(old_vectors,1)/=n )then
        print *, "ERROR crs_matmul :: inconsistent size for old_vectors"
        return
    endif

    new_vectors = zeros(n,m) 
    !!$OMP parallel do default(shared) private(CRS_id,col)
    do row=1,n
        do CRS_id=CRS_row_ptr(row),CRS_row_ptr(row+1)-1
            col = CRS_col(CRS_id)
            do j=1,m
                !!$OMP atomic
                new_vectors(row,j) = new_vectors(row,j) &
                    + CRS_value(CRS_id)*old_vectors(col,j)
            enddo
        enddo
    enddo
    !!$OMP end parallel do 
    
end function
! ###################################################################

function crs_matvec(CRS_value,CRS_col,CRS_row_ptr,old_vector) result(new_vector)
    real(real64),intent(in)  :: CRS_value(:),Old_vector(:)
    integeR(int32),intent(in):: CRS_col(:),CRS_row_ptr(:)

    real(real64),allocatable :: new_vector(:)
    integer(int32) :: i, j, n,gid,lid,row,CRS_id,col
    !> x_i = A_ij b_j


    n = size(CRS_row_ptr)-1
    if(size(old_vector)/=n )then
        print *, "ERROR crs_matvec :: inconsistent size for old_vector"
        return
    endif

    new_vector = zeros(n) 
    !$OMP parallel do default(shared) private(CRS_id,col)
    do row=1,n
        do CRS_id=CRS_row_ptr(row),CRS_row_ptr(row+1)-1
            col = CRS_col(CRS_id)
            !$OMP atomic
            new_vector(row) = new_vector(row) + CRS_value(CRS_id)*old_vector(col)
        enddo
    enddo
    !$OMP end parallel do 
    
end function
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

pure function eyesMatrix(rank1, rank2) result(ret)
    integer(int32),intent(in) ::rank1, rank2
    real(real64),allocatable :: ret(:,:)
    integer(int32) :: i,min_rank

    allocate(ret(rank1, rank2) )
    ret(:,:) = 0.0d0
    min_rank = minval([rank1, rank2] )
    do i=1,min_rank
        if(rank2 > i)exit
        ret(i,i) = 1.0d0
    enddo

    
end function

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

subroutine reduce_crs_matrix(CRS_val,CRS_col,CRS_rowptr,remove_IDs)
    real(real64)  ,allocatable,intent(inout) :: CRS_val(:)
    integer(int32),allocatable,intent(inout) :: CRS_col(:)
    integer(int32),allocatable,intent(inout) :: CRS_rowptr(:)
    real(real64),allocatable :: new_CRS_val(:)
    integer(int32),allocatable :: new_CRS_col(:)
    integer(int32),allocatable :: new_CRS_rowptr(:)
    
    
    integer(int32),intent(in) :: remove_IDs(:)
    integer(int32),allocatable :: remove_IDs_sorted(:)
    integer(int32) :: i,j,old_row_id,old_col_id,old_col_ptr,count_m1
    integer(int32) :: new_row_id,new_col_id,new_col_ptr,remove_id_ptr,rem_count,k,l,m
    integer(int32),allocatable :: new_id_from_old_id(:)

    ! remove_ids should be unique and sorted
    remove_IDs_sorted = remove_IDs
    remove_IDs_sorted = unique(remove_IDs_sorted)
    call heapsort(n=size(remove_IDs_sorted),array=remove_IDs_sorted)
    

    ! allocate
    new_CRS_val = CRS_val
    new_CRS_col = CRS_col
    new_CRS_rowptr = CRS_rowptr
    

    !new_CRS_val = zeros(size(CRS_val) - size(remove_IDs_sorted) )
    !new_CRS_col = zeros(size(CRS_col) - size(remove_IDs_sorted) )
    !new_CRS_rowptr = zeros(size(CRS_rowptr) - size(remove_IDs_sorted) )
    !!
    new_id_from_old_id = zeros(size(CRS_rowptr)-1)
    do i=1,size(CRS_rowptr)-1
        new_id_from_old_id(i) = i
    enddo

    do i=1,size(remove_ids_sorted)
        new_id_from_old_id(remove_ids_sorted(i)+1: )=new_id_from_old_id(remove_ids_sorted(i)+1: )-1
        new_id_from_old_id(remove_ids_sorted(i) )=-1
    enddo
    
    ! if a id is listed in remove_ids_sorted, ignore

    !
    new_row_id  =0
    new_col_id  =0
    new_col_ptr =0
    remove_id_ptr = 1

    ! create new_CRS_col
    ! only for column
    ! blanks are indicated by -1
    ! 当該は-1して，前送り
    do i=1,size(CRS_rowptr)-1
        do j=CRS_rowptr(i),CRS_rowptr(i+1)-1
            old_col_ptr = j    
            old_col_id  = CRS_col(old_col_ptr)
            new_CRS_col(old_col_ptr) = new_id_from_old_id(old_col_id)
        enddo
    enddo

    ! 列に-1
    do i=1,size(remove_IDs_sorted)
        do j=CRS_rowptr( remove_IDs_sorted(i) ),CRS_rowptr( remove_IDs_sorted(i) +1)-1
            old_col_ptr = j
            new_CRS_col(old_col_ptr) = -1
        enddo
    enddo

    ! renew row_ptr
    new_CRS_rowptr = 0.0d0
    do i=1,size(CRS_rowptr)-1
        do j=CRS_rowptr(i),CRS_rowptr(i+1)-1
            if(new_CRS_col(j)/=-1 )then
                new_CRS_rowptr(i) = new_CRS_rowptr(i) + 1
            endif
        enddo
    enddo

    do i=1,size(new_CRS_rowptr)-1
        new_CRS_rowptr(i+1) = new_CRS_rowptr(i+1) + new_CRS_rowptr(i)
    enddo

    do i=size(new_CRS_rowptr),2,-1
        new_CRS_rowptr(i) = new_CRS_rowptr(i-1) +1
    enddo
    new_CRS_rowptr(1) = 1

    ! set -1 to removable rows
    do i=1,size(remove_ids_sorted)
        new_CRS_rowptr( remove_ids_sorted(i) ) = -1
    enddo


    ! renew CRS_val
    k=0
    do i=1,sizE(new_CRS_col)
        if(new_CRS_col(i)==-1 )then
            k = k+1
        endif
    enddo

    new_CRS_val = zeros(  size(CRS_val) - k  )
    k = 0
    do i=1,size(new_CRS_col)
        if(new_CRS_col(i) == -1 )then
            cycle
        else
            k = k + 1
            new_CRS_val(k) = CRS_val(i)
        endif
    enddo
    
    call searchAndRemove(new_CRS_col,eq=-1)
    call searchAndRemove(new_CRS_rowptr,eq=-1)
    
    deallocate(CRS_val)
    deallocate(CRS_col)
    deallocate(CRS_rowptr)

    
    CRS_val = new_CRS_val
    CRS_col = new_CRS_col
    CRS_rowptr = new_CRS_rowptr
    
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
! ###################################################################

function UpperTriangularMatrix(CRS_val,CRS_col,CRS_rowptr) result(UP)
    real(real64),intent(in) :: CRS_val(:)
    integer(int32),intent(in) :: CRS_col(:)
    integer(int32),intent(in) :: CRS_rowptr(:)
    integer(int64) :: i,j,col,row,N,offset
    real(real64) :: val
    real(real64) ,allocatable :: UP(:)

    N  = size(CRS_rowptr) - 1 
    UP = zeros(N*(N+1)/2)

    do row=1,N
        do i=CRS_rowptr(row),CRS_rowptr(row+1) - 1
            col = CRS_col(i)
            val = CRS_val(i)
            if(row<=col)then
                UP( row + (col-1)*col/2 ) = val
            endif
        enddo
    enddo

end function
! ###################################################################

function fillby(element,vec_size,num_repeat) result(new_vec)
    real(real64),intent(in) :: element(:)
    integer(int32),optional,intent(in) :: vec_size,num_repeat
    real(real64),allocatable :: new_vec(:)
    integer(int32) :: i, j
    
    if(present(vec_size) )then
        allocate(new_vec(vec_size))
        i=0
        do 
            do j=1,3
                i=i+1
                if(i>size(new_vec) )then
                    return
                endif
                new_vec(i) = element(j)  
            enddo
        enddo

    elseif(present(num_repeat) )then
        allocate(new_vec( num_repeat*size(element)  ) )
        do i=1,num_repeat
            new_vec( (i-1)*size(element)+1: i*size(element)  ) = element(1:size(element))
        enddo
    else
        new_vec = element
    endif

end function


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
! ################################################################

function unique(old_vec) result(new_vec)
    integer(int32),intent(in) :: old_vec(:)
    integer(int32),allocatable :: new_vec(:),remove_is_one(:)
    integer(int32) :: n_size,i,id,j

    n_size = size(old_vec)  
    ! caution; O(N^2)

    remove_is_one = zeros(n_size)
    do i=1,n_size
        if(remove_is_one(i) == 1) cycle
        do j=i+1,n_size
            if(old_vec(i)==old_vec(j) )then
                remove_is_one(j) = 1            
            endif
        enddo
    enddo

    new_vec = zeros(n_size - sum(remove_is_one) )

    id = 0
    do i=1,n_size
        if(remove_is_one(i) == 1) then
            cycle
        else
            id = id+ 1
            new_vec(id) = old_vec(i)
        endif
        
    enddo



end function


end module 