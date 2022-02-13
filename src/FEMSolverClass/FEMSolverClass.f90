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

        !(5) fix x=\bar{x}
        procedure,public :: fix => fixFEMSolver

        !(6) save matrix
        procedure,public :: saveMatrix => saveMatrixFEMSolver
        
        !re-zero matrix
        procedure,public :: zeros => zerosFEMSolver

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
! ###################################################################
!function LOBPCG(A,B,num_eigens,err) result(eigens)
!    type(FEMSolver_),intent(in) :: A, B
!    complex(real64),allocatable :: eigens(:,:)
!    integer(int32),intent(in) :: num_eigens
!    real(real64),optional,intent(in) :: err
!    real(real64) :: error_tolerance
!    integer(int32) :: i,j,m,nx,nv
!
!    
!end function
! ###################################################################
function LOBPCG_dense(A,B,lambda_min) result(eigen_vectors)
    real(real64),intent(in) :: A(:,:),B(:,:)
    real(real64),allocatable :: eigen_vectors(:,:)
    real(real64),allocatable :: V(:,:),A_(:,:),xtemp(:,:),&
        lambda_and_x(:,:),lambda(:),bmin(:),x(:,:),r(:,:),x0s(:,:),p(:,:),w(:),&
        alpha,beta,gamma,XX(:),WW(:),PP(:),SA(:,:),SB(:,:),w_x_p(:,:),WW_XX_PP(:,:),&
        SB_inv(:,:),x_(:,:),lambda_mat(:,:),lambda_ids(:),Bm(:,:),residual(:)
    integer(int32) :: num_eigen

    real(real64) :: tol=dble(1.0e-10)
    real(real64) :: mu
    real(real64),intent(inout) :: lambda_min(:)
    type(Random_) :: random
    integer(int32) :: i,j,n,id,itr
    integer(int32) :: m

    num_eigen = size(lambda_min,1)
    m = 3*num_eigen
    residual = zeros(num_eigen)
    !https://www.researchgate.net/figure/Algorithm-of-LOBPCG-method-for-matrix-A-Here-the-matrix-T-is-a-preconditioner_fig1_323863889
   !    !x0s = random%randn(n,3)
    !initial guess x0
    n = size(A,1)

    lambda_min = 0.0d0

    n = size(A,1)
    xtemp = zeros(n,3*num_eigen)
    
    eigen_vectors = zeros(n,num_eigen)
    
    x = zeros(n,num_eigen)
    x = random%randn(n,num_eigen)
    
    do i=1,num_eigen
        x(:,i) = x(:,i)/norm(x(:,i) )
    enddo
    r = zeros(n,num_eigen)
    do i=1,num_eigen
        mu = dot_product(x(:,i) ,matmul(A,x(:,i) ))/dot_product(x(:,i),x(:,i) )
        r(:,i) = matmul(A,x(:,i) ) - mu*x(:,i)
    enddo
    !r = r/norm(r)
    p = zeros(n,num_eigen)

    V  = zeros(n,3*num_eigen)
    V(:,1:num_eigen) = x(:,:)
    V(:,num_eigen+1:2*num_eigen) = r(:,:)
    V(:,2*num_eigen+1:3*num_eigen) = p(:,:)
    xtemp = V

    call gram_real(n,2*num_eigen,xtemp,v)
    
    V  = zeros(n,3*num_eigen )
    x(:,:) = V(:,1:num_eigen) 
    r(:,:) = V(:,num_eigen+1:2*num_eigen) 
    p(:,:) = V(:,2*num_eigen+1:3*num_eigen) 
    

    itr=0
    lambda_mat = zeros(num_eigen,num_eigen)
    do 
        itr=itr+1
        
        if(itr==1)then
            
            A_ = matmul(transpose(X),matmul(A,X))
            A_ = 0.50d0*A_ + 0.50d0*transpose(A_)
            call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=tol) 
            ! X1 = X0 B
            x = matmul(x,x_)
            lambda_mat(:,:) = 0.0d0
            do i=1,size(lambda_mat)
                lambda_mat(i,i) = lambda(i)
            enddo
            R = matmul(A,X) - matmul(X,lambda_mat)
            !直交化
            call gram_real(n,num_eigen,X,X)
            call gram_real(n,num_eigen,R,R)
            A_ = zeros(n,2*num_eigen)
            A_(:,1:num_eigen) = X
            A_(:,num_eigen+1:) = R
            A_ = matmul(transpose( A_ ),matmul(A,A_ ))
            A_ = 0.50d0*A_ + 0.50d0*transpose(A_)
            !2m 次元固有値問題
            call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=tol) 
            !下から m 個の固有値と固有ベクトルからなる行列:
            lambda_ids = linspace([1.0d0,dble(num_eigen) ],num_eigen)
            call heapsort(n=2*num_eigen,array=lambda,val=lambda_ids)
            lambda_mat = zeros(num_eigen,num_eigen)
            do i=1,num_eigen
                lambda_mat(i,i) = lambda( int(lambda_ids(i) ) )
            enddo
            Bm = zeros(2*num_eigen,num_eigen)
            do i=1,2*num_eigen
                Bm(:,i) = x_(:, int(lambda_ids(i)) )
            enddo
            ! X2, R2, P2
            X = matmul(A_,Bm)
            R = matmul(A, X) - matmul(X, Lambda_mat)
            A_ = zeros(n,size(r,2)*2 )

            A_(:,num_eigen+1:2*num_eigen)  = r(:,:)
            
            P = matmul(A_,Bm)
            do i=1,num_eigen
                Residual(i) = norm(R(:,i))
            enddo
            
            if(minval(Residual) < tol)then
                exit
            endif

        else
            V  = zeros(n,3*num_eigen) 
            
            V(:,1:num_eigen)  = x(:,:)
            V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            V(:,2*num_eigen+1:3*num_eigen)  = p(:,:)
            A_ = matmul(transpose(V),matmul(A,V) )
            call eigenValueAndVector(A=A_,lambda=lambda,x=x_,tol=tol) 
            lambda_ids = linspace([1.0d0,dble(num_eigen) ],num_eigen)
            lambda_mat = zeros(num_eigen,num_eigen)
            do i=1,num_eigen
                lambda_mat(i,i) = lambda( int(lambda_ids(i) ) )
            enddo
            Bm = zeros(3*num_eigen,num_eigen)
            do i=1,2*num_eigen
                Bm(:,i) = x_(:, int(lambda_ids(i)) )
            enddo

            X = matmul(V,Bm)
            R = matmul(A, X) - matmul(X, Lambda_mat)
            A_ = zeros(n,size(r,2)*3 )

            V(:,num_eigen+1:2*num_eigen)  = r(:,:)
            V(:,2*num_eigen+1:3*num_eigen)  = p(:,:)
            P = matmul(A_,Bm)
            do i=1,num_eigen
                Residual(i) = norm(R(:,i))
            enddo
            if(minval(Residual) < tol)then
                exit
            endif
            
        endif
        !あとは収束判定
        if(itr==10000)then
            print *, "ERROR :: LOBPCG >> did not converge!"
            exit
        endif
    enddo
    print *, "residual:",minval(residual)
    print *, "itr=",itr
    
    do i=1,num_eigen
        lambda_min(i) = lambda( int(lambda_ids(i) ) )
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
        mat_v_out(:,j) = viold/norm
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

function crs_matmul(CRS_value,CRS_col,CRS_row_ptr,old_vector) result(new_vector)
    real(real64),intent(in)  :: CRS_value(:),Old_vector(:)
    integeR(int32),intent(in):: CRS_col(:),CRS_row_ptr(:)

    real(real64),allocatable :: new_vector(:)
    integer(int32) :: i, j, n,gid,lid,row,CRS_id,col
    !> x_i = A_ij b_j


    n = size(CRS_row_ptr)-1
    if(size(old_vector)/=n )then
        print *, "ERROR crs_matmul :: inconsistent size for old_vector"
        return
    endif

    new_vector = zeros(n) 
    !$OMP parallel do default(shared) private(CRS_id,col)
    do row=1,n
        do CRS_id=CRS_row_ptr(i),CRS_row_ptr(i+1)-1
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


end module 