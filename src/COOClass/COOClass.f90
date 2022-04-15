module COOClass
    use ArrayClass
    implicit none

    type :: COO_Row_
        real(real64),allocatable :: val(:)
        integer(int32),allocatable :: col(:)
    end type

    type :: COO_
        type(COO_Row_),allocatable :: row(:)
    contains
        procedure,public :: init => initCOO
        procedure,public :: update => updateCOO
        procedure,public :: add => addCOO
        procedure,public :: getDenseMatrix => getDenseMatrixCOO
        procedure,public :: remove => removeCOO
        procedure,public :: getAllCol => getAllColCOO
        procedure,public :: to_CRS => to_CRSCOO
        !procedure,public ::getAllCol_as_row_obj => getAllCol_as_row_objCOO
        end type
        
        
    type :: CRS_
        integer(int32),allocatable :: col_idx(:)
        integer(int32),allocatable :: row_ptr(:)
        real(real64)  ,allocatable :: val(:)
    contains
        procedure,public :: init => initCRS
        procedure,public :: Lanczos => LanczosCRS
        procedure,public :: matmul => matmulCRS
        procedure,public :: eig => eigCRS
        procedure,public :: to_dense => to_denseCRS
    end type


contains

subroutine initCOO(this,num_row)
    class(COO_),intent(inout) :: this
    integer(int32),intent(in) :: num_row
    allocate(this%row(num_row) )
end subroutine




function to_CRSCOO(this,remove_coo) result(CRS_version)
    class(COO_),intent(inout) :: this
    logical,optional,intent(in) :: remove_coo
    type(CRS_) :: CRS_version
    integer(int32) :: i,j,idx
    integer(int64) :: n

    
    CRS_version%col_idx = this%getAllCol()

    CRS_version%row_ptr = int(zeros( size(this%row) + 1 ) )
    CRS_version%row_ptr(1) = 1
    do i=2, size(this%row)+1
        if( .not. allocated(this%row(i-1)%col ))then
            CRS_version%row_ptr(i) = CRS_version%row_ptr(i-1)
            cycle
        endif
        CRS_version%row_ptr(i) = CRS_version%row_ptr(i-1) + size(this%row(i-1)%col)


        if(present(remove_coo) )then
            if(remove_coo)then
                deallocate(this%row(i-1)%col)
            endif
        endif

    enddo



    
    
    !CRS_version%val      
    n = size(CRS_version%col_idx)
    allocate(CRS_version%val(n) )
    CRS_version%val(:) = 0.0d0
    

    do i=1, size(this%row)-1
        if( .not. allocated(this%row(i)%val ))then
            cycle
        endif
        idx = 0
        do j=CRS_version%row_ptr(i),CRS_version%row_ptr(i+1)-1
            idx=idx+1
            CRS_version%val(j) = this%row(i)%val(idx)
        enddo

        if(present(remove_coo) )then
            if(remove_coo)then
                deallocate(this%row(i)%val)
            endif
        endif
        
    enddo

    call this%remove()
    
end function 


subroutine updateCOO(this,row,col,val)
    class(COO_),intent(inout) :: this
    integer(int32),intent(in)   :: row
    integer(int32),intent(in)   :: col
    real(real64),intent(in)   :: val
    integer(int32) :: i, col_id
    
    if(.not. allocated(this%row) )then
        print *, "ERROR :: initCOO"
        print *, "Please call [COO]%init() before this operation."
        return
    endif

    if(.not. allocated(this%row(row)%val ) )then
        this%row(row)%val = [val]
        this%row(row)%col = [col]
    else

        ! check duplication
        if(minval(abs(this%row(row)%col(:) - col ) )==0 ) then
            ! duplication
            do i=1,size(this%row(row)%col)
                if(this%row(row)%col(i)==col )then
                    col_id = i
                    exit
                endif
            enddo
            this%row(row)%val(col_id) = val
        else
            this%row(row)%val = this%row(row)%val // [val]
            this%row(row)%col = this%row(row)%col // [col]
        endif
    endif
    

end subroutine


subroutine addCOO(this,row,col,val)
    class(COO_),intent(inout) :: this
    integer(int32),intent(in)   :: row
    integer(int32),intent(in)   :: col
    real(real64),intent(in)   :: val
    integer(int32) :: i, col_id
    
    if(.not. allocated(this%row) )then
        print *, "ERROR :: initCOO"
        print *, "Please call [COO]%init() before this operation."
        return
    endif

    if(.not. allocated(this%row(row)%val ) )then
        this%row(row)%val = [val]
        this%row(row)%col = [col]
    else

        ! check duplication
        if(minval(abs(this%row(row)%col(:) - col ) )==0 ) then
            ! duplication
            do i=1,size(this%row(row)%col)
                if(this%row(row)%col(i)==col )then
                    col_id = i
                    exit
                endif
            enddo
            this%row(row)%val(col_id) = this%row(row)%val(col_id) + val
        else
            this%row(row)%val = this%row(row)%val // [val]
            this%row(row)%col = this%row(row)%col // [col]
        endif
    endif
    

end subroutine



function getDenseMatrixCOO(this) result(dense_matrix)
    class(COO_),intent(in) :: this
    real(real64),allocatable :: dense_matrix(:,:)
    integer(int32) :: i, j
    
    dense_matrix = zeros( size(this%row),size(this%row))
    do i=1, size(this%row)
        if(allocated(this%row(i)%col) )then
            do j=1,size(this%row(i)%col)
                dense_matrix(i, this%row(i)%col(j) ) =this%row(i)%val(j)
            enddo
        endif
    enddo

end function

! ###############################################
subroutine removeCOO(this)
    class(COO_),intent(inout) :: this
    
    if(allocated(this%row) ) deallocate(this%row)

end subroutine
! ###############################################

function getAllColCOO(this) result(cols)
    class(COO_),intent(in) :: this
    type(COO_ROW_) :: row
    integer(int32),allocatable :: cols(:)
    integer(int32) :: num_total_cols,i

    num_total_cols = 0
    do i=1,size(this%row)
        if(allocated(this%row(i)%col ) )then
            num_total_cols = num_total_cols + size(this%row(i)%col)
        else
            cycle
        endif
    enddo
    allocate(cols(num_total_cols) )
    cols(:) = 0
    num_total_cols = 0
    do i=1,size(this%row)
        if(allocated(this%row(i)%col ) )then
            cols(num_total_cols+1:num_total_cols+size(this%row(i)%col)  ) &
                =  this%row(i)%col(1:size(this%row(i)%col) )
            num_total_cols = num_total_cols + size(this%row(i)%col)
        else
            cycle
        endif
    enddo
    
    
end function


!recursive function getAllCol_as_row_objCOO(this,row_old) result(row_new)
!    class(COO_),intent(in) :: this
!    type(COO_Row_),optional,intent(in) :: row_old(:)
!    type(COO_Row_) :: row_new
!    type(COO_Row_),allocatable :: row_new_buf(:)
!
!    integer(int32) :: num_row,half_row,num_elem
!
!    print *, "hello"
!    if(.not.present(row_old) )then
!        print *, "hello"
!        row_new = this%getAllCol_as_row_obj(this%row)
!        
!    else
!        if(size(row_old) ==1 )then
!            if(allocated(row_old(1)%col) )then
!                row_new%col = row_old(1)%col
!            else
!                allocate(row_new%col(0) )
!            endif
!        elseif(size(row_old) ==2 )then
!            if(allocated(row_old(1)%col) .and. allocated(row_old(1)%col) )then
!                row_new%col = row_old(1)%col
!                row_new%col = row_new%col // row_old(2)%col
!            elseif(allocated(row_old(1)%col))then
!                row_new%col = row_old(1)%col
!            elseif(allocated(row_old(2)%col))then    
!                row_new%col = row_old(2)%Col
!            else
!                allocate(row_new%col(0) )
!            endif
!        else
!            num_row = size(row_old)
!            half_row = num_row/2
!            print *, half_row, num_row
!            allocate(row_new_buf(2) )
!            row_new_buf(1) = this%getAllCol_as_row_obj(this%row(1:half_row) )
!            row_new_buf(2) = this%getAllCol_as_row_obj(this%row(half_row:) )
!            row_new%col = row_new_buf(1)%col // row_new_buf(2)%col
!        endif
!    endif
!
!end function
!
subroutine LanczosCRS(this,DiagonalVector,subDiagonalVector,V)
    class(CRS_),intent(in) :: this 
    
    real(real64),allocatable,intent(inout) :: V(:,:)!,T(:,:)
    real(real64),allocatable,intent(inout) :: DiagonalVector(:)
    real(real64),allocatable,intent(inout) :: subDiagonalVector(:)
    real(real64),allocatable :: v1(:),w1(:),vj(:),wj(:)
    real(real64) :: alp1,beta_j,alp_j
    type(Random_) :: random
    integer(int32) :: i,N,j,m
    
    ! https://en.wikipedia.org/wiki/Lanczos_algorithm
  
    !N = size(A,1)
    N = size(this%row_ptr)-1
    V = zeros(N,N)
    !T = zeros(N,N)
    DiagonalVector    = zeros(N)
    subDiagonalVector = zeros(N-1)
  
    v1 = 2.0d0*random%randn(N)-1.0d0
    v1 = v1/norm(v1)
    w1 = this%matmul(v1)
    alp1 = dot_product(w1,v1)
    w1 = w1 - alp1*v1
    beta_j = norm(w1)
  
    !T(1,1) = alp1 
    V(:,1) = v1
    DiagonalVector(1) = alp1
    
    do i=2,N
        beta_j = norm(w1)
        if(beta_j/=0.0d0)then
            vj = w1/beta_j
        else
            vj = 2.0d0*random%randn(N)-1.0d0
            stop
        endif
        wj = this%matmul(vj)
        alp_j = dot_product(wj,vj)
        wj    = wj - alp_j * vj - beta_j*v1
        v1 = vj
        w1 =wj
  
        !T(i,i) = alp_j
        !T(i,i-1) = beta_j
        !T(i-1,i) = beta_j
        V(:,i) = vj
        DiagonalVector(i) = alp_j
        subDiagonalVector(i-1) = beta_j
    enddo
  
  
  

end subroutine

function matmulCRS(CRS,old_vector) result(new_vector)
    class(CRS_),intent(in) :: CRS
    real(real64),intent(in)  :: Old_vector(:)
    real(real64),allocatable :: new_vector(:)
  
    new_vector = crs_matvec_generic_cooclass(&
      CRS_value=CRS%val,&
      CRS_col=CRS%col_idx,&
      CRS_row_ptr=CRS%row_ptr,&
      old_vector=old_vector)
  
  end function
  


function crs_matvec_generic_cooclass(CRS_value,CRS_col,CRS_row_ptr,old_vector) result(new_vector)
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
  
subroutine eigCRS(this,Eigen_vectors,eigen_values)
    class(CRS_),intent(in) :: this
    real(real64),allocatable :: A(:,:), V(:,:)
    real(real64),allocatable :: D(:),E(:)
    real(real64),allocatable :: WORK(:)
    integer(int32),allocatable :: IWORK(:),IFAIL(:)
    real(real64),allocatable,intent(inout) :: Eigen_vectors(:,:),eigen_values(:)
    integer(int32) :: num_eigen,INFO
    real(real64)  :: VL, VU, ABSTOL
    integer(int32):: IL, IU 
    character(1) :: JOBZ="V"
    character(1) :: RANGE="A"

    call this%Lanczos(DiagonalVector=D,subDiagonalVector=E,V=V)

    !call print("--")
    !call print(V)
    
    num_eigen = size(D)
    eigen_values = zeros(size(D))
    eigen_vectors = zeros(size(D),num_eigen )
    WORK = zeros(5*size(D) )
    IWORK = int(zeros(5*size(D) ))
    IFAIL = int(zeros(size(D) ))
    ABSTOL = dble(1.0e-14)
    
    ! LAPACK
    call DSTEVX( JOBZ, RANGE, size(D),&
        D, E, VL, VU, IL, IU,ABSTOL,&
        num_eigen,eigen_values, eigen_vectors, size(D), &
        WORK, IWORK, IFAIL, INFO )
    
    
    !eigen_Vectors = matmul(V,matmul(eigen_values, transpose(V) ) )
    eigen_Vectors = matmul(V,eigen_vectors)
    do i_i=1,3
        eigen_vectors(:,i_i) = eigen_vectors(:,i_i)/norm( eigen_vectors(:,i_i) )
        eigen_vectors(:,i_i) =eigen_vectors(:,i_i)/eigen_vectors(3,i_i)   
    enddo

end subroutine

subroutine initCRS(this,val,col_idx,row_ptr)
    class(CRS_),intent(inout) :: this
    integer(int32),intent(in) :: col_idx(:), row_ptr(:)
    real(real64),intent(in)   :: val(:)

    this%val = val
    this%col_idx = col_idx
    this%row_ptr = row_ptr

end subroutine


function to_denseCRS(this) result(dense_mat)
    class(CRS_),intent(inout) :: this
    real(real64),allocatable  :: dense_mat(:,:)
    integer(int32) :: i,j,n,row,col

    n = size(this%row_ptr) - 1
    allocate(dense_mat(n,n) )
    do row=1, n
        do j=this%row_ptr(i), this%row_ptr(i+1) - 1
            col = this%col_idx(j)
            dense_mat(row,col) = this%val(j)
        enddo
    enddo

end function

end module COOClass