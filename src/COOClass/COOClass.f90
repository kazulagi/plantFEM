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
end module COOClass