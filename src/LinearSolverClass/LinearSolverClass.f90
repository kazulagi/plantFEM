module LinearSolverClass
  use, intrinsic :: iso_fortran_env
  use omp_lib
  use IOClass
  use TimeClass
  use MathClass
  use ArrayClass
  use COOClass
  !use MPIClass
  implicit none

  interface BiCGSTAB
    module procedure bicgstab_real32, bicgstab_real64, bicgstab_complex64
  end interface
  
  interface GPBiCG
    module procedure GPBiCG_real32, GPBiCG_real64, GPBiCG_complex64
  end interface
  
  interface Gauss_Jordan_PV
    module procedure Gauss_Jordan_PV_real32, Gauss_Jordan_PV_real64, Gauss_Jordan_PV_complex64
  end interface

  type :: LinearSolver_
    ! non-Element-by-element
    real(real64),allocatable :: a(:,:)
    real(real64),allocatable :: b(:)
    real(real64),allocatable :: x(:)
    ! Element-by-element
    real(real64),allocatable :: a_e(:,:,:)
    real(real64),allocatable :: b_e(:,:)
    real(real64),allocatable :: x_e(:,:)
    
    ! For Sparse 
    ! COO format
    real(real64),allocatable   :: val(:)
    integer(int32),allocatable :: index_I(:)
    integer(int32),allocatable :: index_J(:)
    integer(int32),allocatable :: row_domain_id(:)
    integer(int32),allocatable :: column_domain_id(:)
    ! CRS format
    real(real64),allocatable   :: CRS_val(:)
    integer(int32),allocatable :: CRS_index_I(:)
    integer(int32),allocatable :: CRS_index_J(:)
    integer(int32),allocatable :: CRS_row_domain_id(:)
    integer(int32),allocatable :: CRS_column_domain_id(:)

    integer(int32),allocatable   :: b_Index_J(:)
    integer(int32),allocatable   :: b_Domain_ID(:)
    logical,allocatable   :: Locked(:)

    ! info
    integer(int32),allocatable   :: NumberOfNode(:)
    integer(int32) :: DOF=1
    logical :: debug=.false.
    ! 
    integer(int32),allocatable :: connectivity(:,:)
    integer(int32) :: itrmax=1000000
    integer(int32) :: currentID=1
    integer(int32) :: b_currentID=1
    real(real64) :: er0=dble(1.0e-10)
    logical :: ReadyForFix = .false.

  contains
    
    procedure, public :: init => initLinearSolver

    procedure, public :: set => setLinearSolver
    
    procedure, public :: assemble => assembleLinearSolver
    
    procedure, public :: import => importLinearSolver
    procedure, public :: fix => fixLinearSolver
    procedure, public :: solve => solveLinearSolver
    
    procedure, public :: show => showLinearSolver
    procedure, public :: globalMatrix => globalMatrixLinearSolver
    procedure, public :: globalVector => globalVectorLinearSolver

    procedure, public  :: convertCOOtoCRS => convertCOOtoCRSLinearSolver
    procedure, public  :: matmulCRS => matmulCRSLinearSolver
    procedure, public  :: matmulCOO => matmulCOOLinearSolver

    procedure, public :: prepareFix   => prepareFixLinearSolver
    procedure, public :: getCOOFormat => getCOOFormatLinearSolver
    procedure, public :: exportAsCOO  => exportAsCOOLinearSolver
    procedure, public :: exportRHS    => exportRHSLinearSolver
  end type


  interface crs_matvec
      module procedure :: crs_matvec_generic,crs_matvec_for_CRStype
  end interface crs_matvec

  
  interface crs_matmul
      module procedure :: crs_matmul_generic,crs_matmul_for_CRStype
  end interface crs_matmul
contains

!====================================================================================
subroutine initLinearSolver(obj,NumberOfNode,DOF)
  class(LinearSolver_),intent(inout) :: obj
  integer(int32),optional,intent(in) :: NumberOfNode(:),DOF
  integer(int32) :: i,j,k,n,num_total_unk,node_count
  
  obj%ReadyForFix = .false.
    ! non-Element-by-element
  if(allocated(obj % a) ) deallocate(obj % a)
  if(allocated(obj % b) ) deallocate(obj % b)
  if(allocated(obj % x) ) deallocate(obj % x)
  ! Element-by-element
  if(allocated(obj % a_e) ) deallocate(obj % a_e)
  if(allocated(obj % b_e) ) deallocate(obj % b_e)
  if(allocated(obj % x_e) ) deallocate(obj % x_e)

  if(allocated(obj % val) ) deallocate(obj % val)
  if(allocated(obj % index_I) ) deallocate(obj % index_I)
  if(allocated(obj % index_J) ) deallocate(obj % index_J)
  if(allocated(obj % row_Domain_ID) ) deallocate(obj % row_Domain_ID)
  if(allocated(obj % column_Domain_ID) ) deallocate(obj % column_Domain_ID)

  if(allocated(obj % b_Index_J) ) deallocate(obj % b_Index_J)
  if(allocated(obj % b_Domain_ID) ) deallocate(obj % b_Domain_ID)
  if(allocated(obj % Locked) ) deallocate(obj % Locked)

  if(allocated(obj % connectivity) ) deallocate(obj % connectivity)

  n =  input(default=1, option=DOF)
  ! Number of node of n th domains is NumberOfNode(n)
  if(present(NumberOfNode)  )then
    num_total_unk = sum(NumberOfNode) * n
    allocate(obj%b_Index_J(num_total_unk) )
    allocate(obj%b_Domain_ID(num_total_unk) )
    allocate(obj%Locked(num_total_unk) )
    
    allocate(obj%b(num_total_unk) )
    obj%NumberOfNode = NumberOfNode
    obj%DOF = DOF
    obj%b(:) = 0.0d0

    num_total_unk = 0
    do i=1,size(NumberOfNode)
      node_count= 0
      do j=1,NumberOfNode(i)
        do k=1, n
          num_total_unk = num_total_unk + 1
          node_count = node_count + 1
          obj%b_Domain_ID(num_total_unk) = i
          obj%Locked(num_total_unk) = .false.
          obj%b_Index_J(num_total_unk)   = node_count
        enddo
      enddo
    enddo
  endif

  obj % itrmax=1000000
  obj % currentID=1
  obj % b_currentID=1
  obj % er0=dble(1.0e-08)
end subroutine
!====================================================================================


!====================================================================================
recursive subroutine assembleLinearSolver(obj,connectivity,DOF,eMatrix,eVector,DomainIDs)
  class(LinearSolver_),intent(inout) :: obj 
  integer(int32),intent(in) :: connectivity(:) ! connectivity matrix 
  !(global_node_id#1, global_node_id#2, global_node_id#3, . )
  integer(int32),intent(in) :: DOF ! degree of freedom
  integer(int32),optional,intent(in) :: DomainIDs(:) ! DomainID
  real(real64),optional,intent(in) :: eMatrix(:,:) ! elemental matrix
  real(real64),optional,intent(in) :: eVector(:) ! elemental Vector
  integer(int32) :: i,j,k,l,m,node_id1,node_id2,domain_ID1, domain_ID2
  integer(int32),allocatable :: domID(:)


  if(present(eMatrix) )then
    if(present(DomainIDs) )then
      do j=1, size(connectivity,1)
        do k=1, size(connectivity,1)
          do l=1, DOF
            do m=1, DOF
              node_id1 = connectivity(j)
              node_id2 = connectivity(k)
              if(j<1 .or. k<1)then
                print *, "ERROR :: Assemble solver j<1 .or. k<1"
                stop
              endif
              if(j > size(DomainIDs) .or.k > size(DomainIDs))then
                print *, j,size(DomainIDs),k
                print*, DOmainIDs
                print *, "ERROR :: Assemble solver j >= size(DomainIDs) .or.k >= size(DomainIDs)"
                stop
              endif

              domain_ID1 = DomainIDs(j)
              domain_ID2 = DomainIDs(k)
              call obj%set(&
                  low=DOF*(node_id1-1) + l, &
                  column= DOF*(node_id2-1) + m, &
                  entryvalue=eMatrix( DOF*(j-1) + l  , DOF*(k-1) + m ) ,&
                  row_DomainID = Domain_ID1,&
                  column_DomainID = Domain_ID2)
            enddo
          enddo
        enddo
      enddo
    else
      do j=1, size(DomainIDs,1)
        do k=1, size(DomainIDs,1)
          do l=1, DOF
            do m=1, DOF
              node_id1 = connectivity(j)
              node_id2 = connectivity(k)
              call obj%set(&
                  low=DOF*(node_id1-1) + l, &
                  column= DOF*(node_id2-1) + m, &
                  entryvalue=eMatrix( DOF*(j-1) + l  , DOF*(k-1) + m ) ,&
                  row_DomainID = 1,&
                  column_DomainID = 1)
            enddo
          enddo
        enddo
      enddo
    endif
  endif

  if(present(eVector) )then
    if(present(DomainIDs) )then
      do j=1, size(connectivity)
          do l=1, DOF
              node_id1 = connectivity(j)
              domain_ID1 = DomainIDs(j)
              call obj%set(&
                  low=DOF*(node_id1-1) + l, &
                  entryvalue=eVector( DOF*(j-1) + l ) ,&
                  row_DomainID=Domain_ID1)
        enddo
      enddo
    else
      do j=1, size(connectivity)
        do l=1, DOF
            node_id1 = connectivity(j)
            call obj%set(&
                low=DOF*(node_id1-1) + l, &
                entryvalue=eVector( DOF*(j-1) + l ) ,&
                row_DomainID=1)
      enddo
    enddo
    endif
  endif

end subroutine
!====================================================================================


!====================================================================================
recursive subroutine fixLinearSolver(obj,nodeid,entryvalue,entryID,DOF,row_DomainID,&
    nodeids,entryvalues,debug)
  class(LinearSolver_),intent(inout) :: obj
  logical,optional,intent(in) :: debug
  integer(int32),intent(in) :: nodeid
  integer(int32),optional,intent(in) :: entryID,DOF,row_DomainID,nodeids(:),entryvalues(:)
  real(real64),intent(in) :: entryvalue
  integer(int32),allocatable :: Index_I(:), Index_J(:),NumNodeBeforeDomainID(:)
  integer(int32) :: i,j, n, offset,m
  type(Time_) :: time

  ! [CASE1] :: single-domain & dense matrix
  ! [CASE2] :: single-domain & Sparse matrix
  ! [CASE3] :: multi-domain  & dense matrix 
  ! [CASE4] :: multi-domain  & Sparse matrix
  
  ! [CASE0] :: none of above

  ! [CASE1][CASE0]
  if(.not.allocated(obj%val) )then
    if(allocated(obj%a) .and. allocated(obj%b) )then
      ! [CASE1]
      ! it only has obj%a and obj%b
      !x(nodeid) = entryvalue
      n = size(obj%b)
      
      obj%b(:) = obj%b(:) - obj%a(:,nodeid)*entryvalue
      obj%a(:,nodeid) = 0.0d0
      obj%a(nodeid,:) = 0.0d0
      obj%a(nodeid,nodeid) = 1.0d0
      obj%b(nodeid) = entryvalue
      return
    else
      ![CASE0]
      print *, "[ERROR] LinearSolver >> [CASE0] :: No Ax=b is set."
    endif
  endif

  ! Below, cases [CASE2] or [CASE4] 
  if(.not. present(row_DomainID) )then
    ![CASE2] >> set : row_DomainID = 1 >> [Case4]
    call obj%fix(nodeid=nodeid,entryvalue=entryvalue,entryID=entryID,DOF=DOF,&
      row_DomainID=1,debug=debug)
  endif

  if(obj%debug)then
    print *, "[fixLinearSolver] >> ReadyForFix"
  endif
  
  if(.not.obj%ReadyForFix)then
    call obj%prepareFix()
  endif

  if(obj%debug)then
    print *, "[Done] >> ReadyForFix"
  endif

  if(present(debug) )then
    if(debug)then
      call time%start()
      print *, "fixLinearSolver  >> [0] started!"

    endif
  endif

  if(present(DOF)  )then
    if(.not. present(entryID) )then
      print *, "ERROR :: fixLinearSolver >> argument [DOF] should be called with [entryID]"
      print *, "e.g. x-entry of nodeid=10 in terms of 3d(x,y,z) space is >> "
      print *, "nodeid =10, entryID=1, DOF=3"
      stop
    endif
    n = (nodeid-1)*DOF + entryID
    if(present(row_DomainID) )then
      call obj%fix(nodeid=n,entryvalue=entryvalue,row_DomainID=row_DomainID,debug=debug)
    else
      call obj%fix(nodeid=n,entryvalue=entryvalue,row_DomainID=1,debug=debug)
    endif
    return
  endif


  if(.not.allocated(obj%row_domain_id) .and..not.present(row_DomainID) )then
    ! only for COO-format
    if(.not. allocated(obj%val) .or. .not.allocated(obj%b))then
      print *, "ERROR >> fixLinearSolver .not. allocated(val) "
      stop
    endif

    
    if(present(debug) )then
      if(debug)then
        print *, "fixLinearSolver  >> [1] fix started!"
      endif
    endif
  

    do i=1,size(obj%val)
      if(obj%index_J(i)==nodeid)then
        if( .not.obj%Locked(obj%index_I(i) ) ) then
          obj%b(obj%index_I(i) ) = obj%b(obj%index_I(i) )- obj%val(i) * entryvalue
        endif
        obj%val(i)=0.0d0
      endif
    enddo

    do i=1,size(obj%index_I)

      if(obj%index_I(i)==nodeid)then
        if(obj%index_J(i) ==nodeid)then
          obj%val(i)=1.0d0
        else
          obj%val(i)=0.0d0
        endif
        if(.not.obj%Locked(obj%index_I(i) ) ) then
          obj%b(obj%index_I(i) ) = entryvalue
          obj%Locked(obj%index_I(i) ) = .true.
        endif
        
        !print *, "Locked ",obj%index_I(i),"by",entryvalue
      endif

      if(obj%index_I(i)==nodeid)then
        if(obj%index_J(i)==nodeid)then
          obj%val(i)=1.0d0
        endif
      endif
    enddo

    if(present(debug) )then
      if(debug)then
        print *, "fixLinearSolver  >> [ok] done!"
      endif
    endif
    return

  elseif(allocated(obj%row_domain_id) .and. present(row_DomainID) )then
    ! only for COO-format

    if(present(debug) )then
      if(debug)then
        print *, "fixLinearSolver  >> [1] Multi-domain started!"
        call time%show()
      endif
    endif

    ! fix b vector
    n = row_DomainID
    if(n == 1)then
      offset = 0
    else
      offset = sum( obj%NumberOfNode(1:n-1) )*obj%DOF
    endif 
    if(.not.obj%Locked(offset + nodeid ) )then
      obj%b( offset + nodeid ) = entryvalue
      obj%Locked(offset + nodeid ) = .true.
      !print *, "Locked ",(offset + nodeid),"by",entryvalue
    endif


    if(present(debug) )then
      if(debug)then
        print *, "fixLinearSolver  >> [1-2] count offset number"
        call time%show()
      endif
    endif    
    
    allocate(NumNodeBeforeDomainID(size(obj%row_Domain_ID,1) ))

    NumNodeBeforeDomainID(1) = 0
    
    do m=2,size(obj%NumberOfNode)
      NumNodeBeforeDomainID(m) = sum(obj%NumberOfNode(1:m-1))
    enddo

    if(.not. allocated(obj%val) .or. .not.allocated(obj%b))then
      print *, "ERROR >> fixLinearSolver .not. allocated(val) "
      stop
    endif
    
    !print *, "obj%b",obj%b
    
    if(present(debug) )then
      if(debug)then
        print *, "fixLinearSolver  >> [2] Updating b-vector"
        call time%show()
      endif
    endif

    ! update b-vector (Right-hand side vector)
    do i=1,size(obj%val)
      

      if(obj%val(i)==0.0d0)then
        cycle
      endif

      if(obj%column_Domain_ID(i) /= row_DomainID)then
        cycle
      endif
      
      if(obj%index_J(i)==nodeid  )then  
        
        n = obj%row_Domain_ID(i)

        offset = NumNodeBeforeDomainID(n)*obj%DOF

        n = obj%Index_I(i)
        
        if( .not. obj%Locked(offset + n  )) then
          if(size(obj%NumberOfNode)==1 )then
            obj%b(n ) = obj%b(n ) - obj%val(i) * entryvalue
          else
            obj%b( offset + n ) = obj%b( offset  + n ) - obj%val(i) * entryvalue
          endif
        endif
        
        obj%val(i)=0.0d0
      else
        cycle
      endif
    enddo


    if(present(debug) )then
      if(debug)then
        print *, "fixLinearSolver  >> [3] Updated b-vector"
        call time%show()
      endif
    endif



    do i=1,size(obj%index_I) ! for all queries of A matrix
      
      if(obj%index_I(i)==nodeid .and. obj%row_Domain_ID(i)==row_DomainID )then
        obj%val(i)=0.0d0
        if(obj%index_J(i)==nodeid .and. obj%column_Domain_ID(i)==row_DomainID )then
          obj%val(i)=1.0d0
        endif
      endif

    enddo
    
    if(present(debug) )then
      if(debug)then
        print *, "fixLinearSolver  >> [ok] Done!"
        call time%show()
      endif
    endif

    
  else
    print *, "ERROR  :: fixLinearSolver >> allocated(obj%row_domain_id) /= present(row_DomainID)"
    print *, allocated(obj%row_domain_id),present(row_DomainID)
    stop
  endif
  
  


end subroutine
!====================================================================================


!====================================================================================
recursive subroutine setLinearSolver(obj,low,column,entryvalue,init,row_DomainID,column_DomainID)
  class(LinearSolver_),intent(inout) :: obj
  integer(int32),optional,intent(in) :: low, column,row_DomainID,column_DomainID
  real(real64),optional,intent(in) :: entryvalue
  logical,optional,intent(in) :: init
  integer(int32) :: i,row_DomID,column_DomID,row_offset,column_offset,j,k,find_num,max_thread

  row_DomID = input(default=1,option=row_DomainID)
  column_DomID = input(default=1,option=column_DomainID)
  if(allocated(obj%NumberOfNode) )then
    if(present(row_DomainID) )then
      if(row_DomainID==1)then
        row_offset=0
      else
        row_offset = sum(obj%NumberOfNode(1:row_DomainID-1) )
      endif
    endif
    if(present(column_DomainID) )then
      if(column_DomainID==1)then
        column_offset=0
      else
        column_offset = sum(obj%NumberOfNode(1:column_DomainID-1) )
      endif
    endif
  endif
  
  if(present(init) )then
    if(init .eqv. .true.)then
      if(allocated(obj%val) )then
        obj%val(:)=0.0d0
      endif
      if(allocated(obj%b) )then
        obj%b(:)=0.0d0
      endif
      obj%currentID=1
    endif
  endif

  if(present(low) .and. present(column))then
    
    if(.not. allocated(obj%val) )then
      allocate(obj%val(1) )
      obj%val(1)=input(default=0.0d0, option=entryvalue)
      allocate(obj%index_I(1) )
      obj%index_I(1)=low
      allocate(obj%index_J(1) )
      obj%index_J(1)=column
      allocate(obj%row_Domain_ID(1) )
      allocate(obj%column_Domain_ID(1) )
      obj%row_Domain_ID(obj%currentID)=row_DomID
      obj%column_Domain_ID(obj%currentID)=column_DomID

      ! DomainIDは，rowとcolumnの両方必要！！！
      ! DomainIDは，rowとcolumnの両方必要！！！
      ! DomainIDは，rowとcolumnの両方必要！！！
      ! DomainIDは，rowとcolumnの両方必要！！！
      ! DomainIDは，rowとcolumnの両方必要！！！
      ! DomainIDは，rowとcolumnの両方必要！！！
      ! DomainIDは，rowとcolumnの両方必要！！！
      return
    endif
    
    ! if already exists, add.
!     find_num = 0
!     !$omp parallel
!     !$omp do reduction(+:find_num)
!     do i=1,size(obj%index_I)
!       if(obj%index_i(i)==0 ) cycle
!       if(obj%row_domain_id(i) == row_DomainID .and. obj%column_domain_id(i) == column_DomainID )then
!         if(obj%index_I(i) == low )then
!           if(obj%index_J(i) == column )then
!             obj%val(i) = obj%val(i) + entryvalue
!             find_num = 1
!           endif
!         endif
!       endif
!     enddo
!     !$omp end do
!     !$omp end parallel
!     if(find_num>=1)then
!       return
!     endif

    if(obj%currentID+1 > size(obj%val) )then
      call  extendArray(obj%val,0.0d0,size(obj%val) )
      call  extendArray(obj%index_I,0,size(obj%index_I) )
      call  extendArray(obj%index_J,0,size(obj%index_J) )
      call  extendArray(obj%row_Domain_ID,0,size(obj%row_Domain_ID) )
      call  extendArray(obj%column_Domain_ID,0,size(obj%column_Domain_ID) )
    endif
    obj%currentID=obj%currentID+1
    obj%val(obj%currentID) = entryvalue
    obj%index_I(obj%currentID) = low
    obj%index_J(obj%currentID) = column
    if(present(row_DomainID) )then
      obj%row_Domain_ID(obj%currentID) = row_DomainID
    else
      obj%row_Domain_ID(obj%currentID) = 1
    endif
    if(present(column_DomainID) )then
      obj%column_Domain_ID(obj%currentID) = column_DomainID
    else
      obj%column_Domain_ID(obj%currentID) = 1
    endif
    
    return
  elseif(present(low) .and. .not.present(column) )then ! for right-hand side vector

    if(present(row_DomainID) )then
      ! multi-domain problem
      if(.not. allocated(obj%b) )then
        print *, "ERROR :: setLinearSolver >> for multi-domain problem, please call %init method"
        print *, "with % init( NumberOfNode , DOF )"
        stop
      else
        
        ! Right-hand side vector
        ! Extend one-by-one
        if(row_DomainID==1)then
          row_offset=0
        else
          row_offset = sum(obj%NumberOfNode(1:row_DomainID-1)  )*obj%DOF
        endif
        obj%b(row_offset+low) =obj%b(row_offset+low) + entryvalue
      endif
      return
    else
      ! single-domain problem
      if(.not. allocated(obj%b) )then
        allocate(obj%b(low) )
        obj%b(low) = input(default=0.0d0, option=entryvalue)
        obj%CurrentID = low
        return
      else
        ! Right-hand side vector
        if(low > size(obj%b) )then
          if(obj%currentID < size(obj%val) )then
            obj%b(low)=entryvalue
          else
            call extendArray(obj%b,0.0d0,low-size(obj%b) )
            obj%b(low)=obj%b(low)+entryvalue
          endif
        endif
      endif
    endif
  else
    return
  endif

end subroutine
!====================================================================================

!====================================================================================
subroutine importLinearSolver(obj,a,x,b,a_e,b_e,x_e,connectivity,val,index_I,index_J)
  class(LinearSolver_),intent(inout) :: obj
  real(8),optional,intent(in) :: a(:,:),b(:),x(:),a_e(:,:,:),b_e(:,:),x_e(:,:)
  real(8),optional,intent(in) :: val(:)
  integer(int32),optional,intent(in) :: index_I(:),index_J(:)
  integer(int32),optional,intent(in) :: connectivity(:,:)
  integer(int32) :: k,l,m

  if(present(val) )then
    if(.not. allocated(obj%val) )then
      allocate(obj%val(size(val) ))
      obj%val=val
    endif
  endif
  
  if(present(index_i) )then
    if(.not. allocated(obj%index_i) )then
      allocate(obj%index_i(size(index_i) ))
      obj%index_i=index_i
    endif
  endif

  
  if(present(index_j) )then
    if(.not. allocated(obj%index_j) )then
      allocate(obj%index_j(size(index_j) ))
      obj%index_j=index_j
    endif
  endif
  
  ! in case of non element-by-element
  if(present(a) )then
    ! Set Ax=b
    k=size(a,1)
    l=size(a,2)
    if(.not. allocated(obj%a) )then
      allocate(obj%a(k,l) )
    elseif(size(obj%a,1)/=k .or. size(obj%a,2)/=l )then
      deallocate(obj%a)
      allocate(obj%a(k,l) )
    endif
    obj%a(:,:)=a(:,:)
  endif

  if(present(b) )then
    k=size(b,1)
    if(.not. allocated(obj%b) )then
      allocate(obj%b(k) )
    elseif(size(obj%b,1)/=k)then
      deallocate(obj%b)
      allocate(obj%b(k) )
    endif
    obj%b(:)=b(:)
  endif

  if(present(x) )then
    k=size(x,1)
    if(.not. allocated(obj%x) )then
      allocate(obj%x(k) )
    elseif(size(obj%x,1)/=k)then
      deallocate(obj%x)
      allocate(obj%x(k) )
    endif
    obj%x(:)=x(:)
  endif

  if(present(a_e) )then
    ! Set A_e x_e =b_e
    k=size(a_e,1)
    l=size(a_e,2)
    m=size(a_e,3)
    if(.not. allocated(obj%a_e) )then
      allocate(obj%a_e(k,l,m) )
    endif
    obj%a_e(:,:,:)=a_e(:,:,:)
  endif

  if(present(b_e) )then
    ! Set Ax=b
    k=size(b_e,1)
    l=size(b_e,2)
    if(.not. allocated(obj%b_e) )then
      allocate(obj%b_e(k,l) )
    elseif(size(obj%b_e,1)/=k .or. size(obj%b_e,2)/=l )then
      deallocate(obj%b_e)
      allocate(obj%b_e(k,l) )
    endif
    obj%b_e(:,:)=b_e(:,:)
  endif

  if(present(x_e) )then
    ! Set Ax=b
    k=size(x_e,1)
    l=size(x_e,2)
    if(.not. allocated(obj%x_e) )then
      allocate(obj%x_e(k,l) )
    elseif(size(obj%x_e,1)/=k .or. size(obj%x_e,2)/=l )then
      deallocate(obj%x_e)
      allocate(obj%x_e(k,l) )
    endif
    obj%x_e(:,:)=x_e(:,:)
  endif

end subroutine importLinearSolver
!====================================================================================

!====================================================================================
subroutine prepareFixLinearSolver(obj,debug)
  class(LinearSolver_),intent(inout) :: obj
  logical,optional,intent(in) :: debug
  integer(int32),allocatable :: Index_I(:), Index_J(:),row_domain_id(:),column_Domain_ID(:)
  real(real64),allocatable:: val(:)
  integer(int32),allocatable :: array(:,:)
  integer(int32) :: i,m,n,rn,rd,cn,cd,same_n,count_reduc,j
  integer(int32) :: Index_I_max, Index_J_max,row_domain_id_max,column_Domain_ID_max
  
  ! It's too slow
  if(obj%ReadyForFix) return
  ! remove overlapped elements
  
  count_reduc = 0

  if(present(debug) )then
    if(debug)then
      print *, "prepareFixLinearSolver >> [1] heap-sort started."
    endif
  endif

  ! first, heap sort
  n=size(obj%val)
  allocate(array(n,4) )
  array(:,1) = obj%Index_I
  array(:,2) = obj%Index_J
  array(:,3) = obj%row_domain_id
  array(:,4) = obj%column_Domain_ID

  call heapsortArray(array, obj%val)
  
  obj%Index_I = array(:,1) 
  obj%Index_J = array(:,2) 
  obj%row_domain_id = array(:,3) 
  obj%column_Domain_ID = array(:,4) 

  
  if(present(debug) )then
    if(debug)then
      print *, "prepareFixLinearSolver >> [2] Remove overlaps"
    endif
  endif
  ! second, remove overlap
  do i=1,n-1
    if(obj%index_i(i)==0 ) cycle
    do j=i+1,n
      if(obj%Index_I(i)==obj%Index_I(j) .and. obj%Index_J(i)==obj%Index_J(j) )then
        if(obj%row_domain_id(i)==obj%row_domain_id(j) .and. obj%column_domain_id(i)==obj%column_domain_id(j) )then
          obj%val(i) = obj%val(i) + obj%val(j)
          obj%Index_I(j) = 0
          obj%row_domain_id(j) = 0
          obj%Index_j(j) = 0
          obj%column_domain_id(j) = 0
        else
          exit
        endif
      else
        exit
      endif
    enddo
  enddo
  ! regacy

!  do i=1,size(obj%Index_I)-1
!    if(obj%Index_I(i) == 0 ) cycle
!    
!    !$OMP parallel
!    !$OMP do
!    do j=i+1, size(obj%Index_I)
!      if(obj%row_domain_id(j)==obj%row_domain_id(i) )then
!        if(obj%column_domain_id(j)==obj%column_domain_id(i) )then
!          if(obj%Index_I(j) == obj%Index_I(i) )then
!            if(obj%Index_J(j) == obj%Index_J(i) )then
!              obj%val(i) = obj%val(i) + obj%val(j)
!              obj%Index_I(j) = 0
!              obj%row_domain_id(j) = 0
!              obj%Index_j(j) = 0
!              obj%column_domain_id(j) = 0
!            else
!              cycle
!            endif
!          else
!            cycle
!          endif
!        else
!          cycle
!        endif
!      else
!        cycle
!      endif
!    enddo
!    !$OMP end do
!    !$OMP end parallel
!  enddo



  ! 
  if(present(debug) )then
    if(debug)then
      print *, "prepareFixLinearSolver >> [3] Renew info"
    endif
  endif

  count_reduc = 0
  do i=1,size(obj%index_I)
    if(obj%index_I(i)/=0 )then
      count_reduc = count_reduc + 1
    endif
  enddo
  
  allocate(val( count_reduc ) )
  allocate(Index_I(count_reduc  ) )
  allocate(Index_J(count_reduc  ) )
  allocate(row_domain_id(count_reduc  ) )
  allocate(column_Domain_ID(count_reduc  ) )
  
  n = 0
  do i=1,size(obj%Index_I)
    if(obj%Index_I(i)==0 ) cycle
    n = n+1
    val(n) = obj%val(i)
    Index_I(n) = obj%Index_I(i)
    Index_J(n) = obj%Index_J(i)
    row_domain_id(n) = obj%row_domain_id(i)
    column_Domain_ID(n) = obj%column_Domain_ID(i)
  enddo

  deallocate(obj%val )
  deallocate(obj%Index_I )
  deallocate(obj%Index_J )
  deallocate(obj%row_domain_id )
  deallocate(obj%column_Domain_ID )

  obj%val=val
  obj%Index_I=Index_I
  obj%Index_J=Index_J
  obj%row_domain_id=row_domain_id
  obj%column_Domain_ID=column_Domain_ID


  obj%ReadyForFix = .true.
  if(present(debug) )then
    if(debug)then
      print *, "prepareFixLinearSolver >> [ok] Done"
    endif
  endif

end subroutine
!====================================================================================


!====================================================================================
subroutine solveLinearSolver(obj,Solver,MPI,OpenCL,CUDAC,preconditioning,CRS)
  class(LinearSolver_),intent(inout) :: obj
  character(*),intent(in) :: Solver
  logical,optional,intent(in) :: MPI, OpenCL, CUDAC,preconditioning,CRS
  integer(int32),allocatable :: Index_I(:),CRS_Index_I(:), Index_J(:),row_domain_id(:),column_Domain_ID(:)
  real(real64),allocatable:: val(:)
  integer(int32) :: i,m,n,rn,rd,cn,cd,same_n,count_reduc,j


  ! if not allocated COO format
  if(.not.allocated(obj%val) )then
    if(allocated(obj%a) .and. allocated(obj%b) )then
      ! it only has obj%a and obj%b
      n = size(obj%b)
      obj%x = zeros(n)
      if(Solver=="BiCGSTAB")then
        call bicgstab1d(a=obj%a, b=obj%b, x=obj%x, n=n, itrmax=obj%itrmax, er=obj%er0)
      elseif(Solver=="GPBiCG")then
        call bicgstab1d(a=obj%a, b=obj%b, x=obj%x, n=n, itrmax=obj%itrmax, er=obj%er0)
      elseif(Solver=="GaussJordan")then
        call gauss_jordan_pv(obj%a, obj%x, obj%b, size(obj%b,1) )
      else
        call gauss_jordan_pv(obj%a, obj%x, obj%b, size(obj%b,1) )
      endif
      return
    endif
  endif

  if(.not. allocated(obj%a) .and. .not. allocated(obj%val) )then
    print *, "solveLinearSolver >> ERROR :: .not. allocated(obj%b) "
    stop
  endif


  if(.not. allocated(obj%b) )then
    print *, "solveLinearSolver >> ERROR :: .not. allocated(obj%b) "
    stop
  endif


  if(.not. allocated(obj%x) )then
    allocate(obj%x( size(obj%b) ) )
    obj%x(:)=0.0d0
  endif

  
  if(size(obj%x) /=size(obj%b) )then
    deallocate(obj%x)
    allocate(obj%x( size(obj%b) ) )
    obj%x(:)=0.0d0
  endif
  
  ! No MPI, No OpenCl and No CUDAC
  if(allocated(obj%a) )then
    if(allocated(obj%b) )then
      if(allocated(obj%x))then
        ! run as non EBE-mode
        if(trim(Solver) == "GaussSeidel" )then
          call gauss_seidel(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0 )
        elseif(trim(Solver) == "GaussJordanPV" .or. trim(Solver) == "GaussJordan" )then
          call gauss_jordan_pv(obj%a, obj%x, obj%b, size(obj%a,1) )
        elseif(trim(Solver) == "BiCGSTAB" )then
          if(present(CRS) )then
            if(CRS .eqv. .true.)then
              if(.not.allocated(obj%CRS_val))then
                call obj%convertCOOtoCRS()
              endif
              call bicgstab_CRS(obj%val, obj%CRS_index_I, obj%CRS_index_J, obj%x, obj%b, obj%itrmax, obj%er0,obj%debug)
            elseif(allocated(obj%val) )then
              ! COO format
              if(allocated(obj%locked) )then
                call bicgstab_COO(obj%val, obj%index_I, obj%index_J, obj%x, obj%b, obj%itrmax, obj%er0,obj%debug, obj%locked)
              else
                call bicgstab_COO(obj%val, obj%index_I, obj%index_J, obj%x, obj%b, obj%itrmax, obj%er0,obj%debug)
              endif
            else
              call bicgstab1d(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)  
            endif
          else
            call bicgstab1d(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)
          endif
        elseif(trim(Solver) == "GPBiCG" )then
          if(present(preconditioning) )then
            if(preconditioning .eqv. .true.)then
              call preconditioned_GPBiCG(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)
            else
              call GPBiCG(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)
            endif
          else
            call GPBiCG(obj%a, obj%b, obj%x, size(obj%a,1), obj%itrmax, obj%er0)
          endif
        else
          print *, "LinearSolver_ ERROR:: no such solver as :: ",trim(Solver)
        endif
        return
      endif
    endif
  else
    if(allocated(obj%NumberOfNode) )then
      ! May be overlapped!
      ! remove overlap
      print *, "solveLinearSolver >> preparing..."
      Index_I = obj%Index_I
      Index_J = obj%Index_J
      do i=1, size(Index_I)
        m = obj%row_Domain_ID(i)
        if(m==1)then
          n = 0
        else
          n = sum(obj%NumberOfNode(1:m-1))*obj%DOF
        endif
        Index_I(i) = Index_I(i) + n
      enddo
      do i=1, size(Index_J)
        m = obj%column_Domain_ID(i)
        if(m==1)then
          n = 0
        else
          n = sum(obj%NumberOfNode(1:m-1))*obj%DOF
        endif
        Index_J(i) = Index_J(i) + n
      enddo
      print *, "solveLinearSolver >> start!"
      if(present(CRS) )then
        ! create CRS_Index_I from Index_I@COO

        if(CRS)then
          call bicgstab_CRS(obj%val, CRS_index_I, index_J, obj%x, obj%b, obj%itrmax, obj%er0, obj%debug)
          return
        endif

      endif
      if(Solver=="BiCGSTAB")then
        if(allocated(obj%locked) )then
          call bicgstab_COO(obj%val, index_I, index_J, obj%x, obj%b, obj%itrmax, obj%er0, obj%debug,obj%locked)
        else
          call bicgstab_COO(obj%val, index_I, index_J, obj%x, obj%b, obj%itrmax, obj%er0, obj%debug)
        endif
      else
        call GaussJordan_COO(obj%val, index_I, index_J, obj%x, obj%b)
      endif
    else
      if(present(CRS) )then
        if(CRS)then
          call bicgstab_CRS(obj%val, CRS_index_I, index_J, obj%x, obj%b, obj%itrmax, obj%er0, obj%debug)
          return
        endif
      endif
      if(Solver=="BiCGSTAB")then
        if(allocated(obj%locked) )then
          call bicgstab_COO(obj%val, obj%index_I, obj%index_J, obj%x, obj%b, obj%itrmax, obj%er0, obj%debug&
            ,obj%locked)
        else
          call bicgstab_COO(obj%val, obj%index_I, obj%index_J, obj%x, obj%b, obj%itrmax, obj%er0, obj%debug)
        endif
      else
        call GaussJordan_COO(obj%val, obj%index_I, obj%index_J, obj%x, obj%b)
      endif
    endif
    return
  endif

  print *, "LinearSolver_ ERROR:: EBE-mode is not implemented yet."
  stop 
end subroutine solveLinearSolver
!====================================================================================


!====================================================================================
subroutine gauss_seidel(a, b, x, n, itrmax, er0)
  integer(int32), intent(in) :: n, itrmax
  real(real64), intent(in)  :: a(n, n), b(n), er0
  real(real64), intent(out) :: x(n)
  real(real64) s, er, rd(n), r(n)
  integer(int32) i, itr
  do i = 1, n
    if (a(i, i) == 0.0d0)  stop  'a(i, i) == 0.0d0'
    rd(i) = 1.0d0 / a(i, i)
  enddo
  x(1:n) = 0.0d0
  do itr = 1, itrmax
    do i = 1,n
      s = dot_product(a(i, 1 :i-1), x(1: i-1))
      s = s + dot_product(a(i, i + 1:n), x(i+1:n))
      x(i) = rd(i) * (b(i) - s)
    enddo
    r(1:n) = b(1:n) - matmul(a,x)
    er = dot_product(r, r)
    if(er <= er0) then
      write(20,*) '# converged#'
      exit
    endif
  enddo
 end subroutine gauss_seidel
!===================================================================================
 subroutine GaussJordan_COO(a0, index_i, index_j, x, b)
  integer(int32) :: n
  real(real64), intent(in) :: a0(:), b(:)
  integer(int32),intent(in) :: index_i(:),index_j(:)
  real(real64), allocatable,intent(inout) :: x(:)
  integer(int32) i, j, jj,k, m,nn, mm, id1,id2
  real(real64),allocatable :: a(:,:), w(:)
  real(real32) ar, am, t

  print *, "Solver :: GaussJordan"
  ! CAUTION this code is not optimized.
  ! It just converts a COO-formatted sparse matrix
  ! to Dense matrix, and pass them to 
  ! Gauss_jordan_pv_real64
  n = size(b)
  a = zeros(n,n)
  x = zeros(n)
  print *, "DOF = ",n
  do i=1, size(index_i)
    a( index_i(i),index_j(i) ) = a0(i)
  enddo

  nn = n
  x(:) = b(:)
  do k = 1, n
     m = k
     am = abs(a(k,k))
     do i = k+1, n
        if (abs(a(i,k)) > am) then
          am = abs(a(i,k))
          m = i
        endif
     enddo
     if (am == 0.0d0)   stop  ' A is singular '
     if ( k /= m) then
       w(k:n) = a(k, k:n)
       a(k,k:n) = a(m, k:n)
       a(m, k:n) =w(k:n)
       t = x(k)
       x(k) = x(m)
       x(m) = t
     endif
     ! gauss_jordan
     if (a(k, k) == 0.0d0)  stop  'devide by zero3gauss_jordan_pv'
	    ar = 1.0d0 / a(k,k)
     a(k,k) = 1.0d0
     a(k,k+1:n) = ar * a(k, k+1:n)
     x(k) = ar * x(k)
     do i= 1, n
       if (i /= k) then
         a(i, k+1:n) = a(i, K+1:n) - a(i,k) * a(k, k+1:n)
         x(i) = x(i) - a(i,k) * x(k)
         a(i,k) = 0.0d0
       endif
     enddo
  enddo
  

  return

!
!
!
!
!  nn = size(b,1)
!  n  = size(b,1)
!  a = a0
!  w = zeros(n)
!  x = b
!
!  do k = 1, n
!     m = k
!
!     ! am = A(k,k)
!     am = 0.0d0
!     do j=1,size(index_i)
!      if(index_i(j)==k .and. index_j(j)==k )then
!        am = abs(a(j))
!        exit
!      endif
!     enddo
!     
!     !
!     !do i = k+1, n
!     ! if (abs(a(i,k)) > am) then
!     !   am = abs(a(i,k))
!     !   m = i
!     ! endif
!     !enddo
!
!     do j=1,size(index_i)
!      if(index_i(j)>=k+1 .and.  k == index_j(j) )then
!        if( abs(a(j)) > am )then
!          am = abs(a(j) )
!          m = index_i(j)
!        endif
!      endif
!     enddo
!     
!     
!
!     if (am == 0.0d0)   stop  ' A is singular '
!
!     !if ( k /= m) then 
!     !  w(k:n) = a(k, k:n)
!     !  a(k,k:n) = a(m, k:n)
!     !  a(m, k:n) =w(k:n)
!     !  t = x(k)
!     !  x(k) = x(m)
!     !  x(m) = t
!     !endif
!     if( k/=m)then
!      !(1)  w(k:n) = a(k, k:n)
!      do j=1,size(index_i)
!        id1 = index_i(j)
!        if(id1==k)then
!          id2 = index_j(j)
!          if(k<=id2 .and. id2 <= n)then  
!            w(id2) = a(j)
!          endif
!        enddo
!      enddo
!      !(2)  a(k,k:n) = a(m, k:n)
!      do j=1,size(index_i)
!        id1 = index_i(j)
!        if(id1==k)then
!          id2 = index_j(j)
!          if(k<=id2 .and. id2 <= n)then  
!            w(id2) = a(j)
!          endif
!        enddo
!      enddo
!      
!
!
!     endif
!     
!     
!     ! gauss_jordan
!     if (a(k, k) == 0.0d0)  stop  'devide by zero3gauss_jordan_pv'
!	    ar = 1.0d0 / a(k,k)
!     a(k,k) = 1.0d0
!     a(k,k+1:n) = ar * a(k, k+1:n)
!     x(k) = ar * x(k)
!     do i= 1, n
!       if (i /= k) then
!         a(i, k+1:n) = a(i, K+1:n) - a(i,k) * a(k, k+1:n)
!         x(i) = x(i) - a(i,k) * x(k)
!         a(i,k) = 0.0d0
!       endif
!     enddo
!  enddo
  
 end subroutine 
!===========================================================================

!===================================================================================
subroutine gauss_jordan_pv_real64(a0, x, b, n)
  integer(int32), intent(in) :: n
  real(real64), intent(in) :: a0(n,n), b(n)
  real(real64), intent(inout) :: x(n)
  integer(int32) i, j, k, m,nn, mm
  real(real64) ar, am, t, a(n,n), w(n)
  nn = size(a0,1)

  a(:,:)= a0(:,:)
  x(:) = b(:)
  do k = 1, n
     m = k
     am = abs(a(k,k))
     do i = k+1, n
        if (abs(a(i,k)) > am) then
          am = abs(a(i,k))
          m = i
        endif
     enddo
     if (am == 0.0d0)   stop  ' A is singular '
     if ( k /= m) then
       w(k:n) = a(k, k:n)
       a(k,k:n) = a(m, k:n)
       a(m, k:n) =w(k:n)
       t = x(k)
       x(k) = x(m)
       x(m) = t
     endif
     ! gauss_jordan
     if (a(k, k) == 0.0d0)  stop  'devide by zero3gauss_jordan_pv'
	    ar = 1.0d0 / a(k,k)
     a(k,k) = 1.0d0
     a(k,k+1:n) = ar * a(k, k+1:n)
     x(k) = ar * x(k)
     do i= 1, n
       if (i /= k) then
         a(i, k+1:n) = a(i, K+1:n) - a(i,k) * a(k, k+1:n)
         x(i) = x(i) - a(i,k) * x(k)
         a(i,k) = 0.0d0
       endif
     enddo
  enddo
  
 end subroutine 
!===========================================================================

!===================================================================================
 subroutine gauss_jordan_pv_real32(a0, x, b, n)
  integer(int32), intent(in) :: n
  real(real32), intent(in) :: a0(n,n), b(n)
  real(real32), intent(out) :: x(n)
  integer(int32) i, j, k, m,nn, mm
  real(real32) ar, am, t, a(n,n), w(n)
  nn = size(a0,1)

  a(:,:)= a0(:,:)
  x(:) = b(:)
  do k = 1, n
     m = k
     am = abs(a(k,k))
     do i = k+1, n
        if (abs(a(i,k)) > am) then
          am = abs(a(i,k))
          m = i
        endif
     enddo
     if (am == 0.0d0)   stop  ' A is singular '
     if ( k /= m) then
       w(k:n) = a(k, k:n)
       a(k,k:n) = a(m, k:n)
       a(m, k:n) =w(k:n)
       t = x(k)
       x(k) = x(m)
       x(m) = t
     endif
     ! gauss_jordan
     if (a(k, k) == 0.0d0)  stop  'devide by zero3gauss_jordan_pv'
	    ar = 1.0d0 / a(k,k)
     a(k,k) = 1.0d0
     a(k,k+1:n) = ar * a(k, k+1:n)
     x(k) = ar * x(k)
     do i= 1, n
       if (i /= k) then
         a(i, k+1:n) = a(i, K+1:n) - a(i,k) * a(k, k+1:n)
         x(i) = x(i) - a(i,k) * x(k)
         a(i,k) = 0.0d0
       endif
     enddo
  enddo
  
 end subroutine 
!===========================================================================

!===================================================================================
subroutine gauss_jordan_pv_complex64(a0, x, b, n)
  integer(int32), intent(in) :: n
  complex(real64), intent(in) :: a0(n,n), b(n)
  complex(real64), intent(out) :: x(n)
  integer(int32) i, j, k, m,nn, mm
  complex(real64) ar, am, t, a(n,n), w(n)
  nn = size(a0,1)

  a(:,:)= a0(:,:)
  x(:) = b(:)
  do k = 1, n
     m = k
     am = abs(a(k,k))
     do i = k+1, n
        if (abs(a(i,k)) > abs(am)) then
          am = abs(a(i,k))
          m = i
        endif
     enddo
     if (am == 0.0d0)   stop  ' A is singular '
     if ( k /= m) then
       w(k:n) = a(k, k:n)
       a(k,k:n) = a(m, k:n)
       a(m, k:n) =w(k:n)
       t = x(k)
       x(k) = x(m)
       x(m) = t
     endif
     ! gauss_jordan
     if (a(k, k) == 0.0d0)  stop  'devide by zero3gauss_jordan_pv'
	    ar = 1.0d0 / a(k,k)
     a(k,k) = 1.0d0
     a(k,k+1:n) = ar * a(k, k+1:n)
     x(k) = ar * x(k)
     do i= 1, n
       if (i /= k) then
         a(i, k+1:n) = a(i, K+1:n) - a(i,k) * a(k, k+1:n)
         x(i) = x(i) - a(i,k) * x(k)
         a(i,k) = 0.0d0
       endif
     enddo
  enddo
  
 end subroutine 
!===========================================================================

 subroutine bicgstab_diffusion(a, b, x, n, itrmax, er,DBC,DBCVal)
  integer(int32), intent(in) :: n, itrmax,DBC(:,:)
  real(real64), intent(in) :: a(n,n), b(n), er,DBCVal(:,:)
  real(real64), intent(inout) :: x(n)
     integer(int32) itr,i,j
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-8
   r(:) = b - matmul(a,x)
   ! if DBC => reset residual
   do i=1,size(DBC,1)
    do j=1,size(DBC,2)
      if(DBC(i,j)<1 )then
        cycle
      else
        r( DBC(i,j) )=0.0d0
        x(DBC(i,j)  )=DBCVal(i,j)
      endif
    enddo
   enddo
   
   !
   c1 = dot_product(r,r)
	 init_rr=c1
     if (c1 ==0.0d0) then
      print *, "Caution :: Initial residual is zero"
      return
     endif
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        !c1 = dot_product(r0,r)
        y(:) = matmul(a,p)
        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		    c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
        
        ! if DBC => reset residual
        do i=1,size(DBC,1)
         do j=1,size(DBC,2)
           if(DBC(i,j)<1 )then
             cycle
           else
             r( DBC(i,j) )=0.0d0
             x(DBC(i,j)  )=DBCVal(i,j)
           endif
         enddo
        enddo
        rr = dot_product(r,r)
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) then
          
          print *, "[ok] :: BICGSTAB is converged in ",i," steps."
          exit
        endif

        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
        if(itrmax==itr)then
          print *, "ERROR :: BICGSTAB did not converge"
          return
        endif
     enddo

 end subroutine bicgstab_diffusion
!===============================================================

 subroutine bicgstab_CRS(a, ptr_i, index_j, x, b, itrmax, er, debug)
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
    
    n=size(b)
    if(speak) print *, "BiCGSTAB STARTED >> DOF:", n
    allocate(r(n), r0(n), p(n), y(n), e(n), v(n))

    r(:) = b(:)
    if(speak) print *, "BiCGSTAB >> [1] initialize"

    ax = crs_matvec(CRS_value=a,CRS_col=index_j,&
        CRS_row_ptr=ptr_i,old_vector=x)
    r = b - ax

    
    if(speak) print *, "BiCGSTAB >> [2] dp1"

    c1 = dot_product(r,r)

    init_rr=c1
    if(speak) print *, "BiCGSTAB >>      |r|^2 = ",init_rr
    
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


 !===============================================================

subroutine bicgstab_COO(a, index_i, index_j, x, b, itrmax, er, debug,locked)
  integer(int32), intent(inout) :: index_i(:),index_j(:), itrmax
  real(real64), intent(inout) :: a(:), b(:), er
  real(real64), intent(inout) :: x(:)
  logical,optional,intent(in) :: debug,locked(:)
  logical :: speak = .false.
  integer(int32) itr,i,j,n
  real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
  real(real64),allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:)

  print *, "[ok] BiCGSTAB for COO  started."

  if(present(debug) )then
    speak = debug
  endif

  if(speak) print *, "BiCGSTAB STARTED >> DOF:", n
  n=size(b)
  allocate(r(n), r0(n), p(n), y(n), e(n), v(n))
  er0=er
  r(:) = b(:)
  if(speak) print *, "BiCGSTAB >> [1] initialize"
  
  do i=1,size(a)
    if(index_i(i) <=0) cycle
    r( index_i(i) ) = r( index_i(i) ) - a(i)*x( index_j(i) ) 
  enddo
  
  ! >> if fixed >> r=0, x=b
  if(present(locked ) )then
    !$OMP parallel do private(i)
    do i=1,size(locked)
      if(locked(i) )then
        r(i) = 0.0d0
        x(i) = b(i)
      endif
    enddo
    !$OMP end parallel do
  endif

  !r(:) = b - matmul(a,x)
  if(speak) print *, "BiCGSTAB >> [2] dp1"
  c1 = dot_product(r,r)
	init_rr=c1
  if (c1 < er0) return
  p(:) = r(:)
  r0(:) = r(:)
  do itr = 1, itrmax   
    
    if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] initialize"
    c1 = dot_product(r0,r)
    
    !y(:) = matmul(a,p)
    y(:)=0.0d0

    !!$OMP parallel do reduction(+:y) private(i)
    do i=1,size(a)
      if(index_i(i) <=0) then
        ! do nothing
      else
        y( index_i(i) ) = y( index_i(i) ) + a(i)*p( index_j(i) ) 
      endif
    enddo
    !!$OMP end parallel do
    
    c2 = dot_product(r0,y)
    alp = c1/c2
    e(:) = r(:) - alp * y(:)
    !v(:) = matmul(a,e)
    
    if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] half"


    v(:)=0.0d0

    !!$OMP parallel do reduction(+:v) private(i)
    do i=1,size(a)
      if(index_i(i) <=0) then
        ! do nothing
      else
        v( index_i(i) ) = v( index_i(i) ) + a(i)*e( index_j(i) ) 
      endif
    enddo
    !!$OMP end parallel do 

    ev = dot_product(e,v)
    vv = dot_product(v,v)

    if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		c3 = ev / vv
    x(:) = x(:) + alp * p(:) + c3 * e(:)
    r(:) = e(:) - c3 * v(:)
    
    ! >> if fixed >> r=0, x=b
    if(present(locked ) )then
      !$OMP parallel do private(i)
      do i=1,size(locked)
        if(locked(i) )then
          r(i) = 0.0d0
          x(i) = b(i)
        endif
      enddo
      !$OMP end parallel do
    endif
    
    rr = dot_product(r,r)
    
    
    if(speak) print *, 'itr, |er|/|er0| =', itr,rr/init_rr
    if (rr/init_rr < er0) exit
    c1 = dot_product(r0,r)
    bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
    p(:) = r(:) + bet * (p(:) -c3*y(:) )

  enddo

  print *, "[ok] BiCGSTAB_COO >> error norm",rr/init_rr
 end subroutine 
!===============================================================


!===============================================================
 subroutine bicgstab_real64(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n, itrmax
  real(real64), intent(in) :: a(n,n), b(n), er
  real(real64), intent(inout) :: x(n)
     integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=er

	 r(:) = b - matmul(a,x)
     c1 = dot_product(r,r)
	 init_rr=c1
     if (c1 < er0) return
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        c1 = dot_product(r0,r)


        y(:) = matmul(a,p)

        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
	    	c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
        rr = dot_product(r,r)

        
    
    
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine 
!===============================================================

!===============================================================
 subroutine bicgstab_real32(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n, itrmax
  real(real32), intent(in) :: a(n,n), b(n), er
  real(real32), intent(inout) :: x(n)
     integer(int32) itr
     real(real32) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real32) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=er

	 r(:) = b - matmul(a,x)
     c1 = dot_product(r,r)
	 init_rr=c1
     if (c1 < er0) return
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        c1 = dot_product(r0,r)


        y(:) = matmul(a,p)

        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
	    	c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
        rr = dot_product(r,r)

        
    
    
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine 
!===============================================================

!===============================================================
 subroutine bicgstab_complex64(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n, itrmax
  complex(real64), intent(in) :: a(n,n), b(n), er
  complex(real64), intent(inout) :: x(n)
     integer(int32) itr
     complex(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     complex(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=er

	 r(:) = b - matmul(a,x)
     c1 = dot_product(r,r)
	 init_rr=c1
     if (abs(c1) < abs(er0)) return
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        c1 = dot_product(r0,r)


        y(:) = matmul(a,p)

        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
	    	c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
        rr = dot_product(r,r)

        
    
    
    !    write(*,*) 'itr, er =', itr,rr
        if ( abs(rr/init_rr) < abs(er0)) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine 
!===============================================================


!===============================================================
subroutine bicgstab1d(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n, itrmax
  real(real64), intent(in) :: a(n,n), b(n), er
  real(real64), intent(inout) :: x(n)
     integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=er

	 r(:) = b - matmul(a,x)
     c1 = dot_product(r,r)
	 init_rr=c1
     if (c1 < er0) return
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        c1 = dot_product(r0,r)


        y(:) = matmul(a,p)

        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
	    	c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
        rr = dot_product(r,r)

        
    
    
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine bicgstab1d
!===============================================================

subroutine bicgstab_nr(a, b, x, n, itrmax, er,u_nod_x, u_nod_y)
  integer(int32), intent(in) :: n, itrmax,u_nod_x(:),u_nod_y(:)
  real(real64), intent(in) :: a(n,n),b(n), er
  real(real64), intent(inout) :: x(n)
  integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-14
	 
	 r(:) = b - matmul(a,x)
	 
	 call modify_residual(r, u_nod_x, u_nod_y)
     
	 c1 = dot_product(r,r)
	 
	 init_rr=c1
     
	 if (c1 < er0) return
     
	 p(:) = r(:)
     
	 r0(:) = r(:)
     
	 do itr = 1, itrmax
        y(:) = matmul(a,p)
        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
		call modify_residual(r, u_nod_x, u_nod_y)
        rr = dot_product(r,r)
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine bicgstab_nr
!====================================================================================
subroutine bicgstab_nr1(a, b, x, n, itrmax, er,u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
  integer(int32), intent(in) :: n, itrmax,u_nod_x(:),u_nod_y(:)
  real(real64), intent(in) :: a(n,n),b(n), er,u_nod_dis_x(:),u_nod_dis_y(:)
  real(real64), intent(inout) :: x(n)
     integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-14
	 r(:) = b - matmul(a,x)
	 call modify_residual_1(r,x, u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
     c1 = dot_product(r,r)
	 init_rr=c1
     if (c1 < er0) return
     p(:) = r(:)
     r0(:) = r(:)
     do itr = 1, itrmax
        y(:) = matmul(a,p)
        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
		call modify_residual_1(r,x, u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
        rr = dot_product(r,r)
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine bicgstab_nr1
!====================================================================================

subroutine bicgstab_dirichlet(a, b, x, n, itrmax, er,DBoundNodID, DBoundVal,SetBC)
  integer(int32), intent(in) :: n, itrmax,DBoundNodID(:,:),SetBC
  real(real64), intent(in) :: a(n,n),b(n), er,DBoundVal(:,:)
  real(real64), intent(inout) :: x(n)
     integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=1.00e-14
	 
	 r(:) = b - matmul(a,x)
	 
	 call modify_residual_dirichlet(r,x, DBoundNodID, DBoundVal,SetBC)
     
	 c1 = dot_product(r,r)
	 
	 init_rr=c1
     
	 if (c1 < er0) return
     
	 p(:) = r(:)
     
	 r0(:) = r(:)
     
	 do itr = 1, itrmax
        y(:) = matmul(a,p)
        c2 = dot_product(r0,y)
        alp = c1/c2
        e(:) = r(:) - alp * y(:)
        v(:) = matmul(a,e)
        ev = dot_product(e,v)
        vv = dot_product(v,v)
        if(  vv==0.0d0 ) stop "Bicgstab devide by zero"
		c3 = ev / vv
        x(:) = x(:) + alp * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
		call modify_residual_dirichlet(r,x, DBoundNodID, DBoundVal,SetBC)
        rr = dot_product(r,r)
    !    write(*,*) 'itr, er =', itr,rr
        if (rr/init_rr < er0) exit
        c1 = dot_product(r0,r)
        bet = c1 / (c2 * c3)
		if(  (c2 * c3)==0.0d0 ) stop "Bicgstab devide by zero"
		
        p(:) = r(:) + bet * (p(:) -c3*y(:) )
     enddo
 end subroutine bicgstab_dirichlet
!====================================================================================


subroutine modify_residual_1(r,x, u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
	integer(int32),intent(in)::u_nod_x(:),u_nod_y(:)
	real(real64), intent(in) :: u_nod_dis_x(:),u_nod_dis_y(:)
	real(real64),intent(inout)::r(:),x(:)
	 integer(int32) i
	 
	 do i=1,size(u_nod_x)

		r( 2*u_nod_x(i)-1 )=0.0d0
		x( 2*u_nod_x(i)-1 )=u_nod_dis_x(i)
	 enddo
	 
	 do i=1,size(u_nod_y)

		r( 2*u_nod_y(i) )=0.0d0
		x( 2*u_nod_y(i) )=u_nod_dis_y(i)
	 enddo
	
  end subroutine modify_residual_1
!====================================================================================
subroutine modify_residual(r, u_nod_x, u_nod_y)
	integer(int32),intent(in)::u_nod_x(:),u_nod_y(:)
	real(real64),intent(inout)::r(:)
	 integer(int32) i
	 
	 do i=1,size(u_nod_x)

		r( 2*u_nod_x(i)-1 )=0.0d0
	 enddo
	 
	 do i=1,size(u_nod_y)

		r( 2*u_nod_y(i) )=0.0d0
	 enddo
	
  end subroutine modify_residual
!====================================================================================
subroutine modify_residual_dirichlet(r,x, DBoundNodID, DBoundVal,SetBC)
  integer(int32),intent(in)::DBoundNodID(:,:),SetBC
  real(real64),intent(in)::DBoundVal(:,:)
	real(real64),intent(inout)::r(:),x(:)

  
	integer(int32) :: i,j,k,dim_num,dbc_num
	real(real64) :: val

  if(SetBC==1)then

	  dim_num=size(DBoundNodID,2)
	  dbc_num=size(DBoundNodID,1)
    
	  do i=1,dim_num
	  	do j=1,dbc_num
	  		k=DBoundNodID(j,i)
	  		val=DBoundVal(j,i)
	  		if(k<1)then
	  			cycle
	  		elseif(k>=1)then
          x(dim_num*(k-1)+i)=val*dble(SetBC)
          r(dim_num*(k-1)+i)=0.0d0
	  		else
	  			cycle
	  		endif
	  	enddo
    enddo
  else
    
	  dim_num=size(DBoundNodID,2)
	  dbc_num=size(DBoundNodID,1)
    
	  do i=1,dim_num
	  	do j=1,dbc_num
	  		k=DBoundNodID(j,i)
	  		val=DBoundVal(j,i)
	  		if(k<1)then
	  			cycle
	  		elseif(k>=1)then
          r(dim_num*(k-1)+i)=0.0d0
          x(dim_num*(k-1)+i)=0.0d0
          
	  		else
	  			cycle
	  		endif
	  	enddo
    enddo
  endif
   

end subroutine
!====================================================================================

!===========================================================================
subroutine GPBiCG_real64(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n
  integer(int32),optional,intent(in) :: itrmax
  real(real64), intent(in) :: a(n,n), b(n)
  real(real64), optional,intent(in)::er
  real(real64), intent(inout) :: x(n)
     integer(int32) itr,itrmax_
     real(real64) alp,c1, rr,er0,init_rr,beta
     real(real64) gzi,nu,val1,val2,r0rk,eps
     real(real64) r(n), r0(n), p(n), y(n),ap(n),q(n)
     real(real64) u(n),w(n),t(n),t_(n),z(n)
     eps=1.0e-18
     er0=input(default=eps,option=er)

    r(:) = b - matmul(a,x)
    c1 = dot_product(r,r)
    init_rr=c1
    if (c1 < er0) return
    beta=0.0d0
    p(:) = 0.0d0
    r0(:) = r(:)
    w(:) = 0.0d0
    u(:) = 0.0d0
    t(:) = 0.0d0
    t_(:)= 0.0d0
    z(:) = 0.0d0
    itrmax_=input(default=1000,option=itrmax)
    do itr = 1, itrmax_
      p(:) = r(:)+beta*(p(:)-u(:) )  ! triple checked.

      ap(:)   = matmul(a,p) ! triple checked. ap=v,
      alp  = dot_product(r0,r )/dot_product(r0,ap )! triple checked.
      y(:) = t(:) - r(:) - alp*w(:) + alp*ap(:) !triple checked.
      
      t(:) = r(:) - alp*ap(:) ! triple checked.

      q(:) = matmul(a,t) ! triple checked. s=q=at=c
      
      r0rk=dot_product(r0,r)
      if(itr==1)then
        gzi  = dot_product(q,t)/dot_product(q,q)! double checked.
        nu   = 0.0d0 ! double checked.
      else
        val1 = dot_product(y,y)*dot_product(q,t) - dot_product(y,t)*dot_product(q,y)
        val2 = dot_product(q,q)*dot_product(y,y) - dot_product(y,q)*dot_product(q,y)
        
        if(  val2==0.0d0 ) then
          !print *, itr
          !print *,  r(:), alp*ap(:) 
          stop "GPBiCG devide by zero"
        endif
        gzi  = val1/val2
        val1 = dot_product(q,q)*dot_product(y,t) - dot_product(y,q)*dot_product(q,t)
        nu  = val1/val2  !triple checked.
      endif
      
      u(:) = gzi*ap + nu*(t_(:) -r(:) + beta*u(:)  ) !double checked.
      z(:) = gzi*r(:) + nu*z(:) - alp*u(:)!double checked.
      x(:) = x(:) +alp*p(:) +z(:) !double checked.

      r(:) = t(:) -nu*y(:) -gzi*q(:)!double checked.
      t_(:)=t(:)
      rr = dot_product(r,r)
      !print *, 'itr, er =', itr,sqrt(rr),sqrt(rr)/sqrt(init_rr)
      !print *, "ans = ",x(:)
      if (sqrt(rr)/sqrt(init_rr) < er0 ) exit
      r0rk=dot_product(r0,r)
      beta=alp/gzi*dot_product(r0,r)/r0rk
      w(:) = q(:) + beta * ap(:)
      if(itr==itrmax_) then
        print *, "ERROR :: GPBiCG did not converge."
      endif
    enddo
 end subroutine 
!===============================================================

!===========================================================================
 subroutine GPBiCG_real32(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n
  integer(int32),optional,intent(in) :: itrmax
  real(real32), intent(in) :: a(n,n), b(n)
  real(real32), optional,intent(in)::er
  real(real32), intent(inout) :: x(n)
     integer(int32) itr,itrmax_
     real(real32) alp,c1, rr,er0,init_rr,beta
     real(real32) gzi,nu,val1,val2,r0rk,eps
     real(real32) r(n), r0(n), p(n), y(n),ap(n),q(n)
     real(real32) u(n),w(n),t(n),t_(n),z(n)
     eps=1.0e-18
     er0=input(default=eps,option=er)

    r(:) = b - matmul(a,x)
    c1 = dot_product(r,r)
    init_rr=c1
    if (c1 < er0) return
    beta=0.0d0
    p(:) = 0.0d0
    r0(:) = r(:)
    w(:) = 0.0d0
    u(:) = 0.0d0
    t(:) = 0.0d0
    t_(:)= 0.0d0
    z(:) = 0.0d0
    itrmax_=input(default=1000,option=itrmax)
    do itr = 1, itrmax_
      p(:) = r(:)+beta*(p(:)-u(:) )  ! triple checked.

      ap(:)   = matmul(a,p) ! triple checked. ap=v,
      alp  = dot_product(r0,r )/dot_product(r0,ap )! triple checked.
      y(:) = t(:) - r(:) - alp*w(:) + alp*ap(:) !triple checked.
      
      t(:) = r(:) - alp*ap(:) ! triple checked.

      q(:) = matmul(a,t) ! triple checked. s=q=at=c
      
      r0rk=dot_product(r0,r)
      if(itr==1)then
        gzi  = dot_product(q,t)/dot_product(q,q)! double checked.
        nu   = 0.0d0 ! double checked.
      else
        val1 = dot_product(y,y)*dot_product(q,t) - dot_product(y,t)*dot_product(q,y)
        val2 = dot_product(q,q)*dot_product(y,y) - dot_product(y,q)*dot_product(q,y)
        
        if(  val2==0.0d0 ) then
          !print *, itr
          !print *,  r(:), alp*ap(:) 
          stop "GPBiCG devide by zero"
        endif
        gzi  = val1/val2
        val1 = dot_product(q,q)*dot_product(y,t) - dot_product(y,q)*dot_product(q,t)
        nu  = val1/val2  !triple checked.
      endif
      
      u(:) = gzi*ap + nu*(t_(:) -r(:) + beta*u(:)  ) !double checked.
      z(:) = gzi*r(:) + nu*z(:) - alp*u(:)!double checked.
      x(:) = x(:) +alp*p(:) +z(:) !double checked.

      r(:) = t(:) -nu*y(:) -gzi*q(:)!double checked.
      t_(:)=t(:)
      rr = dot_product(r,r)
      !print *, 'itr, er =', itr,sqrt(rr),sqrt(rr)/sqrt(init_rr)
      !print *, "ans = ",x(:)
      if (sqrt(rr)/sqrt(init_rr) < er0 ) exit
      r0rk=dot_product(r0,r)
      beta=alp/gzi*dot_product(r0,r)/r0rk
      w(:) = q(:) + beta * ap(:)
      if(itr==itrmax_) then
        print *, "ERROR :: GPBiCG did not converge."
      endif
    enddo
 end subroutine 
!===============================================================

!===========================================================================
 subroutine GPBiCG_complex64(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n
  integer(int32),optional,intent(in) :: itrmax
  complex(real64), intent(in) :: a(n,n), b(n)
  complex(real64), optional,intent(in)::er
  complex(real64), intent(inout) :: x(n)
     integer(int32) itr,itrmax_
     complex(real64) alp,c1, rr,er0,init_rr,beta
     complex(real64) gzi,nu,val1,val2,r0rk,eps
     complex(real64) r(n), r0(n), p(n), y(n),ap(n),q(n)
     complex(real64) u(n),w(n),t(n),t_(n),z(n)
     eps=1.0e-18
     er0=input(default=eps,option=er)

    r(:) = b - matmul(a,x)
    c1 = dot_product(r,r)
    init_rr=c1
    if (abs(c1) < abs(er0)) return
    beta=0.0d0
    p(:) = 0.0d0
    r0(:) = r(:)
    w(:) = 0.0d0
    u(:) = 0.0d0
    t(:) = 0.0d0
    t_(:)= 0.0d0
    z(:) = 0.0d0
    itrmax_=input(default=1000,option=itrmax)
    do itr = 1, itrmax_
      p(:) = r(:)+beta*(p(:)-u(:) )  ! triple checked.

      ap(:)   = matmul(a,p) ! triple checked. ap=v,
      alp  = dot_product(r0,r )/dot_product(r0,ap )! triple checked.
      y(:) = t(:) - r(:) - alp*w(:) + alp*ap(:) !triple checked.
      
      t(:) = r(:) - alp*ap(:) ! triple checked.

      q(:) = matmul(a,t) ! triple checked. s=q=at=c
      
      r0rk=dot_product(r0,r)
      if(itr==1)then
        gzi  = dot_product(q,t)/dot_product(q,q)! double checked.
        nu   = 0.0d0 ! double checked.
      else
        val1 = dot_product(y,y)*dot_product(q,t) - dot_product(y,t)*dot_product(q,y)
        val2 = dot_product(q,q)*dot_product(y,y) - dot_product(y,q)*dot_product(q,y)
        
        if(  val2==0.0d0 ) then
          !print *, itr
          !print *,  r(:), alp*ap(:) 
          stop "GPBiCG devide by zero"
        endif
        gzi  = val1/val2
        val1 = dot_product(q,q)*dot_product(y,t) - dot_product(y,q)*dot_product(q,t)
        nu  = val1/val2  !triple checked.
      endif
      
      u(:) = gzi*ap + nu*(t_(:) -r(:) + beta*u(:)  ) !double checked.
      z(:) = gzi*r(:) + nu*z(:) - alp*u(:)!double checked.
      x(:) = x(:) +alp*p(:) +z(:) !double checked.

      r(:) = t(:) -nu*y(:) -gzi*q(:)!double checked.
      t_(:)=t(:)
      rr = dot_product(r,r)
      !print *, 'itr, er =', itr,sqrt(rr),sqrt(rr)/sqrt(init_rr)
      !print *, "ans = ",x(:)
      if (abs(sqrt(rr)/sqrt(init_rr)) < abs(er0) ) exit
      r0rk=dot_product(r0,r)
      beta=alp/gzi*dot_product(r0,r)/r0rk
      w(:) = q(:) + beta * ap(:)
      if(itr==itrmax_) then
        print *, "ERROR :: GPBiCG did not converge."
      endif
    enddo
 end subroutine 
!===============================================================




 
!===========================================================================
subroutine preconditioned_GPBiCG(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n
  integer(int32),optional,intent(in) :: itrmax
  real(real64), intent(in) :: a(1:n,1:n), b(1:n)
  real(real64), optional,intent(in)::er
  real(real64), intent(inout) :: x(1:n)
  real(real64) :: L(1:n,1:n),d(1:n)
    ! presented by Moe Thuthu et al., 2009, algorithm #3
    integer(int32) itr,itrmax_,i,j,k
    real(real64) alp,c1, rr,er0,init_rr,beta,lld,ld
    real(real64) gzi,nu,val1,val2,r0rk,eps
    real(real64) r(n),rk(n),r_(n),r_k(n), r0(n), p(n), y(n),ap(n),q(n)
    real(real64) u(n),w(n),t(n),t_(n),z(n)

    print *, "<<< Under implementation >>>"
    
    ! Incomplete Cholosky decomposition.
    ! http://www.slis.tsukuba.ac.jp/~fujisawa.makoto.fu/cgi-bin/wiki/index.php?%A5%B3%A5%EC%A5%B9%A5%AD%A1%BC%CA%AC%B2%F2
    ! >>>>>>>>>>
    L(:,:) = 0.0d0
    d(:) = 0.0d0
    d(1) = a(1,1)
    L(1,1) = 1.0d0
    L(:,:) = imcompleteCholosky(a)
    call showArray(L)
    
    !do i=2, n
    !  ! i < kの場合
    !  do j=1, i
    !    if( abs(a(i,j)) < dble(1.0e-10)  )then
    !      cycle
    !    else
    !      lld = a(i,j)
    !      do k=1, j
    !        lld = lld - L(i,k)*L(j,k)*d(k)
    !      enddo
    !      if(d(j)==0.0d0)then
    !        stop "Error :: d(j)==0.0d0"
    !      endif
    !      L(i,j) = 1.0d0/d(j)*lld
    !    endif
    !    ld = a(i,i)
    !    do k=1, i
    !      ld = ld - L(i,k)*L(i,k)*d(k)
    !    enddo
    !    d(i) = ld
    !    L(i,i) = 1.0d0
    !  enddo
    !  
    !enddo
!
    print *, d

    ! <<<<<<<<<<

    eps=dble(1.00e-14)
    er0=input(default=eps,option=er)

    r(:) = b - matmul(a,x)
    call icres(L, d, r, p, n)
    


    c1 = dot_product(r,r)
    init_rr=c1
    if (c1 < er0) return
    beta=0.0d0
    !p(:) = 0.0d0
    r0(:) = r(:)
    w(:) = 0.0d0
    u(:) = 0.0d0
    t(:) = 0.0d0
    t_(:)= 0.0d0
    z(:) = 0.0d0
    itrmax_=input(default=100000,option=itrmax)
    itrmax_=10
    do itr = 1, itrmax_ 
      call icres(L, d, r, r_k, n)

      !p(:) = r(:)+beta*(p(:)-u(:) )  ! triple checked.
      !r0rk=dot_product(r0,r)
      ap   = matmul(a,p) ! triple checked. ap=v,
      alp  = dot_product(r,r_k )/dot_product(p,ap )! triple checked.
      x(:) = x(:) + alp*ap(:)
      rk(:)=r(:)
      r(:) = r(:) - alp*ap(:)
      !y(:) = t(:) - r(:) - alp*w(:) + alp*ap(:) !triple checked.
      !t(:) = r(:) - alp*ap(:) ! triple checked.

      !q(:) = matmul(a,t) ! triple checked. s=q=at=c
      !if(itr==1)then
      !  gzi  = dot_product(q,t)/dot_product(q,q)! double checked.
      !  nu   = 0.0d0 ! double checked.
      !else
      !  val1 = dot_product(y,y)*dot_product(q,t) - dot_product(y,t)*dot_product(q,y)
      !  val2 = dot_product(q,q)*dot_product(y,y) - dot_product(y,q)*dot_product(q,y)
      !  
      !  if(  val2==0.0d0 ) stop "Bicgstab devide by zero"
      !  gzi  = val1/val2
      !  val1 = dot_product(q,q)*dot_product(y,t) - dot_product(y,q)*dot_product(q,t)
      !  nu  = val1/val2  !triple checked.
      !endif

      !u(:) = gzi*ap + nu*(t_(:) -r(:) + beta*u(:)  ) !double checked.
      !z(:) = gzi*r(:) + nu*z(:) - alp*u(:)!double checked.
      !x(:) = x(:) +alp*p(:) +z(:) !double checked.
      !r(:) = t(:) -nu*y(:) -gzi*q(:)!double checked.
      !t_(:)=t(:)
      rr = dot_product(r,r)
      print *, rr
      !print *, 'itr, er =', itr,rr,sqrt(rr)/sqrt(init_rr)
      !print *, "ans = ",x(:)
      if (sqrt(rr)/sqrt(init_rr) < er0 ) exit
      call icres(L, d, r, r_, n)
      beta = dot_product(r,r_)/dot_product(rk,r_k)
      p(:)=r_(:)+beta*p(:)
      !beta=alp/gzi*dot_product(r0,r)/r0rk
      !w(:) = q(:) + beta * ap(:)
      if(itr==itrmax_) then
        print *, "ERROR :: preconditioned-CG did not converge."
      endif
    enddo
 end subroutine 
!===============================================================

subroutine icres(L, d, r, u, n)
  real(real64) :: y(n), rly,lu
  real(real64),intent(inout) :: L(1:n,1:n), d(1:n),r(1:n),u(1:n)
  integer(int32),intent(in)  :: n
  integer(int32) :: i,j
  do i=1, n
    rly = r(i)
    do j=1, i
      rly = rly - L(i,j)*y(j)
    enddo
    y(i) = rly/L(i,i)
  enddo

  do i=n, 1, -1
    lu=0.0d0
    do j=i+1, n
      lu = lu + L(j,i)*u(j)
    enddo
    u(i)= y(i)-d(i)*lu
  enddo


end subroutine icres

!==========================================================
function eigen_3d(tensor) result(eigenvector)
	real(real64),intent(in) :: tensor(3,3)
	integer(int32) :: i,j,n
	real(real64) :: eigenvector(3,3),a,b,c,d
	real(real64) :: eigenvalue(3),mat(3,3),ev(3),zero(3),unitmat(3,3)
	
	zero(:)=0.0d0
	unitmat(:,:) =0.0d0
	unitmat(1,1) = 1.0d0
	unitmat(2,2) = 1.0d0
	unitmat(3,3) = 1.0d0
	! get eigen values
	! (1) solve cubic equation
	!https://keisan.casio.jp/exec/user/1305724050
	a = 1.0d0
	b = -tensor(1,1)-tensor(2,2)-tensor(3,3)
	c = tensor(1,1)*tensor(2,2)+tensor(2,2)*tensor(3,3)+tensor(3,3)*tensor(1,1)&
		-tensor(1,2)*tensor(2,1)-tensor(2,3)*tensor(3,2)-tensor(3,1)*tensor(1,3)
	d = -tensor(1,1)*tensor(2,2)*tensor(3,3)-tensor(1,2)*tensor(2,3)*tensor(3,1)&
		-tensor(1,3)*tensor(2,1)*tensor(3,2)+tensor(1,3)*tensor(2,2)*tensor(3,1)&
		+tensor(2,3)*tensor(3,2)*tensor(1,1)+tensor(3,3)*tensor(1,2)*tensor(2,1)
	eigenvalue = cubic_equation(a,b,c,d)

	do i=1,3	
		mat(:,:) = eigenvalue(i)*unitmat(:,:)-tensor(:,:)
		call gauss_jordan_pv(mat, ev, zero,3)
		eigenvector(:,3)=ev(:)
	enddo



end function
!====================================================================================
subroutine showLinearSolver(obj)
  class(LinearSolver_),intent(in) :: obj
  real(real64),allocatable :: A_ij(:,:)
  integer(int32) :: i,j,n,m

  n = maxval(obj%Index_I)
  m = maxval(obj%Index_J)
  n = maxval( (/n, m/) )

  A_ij = zeros(n,n) 
  do i=1,size(obj%val)
    A_ij(obj%Index_I(i),obj%Index_J(i) ) = A_ij(obj%Index_I(i),obj%Index_J(i) ) + obj%val(i)
  enddo

  call print(A_ij)
end subroutine
!====================================================================================
!====================================================================================
function globalMatrixLinearSolver(obj) result(ret)
  class(LinearSolver_),intent(in) :: obj
  real(real64),allocatable :: ret(:,:)
  integer(int32) :: i,j,n,m,p1,p2,domain_id,offset

  if (allocated(obj%NumberOfNode) )then
    n = sum(obj%NumberOfNode) * obj%DOF
    ret = zeros(n,n)
    do i=1,size(obj%val)
      domain_id = obj%row_domain_id(i)
      offset = sum( obj%NumberOfNode(:domain_id-1) )
      !print *, offset
      p1 = offset*obj%DOF + obj%Index_I(i)

      domain_id = obj%column_domain_id(i)
      offset = sum( obj%NumberOfNode(:domain_id-1) )
      
      p2 = offset*obj%DOF + obj%Index_J(i)
      ret(p1,p2) = ret(p1,p2) + obj%val(i)
    enddo
    return
  else
    n = maxval(obj%Index_I)
    m = maxval(obj%Index_J)
    n = maxval( (/n, m/) )

    ret = zeros(n,n) 
    do i=1,size(obj%val)
      ret(obj%Index_I(i),obj%Index_J(i) ) = ret(obj%Index_I(i),obj%Index_J(i) ) + obj%val(i)
    enddo
    return
  endif
end function
!====================================================================================

!====================================================================================
function globalVectorLinearSolver(obj) result(ret)
  class(LinearSolver_),intent(in) :: obj
  real(real64),allocatable :: ret(:)
  integer(int32) :: i,j,n,m,p1,p2,domain_id,offset

  if (allocated(obj%NumberOfNode) )then
    n = sum(obj%NumberOfNode) * obj%DOF
    allocate(ret(n) )
    ret(:) = 0.0d0
    do i=1,size(obj%b_Index_J)
      domain_id = obj%b_domain_id(i)
      offset = sum( obj%NumberOfNode(:domain_id-1) )
      p1 = offset*obj%DOF + obj%b_Index_J(i)
      ret(p1) = ret(p1) + obj%b(i)
    enddo
    return
  else
    ret = obj%b
  endif
end function
!====================================================================================

subroutine convertCOOtoCRSLinearSolver(obj,OpenMP) 
  class(LinearSolver_),intent(inout) :: obj
  integer(int32),allocatable :: buf(:)
  integer(int32) :: n, nnz,i,nrhs
  logical,optional,intent(in) :: OpenMP
  logical :: omp_swich
  !real(real64),allocatable   :: CRS_val(:)
  !integer(int32),allocatable :: CRS_index_I(:)
  !integer(int32),allocatable :: CRS_index_J(:)
  !integer(int32),allocatable :: CRS_row_domain_id(:)
  !integer(int32),allocatable :: CRS_column_domain_id(:)
  omp_swich = input(default=.false.,option=OpenMP)
  ! Notice :: COO format data should be created and sorted.
  ! Further, multi-domain is not supported.

  nrhs = size(obj%b)+1
  obj%CRS_index_I = int(zeros(nrhs))
  
  if(omp_swich)then

    !$OMP parallel do private(i)
    do i=1,size(obj%index_I)
      obj%CRS_index_I( obj%index_I(i) ) =obj%CRS_index_I( obj%index_I(i) ) +1 
    enddo
    !$OMP end parallel do
    buf = obj%CRS_index_I

    !$OMP parallel do private(i)
    do i=nrhs-1,2,-1
      obj%CRS_index_I(i) = sum(buf(1:i-1))
    enddo
    !$OMP end parallel do

    obj%CRS_index_I(1) = 0
    obj%CRS_index_I(nrhs) = size(obj%val)
  else
    do i=1,size(obj%index_I)
      obj%CRS_index_I( obj%index_I(i) ) =obj%CRS_index_I( obj%index_I(i) ) +1 
    enddo
    buf = obj%CRS_index_I
  
    do i=nrhs-1,2,-1
      obj%CRS_index_I(i) = sum(buf(1:i-1))
    enddo
  
    obj%CRS_index_I(1) = 0
    obj%CRS_index_I(nrhs) = size(obj%val)
  endif
end subroutine
! #######################################################
function matmulCRSLinearSolver(obj,openMP) result(mm)
  class(LinearSolver_),intent(inout) :: obj
  real(real64),allocatable :: mm(:)
  integer(int32) :: i,j
  logical,optional,intent(in) :: openMP
  logical :: omp_swich
  omp_swich = input(default=.false.,option=OpenMP)

  mm = zeros(size(obj%b))
  ! Notice :: CRS format data should be created and sorted.
  
  if(omp_swich)then
    !$OMP parallel do private(i)
    do i=1,size(obj%b)
      do j=obj%CRS_Index_I(i)+1,obj%CRS_Index_I(i+1)
          mm(i) = mm(i) + obj%b( obj%Index_J(j) )*obj%val(j)
      enddo
    enddo
    !$OMP end parallel do
  else
    do i=1,size(obj%b)
      do j=obj%CRS_Index_I(i)+1,obj%CRS_Index_I(i+1)
          mm(i) = mm(i) + obj%b( obj%Index_J(j) )*obj%val(j)
      enddo
    enddo
  endif
end function

! #######################################################
function matmulCOOLinearSolver(obj,OpenMP) result(mm)
  class(LinearSolver_),intent(inout) :: obj
  real(real64),allocatable :: mm(:)
  integer(int32) :: i,j
  logical,optional,intent(in) :: openMP
  logical :: omp_swich
  omp_swich = input(default=.false.,option=OpenMP)

  mm = zeros(size(obj%b))
  ! Notice :: CRS format data should be created and sorted.
  if(omp_swich)then
    !$OMP parallel do private(i)
    do i=1,size(obj%val)
      mm( obj%index_I(i) ) = mm( obj%index_I(i) ) + obj%val(i)*obj%b(obj%index_J(i) )
    enddo
    !$OMP end parallel do
  else
    do i=1,size(obj%val)
      mm( obj%index_I(i) ) = mm( obj%index_I(i) ) + obj%val(i)*obj%b(obj%index_J(i) )
    enddo
  endif
end function
! #######################################################

! #######################################################
subroutine getCOOFormatLinearSolver(obj) 
  class(LinearSolver_),intent(inout) :: obj
  integer(int32) :: i,j,n

  if(.not.allocated(obj%val) )then
    if(allocated(obj%a) .and. allocated(obj%b) )then
      ! it only has obj%a and obj%b
      ! so, convert it to COO format
      ! count non-zero values
      n=0
      do j=1,size(obj%a,2)
        do i=1, size(obj%a,1)
          if(obj%a(i,j)/=0.0d0 )then
            n=n+1
          endif
        enddo
      enddo
      
      obj%val             = zeros(n)
      obj%index_I         = int(zeros(n))
      obj%index_J         = int(zeros(n))

      obj%row_domain_id   = int(zeros(n))
      obj%column_domain_id= int(zeros(n))
      obj%b_Index_J       = int(zeros(size(obj%b)))
      obj%b_Domain_ID     = int(zeros(size(obj%b)))
      
      obj%row_domain_id(:)    = 1
      obj%column_domain_id(:) = 1
      obj%b_domain_id(:)      = 1
      do i=1,size(obj%b)
        obj%b_Index_J(i) = i
      enddo

      n=0
      do j=1,size(obj%a,2)
        do i=1, size(obj%a,1)
          if(obj%a(i,j)/=0.0d0 )then
            n=n+1
            obj%val(n)     = obj%a(i,j)
            obj%index_I(n) = i
            obj%index_J(n) = j
          endif
        enddo
      enddo
      
      
    endif
  endif

end subroutine
! #######################################################


! #######################################################
subroutine exportAsCOOLinearSolver(obj,name)
  class(LinearSolver_),intent(inout) :: obj
  character(*),intent(in) :: name
  type(IO_) :: f

  call f%open(name,"w")
  call f%write(obj%Index_I,obj%Index_J, obj%val)
  call f%close()

end subroutine
! #######################################################


! #######################################################
subroutine exportRHSLinearSolver(obj,name)
  class(LinearSolver_),intent(inout) :: obj
  character(*),intent(in) :: name
  type(IO_) :: f

  call f%open(name,"w")
  call f%write(obj%b)
  call f%close()

end subroutine
! #######################################################

! #######################################################
! FITTING by Stochastic Gradient Descend
function fit(f,training_data,params,eta,error,max_itr,use_ratio,logfile,algorithm) result(fit_params)
    
  interface
      function f(x,params) result(ret)
          use iso_fortran_env
          real(real64),intent(in) :: x
          real(real64),intent(in) :: params(:) 
          real(real64) :: ret
      end function
  end interface

  real(real64),intent(in) :: training_data(:,:) !(sample, {input=1, output=2})
  real(real64),intent(in) :: params(:),eta
  real(real64),optional,intent(inout) :: error
  real(real64),optional,intent(in) :: use_ratio
  real(real64),allocatable :: grad_params(:),dp(:)
  integer(int32),optional,intent(in) :: max_itr
  character(*),optional,intent(in) :: logfile
  real(real64),allocatable :: fit_params(:)
  real(real64) :: error_function,error_function_b,error_function_f,err_0,h
  integer(int32) :: i,j,n,trial,shuffle_id,param_id
  real(real64) :: eps_val = dble(1.0e-4)
  real(real64) :: tol = dble(1.0e-9)
  real(real64) :: sgd_use_ratio
  character(*),optional,intent(in) :: algorithm
  character(:),allocatable :: algorithm_type

  type(IO_) :: log_file
  

  real(real64) :: eta_zero 
  real(real64) :: eta_tr 
  integer(int32):: max_trial = 1000000
  integer(int32) :: num_select !
  type(Random_) :: random
  integer(int32),allocatable :: selected_data_ids(:)

  if(present(algorithm) )then
    algorithm_type = algorithm
  else
    algorithm_type = "SGD"
  endif
  

  if(algorithm_type == "SGD")then

    sgd_use_ratio = 0.050d0
    if(present(use_ratio) )then
      sgd_use_ratio = use_ratio
    endif

    num_select = int(dble(size(training_data,1))*sgd_use_ratio) + 1

    selected_data_ids = zeros(num_select)

    if(present(max_itr) )then
      max_trial  = max_itr
    endif
    if(present(logfile) )then
        call log_file%open(logfile,"w")
        call log_file%write("# itr    error-norm")
    endif

    eta_zero = eta
    fit_params = params
    error_function = 0.0d0
    do i=1,size(training_data,1)
        error_function = error_function + ( f(x=training_data(i,1),params=fit_params) - training_data(i,2) )**2
    enddo


    ! compute grad for each params
    ! by Stochastic Gradient Descend
    do trial=1,max_trial
        error_function = 0.0d0
        !$OMP parallel default(shared) 
        !$OMP do reduction(+:error_function)
        do i=1,size(training_data,1)
            error_function = error_function + ( f(x=training_data(i,1),params=fit_params) - training_data(i,2) )**2
        enddo
        !$OMP end do
        !$OMP end parallel 

        if(trial==1)then
            err_0 = error_function
        endif

        if(present(error) )then
            error = error_function/err_0
        endif


        if(error_function/err_0 <= tol) then

            if(present(logfile) )then
              call log_file%close()
            endif
            exit
        endif


        if(trial==max_trial) then

            if(present(logfile) )then
              call log_file%close()
            endif
            !print *, "ERROR: fit() > SGD did not converge. "
            return
        endif

        ! Robbins-Monro Method
        eta_tr = eta_zero/dble(trial)

        ! shuffle training data and select 1
        shuffle_id = random%randint(from=1,to=size(training_data,1) )
        do i=1,num_select
          selected_data_ids(i) = random%randint(from=1,to=size(training_data,1) )
        enddo

        ! compute grad
        grad_params = zeros( size(params) )
        !$OMP parallel private(shuffle_id,dp,error_function_f,error_function_b) 
        !$OMP do reduction(+:grad_params)
        do j=1,num_select
          shuffle_id = selected_data_ids(j)
          dp = zeros( size(params) )
          do param_id = 1,size(params)
              dp(:) = 0.0d0
              dp(param_id) = eps_val

              error_function_f = 0.0d0
              do i=1,size(training_data,1)
                  error_function_f = error_function_f + ( f(x=training_data(shuffle_id,1),params=fit_params+dp) &
                      - training_data(shuffle_id,2) )**2
              enddo

              dp(param_id) = - eps_val

              error_function_b = 0.0d0
              do i=1,size(training_data,1)
                  error_function_b = error_function_b + ( f(x=training_data(shuffle_id,1),params=fit_params+dp) &
                       - training_data(shuffle_id,2) )**2
              enddo
              ! numerical derivative
              grad_params(param_id) = grad_params(param_id) + &
              (error_function_f - error_function_b )/(2.0d0*eps_val)
          enddo
        enddo
        !$OMP end do
        !$OMP end parallel 
        

        ! average_gradient
        grad_params = grad_params/dble(num_select)
        !grad_params = grad_params/norm(grad_params)

        fit_params = fit_params - eta_tr*grad_params


        if(norm(grad_params) <= tol) exit
        grad_params = 0.0d0

        if(present(logfile) )then
            write(log_file%fh,*) trial,error
        endif
    enddo

    if(present(logfile) )then
      call log_file%close()
    endif

  else
    print *, "[ERROR] FIT > no algorithm was found:: ",algorithm_type
  endif

end function

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
function crs_matmul_generic(CRS_value,CRS_col,CRS_row_ptr,old_vectors) result(new_vectors)
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


function crs_matmul_for_CRStype(CRS,old_vectors) result(new_vectors)
  type(CRS_),intent(in) :: CRS
  real(real64),intent(in)  :: Old_vectors(:,:)
  real(real64),allocatable :: new_vectors(:,:)

  new_vectors = crs_matmul_generic(&
    CRS_value=CRS%val,&
    CRS_col=CRS%col_idx,&
    CRS_row_ptr=CRS%row_ptr,&
    old_vectors=old_vectors)
    
end function


function crs_matvec_generic(CRS_value,CRS_col,CRS_row_ptr,old_vector) result(new_vector)
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


function crs_matvec_for_CRStype(CRS,old_vector) result(new_vector)
  type(CRS_),intent(in) :: CRS
  real(real64),intent(in)  :: Old_vector(:)
  real(real64),allocatable :: new_vector(:)

  new_vector = crs_matvec_generic(&
    CRS_value=CRS%val,&
    CRS_col=CRS%col_idx,&
    CRS_row_ptr=CRS%row_ptr,&
    old_vector=old_vector)

end function

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
