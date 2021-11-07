module LinearSolverClass
  use, intrinsic :: iso_fortran_env
  use omp_lib
  use IOClass
  use TimeClass
  use MathClass
  use ArrayClass
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
    real(real64) :: er0=dble(1.0e-08)
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
recursive subroutine fixLinearSolver(obj,nodeid,entryvalue,entryID,DOF,row_DomainID,debug)
  class(LinearSolver_),intent(inout) :: obj
  logical,optional,intent(in) :: debug
  integer(int32),intent(in) :: nodeid
  integer(int32),optional,intent(in) :: entryID,DOF,row_DomainID
  real(real64),intent(in) :: entryvalue
  integer(int32),allocatable :: Index_I(:), Index_J(:),NumNodeBeforeDomainID(:)
  integer(int32) :: i,j, n, offset,m
  type(Time_) :: time

  if(.not.allocated(obj%val) )then
    if(allocated(obj%a) .and. allocated(obj%b) )then
      ! it only has obj%a and obj%b
      !x(nodeid) = entryvalue
      n = size(obj%b)
      
      obj%b(:) = obj%b(:) - obj%a(:,nodeid)*entryvalue
      obj%a(:,nodeid) = 0.0d0
      obj%a(nodeid,:) = 0.0d0
      obj%a(nodeid,nodeid) = 1.0d0
      obj%b(nodeid) = entryvalue
      return
    endif
  endif

  if(.not. present(row_DomainID) )then
    call obj%fix(nodeid=nodeid,entryvalue=entryvalue,entryID=entryID,DOF=DOF,&
      row_DomainID=1,debug=debug)
  endif

  ! too slow
  if(.not.obj%ReadyForFix)then
    call obj%prepareFix()
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
!
!    if(present(debug) )then
!      if(debug)then
!        print *, "fixLinearSolver  >> [1-2] lock checking"
!        call time%show()
!      endif
!    endif    
!
!    ! update other values
!    Index_I = obj%Index_I
!    Index_J = obj%Index_J
!    
!    do i=1, size(Index_I)
!      m = obj%row_Domain_ID(i)
!      if(m==1)then
!        cycle
!      endif
!      Index_I(i) = Index_I(i) + NumNodeBeforeDomainID(m)
!    enddo
!
!    do i=1, size(Index_J)
!      m = obj%column_Domain_ID(i)
!      if(m==1)then
!        cycle
!      endif
!      Index_J(i) = Index_J(i) + NumNodeBeforeDomainID(m)
!    enddo    

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
        !if(n == 1)then
        !  offset = 0
        !else
        !  offset = sum( obj%NumberOfNode(1:n-1) )*obj%DOF
        !endif
        offset = NumNodeBeforeDomainID(n)*obj%DOF

        n = obj%Index_I(i)
        !print *, "obj%b( offset + nodeid )",obj%b( offset + n ), offset, n,offset+ n
        if( .not. obj%Locked(offset + n  )) then
          if(size(obj%NumberOfNode)==1 )then
            obj%b(n ) = obj%b(n ) - obj%val(i) * entryvalue
          else
            obj%b( offset + n ) = obj%b( offset  + n ) - obj%val(i) * entryvalue
          endif
        endif
        !if(obj%Index_I(i)==nodeid .and. obj%row_domain_id(i)==row_DomainID )then
        !  obj%b( offset + n ) = entryvalue
        !endif
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
      !if(obj%index_I(i)==nodeid .and. obj%row_Domain_ID(i)==row_DomainID )then
      !  obj%val(i)=0.0d0
      !endif

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
              call bicgstab_COO(obj%val, obj%index_I, obj%index_J, obj%x, obj%b, obj%itrmax, obj%er0,obj%debug)
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
          call bicgstab_CRS(obj%val, CRS_index_I, index_J, obj%x, obj%b, obj%itrmax, obj%er0)
          return
        endif
      endif
      call bicgstab_COO(obj%val, index_I, index_J, obj%x, obj%b, obj%itrmax, obj%er0)

    else
      if(present(CRS) )then
        if(CRS)then
          call bicgstab_CRS(obj%val, CRS_index_I, index_J, obj%x, obj%b, obj%itrmax, obj%er0)
          return
        endif
      endif
      call bicgstab_COO(obj%val, obj%index_I, obj%index_J, obj%x, obj%b, obj%itrmax, obj%er0)
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
subroutine gauss_jordan_pv_real64(a0, x, b, n)
  integer(int32), intent(in) :: n
  real(real64), intent(in) :: a0(n,n), b(n)
  real(real64), intent(out) :: x(n)
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
  real(real64),allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:)

  if(present(debug) )then
    speak = debug
  endif

  if(speak) print *, "BiCGSTAB STARTED >> DOF:", n
  n=size(b)
  allocate(r(n), r0(n), p(n), y(n), e(n), v(n))
  er0=dble(1.00e-14)
  r(:) = b(:)
  if(speak) print *, "BiCGSTAB >> [1] initialize"
  
  !do i=1,size(a)
  !  if(ptr_i(i) <=0) cycle
  !  r( ptr_i(i) ) = r( ptr_i(i) ) - a(i)*x( index_j(i) ) 
  !enddo
  
  !$OMP parallel do private(i)
  do i=1,size(b)
    do j=ptr_I(i)+1,ptr_I(i+1)
        r(i) = r(i) + x(Index_J(j) )*a(j)
    enddo
  enddo
  !$OMP end parallel do
  

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
    !y(:)=0.0d0
    !do i=1,size(a)
    !  if(ptr_i(i) <=0) cycle
    !  y( ptr_i(i) ) = y( ptr_i(i) ) + a(i)*p( index_j(i) ) 
    !enddo
    
    y(:)=0.0d0
    !$OMP parallel do private(i)
    do i=1,size(b)
      do j=ptr_I(i)+1,ptr_I(i+1)
          y(i) = y(i) + p(Index_J(j) )*a(j)
      enddo
    enddo
    !$OMP end parallel do

    c2 = dot_product(r0,y)
    alp = c1/c2
    e(:) = r(:) - alp * y(:)
    !v(:) = matmul(a,e)
    
    if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] half"
    v(:)=0.0d0
    
    !do i=1,size(a)
    !  if(ptr_i(i) <=0) cycle
    !  v( ptr_i(i) ) = v( ptr_i(i) ) + a(i)*e( index_j(i) ) 
    !enddo
    !$OMP parallel do private(i)
    do i=1,size(b)
      do j=ptr_I(i)+1,ptr_I(i+1)
          v(i) = v(i) + e(Index_J(j) )*a(j)
      enddo
    enddo
    !$OMP end parallel do

    
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

subroutine bicgstab_COO(a, index_i, index_j, x, b, itrmax, er, debug)
  integer(int32), intent(inout) :: index_i(:),index_j(:), itrmax
  real(real64), intent(inout) :: a(:), b(:), er
  real(real64), intent(inout) :: x(:)
  logical,optional,intent(in) :: debug
  logical :: speak = .false.
  integer(int32) itr,i,j,n
  real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
  real(real64),allocatable:: r(:), r0(:), p(:), y(:), e(:), v(:)

  if(present(debug) )then
    speak = debug
  endif

  if(speak) print *, "BiCGSTAB STARTED >> DOF:", n
  n=size(b)
  allocate(r(n), r0(n), p(n), y(n), e(n), v(n))
  er0=dble(1.00e-14)
  r(:) = b(:)
  if(speak) print *, "BiCGSTAB >> [1] initialize"
  
  do i=1,size(a)
    if(index_i(i) <=0) cycle
    r( index_i(i) ) = r( index_i(i) ) - a(i)*x( index_j(i) ) 
  enddo
  
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
    do i=1,size(a)
      if(index_i(i) <=0) cycle
      y( index_i(i) ) = y( index_i(i) ) + a(i)*p( index_j(i) ) 
    enddo
    
    c2 = dot_product(r0,y)
    alp = c1/c2
    e(:) = r(:) - alp * y(:)
    !v(:) = matmul(a,e)
    
    if(speak) print *, "BiCGSTAB >> ["//str(itr)//"] half"
    v(:)=0.0d0
    do i=1,size(a)
      if(index_i(i) <=0) cycle
      v( index_i(i) ) = v( index_i(i) ) + a(i)*e( index_j(i) ) 
    enddo
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
 subroutine bicgstab_real64(a, b, x, n, itrmax, er)
  integer(int32), intent(in) :: n, itrmax
  real(real64), intent(in) :: a(n,n), b(n), er
  real(real64), intent(inout) :: x(n)
     integer(int32) itr
     real(real64) alp, bet, c1,c2, c3, ev, vv, rr,er0,init_rr
     real(real64) r(n), r0(n), p(n), y(n), e(n), v(n)
     er0=dble(1.00e-14)

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
     er0=dble(1.00e-14)

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
     er0=dble(1.00e-14)

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
     er0=dble(1.00e-14)

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

end module
