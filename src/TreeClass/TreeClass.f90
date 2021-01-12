module TreeClass
    use, intrinsic :: iso_fortran_env
    use ArrayClass
    implicit none


    type :: Nodep_
        type(Node_),pointer :: Nodep
    end type

    type :: Node_
        type(Node_), pointer    :: Parent
        type(Nodep_),allocatable :: Child(:)
        real(real64)         :: coord(3)
        real(real64)         :: vector(3)
        character*200   :: Name
        real(real64) :: fpval
        integer(int32)         :: intval
        integer(int32)         :: ID
    contains
        procedure,public :: Init => InitializeNode
        procedure,public :: create => CreateNode
    end type


    type :: Tree_
        type(Nodep_),allocatable :: Node(:)
        integer(int32) :: SortedUntil
    contains
        procedure,public :: Init => InitializeTree
        procedure,public :: Add  => AddNodeInTree
        procedure,public :: cut  => cutNodeInTree
        procedure,public :: show => showTree
        procedure,public :: NumOfTree => NumOfTree
        procedure,public :: parentNodeID => parentNodeIDTree
        procedure,public :: countIfParentIDis => countIfParentIDis
        procedure,public :: setVisualMap =>  setVisualMapTree
    end type

contains


! #######################################################
subroutine InitializeNode(obj)
    class(Node_),intent(inout) :: obj

    allocate(obj%Child(1) )
    obj%coord(:)=0.0d0
    obj%vector(:)=0.0d0

end subroutine InitializeNode
! #######################################################


! #######################################################
subroutine CreateNode(obj,parent,Name)
    class(Node_),target,intent(inout) :: obj
    class(Node_),target,optional,intent(inout) :: parent
    character(*),intent(in)    :: Name

    call obj%init()
    if(present(parent) )then
        obj%Parent => parent
    else
        obj%Parent => obj
    endif
    obj%Name   = Name

end subroutine
! #######################################################


! #######################################################
! #######################################################



! #######################################################
subroutine InitializeTree(obj,NumOfNode)
    class(Tree_),intent(inout)::obj
    integer(int32),optional,intent(in)::NumOfNode
    integer(int32) :: i,n,num

    num=input(default=10000,option=NumOfNode)
    if(.not.allocated (obj%Node))then
        allocate(obj%Node(num) )
    endif

    obj%SortedUntil=0

end subroutine
! #######################################################



! #######################################################
subroutine AddNodeInTree(obj,NodeObj)
    class(Tree_),intent(inout)::obj
    class(Node_),target,intent(in)::NodeObj


    obj%SortedUntil=obj%SortedUntil+1
    obj%Node(obj%SortedUntil)%Nodep => NodeObj
    
    print *, "A Node is imported. now number of node is ",obj%SortedUntil &
        ,"| Name = ",trim(obj%Node(obj%SortedUntil)%Nodep%Name)
    

end subroutine
! #######################################################



! #######################################################
subroutine cutNodeInTree(obj,NodeObj)
    class(Tree_),intent(inout)::obj
    class(Node_),target,intent(in)::NodeObj
    integer(int32) :: i,num

    num=obj%SortedUntil
    do i=1,obj%SortedUntil
        if(obj%Node(i)%Nodep%Name == NodeObj%Name)then
            
            print *, "A Node is cut. now number of node is ",obj%SortedUntil &
            ,"cut node is : ",trim(obj%Node(i)%Nodep%Name),"Node id : ",i
            nullify(obj%Node(i)%Nodep )
            num=num-1
        endif
        

        
    enddo
    obj%SortedUntil=num


end subroutine
! #######################################################


! #######################################################
subroutine showTree(obj)
    class(Tree_),intent(in)::obj

    integer(int32) :: i,n
    real(real64) :: x,y,vx,vy

    print *, "Num of Tree = ",obj%NumOfTree()
    do i=1,obj%NumOfTree()
        print *, "Parent Node ID = ",obj%parentNodeID(ParentID=i)
    enddo
    call obj%setVisualMap()

    do i=1,obj%SortedUntil
        print *, "child = ",trim(obj%Node(i)%Nodep%Name) &
            ," | parent = ",trim(obj%Node(i)%Nodep%parent%Name)
    enddo

    do i=1,obj%SortedUntil
        print *, obj%Node(i)%Nodep%coord(:),obj%Node(i)%Nodep%vector(:)
    enddo


end subroutine
! #######################################################


! #######################################################
function NumOfTree(obj) result(num)
    class(Tree_),intent(in)::obj
    integer(int32) :: i,n,num

    num=0
    do i=1,obj%SortedUntil
        if(obj%Node(i)%Nodep%Name == obj%Node(i)%Nodep%parent%Name)then
            num=num+1
        endif
    enddo

end function
! #######################################################

! #######################################################
function countIfParentIDis(obj,ParentID) result(num)
    class(Tree_),intent(in)::obj
    integer(int32),intent(in)::ParentID
    integer(int32) :: i,n,num
    

    num=0
    do i=1,obj%SortedUntil
        if(obj%Node(ParentID)%Nodep%Name == obj%Node(i)%Nodep%parent%Name)then
            num=num+1
        endif
    enddo

end function
! #######################################################


! #######################################################
function parentNodeIDTree(obj,ParentID) result(NodeID)
    class(Tree_),intent(in)::obj
    integer(int32),optional,intent(in)::ParentID
    integer(int32) :: i,n,num,pid,NodeID

    pid = input(default=1,option=ParentID)
    num=0
    do i=1,obj%SortedUntil
        if(obj%Node(i)%Nodep%Name == obj%Node(i)%Nodep%parent%Name)then
            num=num+1
            if(pid==num)then
                NodeID=i
                return
            endif
        endif
    enddo
end function
! #######################################################

! #######################################################
subroutine setVisualMapTree(obj)
    class(Tree_),intent(in)::obj
    
    integer(int32) :: i,j,n,num,num_i,num_of_node
    real(real64) :: vec(3),pi,theta,dtheta
    real(real64),allocatable :: rotate(:,:)

    allocate(rotate(3,3))

    num=0
    pi=3.14159d0
    do i=1,obj%SortedUntil
        if(obj%Node(i)%Nodep%Name == obj%Node(i)%Nodep%parent%Name)then
            num=num+1
            num_i=0
            ! primary node
            ! set x(:)=0
            obj%Node(i)%Nodep%coord(:)=0.0d0
            num_of_node=obj%countIfParentIDis(parentID=i)
            dtheta=pi/dble(num_of_node)/2.0d0
            rotate(:,:)=0.0d0
            theta=0.0d0
            do j=1, obj%SortedUntil
                if(obj%Node(i)%Nodep%Name == obj%Node(j)%Nodep%parent%Name)then
                    theta=theta+dtheta
                    vec(:)=0.0d0
                    vec(1)=1.0d0
                    rotate(3,3)=1.0d0
                    rotate(1,1)=cos(theta)
                    rotate(1,2)=-sin(theta)
                    rotate(2,1)=sin(theta)
                    rotate(2,2)=cos(theta)
                    vec(:)=matmul(rotate,vec)
                    obj%Node(j)%Nodep%coord(:)=obj%Node(i)%Nodep%parent%coord(:)+vec(:)
                    obj%Node(j)%Nodep%vector(:)=vec(:)
                endif
            enddo
        else
            cycle            
        endif
    enddo

    

end subroutine
! #######################################################


end module

