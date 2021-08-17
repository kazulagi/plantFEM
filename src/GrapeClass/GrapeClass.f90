module GrapeClass
    use LeafClass
    implicit none

    type :: Grape_

        ! 節-節点データ構造
        type(Mesh_) :: struct 
        integer(int32),allocatable :: leaf2stem(:,:)
        integer(int32),allocatable :: stem2stem(:,:)
        integer(int32),allocatable :: root2stem(:,:)
        integer(int32),allocatable :: root2root(:,:)

        real(real64)   :: mainstem_length
        real(real64)   :: mainstem_width
        integer(int32) :: mainstem_node

        real(real64)   :: mainroot_length
        real(real64)   :: mainroot_width
        integer(int32) :: mainroot_node

        integer(int32) :: num_branch
        integer(int32) :: num_branch_node

        integer(int32) :: num_branch_root
        integer(int32) :: num_branch_root_node

        integer(int32) :: num_leaf
        integer(int32) :: num_stem
        integer(int32) :: num_root


        ! 器官オブジェクト配列
        type(FEMDomain_),allocatable :: leaf_list(:)
        type(FEMDomain_),allocatable :: stem_list(:)
        type(FEMDomain_),allocatable :: root_list(:)
    contains
        procedure :: create => createGrape
    end type
contains

! #############################################################
subroutine createGrape(obj,config)
    class(Grape_),intent(inout) :: obj
    character(*),intent(in) :: config
    character(:),allocatable :: line
    type(IO_) :: grapeconfig
    type(Random_) :: random
    integer(int32)::num_branch,i,n

    obj%mainstem_length = freal(grapeconfig%parse(config,key1="Mainstem",key2="Length"))
    obj%mainstem_width = freal(grapeconfig%parse(config,key1="Mainstem",key2="Width"))
    obj%mainstem_node = fint(grapeconfig%parse(config,key1="Mainstem",key2="Node"))
    
    ! get number of branch && number of node
    obj%num_branch=1
    obj%num_branch_node=0
    do 
        line = grapeconfig%parse(config,key1="Branch#"//trim(str(obj%num_branch)),key2="Node" )
        if(len(trim(line))==0)then
            obj%num_branch = obj%num_branch -1
            exit
        else
            obj%num_branch = obj%num_branch  + 1
            obj%num_branch_node = obj%num_branch_node + fint(line)
            cycle
        endif
    enddo


    obj%mainroot_length = freal(grapeconfig%parse(config,key1="Mainroot",key2="Length"))
    obj%mainroot_width = freal(grapeconfig%parse(config,key1="Mainroot",key2="Width"))
    obj%mainroot_node = fint(grapeconfig%parse(config,key1="Mainroot",key2="Node"))

    ! get number of branch && number of node
    obj%num_branch_root=1
    obj%num_branch_root_node=0
    do 
        line = grapeconfig%parse(config,key1="Branch#"//trim(str(obj%num_branch_root)),key2="Node" )
        if(len(trim(line))==0)then
            obj%num_branch_root = obj%num_branch_root -1
            exit
        else
            obj%num_branch_root = obj%num_branch_root  + 1
            obj%num_branch_root_node = obj%num_branch_root_node + fint(line)
            cycle
        endif
    enddo

    obj%num_leaf =obj%num_branch_node + obj%mainstem_node 
    obj%num_stem =obj%num_branch_node + obj%mainstem_node 
    obj%num_root =obj%num_branch_root_node + obj%mainroot_node

    allocate(obj%leaf_list(obj%num_leaf))
    allocate(obj%stem_list(obj%num_stem))
    allocate(obj%root_list(obj%num_root))

    obj%leaf2stem = zeros( obj%num_leaf , obj%num_stem ) 
    obj%stem2stem = zeros( obj%num_stem , obj%num_stem ) 
    obj%root2stem = zeros( obj%num_root , obj%num_stem ) 
    obj%root2root = zeros( obj%num_root , obj%num_root ) 

end subroutine
! #############################################################

end module GrapeClass