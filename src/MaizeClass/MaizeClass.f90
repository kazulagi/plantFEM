module MaizeClass
    use LeafClass
    use StemClass
    use RootClass
    use EarClass
    use PanicleClass
    implicit none


    type :: Maize_internode_info_
        real(real64),allocatable :: FinalInterNodeLength(:)
        real(real64),allocatable :: FinalPetioleLength(:)
        real(real64),allocatable :: FinalLeafLength(:)
        real(real64),allocatable :: FinalLeafWidth(:)
    end type
    
    type :: Maize_NodeID_Branch_
        integer(int32),allocatable :: ID(:)
    contains 
        !procedure, public :: sync => syncMaize_NodeID_Branch
    end type
    


    type :: Maize_

        ! 節-節点データ構造
        type(Mesh_) :: struct 
        integer(int32),allocatable :: leaf2stem(:,:)
        integer(int32),allocatable :: stem2stem(:,:)
        integer(int32),allocatable :: ear2stem(:,:)
        integer(int32),allocatable :: panicle2stem(:,:)
        integer(int32),allocatable :: root2stem(:,:)
        integer(int32),allocatable :: root2root(:,:)

        real(real64)   :: mainstem_length
        real(real64)   :: mainstem_width
        integer(int32) :: mainstem_node

        real(real64)   :: mainroot_length
        real(real64)   :: mainroot_width
        integer(int32) :: mainroot_node


        integer(int32) :: num_branch_root
        integer(int32) :: num_branch_root_node

        real(real64) :: ms_angle_ave = 0.0d0
        real(real64) :: ms_angle_sig = 0.0d0

        integer(int32),allocatable :: Leaf_From(:)

        !real(real64),allocatable :: leaf_Length(:)
        !real(real64),allocatable :: leaf_Width(:)

        real(real64),allocatable :: leaf_curvature(:)

        real(real64),allocatable :: leaf_thickness_ave(:)
        real(real64),allocatable :: leaf_thickness_sig(:)

        real(real64),allocatable :: leaf_angle_ave_x(:)
        real(real64),allocatable :: leaf_angle_sig_x(:)
        real(real64),allocatable :: leaf_angle_ave_z(:)
        real(real64),allocatable :: leaf_angle_sig_z(:)

        real(real64),allocatable :: leaf_length_ave(:)
        real(real64),allocatable :: leaf_length_sig(:)
        real(real64),allocatable :: leaf_width_ave(:)
        real(real64),allocatable :: leaf_width_sig(:)
        
        integer(int32) :: num_leaf
        integer(int32) :: num_stem
        integer(int32) :: num_ear = 1
        integer(int32) :: num_panicle = 1
        integer(int32) :: num_root


        ! 器官オブジェクト配列
        type(FEMDomain_),allocatable :: leaf_list(:)
        type(FEMDomain_),allocatable :: stem_list(:)
        type(FEMDomain_),allocatable :: root_list(:)

        character(:),allocatable :: LeafSurfaceData
        type(Leaf_),allocatable :: Leaf(:)
        type(Stem_),allocatable :: Stem(:)
        type(Ear_),allocatable :: Ear(:)
        type(Panicle_),allocatable :: Panicle(:)
        type(Root_),allocatable :: Root(:)



        
        integer(int32),allocatable :: NodeID_MainStem(:)
        type(maize_NodeID_Branch_),allocatable :: NodeID_Branch(:)
        
        logical ::  inLoop = .false.
        real(real64) :: hours = 0.0d0
        
        ! growth simulation
        real(real64) :: FullyExpanded_stem_threshold = 0.10d0
        integer(int32) :: MaxBranchNum = 20
        type(maize_internode_info_),allocatable :: InterNodeInfo(:)
        real(real64) :: default_Leaf_growth_ratio = 1.0d0/3.0d0
        real(real64) :: default_Stem_growth_ratio = 1.0d0/3.0d0
        integer(int32),allocatable :: MainStem_num_branch(:)
        real(real64) :: apical_dominance_distance = 1.0d0
        
    contains
        procedure :: create => createMaize
        procedure,public :: msh => mshMaize
        procedure,public :: vtk => vtkMaize
        procedure,public :: stl => stlMaize
        procedure,public :: json => jsonMaize
        procedure,public :: move => moveMaize
        procedure,public :: rotate => rotateMaize
        procedure,public :: update => updateMaize
    end type
contains

! #############################################################
subroutine createMaize(obj,config,debug)
    class(Maize_),intent(inout) :: obj
    character(*),intent(in) :: config
    character(:),allocatable :: line
    logical,optional,intent(in) :: debug
    logical :: debug_log
    type(IO_) :: Maizeconfig
    type(Random_) :: random
    integer(int32)::i,n,j,k,num_leaf,num_stem_node,num_branch_branch,cpid
    real(real64) :: x_A(1:3)

    debug_log = input(default=.false.,option=debug)
    cpid = 0

    if(debug_log)then
        cpid = cpid + 1
        call print("createMaize #" + str(cpid) )
    endif
    obj%mainstem_length = freal(Maizeconfig%parse(config,key1="Mainstem",key2="Length"))
    obj%mainstem_width = freal(Maizeconfig%parse(config,key1="Mainstem",key2="Width"))
    obj%mainstem_node = fint(Maizeconfig%parse(config,key1="Mainstem",key2="Node"))
    obj%ms_angle_ave = freal(Maizeconfig%parse(config,key1="Mainstem",key2="ms_angle_ave"))
    obj%ms_angle_sig = freal(Maizeconfig%parse(config,key1="Mainstem",key2="ms_angle_sig"))


    if(debug_log)then
        cpid = cpid + 1
        call print("createMaize #" + str(cpid) )
    endif
    
    ! get number of leaf
    obj%num_leaf=1
    do
        if(obj%num_leaf ==  obj%mainstem_node)then
            obj%num_leaf = obj%num_leaf -1
            exit
        endif

        line = Maizeconfig%parse(config,key1="Leaf_"//str(obj%num_leaf)//"_",key2="From")
        
        if(len(trim(line))==0)then
            obj%num_leaf = obj%num_leaf -1
            exit
        else
            obj%num_leaf = obj%num_leaf  + 1
            cycle
        endif
        
    enddo

    

    if(debug_log)then
        cpid = cpid + 1
        call print("createMaize #" + str(cpid) )
    endif

    allocate(obj%leaf_curvature(obj%num_leaf))
    allocate(obj%leaf_thickness_ave(obj%num_leaf))
    allocate(obj%leaf_thickness_sig(obj%num_leaf))
    allocate(obj%leaf_angle_ave_x(obj%num_leaf))
    allocate(obj%leaf_angle_sig_x(obj%num_leaf))
    allocate(obj%leaf_angle_ave_z(obj%num_leaf))
    allocate(obj%leaf_angle_sig_z(obj%num_leaf))
    allocate(obj%leaf_length_ave(obj%num_leaf))
    allocate(obj%leaf_length_sig(obj%num_leaf))
    allocate(obj%leaf_width_ave(obj%num_leaf))
    allocate(obj%leaf_width_sig(obj%num_leaf))
    allocate(obj%leaf_From(obj%num_leaf))
    !allocate(obj%leaf_Length(obj%num_leaf))
    !allocate(obj%leaf_Width(obj%num_leaf))


    if(debug_log)then
        cpid = cpid + 1
        call print("createMaize #" + str(cpid) )
    endif

    do i=1,obj%num_leaf
        obj%leaf_From(i)= fint(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="From"))
        !obj%leaf_Length(i)= freal(Maizeconfig%parse(&
        !    config,key1="Leaf_"//str(i)//"_",key2="Length"))
        !obj%leaf_Width(i)= freal(Maizeconfig%parse(&
        !    config,key1="Leaf_"//str(i)//"_",key2="Width"))
        obj%leaf_curvature(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_curvature"))
        obj%leaf_thickness_ave(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_thickness_ave"))
        obj%leaf_thickness_sig(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_thickness_sig"))
        obj%leaf_angle_ave_x(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_angle_ave_x"))
        obj%leaf_angle_sig_x(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_angle_sig_x"))
        obj%leaf_angle_ave_z(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_angle_ave_z"))
        obj%leaf_angle_sig_z(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_angle_sig_z"))
        obj%leaf_length_ave(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_length_ave"))
        obj%leaf_length_sig(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_length_sig"))
        obj%leaf_width_ave(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_width_ave"))
        obj%leaf_width_sig(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf_"//str(i)//"_",key2="leaf_width_sig"))
    enddo


    if(debug_log)then
        cpid = cpid + 1
        call print("createMaize #" + str(cpid) )
    endif
    obj%mainroot_length = freal(Maizeconfig%parse(config,key1="Mainroot",key2="Length"))
    obj%mainroot_width = freal(Maizeconfig%parse(config,key1="Mainroot",key2="Width"))
    obj%mainroot_node = fint(Maizeconfig%parse(config,key1="Mainroot",key2="Node"))



    ! get number of branch && number of node
    obj%num_branch_root=1
    obj%num_branch_root_node=0
    do 
        line = Maizeconfig%parse(config,key1="Branchroot_"//str(obj%num_branch_root)//"_",key2="Node" )
        if(len(trim(line))==0)then
            obj%num_branch_root = obj%num_branch_root -1
            exit
        else
            obj%num_branch_root = obj%num_branch_root  + 1
            obj%num_branch_root_node = obj%num_branch_root_node + fint(line)
            cycle
        endif
    enddo


    if(debug_log)then
        cpid = cpid + 1
        call print("createMaize #" + str(cpid) )
    endif
    obj%num_stem = obj%mainstem_node 
    obj%num_root =obj%num_branch_root_node + obj%mainroot_node

    allocate(obj%leaf_list(obj%num_leaf))
    allocate(obj%stem_list(obj%num_stem))
    allocate(obj%root_list(obj%num_root))

    allocate(obj%leaf(obj%num_leaf))
    allocate(obj%stem(obj%num_stem))
    allocate(obj%root(obj%num_root))



    obj%leaf2stem = zeros( obj%num_leaf , obj%num_stem) 
    obj%stem2stem = zeros( obj%num_stem, obj%num_stem) 
    obj%ear2stem = zeros( obj%num_ear, obj%num_stem) 
    obj%panicle2stem = zeros( obj%num_panicle, obj%num_stem) 
    obj%root2stem = zeros( obj%num_root , obj%num_stem) 
    obj%root2root = zeros( obj%num_root , obj%num_root ) 

    if(debug_log)then
        cpid = cpid + 1
        call print("createMaize #" + str(cpid) )
    endif

    ! set mainstem
    do i=1,obj%mainstem_node

        call obj%stem(i)%init()
        call obj%stem(i)%resize(&
            x = obj%mainstem_width, &
            y = obj%mainstem_width, &
            z = obj%mainstem_length/dble(obj%mainstem_node) &
            )
        call obj%stem(i)%rotate(&
            x = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
            y = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
            z = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig))   &
            )
    enddo

    do i=1,obj%mainstem_node-1
        call obj%stem(i+1)%connect("=>",obj%stem(i))
        obj%stem2stem(i+1,i) = 1
    enddo
    

    if(debug_log)then
        cpid = cpid + 1
        call print("createMaize #" + str(cpid) )
    endif

    !set leaf
    num_leaf = 0
    do i=1,obj%num_leaf
        ! 1葉/1節
        ! add leaves
        
        num_leaf=num_leaf+1
        
        call obj%leaf(num_leaf)%init(species=PF_MAIZE)
        call obj%leaf(num_leaf)%femdomain%resize(&
                y = random%gauss(mu=obj%leaf_thickness_ave(i),sigma=obj%leaf_thickness_sig(i))  , &
                z = random%gauss(mu=obj%leaf_length_ave(i)   ,sigma=obj%leaf_length_sig(i)) , &
                x = random%gauss(mu=obj%leaf_width_ave(i)    ,sigma=obj%leaf_width_sig(i)) &
            )
        call obj%leaf(num_leaf)%curve(curvature=obj%leaf_curvature(i) )
        
        call obj%leaf(num_leaf)%femdomain%rotate(&
                x = radian(random%gauss(mu=obj%leaf_angle_ave_x(i),sigma=obj%leaf_angle_sig_x(i))), &
                y = 0.0d0, &
                z = radian(random%gauss(mu=obj%leaf_angle_ave_z(i),sigma=obj%leaf_angle_sig_z(i)) ) &
            )
        call obj%leaf(num_leaf)%connect("=>",obj%stem( obj%Leaf_From(i) ))
            obj%leaf2stem(num_leaf, obj%Leaf_From(i) ) = 1
    enddo

    ! set panicle

    ! set panicles
    ! get number of panicles
    obj%num_panicle=1
    do
        if(obj%num_panicle ==  obj%mainstem_node)then
            obj%num_panicle = obj%num_panicle -1
            exit
        endif

        line = Maizeconfig%parse(config,key1="Panicle_"//str(obj%num_panicle)//"_",key2="From")
        
        if(len(trim(line))==0)then
            obj%num_panicle = obj%num_panicle -1
            exit
        else
            obj%num_panicle = obj%num_panicle  + 1
            cycle
        endif
        
    enddo

    allocate(obj%panicle(obj%num_panicle) )
    obj%panicle2stem = zeros(obj%num_panicle, size(obj%stem2stem,1 ) )
    do i=1,obj%num_panicle
        call obj%panicle(i)%init(&
            Length=freal(Maizeconfig%parse(config,key1="Panicle_"//str(i)//"_",key2="Length")),&
            Width= freal(Maizeconfig%parse(config,key1="Panicle_"//str(i)//"_",key2="Width")),&
            Node= fint(Maizeconfig%parse(config,key1="Panicle_"//str(i)//"_",key2="Node")), &
            shape_factor= freal(Maizeconfig%parse(config,key1="Panicle_"//str(i)//"_",key2="shape_factor")) & ! optional, then, small
            )
        n = fint(Maizeconfig%parse(config,key1="Panicle_"//str(i)//"_",key2="From"))
        obj%panicle2stem(i,n) = 1
        
        ! こいつを実装する．
        call obj%panicle(i)%connect("=>",obj%stem(n) )

    enddo



    ! set ears
    ! get number of ears
    obj%num_ear=1
    do
        if(obj%num_ear ==  obj%mainstem_node)then
            obj%num_ear = obj%num_ear -1
            exit
        endif

        line = Maizeconfig%parse(config,key1="Ear_"//str(obj%num_ear)//"_",key2="From")
        
        if(len(trim(line))==0)then
            obj%num_ear = obj%num_ear -1
            exit
        else
            obj%num_ear = obj%num_ear  + 1
            cycle
        endif
        
    enddo

    allocate(obj%ear(obj%num_ear) )
    obj%ear2stem = zeros(obj%num_ear, size(obj%stem2stem,1 ) )
    do i=1,obj%num_ear
        call obj%ear(i)%init(&
            Length=freal(Maizeconfig%parse(config,key1="Ear_"//str(i)//"_",key2="Length")),&
            Width= freal(Maizeconfig%parse(config,key1="Ear_"//str(i)//"_",key2="Width")),&
            Angle= freal(Maizeconfig%parse(config,key1="Ear_"//str(i)//"_",key2="Angle")) & ! deg.
            )
        n = fint(Maizeconfig%parse(config,key1="Ear_"//str(i)//"_",key2="From"))
        obj%ear2stem(i,n) = 1
        
        ! こいつを実装する．
        call obj%ear(i)%connect("=>",obj%stem(n) )

    enddo

    
    call obj%update()
    


end subroutine
! #############################################################


! ########################################
subroutine mshMaize(obj,name,num_threads)
    class(Maize_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n

    n = input(default=1,option=num_threads)
    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%msh(name=name//"_stem"//str(i))
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%msh(name=name//"_root"//str(i))
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%msh(name=name//"_leaf"//str(i))
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

end subroutine
! ########################################


! ########################################
subroutine vtkMaize(obj,name,num_threads,single_file,&
    scalar_field,vector_field,tensor_field,field_name)
    class(Maize_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    logical,optional,intent(in) :: single_file
    character(*),optional,intent(in) :: field_name
    integer(int32) :: i,n
    type(FEMDomain_) :: femdomain
    real(real64),optional,intent(in) :: scalar_field(:)
    real(real64),optional,intent(in) :: vector_field(:,:)
    real(real64),optional,intent(in) :: tensor_field(:,:,:)

    if(present(single_file) )then
        if(single_file)then
            ! export mesh for a single file
            if(allocated(obj%stem) )then
                do i=1,size(obj%stem)
                    if(.not.obj%stem(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%stem(i)%femdomain
                    endif
                enddo
            endif

            if(allocated(obj%leaf) )then
                do i=1,size(obj%leaf)
                    if(.not.obj%leaf(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%leaf(i)%femdomain
                    endif
                enddo
            endif

            if(allocated(obj%Ear) )then
                do i=1,size(obj%Ear)
                    if(.not.obj%Ear(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%Ear(i)%femdomain
                    endif
                enddo
            endif

            if(allocated(obj%Panicle) )then
                do i=1,size(obj%Panicle)
                    if(.not.obj%Panicle(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%Panicle(i)%femdomain
                    endif
                enddo
            endif

            if(allocated(obj%root) )then
                do i=1,size(obj%root)
                    if(.not.obj%root(i)%femdomain%empty() )then
                        femdomain = femdomain + obj%root(i)%femdomain
                    endif
                enddo
            endif

            if(present(scalar_field) )then
                ! export scalar-valued field 
                ! as a single file
                call femdomain%vtk(field=field_name,name=name,scalar=scalar_field)
            elseif(present(vector_field) )then
                ! export vector-valued field 
                ! as a single file
                call femdomain%vtk(field=field_name,name=name,vector=vector_field)
            elseif(present(tensor_field) )then
                ! export tensor-valued field 
                ! as a single file
                call femdomain%vtk(field=field_name,name=name,tensor=tensor_field)
            else
                call femdomain%vtk(field=field_name,name=name)
            endif
            return
        endif
    endif


    n = input(default=1,option=num_threads)
    if(allocated(obj%stem) )then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%stem(i)%vtk(name=name//"_stem"//str(i))
            endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif

    if(allocated(obj%root))then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%root)
            if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%root(i)%vtk(name=name//"_root"//str(i))
            endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif


    if(allocated(obj%leaf))then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%leaf(i)%vtk(name=name//"_leaf"//str(i))
            endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif


    if(allocated(obj%ear))then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%ear)
            if(obj%ear(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%ear(i)%vtk(name=name//"_ear"//str(i))
            endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif


    if(allocated(obj%panicle))then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%panicle)
            if(obj%panicle(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%panicle(i)%vtk(name=name//"_panicle"//str(i))
            endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif

end subroutine
! ########################################


! ########################################
subroutine jsonMaize(obj,name)
    class(Maize_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32) :: i,countnum
    type(IO_) :: f

    call f%open(name//".json")
    call f%write("{")
    countnum=0
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"stem"//str(i)//'":')
            call obj%stem(i)%femdomain%json(name=name//"_stem"//str(i),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_stem":'//str(countnum)//',' )

    countnum=0
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"root"//str(i)//'":')
            call obj%root(i)%femdomain%json(name=name//"_root"//str(i),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_root":'//str(countnum)//',' )
    
    countnum=0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"leaf"//str(i)//'":')
            call obj%leaf(i)%femdomain%json(name=name//"_leaf"//str(i),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_leaf":'//str(countnum)//',' )
    call f%write('"return_Maize":0')
    call f%write("}")
    call f%close()
end subroutine
! ########################################

! ########################################
subroutine stlMaize(obj,name,num_threads)
    class(Maize_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n

    n = input(default=1,option=num_threads)
    !call execute_command_line("echo ' ' > "//name//".stl")
    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%stl(name=name//"_stem"//str(i))
            !call execute_command_line("cat "//name//"_stem"//str(i)//"_000001.stl >> "//name//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%stl(name=name//"_root"//str(i))
            !call execute_command_line("cat "//name//"_root"//str(i)//"_000001.stl >> "//name//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%stl(name=name//"_leaf"//str(i))
            !call execute_command_line("cat "//name//"_leaf"//str(i)//"_000001.stl >> "//name//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel


end subroutine
! ########################################

! ########################################
subroutine moveMaize(obj,x,y,z)
    class(Maize_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    integer(int32) :: i

    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%move(x=x,y=y,z=z)
        endif
    enddo

    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%move(x=x,y=y,z=z)
        endif
    enddo

    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%move(x=x,y=y,z=z)
        endif
    enddo


    do i=1,size(obj%Ear)
        if(obj%Ear(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%Ear(i)%move(x=x,y=y,z=z)
        endif
    enddo


    do i=1,size(obj%panicle)
        if(obj%panicle(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%panicle(i)%move(x=x,y=y,z=z)
        endif
    enddo

end subroutine
! ########################################



! ########################################
subroutine rotateMaize(obj,x,y,z)
    class(Maize_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    integer(int32) :: i

    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%rotate(x=x,y=y,z=z)
        endif
    enddo

    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%rotate(x=x,y=y,z=z)
        endif
    enddo

    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%rotate(x=x,y=y,z=z)
        endif
    enddo


    do i=1,size(obj%Ear)
        if(obj%Ear(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%Ear(i)%rotate(x=x,y=y,z=z)
        endif
    enddo


    do i=1,size(obj%panicle)
        if(obj%panicle(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%panicle(i)%rotate(x=x,y=y,z=z)
        endif
    enddo

end subroutine
! ########################################



! ########################################
recursive subroutine updateMaize(obj,stem_id, root_id, leaf_id, overset_margin,debug)
    class(Maize_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: stem_id, root_id, leaf_id    
    real(real64),optional,intent(in) :: overset_margin
    integer(int32) :: i,j,this_stem_id,next_stem_id,A_id,B_id,itr_tol,itr,k,kk
    integer(int32) :: this_leaf_id,next_leaf_id
    integer(int32) :: this_root_id,next_root_id,InterNodeID,PetioleID,StemID,LeafID
    real(real64) :: x_A(3),x_B(3),diff(3),error,last_error,mgn,overset_m,error_tol
    logical,optional,intent(in) :: debug
    integer(int32) :: next_ear_id,next_panicle_id

!    if(obj%default_Leaf_growth_ratio > 0.0d0)then
!        do i=1,size(obj%leaf)
!            if(obj%leaf(i)%empty() ) cycle
!            obj%leaf(i)%length_growth_ratio = obj%default_Leaf_growth_ratio
!            obj%leaf(i)%Width_growth_ratio = obj%default_Leaf_growth_ratio
!        enddo
!    endif
!
!
!    if(obj%default_stem_growth_ratio > 0.0d0)then
!        do i=1,size(obj%stem)
!            if(obj%stem(i)%empty() ) cycle
!            obj%stem(i)%length_growth_ratio = obj%default_stem_growth_ratio
!            obj%stem(i)%Width_growth_ratio = obj%default_stem_growth_ratio
!        enddo
!    endif

    ! if Maize_internode_info_ is active
    ! update parameters
!    if(allocated(obj%InterNodeInfo) )then
!        do i=0,obj%MaxBranchNum
!            
!            if(allocated(obj%InterNodeInfo(i)%FinalInterNodeLength ) )then
!                do j=1,obj%maxInterNodeID(StemID=i)
!                    InterNodeID = obj%searchStem(StemID=i,InterNodeID=j)
!                    if(size(obj%InterNodeInfo(i)%FinalInterNodeLength) < j ) then
!                        print *, "ERROR :: updateMaize >> "
!                        print *, "size(obj%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
!                        stop
!                    endif
!                    if(InterNodeID<1)then
!                        cycle
!                    endif
!                    obj%stem(InterNodeID)%final_length = obj%InterNodeInfo(i)%FinalInterNodeLength(j)
!                enddo
!            endif
!            
!            if(allocated(obj%InterNodeInfo(i)%FinalPetioleLength) )then
!                do j=1,obj%maxInterNodeID(StemID=i)
!                    do k=1,obj%maxPetioleID(StemID=i,InterNodeID=j)
!                        if(size(obj%InterNodeInfo(i)%FinalPetioleLength) < j ) then
!                            print *, "ERROR :: updateMaize >> "
!                            print *, "size(obj%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
!                            stop
!                        endif
!
!                        PetioleID = obj%searchPetiole(StemID=i,InterNodeID=j,PetioleID=k)
!                        
!                        obj%stem(PetioleID)%final_length = obj%InterNodeInfo(i)%FinalPetioleLength(j)
!                    enddo
!                enddo
!            endif
!
!                if(allocated(obj%InterNodeInfo(i)%FinalLeafLength) )then
!                    do j=1,obj%maxInterNodeID(StemID=i)
!                        do k=1,obj%maxPetioleID(StemID=i,InterNodeID=j)
!                            do kk = 1, obj%maxleafID(StemID=i,InterNodeID=j,PetioleID=k)
!                                if(size(obj%InterNodeInfo(i)%FinalLeafLength) < j ) then
!                                    print *, "ERROR :: updateMaize >> "
!                                    print *, "size(obj%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
!                                    stop
!                                endif
!                                LeafID = obj%searchleaf(StemID=i,InterNodeID=j,PetioleID=k,LeafID=kk)
!                                obj%leaf(LeafID)%final_length = obj%InterNodeInfo(i)%FinalLeafLength(j)
!                            enddo
!                        enddo
!                    enddo
!                endif
!
!
!                if(allocated(obj%InterNodeInfo(i)%FinalLeafWidth) )then
!                    do j=1,obj%maxInterNodeID(StemID=i)
!                        do k=1,obj%maxPetioleID(StemID=i,InterNodeID=j)
!                            do kk = 1, obj%maxleafID(StemID=i,InterNodeID=j,PetioleID=k)
!                                if(size(obj%InterNodeInfo(i)%FinalLeafWidth) < j ) then
!                                    print *, "ERROR :: updateMaize >> "
!                                    print *, "size(obj%InterNodeInfo(i)%FinalInterNodeLength) is not enough"
!                                    stop
!                                endif
!                                LeafID = obj%searchleaf(StemID=i,InterNodeID=j,PetioleID=k,LeafID=kk)
!                                obj%leaf(LeafID)%final_Width = obj%InterNodeInfo(i)%FinalLeafWidth(j)
!                            enddo
!                        enddo
!                    enddo
!                endif
!            
!        enddo
!    endif

    ! update connectivity
    if(.not. allocated(obj%stem2stem ))then
        print *, "updateMaize >> ERROR :: .not. allocated(obj%stem2stem )"
        return
    endif



    error_tol = dble(1.0e-14)

    ! margin between subdomains
    overset_m = input(default=0.03d0, option=overset_margin)
    
    itr_tol = 100
    itr=0

    ! if debug
    !if(present(debug) )then
    !    if(debug)then
    !        print *, "obj%stem2stem"
    !        call print(obj%stem2stem)
    !    endif
    !endif

    ! stem to stem
    last_error = 1.0d0
    if(maxval(obj%stem2stem)/=0) then
        

        do 
            itr=itr+1
            error = 0.0d0
            do i=1, size(obj%stem2stem,1)
                do j=1, size(obj%stem2stem,2)
                    this_stem_id = j
                    next_stem_id = i
                    if(obj%stem2stem(i,j)/=0 .and. i /= j)then
                        ! this_stem_id ===>>> next_stem_id, connected!

                        !x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                        !x_A(:) = obj%stem(next_stem_id)%getCoordinate("A")
                        ! Overset分食い込ませる
                        x_B(:) = (1.0d0-overset_m)*obj%stem(this_stem_id)%getCoordinate("B")&
                            + overset_m*obj%stem(this_stem_id)%getCoordinate("A")
                        ! Overset分食い込ませる
                        x_A(:) = (1.0d0-overset_m)*obj%stem(next_stem_id)%getCoordinate("A") &
                            + overset_m*obj%stem(next_stem_id)%getCoordinate("B")


                        diff(:) = x_B(:) - x_A(:)
                        
                        error = error + dot_product(diff,diff)
                        call obj%stem(next_stem_id)%move(x=diff(1),y=diff(2),z=diff(3) )

                    endif
                enddo
            enddo
            if(present(debug) )then
                if(debug)then
                    print *, "Maize % update s2s >> error :: ",error
                endif
            endif
            if(itr > itr_tol) then
                print *, "Maize % update s2s >> ERROR :: not converged"
                stop
            endif

            if( abs(error) + abs(last_error) < error_tol) exit
            last_error = error
        enddo
    endif

    ! root to stem
    if(allocated(obj%root2stem) )then
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%root2stem,1)
            do j=1, size(obj%root2stem,2)
                this_stem_id = j
                next_root_id = i
                if(obj%root2stem(i,j)==1)then
                    ! this_stem_id ===>>> next_root_id, connected!
                    !x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    !x_A(:) = obj%root(next_root_id)%getCoordinate("A")

                    ! Overset分食い込ませる
                    x_B(:) = (1.0d0-overset_m)*obj%stem(this_stem_id)%getCoordinate("A")&
                        + overset_m*obj%stem(this_stem_id)%getCoordinate("B")
                    ! Overset分食い込ませる
                    x_A(:) = (1.0d0-overset_m)*obj%root(next_root_id)%getCoordinate("A") &
                        + overset_m*obj%root(next_root_id)%getCoordinate("B")
                        

                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%root(next_root_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "Maize % update r2s >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "Maize % update r2s  >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error)  < error_tol) exit
        last_error = error
    enddo
    endif

    
    if(allocated(obj%root2root) )then
    ! root to root
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%root2root,1)
            do j=1, size(obj%root2root,2)
                this_root_id = j
                next_root_id = i
                if(next_root_id==1)then
                    cycle
                endif
                if(obj%root2root(i,j)/=0 .and. i /= j)then
                    ! this_root_id ===>>> next_root_id, connected!
                    !x_B(:) = obj%root(this_root_id)%getCoordinate("B")
                    !x_A(:) = obj%root(next_root_id)%getCoordinate("A")
                    
                    ! Overset分食い込ませる
                    x_B(:) = (1.0d0-overset_m)*obj%root(this_root_id)%getCoordinate("B")&
                        + overset_m*obj%root(this_root_id)%getCoordinate("A")
                    ! Overset分食い込ませる
                    x_A(:) = (1.0d0-overset_m)*obj%root(next_root_id)%getCoordinate("A") &
                        + overset_m*obj%root(next_root_id)%getCoordinate("B")


                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%root(next_root_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "Maize % update r2r >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "Maize % update r2r >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error)  < error_tol) exit
        last_error = error
    enddo
    endif

    ! leaf to stem
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%leaf2stem,1)
            do j=1, size(obj%leaf2stem,2)
                this_stem_id = j
                next_leaf_id = i
                if(obj%leaf2stem(i,j)==1)then
                    ! this_stem_id ===>>> next_leaf_id, connected!
                    !x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    !x_A(:) = obj%leaf(next_leaf_id)%getCoordinate("A")

                    ! Overset分食い込ませる
                    x_B(:) = (1.0d0-overset_m)*obj%stem(this_stem_id)%getCoordinate("B")&
                        + overset_m*obj%stem(this_stem_id)%getCoordinate("A")
                    ! Overset分食い込ませる
                    x_A(:) = obj%leaf(next_leaf_id)%getCoordinate("A")
                        

                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%leaf(next_leaf_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "Maize % update l2s >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "Maize % update l2s  >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error)  < error_tol) exit
        last_error = error
    enddo

    


    ! ear to stem
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%ear2stem,1)
            do j=1, size(obj%ear2stem,2)
                this_stem_id = j
                next_ear_id = i
                if(obj%ear2stem(i,j)==1)then
                    ! this_stem_id ===>>> next_ear_id, connected!
                    !x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    !x_A(:) = obj%ear(next_ear_id)%getCoordinate("A")

                    ! Overset分食い込ませる
                    x_B(:) = (1.0d0-overset_m)*obj%stem(this_stem_id)%getCoordinate("B")&
                        + overset_m*obj%stem(this_stem_id)%getCoordinate("A")
                    ! Overset分食い込ませる
                    x_A(:) = obj%ear(next_ear_id)%getCoordinate("A")
                        

                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%ear(next_ear_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo

        if(present(debug) )then
            if(debug)then
                print *, "Maize % update l2s >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "Maize % update l2s  >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error)  < error_tol) exit
        last_error = error
    enddo

    
    ! panicle to stem
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%panicle2stem,1)
            do j=1, size(obj%panicle2stem,2)
                this_stem_id = j
                next_panicle_id = i
                if(obj%panicle2stem(i,j)==1)then
                    ! this_stem_id ===>>> next_panicle_id, connected!
                    !x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    !x_A(:) = obj%panicle(next_panicle_id)%getCoordinate("A")

                    ! Overset分食い込ませる
                    x_B(:) = (1.0d0-overset_m)*obj%stem(this_stem_id)%getCoordinate("B")&
                        + overset_m*obj%stem(this_stem_id)%getCoordinate("A")
                    ! Overset分食い込ませる
                    x_A(:) = obj%panicle(next_panicle_id)%getCoordinate("A")
                        

                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%panicle(next_panicle_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        
        if(present(debug) )then
            if(debug)then
                print *, "Maize % update l2s >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "Maize % update l2s  >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error)  < error_tol) exit
        last_error = error
    enddo

    
end subroutine
! ########################################



! ########################################
!recursive subroutine updateMaize(obj,debug)
!    class(Maize_),intent(inout) :: obj
!    integer(int32) :: i,j,this_stem_id,next_stem_id,A_id,B_id,itr_tol,itr
!    integer(int32) :: this_leaf_id,next_leaf_id
!    integer(int32) :: this_root_id,next_root_id
!    real(real64) :: x_A(3),x_B(3),diff(3),error,last_error
!    logical,optional,intent(in) :: debug
!    
!    
!    ! update connectivity
!    if(.not. allocated(obj%stem2stem ))then
!        print *, "updateMaize >> ERROR :: .not. allocated(obj%stem2stem )"
!        return
!    endif
!
!    itr_tol = 100
!    itr=0
!
!    ! stem to stem
!    last_error = 1.0d0
!    do 
!        itr=itr+1
!        error = 0.0d0
!        do i=1, size(obj%stem2stem,1)
!            do j=1, size(obj%stem2stem,2)
!                this_stem_id = j
!                next_stem_id = i
!                if(obj%stem2stem(i,j)/=0 .and. i /= j)then
!                    ! this_stem_id ===>>> next_stem_id, connected!
!                    x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
!                    x_A(:) = obj%stem(next_stem_id)%getCoordinate("A")
!                    diff(:) = x_B(:) - x_A(:)
!                    error = error + dot_product(diff,diff)
!                    call obj%stem(next_stem_id)%move(x=diff(1),y=diff(2),z=diff(3) )
!                endif
!            enddo
!        enddo
!        if(present(debug) )then
!            if(debug)then
!                print *, "Maize % update >> error :: ",error
!            endif
!        endif
!        if(itr > itr_tol) then
!            print *, "Maize % update >> ERROR :: not converged"
!            stop
!        endif
!        
!        if( abs(error) + abs(last_error) == 0.0d0) exit
!        last_error = error
!    enddo
!
!    ! leaf to stem
!    last_error = 1.0d0
!    do 
!        itr=itr+1
!        error = 0.0d0
!        do i=1, size(obj%leaf2stem,1)
!            do j=1, size(obj%leaf2stem,2)
!                this_stem_id = j
!                next_leaf_id = i
!                if(obj%leaf2stem(i,j)==1)then
!                    ! this_stem_id ===>>> next_leaf_id, connected!
!                    x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
!                    x_A(:) = obj%leaf(next_leaf_id)%getCoordinate("A")
!                    diff(:) = x_B(:) - x_A(:)
!                    error = error + dot_product(diff,diff)
!                    call obj%leaf(next_leaf_id)%move(x=diff(1),y=diff(2),z=diff(3) )
!                endif
!            enddo
!        enddo
!        if(present(debug) )then
!            if(debug)then
!                print *, "Maize % update >> error :: ",error
!            endif
!        endif
!        
!        if(itr > itr_tol) then
!            print *, "Maize % update >> ERROR :: not converged"
!            stop
!        endif
!        
!        if( abs(error) + abs(last_error) == 0.0d0) exit
!        last_error = error
!    enddo
!
!
!    return
!
!
!    ! root to root
!    last_error = 1.0d0
!    do 
!        itr=itr+1
!        error = 0.0d0
!        do i=1, size(obj%root2root,1)
!            do j=1, size(obj%root2root,2)
!                this_root_id = j
!                next_root_id = i
!                if(obj%root2root(i,j)/=0 .and. i /= j)then
!                    ! this_root_id ===>>> next_root_id, connected!
!                    x_B(:) = obj%root(this_root_id)%getCoordinate("B")
!                    x_A(:) = obj%root(next_root_id)%getCoordinate("A")
!                    diff(:) = x_B(:) - x_A(:)
!                    error = error + dot_product(diff,diff)
!                    call obj%root(next_root_id)%move(x=diff(1),y=diff(2),z=diff(3) )
!                endif
!            enddo
!        enddo
!        if(present(debug) )then
!            if(debug)then
!                print *, "Maize % update >> error :: ",error
!            endif
!        endif
!        if(itr > itr_tol) then
!            print *, "Maize % update >> ERROR :: not converged"
!            stop
!        endif
!        
!        if( abs(error) + abs(last_error) == 0.0d0) exit
!        last_error = error
!    enddo
!
!    
!    
!end subroutine
! ########################################


end module MaizeClass