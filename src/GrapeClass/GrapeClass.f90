module GrapeClass
    use LeafClass
    use StemClass
    use RootClass
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

        real(real64) :: ms_angle_ave = 0.0d0
        real(real64) :: ms_angle_sig = 0.0d0
        real(real64),allocatable :: br_angle_ave_x(:) 
        real(real64),allocatable :: br_angle_sig_x(:) 
        real(real64),allocatable :: br_angle_ave_z(:) 
        real(real64),allocatable :: br_angle_sig_z(:) 



        real(real64),allocatable :: peti_size_ave(:)
        real(real64),allocatable :: peti_size_sig(:)
        real(real64),allocatable :: peti_width_ave(:)
        real(real64),allocatable :: peti_width_sig(:)
        real(real64),allocatable :: peti_angle_ave(:)
        real(real64),allocatable :: peti_angle_sig(:)

        real(real64),allocatable :: leaf_thickness_ave(:)
        real(real64),allocatable :: leaf_thickness_sig(:)
        real(real64),allocatable :: leaf_angle_ave(:)
        real(real64),allocatable :: leaf_angle_sig(:)
        real(real64),allocatable :: leaf_length_ave(:)
        real(real64),allocatable :: leaf_length_sig(:)
        real(real64),allocatable :: leaf_width_ave(:)
        real(real64),allocatable :: leaf_width_sig(:)


        integer(int32),allocatable :: br_node(:)
        integer(int32),allocatable :: br_from(:)
        real(real64),allocatable :: br_length(:)
        real(real64),allocatable :: br_width(:)

        
        integer(int32) :: num_leaf
        integer(int32) :: num_stem
        integer(int32) :: num_root


        ! 器官オブジェクト配列
        type(FEMDomain_),allocatable :: leaf_list(:)
        type(FEMDomain_),allocatable :: stem_list(:)
        type(FEMDomain_),allocatable :: root_list(:)

        character(:),allocatable :: LeafSurfaceData
        type(Leaf_),allocatable :: Leaf(:)
        type(Stem_),allocatable :: Stem(:)
        type(Root_),allocatable :: Root(:)


        
    contains
        procedure :: create => createGrape
        procedure,public :: msh => mshGrape
        procedure,public :: vtk => vtkGrape
        procedure,public :: stl => stlGrape
        procedure,public :: json => jsonGrape
        procedure,public :: move => moveGrape
    end type
contains

! #############################################################
subroutine createGrape(obj,config)
    class(Grape_),intent(inout) :: obj
    character(*),intent(in) :: config
    character(:),allocatable :: line
    type(IO_) :: grapeconfig
    type(Random_) :: random
    integer(int32)::i,n,j,k,num_leaf,num_stem_node,num_branch_branch

    obj%LeafSurfaceData = trim(grapeconfig%parse(config,key1="LeafSurfaceData"))
    obj%mainstem_length = freal(grapeconfig%parse(config,key1="Mainstem",key2="Length"))
    obj%mainstem_width = freal(grapeconfig%parse(config,key1="Mainstem",key2="Width"))
    obj%mainstem_node = fint(grapeconfig%parse(config,key1="Mainstem",key2="Node"))
    obj%ms_angle_ave = freal(grapeconfig%parse(config,key1="Mainstem",key2="ms_angle_ave"))
    obj%ms_angle_sig = freal(grapeconfig%parse(config,key1="Mainstem",key2="ms_angle_sig"))

    
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
            ! Further branch
            ! line = grapeconfig%parse(config,key1="Branch#"//trim(str(i)),key2="Branch#1")    
            ! num_branch_branch = fint(line)
            ! if(num_branch_branch/=0)then
            !     ! 2nd order branch
            !     line = grapeconfig%parse(config,key1="Branch#"//trim(str(i)),key2="Branch#1")    
            ! endif
            cycle
        endif
    enddo


    allocate(obj%br_node(obj%num_branch) )
    allocate(obj%br_from(obj%num_branch) )
    allocate(obj%br_length(obj%num_branch) )
    allocate(obj%br_width(obj%num_branch) )
    allocate(obj%br_angle_ave_x(obj%num_branch) )
    allocate(obj%br_angle_sig_x(obj%num_branch) )
    allocate(obj%br_angle_ave_z(obj%num_branch) )
    allocate(obj%br_angle_sig_z(obj%num_branch) )

    allocate(obj%peti_size_ave(obj%num_branch))
    allocate(obj%peti_size_sig(obj%num_branch))
    allocate(obj%peti_width_ave(obj%num_branch))
    allocate(obj%peti_width_sig(obj%num_branch))
    allocate(obj%peti_angle_ave(obj%num_branch))
    allocate(obj%peti_angle_sig(obj%num_branch))

    allocate(obj%leaf_thickness_ave(obj%num_branch))
    allocate(obj%leaf_thickness_sig(obj%num_branch))
    allocate(obj%leaf_angle_ave(obj%num_branch))
    allocate(obj%leaf_angle_sig(obj%num_branch))
    allocate(obj%leaf_length_ave(obj%num_branch))
    allocate(obj%leaf_length_sig(obj%num_branch))
    allocate(obj%leaf_width_ave(obj%num_branch))
    allocate(obj%leaf_width_sig(obj%num_branch))


    do i=1,obj%num_branch
        line = grapeconfig%parse(config,key1="Branch#"//trim(str(i)),key2="Node" )    
        obj%br_node(i) = fint(line)
        line = grapeconfig%parse(config,key1="Branch#"//trim(str(i)),key2="From" )    
        obj%br_from(i) = fint(line)
        line = grapeconfig%parse(config,key1="Branch#"//trim(str(i)),key2="Length" )    
        obj%br_length(i) = freal(line)
        line = grapeconfig%parse(config,key1="Branch#"//trim(str(i)),key2="Width" )    
        obj%br_width(i) = freal(line)
        obj%br_angle_ave_x(i) = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="br_angle_ave_x"))
        obj%br_angle_ave_z(i) = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="br_angle_ave_z"))
        obj%br_angle_sig_x(i) = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="br_angle_sig_x"))
        obj%br_angle_sig_z(i) = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="br_angle_sig_z"))
        obj%peti_size_ave(i)  = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="peti_size_ave"))
        obj%peti_size_sig(i)  = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="peti_size_sig"))
        obj%peti_width_ave(i) = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="peti_width_ave"))
        obj%peti_width_sig(i) = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="peti_width_sig"))
        obj%peti_angle_ave(i) = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="peti_angle_ave"))
        obj%peti_angle_sig(i) = freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="peti_angle_sig"))

        obj%leaf_thickness_ave(i)= freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="leaf_thickness_ave"))
        obj%leaf_thickness_sig(i)= freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="leaf_thickness_sig"))
        obj%leaf_angle_ave(i)= freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="leaf_angle_ave"))
        obj%leaf_angle_sig(i)= freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="leaf_angle_sig"))
        obj%leaf_length_ave(i)= freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="leaf_length_ave"))
        obj%leaf_length_sig(i)= freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="leaf_length_sig"))
        obj%leaf_width_ave(i)= freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="leaf_width_ave"))
        obj%leaf_width_sig(i)= freal(grapeconfig%parse(&
            config,key1="Branch#"//trim(str(i)),key2="leaf_width_sig"))
        
    enddo

    obj%mainroot_length = freal(grapeconfig%parse(config,key1="Mainroot",key2="Length"))
    obj%mainroot_width = freal(grapeconfig%parse(config,key1="Mainroot",key2="Width"))
    obj%mainroot_node = fint(grapeconfig%parse(config,key1="Mainroot",key2="Node"))



    ! get number of branch && number of node
    obj%num_branch_root=1
    obj%num_branch_root_node=0
    do 
        line = grapeconfig%parse(config,key1="Branchroot#"//trim(str(obj%num_branch_root)),key2="Node" )
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

    allocate(obj%leaf(obj%num_leaf*2))
    allocate(obj%stem(obj%num_stem*2))
    allocate(obj%root(obj%num_root*2))



    obj%leaf2stem = zeros( obj%num_leaf , obj%num_stem*2 ) 
    obj%stem2stem = zeros( obj%num_stem*2 , obj%num_stem*2 ) 
    obj%root2stem = zeros( obj%num_root , obj%num_stem*2 ) 
    obj%root2root = zeros( obj%num_root , obj%num_root ) 


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

    ! set branches
    k=obj%mainstem_node
    do i=1,size(obj%br_node)
        do j=1, obj%br_node(i)
            k = k + 1
            call obj%stem(k)%init()
            call obj%stem(k)%resize(&
                x = obj%br_width(i), &
                y = obj%br_width(i), &
                z = obj%br_length(i)/dble(obj%br_node(i) ) &
                )
                
            call obj%stem(k)%rotate(&
                x = radian(random%gauss(mu=obj%br_angle_ave_x(i),sigma=obj%br_angle_sig_x(i) )),  &
                y = 0.0d0,  &
                z = radian(random%gauss(mu=obj%br_angle_ave_z(i),sigma=obj%br_angle_sig_z(i) ))  &
                )                
            
            if(j==1)then
                call obj%stem(k)%connect("=>",obj%stem(obj%br_from(i)  ))
                obj%stem2stem(k,obj%br_from(i) ) = 1
            else
                call obj%stem(k)%connect("=>",obj%stem(k-1))
                obj%stem2stem(k,k-1) = 1
            endif   
        enddo
    enddo
!    


        ! peti and leaf
    num_stem_node = k
    num_leaf = 0
    do i=1, size(obj%br_node)
        do j=1,obj%br_node(i)
            ! ３複葉
            ! add peti
            num_stem_node = num_stem_node +1
            call obj%stem(num_stem_node)%init()

            call obj%stem(num_stem_node)%resize(&
                x = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
                y = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
                z = random%gauss(mu=obj%peti_size_ave(i),sigma=obj%peti_size_sig(i)) &
                )
            
            call obj%stem(num_stem_node)%rotate(&
                x = radian(random%gauss(mu=obj%peti_angle_ave(i),sigma=obj%peti_angle_sig(i) )),  &
                y = 0.0d0,  &
                z = radian(360.0d0*random%random() )   &
                )      
            
            !obj%leaf2stem(num_stem_node,i) = 1 
            if(i==1)then
                call obj%stem(num_stem_node)%connect("=>",obj%stem( j + obj%mainroot_node))
                obj%stem2stem(num_stem_node, j + obj%mainroot_node ) = 1            
            else
                call obj%stem(num_stem_node)%connect("=>",obj%stem( j + sum( obj%br_node(1:i-1) ) + obj%mainroot_node ))
                obj%stem2stem(num_stem_node, j + sum( obj%br_node(1:i-1) ) + obj%mainroot_node ) = 1            
            endif
            

            ! add leaves
            
            num_leaf=num_leaf+1
            !call obj%leaf(num_leaf)%create(filename=trim(obj%LeafSurfaceData))
            call obj%leaf(num_leaf)%create(filename=trim(obj%LeafSurfaceData) )
            call obj%leaf(num_leaf)%femdomain%resize(&
                    z = random%gauss(mu=obj%leaf_thickness_ave(i),sigma=obj%leaf_thickness_sig(i))  , &
                    x = random%gauss(mu=obj%leaf_length_ave(i)   ,sigma=obj%leaf_length_sig(i)) , &
                    y = random%gauss(mu=obj%leaf_width_ave(i)    ,sigma=obj%leaf_width_sig(i)) &
                )
            call obj%leaf(num_leaf)%femdomain%rotate(&
                    x = radian(random%gauss(mu=obj%leaf_angle_ave(i),sigma=obj%leaf_angle_sig(i))), &
                    y = 0.0d0, &
                    z = radian(random%random()*360.0d0) &
                )
            call obj%leaf(num_leaf)%connect("=>",obj%stem(num_stem_node))
                obj%leaf2stem(num_leaf,num_stem_node) = 1
        enddo
    enddo



end subroutine
! #############################################################


! ########################################
subroutine mshGrape(obj,name,num_threads)
    class(Grape_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n

    n = input(default=1,option=num_threads)
    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%stem)
        !if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%msh(name=trim(name)//"_stem"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%root)
        !if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%msh(name=trim(name)//"_root"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%leaf)
        !if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%msh(name=trim(name)//"_leaf"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel

end subroutine
! ########################################


! ########################################
subroutine vtkGrape(obj,name,num_threads)
    class(Grape_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i, n

    n = input(default=1,option=num_threads)
    if(allocated(obj%stem) )then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%stem)
            !if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%stem(i)%vtk(name=trim(name)//"_stem"//trim(str(i)))
            !endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif

    
    if(allocated(obj%root))then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%root)
            !if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%root(i)%vtk(name=trim(name)//"_root"//trim(str(i)))
            !endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif


    
    if(allocated(obj%leaf))then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%leaf)
            !if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%leaf(i)%vtk(name=trim(name)//"_leaf"//trim(str(i)))
            !endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif

end subroutine
! ########################################


! ########################################
subroutine jsonGrape(obj,name)
    class(Grape_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32) :: i,countnum
    type(IO_) :: f

    call f%open(trim(name)//".json")
    call f%write("{")
    countnum=0
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"stem"//trim(str(i))//'":')
            call obj%stem(i)%femdomain%json(name=trim(name)//"_stem"//trim(str(i)),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_stem":'//str(countnum)//',' )

    countnum=0
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"root"//trim(str(i))//'":')
            call obj%root(i)%femdomain%json(name=trim(name)//"_root"//trim(str(i)),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_root":'//str(countnum)//',' )
    
    countnum=0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"leaf"//trim(str(i))//'":')
            call obj%leaf(i)%femdomain%json(name=trim(name)//"_leaf"//trim(str(i)),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_leaf":'//str(countnum)//',' )
    call f%write('"return_Grape":0')
    call f%write("}")
    call f%close()
end subroutine
! ########################################

! ########################################
subroutine stlGrape(obj,name,num_threads)
    class(Grape_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n

    n = input(default=1,option=num_threads)
    !call execute_command_line("echo ' ' > "//trim(name)//".stl")
    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%stl(name=trim(name)//"_stem"//trim(str(i)))
            !call execute_command_line("cat "//trim(name)//"_stem"//trim(str(i))//"_000001.stl >> "//trim(name)//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%stl(name=trim(name)//"_root"//trim(str(i)))
            !call execute_command_line("cat "//trim(name)//"_root"//trim(str(i))//"_000001.stl >> "//trim(name)//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%stl(name=trim(name)//"_leaf"//trim(str(i)))
            !call execute_command_line("cat "//trim(name)//"_leaf"//trim(str(i))//"_000001.stl >> "//trim(name)//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel


end subroutine
! ########################################

! ########################################
subroutine moveGrape(obj,x,y,z)
    class(Grape_),intent(inout) :: obj
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

end subroutine
! ########################################



end module GrapeClass