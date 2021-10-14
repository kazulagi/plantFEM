module MaizeClass
    use LeafClass
    use StemClass
    use RootClass
    implicit none

    type :: Maize_

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


        integer(int32) :: num_branch_root
        integer(int32) :: num_branch_root_node

        real(real64) :: ms_angle_ave = 0.0d0
        real(real64) :: ms_angle_sig = 0.0d0

        integer(int32),allocatable :: Leaf_From(:)

        real(real64),allocatable :: leaf_Length(:)
        real(real64),allocatable :: leaf_Width(:)

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
        procedure :: create => createMaize
        procedure,public :: msh => mshMaize
        procedure,public :: vtk => vtkMaize
        procedure,public :: stl => stlMaize
        procedure,public :: json => jsonMaize
        procedure,public :: move => moveMaize
        procedure,public :: update => updateMaize
    end type
contains

! #############################################################
subroutine createMaize(obj,config)
    class(Maize_),intent(inout) :: obj
    character(*),intent(in) :: config
    character(:),allocatable :: line
    type(IO_) :: Maizeconfig
    type(Random_) :: random
    integer(int32)::i,n,j,k,num_leaf,num_stem_node,num_branch_branch


    obj%mainstem_length = freal(Maizeconfig%parse(config,key1="Mainstem",key2="Length"))
    obj%mainstem_width = freal(Maizeconfig%parse(config,key1="Mainstem",key2="Width"))
    obj%mainstem_node = fint(Maizeconfig%parse(config,key1="Mainstem",key2="Node"))
    obj%ms_angle_ave = freal(Maizeconfig%parse(config,key1="Mainstem",key2="ms_angle_ave"))
    obj%ms_angle_sig = freal(Maizeconfig%parse(config,key1="Mainstem",key2="ms_angle_sig"))

    
    ! get number of leaf
    obj%num_leaf=1
    do 
        line = Maizeconfig%parse(config,key1="Leaf#"//trim(str(obj%num_leaf)),key2="From" )
        if(len(trim(line))==0)then
            obj%num_leaf = obj%num_leaf -1
            exit
        else
            obj%num_leaf = obj%num_leaf  + 1
            cycle
        endif
    enddo

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
    allocate(obj%leaf_Length(obj%num_leaf))
    allocate(obj%leaf_Width(obj%num_leaf))

    do i=1,obj%num_leaf
        obj%leaf_From(i)= fint(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="From"))
        obj%leaf_Length(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="Length"))
        obj%leaf_Width(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="Width"))
        obj%leaf_curvature(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_curvature"))
        obj%leaf_thickness_ave(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_thickness_ave"))
        obj%leaf_thickness_sig(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_thickness_sig"))
        obj%leaf_angle_ave_x(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_angle_ave_x"))
        obj%leaf_angle_sig_x(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_angle_sig_x"))
        obj%leaf_angle_ave_z(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_angle_ave_z"))
        obj%leaf_angle_sig_z(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_angle_sig_z"))
        obj%leaf_length_ave(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_length_ave"))
        obj%leaf_length_sig(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_length_sig"))
        obj%leaf_width_ave(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_width_ave"))
        obj%leaf_width_sig(i)= freal(Maizeconfig%parse(&
            config,key1="Leaf#"//trim(str(i)),key2="leaf_width_sig"))
    enddo

    obj%mainroot_length = freal(Maizeconfig%parse(config,key1="Mainroot",key2="Length"))
    obj%mainroot_width = freal(Maizeconfig%parse(config,key1="Mainroot",key2="Width"))
    obj%mainroot_node = fint(Maizeconfig%parse(config,key1="Mainroot",key2="Node"))



    ! get number of branch && number of node
    obj%num_branch_root=1
    obj%num_branch_root_node=0
    do 
        line = Maizeconfig%parse(config,key1="Branchroot#"//trim(str(obj%num_branch_root)),key2="Node" )
        if(len(trim(line))==0)then
            obj%num_branch_root = obj%num_branch_root -1
            exit
        else
            obj%num_branch_root = obj%num_branch_root  + 1
            obj%num_branch_root_node = obj%num_branch_root_node + fint(line)
            cycle
        endif
    enddo

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
    obj%root2stem = zeros( obj%num_root , obj%num_stem) 
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
    
    !set leaf
    num_leaf = 0
    do i=1,obj%num_leaf
        ! 1葉/1節
        ! add leaves
        
        num_leaf=num_leaf+1
        !call obj%leaf(num_leaf)%create(filename=trim(obj%LeafSurfaceData))
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
            call obj%stem(i)%msh(name=trim(name)//"_stem"//trim(str(i)))
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%msh(name=trim(name)//"_root"//trim(str(i)))
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%msh(name=trim(name)//"_leaf"//trim(str(i)))
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

end subroutine
! ########################################


! ########################################
subroutine vtkMaize(obj,name,num_threads)
    class(Maize_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n

    n = input(default=1,option=num_threads)
    if(allocated(obj%stem) )then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%stem(i)%vtk(name=trim(name)//"_stem"//trim(str(i)))
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
                call obj%root(i)%vtk(name=trim(name)//"_root"//trim(str(i)))
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
                call obj%leaf(i)%vtk(name=trim(name)//"_leaf"//trim(str(i)))
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

end subroutine
! ########################################


! ########################################
recursive subroutine updateMaize(obj,debug)
    class(Maize_),intent(inout) :: obj
    integer(int32) :: i,j,this_stem_id,next_stem_id,A_id,B_id,itr_tol,itr
    integer(int32) :: this_leaf_id,next_leaf_id
    integer(int32) :: this_root_id,next_root_id
    real(real64) :: x_A(3),x_B(3),diff(3),error,last_error
    logical,optional,intent(in) :: debug
    ! update connectivity
    if(.not. allocated(obj%stem2stem ))then
        print *, "updateMaize >> ERROR :: .not. allocated(obj%stem2stem )"
        return
    endif

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
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%stem2stem,1)
            do j=1, size(obj%stem2stem,2)
                this_stem_id = j
                next_stem_id = i
                if(obj%stem2stem(i,j)/=0 .and. i /= j)then
                    ! this_stem_id ===>>> next_stem_id, connected!
                    x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    x_A(:) = obj%stem(next_stem_id)%getCoordinate("A")
                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%stem(next_stem_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "Maize % update >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "Maize % update >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error) == 0.0d0) exit
        last_error = error
    enddo

    ! root to root
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%root2root,1)
            do j=1, size(obj%root2root,2)
                this_root_id = j
                next_root_id = i
                if(obj%root2root(i,j)/=0 .and. i /= j)then
                    ! this_root_id ===>>> next_root_id, connected!
                    x_B(:) = obj%root(this_root_id)%getCoordinate("B")
                    x_A(:) = obj%root(next_root_id)%getCoordinate("A")
                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%root(next_root_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "Maize % update >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "Maize % update >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error) == 0.0d0) exit
        last_error = error
    enddo


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
                    x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    x_A(:) = obj%leaf(next_leaf_id)%getCoordinate("A")
                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%leaf(next_leaf_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "Maize % update >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "Maize % update >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error) == 0.0d0) exit
        last_error = error
    enddo

    
    
end subroutine
! ########################################


end module MaizeClass