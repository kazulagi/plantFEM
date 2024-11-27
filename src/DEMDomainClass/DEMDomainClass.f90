module DemDomainClass
    use RangeClass
    use FEMDomainClass
    implicit none

    integer(int32),public :: DEM_ACTIVE_PARTICLE = 0
    integer(int32),public :: DEM_DUMMY_PARTICLE  = 1

    type :: DEM_Particle_List_
        integer(int32) :: num_particle = 0
        integer(int32),allocatable :: particle(:)
    end type

    type :: DEM_3D_NeighborList_
        type(DEM_Particle_List_),allocatable :: grid(:,:,:)
    end type
    

    type :: DEMDomain_

        integer(int32) :: timestep 
        real(real64),allocatable :: xyz(:,:)
        real(real64),allocatable :: r(:)

        real(real64),allocatable :: m(:) ! 
        real(real64),allocatable :: u(:,:) ! displacement
        real(real64),allocatable :: v(:,:) ! velocity
        real(real64),allocatable :: a(:,:) ! accel.

        real(real64),allocatable :: f(:,:) ! force
        real(real64),allocatable :: g(:,:) ! gravity
        real(real64),allocatable :: contactForce(:,:) ! contact force
        integer(int32),allocatable :: status(:)

        real(real64),allocatable :: wall(:,:,:) ! wall(wall_idx,node_idx,x-z)
        type(DEM_3D_NeighborList_) :: NeighborList

        real(real64) :: contact_stiffness
        real(real64) :: contact_damping
        real(real64) :: grid_scale_factor = 5.0d0
    contains
        procedure,public :: init => initDEMDomainClass
        procedure,public :: closepack => closepackDEMDomainClass
        procedure,public :: np   => getNumberOfPointDEMDomain
        procedure,public :: nd   => getNumberOfDimDEMDomain
        procedure,public :: setWall => setWallDEMDomain
        procedure,public :: vtk  => vtkDEMDomain
        procedure,public :: getStiffnessMatrix => getStiffnessMatrixDEMDomain


        ! pre-processing
        procedure,pass :: addDEMDomain
        procedure,pass :: addDEMDomain_to_DEMDomain
        generic,public :: add => addDEMDomain, addDEMDomain_to_DEMDomain

        procedure,public :: updateneighborList   => updateneighborListDEMDomain
        procedure,public :: updateForce        => updateForceDEMDomain
        procedure,public :: updateDisplacement => updateDisplacementDEMDomain

        procedure,public :: StiffnessMatrix => StiffnessMatrixDEMDomain

        procedure,public :: distance => distanceDEMDomain

    end type 

    public :: assignment(=)

    interface addRow
        module procedure addRowInt32Mat,addRowreal64Mat
    end interface addRow

    interface toDEMDomain
        module procedure toDEMDomain_from_FEMDomain
    end interface

    
    
    interface assignment(=)
        module procedure FEMDomaintoDEMDomain_from_FEMDomain
    end interface

contains



subroutine initDEMDomainClass(this,x_axis,y_axis,z_axis,radius)
    class(DEMDomain_),intent(inout) :: this
    real(real64),intent(in) :: x_axis(:),y_axis(:),z_axis(:),radius(:)
    integer(int32) :: i,j,k,idx
    
    this%timestep = 0
    this%xyz = zeros(size(x_axis)*size(y_axis)*size(z_axis),3)
    this%status = int(zeros(this%np()) )
    this%status(:) = DEM_ACTIVE_PARTICLE
    do i=1,size(x_axis)
        do j=1,size(y_axis)
            do k=1,size(z_axis)
                idx = (i-1)*size(y_axis)*size(z_axis) + (j-1)*size(z_axis) + k
                this%xyz(idx,1) = x_axis(i)
                this%xyz(idx,2) = y_axis(j)
                this%xyz(idx,3) = z_axis(k)
            enddo
        enddo
    enddo
    if(size(radius)==1)then
        this%r = radius(1)*ones(size(this%xyz,1))
    else
        this%r = radius
    endif

    this%m = ones(this%np())
    this%f = zeros(this%np(),this%nd())
    this%u = zeros(this%np(),this%nd())
    this%v = zeros(this%np(),this%nd())
    this%a = zeros(this%np(),this%nd())
    this%g = zeros(this%np(),this%nd())
    this%contactForce = zeros(this%np(),this%nd())

    this%g(:,3) = -9.810d0
    this%contact_stiffness = 1.0d0
end subroutine


subroutine addDEMDomain(this,position,r,m,status)
    class(DEMDomain_),intent(inout) :: this
    real(real64),intent(in) :: position(:),r,m
    integer(int32),optional,intent(in) :: status

    call extend(this%xyz,extend1stColumn=.true.)
    this%xyz(size(this%xyz,1),:) = position(:)
    this%r = this%r // [r]
    this%m = this%m // [m]
    this%status = this%status // [status]
    
    
    

    call addRow(this%u)
    call addRow(this%v)
    call addRow(this%a)
    call addRow(this%f)
    call addRow(this%g)
    call addRow(this%contactForce)
    

end subroutine

subroutine vtkDEMDomain(this,name,displacement)
    class(DEMDomain_),intent(in) :: this
    character(*),intent(in) :: name
    real(real64),optional,intent(in):: displacement(:)
    real(real64),allocatable :: disp(:)
    type(IO_) :: f
    integer(int32) :: i

    if (".vtk" .in. name)then        
        call f%open(name,"w")
    else
        call f%open(name+".vtk","w")
    endif

    call f%write("# vtk DataFile Version 3.0")
    call f%write(name+".vtk")
    call f%write("ASCII")
    call f%write("DATASET UNSTRUCTURED_GRID")
    call f%write("POINTS "+str(this%np())+" double")
    disp = zeros(this%nd())
    do i=1,this%np()
        if (present(displacement))then
            disp(:) = displacement( (i-1)*this%nd()+1: (i-1)*this%nd()+this%nd())
        endif
        write(f%fh,*) this%xyz(i,:) + disp(:)
    enddo
    call f%write("CELL_TYPES "+str(this%np()))
    call f%write(int(ones(this%np()) ))
    call f%write("POINT_DATA "+str(this%np()))
    call f%write("SCALARS radius double")
    call f%write("LOOKUP_TABLE default")
    call f%write(this%r)
    call f%close()


end subroutine

elemental function getNumberOfPointDEMDomain(this) result(ret)
    class(DEMDomain_),intent(in) :: this
    integer(int32) :: ret

    ret = size(this%xyz,1)

end function

elemental function getNumberOfDimDEMDomain(this) result(ret)
    class(DEMDomain_),intent(in) :: this
    integer(int32) :: ret

    ret = size(this%xyz,2)

end function

subroutine updateDisplacementDEMDomain(this,dt,active_range)
    class(DEMDomain_),intent(inout) :: this
    real(real64),intent(in) :: dt
    integer(int32) :: pointIdx,i
    type(Range_),intent(in) :: active_range

    ! forward Euler
    do i=1,size(this%a,1)
        this%a(i,:) = this%g(i,:) + this%f(i,:)/this%m(i)
    enddo
    this%v = this%v + this%a*dt 
    this%u = this%v*dt + 1.0d0/2.0d0*this%a*dt*dt 


    do pointIdx=1,this%np()
        if (this%xyz(pointIdx,:) .in. active_range)then
            cycle
        else
            this%a(pointIdx,:) = 0.0d0
            this%v(pointIdx,:) = 0.0d0
            this%u(pointIdx,:) = -this%u(pointIdx,:)*this%contact_stiffness
        endif
    enddo

    

    do i=1,size(this%xyz,1)
        if(this%status(i)==DEM_DUMMY_PARTICLE)then
            this%u(i,:) = 0.0d0
            this%v(i,:) = 0.0d0
            this%a(i,:) = 0.0d0
        endif
    enddo

    this%xyz = this%xyz + this%u
end subroutine


subroutine updateForceDEMDomain(this,dt)
    class(DEMDomain_),intent(inout) :: this
    real(real64),intent(in) :: dt
    real(real64),allocatable :: direction(:),relative_veocity(:),contactForce(:,:)
    type(IO_) :: f
    
    real(real64)   :: distance_len,overlap
    integer(int32) :: pointIdx, i, j, contact_algorithm, DEFAULT_ALGORITHM,EXPMTAL_ALGORITHM,&
        x_idx,y_idx,z_idx,x_min,y_min,z_min,x_max,y_max,z_max,x,y,z,grid_size,idx1,idx2,idx3
    integer(int32),allocatable :: gridIdxList(:,:),order(:),max_idx(:),point_list(:)

    ! contact detection & update contact force
    contactForce = 0.0d0*this%xyz
    
    DEFAULT_ALGORITHM = 0
    EXPMTAL_ALGORITHM = 1
    contact_algorithm = EXPMTAL_ALGORITHM
    !contact_algorithm = DEFAULT_ALGORITHM

    if(contact_algorithm==DEFAULT_ALGORITHM)then
        !$OMP parallel do private(direction,relative_veocity,overlap,j,distance_len) reduction(+:contactForce)
        do pointIdx=1,this%np()
            do j=pointIdx+1,this%np()
                distance_len = sqrt(dot_product(this%xyz(pointIdx,:)-this%xyz(j,:),this%xyz(pointIdx,:)-this%xyz(j,:)))
                if(distance_len <= (this%r(pointIdx)+this%r(j)) )then
                        ! contact detected!
                        if(distance_len==0)cycle
                        ! 初回接触時から法線方向を保存するべき?
                        direction = (this%xyz(pointIdx,:)-this%xyz(j,:))/distance_len
                        relative_veocity = this%v(pointIdx,:)-this%v(j,:)
                        overlap   = (this%r(pointIdx)+this%r(j)) - distance_len

                        contactForce(pointIdx,:) = contactForce(pointIdx,:) + this%contact_stiffness*abs(overlap)*direction(:) &
                            - this%contact_damping*relative_veocity(:)
                        contactForce(j       ,:) = contactForce(pointIdx,:) -this%contact_stiffness*abs(overlap)*direction(:) &
                            + this%contact_damping*relative_veocity(:)
                endif
            end do
        enddo
        !$OMP end parallel do
    else
        ! UNDER DEVELOPMENT
        ! ONLY FOR 3D
        call this%updateNeighborList()
        

        x_min = minval(this%xyz(:,1));x_max = maxval(this%xyz(:,1))
        y_min = minval(this%xyz(:,2));y_max = maxval(this%xyz(:,2))
        z_min = minval(this%xyz(:,3));z_max = maxval(this%xyz(:,3))
        grid_size = average(this%r)*this%grid_scale_factor
        
        !$OMP parallel do private(direction,relative_veocity,overlap,j,distance_len,idx1,idx2,idx3) reduction(+:contactForce)
        do pointIdx=1,this%np()
            ! same group
            x = this%xyz(pointIdx,1)
            y = this%xyz(pointIdx,2)
            z = this%xyz(pointIdx,3)
            x_idx = int((x-x_min)/dble(grid_size)) + 1
            y_idx = int((y-y_min)/dble(grid_size)) + 1
            z_idx = int((z-z_min)/dble(grid_size)) + 1
            
            do idx3=z_idx-1,z_idx+1
                if(idx3 > size(this%NeighborList%grid,3) ) cycle
                if(idx3 < 1 ) cycle
                do idx2=y_idx-1,y_idx+1
                    if(idx2 > size(this%NeighborList%grid,2) ) cycle
                    if(idx2 < 1 ) cycle
                    do idx1=x_idx-1,x_idx+1
                        if(idx1 > size(this%NeighborList%grid,1) ) cycle
                        if(idx1 < 1 ) cycle

                        if(.not. allocated(this%NeighborList%grid(idx1,idx2,idx3)%particle) ) cycle
                        
                        
                        do i = 1,size(this%NeighborList%grid(idx1,idx2,idx3)%particle(:))
                            if(this%NeighborList%grid(idx1,idx2,idx3)%particle(i)==pointIdx ) cycle
                            j = this%NeighborList%grid(idx1,idx2,idx3)%particle(i)
                            distance_len = sqrt(dot_product(this%xyz(pointIdx,:)-this%xyz(j,:),&
                                this%xyz(pointIdx,:)-this%xyz(j,:)))
                            if(distance_len <= (this%r(pointIdx)+this%r(j)) )then
                                ! contact detected!
                                direction = (this%xyz(pointIdx,:)-this%xyz(j,:))/distance_len
                                relative_veocity = this%v(pointIdx,:)-this%v(j,:)
                                overlap   = (this%r(pointIdx)+this%r(j)) - distance_len
                                contactForce(pointIdx,:) = contactForce(pointIdx,:) &
                                    + this%contact_stiffness*abs(overlap)*direction(:) &
                                    - this%contact_damping*relative_veocity(:)
                                contactForce(j       ,:) = contactForce(pointIdx,:) &
                                    - this%contact_stiffness*abs(overlap)*direction(:) &
                                    + this%contact_damping*relative_veocity(:)
                            endif
                        enddo
                        
                    enddo
                enddo
            enddo
        enddo
        !$OMP end parallel do



!        if(.not.allocated(this%NeighborList%row) )then
!            call this%NeighborList%init(this%np())
!        endif

        ! 演算コア数分割り振る
        
!        ! create Neighbor-list
!        ! (1) rの大きさのグリッドで切って，隣接3x3x3=27グリッド以内にあるものをグループにする．
!        gridIdxList = int(zeros(this%np(),this%nd()))
!        !$OMP parallel do shared(gridIdxList)
!        do pointIdx=1,this%np()
!            gridIdxList(pointIdx,:) = int(this%xyz(pointIdx,:)/minval(this%r))
!        enddo
!        !$OMP end parallel do
!
!        ! (2) x-y-zグリッドリストでソートする．
!        max_idx = int(zeros(this%nd()) )
!        do i=1,this%nd()
!            max_idx(i) = maxval(this%xyz(:,i)/minval(this%r)) - minval(this%xyz(:,i)/minval(this%r)) + 1 
!        enddo
!
!        order = [(i,i=1,this%np())]
!        call heapsort(array=gridIdxList,order=order,exec_row_sort=.true.)
!!        do pointIdx=1,this%np()
!!            this%NeighborList%add(row=pointIdx,col=,value=)
!!        enddo
!        
!        call print(gridIdxList)
!        stop

    endif
    
    this%contactForce = contactForce


    ! update force
    this%f = this%contactForce

end subroutine



subroutine setWallDEMDomain(this,range)
    class(DEMDomain_),intent(inout) :: this
    type(Range_),intent(in) :: range
    
end subroutine

subroutine addRowInt32Mat(mat)
    integer(int32),allocatable,intent(inout) :: mat(:,:)
    integer(int32),allocatable :: old_mat(:,:)

    old_mat = mat
    deallocate(mat)
    allocate(mat(size(old_mat,1)+1,size(old_mat,2) ))
    mat(1:size(old_mat,1),:) = old_mat(:,:)
    mat(size(mat,1),: ) = 0

end subroutine

subroutine addRowreal64Mat(mat)
    real(real64),allocatable,intent(inout) :: mat(:,:)
    real(real64),allocatable :: old_mat(:,:)

    old_mat = mat
    deallocate(mat)
    allocate(mat(size(old_mat,1)+1,size(old_mat,2) ))
    mat(1:size(old_mat,1),:) = old_mat(:,:)
    mat(size(mat,1),: ) = 0

end subroutine


function toDEMDomain_from_FEMDomain(femdomain) result(ret)
    type(FEMDomain_),intent(in) :: femdomain
    type(DEMDomain_) :: ret
    integer(int32) :: elemIdx,j
    integer(int32),allocatable :: num_dup(:)
    real(real64),allocatable :: center(:),x(:)
    real(real64) :: r


    ret%timestep = 0
    ret%xyz = femdomain%mesh%nodcoord
    ret%m   = ones(femdomain%nn()) 
    
    ! get radius
    ret%r   = zeros(femdomain%nn()) 
    num_dup = int(zeros(femdomain%nn()))
    do elemIdx=1,femdomain%ne()
        center = femdomain%getCenter(elemIdx)
        do j=1,femdomain%nne()
            x = femdomain%mesh%nodcoord(femdomain%mesh%elemnod(elemIdx,j),:)
            r = sqrt(dot_product(r-center,r-center))
            ret%r(femdomain%mesh%elemnod(elemIdx,j)) = r
            num_dup(femdomain%mesh%elemnod(elemIdx,j)) = num_dup(femdomain%mesh%elemnod(elemIdx,j)) + 1
        enddo
    enddo

    ret%u = zeros(femdomain%nn(),femdomain%nd())
    ret%v = zeros(femdomain%nn(),femdomain%nd())
    ret%a = zeros(femdomain%nn(),femdomain%nd())

    ret%f = zeros(femdomain%nn(),femdomain%nd())!(:,:) ! force
    ret%g = zeros(femdomain%nn(),femdomain%nd())!(:,:) ! gravity
    ret%contactForce = zeros(femdomain%nn(),femdomain%nd())!(:,:) ! contact force
    
    ret%status = DEM_ACTIVE_PARTICLE*int(ones(femdomain%nn()))

    ret%contact_stiffness = 0.0d0
    ret%contact_damping = 0.0d0

end function


subroutine FEMDomaintoDEMDomain_from_FEMDomain(ret,femdomain)
    type(FEMDomain_),intent(in) :: femdomain
    type(DEMDomain_),intent(out) :: ret
    integer(int32) :: elemIdx,j
    integer(int32),allocatable :: num_dup(:)
    real(real64),allocatable :: center(:),x(:),radius(:)
    real(real64) :: r


    ret%timestep = 0
    ret%xyz = femdomain%mesh%nodcoord
    ret%m   = ones(femdomain%nn()) 
    
    ! get radius
    radius   = zeros(femdomain%nn()) 
    num_dup = int(zeros(femdomain%nn()))
    !!$OMP parallel do private(center,j,x,r) reduction(+:radius,num_dup)
    !do elemIdx=1,femdomain%ne()
    !    center = femdomain%getCenter(elemIdx)
    !    do j=1,femdomain%nne()
    !        x = femdomain%mesh%nodcoord(femdomain%mesh%elemnod(elemIdx,j),:)
    !        r = sqrt(dot_product(r-center,r-center))
    !        radius(femdomain%mesh%elemnod(elemIdx,j)) = radius(femdomain%mesh%elemnod(elemIdx,j)) + r
    !        num_dup(femdomain%mesh%elemnod(elemIdx,j)) = num_dup(femdomain%mesh%elemnod(elemIdx,j)) + 1
    !    enddo
    !enddo
    !!$OMP end parallel do

    ret%r = ones(femdomain%nn()) !radius(:)/dble(num_dup(:))

    ret%u = zeros(femdomain%nn(),femdomain%nd())
    ret%v = zeros(femdomain%nn(),femdomain%nd())
    ret%a = zeros(femdomain%nn(),femdomain%nd())

    ret%f = zeros(femdomain%nn(),femdomain%nd())!(:,:) ! force
    ret%g = zeros(femdomain%nn(),femdomain%nd())!(:,:) ! gravity
    ret%contactForce = zeros(femdomain%nn(),femdomain%nd())!(:,:) ! contact force
    
    ret%status = DEM_ACTIVE_PARTICLE*int(ones(femdomain%nn()))

    ret%contact_stiffness = 0.0d0
    ret%contact_damping = 0.0d0

    ret%g(:,3) = -9.810d0

end subroutine


subroutine addDEMDomain_to_DEMDomain(this,DEMDomain)
    class(DEMDomain_),intent(inout) :: this
    type(DEMDomain_),intent(in) :: DEMDomain

    
    this%xyz = this%xyz  .v. DEMDomain%xyz
    this%m = this%m // DEMDomain%m
    this%r = this%r // DEMDomain%r
    this%u = this%u .v. DEMDomain%u
    this%v = this%v .v. DEMDomain%v
    this%a = this%a .v. DEMDomain%a
    this%f = this%f .v. DEMDomain%f
    this%g = this%g .v. DEMDomain%g
    this%contactForce = this%contactForce .v. DEMDomain%contactForce
    this%status = this%status // DEMDomain%status
    

end subroutine



subroutine updateNeighborListDEMDomain(this)
    class(DEMDomain_),intent(inout) :: this

    real(real64) :: grid_size
    real(real64),allocatable   :: min_loc_coord(:)
    integer(int32),allocatable :: num_grid(:),grid_idx(:,:),max_grid_idx(:),min_grid_idx(:),&
        num_particle_per_grid(:),num_particle_array(:,:,:)
    integer(int32) :: i,j,k,n,total_grid_num,one_d_grid_idx


    if(allocated(this%NeighborList%grid) )then
        deallocate(this%NeighborList%grid)
    endif

    min_loc_coord = zeros(this%nd())
    ! move xyz
    do i=1,this%nd()
        min_loc_coord(i) = minval(this%xyz(:,i))
        this%xyz(:,i) = this%xyz(:,i) - min_loc_coord(i)
    enddo
    ! origin = (0,0,0)

    grid_size = average(this%r)*this%grid_scale_factor
    grid_idx = int(0.0d0*this%xyz)
    max_grid_idx = int(zeros(this%nd()) )
    min_grid_idx = int(zeros(this%nd()) )

    !$OMP parallel do private(j) shared(grid_idx)
    do i=1,this%np()
        do j=1,this%nd()
            grid_idx(i,j) = this%xyz(i,j)/grid_size
        enddo
    enddo
    !$OMP end parallel do

    ! CAUTION :: gridIdx starts from zero

    total_grid_num = 1
    do i=1,this%nd()
        total_grid_num = total_grid_num * (max_grid_idx(i)+1)
        max_grid_idx(i) = maxval(grid_idx(:,i))
    enddo

    if(allocated(this%NeighborList%grid) )then
        deallocate(this%NeighborList%grid)
    endif

    allocate(this%NeighborList%grid(max_grid_idx(1)+1,max_grid_idx(2)+1,max_grid_idx(3)+1))
    allocate(num_particle_array(max_grid_idx(1)+1,max_grid_idx(2)+1,max_grid_idx(3)+1))
    
    num_particle_array(:,:,:) = 0
    !$OMP parallel do reduction(+:num_particle_array)
    do i=1,size(grid_idx,1)
        !this%NeighborList%grid( grid_idx(i,1)+1, grid_idx(i,2)+1, grid_idx(i,3)+1 )%num_particle = &
        !    + this%NeighborList%grid( grid_idx(i,1)+1, grid_idx(i,2)+1, grid_idx(i,3)+1 )%num_particle + 1
        num_particle_array(grid_idx(i,1)+1, grid_idx(i,2)+1, grid_idx(i,3)+1) = &
            num_particle_array(grid_idx(i,1)+1, grid_idx(i,2)+1, grid_idx(i,3)+1) + 1
    enddo
    !$OMP end parallel do
    

    !$OMP parallel do private(j,k)
    do i=1,size(this%NeighborList%grid,1)
        do j=1,size(this%NeighborList%grid,2)
            do k=1,size(this%NeighborList%grid,3)
                if(num_particle_array(i,j,k)==0)then
                    cycle
                else
                    allocate(this%NeighborList%grid(i,j,k)%particle(num_particle_array(i,j,k)))
                    this%NeighborList%grid(i,j,k)%num_particle = 0
                endif
            enddo
        enddo
    enddo
    !$OMP end parallel do
    deallocate(num_particle_array)
    
    do i=1,size(grid_idx,1)
        if(.not.allocated(this%NeighborList%grid( grid_idx(i,1)+1, grid_idx(i,2)+1, grid_idx(i,3)+1 )%particle) ) cycle
        n = this%NeighborList%grid( grid_idx(i,1)+1, grid_idx(i,2)+1, grid_idx(i,3)+1 )%num_particle
        n = n + 1
        this%NeighborList%grid( grid_idx(i,1)+1, grid_idx(i,2)+1, grid_idx(i,3)+1 )%num_particle = n
        this%NeighborList%grid( grid_idx(i,1)+1, grid_idx(i,2)+1, grid_idx(i,3)+1 )%particle(n)  = i 
    enddo

    ! move xyz
    do i=1,this%nd()
        min_loc_coord(i) = minval(this%xyz(:,i))
        this%xyz(:,i) = this%xyz(:,i) + min_loc_coord(i)
    enddo
end subroutine

! USE BACKGROUND MESH INSTEAD OF OCTREE GRID
!recursive function grouping_point_by_octree(xyz,range,minimal_r) result(group_idx_list)
!    real(real64),intent(in) :: xyz(:,:)
!    type(Range_),intent(in)  :: range
!
!    type(Range_),allocatable :: subranges(:)
!    real(real64),allocatable :: sub_xyz(:,:)
!    
!    real(real64),intent(in)  :: minimal_r
!    integer(int32),allocatable :: group_idx_list(:),subrange_idx(:),num_pt(:),sub_group_idx_list(:)
!    type(COO_) :: group_list
!    integer(int32) :: subdomain_idx,i,j
!
!    subdomain_idx = 0
!    
!    ! octreeにより粒子をグルーピング O(NlogN)
!
!    ! いずれかのsubrangeがminimal_rより小さければ，戻る．
!    if(size(xyz,2)==1)then
!        if(range%x_range(2)-range%x_range(1) <= minimal_r ) then
!            return
!        endif
!    elseif(size(xyz,2)==2)then
!        if(range%x_range(2)-range%x_range(1) <= minimal_r ) then
!            return
!        endif
!        if(range%y_range(2)-range%y_range(1) <= minimal_r ) then
!            return
!        endif
!        
!    elseif(size(xyz,2)==3)then
!        if(range%x_range(2)-range%x_range(1) <= minimal_r ) then
!            return
!        endif
!        if(range%y_range(2)-range%y_range(1) <= minimal_r ) then
!            return
!        endif
!        if(range%z_range(2)-range%z_range(1) <= minimal_r ) then
!            return
!        endif
!    else
!        return
!    endif
!    
!    allocate(group_idx_list(size(xyz,1)))
!    allocate(num_pt(2**size(xyz,2)) )
!    num_pt(:) = 0
!
!    ! octree subdomain idx
!    subranges = range%getSubrange(dim=size(xyz,2),n=2)
!    do i=1,size(xyz,1)
!        group_idx_list(i) = range%getSubrangeIdx(xyz=xyz(i,:),dim=size(xyz,2),n=2)
!        num_pt(group_idx_list(i)) = num_pt(group_idx_list(i)) + 1 
!    enddo
!
!    call group_list%init(2**size(xyz,2))
!    
!    do i=1,size(group_list%row)
!        allocate(group_list%row(i)%col(num_pt(i)))
!        num_pt(i) = 0
!    enddo
!
!    do i=1,size(group_idx_list)
!        num_pt(group_idx_list(i)) = num_pt(group_idx_list(i)) + 1
!        group_list%row(i)%col(num_pt(group_idx_list(i))) = group_idx_list(i)
!    enddo
!
!    do i=1,size(group_list%row)
!        ! for each group
!        allocate(sub_xyz(num_pt(i),size(xyz,2)))
!        do j=1,size(group_list%row(i)%col)
!            sub_xyz(j,:) = xyz(group_list%row(i)%col(j),:)
!        enddo
!        sub_group_idx_list = grouping_point_by_octree(&
!            xyz=sub_xyz,&
!            range=subranges(i),&
!            minimal_r=minimal_r )
!        do j=1,size(group_list%row(i)%col)
!            group_idx_list(group_list%row(i)%col(j)) = &
!                  (group_idx_list(group_list%row(i)%col(j))-1)*size(group_list%row) &
!                + sub_group_idx_list(j)
!        enddo
!    enddo
!
!end function
!
function StiffnessMatrixDEMDomain(this,springCoefficient) result(ret)
    class(DEMDomain_),intent(in) :: this
    real(real64),intent(in) :: springCoefficient(:)
    type(CRS_) :: ret

    ! under implementation

end function


subroutine closepackDEMDomainClass(this,radius,length)
    class(DEMDomain_),intent(inout) :: this
    real(real64),intent(in) :: radius,length(3)
    integer(int32) :: i,j,k,n_xy,n_xyz,m,nx,mx,ny,nz,idx,lx,ly,lz
    real(real64) :: dx,dy

    
    lx = length(1)
    ly = length(2)
    lz = length(3)

    ! rangeに従い，最密充填構造を作成する．
    nx = int(lx/(2*radius)) + 1
    mx = int(lx/(2*radius)) 
    ny = int(ly/((radius)*sqrt(3.0d0))) 
    nz = int(lz/((2*radius)*sqrt(6.0d0)/3.0d0)) + 1


    if (mod(ny,2)==0)then
        ! even
        n_xy = (nx + mx)*(ny/2)
    else
        ! odd
        n_xy = (nx + mx)*(ny/2) + nx
    endif
    

    this%xyz = zeros(n_xy*nz,3)
    this%r = radius*ones(n_xy*nz)

    idx  = 0
    do k=1,nz
        if (mod(k,2)==1)then
            dx = 0.0d0
            dy = 0.0d0

            do j=1,ny
                if (mod(j,2)==1)then
                    do i=1,nx
                        idx = idx + 1
                        this%xyz(idx,1) = (i-1)*(2.0d0*radius) + dx
                        this%xyz(idx,2) = (j-1)*(sqrt(3.0d0)*radius) + dy
                        this%xyz(idx,3) = (k-1)*((2*radius)*sqrt(6.0d0)/3.0d0) 
                    enddo
                else
                    do i=1,mx
                        idx = idx + 1
                        this%xyz(idx,1) = (i-1)*(2.0d0*radius) + radius + dx
                        this%xyz(idx,2) = (j-1)*(sqrt(3.0d0)*radius) + dy
                        this%xyz(idx,3) = (k-1)*((2*radius)*sqrt(6.0d0)/3.0d0) 
                    enddo
                endif
            enddo
        else
            
            dx = radius
            dy = radius*sqrt(3.0d0)*(1.0d0/3.0d0)

            do j=1,ny
                if (mod(j,2)==1)then
                    do i=1,nx
                        idx = idx + 1
                        this%xyz(idx,1) = (i-1)*(2.0d0*radius) + dx
                        this%xyz(idx,2) = (j-1)*(sqrt(3.0d0)*radius) + dy
                        this%xyz(idx,3) = (k-1)*((2*radius)*sqrt(6.0d0)/3.0d0) 
                    enddo
                else
                    do i=1,mx
                        idx = idx + 1
                        this%xyz(idx,1) = (i-1)*(2.0d0*radius) + radius + dx
                        this%xyz(idx,2) = (j-1)*(sqrt(3.0d0)*radius) + dy
                        this%xyz(idx,3) = (k-1)*((2*radius)*sqrt(6.0d0)/3.0d0) 
                    enddo
                endif
            enddo
        endif
        
    enddo

end subroutine
! ####################################################################


! ####################################################################
function distanceDEMDomain(this,idx1,idx2) result(ret)
    class(DEMDomain_),intent(in) :: this
    integer(int32),intent(in) :: idx1,idx2
    real(real64) :: ret

    ret = sqrt(dot_product(this%xyz(idx1,:)-this%xyz(idx2,:),this%xyz(idx1,:)-this%xyz(idx2,:)))

end function
! ####################################################################


! ####################################################################
function getStiffnessMatrixDEMDomain(this) result(ret)
    class(DEMDomain_),intent(in) :: this
    type(CRS_) :: ret
    type(COO_) :: ret_coo
    integer(int32) :: i,j,k1,k2,nd
    real(real64),allocatable :: normal_vector(:),mat(:,:)
    
    ! 接しているものとbondingを形成
    ! normal and tangential
    call ret_coo%init(this%np()*3)
    nd = size(this%xyz,2)
    do i=1,this%np()-1
        do j=i+1,this%np()
            if(this%distance(i,j) <= this%r(i) + this%r(j)  )then
                normal_vector = (this%xyz(j,:) - this%xyz(i,:) )
                normal_vector = normal_vector/sqrt(dot_product(normal_vector,normal_vector))
                mat = diadic(normal_vector,normal_vector)
                do k1=1,nd
                    do k2=1,nd
                        call ret_coo%add((i-1)*nd+k1,(j-1)*nd+k2,mat(k1,k2))
                    enddo
                enddo
                do k1=1,nd
                    do k2=1,nd
                        call ret_coo%add((j-1)*nd+k1,(i-1)*nd+k2,mat(k1,k2))
                    enddo
                enddo

                do k1=1,nd
                    do k2=1,nd
                        call ret_coo%add((i-1)*nd+k1,(i-1)*nd+k2,-mat(k1,k2))
                    enddo
                enddo
                do k1=1,nd
                    do k2=1,nd
                        call ret_coo%add((j-1)*nd+k1,(j-1)*nd+k2,-mat(k1,k2))
                    enddo
                enddo

            endif
        enddo
    enddo
    ret = ret_coo%to_crs()



end function
! ####################################################################

end module DemDomainClass