module PanicleClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use StemClass
    use FEMDomainClass

    implicit none

    type :: Panicle_
        type(FEMDomain_)    ::  FEMDomain
        real(real64)        :: Length,Width,Angle
        type(Stem_),pointer ::  pStem
        integer(int32) :: division(1:3) = [5,5,5] ! for maize
        integer(int32) :: rice_seed_division(1:3) = [3,3,3] ! for rice


        integer(int32),allocatable  :: I_planeNodeID(:)
        integer(int32),allocatable  :: I_planeElementID(:)
        integer(int32),allocatable  :: II_planeNodeID(:)
        integer(int32),allocatable  :: II_planeElementID(:)
        integer(int32)  :: A_PointNodeID
        integer(int32)  :: B_PointNodeID
        integer(int32)  :: C_PointNodeID
        integer(int32)  :: D_PointNodeID
        
        integer(int32)  :: A_PointElementID
        integer(int32)  :: B_PointElementID

        real(real64) :: default_rice_seed_interval  = 3.0d0/1000.0d0 ! 3 mm 
        real(real64) :: default_rice_seed_branch_length = 3.0d0/1000.0d0 ! 2 mm 
        real(real64) :: default_rice_seed_length    = 6.0d0/1000.0d0 ! 2 mm 
        real(real64) :: default_rice_seed_width     = 4.0d0/1000.0d0 ! 2 mm 
        real(real64) :: default_rice_seed_thickness = 2.0d0/1000.0d0 ! 2 mm 
        real(real64) :: default_rice_panicle_curvature =  0.20d0

        real(real64) :: disp_x
        real(real64) :: disp_y
        real(real64) :: disp_z

        ! For deformation analysis
        real(real64),allocatable :: YoungModulus(:)! element-wise
        real(real64),allocatable :: PoissonRatio(:)! element-wise
        real(real64),allocatable :: Density(:)     ! element-wise
        real(real64),allocatable :: Stress(:,:,:)     ! Gauss point-wise
        real(real64),allocatable :: Displacement(:,:) ! node-wise, three dimensional


    contains
        procedure, public :: Init => initPanicle
        procedure, public :: move => movePanicle
        procedure, public :: rotate => rotatePanicle
        procedure, public :: getCoordinate => getCoordinatePanicle
        procedure, public :: connect => connectPanicle
        procedure, public :: vtk => vtkPanicle
        procedure, public :: stl => stlPanicle
        procedure, public :: remove => removePanicle
    end type

contains

! #####################################################
recursive subroutine initPanicle(this,Length,Width,Node,shape_factor,debug,x_num,y_num,z_num,rice,&
    rice_seed_interval,rice_seed_branch_length,&
    rice_seed_length,rice_seed_width,rice_seed_thickness,&
    rice_panicle_curvature,rice_seed_division)
    class(Panicle_),intent(inout) :: this
    real(real64),intent(in) :: Length, width ! need for all panicle type

    integer(int32),optional,intent(in) :: Node ! for maize
    integer(int32),optional,intent(in) :: x_num,y_num,z_num ! for all
    integer(int32),optional,intent(in) :: rice_seed_division(1:3)
    real(real64),optional,intent(in) :: rice_seed_interval,rice_seed_branch_length,& ! for rice 
    rice_seed_length,rice_seed_width,rice_seed_thickness,&
    rice_panicle_curvature
    real(real64),optional,intent(in) :: shape_factor ! only for Maize
    !integer(int32),optional,intent(in) :: rice_panicle_branch_num  ! for rice
    
    real(real64):: Angle
    type(Math_) :: math
    type(Random_) :: random
    real(real64) :: dist_val
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:),z_axis0(:)
    integer(int32) :: i,j
    integer(int32),allocatable :: buf(:), kill_element_list(:)
    logical,optional,intent(in) :: debug,rice

    type(FEMDomain_) :: femdomain
    type(FEMDomain_) :: seed,this_seed
    type(Panicle_),allocatable    :: branch_panicle(:)
    type(FEMDomain_),allocatable :: seeds(:),panicles(:)

    real(real64) :: center_coord(1:3)
    real(real64) :: shape_factor_val 

    integer(int32),allocatable :: seed_joint(:)

    real(real64),allocatable :: nodcoord(:,:)
    integer(int32),allocatable :: elemnod(:,:)

    real(real64) :: seed_interval, seed_branch_length,panicle_curvature

    real(real64) :: ln, normal_vector(1:3),x1(1:3),x2(1:3)
    integer(int32) :: elem_idx,n1,n2,n3,n4,num_seed,node_list(1:4)
    integer(int32) :: case_id, nx, ny, nz
    real(real64) :: a(2,3), b(2,3),x,y,z,center(1:3),alpha,theta,&
        seed_length, seed_width, seed_thickness,dx,dy,dz,r,z_max



    logical :: rice_mode


    rice_mode = input(default=.false.,option=rice)
    if(rice_mode)then
        seed_interval  =input(default=this%default_rice_seed_interval,&
            option=rice_seed_interval)
        seed_branch_length =input(default=this%default_rice_seed_branch_length,&
            option=rice_seed_branch_length)
        seed_length    =input(default=this%default_rice_seed_length,&
            option=rice_seed_length)
        seed_width     =input(default=this%default_rice_seed_width,&
            option=rice_seed_width)
        seed_thickness =input(default=this%default_rice_seed_thickness,&
            option=rice_seed_thickness)
        panicle_curvature =input(default=this%default_rice_panicle_curvature,&
            option=rice_panicle_curvature)

        if(present(rice_seed_division) )then
            this%rice_seed_division = rice_seed_division
        endif
        nx = this%rice_seed_division(1)
        ny = this%rice_seed_division(2)
        nz = this%rice_seed_division(3)    
        
        
        

        call femdomain%create(meshtype="Cube3D",x_num=x_num,y_num=y_num,z_num=z_num)
        call femdomain%resize(x=Width,y=width,z=Length)


        call seed%create(meshtype="Cube3D",x_num=nx,y_num=ny,z_num=nz)

        call seed%resize(x=seed_width,y=seed_thickness,z=seed_length)
        center = seed%centerPosition()
        call seed%move(x=-center(1),y=-center(2),z=-seed%zmin())
        do i=1,seed%nn()
            x = seed%mesh%nodcoord(i,1)
            y = seed%mesh%nodcoord(i,2)
            z = seed%mesh%nodcoord(i,3)
            alpha = z/seed%zmax()
            theta = 1.00d0 - abs(alpha-0.50d0)
            seed%mesh%nodcoord(i,1) = x*theta
        enddo


        seed_joint = zeros(0)
        num_seed = int(Length/seed_interval)


        nodcoord = zeros( num_seed * 4, 3 )
        elemnod  = int(zeros( num_seed , FEMDomain%nne() ))
        allocate(seeds(num_seed) )
        do i=1,num_seed
            do 
                elem_idx = random%randint(From=1,To=femdomain%ne())
            
                n1 = FEMDomain%mesh%elemnod(elem_idx,1)
                n2 = FEMDomain%mesh%elemnod(elem_idx,2)
                n3 = FEMDomain%mesh%elemnod(elem_idx,3)
                n4 = FEMDomain%mesh%elemnod(elem_idx,4)
                x1 = FEMDomain%mesh%nodcoord(n2,:) - FEMDomain%mesh%nodcoord(n1,:)
                x2 = FEMDomain%mesh%nodcoord(n4,:) - FEMDomain%mesh%nodcoord(n1,:)
            

            
                case_id = 0
                if(minval( FEMDomain%mesh%nodcoord([n1,n2,n3,n4],1) )== FEMDomain%xmin() )then
                    if(minval( FEMDomain%mesh%nodcoord([n1,n2,n3,n4],2) )== FEMDomain%ymin() )then
                        case_id = 1 
                    elseif(maxval( FEMDomain%mesh%nodcoord([n1,n2,n3,n4],2) )== FEMDomain%ymax() )then
                        case_id = 2
                    endif
                
                elseif(maxval( FEMDomain%mesh%nodcoord([n1,n2,n3,n4],1) )== FEMDomain%xmax() )then
                    if(minval( FEMDomain%mesh%nodcoord([n1,n2,n3,n4],2) )== FEMDomain%ymin() )then
                        case_id = 4
                    elseif(maxval( FEMDomain%mesh%nodcoord([n1,n2,n3,n4],2) )== FEMDomain%ymax() )then
                        case_id = 3 
                    endif
                endif
                if(case_id /=0)exit
            enddo
            seed_joint = seed_joint // [elem_idx]
            select case(case_id)
                case(1)
                    !x-min and y_min
                    node_list = [4, 1, 5, 8]
                case(2)
                    !x-min and y_max
                    node_list = [3, 4, 8, 7]
                case(4)
                    !x-max and y_min 
                    node_list = [1, 2, 6, 5]
                case(3)
                    !x-max and y_max
                    node_list = [2, 3, 7, 6]
            end select
        
            n1 = FEMDomain%mesh%elemnod(elem_idx,node_list(1) )
            n2 = FEMDomain%mesh%elemnod(elem_idx,node_list(2) )
            n3 = FEMDomain%mesh%elemnod(elem_idx,node_list(3) )
            n4 = FEMDomain%mesh%elemnod(elem_idx,node_list(4) )
        
            x1 = FEMDomain%mesh%nodcoord(n2,:) - FEMDomain%mesh%nodcoord(n1,:)
            x2 = FEMDomain%mesh%nodcoord(n4,:) - FEMDomain%mesh%nodcoord(n1,:)
            normal_vector = cross_product(x1,x2)
            normal_vector = normal_vector/norm(normal_vector)
            nodcoord( 4*i -3, 1:3 ) = seed_branch_length*normal_vector &
                + FEMDomain%mesh%nodcoord(FEMDomain%mesh%elemnod(elem_idx,node_list(1)),:)
            nodcoord( 4*i -2, 1:3 ) = seed_branch_length*normal_vector &
                + FEMDomain%mesh%nodcoord(FEMDomain%mesh%elemnod(elem_idx,node_list(2)),:)
            nodcoord( 4*i -1, 1:3 ) = seed_branch_length*normal_vector &
                + FEMDomain%mesh%nodcoord(FEMDomain%mesh%elemnod(elem_idx,node_list(3)),:)
            nodcoord( 4*i -0, 1:3 ) = seed_branch_length*normal_vector &
                + FEMDomain%mesh%nodcoord(FEMDomain%mesh%elemnod(elem_idx,node_list(4)),:)
            elemnod(i,1:4) = FEMDomain%mesh%elemnod(elem_idx,node_list(1:4))
            elemnod(i,5:8) =[ 4*i-3, 4*i-2, 4*i-1, 4*i-0 ] + femdomain%nn()
        
            ! 果梗と子実をがっちゃんこ
            ! seedの5要素目，1234番
            seeds(i)  = seed
            dx = sum(nodcoord( 4*i -3:4*i,1 ))/4.0d0
            dy = sum(nodcoord( 4*i -3:4*i,2 ))/4.0d0
            dz = sum(nodcoord( 4*i -3:4*i,3 ))/4.0d0 + seed_branch_length
            call seeds(i)%move(x=dx,y=dy,z=dz)
            call seeds(i)%rotate(z=math%pi/2.0d0*dble(case_id) )

            select case(case_id)
            case(1)
                !x-min and y_min

                nodcoord( 4*i -3, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),3),:)
                nodcoord( 4*i -2, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),4),:)
                nodcoord( 4*i -1, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),1),:)
                nodcoord( 4*i -0, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),2),:)
            case(2)
                !x-min and y_max
                ![ok]
                nodcoord( 4*i -3, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),1),:)
                nodcoord( 4*i -2, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),2),:)
                nodcoord( 4*i -1, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),3),:)
                nodcoord( 4*i -0, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),4),:)
            case(4)
                !x-max and y_min 
                ![ok]
                nodcoord( 4*i -3, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),1),:)
                nodcoord( 4*i -2, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),2),:)
                nodcoord( 4*i -1, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),3),:)
                nodcoord( 4*i -0, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),4),:)
            case(3)
                !x-max and y_max
                !
                nodcoord( 4*i -3, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),3),:)
                nodcoord( 4*i -2, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),4),:)
                nodcoord( 4*i -1, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),1),:)
                nodcoord( 4*i -0, 1:3 ) = seeds(i)%mesh%nodcoord(seeds(i)%mesh%elemnod( int(dble(nx*ny)/2.0d0),2),:)
            end select

        enddo


        FEMDomain%mesh%nodcoord = FEMDomain%mesh%nodcoord // nodcoord
        FEMDomain%mesh%elemnod  = FEMDomain%mesh%elemnod // elemnod

        do i=1,size(seeds)
            seeds(i)%mesh%elemnod = seeds(i)%mesh%elemnod + femdomain%nn()
            femdomain%mesh%nodcoord = femdomain%mesh%nodcoord // seeds(i)%mesh%nodcoord
            femdomain%mesh%elemnod = femdomain%mesh%elemnod // seeds(i)%mesh%elemnod
        enddo



        ! remove duplicate nodes
        call femdomain%Deduplicate(error=dble(1.0e-8))
        

        ! bend
        z_max = femdomain%zmax()
        call femdomain%move(x=panicle_curvature)
        do i=1,femdomain%nn()
            x = femdomain%mesh%nodcoord(i,1)
            z = femdomain%mesh%nodcoord(i,3)
            r = x
            theta = z/panicle_curvature
            femdomain%mesh%nodcoord(i,1) = r*cos(theta)
            femdomain%mesh%nodcoord(i,3) = r*sin(theta)
        
        enddo
        
        call femdomain%move(x=-panicle_curvature)
        theta = random%gauss(mu=0.0d0,sigma=math%pi/4.0d0)
        call femdomain%rotate(z=theta )
        dx = femdomain%mesh%nodcoord(1,1)
        dy = femdomain%mesh%nodcoord(1,2)
        dz = femdomain%mesh%nodcoord(1,3)
        call femdomain%move(x=-dx,y=-dy,z=-dz ) 
        this%femdomain = femdomain

        this%A_PointNodeID = this%FEMDomain%getNearestNodeID(x=0.0d0,y=0.0d0,z=0.0d0)
        this%B_PointNodeID = this%FEMDomain%getNearestNodeID(x=0.0d0,y=0.0d0,z=Length)
        
        ! branch exists
        !if(present(rice_panicle_branch_num) )then
        !    allocate(branch_panicle(rice_panicle_branch_num) )
        !    ! create branches
        !    do i=1,rice_panicle_branch_num
        !        
        !        call branch_panicle(i)%init(&
        !            Length=Length,Width=Width,x_num=x_num,y_num=y_num,z_num=z_num,&
        !            rice=.true.,&
        !            rice_seed_interval= rice_seed_interval ,&
        !            rice_seed_branch_length= rice_seed_branch_length ,&
        !            rice_seed_length= rice_seed_length ,&
        !            rice_seed_width= rice_seed_width ,&
        !            rice_seed_thickness= rice_seed_thickness ,&
        !            rice_panicle_curvature= rice_panicle_curvature ,&
        !            rice_seed_division= rice_seed_division  )
        !        
        !    enddo
        !endif

        
        return
    else
        !<<<< MAIZE MODE >>>>>
        shape_factor_val = input(default=0.33d0,option=shape_factor)
        
        


        Angle = 0.0d0 ! vertical panicle
        this%Length = length
        this%Width = Width
        this%Angle = Angle
        this%division(1) = input(default=this%division(1),option=x_num )
        this%division(2) = input(default=this%division(2),option=y_num )
        this%division(3) = input(default=this%division(3),option=z_num )

        x_axis = [-Length*shape_factor_val,-width/2.0d0,0.0d0,width/2.0d0,Length*shape_factor_val]
        call refine(x_axis,this%division(1) )

        !y_axis=[-width/2.0d0,0.0d0,width/2.0d0]
        !call refine(y_axis,this%division(2))
        ! debug
        y_axis = [-Length*shape_factor_val,-width/2.0d0,0.0d0,width/2.0d0,Length*shape_factor_val]
        call refine(y_axis,this%division(2))

        z_axis = [0.0d0]
        do i=1,Node
            z_axis = z_axis // [ this%Length*shape_factor_val/dble(Node)*dble(i)]
            z_axis = z_axis // [ z_axis(size(z_axis) )+this%width ]
        enddo

        z_axis = z_axis // [this%Length]
        z_axis0 = z_axis
        call refine(z_axis,this%division(3))

        call this%FEMDomain%create("Cube3D",&
            x_axis=x_axis,y_axis=y_axis,z_axis=z_axis)
        kill_element_list = zeros(this%FEMDomain%ne() ) 
        !do i=1,this%FEMDomain%ne()
        !    center_coord = this%FEMDomain%centerPosition(ElementID=i)
        !    do j=1,size(z_axis0)-1,2
        !        if( z_axis0(j)< center_coord(3) .and. center_coord(3) < z_axis0(j+1) )then
        !            if( abs(center_coord(1)) > width/2.0d0  )then
        !                kill_element_list(i)  = 1
        !            endif
        !        endif
        !    enddo
        !enddo
        do i=1,this%FEMDomain%ne()
            center_coord = this%FEMDomain%centerPosition(ElementID=i)
            if( abs(center_coord(1)) > width/2.0d0  .and. abs(center_coord(2)) > width/2.0d0 )then
                kill_element_list(i)  = 1
            endif
            do j=1,size(z_axis0)-1,2

                if( z_axis0(j)< center_coord(3) .and. center_coord(3) < z_axis0(j+1) )then
                    if( abs(center_coord(1)) > width/2.0d0  .and. abs(center_coord(2)) < width/2.0d0 )then
                        kill_element_list(i)  = 1
                    endif

                    if( abs(center_coord(1)) < width/2.0d0  .and. abs(center_coord(2)) > width/2.0d0 )then
                        kill_element_list(i)  = 1
                    endif

                endif
            enddo
        enddo

        call this%FEMDomain%killElement(blacklist=kill_element_list,flag=1)

        ! edit
        do i=1,this%FEMDomain%nn()
            center_coord = this%FEMDomain%mesh%nodcoord(i,:)
            if(abs(center_coord(1))>width/2.0d0  )then
                alpha = center_coord(3)/Length
                if(alpha==1.0d0) cycle
                theta = radian(alpha*90.0d0) ! angle:: alpha=0 => 0, alpha=1 => radian(90.0)

                ! new x
                this%FEMDomain%mesh%nodcoord(i,1) = this%FEMDomain%mesh%nodcoord(i,1)*cos(theta)
                ! new z
                this%FEMDomain%mesh%nodcoord(i,3) = this%FEMDomain%mesh%nodcoord(i,3) + abs(center_coord(1))*tan(theta)  ! x * tan(theta)

            endif

            if(abs(center_coord(2))>width/2.0d0  )then
                alpha = center_coord(3)/Length
                if(alpha==1.0d0) cycle
                theta = radian(alpha*90.0d0) ! angle:: alpha=0 => 0, alpha=1 => radian(90.0)

                ! new x
                this%FEMDomain%mesh%nodcoord(i,2) = this%FEMDomain%mesh%nodcoord(i,2)*cos(theta)
                ! new z
                this%FEMDomain%mesh%nodcoord(i,3) = this%FEMDomain%mesh%nodcoord(i,3) + abs(center_coord(2))*tan(theta)  ! x * tan(theta)

            endif
        enddo


        call this%FEMDomain%rotate(z=math%PI*2.0d0*random%random() )

    !    ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
    !    this%I_planeNodeID = this%FEMdomain%mesh%getNodeList(zmax=0.0d0)
    !    this%I_planeElementID = this%FEMdomain%mesh%getElementList(zmax=0.0d0)
    !    
    !    ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
    !    this%II_planeNodeID = this%FEMdomain%mesh%getNodeList(zmin=this%length)
    !    this%II_planeElementID = this%FEMdomain%mesh%getElementList(zmin=this%length)
    !    
    !    
    !
    !    center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),1) )&
    !        /size(this%I_planeNodeID)
    !    center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),2) )&
    !        /size(this%I_planeNodeID)
    !    center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),3) )&
    !        /size(this%I_planeNodeID)
    !
    !    dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(1),:)-center_coord)
    !    this%A_PointNodeID = this%I_planeNodeID(1)
    !    
    !    do i=2, size(this%I_planeNodeID)
    !        if(  norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i),:)-center_coord) < dist_val  )then
    !            this%A_PointNodeID = this%I_planeNodeID(i)
    !            dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i),:)-center_coord)
    !        endif
    !    enddo
    !    
    !    
    !    center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),1) )&
    !        /size(this%I_planeNodeID)
    !    center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),2) )&
    !        /size(this%I_planeNodeID)
    !    center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),3) )&
    !        /size(this%I_planeNodeID)
    !
    !    dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(1),:)-center_coord)
    !    this%B_PointNodeID = this%II_planeNodeID(1)
    !    
    !    do i=2, size(this%II_planeNodeID)
    !        if(  norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i),:)-center_coord) < dist_val  )then
    !            this%B_PointNodeID = this%II_planeNodeID(i)
    !            dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i),:)-center_coord)
    !        endif
    !    enddo
    !    
    !    
    !
    !    center_coord(1) = maxval(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),1) )
    !
    !    center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),2) )&
    !        /size(this%I_planeNodeID)
    !
    !    center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),3) )&
    !        /size(this%I_planeNodeID)
    !
    !    dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(1),:)-center_coord)
    !    this%C_PointNodeID = this%I_planeNodeID(1)
    !    
    !    do i=2, size(this%I_planeNodeID)
    !        if(  norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i),:)-center_coord) < dist_val  )then
    !            this%C_PointNodeID = this%I_planeNodeID(i)
    !            dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i),:)-center_coord)
    !        endif
    !    enddo
    !    
    !    
    !
    !    center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),1) )&
    !        /size(this%II_planeNodeID)
    !
    !    center_coord(2) = maxval(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),2) )
    !
    !
    !    center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),3) )&
    !        /size(this%II_planeNodeID)
    !
    !    dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(1),:)-center_coord)
    !    this%D_PointNodeID = this%II_planeNodeID(1)
    !    
    !    do i=2, size(this%II_planeNodeID)
    !        if(  norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i),:)-center_coord) < dist_val  )then
    !            this%D_PointNodeID = this%II_planeNodeID(i)
    !            dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i),:)-center_coord)
    !        endif
    !    enddo
    !
    !    buf    = this%FEMDomain%mesh%getElementList(&
    !        xmin=this%width/2.0d0 - this%width/dble(this%division(1) )/2.0d0 ,&
    !        xmax=this%width/2.0d0 + this%width/dble(this%division(1) )/2.0d0 ,&
    !        ymin=this%width/2.0d0 - this%width/dble(this%division(2) )/2.0d0 ,&
    !        ymax=this%width/2.0d0 + this%width/dble(this%division(2) )/2.0d0 ,&
    !        zmax=0.0d0)
    !    !this%A_PointElementID = buf(1)
    !    this%A_PointElementID = median(buf)
    !    
    !    
    !    buf    = this%FEMDomain%mesh%getElementList(&
    !        xmin=this%width/2.0d0 - this%width/dble(this%division(1) )/2.0d0 ,&
    !        xmax=this%width/2.0d0 + this%width/dble(this%division(1) )/2.0d0 ,&
    !        ymin=this%width/2.0d0 - this%width/dble(this%division(2) )/2.0d0 ,&
    !        ymax=this%width/2.0d0 + this%width/dble(this%division(2) )/2.0d0 ,&
    !        zmin=this%length)
    !
    !    !this%B_PointElementID = buf(1)
    !    this%B_PointElementID = median(buf)
    !
    !    if(debug) print *, this%A_PointNodeID
    !    if(debug) print *, this%B_PointNodeID
    !    if(debug) print *, this%A_PointElementID
    !    if(debug) print *, this%B_PointElementID
    !
        this%A_PointNodeID = this%FEMDomain%getNearestNodeID(x=0.0d0,y=0.0d0,z=0.0d0)
        this%B_PointNodeID = this%FEMDomain%getNearestNodeID(x=0.0d0,y=0.0d0,z=Length)
    endif
end subroutine
! #####################################################

! #####################################################
subroutine vtkPanicle(this,name)
    class(Panicle_),intent(inout) :: this
    character(*),intent(in) :: name

    call this%FEMDomain%vtk(name=name)
    
end subroutine
! #####################################################


! ########################################
recursive subroutine movePanicle(this,x,y,z,reset)
    class(Panicle_),intent(inout) :: this
    real(real64),optional,intent(in) :: x,y,z
    logical,optional,intent(in) :: reset
    real(real64),allocatable :: origin1(:),origin2(:),disp(:)

    if(present(reset) )then
        if(reset .eqv. .true.)then
            call this%femdomain%move(-this%disp_x,-this%disp_y,-this%disp_z)
            this%disp_x = 0.0d0
            this%disp_y = 0.0d0
            this%disp_z = 0.0d0
        endif
    endif

    call this%femdomain%move(x,y,z)
    this%disp_x = this%disp_x + input(default=0.0d0, option=x)
    this%disp_y = this%disp_y + input(default=0.0d0, option=y)
    this%disp_z = this%disp_z + input(default=0.0d0, option=z)
    
end subroutine
! ########################################



! ########################################
function getCoordinatePanicle(this,nodetype) result(ret)
    class(Panicle_),intent(in) :: this
    character(*),intent(in) :: nodetype
    real(real64),allocatable :: ret(:)
    integer(int32) :: dimnum,n,i
    integer(int32),allocatable :: buf(:)


    dimnum = size(this%femdomain%mesh%nodcoord,2)
    
    allocate(ret(dimnum) )
    ret(:) = 0.0d0
    if( nodetype=="A" .or. nodetype=="a")then
        
        
        ! 20220701 this may be correct
        ret = this%femdomain%mesh%nodcoord( this%A_PointNodeID,: ) 
        return



        n = size(this%I_planeNodeID )
        if(n==0)then
            print *, "ERROR >> getCoordinatePanicle >> size(this%I_planeNodeID) = 0"
        endif
        if(.not.allocated(this%I_planeNodeID))then
            
            print *, "ERROR >> getCoordinatePanicle >> .not. allocated(this%I_planeNodeID) "
            
        endif
        do i=1,n
            ret(:) = ret(:) + this%femdomain%mesh%nodcoord( this%I_planeNodeID(i),: ) 
        enddo
        ret(:) = 1.0d0/dble(n) * ret(:)

        !ret = this%femdomain%mesh%nodcoord(this%A_PointNodeID,:)
    endif

    if( nodetype=="B" .or. nodetype=="b")then
        !ret = this%femdomain%mesh%nodcoord(this%B_PointNodeID,:)


        ! 20220701 this may be correct
        ret = this%femdomain%mesh%nodcoord( this%B_PointNodeID,: ) 
        return

        n = size(this%II_planeNodeID )
        if(n==0)then
            print *, "ERROR >> getCoordinatePanicle >> size(this%II_planeNodeID) = 0"
        endif
        if(.not.allocated(this%I_planeNodeID))then
            
            print *, "ERROR >> getCoordinatePanicle >> .not. allocated(this%II_planeNodeID) "
            
        endif
        do i=1,n
            ret(:) = ret(:) + this%femdomain%mesh%nodcoord( this%II_planeNodeID(i),: ) 
        enddo
        ret(:) = 1.0d0/dble(n) * ret(:)
    endif

end function
! ########################################

! ########################################

subroutine connectPanicle(obj,direct,stem)
    class(Panicle_),intent(inout) :: obj
    class(Stem_),intent(inout) :: stem

    character(2),intent(in) :: direct
    real(real64),allocatable :: x1(:),x2(:),disp(:)

    if(direct=="->" .or. direct=="=>")then
        ! move obj to connect stem (stem is not moved.)
        x1 =  obj%getCoordinate("A")
        x2 = stem%getCoordinate("B")
        disp = x2 - x1
        call obj%move(x=disp(1),y=disp(2),z=disp(3) )
    endif


    if(direct=="<-" .or. direct=="<=")then
        ! move obj to connect stem (stem is not moved.)
        x1 = stem%getCoordinate("A")
        x2 =  obj%getCoordinate("B")
        disp = x2 - x1
        call stem%move(x=disp(1),y=disp(2),z=disp(3) )
    endif
end subroutine


! ########################################

! ########################################
recursive subroutine rotatePanicle(this,x,y,z)
    class(Panicle_),intent(inout) :: this
    real(real64),optional,intent(in) :: x,y,z

    call this%FEMDomain%rotate(x=x,y=y,z=z)
end subroutine
! ########################################

! ##############################################

subroutine stlPanicle(obj,name)
    class(Panicle_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%stl(Name=name)
end subroutine
! ########################################

subroutine removePanicle(this)
    class(Panicle_),intent(inout) :: this

    call this%FEMDomain%remove()
    this%Length = 0.0d0
    this%Width = 0.0d0
    this%Angle = 0.0d0
    if(associated(this% pStem)) nullify(this%pStem)
    this% division(1:3) = [5,5,5]


    if(allocated(this% I_planeNodeID) ) deallocate(this% I_planeNodeID)! (:)
    if(allocated(this% I_planeElementID) ) deallocate(this% I_planeElementID)! (:)
    if(allocated(this% II_planeNodeID) ) deallocate(this% II_planeNodeID)! (:)
    if(allocated(this% II_planeElementID) ) deallocate(this% II_planeElementID)! (:)
    this % A_PointNodeID = 0
    this % B_PointNodeID = 0
    this % C_PointNodeID = 0
    this % D_PointNodeID = 0
    
    this % A_PointElementID = 0
    this % B_PointElementID = 0

    this%default_rice_seed_interval  = 3.0d0/1000.0d0 ! 3 mm 
    this%default_rice_seed_branch_length = 3.0d0/1000.0d0 ! 2 mm 
    this%default_rice_seed_length    = 6.0d0/1000.0d0 ! 2 mm 
    this%default_rice_seed_width     = 4.0d0/1000.0d0 ! 2 mm 
    this%default_rice_seed_thickness = 2.0d0/1000.0d0 ! 2 mm 
    this%default_rice_panicle_curvature =  0.20d0

    this%disp_x = 0.0d0
    this%disp_y = 0.0d0
    this%disp_z = 0.0d0

    ! For deformation analysis
    if(allocated(this% YoungModulus) ) deallocate(this% YoungModulus)! (:)! element-wise
    if(allocated(this% PoissonRatio) ) deallocate(this% PoissonRatio)! (:)! element-wise
    if(allocated(this% Density) ) deallocate(this% Density)! (:)     ! element-wise
    if(allocated(this% Stress) ) deallocate(this% Stress)! (:,:,:)     ! Gauss point-wise
    if(allocated(this% Displacement) ) deallocate(this% Displacement)! (:,:) ! node-wise, three dimensional


end subroutine

end module