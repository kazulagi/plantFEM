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
        integer(int32) :: division(1:3) = [5,5,5]


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
    end type

contains

! #####################################################
subroutine initPanicle(this,Length,Width,Node,shape_factor,debug,x_num,y_num,z_num)
    class(Panicle_),intent(inout) :: this
    real(real64),intent(in) :: Length, width
    integer(int32),intent(in) :: Node
    integer(int32),optional,intent(in) :: x_num,y_num,z_num
    real(real64),optional,intent(in) :: shape_factor
    
    real(real64):: Angle
    type(Math_) :: math
    type(Random_) :: random
    real(real64) :: x,y,z,r,theta,alpha,dist_val
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:),z_axis0(:)
    integer(int32) :: i,j
    integer(int32),allocatable :: buf(:), kill_element_list(:)
    logical,optional,intent(in) :: debug

    real(real64) :: center_coord(1:3)
    real(real64) :: shape_factor_val 

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

end module