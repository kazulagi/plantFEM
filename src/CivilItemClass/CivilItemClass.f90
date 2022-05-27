
module CivilItemClass
    use FEMDomainClass
    implicit none
    
    type :: CivilItem_
    
    contains
        !procedure, public :: BridgePier => BridgePierCivilItem
        procedure, pass :: BridgePierCivilItem
        procedure, pass :: BridgePierCivilItem_JSON
        
        generic :: BridgePier => BridgePierCivilItem, BridgePierCivilItem_JSON

        procedure, public :: BridgeGirder => BridgeGirderCivilItem
        procedure, public :: BridgeShoe => BridgeShoeCivilItem
        procedure, public :: BridgeShoes => BridgeShoesCivilItem

        procedure, pass :: RigidFrameViaductCivilItem
        procedure, pass :: RigidFrameViaductCivilItem_JSON
        generic :: RigidFrameViaduct => RigidFrameViaductCivilItem,RigidFrameViaductCivilItem_JSON
    end type

contains


! #######################################################################
function BridgePierCivilItem_JSON(this,config,debug) result(femdomain)
    class(CivilItem_),intent(in) :: this
    character(*),intent(in) :: config
    logical,optional,intent(in) :: debug
    real(real64) :: Bottom(1:2),Top(1:2),height
    real(real64) :: Transition(1:2)
    integer(int32) :: divisions(1:3)
    type(FEMDomain_) :: femdomain
    integer(int32) :: i, nf, nt
    type(IO_) :: f
    character(:),allocatable :: line

    Bottom(1:2) = 0.0d0
    Top(1:2)= 0.0d0
    Transition(1:2)= 0.0d0
    divisions(1:3)=0
    height= 0.0d0

    call f%open(config,"r")
    do
        if(f%EOF) exit

        line = f%readline()
        
        if(index(line,"Bottom")/=0 )then
            nf = index(line, "[")
            nt = index(line, "]")
            read(line(nf+1:nt-1),* ) Bottom(1:2)
            cycle
        endif


        if(index(line,"Top")/=0 )then
            nf = index(line, "[")
            nt = index(line, "]")
            read(line(nf+1:nt-1),* ) Top(1:2)
            cycle
        endif

        if(index(line,"Transition")/=0 )then
            nf = index(line, "[")
            nt = index(line, "]")
            read(line(nf+1:nt-1),* ) Transition(1:2)
            cycle
        endif

        if(index(line,"Divisions")/=0 )then
            nf = index(line, "[")
            nt = index(line, "]")
            read(line(nf+1:nt-1),* ) Divisions(1:3)
            cycle
        endif

        if(index(line,"Height")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) Height
            cycle
        endif
        
    enddo
    call f%close()

    if(present(debug) )then
        if(debug)then
            print *, "Bottom :: ",Bottom
            print *, "Top :: ",Top
            print *, "Transition :: ",Transition
            print *, "Divisions :: ",Divisions
            print *, "Height :: ",Height
            
        endif
    endif
    femdomain = this%BridgePier(Bottom=Bottom,Top=Top,Transition=Transition,&
        divisions=divisions,height=height)

end function
! #######################################################################

function BridgePierCivilItem(this,Bottom,Top,Transition,divisions,height) result(femdomain)
    class(CivilItem_),intent(in) :: this
    real(real64),intent(in) :: Bottom(1:2),Top(1:2),height
    real(real64),optional,intent(in) :: Transition(1:2)
    integer(int32),intent(in) :: divisions(1:3)
    type(FEMDomain_) :: femdomain
    integer(int32) :: i
    real(real64) :: z, x,theta
    
    
    call femdomain%create("Cube3D",x_num=divisions(1),y_num=divisions(2),z_num=divisions(3) )
    call femdomain%resize(x=Bottom(1),y=Bottom(2),z=height )
    call femdomain%move(x=-Bottom(1)/2.0d0,y=-Bottom(2)/2.0d0 )
    
    if(present(Transition) )then
        do i=1,femdomain%nn()
            if(femdomain%position_z(i) >= Transition(2)  ) then
                femdomain%mesh%nodcoord(i,1) = femdomain%mesh%nodcoord(i,1)*Top(1)/Bottom(1)
                femdomain%mesh%nodcoord(i,2) = femdomain%mesh%nodcoord(i,2)*Top(2)/Bottom(2)
            elseif(femdomain%position_z(i) <= Transition(2) .and. femdomain%position_z(i) >= Transition(1) )then
                z = femdomain%position_z(i)
                theta = (z - Transition(1))/(Transition(2) - Transition(1) )
                femdomain%mesh%nodcoord(i,1) = femdomain%mesh%nodcoord(i,1)*(Bottom(1) + theta*(Top(1) - Bottom(1) )   )/Bottom(1)
                femdomain%mesh%nodcoord(i,2) = femdomain%mesh%nodcoord(i,2)*(Bottom(2) + theta*(Top(2) - Bottom(2) ) )/Bottom(2)
            endif
        enddo
    endif


end function

function BridgeGirderCivilItem(this,From,To,Thickness,Width,Divisions,fitPiers) result(femdomain)
    class(CivilItem_),intent(in) :: this
    type(FEMDomain_),intent(inout) :: From, To
    real(real64),intent(in) :: Thickness,Width
    real(real64),allocatable :: origins_direct(:)
    integer(int32),intent(in) :: divisions(1:3)
    logical,optional,intent(in) :: fitPiers(2)

    type(FEMDomain_) :: femdomain
    integer(int32) :: i
    real(real64) :: girder_length,x1(1:3),x2(1:3),x0(1:3),theta_z,theta_x
    

    
    x1(1) = sum(from%mesh%nodcoord(:,1) )/dble(from%nn() )
    x1(2) = sum(from%mesh%nodcoord(:,2) )/dble(from%nn() )
    x1(3) = from%z_max()
    
    x2(1) = sum(to%mesh%nodcoord(:,1) )/dble(to%nn() )
    x2(2) = sum(to%mesh%nodcoord(:,2) )/dble(to%nn() )
    x2(3) = to%z_max()

    girder_length = norm(x2(1:2)-x1(1:2) )
    theta_z = dot_product(x2(1:2)-x1(1:2),[0.0d0,1.0d0])/norm(x2(1:2)-x1(1:2) )/1.0d0

    theta_x = dot_product(x2(2:3)-x1(2:3),[1.0d0,0.0d0])/norm(x2(2:3)-x1(2:3) )/1.0d0

    call femdomain%create("Cube3D",x_num=divisions(1),y_num=divisions(2),z_num=divisions(3) )
    call femdomain%resize(x=Width,y=girder_length,z=thickness )
    call femdomain%move(x=-Width/2.0d0,y=-girder_length/2.0d0)
    call femdomain%move(&
        x = (x1(1)+x2(1))*0.50d0,&
        y = (x1(2)+x2(2))*0.50d0,& 
        z = (from%z_max() + to%z_max())*0.50d0 )
    !call femdomain%rotate(x=degrees(theta_x),z=degrees(theta_z))
    call femdomain%rotate(x=acos(theta_x),z=-acos(theta_z))

    if(present(fitPiers) )then
        if(fitPiers(1) )then
            
            origins_direct = from%position()
            call From%move(x=-origins_direct(1),y=-origins_direct(2),z=-origins_direct(3) )
            call From%rotate(z=-acos(theta_z))
            call From%move(x=origins_direct(1),y=origins_direct(2),z=origins_direct(3) )
        endif
        
        if(fitPiers(2) )then
            origins_direct = to%position()
            call to%move(x=-origins_direct(1),y=-origins_direct(2),z=-origins_direct(3) )
            call to%rotate(z=-acos(theta_z))
            call to%move(x=origins_direct(1),y=origins_direct(2),z=origins_direct(3) )
            
        endif
    endif
    
end function
! ###########################################################

function BridgeShoeCivilItem(this,pier,Thickness,Width,Divisions) result(femdomain)
    class(CivilItem_),intent(in) :: this
    type(FEMDomain_),intent(in) :: pier
    real(real64),intent(in) :: Thickness,Width
    integer(int32),intent(in) :: divisions(1:3)
    
    type(FEMDomain_) :: femdomain
    
    integer(int32) :: i
    
    call femdomain%create("Cube3D",&
        x_num=Divisions(1),&
        y_num=Divisions(2),&
        z_num=Divisions(3) )
    call femdomain%resize(x=width,y=width,z=thickness)
    call femdomain%move(&
        x=-femdomain%x_max()*0.50d0,&
        y=-femdomain%y_max()*0.50d0 )

    call femdomain%move(&
        x=pier%position_x(),&
        y=pier%position_y(),&
        z=pier%z_max()      )

end function



function BridgeShoesCivilItem(this,pier,num_shoes,Thickness,Width,Divisions) result(femdomains)
    class(CivilItem_),intent(in) :: this
    type(FEMDomain_),intent(in) :: pier
    real(real64),intent(in) :: Thickness,Width
    integer(int32),intent(in) :: divisions(1:3),num_shoes(1:2)
    
    type(FEMDomain_),allocatable :: femdomains(:)
    real(real64)   :: Ll,Lw
    integer(int32) :: i,j
    
    allocate(femdomains(num_shoes(1)*num_shoes(2) ) )

    Ll = Pier%x_max() - Pier%x_min()
    Lw = Pier%y_max() - Pier%y_min() 

    do i=1,num_shoes(1)
        do j=1,num_shoes(2)
            femdomains( num_shoes(2)*(i-1) + j ) &
                = this%BridgeShoe(pier,Thickness,Width,Divisions)
            
            call femdomains( num_shoes(2)*(i-1) + j )%move(&
                x = Pier%x_min() - Pier%position_x()  ,&
                y = Pier%y_min() - Pier%position_y()  )
            
            call femdomains( num_shoes(2)*(i-1) + j )%move(&
                x = 0.50d0*Ll/num_shoes(1) + dble(i-1)*Ll/num_shoes(1)  ,&
                y = 0.50d0*Lw/num_shoes(2) + dble(j-1)*Lw/num_shoes(2)  )
        enddo
    enddo

end function
! #############################################################################
function RigidFrameViaductCivilItem_JSON(this,config,debug) result(RFV)
    class(CivilItem_),intent(inout) :: this
    character(*),intent(in) :: config
    integer(int32) :: NumPiers(1:2) ! n by m, total n*m piers
    integer(int32) :: divisions(1:3)
    real(real64) :: length
    real(real64) :: width
    real(real64) :: height
    real(real64) :: PierThickness
    real(real64),allocatable :: MiddlePierHeights(:)
    logical,optional,intent(in) :: debug

    type(FEMDomain_) :: RFV
    integer(int32) :: i, n , m ,NumMiddlePier,nt,nf
    character(:),allocatable :: line

    type(IO_) :: f


    call f%open(config,"r")
    do
        if(f%EOF) exit

        line = f%readline()
        
        if(index(line,"NumPiers")/=0 )then
            nf = index(line, "[")
            nt = index(line, "]")
            read(line(nf+1:nt-1),* ) NumPiers(1:2)
            cycle
        endif


        if(index(line,"Width")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) Width
            cycle
        endif

        if(index(line,"Length")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) Length
            cycle
        endif


        if(index(line,"Height")/=0 .and. index(line,"Middle")==0)then
            nf = index(line, ":")
            read(line(nf+1:),* ) Height
            cycle
        endif

        if(index(line,"Divisions")/=0 )then
            nf = index(line, "[")
            nt = index(line, "]")
            read(line(nf+1:nt-1),* ) Divisions(1:3)
            cycle
        endif

        if(index(line,"PierThickness")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) PierThickness
            cycle
        endif
        
        if(index(line,"NumMiddlePier")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) NumMiddlePier
            cycle
        endif

        if(index(line,"MiddlePierHeights")/=0 .and. index(line,"Middle")/=0)then
            nf = index(line, "[")
            nt = index(line, "]")
            MiddlePierHeights = zeros(NumMiddlePier)
            read(line(nf+1:nt-1),* ) MiddlePierHeights(1:NumMiddlePier)
            cycle
        endif
    enddo
    call f%close()


    if(present(debug) )then
        if(debug)then
            print *, "NumPiers ::   ",NumPiers
            print *, "Width ::      ",Width
            print *, "Length ::     ",Length
            print *, "Height ::     ",Height
            print *, "PierThickness ::  ",PierThickness
            print *, "Divisions ::      ",Divisions
            print *, "NumMiddlePier ::  ",NumMiddlePier
            print *, "MiddlePierHeights ::  ",MiddlePierHeights
        endif
    endif

    if(allocated(MiddlePierHeights) )then
        RFV = this%RigidFrameViaduct(NumPiers=NumPiers,&
            length=length,&
            width=width,&
            PierThickness=PierThickness,&
            divisions=divisions,&
            height=height,&
            MiddlePierHeights=MiddlePierHeights,&
            debug=debug)
    else
        RFV = this%RigidFrameViaduct(NumPiers=NumPiers,&
            length=length,&
            width=width,&
            PierThickness=PierThickness,&
            divisions=divisions,&
            height=height,&
            debug=debug)
    endif

end function
! #############################################################################



! #############################################################################
function RigidFrameViaductCivilItem(this,NumPiers,length,width,PierThickness,divisions,height,MiddlePierHeights,&
        GirderThickness,& ! >> From here, args are optional!!
        GirderWidth, GirderEdgeHeight, GirderEdgeThickness, JointHeight, JointThickness, JointLength,debug) result(RFV)
    class(CivilItem_),intent(inout) :: this
    integer(int32),intent(in) :: NumPiers(1:2) ! n by m, total n*m piers
    integer(int32),optional,intent(in) :: divisions(1:3)
    real(real64),intent(in) :: length
    real(real64),intent(in) :: width
    real(real64),intent(in) :: height
    real(real64),intent(in) :: PierThickness

    ! Lv. 1:: with Middle Piers and Girders
    real(real64),optional,intent(in) :: MiddlePierHeights(:)
    real(real64),optional,intent(in) :: GirderThickness

    ! Lv. 2:: Detail
    real(real64),optional,intent(in) ::GirderWidth
    real(real64),optional,intent(in) ::GirderEdgeHeight
    real(real64),optional,intent(in) ::GirderEdgeThickness
    real(real64),optional,intent(in) ::JointHeight
    real(real64),optional,intent(in) ::JointThickness
    real(real64),optional,intent(in) ::JointLength
    
    ! debug option
    logical,optional,intent(in) :: debug

    ! internal variables
    real(real64) :: dx,dy,dz,thickness,interval
    real(real64),allocatable :: point(:)
    type(FEMDomain_) :: RFV
    type(FEMDomain_) :: remove_zone
    integer(int32) :: i, n , m 
    real(real64),allocatable :: remove_zone_x(:,:)
    real(real64),allocatable :: remove_zone_y(:,:)
    real(real64),allocatable :: remove_zone_z(:,:)

    real(real64),allocatable :: x_axis(:)
    real(real64),allocatable :: y_axis(:)
    real(real64),allocatable :: z_axis(:)
    
    real(real64),allocatable :: x_axis_origin(:)
    real(real64),allocatable :: y_axis_origin(:)
    real(real64),allocatable :: z_axis_origin(:)


    real(real64) :: z_init_max
    integer(int32) :: girderedge_offset
    integer(int32) :: ElementID, remove_count,j,k,last_n,girder_offset,joint_offset
    real(real64) :: present_width,extra_half_width,op_GirderEdgeHeight,w_center
    integer(int32),allocatable :: remove_elem(:),buf(:,:),remove_node(:),new_node_id(:),killElemList(:)
    real(real64),allocatable :: realbuf(:,:),center_coord(:),shift_x(:),bufvec(:)
    logical :: debug_mode_requested = .false.

    op_GirderEdgeHeight = input(default=0.0d0,option=GirderEdgeHeight)
    last_n = 0
    if(present(GirderThickness) )then
        last_n = -1
    endif

    if(present(debug) )then
        debug_mode_requested = debug
    endif

    thickness = PierThickness

    if(maxval(NumPiers) <= 1 )then
        print *, "ERROR :: RigidFrameViaductCivilItem :: for single pier,"
        print *, "please use %BridgePier()"
        return
    endif

    ! cut axis
    ! x-direction (Length)
    x_axis = zeros( NumPiers(2)*2 ) 
    interval = (Length - PierThickness*NumPiers(2))/dble(NumPiers(2) -1  )
    x_axis(1 ) = 0.0d0 
    x_axis(2 ) = PierThickness

    do i=2,NumPiers(2)
        x_axis(2*i -1 ) = x_axis(2*(i-1) -1 ) + interval + PierThickness
        x_axis(2*i    ) = x_axis(2*(i-1)    ) + interval + PierThickness
    enddo

    ! x-direction (width)
    y_axis = zeros( NumPiers(1)*2 ) 
    interval = (Width - PierThickness*NumPiers(1))/dble(NumPiers(1) -1  )
    y_axis(1 ) = 0.0d0 
    y_axis(2 ) = PierThickness
    
    do i=2,NumPiers(1)
        y_axis(2*i -1 ) = y_axis(2*(i-1) -1 ) + interval + PierThickness
        y_axis(2*i    ) = y_axis(2*(i-1)    ) + interval + PierThickness
    enddo
    if(present(GirderWidth) )then
        if(.not. present(GirderThickness))then
            print *, "ERROR :: CivilItem%rigidframeciaduct >> "
            print *, "GirderThickness shoud be passed when GirderWidth is present."
            stop
        endif
        ! both GirderWidth and GirderThickness are present.
        
        present_width    = maxval(y_axis) - minval(y_axis)
        extra_half_width = GirderWidth/2.0d0 - present_width*0.50d0 
        y_axis = [-extra_half_width+minval(y_axis)] // y_axis
        y_axis = y_axis // [extra_half_width + maxval(y_axis) ]
    endif

    
    ! Lv. 1
    if(present(MiddlePierHeights) )then
        z_axis = zeros(size(MiddlePierHeights)*2 + 3 )
        z_axis(1) = 0.0d0
        j=1
        do i=1,size(MiddlePierHeights)
            j=j+1
            z_axis(j) = MiddlePierHeights(i) - PierThickness/2.0d0
            j=j+1
            z_axis(j) = MiddlePierHeights(i) + PierThickness/2.0d0
        enddo
        
        z_axis(size(z_axis)-1:size(z_axis)  ) =[Height-PierThickness,Height]
        
        if(present(GirderThickness) )then
            z_axis = z_axis // [ maxval(z_axis)+GirderThickness ]
        endif
        
        
        z_init_max = maxval(z_axis)

        if(present(GirderEdgeHeight) .or. present(GirderEdgeThickness) )then
            if(present(GirderEdgeHeight) .and. present(GirderEdgeThickness) )then
                y_axis = [- GirderEdgeThickness + minval(y_axis) ] // y_axis
                y_axis = y_axis // [maxval(y_axis)]
                y_axis(size(y_axis)-1 ) = maxval(y_axis) - GirderEdgeThickness 
                
                z_axis = z_axis // [ maxval(z_axis) + GirderEdgeHeight]
                !z_axis = z_axis // [ maxval(z_axis) + GirderEdgeHeight]

    
            else
                print *, "ERROR :: CivilItem%rigidframeciaduct >> "
                print *, "GirderEdgeHeight shoud be passed with GirderEdgeThickness"
                stop
            endif
        endif
        
        ! for joint
        if(present(JointLength) .or. present(JointThickness) )then
            if(present(JointLength) .and. present(JointThickness) )then
                x_axis = [minval(x_axis) -JointLength] // x_axis
                x_axis =  x_axis // [maxval(x_axis) + JointLength ]
            else
                print *, "ERROR :: CivilItem%rigidframeciaduct >> "
                print *, "JointLength,JointHeight,JointThickness shoud be passed at the same time!"
                stop
            endif
        endif
        

        ! joint
        joint_offset = 0
        if(present(JointLength) )then
            do n=1,size(x_axis)
                if(x_axis(n) < 0.0d0)then
                    joint_offset = joint_offset + 1
                else
                    exit
                endif
            enddo

            n = 0
            do i=1,size(z_axis)
                if(z_axis(i) > JointHeight )then
                    n = i
                    exit
                endif
            enddo
            if(n==0)then
                print *, "ERROR :: RFV :: JointHeight >= Height!"
                stop
            endif

            bufvec = z_axis
            z_axis = bufvec(1:n-1) // [JointHeight] 
            z_axis = z_axis // bufvec(n:)
            deallocate(bufvec)
            
            n = 0
            do i=1,size(z_axis)
                if(z_axis(i) > JointHeight + JointThickness )then
                    n = i
                    exit
                endif
            enddo
            if(n==0)then
                print *, "ERROR :: RFV :: JointHeight + JointThickness<= Height!"
                stop
            endif

            bufvec = z_axis
            z_axis = bufvec(1:n-1) // [JointHeight + JointThickness] 
            z_axis = z_axis // bufvec(n:)
            deallocate(bufvec)
            
           
        endif




        x_axis_origin = x_axis
        y_axis_origin = y_axis
        z_axis_origin = z_axis
        if(present(Divisions)  )then
            do i=1,Divisions(1)
                call Refine(x_axis_origin,1)
            enddo
            do i=1,Divisions(2)
                call Refine(y_axis_origin,1)
            enddo
            do i=1,Divisions(3)
                call Refine(z_axis_origin,1)
            enddo
        endif
        girder_offset = 0
        if(present(GirderWidth) )then
            do n=1,size(y_axis)
                if(y_axis(n) < 0.0d0)then
                    girder_offset = girder_offset + 1
                else
                    exit
                endif
            enddo
        endif

        
        girderedge_offset = 0
        if(present(GirderEdgeThickness) )then
            do n=size(z_axis),1,-1
                if(z_axis(n) > z_init_max)then
                    girderedge_offset = girderedge_offset + 0
                else
                    exit
                endif
            enddo
        endif

        call RFV%create("Cube3D",&
            x_axis = x_axis_origin ,&
            y_axis = y_axis_origin ,&
            z_axis = z_axis_origin  &
        )
        ! x-direction (Length)
        
        killElemList = int(zeros(RFV%ne()))
        do k = 1, size(MiddlePierHeights)+3
            do i=1,NumPiers(2)-1
                do j=1,RFV%ne()
                    center_coord = RFV%centerPosition(ElementID=j)
                    if(x_axis(2*i + joint_offset) < center_coord(1) .and. center_coord(1) < x_axis(2*i+1 + joint_offset))then
                        if(z_axis(2*k-1) < center_coord(3) .and. center_coord(3) < z_axis(2*k) )then
                            if(present(GirderThickness) .and. center_coord(3) >height - PierThickness )then
                                cycle
                            endif

                    if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                        cycle
                    endif
                            killElemList(j) =  1
                        endif
                    endif
                enddo
            enddo
            
            do i=1,NumPiers(1)-1
                do j=1,RFV%ne()
                    center_coord = RFV%centerPosition(ElementID=j)
                    if(y_axis(2*i + girder_offset) < center_coord(2) .and. &
                        center_coord(2) < y_axis(2*i+1 + girder_offset))then
                        if(z_axis(2*k-1) < center_coord(3) .and. center_coord(3) < z_axis(2*k) )then
                            if(present(GirderThickness) .and. center_coord(3) >height- PierThickness )then
                                cycle
                            endif

                            if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                                cycle
                            endif
                            killElemList(j) =  1
                        endif
                    endif
                enddo
            enddo
        enddo

        ! for last pier
        if(present(JointHeight) )then
            
            do i=1,NumPiers(2)-1
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if(x_axis(2*i + joint_offset) < center_coord(1) .and. center_coord(1) < x_axis(2*i+1 + joint_offset))then
                    if(MiddlePierHeights(size(MiddlePierHeights))+PierThickness/2.0d0 < center_coord(3) &
                        .and. center_coord(3) < height - PierThickness )then
                        if(present(GirderThickness) .and. center_coord(3) >height - PierThickness )then
                            cycle
                        endif

                if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                    cycle
                endif
                        killElemList(j) =  1
                    endif
                endif
            enddo
            enddo

            do i=1,NumPiers(1)-1
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if(y_axis(2*i + girder_offset) < center_coord(2) .and. &
                    center_coord(2) < y_axis(2*i+1 + girder_offset))then
                    if(MiddlePierHeights(size(MiddlePierHeights))+PierThickness/2.0d0 < center_coord(3) &
                        .and. center_coord(3) < height - PierThickness )then
                        if(present(GirderThickness) .and. center_coord(3) >height- PierThickness )then
                            cycle
                        endif

                        if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                            cycle
                        endif
                        killElemList(j) =  1
                    endif
                endif
            enddo
            enddo
        endif




        if(allocated(killElemList) )then
            call RFV%killElement(blacklist=killElemList,flag=1)
        endif
        ! cut top
        ! x-direction (Length)
        killElemList = int(zeros(RFV%ne()))
        do i=1,NumPiers(2)-1
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if(x_axis(2*i + joint_offset) < center_coord(1) .and. center_coord(1) < x_axis(2*i+1 + joint_offset))then
                    !if(center_coord(3) < z_axis( size(z_axis)-1) )then
                    if(present(GirderThickness) .and. center_coord(3) > height )then
                        cycle
                    endif
                    if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                        cycle
                    endif
                    killElemList(j) = killElemList(j) + 1
                    !endif
                endif
            enddo
        enddo
        
        do i=1,NumPiers(1)-1
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if(y_axis(2*i+girder_offset) < center_coord(2) .and. &
                    center_coord(2) < y_axis(2*i+1+girder_offset))then
                    !if(center_coord(3) < z_axis( size(z_axis)-1) )then
                    if(present(GirderThickness) .and. center_coord(3) > height )then
                        cycle
                    endif
                    if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                        cycle
                    endif
                    killElemList(j) = killElemList(j) + 1
                    !endif
                endif
            enddo
        enddo
        if(allocated(killElemList) )then
            call RFV%killElement(blacklist=killElemList,flag=2)
        endif

        if(present(GirderWidth) )then
            ! below-girder
            ! x-direction (Length)
            killElemList = int(zeros(RFV%ne()))
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if( center_coord(2) < 0.0d0 .or. &
                    center_coord(2) > RFV%y_max()-extra_half_width  )then
                    if( center_coord(3) < RFV%z_max() - GirderThickness - op_GirderEdgeHeight )then
                        if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                            cycle
                        endif
                        killElemList(j) = 1
                    endif
                endif
            enddo

            if(allocated(killElemList) )then
                call RFV%killElement(blacklist=killElemList,flag=1)
            endif


        endif

    else
        z_axis = [0.0d0,Height-PierThickness,Height]

        if(present(GirderThickness) )then
            z_axis = z_axis // [ maxval(z_axis)+GirderThickness ]
        endif
        z_init_max = maxval(z_axis)
    
        if(present(GirderEdgeHeight) .or. present(GirderEdgeThickness) )then
            if(present(GirderEdgeHeight) .and. present(GirderEdgeThickness) )then
                y_axis = [- GirderEdgeThickness + minval(y_axis) ] // y_axis
                y_axis = y_axis // [maxval(y_axis)]
                y_axis(size(y_axis)-1 ) = maxval(y_axis) - GirderEdgeThickness 
                
                z_axis = z_axis // [ maxval(z_axis) + GirderEdgeHeight]
                !z_axis = z_axis // [ maxval(z_axis) + GirderEdgeHeight]

    
            else
                print *, "ERROR :: CivilItem%rigidframeciaduct >> "
                print *, "GirderEdgeHeight shoud be passed with GirderEdgeThickness"
                stop
            endif
        endif
        
        ! for joint
        if(present(JointLength) .or. present(JointThickness) )then
            if(present(JointLength) .and. present(JointThickness) )then
                x_axis = [minval(x_axis) -JointLength] // x_axis
                x_axis =  x_axis // [maxval(x_axis) + JointLength ]
            else
                print *, "ERROR :: CivilItem%rigidframeciaduct >> "
                print *, "JointLength,JointHeight,JointThickness shoud be passed at the same time!"
                stop
            endif
        endif


        ! joint
        joint_offset = 0
        if(present(JointLength) )then
            do n=1,size(x_axis)
                if(x_axis(n) < 0.0d0)then
                    joint_offset = joint_offset + 1
                else
                    exit
                endif
            enddo

            n = 0
            do i=1,size(z_axis)
                if(z_axis(i) > JointHeight )then
                    n = i
                    exit
                endif
            enddo
            if(n==0)then
                print *, "ERROR :: RFV :: JointHeight >= Height!"
                stop
            endif

            bufvec = z_axis
            z_axis = bufvec(1:n-1) // [JointHeight] 
            z_axis = z_axis // bufvec(n:)
            deallocate(bufvec)
            
            n = 0
            do i=1,size(z_axis)
                if(z_axis(i) > JointHeight + JointThickness )then
                    n = i
                    exit
                endif
            enddo
            if(n==0)then
                print *, "ERROR :: RFV :: JointHeight + JointThickness<= Height!"
                stop
            endif

            bufvec = z_axis
            z_axis = bufvec(1:n-1) // [JointHeight + JointThickness] 
            z_axis = z_axis // bufvec(n:)
            deallocate(bufvec)
            
           
        endif



        x_axis_origin = x_axis
        y_axis_origin = y_axis
        z_axis_origin = z_axis
        if(present(Divisions)  )then
            do i=1,Divisions(1)
                call Refine(x_axis_origin,1)
            enddo
            do i=1,Divisions(2)
                call Refine(y_axis_origin,1)
            enddo
            do i=1,Divisions(3)
                call Refine(z_axis_origin,1)
            enddo
        endif

        girder_offset = 0
        if(present(GirderWidth) )then
            do n=1,size(y_axis)
                if(y_axis(n) < 0.0d0)then
                    girder_offset = girder_offset + 1
                else
                    exit
                endif
            enddo
        endif
        


        girderedge_offset = 0
        if(present(GirderEdgeThickness) )then
            do n=size(z_axis),1,-1
                if(z_axis(n) > z_init_max)then
                    girderedge_offset = girderedge_offset + 0
                else
                    exit
                endif
            enddo
        endif

        call RFV%create("Cube3D",&
            x_axis = x_axis_origin ,&
            y_axis = y_axis_origin ,&
            z_axis = z_axis_origin  &
        )

        ! x-direction (Length)
        killElemList = int(zeros(RFV%ne()))
        do i=1,NumPiers(2)-1
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if(x_axis(2*i + joint_offset) < center_coord(1) .and. center_coord(1) < x_axis(2*i+1 + joint_offset) )then
                    if(center_coord(3) < z_axis( size(z_axis)-1+ last_n - girderedge_offset) )then
                        if(present(GirderThickness) .and. center_coord(3) >height -PierThickness)then
                            cycle
                        endif
                        if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                            cycle
                        endif
                        killElemList(j) =  1
                    endif
                endif
            enddo
        enddo
        
        do i=1,NumPiers(1)-1
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if(y_axis(2*i+girder_offset) < center_coord(2) .and. &
                    center_coord(2) < y_axis(2*i+1+girder_offset))then
                    if(center_coord(3) < z_axis( size(z_axis)-1 + last_n - girderedge_offset) )then
                        if(present(GirderThickness) .and. center_coord(3) >height -PierThickness )then
                            cycle
                        endif
                        if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                            cycle
                        endif
                        killElemList(j) =  1
                    endif
                endif
            enddo
        enddo
        if(allocated(killElemList) )then
            call RFV%killElement(blacklist=killElemList,flag=1)
        endif

        ! cut top
        ! x-direction (Length)
        killElemList = int(zeros(RFV%ne()))
        do i=1,NumPiers(2)-1
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if(x_axis(2*i+ joint_offset) < center_coord(1) .and. center_coord(1) < x_axis(2*i+1+ joint_offset))then
                    !if(center_coord(3) < z_axis( size(z_axis)-1) )then
                    if(present(GirderThickness) .and. center_coord(3) >height )then
                        cycle
                    endif
                    if(present(GirderEdgeThickness)  )then
                        if(center_coord(2) > RFV%ymax() - GirderEdgeThickness .or. &
                            center_coord(2) < RFV%ymin() + GirderEdgeThickness)then
                            cycle
                        endif
                    endif
                    if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                        cycle
                    endif
                    
                    killElemList(j) = killElemList(j) + 1
                    !endif
                endif
            enddo
        enddo
        
        do i=1,NumPiers(1)-1
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if(y_axis(2*i+girder_offset) < center_coord(2) .and. &
                    center_coord(2) < y_axis(2*i+1+girder_offset))then
                    !if(center_coord(3) < z_axis( size(z_axis)-1) )then
                    if(present(GirderThickness) .and. center_coord(3) > height )then
                        cycle
                    endif
                    if(present(GirderEdgeThickness)  )then
                        if( center_coord(2) > RFV%ymax() - GirderEdgeThickness .or. &
                            center_coord(2) < RFV%ymin() + GirderEdgeThickness)then
                            cycle
                        endif
                    endif
                    if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                        cycle
                    endif
                    killElemList(j) = killElemList(j) + 1
                    !endif
                endif
            enddo
        enddo
        if(allocated(killElemList) )then
            call RFV%killElement(blacklist=killElemList,flag=2)
        endif
        


    endif


    if(present(GirderWidth) )then
        if(.not.present(GirderEdgeHeight) )then
            ! below-girder
            ! x-direction (Length)
            killElemList = int(zeros(RFV%ne()))
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if( center_coord(2) < 0.0d0 .or. &
                    center_coord(2) > RFV%y_max()-extra_half_width  )then
                    if( center_coord(3) < RFV%z_max() - GirderThickness )then

                        if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                            cycle
                        endif
                        killElemList(j) = 1
                    endif
                endif
            enddo

            if(allocated(killElemList) )then
                call RFV%killElement(blacklist=killElemList,flag=1)
            endif
        else
            ! below-girder
            killElemList = int(zeros(RFV%ne()))
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if( center_coord(2) < 0.0d0 .or. &
                    center_coord(2) > RFV%y_max()-extra_half_width  )then
                    if( center_coord(3) < RFV%z_max() - GirderThickness - GirderEdgeHeight )then

                        if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                            cycle
                        endif
                        killElemList(j) = 1
                    endif
                endif
            enddo

            if(allocated(killElemList) )then
                call RFV%killElement(blacklist=killElemList,flag=1)
            endif
            ! upper

            killElemList = int(zeros(RFV%ne()))
            do j=1,RFV%ne()
                center_coord = RFV%centerPosition(ElementID=j)
                if( RFV%y_min() + GirderEdgeThickness < center_coord(2)  .and. &
                    center_coord(2) < RFV%y_max()-GirderEdgeThickness  )then
                    if( center_coord(3) > RFV%z_max() - GirderEdgeHeight )then

                        if(center_coord(1)<0.0d0 .or. center_coord(1) > length )then
                            cycle
                        endif
                        killElemList(j) = 1
                    endif
                endif
            enddo

            if(allocated(killElemList) )then
                call RFV%killElement(blacklist=killElemList,flag=1)
            endif
        endif
    endif

    if(present(JointHeight) )then
        killElemList = int(zeros(RFV%ne()))
        w_center = (RFV%ymax() + RFV%ymin())*0.50d0
        do j=1,RFV%ne()
            center_coord = RFV%centerPosition(ElementID=j)
            if(center_coord(1) > 0.0d0 .and. center_coord(1) < Length )then
                cycle
            endif

            if(center_coord(3) < JointHeight .or. center_coord(3) > JointHeight + JointThickness  )then
                killElemList(j) = 1
                cycle
            endif

            if(center_coord(2) > w_center + Width/2.0d0 .or. center_coord(2) < w_center - Width/2.0d0  )then
                killElemList(j) = 1
                cycle
            endif

        enddo

        if(allocated(killElemList) )then
            call RFV%killElement(blacklist=killElemList,flag=1)
        endif


    endif

    call RFV%move(x = -(RFV%xmax()-RFV%xmin())*0.50d0  )
    

end function

end module