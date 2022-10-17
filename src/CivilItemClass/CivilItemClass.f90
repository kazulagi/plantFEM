
module CivilItemClass
    use FEMDomainClass
    use FEMSolverClass
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

        procedure, pass :: EarthDam_with_ground_CivilItem
        procedure, pass :: EarthDam_with_ground_CivilItem_JSON
        generic :: EarthDam => EarthDam_with_ground_CivilItem,EarthDam_with_ground_CivilItem_JSON

        procedure :: PaddyField => PaddyFieldCivilItem
        procedure :: OpenChannel => OpenChannelCivilItem


        procedure :: ground => groundCivilItem
        procedure :: beam => beamCivilItem

        procedure,pass :: BoxCulvertCivilItem
        procedure,pass :: BoxCulvertCivilItem_JSON
        generic :: BoxCulvert => BoxCulvertCivilItem,BoxCulvertCivilItem_JSON

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
    real(real64) :: PierThickness,GirderExists_01
    real(real64) :: GirderWidth = 0.0d0! option
    real(real64) :: GirderThickness = 0.0d0! option
    real(real64) :: GirderEdgeHeight = 0.0d0! option
    real(real64) :: GirderEdgeThickness = 0.0d0! option
    real(real64),allocatable :: MiddlePierHeights(:)
    logical,optional,intent(in) :: debug
    logical :: GirderExists = .false.

    type(FEMDomain_) :: RFV
    integer(int32) :: i, n , m ,NumMiddlePier,nt,nf
    character(:),allocatable :: line

    type(IO_) :: f, json_file


    call f%open(config,"r")
    do
        if(f%EOF) exit

        line = f%readline()
        
        if(index(line,"NumPiers")/=0 .and. index(line,"NumPiers_")==0)then
            nf = index(line, "[")
            nt = index(line, "]")
            read(line(nf+1:nt-1),* ) NumPiers(1:2)
            cycle
        endif

        if(index(line,"NumPiers_x")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) NumPiers(1) 
            cycle
        endif


        if(index(line,"NumPiers_y")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) NumPiers(2) 
            cycle
        endif

        if(index(line,"Divisions")/=0 .and. index(line,"Divisions_")==0)then
            nf = index(line, "[")
            nt = index(line, "]")
            read(line(nf+1:nt-1),* ) Divisions(1:3)
            cycle
        endif


        if(index(line,"Divisions_x")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) Divisions(1)
            cycle
        endif


        if(index(line,"Divisions_y")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) Divisions(2)
            cycle
        endif


        if(index(line,"Divisions_z")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) Divisions(3)
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


        if(index(line,"GirderWidth")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) GirderWidth
            cycle
        endif

        if(index(line,"GirderThickness")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) GirderThickness
            cycle
        endif

        if(index(line,"GirderEdgeHeight")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) GirderEdgeHeight
            cycle
        endif

        if(index(line,"GirderEdgeThickness")/=0 )then
            nf = index(line, ":")
            read(line(nf+1:),* ) GirderEdgeThickness
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



    enddo
    call f%close()

    MiddlePierHeights = int(zeros(NumMiddlePier))
    MiddlePierHeights(1:NumMiddlePier) = to_vector(&
        json_file%parse(filename=config,key1="MiddlePierHeights"),NumMiddlePier)


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
            print *, "GirderWidth :: " , GirderWidth
            print *, "GirderThickness :: " ,GirderThickness
            print *, "GirderEdgeHeight :: " ,GirderEdgeHeight
            print *, "GirderEdgeThickness :: " ,GirderEdgeThickness
        endif
    endif

    GirderExists_01 = GirderWidth*GirderThickness*GirderEdgeHeight*GirderEdgeThickness
    print *, GirderExists_01
    if(GirderExists_01==0.0d0)then
        GirderExists = .false.
    else
        GirderExists = .true.
    endif

    if(allocated(MiddlePierHeights) )then
        if(GirderExists)then
            RFV = this%RigidFrameViaduct(NumPiers=NumPiers,&
                length=length,&
                width=width,&
                PierThickness=PierThickness,&
                divisions=divisions,&
                height=height,&
                MiddlePierHeights=MiddlePierHeights,&
                GirderWidth=GirderWidth,&
                GirderThickness=GirderThickness,&
                GirderEdgeHeight=GirderEdgeHeight,&
                GirderEdgeThickness=GirderEdgeThickness,&
                debug=debug)
        else
            RFV = this%RigidFrameViaduct(NumPiers=NumPiers,&
                length=length,&
                width=width,&
                PierThickness=PierThickness,&
                divisions=divisions,&
                height=height,&
                MiddlePierHeights=MiddlePierHeights,&
                debug=debug)
        endif
    else
        if(GirderExists)then
            RFV = this%RigidFrameViaduct(NumPiers=NumPiers,&
                length=length,&
                width=width,&
                PierThickness=PierThickness,&
                divisions=divisions,&
                height=height,&
                GirderWidth=GirderWidth,&
                GirderThickness=GirderThickness,&
                GirderEdgeHeight=GirderEdgeHeight,&
                GirderEdgeThickness=GirderEdgeThickness,&
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
        !real(Real64) :: z_from,z_to

        killElemList = int(zeros(RFV%ne()))
        !do k = 1, size(MiddlePierHeights)+3
        do k = 1, size(z_axis)/2
            
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

! ############################################
function EarthDam_with_ground_CivilItem_JSON(this,config) result(femdomain)
    class(CivilItem_),intent(in) :: this
    character(*),intent(in) :: config
    type(FEMDomain_) :: femdomain

    real(real64) :: height = 2.200d0
    real(real64) :: width = 11.00d0
    real(real64) :: length = 117.00d0
    real(real64) :: depth = 30.00d0
    real(real64) :: margin = 50.00d0
    real(real64) :: angles(1:2)  =[20.00d0, 20.00d0]
    real(real64) :: top_width = 4.0d0

    integer(int32) :: refine_level(1:3) = [5,5,3]
    integer(int32) :: depth_cut = 10
    integer(int32) :: margin_cut= 10

    type(IO_) :: json_file
    ! check json
    if(index(config,".json")==0 )then
        ! not json file
        return
    endif

    angles = to_vector(json_file%parse(filename=config,key1="angles"),2)
    
    height = freal(json_file%parse(filename=config,key1="height"))
    width  = freal(json_file%parse(filename=config,key1="width"))
    length = freal(json_file%parse(filename=config,key1="length"))
    depth  = freal(json_file%parse(filename=config,key1="depth"))
    margin = freal(json_file%parse(filename=config,key1="margin"))
    top_width  = freal(json_file%parse(filename=config,key1="top"))

    depth_cut  = fint(json_file%parse(filename=config,key1="division_v"))
    margin_cut  = fint(json_file%parse(filename=config,key1="division_h"))

    refine_level = to_list(json_file%parse(filename=config,key1="refine_level"),3)


    femdomain = this%EarthDam(&
        height=height,&!=2.20d0, &
        width=width,&! =11.0d0, &
        length=length,&!=117.0d0,&
        depth=depth,&! =30.0d0,&
        margin=margin,&!=50.0d0,&
        angles=angles,&!=[20.0d0, 20.0d0],& ! 20.0 degrees & 15.0d0 degrees
        top_width=top_width,&!=4.0d0, & ! top width
        refine_level=refine_level,&! = [5,5,3] ,& ! refinement level
        depth_cut=depth_cut,&!    = 10, &
        margin_cut=margin_cut &!   = 10 &
    )

end function
! ############################################


! ############################################
function EarthDam_with_ground_CivilItem(this, height, length, width, depth, margin,angles,top_width,refine_level,&
    depth_cut,margin_cut,R) result(dam)
    class(CivilItem_),intent(in) :: this
    real(real64),intent(in) :: height, length, width, depth, margin  ,top_width
    real(real64),intent(in) :: angles(1:2)
    integer(int32),intent(in) :: refine_level(1:3),depth_cut,margin_cut
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:)
    real(real64),optional,intent(in) :: R
    real(real64) :: center_coord(1:3),h,w,a,x,y,z,xmax,ym,xm,dx,xmm
    integer(int32),allocatable :: killElemList(:)
    integer(int32) :: i,j
    type(FEMDomain_) :: dam


    x_axis = [&
        -top_width/2.0d0-height/tan(radian(angles(2))),&
        -top_width/2.0d0,top_width/2.0d0,top_width/2.0d0+height/tan(radian(angles(1))) &
        ]

    call Refine(x_axis,refine_level(1) )

    x_axis = [-margin-top_width/2.0d0-height/tan(radian(angles(2)))]//x_axis// &
        [margin+top_width/2.0d0+height/tan(radian(angles(1))) ]
    call Refine(x_axis,margin_cut )
    
    y_axis = [-length/2.0d0,length/2.0d0]

    
    call Refine(y_axis,refine_level(2) )


    z_axis = [-height, 0.0d0, height]
    call Refine(z_axis,refine_level(3) )
    z_axis = [-depth] // z_axis
    call Refine(z_axis,depth_cut )
    
    call dam%create("Cube3D",&
        x_axis = x_axis ,&
        y_axis = y_axis ,&
        z_axis = z_axis  &
    )

    ! remove
    killElemList = int(zeros(dam%ne()))
    do j=1,dam%ne()
        center_coord = dam%centerPosition(ElementID=j)
        if( 0.0d0 < center_coord(3)  .and. &
            center_coord(1) <  -top_width/2.0d0-height/tan(radian(angles(2))) )then    
            killElemList(j) = 1
        endif
    enddo

    do j=1,dam%ne()
        center_coord = dam%centerPosition(ElementID=j)
        if( 0.0d0 < center_coord(3)  .and. &
            center_coord(1) >  top_width/2.0d0+height/tan(radian(angles(1))) )then    
            killElemList(j) = 1
        endif
    enddo

    if(allocated(killElemList) )then
        call dam%killElement(blacklist=killElemList,flag=1)
    endif


    ! reshape
    h = height
    w = top_width/2.0d0
    
    do i=1,dam%nn()
        center_coord = dam%position(i)
        if(center_coord(3)>0.0d0 )then
            if(center_coord(1)>0.0d0 )then
                a = height/tan(radian(angles(1))) + (top_width/2.0d0)
                x = center_coord(1)
                z = center_coord(3)
                xmax = a + (w-a)/h*z
                dam%mesh%nodcoord(i,1) = x*xmax/a
            else
                a = height/tan(radian(angles(2))) + (top_width/2.0d0)
                x = center_coord(1)
                z = center_coord(3)
                xmax = a + (w-a)/h*z
                dam%mesh%nodcoord(i,1) = x*xmax/a
            endif
        endif
    enddo

    if(present(R) )then
        if(R>0.0d0)then
            xm = dam%xmax()
            xmm = dam%xmin()
            
            do i=1,dam%nn()
                
                center_coord = dam%position(i)
                x = center_coord(1)

                if(x>0.0d0)then
                    y = center_coord(2)
                    dx = (R - sqrt(R*R - y*y) )
                    dam%mesh%nodcoord(i,1) = dx + x - dx/xm*x
                else
                    y = center_coord(2)
                    dx = (R - sqrt(R*R - y*y) )
                    dam%mesh%nodcoord(i,1) = dx + x - dx/xmm*x
                endif
                
            enddo

        else

        endif
    endif

end function

! #################################################
function PaddyFieldCivilItem(this,Length,Width,Depth,RidgeWidth,RidgeHeight,refine_level) result(PaddyField)
    class(CivilItem_),intent(inout) :: this
    real(real64),intent(in) :: Length,Width,Depth,RidgeWidth,RidgeHeight
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:),center_coord(:)
    integer(int32),intent(in) :: refine_level(1:3)
    integer(int32),allocatable :: killElemList(:)
    integer(int32) :: i,j
    type(FEMDomain_) :: paddyfield

    x_axis = [-Length/2.0d0,-Length/2.0d0+RidgeWidth/2.0d0,0.0d0,Length/2.0d0-RidgeWidth/2.0d0,Length/2.0d0]
    y_axis = [-Width/2.0d0,-Width/2.0d0+RidgeWidth/2.0d0,0.0d0,Width/2.0d0-RidgeWidth/2.0d0,Width/2.0d0]
    z_axis = [-depth,0.0d0,RidgeHeight]
    
    call refine(x_axis,refine_level(1) )
    call refine(y_axis,refine_level(2) )
    call refine(z_axis,refine_level(3) )

    call paddyfield%create("Cube3D",&
        x_axis = x_axis ,&
        y_axis = y_axis ,&
        z_axis = z_axis  &
    )

    ! remove
    killElemList = int(zeros(PaddyField%ne()))
    do j=1,PaddyField%ne()
        center_coord = PaddyField%centerPosition(ElementID=j)
        if( 0.0d0 < center_coord(3)  )then  
            if( -Length/2.0d0+RidgeWidth/2.0d0 < center_coord(1) .and. &
                center_coord(1) < Length/2.0d0-RidgeWidth/2.0d0 )then
                if( -Width/2.0d0+RidgeWidth/2.0d0 < center_coord(2) .and. &
                    center_coord(2) < Width/2.0d0-RidgeWidth/2.0d0 )then
                    
                    killElemList(j) = 1
                endif
            endif
        endif
    enddo

    if(allocated(killElemList) )then
        call PaddyField%killElement(blacklist=killElemList,flag=1)
    endif



    ! reshape

!    h = height
!    w = top_width/2.0d0
!    do i=1,PaddyField%nn()
!        center_coord = PaddyField%position(i)
!        if(center_coord(3)>0.0d0 )then
!            if(center_coord(1) > 0.0d0 )then
!                a = height/tan(radian(RidgeAngle)) + (top_width/2.0d0)
!                x = center_coord(1)
!                z = center_coord(3)
!                xmax = a + (w-a)/h*z
!                PaddyField%mesh%nodcoord(i,1) = x*xmax/a
!            else
!                a = height/tan(radian(RidgeAngle)) + (top_width/2.0d0)
!                x = center_coord(1)
!                z = center_coord(3)
!                xmax = a + (w-a)/h*z
!                PaddyField%mesh%nodcoord(i,1) = x*xmax/a
!            endif
!        endif
!    enddo
end function

! ########################################################
function OpenChannelCivilItem(this,Length,Width,Depth,ChannelWidth,ChannelDepth,SlopeAngle, &
        SlopeDepth,refine_level) result(channel)
    class(CivilItem_),intent(inout) :: this
    real(real64),intent(in) :: Length,Width,Depth,ChannelWidth,ChannelDepth
    real(real64),optional,intent(in) :: SlopeAngle,SlopeDepth
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:),center_coord(:),point_coord(:)
    integer(int32),intent(in) :: refine_level(1:3)
    integer(int32),allocatable :: killElemList(:)
    integer(int32) :: i,j
    type(FEMDomain_) :: channel
    real(real64) :: xm,x,z,dx,w,cw

    if(present(SlopeAngle) .or. present(SlopeDepth)  )then
        if(present(SlopeAngle) .and. present(SlopeDepth)  )then
            x_axis = [-Length/2.0d0,0.0d0,Length/2.0d0]
            y_axis = [-Width/2.0d0,-ChannelWidth/2.0d0,0.0d0,ChannelWidth/2.0d0,Width/2.0d0]
            z_axis = [-abs(depth),-abs(ChannelDepth),-abs(SlopeDepth),0.0d0]
            
            call refine(x_axis,refine_level(1) )
            call refine(y_axis,refine_level(2) )
            call refine(z_axis,refine_level(3) )
            
            call channel%create("Cube3D",&
                x_axis = x_axis ,&
                y_axis = y_axis ,&
                z_axis = z_axis  &
            )
            
            ! remove
            killElemList = int(zeros(channel%ne()))
            do j=1,channel%ne()
                center_coord = channel%centerPosition(ElementID=j)
                if( -abs(ChannelDepth) -abs(SlopeDepth) < center_coord(3)  )then  
                    if( -ChannelWidth/2.0d0 < center_coord(2) .and. &
                        center_coord(2) < ChannelWidth/2.0d0 )then
                        
                        killElemList(j) = 1
                        
                    endif
                endif
            enddo
        
            if(allocated(killElemList) )then
                call channel%killElement(blacklist=killElemList,flag=1)
            endif
            
            ! reshape slope
            w = abs(Width/2.0d0)
            cw = abs(ChannelWidth/2.0d0)
            
            do j=1,channel%nn()
                point_coord = channel%Position(j)
                if(  -abs(SlopeDepth) < point_coord(3)  )then  
                    x = point_coord(2)
                    z = abs(abs(slopeDepth) - abs(point_coord(3)) )
                    dx = z/tan(radian(SlopeAngle))

                    channel%mesh%nodcoord(j,2)=x/abs(x)*&
                    ( (dx+cw )*(w- abs(x) ) + w*(abs(x) - cw ) )/(abs(w)-abs(cw) )
                    
                endif
            enddo
            
        else
            print *, "[ERROR] OpenChannelCivilItem >> slope shape needs both of SlopeAngle and SlopeDepth"
        endif
    else
        x_axis = [-Length/2.0d0,0.0d0,Length/2.0d0]
        y_axis = [-Width/2.0d0,-ChannelWidth/2.0d0,0.0d0,ChannelWidth/2.0d0,Width/2.0d0]
        z_axis = [-abs(depth),-abs(ChannelDepth),0.0d0]
        
        call refine(x_axis,refine_level(1) )
        call refine(y_axis,refine_level(2) )
        call refine(z_axis,refine_level(3) )

        call channel%create("Cube3D",&
            x_axis = x_axis ,&
            y_axis = y_axis ,&
            z_axis = z_axis  &
        )

        ! remove
        killElemList = int(zeros(channel%ne()))
        do j=1,channel%ne()
            center_coord = channel%centerPosition(ElementID=j)
            if( -abs(ChannelDepth) < center_coord(3)  )then  
                if( -ChannelWidth/2.0d0 < center_coord(2) .and. &
                    center_coord(2) < ChannelWidth/2.0d0 )then
                    
                    killElemList(j) = 1
                    
                endif
            endif
        enddo

        if(allocated(killElemList) )then
            call channel%killElement(blacklist=killElemList,flag=1)
        endif



    endif



end function
! ##########################################################

function beamCivilItem(this,from,to,width,height,division) result(beam)
    class(CivilItem_),intent(in) :: this
    real(real64),intent(in) :: from(1:3),to(1:3),width,height
    integer(int32),intent(in) :: division(1:3)
    real(real64) :: beam_length,angles(1:3),vec(1:3),xa(1:3),ya(1:3),za(1:3)
    type(FEMDomain_) :: beam

    angles(:) = 0.0d0
    beam_length = norm(to - from)
    vec = to - from
    xa = [1.0d0, 0.0d0, 0.0d0]
    ya = [0.0d0, 1.0d0, 0.0d0]
    za = [0.0d0, 0.0d0, 1.0d0]

    call beam%create("Cube3D",x_num=division(1),y_num=division(2),z_num=division(3) )
    call beam%resize(x=width,y=height,z=beam_length)
    call beam%move(x=-0.50d0*width,y=-0.50d0*height)
    
    if(norm(vec)==0.0d0 )then
        return
    endif
    angles(1) = acos(dot_product(vec,xa)/norm(vec)/norm(xa) )
    angles(2) = acos(dot_product(vec,ya)/norm(vec)/norm(ya) )
    angles(3) = acos(dot_product(vec,za)/norm(vec)/norm(za) )
    
    call beam%rotate(x=angles(1),y=angles(2),z=angles(3) )
    call beam%move(to="center")
    call beam%move(x=0.50d0*(from(1)+to(1)),y=0.50d0*(from(2)+to(2)),z=0.50d0*(from(3)+to(3)) )

end function

! ############################################################
function groundCivilItem(this,femdomain,surface_point,radius,debug,er) result(ret)
    class(CivilItem_),intent(in) :: this
    type(FEMDomain_),intent(in)  :: femdomain
    real(real64),intent(in)      :: surface_point(:,:) !(point-id,[x, y, z])
    real(real64),optional,intent(in)      :: radius,er
    logical,optional,intent(in) :: debug

    type(FEMDomain_) :: ground(1),ret
    real(real64),allocatable :: my_point(:),ref_point(:),&
        all_disp(:),FixValues(:)
    integer(int32),allocatable :: bottom(:),top(:),side(:),IDs(:)
    real(real64) :: r,z_diff
    type(FEMSolver_) :: solver
    integer(int32) :: i, j, ElementID


    ground(1) = femdomain

    bottom = ground(1)%select(z_max = ground(1)%zmin() )
    top    = ground(1)%select(z_min = ground(1)%zmax() )

    r = norm(ground(1)%mesh%nodcoord(1,:)-ground(1)%mesh%nodcoord(2,:))
    r = input(default=r,option=radius)

    side   =         ground(1)%select(x_max = ground(1)%xmin() )
    side   = side // ground(1)%select(x_min = ground(1)%xmax() )
    side   = side // ground(1)%select(y_max = ground(1)%ymin() )
    side   = side // ground(1)%select(y_min = ground(1)%ymax() )


    ! setup solver
    call solver%init(NumDomain=1 )
    call solver%setDomain(FEMDomains=ground(:),DomainIDs=[1])
    call solver%setCRS(DOF=3)

    !$OMP parallel 
    !$OMP do
    do ElementID = 1, ground(1)%ne()
        call solver%setMatrix(DomainID=1,ElementID=ElementID,DOF=3,&
           Matrix=ground(1)%StiffnessMatrix(ElementID=ElementID,E=1.0d0, v=0.450d0) )
    enddo
    !$OMP end do
    !$OMP end parallel

    ! x,y,z -> fix
    call solver%fix(DomainID=1,IDs=bottom*3-2,FixValue=0.0d0)
    call solver%fix(DomainID=1,IDs=bottom*3-1,FixValue=0.0d0)
    call solver%fix(DomainID=1,IDs=bottom*3-0,FixValue=0.0d0)

    ! x,y -> fix, z: free
    call solver%fix(DomainID=1,IDs=side*3-2,  FixValue=0.0d0)
    call solver%fix(DomainID=1,IDs=side*3-1,  FixValue=0.0d0)

    ! top -> move
    IDs = int(zeros(size(top) ) )
    FixValues = zeros(size(top) ) 
    !$OMP parallel default(shared) private(my_point,j,ref_point,z_diff)
    !$OMP do
    do i=1,size(top)
        my_point = ground(1)%mesh%nodcoord(top(i),1:2 )
        do j=1, size(surface_point,1)
            ref_point = surface_point(j,1:2)
            if(norm( my_point - ref_point ) <= r )then
                z_diff = surface_point(j,3) - ground(1)%mesh%nodcoord(top(i),3 )
                if(present(debug) )then
                    if(debug)then
                        print *, "node,",i,"/",size(top),"u_z",z_diff
                    endif
                endif
                IDs(i) = top(i)*3-0
                FixValues(i) = z_diff
                !call solver%fix(DomainID=1,IDs=[top(i)*3-0],  FixValue=z_diff)
                exit
            endif
        enddo
    enddo
    !$OMP end do
    !$OMP end parallel 
    

    call solver%fix(DomainID=1,IDs=IDs,  FixValues=FixValues)
    

    solver%debug = input(default=.true.,option= debug)
    solver%er0 = input(default=dble(1.0e-5),option= er)
    solver%relative_er = input(default=dble(1.0e-5),option= er)
    
    all_disp = solver%solve()
    
    call ground(1)%deform(disp=all_disp)

    ret = ground(1)

end function
! ############################################################

function BoxCulvertCivilItem(this,width,height,length,&
    top_thickness,side_thickness,bottom_thickness,&
    edge_thickness,divisions,&
    cut_angles) result(culvert)
    class(CivilItem_),intent(in) :: this
    type(FEMDomain_) :: culvert
    real(real64),intent(in) :: width,height,length,&
    top_thickness,side_thickness,bottom_thickness,&
    edge_thickness
    integer(int32),allocatable :: killElemList(:)
    integer(int32),intent(in) :: divisions(1:3)
    real(real64),optional,intent(in) :: cut_angles(1:2)

    real(real64) :: W, y_bar, B, T,alpha
    integer(int32) :: i,j
    real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:),center_coord(:),coord(:)


    x_axis = [-Length/2.0d0,0.0d0,Length/2.0d0]
    y_axis = [-Width/2.0d0,-Width/2.0d0+side_thickness,-Width/2.0d0+side_thickness+edge_thickness,&
        0.0d0,Width/2.0d0-side_thickness-edge_thickness,Width/2.0d0-side_thickness,Width/2.0d0]
    z_axis = [-Height/2.0d0,-Height/2.0d0+bottom_thickness,&
        -Height/2.0d0+bottom_thickness+edge_thickness,&
        0.0d0,Height/2.0d0-top_thickness-edge_thickness,&
         Height/2.0d0-top_thickness,Height/2.0d0]

    call refine(x_axis,divisions(1) )
    call refine(y_axis,divisions(2) )
    call refine(z_axis,divisions(3) )

    call culvert%create(meshtype="Cube3D",&
        x_axis=x_axis,&
        y_axis=y_axis,&
        z_axis=z_axis)

    killElemList = int(zeros(culvert%ne() ) )


    
    do j=1,culvert%ne()
        center_coord = culvert%centerPosition(ElementID=j)
        !if( abs(center_coord(2)) < Width/2.0d0-side_thickness )then    
        !    if( -Height/2.0d0+bottom_thickness+edge_thickness < center_coord(3) .and.&
        !        center_coord(3) < Height/2.0d0-top_thickness-edge_thickness )then    
        !        killElemList(j) = 1
        !    endif
        !endif
        if( abs(center_coord(2)) < Width/2.0d0-side_thickness-edge_thickness )then    
            if( -Height/2.0d0+bottom_thickness < center_coord(3) .and.&
                center_coord(3) < Height/2.0d0-top_thickness )then    
                killElemList(j) = 1
            endif
        endif
    enddo
    call culvert%killElement(blacklist=killElemList,flag=1)

    do i=1,culvert%nn()
        coord = culvert%mesh%nodcoord(i,:)
        if( -Height/2.0d0 + bottom_thickness < coord(3) .and.&
            coord(3) < Height/2.0d0 - top_thickness  )then
            if(Height/2.0d0 - top_thickness> coord(3) .and. &
                coord(3) > Height/2.0d0 - top_thickness - edge_thickness)then
                alpha = (Height/2.0d0 - top_thickness) - coord(3)
                alpha = alpha/edge_thickness
                
                if(coord(2)>0.0d0)then
                    ! 0 < z < Height/2 - top_thickness 
                    W = Width/2.0d0
                    y_bar = coord(2)
                    B = Width/2.0d0-side_thickness-edge_thickness
                    T = Width/2.0d0-side_thickness
                    culvert%mesh%nodcoord(i,2) = alpha*(  (y_bar-B)*W + (W-y_bar)*T  )/(W -B) &
                        + (1-alpha)*culvert%mesh%nodcoord(i,2)
                endif

                if(coord(2)<0.0d0)then
                    ! 0 < z < Height/2 - top_thickness 
                    W = Width/2.0d0
                    y_bar = abs(coord(2))
                    B = Width/2.0d0-side_thickness-edge_thickness
                    T = Width/2.0d0-side_thickness
                    culvert%mesh%nodcoord(i,2) = -alpha*abs((  (y_bar-B)*W + (W-y_bar)*T  )/(W -B)) &
                        + (1-alpha)*culvert%mesh%nodcoord(i,2)
                endif
            elseif(- Height/2.0d0 + bottom_thickness< coord(3) .and. &
                coord(3) < -Height/2.0d0 + bottom_thickness + edge_thickness)then
                alpha = abs(coord(3) - (-Height/2.0d0 + bottom_thickness) )
                alpha = alpha/edge_thickness
                
                if(coord(2)>0.0d0)then
                    ! 0 < z < Height/2 - bottom_thickness 
                    W = Width/2.0d0
                    y_bar = coord(2)
                    B = Width/2.0d0-side_thickness-edge_thickness
                    T = Width/2.0d0-side_thickness
                    culvert%mesh%nodcoord(i,2) = alpha*(  (y_bar-B)*W + (W-y_bar)*T  )/(W -B) &
                        + (1-alpha)*culvert%mesh%nodcoord(i,2)
                endif

                if(coord(2)<0.0d0)then
                    ! 0 < z < Height/2 - bottom_thickness 
                    W = Width/2.0d0
                    y_bar = abs(coord(2))
                    B = Width/2.0d0-side_thickness-edge_thickness
                    T = Width/2.0d0-side_thickness
                    culvert%mesh%nodcoord(i,2) = -alpha*abs((  (y_bar-B)*W + (W-y_bar)*T  )/(W -B)) &
                        + (1-alpha)*culvert%mesh%nodcoord(i,2)
                endif
            else
                if(coord(2)>0.0d0)then
                    ! 0 < z < Height/2 - top_thickness 
                    W = Width/2.0d0
                    y_bar = coord(2)
                    B = Width/2.0d0-side_thickness-edge_thickness
                    T = Width/2.0d0-side_thickness
                    culvert%mesh%nodcoord(i,2) = (  (y_bar-B)*W + (W-y_bar)*T  )/(W -B)
                endif

                if(coord(2)<0.0d0)then
                    ! 0 < z < Height/2 - top_thickness 
                    W = Width/2.0d0
                    y_bar = abs(coord(2))
                    B = Width/2.0d0-side_thickness-edge_thickness
                    T = Width/2.0d0-side_thickness
                    culvert%mesh%nodcoord(i,2) = -abs((  (y_bar-B)*W + (W-y_bar)*T  )/(W -B))
                endif
            endif
        endif
    enddo

    if(present(cut_angles) )then
        ! cut-off edges
        do i=1, culvert%nn()
            coord = culvert%mesh%nodcoord(i,:)
            if(coord(1)<0.0d0)then
                alpha = coord(2)/(Width/2.0d0) ! alpha = [-1,1]
                alpha = (alpha + 1.0d0)*0.50d0 ! alpha = [0,1]
                culvert%mesh%nodcoord(i,1) = culvert%mesh%nodcoord(i,1)  &
                 - alpha*Width/(Length/2.0d0)*tan(radian(cut_angles(1)))*culvert%mesh%nodcoord(i,1)
            else
                alpha = coord(2)/(Width/2.0d0) ! alpha = [-1,1]
                alpha = (alpha + 1.0d0)*0.50d0 ! alpha = [0,1]
                culvert%mesh%nodcoord(i,1) = culvert%mesh%nodcoord(i,1)  &
                 - alpha*Width/(Length/2.0d0)*tan(radian(cut_angles(2)))*culvert%mesh%nodcoord(i,1)
            endif
        enddo
    endif

end function
! ############################################################


function BoxCulvertCivilItem_JSON(this,config) result(culvert)
    class(CivilItem_),intent(in) :: this
    character(*),intent(in) :: config
    type(FEMDomain_) :: culvert

    real(real64) :: width,height,length,&
    top_thickness,side_thickness,bottom_thickness,&
    edge_thickness,Cut_angles(1:2),position(1:3),rotation(1:3)
    integer(int32) :: divisions(1:3)

    type(IO_) :: json_file

    Width = freal(json_file%parse(filename=config,key1="Width"))
    Height = freal(json_file%parse(filename=config,key1="Height"))
    Length = freal(json_file%parse(filename=config,key1="Length"))
    Top_thickness = freal(json_file%parse(filename=config,key1="Top_thickness"))
    Side_thickness = freal(json_file%parse(filename=config,key1="Side_thickness"))
    Bottom_thickness = freal(json_file%parse(filename=config,key1="Bottom_thickness"))
    Edge_thickness = freal(json_file%parse(filename=config,key1="Edge_thickness"))


    divisions = to_list(&
        json_file%parse(filename=config,key1="Divisions"),3)

    cut_angles = to_vector(&
        json_file%parse(filename=config,key1="Cut_angles"),2)
    
    position = to_vector(&
        json_file%parse(filename=config,key1="Position"),3)

    rotation = to_vector(&
        json_file%parse(filename=config,key1="Rotation"),3)

    culvert = this%BoxCulvert(&
        width=width,&
        height=height,&
        length=length,&
        top_thickness=top_thickness,&
        side_thickness=side_thickness,&
        bottom_thickness=bottom_thickness,&
        edge_thickness=edge_thickness,&
        divisions=divisions, &
        cut_angles=cut_angles)
    call culvert%move(x=position(1),y=position(2),z=position(3))
    call culvert%rotate(x=radian(rotation(1)),&
        y=radian(rotation(2)),&
        z=radian(rotation(3)))

end function 
! ############################################################

end module