
module CivilItemClass
    use FEMDomainClass
    implicit none
    
    type :: CivilItem_
    
    contains
        procedure, public :: BridgePier => BridgePierCivilItem
        procedure, public :: BridgeGirder => BridgeGirderCivilItem
        procedure, public :: BridgeShoe => BridgeShoeCivilItem
        procedure, public :: BridgeShoes => BridgeShoesCivilItem

        procedure, public :: RigidFrameViaduct => RigidFrameViaductCivilItem
    end type

contains

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

function RigidFrameViaductCivilItem(this,NumPiers,length,width,PierThickness,division,height,MiddlePierHeights,debug) result(RFV)
    class(CivilItem_),intent(inout) :: this
    integer(int32),intent(in) :: NumPiers(1:2) ! n by m, total n*m piers
    integer(int32),intent(in) :: division(1:3)
    real(real64),intent(in) :: length
    real(real64),intent(in) :: width
    real(real64),intent(in) :: height
    real(real64),intent(in) :: PierThickness
    real(real64),optional,intent(in) :: MiddlePierHeights(:)
    logical,optional,intent(in) :: debug

    real(real64) :: dx,dy,dz,thickness
    real(real64),allocatable :: point(:)
    type(FEMDomain_) :: RFV
    integer(int32) :: i, n , m 
    real(real64),allocatable :: remove_zone_x(:,:)
    real(real64),allocatable :: remove_zone_y(:,:)
    real(real64),allocatable :: remove_zone_z(:,:)

    integer(int32) :: ElementID, remove_count, j
    integer(int32),allocatable :: remove_elem(:),buf(:,:),remove_node(:),new_node_id(:)
    real(real64),allocatable :: realbuf(:,:)
    logical :: debug_mode_requested = .false.

    if(present(debug) )then
        debug_mode_requested = debug
    endif

    thickness = PierThickness

    if(maxval(NumPiers) <= 1 )then
        print *, "ERROR :: RigidFrameViaductCivilItem :: for single pier,"
        print *, "please use %BridgePier()"
        return
    endif


    call RFV%create("Cube3D",&
            x_num = division(1) ,&
            y_num = division(2) ,&
            z_num = division(3)  &
        )
    call RFV%resize(x=width, y=length, z=height)


    ! remove_zone
    if(NumPiers(1) <= 1 )then
        n = 1
        ! y
        ! |
        ! ---------------------------> x
        call RFV%resize(x=thickness, y=length, z=height)


        allocate(remove_zone_y(NumPiers(2)-1,2 ))

        remove_zone_y(1,1) = thickness  ! from
        remove_zone_y(1,2) = thickness + ( Length - dble(NumPiers(2))*thickness)/dble(NumPiers(2)-1 ) ! to
        ! y-direction
        do i=2,NumPiers(2)-1
            remove_zone_y(i,1) = remove_zone_y(i-1,2) +  thickness  ! from
            remove_zone_y(i,2) = remove_zone_y(i  ,1) +  ( Length - dble(NumPiers(2))*thickness)/dble(NumPiers(2)-1 ) ! to
        enddo

        ! z-direction
        if(present (MiddlePierHeights) )then
            allocate(remove_zone_z(size(MiddlePierHeights,1)+1,2) )
            remove_zone_z(1,1) = 0.0d0! from
            remove_zone_z(1,2) = MiddlePierHeights(1) - thickness/2.0d0! to
            do i=2,size(MiddlePierHeights,1)
                remove_zone_z(i,1) = remove_zone_z(i-1,2) + thickness ! from
                remove_zone_z(i,2) = remove_zone_z(i,1)   + MiddlePierHeights(i) - thickness/2.0d0  ! to
            enddo
            i = size(remove_zone_z,1)
            remove_zone_z(i,1) = remove_zone_z(i-1,2) + thickness ! from
            remove_zone_z(i,2) = height - thickness  ! to
            
        else
            allocate(remove_zone_z(1,2) )
            remove_zone_z(1,1) = 0.0d0 ! from
            remove_zone_z(1,2) = height - thickness  ! to
        endif

        ! debug

        if(debug_mode_requested )then
            print *, "[ok] remove_box set"
        endif
        
        allocate(remove_elem(RFV%ne() ))
        allocate(remove_node(RFV%nn() )  )
        remove_elem(:) = 0
        remove_node(:) = 0
        
        !$OMP parallel do default(shared) private(point)
        do ElementID=1, RFV%ne()
                
            point = RFV%centerPosition(ElementID=ElementID)
            
            do i=1,size(remove_zone_y,1)
                if(remove_zone_y(i,1) < point(2) .and. point(2) < remove_zone_y(i,2)  )then
                    remove_elem(ElementID) = remove_elem(ElementID) + 1
                    exit
                endif
            enddo
    
            do i=1,size(remove_zone_z,1)
                if(remove_zone_z(i,1) < point(3) .and. point(3) < remove_zone_z(i,2)  )then
                    remove_elem(ElementID) = remove_elem(ElementID) + 1
                    exit
                endif
            enddo
        enddo
        !$OMP end parallel do
        
        if(debug_mode_requested )then
            print *, "[ok] remove_elem ready"
        endif
    
        remove_count = 0
        do i=1,size(remove_elem)
            if(remove_elem(i) >=2)then
                remove_count = remove_count + 1
            endif
        enddo
        buf = RFV%mesh%elemnod
    
        deallocate(RFV%mesh%elemnod)
        allocate(RFV%mesh%elemnod( size(buf,1)-remove_count,size(buf,2) ) )
    
        j = 0
    
        do i=1,size(buf,1)
            if(remove_elem(i) < 2)then
                j = j + 1
                RFV%mesh%elemnod(j,:) = buf(i,:)
            endif
        enddo
    
        deallocate(remove_elem )
        deallocate(buf)
    
    
        if(debug_mode_requested )then
            print *, "[ok] remove_elem done"
        endif
    
    
        remove_node(:) = 1
        
        do i=1, size(RFV%mesh%elemnod,1)
            do j = 1,size(RFV%mesh%elemnod,2)
                remove_node(RFV%mesh%elemnod(i,j) ) = 0
            enddo
        enddo
        
        allocate(new_node_id(size(RFV%mesh%nodcoord,1) ))
        j = 0
        do i=1,size(RFV%mesh%nodcoord,1)
            if(remove_node(i)==0 )then
                ! not removed
                j=j+1
                new_node_id(i)=j
            else
                ! removed
                new_node_id(i) = j
                cycle
            endif
        enddo
        
        realbuf = RFV%mesh%nodcoord(:,:)
        RFV%mesh%nodcoord = zeros(size(realbuf,1)-sum(remove_node),size(realbuf,2) )
        j = 0
        do i=1, size(realbuf,1)
            if(remove_node(i) /=1)then
                j = j + 1
                RFV%mesh%nodcoord(j,:) = realbuf(i,:)
            endif
        enddo
        
        !$OMP parallel do
        do i=1, size(RFV%mesh%elemnod,1)
            do j = 1,size(RFV%mesh%elemnod,2)
                RFV%mesh%elemnod(i,j) = new_node_id(RFV%mesh%elemnod(i,j) )
            enddo
        enddo
        !$OMP end parallel do


    elseif(NumPiers(2) <= 1 )then
        m = 1
        ! y
        ! |
        ! ---------------------------> x
        call RFV%resize(x=Width, y=thickness, z=height)

        allocate(remove_zone_x(NumPiers(1)-1,2 ))

        remove_zone_x(1,1) = thickness  ! from
        remove_zone_x(1,2) = thickness + ( Width - dble(NumPiers(1))*thickness)/dble(NumPiers(1)-1 ) ! to
        ! y-direction
        do i=2,NumPiers(1)-1
            remove_zone_x(i,1) = remove_zone_x(i-1,2) +  thickness  ! from
            remove_zone_x(i,2) = remove_zone_x(i  ,1) +  ( Width - dble(NumPiers(1))*thickness)/dble(NumPiers(1)-1 ) ! to
        enddo

        ! z-direction
        if(present (MiddlePierHeights) )then
            allocate(remove_zone_z(size(MiddlePierHeights,1)+1,2) )
            remove_zone_z(1,1) = 0.0d0! from
            remove_zone_z(1,2) = MiddlePierHeights(1) - thickness/2.0d0! to
            do i=2,size(MiddlePierHeights,1)
                remove_zone_z(i,1) = remove_zone_z(i-1,2) + thickness ! from
                remove_zone_z(i,2) = remove_zone_z(i,1)   + MiddlePierHeights(i) - thickness/2.0d0  ! to
            enddo
            i = size(remove_zone_z,1)
            remove_zone_z(i,1) = remove_zone_z(i-1,2) + thickness ! from
            remove_zone_z(i,2) = height - thickness  ! to
            
        else
            allocate(remove_zone_z(1,2) )
            remove_zone_z(1,1) = 0.0d0 ! from
            remove_zone_z(1,2) = height - thickness  ! to
        endif

        ! debug

        if(debug_mode_requested )then
            print *, "[ok] remove_box set"
        endif
        
        allocate(remove_elem(RFV%ne() ))
        allocate(remove_node(RFV%nn() )  )
        remove_elem(:) = 0
        remove_node(:) = 0
        
        !$OMP parallel do default(shared) private(point)
        do ElementID=1, RFV%ne()
                
            point = RFV%centerPosition(ElementID=ElementID)
            
            do i=1,size(remove_zone_x,1)
                if(remove_zone_x(i,1) < point(1) .and. point(1) < remove_zone_x(i,2)  )then
                    remove_elem(ElementID) = remove_elem(ElementID) + 1
                    exit
                endif
            enddo
    
            do i=1,size(remove_zone_z,1)
                if(remove_zone_z(i,1) < point(3) .and. point(3) < remove_zone_z(i,2)  )then
                    remove_elem(ElementID) = remove_elem(ElementID) + 1
                    exit
                endif
            enddo
        enddo
        !$OMP end parallel do
        
        if(debug_mode_requested )then
            print *, "[ok] remove_elem ready"
        endif
    
        remove_count = 0
        do i=1,size(remove_elem)
            if(remove_elem(i) >=2)then
                remove_count = remove_count + 1
            endif
        enddo
        buf = RFV%mesh%elemnod
    
        deallocate(RFV%mesh%elemnod)
        allocate(RFV%mesh%elemnod( size(buf,1)-remove_count,size(buf,2) ) )
    
        j = 0
    
        do i=1,size(buf,1)
            if(remove_elem(i) < 2)then
                j = j + 1
                RFV%mesh%elemnod(j,:) = buf(i,:)
            endif
        enddo
    
        deallocate(remove_elem )
        deallocate(buf)
    
    
        if(debug_mode_requested )then
            print *, "[ok] remove_elem done"
        endif
    
    
        remove_node(:) = 1
        
        do i=1, size(RFV%mesh%elemnod,1)
            do j = 1,size(RFV%mesh%elemnod,2)
                remove_node(RFV%mesh%elemnod(i,j) ) = 0
            enddo
        enddo
        
        allocate(new_node_id(size(RFV%mesh%nodcoord,1) ))
        j = 0
        do i=1,size(RFV%mesh%nodcoord,1)
            if(remove_node(i)==0 )then
                ! not removed
                j=j+1
                new_node_id(i)=j
            else
                ! removed
                new_node_id(i) = j
                cycle
            endif
        enddo
        
        realbuf = RFV%mesh%nodcoord(:,:)
        RFV%mesh%nodcoord = zeros(size(realbuf,1)-sum(remove_node),size(realbuf,2) )
        j = 0
        do i=1, size(realbuf,1)
            if(remove_node(i) /=1)then
                j = j + 1
                RFV%mesh%nodcoord(j,:) = realbuf(i,:)
            endif
        enddo
        
        !$OMP parallel do
        do i=1, size(RFV%mesh%elemnod,1)
            do j = 1,size(RFV%mesh%elemnod,2)
                RFV%mesh%elemnod(i,j) = new_node_id(RFV%mesh%elemnod(i,j) )
            enddo
        enddo
        !$OMP end parallel do
        print *, "debug"

    else
        ! y
        ! |
        ! ---------------------------> x

        allocate(remove_zone_x(NumPiers(1)-1,2 ))
        
        remove_zone_x(1,1) = thickness  ! from
        remove_zone_x(1,2) = thickness + ( Width - dble(NumPiers(1))*thickness)/dble(NumPiers(1)-1 ) ! to
        ! y-direction
        do i=2,NumPiers(1)-1
            remove_zone_x(i,1) = remove_zone_x(i-1,2) +  thickness  ! from
            remove_zone_x(i,2) = remove_zone_x(i  ,1) + ( Width - dble(NumPiers(1))*thickness)/dble(NumPiers(1)-1 ) ! to
        enddo

        

        allocate(remove_zone_y(NumPiers(2)-1,2 ))

        remove_zone_y(1,1) = thickness  ! from
        remove_zone_y(1,2) = thickness + ( Length - dble(NumPiers(2))*thickness)/dble(NumPiers(2)-1 ) ! to
        ! y-direction
        do i=2,NumPiers(2)-1
            remove_zone_y(i,1) = remove_zone_y(i-1,2) +  thickness  ! from
            remove_zone_y(i,2) = remove_zone_y(i  ,1) + ( Length - dble(NumPiers(2))*thickness)/dble(NumPiers(2)-1 ) ! to
        enddo


        ! z-direction
        if(present (MiddlePierHeights) )then
            allocate(remove_zone_z(size(MiddlePierHeights,1)+1,2) )
            remove_zone_z(1,1) = 0.0d0! from
            remove_zone_z(1,2) = MiddlePierHeights(1) - thickness/2.0d0! to
            do i=2,size(MiddlePierHeights,1)
                remove_zone_z(i,1) = remove_zone_z(i-1,2) + thickness ! from
                remove_zone_z(i,2) = MiddlePierHeights(i) - thickness/2.0d0  ! to
            enddo
            i = size(remove_zone_z,1)
            remove_zone_z(i,1) = remove_zone_z(i-1,2) + thickness ! from
            remove_zone_z(i,2) = height - thickness  ! to
            
        else
            allocate(remove_zone_z(1,2) )
            remove_zone_z(1,1) = 0.0d0 ! from
            remove_zone_z(1,2) = height - thickness  ! to
        endif
        

        if(debug_mode_requested )then
            print *, "[ok] remove_box set"
        endif
        
        allocate(remove_elem(RFV%ne() ))
        allocate(remove_node(RFV%nn() )  )
        remove_elem(:) = 0
        remove_node(:) = 0
        
        !$OMP parallel do default(shared) private(point)
        do ElementID=1, RFV%ne()
                
            point = RFV%centerPosition(ElementID=ElementID)
            do i=1,size(remove_zone_x,1)
                if(remove_zone_x(i,1) < point(1) .and. point(1) < remove_zone_x(i,2)  )then
                    remove_elem(ElementID) = remove_elem(ElementID) + 1
                    exit
                endif
            enddo
    
            do i=1,size(remove_zone_y,1)
                if(remove_zone_y(i,1) < point(2) .and. point(2) < remove_zone_y(i,2)  )then
                    remove_elem(ElementID) = remove_elem(ElementID) + 1
                    exit
                endif
            enddo
    
            do i=1,size(remove_zone_z,1)
                if(remove_zone_z(i,1) < point(3) .and. point(3) < remove_zone_z(i,2)  )then
                    remove_elem(ElementID) = remove_elem(ElementID) + 1
                    exit
                endif
            enddo
        enddo
        !$OMP end parallel do
        
        if(debug_mode_requested )then
            print *, "[ok] remove_elem ready"
        endif
    
        remove_count = 0
        do i=1,size(remove_elem)
            if(remove_elem(i) >=2)then
                remove_count = remove_count + 1
            endif
        enddo
        buf = RFV%mesh%elemnod
    
        deallocate(RFV%mesh%elemnod)
        allocate(RFV%mesh%elemnod( size(buf,1)-remove_count,size(buf,2) ) )
    
        j = 0
    
        do i=1,size(buf,1)
            if(remove_elem(i) < 2)then
                j = j + 1
                RFV%mesh%elemnod(j,:) = buf(i,:)
            endif
        enddo
    
        deallocate(remove_elem )
        deallocate(buf)
    
    
        if(debug_mode_requested )then
            print *, "[ok] remove_elem done"
        endif
    
    
        remove_node(:) = 1
        
        do i=1, size(RFV%mesh%elemnod,1)
            do j = 1,size(RFV%mesh%elemnod,2)
                remove_node(RFV%mesh%elemnod(i,j) ) = 0
            enddo
        enddo
        
        allocate(new_node_id(size(RFV%mesh%nodcoord,1) ))
        j = 0
        do i=1,size(RFV%mesh%nodcoord,1)
            if(remove_node(i)==0 )then
                ! not removed
                j=j+1
                new_node_id(i)=j
            else
                ! removed
                new_node_id(i) = j
                cycle
            endif
        enddo
        
        realbuf = RFV%mesh%nodcoord(:,:)
        RFV%mesh%nodcoord = zeros(size(realbuf,1)-sum(remove_node),size(realbuf,2) )
        j = 0
        do i=1, size(realbuf,1)
            if(remove_node(i) /=1)then
                j = j + 1
                RFV%mesh%nodcoord(j,:) = realbuf(i,:)
            endif
        enddo
        
        !$OMP parallel do
        do i=1, size(RFV%mesh%elemnod,1)
            do j = 1,size(RFV%mesh%elemnod,2)
                RFV%mesh%elemnod(i,j) = new_node_id(RFV%mesh%elemnod(i,j) )
            enddo
        enddo
        !$OMP end parallel do
        
    endif

    

end function

end module