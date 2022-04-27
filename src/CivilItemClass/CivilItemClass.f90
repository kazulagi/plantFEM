
module CivilItemClass
    use FEMDomainClass
    implicit none
    
    type :: CivilItem_
    
    contains
        procedure, public :: BridgePier => BridgePierCivilItem
        procedure, public :: BridgeGirder => BridgeGirderCivilItem
        procedure, public :: BridgeShoe => BridgeShoeCivilItem
        procedure, public :: BridgeShoes => BridgeShoesCivilItem
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



end module