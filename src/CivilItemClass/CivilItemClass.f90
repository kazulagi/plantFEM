module CivilItemClass
    use FEMDomainClass
    implicit none
    
    type :: CivilItem_
    
    contains
        procedure, public :: BridgePier => BridgePierCivilItem
        procedure, public :: BridgeGirder => BridgeGirderCivilItem
    end type

contains

function BridgePierCivilItem(this,Bottom,Top,Transition,divisions) result(femdomain)
    class(CivilItem_),intent(in) :: this
    real(real64),intent(in) :: Bottom(1:3),Top(1:3)
    real(real64),optional,intent(in) :: Transition(1:2)
    integer(int32),intent(in) :: divisions(1:3)
    type(FEMDomain_) :: femdomain
    integer(int32) :: i
    real(real64) :: z, x,theta
    
    
    call femdomain%create("Cube3D",x_num=divisions(1),y_num=divisions(2),z_num=divisions(3) )
    call femdomain%resize(x=Bottom(1),y=Bottom(2),z=Bottom(3) )
    call femdomain%move(x=-Bottom(1)/2.0d0,y=-Bottom(2)/2.0d0 )
    
    if(present(Transition) )then
        do i=1,femdomain%nn()
            if(femdomain%position_z(i) >= Transition(2)  ) then
                femdomain%mesh%nodcoord(i,1) = femdomain%mesh%nodcoord(i,1)*Top(1)/Bottom(1)
            elseif(femdomain%position_z(i) <= Transition(2) .and. femdomain%position_z(i) >= Transition(1) )then
                z = femdomain%position_z(i)
                theta = (z - Transition(1))/(Transition(2) - Transition(1) )
                femdomain%mesh%nodcoord(i,1) = femdomain%mesh%nodcoord(i,1)*(Bottom(1) + theta*(Top(1) - Bottom(1) )   )/Bottom(1)
            endif
        enddo
    endif

end function

function BridgeGirderCivilItem(this,From,To,Thickness,Width,Divisions) result(femdomain)
    class(CivilItem_),intent(in) :: this
    type(FEMDomain_),intent(in) :: From, To
    real(real64),intent(in) :: Thickness,Width
    integer(int32),intent(in) :: divisions(1:3)

    type(FEMDomain_) :: femdomain
    integer(int32) :: i
    real(real64) :: girder_length,x1(1:3),x2(1:3),theta_z,theta_x
    
    x1(1) = sum(from%mesh%nodcoord(:,1) )/dble(from%nn() )
    x1(2) = sum(from%mesh%nodcoord(:,2) )/dble(from%nn() )
    x1(3) = from%z_max()
    
    x2(1) = sum(to%mesh%nodcoord(:,1) )/dble(to%nn() )
    x2(2) = sum(to%mesh%nodcoord(:,2) )/dble(to%nn() )
    x2(3) = to%z_max()

    girder_length = norm(x2(1:2)-x1(1:2) )
    print *, girder_length,x2(1:2),x1(1:2)
    theta_z = dot_product(x2(1:2)-x1(1:2),[0.0d0,1.0d0])/norm(x2(1:2)-x1(1:2) )/1.0d0

    theta_x = dot_product(x2(2:3)-x1(2:3),[1.0d0,0.0d0])/norm(x2(2:3)-x1(2:3) )/1.0d0

    call femdomain%create("Cube3D",x_num=divisions(1),y_num=divisions(2),z_num=divisions(3) )
    call femdomain%resize(x=Width,y=girder_length,z=thickness )
    call femdomain%move(&
        x = -(x1(1)+x2(1))*0.50d0,&
        y = -(x1(2)+x2(2))*0.50d0,& 
        z = (from%z_max() + to%z_max())*0.50d0  )
    !call femdomain%rotate(x=theta_x,z=theta_z)
    
end function


end module
