module GeometryClass
    use MathClass
    use ArrayOperationClass
    implicit none

    type Point_
        real(8),allocatable :: coord(:)
    contains
        procedure :: Init   => InitPoint
        procedure :: set    => setPoint
        procedure :: show   => showPoint
    end type

    type Line_
        real(8),allocatable :: coord(:,:)
    contains
        procedure :: Init       => InitLine
        procedure :: setNode    => SetNodeLine
        procedure :: import     => importLine
        procedure :: show       => showLine 
    end type

    type Circle_
        real(8) :: radius
        real(8),allocatable :: center(:)
    contains
        procedure :: Init       => InitCircle     
        procedure :: SetCenter  => InitSetCenterCircle     
        procedure :: SetRadius  => InitSetRadiusCircle   
        procedure :: getArea    => getAreaCircle
        procedure :: show       => showCircle
    end type

    type Sphere_
        real(8) :: radius
        real(8) :: center(3)
    contains
        procedure :: Init       => InitSphere     
        procedure :: SetCenter  => InitSetCenterSphere     
        procedure :: SetRadius  => InitSetRadiusSphere 
        procedure :: show       => showSphere
    end type


    type Triangle_
        real(8),allocatable :: NodCoord(:,:)
    contains
        procedure :: Init       => InitTriangle
        procedure :: setNode    => setNodeTriangle
        procedure :: import     => importTriangle
        procedure :: getCircle  => getCircleTriangle
        procedure :: getArea    => getAreaTriangle 
        procedure :: show       => showTriangle
    end type

    type Tetragon_
        real(8),allocatable :: NodCoord(:,:)
    end type

    type Tetrahedron_
        real(8) :: NodCoord(4,3)
        real(8) :: radius
        real(8) :: center(3)
    contains
        procedure :: Init => InitTetrahedron
        procedure :: getCircle => getCircleTetrahedron
    end type
    
    type Octahedron_
        real(8),allocatable :: NodCoord(:,:)
    end type
contains

!#########################################################
subroutine InitPoint(obj,dim)
    class(Point_),intent(inout)::obj
    integer,optional,intent(in)::dim

    ! default == 3
    if(allocated(obj%coord) )then
        deallocate(obj%coord)
    endif
    allocate(obj%coord(input(default=3,option=dim) ) )
    obj%coord(:)=0.0d0

end subroutine
!#########################################################


!#########################################################
subroutine showPoint(obj,Name)
    class(Point_),intent(in)::obj
    character(*),optional,intent(in)::Name

    if(present(Name) )then
        open(10,file=Name)
        write(10,*) obj%coord(:)
        close(10)
    else
        print *, "Point> x(:) = ",obj%coord(:)
    endif
end subroutine
!#########################################################



!#########################################################
subroutine setPoint(obj,x,y,z,xvec)
    class(Point_),intent(inout)::obj
    real(8),optional,intent(in)::x,y,z
    real(8),optional,intent(in)::xvec(:)
    integer :: n

    n=size(obj%coord)
    if(present(xvec) )then
        if(n/=size(xvec) )then
            print *, "ERROR :: setPoint :: n/=size(xvec)"
            return
        endif
        obj%coord(:)=xvec(:)
        return
    endif

    if(present(x) )then
        obj%coord(1)=x
        
    endif


    if(present(y) )then
        obj%coord(2)=y
        
    endif

    if(size(obj%coord)<=2 )then
        return
    endif

    if(present(z) )then
        obj%coord(3)=z
    endif

end subroutine
!#########################################################


!#########################################################
subroutine InitLine(obj,dim)
    class(Line_),intent(inout)::obj
    integer,optional,intent(inout)::dim

    ! default = 3D
    if(allocated(obj%coord) )then
        deallocate(obj%coord)
    endif
    allocate(obj%coord(2,input(default=3,option=dim) ) )
    obj%coord(:,:)=0.0d0
    
end subroutine
!#########################################################

!#########################################################
subroutine SetNodeLine(obj,point,position)
    class(Line_),intent(inout)::obj
    class(Point_),intent(in)::Point
    integer,intent(in)::position

    if(position <=0 .or. position >= 3)then
        print *, "ERROR :: Line%SetNode => (position <=0 .or. position >= 3)",position
        return
    endif

    if(size(obj%coord,2)/=size(point%coord) )then
        print *, "ERROR :: Line%SetNode => (size(obj%coord,2)/=size(point%coord) )"
        return
    else
        obj%coord(position,:)=point%coord(:)
    endif

end subroutine
!#########################################################



!#########################################################
subroutine importLine(obj,NodCoord)
    class(Line_),intent(inout)::obj
    integer,intent(in)::NodCoord(:,:)

    if(size(obj%coord,2)/=size(NodCoord,2) )then
        print *, "ERROR :: importLine :: size(obj%NodCoord,2)/=size(NodCoord,2)"
        return
    endif
    obj%coord(1:2,:)=NodCoord(1:2,:)

end subroutine
!#########################################################



!#########################################################
subroutine showLine(obj,Name)
    class(Line_),intent(in)::obj
    character(*),optional,intent(in)::Name

    if(present(Name) )then
        open(10,file=Name)
        write(10,*) obj%coord(1,:)
        write(10,*) obj%coord(2,:)
        close(10)
    else
        print *, "Line> x1(:) = ",obj%coord(1,:)
        print *, "Line> x2(:) = ",obj%coord(2,:)
    endif
end subroutine
!#########################################################



!#########################################################
subroutine InitCircle(obj,dim)
    class(Circle_),intent(inout)::obj
    integer,optional,intent(inout)::dim

    ! default = 3D
    if(allocated(obj%center) )then
        deallocate(obj%center)
    endif
    allocate(obj%center(input(default=3,option=dim) ) )
    obj%center(:)=0.0d0
    obj%radius=1.0d0

end subroutine
!#########################################################

!#########################################################
subroutine InitSetCenterCircle(obj,point)
    class(Circle_),intent(inout)::obj
    class(Point_),intent(in)::point

    if( size(obj%center)/=size(point%coord) )then
        print *, "ERROR ::InitSetCenterCircle ::  ( size(obj%center)/=size(point%coord) )"
        return
    endif

    obj%center(:)=point%coord(:)

end subroutine
!#########################################################

!#########################################################
subroutine InitSetRadiusCircle(obj,radius)
    class(Circle_),intent(inout)::obj
    real(8),intent(in)::radius

    obj%radius=radius
    
end subroutine
!#########################################################



!#########################################################
function getAreaCircle(obj) result(area)
    class(Circle_),intent(inout)::obj
    real(8) :: area,pi
    pi=3.14159260d0

    area=obj%radius*obj%radius*pi

end function
!#########################################################



!#########################################################
subroutine showCircle(obj,Name)
    class(Circle_),intent(in)::obj
    character(*),optional,intent(in)::Name
    real(8) :: angle,dtheta,pi
    integer :: i

    pi=3.14159260d0

    if(present(Name) )then
        open(10,file=Name)
        angle=0.0d0
        dtheta=2.0d0*pi/360.0
        do i=1, 360
            write(10,*) obj%center(1)+obj%radius*cos(angle) , obj%center(2)+obj%radius*sin(angle)
            angle = angle + dtheta
        enddo
        close(10)
    else
        print *, "center> O(:) = ", obj%center(:)
        print *, "radius = ", obj%radius
    endif

end subroutine
!#########################################################




!#########################################################
subroutine InitSphere(obj,dim)
    class(Sphere_),intent(inout)::obj
    integer,optional,intent(inout)::dim

    obj%center(:)=0.0d0
    obj%radius=1.0d0

end subroutine
!#########################################################

!#########################################################
subroutine InitSetCenterSphere(obj,point)
    class(Sphere_),intent(inout)::obj
    class(Point_),intent(in)::point

    if( size(obj%center)/=size(point%coord) )then
        print *, "ERROR ::InitSetCenterSphere ::  ( size(obj%center)/=size(point%coord) )"
        return
    endif

    obj%center(:)=point%coord(:)

end subroutine
!#########################################################

!#########################################################
subroutine InitSetRadiusSphere(obj,radius)
    class(Sphere_),intent(inout)::obj
    real(8),intent(in)::radius

    obj%radius=radius
    
end subroutine
!#########################################################


!#########################################################
subroutine showSphere(obj,Name)
    class(Sphere_),intent(in)::obj
    character(*),optional,intent(in)::Name
    real(8) :: angle1,angle2,dtheta,pi
    integer :: i,j

    pi=3.14159260d0

    if(present(Name) )then
        open(10,file=Name)
        angle1=0.0d0
        angle2=0.0d0
        dtheta=2.0d0*pi/360.0
        do i=1,360
            do j=1,360
                write(10,*) obj%center(1)+obj%radius*sin(angle1)*cos(angle2),&
                    obj%center(2)+obj%radius*sin(angle1)*sin(angle2),&
                    obj%center(3)+obj%radius*cos(angle1)
            enddo
        enddo
        close(10)
    else
        print *, "center> O(:) = ",obj%center(:)
        print *, "radius = ",obj%radius
    endif
    
end subroutine
!#########################################################



!#########################################################
subroutine InitTriangle(obj,dim)
    class(Triangle_),intent(inout)::obj
    integer,optional,intent(in)::dim


    if(allocated(obj%NodCoord) )then
        deallocate(obj%NodCoord)
    endif
    allocate(obj%NodCoord(3,input(default=3,option=dim) ) )
    obj%NodCoord(:,:)=0.0d0

end subroutine
!#########################################################

!#########################################################
subroutine setNodeTriangle(obj,point,order)
    class(Triangle_),intent(inout)::obj
    class(Point_),intent(in)::point
    integer,intent(in)::order

    if( size(obj%NodCoord,2)/=size(point%coord) )then
        print *, "ERROR ::InitSetNodeTriangle ::  ( size(obj%NodCoord,2)/=size(point%coord)  )"
        return
    endif
    obj%NodCoord(order,:)=point%coord(:)
end subroutine
!#########################################################



!#########################################################
subroutine importTriangle(obj,NodCoord)
    class(Triangle_),intent(inout)::obj
    integer,intent(in)::NodCoord(:,:)

    if(size(obj%NodCoord,2)/=size(NodCoord,2) )then
        print *, "ERROR :: importTriangle :: size(obj%NodCoord,2)/=size(NodCoord,2)"
        return
    endif
    obj%NodCoord(1:3,:)=NodCoord(1:3,:)

end subroutine
!#########################################################


!#########################################################
subroutine getCircleTriangle(obj,type_of_circle,circle) 
    class(Triangle_),intent(in)::obj
    type(Circle_),intent(inout) :: circle
    character(*),intent(in) :: type_of_circle
    real(8) :: x1,x2,x3,y1,y2,y3
    real(8),allocatable :: a(:),b(:),c(:)
    integer :: i,n

    n=size(obj%NodCoord,2)
    call initCircle(circle,dim=n)

    if(type_of_circle == "center_of_gravity")then
        
        do i=1,3
            circle%center(:)=circle%center(:)+1.0d0/3.0d0*obj%NodCoord(i,:)
        enddo
        circle%radius=1.0d0
        
    elseif( type_of_circle == "circumcenter"  )then
        
        ! 外心を計算する　
        allocate(a( size(obj%NodCoord,2) ) )
        a(:) = obj%NodCoord(1,:)
        x1 = obj%NodCoord( 1,1 )
        y1 = obj%NodCoord( 1,2 )
        x2 = obj%NodCoord( 2,1 )
        y2 = obj%NodCoord( 2,2 )
        x3 = obj%NodCoord( 3,1 )
        y3 = obj%NodCoord( 3,2 )
        circle%center(1)=  0.50d0*( &
                    (x1*x1+y1*y1)*(y2-y3) + &
                    (x2*x2+y2*y2)*(y3-y1) + &
                    (x3*x3+y3*y3)*(y1-y2)  &
                    )
        circle%center(1)=  circle%center(1)/(&
                    x1*(y2-y3) + &
                    x2*(y3-y1) + &
                    x3*(y1-y2)  &
                    )
        circle%center(2)=  0.50d0*( &
                    (x1*x1+y1*y1)*(x2-x3) + &
                    (x2*x2+y2*y2)*(x3-x1) + &
                    (x3*x3+y3*y3)*(x1-x2)  &
                    )
        circle%center(2)=  -circle%center(2)/(&
                    x1*(y2-y3) + &
                    x2*(y3-y1) + &
                    x3*(y1-y2)  &
                    )
        circle%radius = distance(circle%center,a )

    elseif( type_of_circle == "innter_center"  )then
        
        ! 外心を計算する　
        allocate(a( size(obj%NodCoord,2) ) )
        allocate(b( size(obj%NodCoord,2) ) )
        allocate(c( size(obj%NodCoord,2) ) )
        a(:) = obj%NodCoord(1,:)
        b(:) = obj%NodCoord(2,:)
        c(:) = obj%NodCoord(3,:)
        x1 = obj%NodCoord( 1,1 )
        y1 = obj%NodCoord( 1,2 )
        x2 = obj%NodCoord( 2,1 )
        y2 = obj%NodCoord( 2,2 )
        x3 = obj%NodCoord( 3,1 )
        y3 = obj%NodCoord( 3,2 )
        circle%center(1)=  sqrt( (x3-x2)*(x3-x2)+(y3-y2)*(y3-y2) )*x1 + &
                    sqrt( (x1-x3)*(x1-x3)+(y1-y3)*(y1-y3) )*x2 + &
                    sqrt( (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1) )*x3 
        circle%center(1)=  circle%center(1)/(& 
                    sqrt( (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1) ) + &
                    sqrt( (x3-x2)*(x3-x2)+(y3-y2)*(y3-y2) ) + &
                    sqrt( (x1-x3)*(x1-x3)+(y1-y3)*(y1-y3) ) &
                    )
        circle%center(2)=  sqrt( (x3-x2)*(x3-x2)+(y3-y2)*(y3-y2) )*y1 + &
                    sqrt( (x1-x3)*(x1-x3)+(y1-y3)*(y1-y3) )*y2 + &
                    sqrt( (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1) )*y3 
        circle%center(2)=  circle%center(2)/(& 
                    sqrt( (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1) ) + &
                    sqrt( (x3-x2)*(x3-x2)+(y3-y2)*(y3-y2) ) + &
                    sqrt( (x1-x3)*(x1-x3)+(y1-y3)*(y1-y3) ) &
                    )   
        circle%radius = 2.0d0*obj%getArea()/(distance(a,b )+ distance(b,c )+ distance(c,a ) )

    elseif( type_of_circle == "excenter"  )then
        print *, "ERROR :: getCircleTriangle :: type_of_circle/excenter is now being implemented"
    elseif( type_of_circle == "orthocenter"  )then
        print *, "ERROR :: getCircleTriangle :: type_of_circle/orthocenter is now being implemented"
    endif

end subroutine
!#########################################################


!#########################################################
function getAreaTriangle(obj) result(area)
    class(Triangle_),intent(in)::obj
    real(8) :: area
    real(8) :: x1,x2,x3,y1,y2,y3

    if(size(obj%NodCoord,2)==2 )then

        x1 = obj%NodCoord( 1,1 )
        y1 = obj%NodCoord( 1,2 )
        x2 = obj%NodCoord( 2,1 )
        y2 = obj%NodCoord( 2,2 )
        x3 = obj%NodCoord( 3,1 )
        y3 = obj%NodCoord( 3,2 )
        area = abs(0.50d0*( x1*y2 + x2*y3 + x3*y1 - y1*x2 -y2*x3 - y3*x1 ))

    elseif( size(obj%NodCoord,2)==3 )then

    else
        print *, "ERROR :: getAreaTriangle, size(obj%NodCoord,2)==2 "
        return
    endif

end function
!#########################################################



!#########################################################
subroutine showTriangle(obj,Name,option)
    class(Triangle_),intent(in)::obj
    character(*),optional,intent(in)::Name,option

    if(present(Name) )then
        if(present(option) )then
            open(10,file=Name)
            write(10,*) obj%NodCoord(1,:)
            write(10,*) obj%NodCoord(2,:)
            write(10,*) obj%NodCoord(3,:)
            write(10,*) obj%NodCoord(1,:)
            close(10)
        else
            open(10,file=Name)
            write(10,*) obj%NodCoord(1,:),obj%NodCoord(2,:)-obj%NodCoord(1,:)
            write(10,*) obj%NodCoord(2,:),obj%NodCoord(3,:)-obj%NodCoord(2,:)
            write(10,*) obj%NodCoord(3,:),obj%NodCoord(1,:)-obj%NodCoord(3,:)
            close(10)
        endif
    else
        print *, "Triangle> x1(:) = ",obj%NodCoord(1,:)
        print *, "Triangle> x2(:) = ",obj%NodCoord(2,:)
        print *, "Triangle> x3(:) = ",obj%NodCoord(3,:)
    endif
end subroutine
!#########################################################


!#########################################################
subroutine InitTetrahedron(obj,NodCoord)
    class(Tetrahedron_),intent(inout) :: obj
    real(8),intent(in) :: NodCoord(4,3)

    obj%NodCoord(:,:)=NodCoord(:,:)

end subroutine
!#########################################################

!#########################################################
subroutine getCircleTetrahedron(obj)
    class(Tetrahedron_),intent(inout) :: obj
    real(8) :: a(3),b(3),c(3),d(3),e(3),f(3),g(3),s,t,r,N(3)
    real(8) :: a_(3),b_(3),c_(3),d_(3),aA,aB,aC,aD,V,k
    
    a(:)=obj%NodCoord(1,:)
    b(:)=obj%NodCoord(2,:)
    c(:)=obj%NodCoord(3,:)
    d(:)=obj%NodCoord(4,:)

    a_(:) = a(:) - d(:)
    b_(:) = b(:) - d(:)
    c_(:) = c(:) - d(:)
    d_(:) = 0.0d0

    aA = 0.50d0*norm(cross_product(a_,b_) )
    aB = 0.50d0*norm(cross_product(b_,c_) )
    aC = 0.50d0*norm(cross_product(c_,d_) )
    aD = 0.50d0*norm(cross_product(d_,a_) )

    V=1.0d0/6.0d0*dot_product(cross_product(a_(:),b_(:) ),c_(:) ) 
    r = 3.0d0*V/( aA+aB+aC+aD )
    obj%radius = r

end subroutine
!#########################################################


!#########################################################
!subroutine 
!end subroutine
!#########################################################
end module GeometryClass