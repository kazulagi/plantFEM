module GeometryClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    use ArrayClass
    implicit none

    type Point_
        real(real64),allocatable :: coord(:)
        character*30 :: name
    contains
        procedure :: Init   => InitPoint
        procedure :: set    => setPoint
        procedure :: show   => showPoint
    end type

    type Line_
        real(real64),allocatable :: coord(:,:)
    contains
        procedure :: Init       => InitLine
        procedure :: setNode    => SetNodeLine
        procedure :: import     => importLine
        procedure :: show       => showLine 
    end type

    type Circle_
        real(real64) :: radius
        real(real64),allocatable :: center(:)
    contains
        procedure :: Init       => InitCircle     
        procedure :: SetCenter  => InitSetCenterCircle     
        procedure :: SetRadius  => InitSetRadiusCircle   
        procedure :: getArea    => getAreaCircle
        procedure :: show       => showCircle
    end type

    type Sphere_
        real(real64) :: radius
        real(real64) :: center(3)
    contains
        procedure :: Init       => InitSphere     
        procedure :: SetCenter  => InitSetCenterSphere     
        procedure :: SetRadius  => InitSetRadiusSphere 
        procedure :: show       => showSphere
    end type


    type Triangle_
        real(real64),allocatable :: NodCoord(:,:)
        real(real64),allocatable :: OuterNormal(:)
        real(real64),allocatable :: Center(:)
    contains
        procedure :: Init       => InitTriangle
        procedure :: setNode    => setNodeTriangle
        procedure :: import     => importTriangle
        procedure :: getCircle  => getCircleTriangle
        procedure :: getArea    => getAreaTriangle 
        procedure :: show       => showTriangle
        procedure :: GetOuterNormal => GetOuterNormalTriangle
    end type


    type Rectangle_
        real(real64),allocatable :: NodCoord(:,:)
    contains
        procedure :: Init       => InitRectangle
        procedure :: create     => createRectangle
        procedure :: move       => moveRectangle
        procedure :: setNode    => setNodeRectangle
        procedure :: import     => importRectangle
        procedure :: getCircle  => getCircleRectangle
        procedure :: getArea    => getAreaRectangle 
        procedure :: show       => showRectangle
        procedure :: contact   => contactRectangle
    end type

    type Tetragon_
        real(real64),allocatable :: NodCoord(:,:)
    end type

    type Tetrahedron_
        real(real64) :: NodCoord(4,3)
        real(real64) :: radius
        real(real64) :: center(3)
    contains
        procedure :: Init => InitTetrahedron
        procedure :: getCircle => getCircleTetrahedron
    end type
    
    type Octahedron_
        real(real64),allocatable :: NodCoord(:,:)
    end type
contains

!#########################################################
subroutine InitPoint(obj,dim)
    class(Point_),intent(inout)::obj
    integer(int32),optional,intent(in)::dim

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
    real(real64),optional,intent(in)::x,y,z
    real(real64),optional,intent(in)::xvec(:)
    integer(int32) :: n

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
    integer(int32),optional,intent(inout)::dim

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
    integer(int32),intent(in)::position

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
    integer(int32),intent(in)::NodCoord(:,:)

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
    integer(int32),optional,intent(inout)::dim

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
    real(real64),intent(in)::radius

    obj%radius=radius
    
end subroutine
!#########################################################



!#########################################################
function getAreaCircle(obj) result(area)
    class(Circle_),intent(inout)::obj
    real(real64) :: area,pi
    pi=3.14159260d0

    area=obj%radius*obj%radius*pi

end function
!#########################################################



!#########################################################
subroutine showCircle(obj,Name)
    class(Circle_),intent(in)::obj
    character(*),optional,intent(in)::Name
    real(real64) :: angle,dtheta,pi
    integer(int32) :: i

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
    integer(int32),optional,intent(inout)::dim

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
    real(real64),intent(in)::radius

    obj%radius=radius
    
end subroutine
!#########################################################


!#########################################################
subroutine showSphere(obj,Name)
    class(Sphere_),intent(in)::obj
    character(*),optional,intent(in)::Name
    real(real64) :: angle1,angle2,dtheta,pi
    integer(int32) :: i,j

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
    integer(int32),optional,intent(in)::dim


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
    integer(int32),intent(in)::order

    if( size(obj%NodCoord,2)/=size(point%coord) )then
        print *, "ERROR ::InitSetNodeTriangle ::  ( size(obj%NodCoord,2)/=size(point%coord)  )"
        return
    endif
    obj%NodCoord(order,:)=point%coord(:)
end subroutine
!#########################################################



!#########################################################
subroutine importTriangle(obj,NodCoord,FileName)
    class(Triangle_),intent(inout)::obj
    integer(int32),optional,intent(in)::NodCoord(:,:)

    integer(int32) :: n,i
    character(*),optional,intent(in)::FileName

    if(present(FileName) )then
        open(30,file=trim(FileName))
        read(30,*) n 
        call obj%init(dim=n)
        do i=1,size(obj%NodCoord,1)
            read(30,*) obj%NodCoord(i,:)
        enddo
        close(30)
        print *, "Imported triangle from ",trim(FileName)
        return
    endif

    if(present(NodCoord) )then
    
        if(size(obj%NodCoord,2)/=size(NodCoord,2) )then
            print *, "ERROR :: importTriangle :: size(obj%NodCoord,2)/=size(NodCoord,2)"
            return
        endif
        obj%NodCoord(1:3,:)=NodCoord(1:3,:)
    endif

end subroutine
!#########################################################


!#########################################################
subroutine getCircleTriangle(obj,type_of_circle,circle) 
    class(Triangle_),intent(in)::obj
    type(Circle_),intent(inout) :: circle
    character(*),intent(in) :: type_of_circle
    real(real64) :: x1,x2,x3,y1,y2,y3
    real(real64),allocatable :: a(:),b(:),c(:)
    integer(int32) :: i,n

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
    real(real64) :: area
    real(real64) :: x1,x2,x3,y1,y2,y3
    real(real64) :: a(3),b(3),c(3)

    if(size(obj%NodCoord,2)==2 )then

        x1 = obj%NodCoord( 1,1 )
        y1 = obj%NodCoord( 1,2 )
        x2 = obj%NodCoord( 2,1 )
        y2 = obj%NodCoord( 2,2 )
        x3 = obj%NodCoord( 3,1 )
        y3 = obj%NodCoord( 3,2 )
        area = abs(0.50d0*( x1*y2 + x2*y3 + x3*y1 - y1*x2 -y2*x3 - y3*x1 ))

    elseif( size(obj%NodCoord,2)==3 )then
        a(:)=obj%NodCoord(1,:)
        b(:)=obj%NodCoord(2,:)
        c(:)=obj%NodCoord(3,:)
        a(:)=a(:)-c(:)
        b(:)=b(:)-c(:)
        area=(a(1)*a(1) + a(2)*a(2) + a(3)*a(3))*(b(1)*b(1) + b(2)*b(2) + b(3)*b(3))
        area=area-(a(1)*b(1)+a(2)*b(2)+a(3)*b(3))*(a(1)*b(1)+a(2)*b(2)+a(3)*b(3))
        area=0.50d0*area
        area=sqrt(area)
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
subroutine GetOuterNormalTriangle(obj)
    class(Triangle_),intent(inout) :: obj
    real(real64) :: x1(3),x2(3),x3(3),on(3)
    integer(int32) :: n

    if(.not.allocated(obj%OuterNormal) )then
        n=size(obj%NodCoord,2)
        allocate(obj%OuterNormal(n) )
        obj%OuterNormal(:)=0.0d0
    endif

    x1(:)=0.0d0
    x2(:)=0.0d0
    x1(:)=obj%NodCoord(2,1:n)-obj%NodCoord(1,1:n)
    x2(:)=obj%NodCoord(3,1:n)-obj%NodCoord(1,1:n)
    on(:)=1.0d0/norm(cross_product(x1,x2)) *cross_product(x1,x2)
    obj%OuterNormal(1:n)=on(1:n)

    x1(1:n) = obj%NodCoord(1,1:n) 
    x2(1:n) = obj%NodCoord(2,1:n) 
    x3(1:n) = obj%NodCoord(3,1:n)
    if(allocated(obj%center) ) then
        deallocate(obj%center)
    endif
    allocate(obj%center(n) )
    obj%center(:)=0.0d0
    obj%center(1:n)=obj%center(1:n)+x1(1:n)
    obj%center(1:n)=obj%center(1:n)+x2(1:n)
    obj%center(1:n)=obj%center(1:n)+x3(1:n)
    obj%center(1:n)=1.0d0/3.0d0*obj%center(1:n)
    
end subroutine
!#########################################################



!#########################################################
subroutine InitRectangle(obj,dim)
    class(Rectangle_),intent(inout)::obj
    integer(int32),optional,intent(in)::dim


    if(allocated(obj%NodCoord) )then
        deallocate(obj%NodCoord)
    endif
    allocate(obj%NodCoord(4,input(default=3,option=dim) ) )
    obj%NodCoord(:,:)=0.0d0

end subroutine
!#########################################################

!#########################################################
subroutine createRectangle(obj)
    class(Rectangle_),intent(inout) :: obj

    ! create unit one
    allocate(obj%NodCoord(8,3) )
    obj%NodCoord(1,1)=-1.0d0; obj%NodCoord(1,2)=-1.0d0; obj%NodCoord(1,3)=-1.0d0;
    obj%NodCoord(2,1)= 1.0d0; obj%NodCoord(2,2)=-1.0d0; obj%NodCoord(2,3)=-1.0d0;
    obj%NodCoord(3,1)= 1.0d0; obj%NodCoord(3,2)= 1.0d0; obj%NodCoord(3,3)=-1.0d0;
    obj%NodCoord(4,1)=-1.0d0; obj%NodCoord(4,2)= 1.0d0; obj%NodCoord(4,3)=-1.0d0;
    obj%NodCoord(5,1)=-1.0d0; obj%NodCoord(5,2)=-1.0d0; obj%NodCoord(5,3)= 1.0d0;
    obj%NodCoord(6,1)= 1.0d0; obj%NodCoord(6,2)=-1.0d0; obj%NodCoord(6,3)= 1.0d0;
    obj%NodCoord(7,1)= 1.0d0; obj%NodCoord(7,2)= 1.0d0; obj%NodCoord(7,3)= 1.0d0;
    obj%NodCoord(8,1)=-1.0d0; obj%NodCoord(8,2)= 1.0d0; obj%NodCoord(8,3)= 1.0d0;
end subroutine createRectangle
!#########################################################


!#########################################################
subroutine moveRectangle(obj,x,y,z)
    class(Rectangle_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x, y, z

    obj%NodCoord(:,1)=obj%NodCoord(:,1)+input(default=0.0d0,option=x)
    obj%NodCoord(:,2)=obj%NodCoord(:,2)+input(default=0.0d0,option=y)
    obj%NodCoord(:,3)=obj%NodCoord(:,3)+input(default=0.0d0,option=z)

end subroutine moveRectangle
!#########################################################

!#########################################################
subroutine setNodeRectangle(obj,point,order)
    class(Rectangle_),intent(inout)::obj
    class(Point_),intent(in)::point
    integer(int32),intent(in)::order

    if( size(obj%NodCoord,2)/=size(point%coord) )then
        print *, "ERROR ::InitSetNodeRectangle ::  ( size(obj%NodCoord,2)/=size(point%coord)  )"
        return
    endif
    obj%NodCoord(order,:)=point%coord(:)
end subroutine
!#########################################################



!#########################################################
subroutine importRectangle(obj,NodCoord,FileName)
    class(Rectangle_),intent(inout)::obj
    integer(int32),optional,intent(in)::NodCoord(:,:)
    integer(int32) :: n,i
    character(*),optional,intent(in)::FileName

    if(present(FileName) )then
        open(30,file=trim(FileName))
        read(30,*) n 
        call obj%init(dim=n)
        do i=1,size(obj%NodCoord,1)
            read(30,*) obj%NodCoord(i,:)
        enddo
        close(30)
        print *, "Imported rectangle from ",trim(FileName)
        return
    endif


    if(present(NodCoord) )then
        if(size(obj%NodCoord,2)/=size(NodCoord,2) )then
            print *, "ERROR :: importRectangle :: size(obj%NodCoord,2)/=size(NodCoord,2)"
            return
        endif
        obj%NodCoord(1:4,:)=NodCoord(1:4,:)
    endif
end subroutine
!#########################################################


!#########################################################
subroutine getCircleRectangle(obj,type_of_circle,circle) 
    class(Rectangle_),intent(in)::obj
    type(Circle_),intent(inout) :: circle
    character(*),intent(in) :: type_of_circle
    real(real64) :: x1,x2,x3,x4,y1,y2,y3,y4
    real(real64),allocatable :: a(:),b(:),c(:)
    integer(int32) :: i,n

    n=size(obj%NodCoord,2)
    call initCircle(circle,dim=n)

    if(type_of_circle == "center_of_gravity")then
        
        do i=1,4
            circle%center(:)=circle%center(:)+1.0d0/4.0d0*obj%NodCoord(i,:)
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
        x4 = obj%NodCoord( 4,1 )
        y4 = obj%NodCoord( 4,2 )

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
        print *, "ERROR :: getCircleRectangle :: type_of_circle/excenter is now being implemented"
    elseif( type_of_circle == "orthocenter"  )then
        print *, "ERROR :: getCircleRectangle :: type_of_circle/orthocenter is now being implemented"
    endif

end subroutine
!#########################################################


!#########################################################
function getAreaRectangle(obj) result(area)
    class(Rectangle_),intent(in)::obj
    real(real64) :: area
    real(real64) :: x1,x2,x3,x4,y1,y2,y3,y4

    if(size(obj%NodCoord,2)==2 )then

        x1 = obj%NodCoord( 1,1 )
        y1 = obj%NodCoord( 1,2 )
        x2 = obj%NodCoord( 2,1 )
        y2 = obj%NodCoord( 2,2 )
        x3 = obj%NodCoord( 3,1 )
        y3 = obj%NodCoord( 3,2 )
        area = abs(0.50d0*( x1*y2 + x2*y3 + x3*y1 - y1*x2 -y2*x3 - y3*x1 ))


        x2 = obj%NodCoord( 1,1 )
        y2 = obj%NodCoord( 1,2 )
        x3 = obj%NodCoord( 2,1 )
        y3 = obj%NodCoord( 2,2 )
        x4 = obj%NodCoord( 3,1 )
        y4 = obj%NodCoord( 3,2 )
        area = area+abs(0.50d0*( x1*y2 + x2*y3 + x3*y1 - y1*x2 -y2*x3 - y3*x1 ))


    elseif( size(obj%NodCoord,2)==3 )then
        print *, " getAreaRectangle >> not ready for 3D"
        stop 
    else
        print *, "ERROR :: getAreaRectangle, size(obj%NodCoord,2)==2 "
        return
    endif

end function
!#########################################################



!#########################################################
subroutine showRectangle(obj,Name,option)
    class(Rectangle_),intent(in)::obj
    character(*),optional,intent(in)::Name,option

    if(present(Name) )then
        if(present(option) )then
            open(10,file=Name)
            write(10,*) obj%NodCoord(1,:)
            write(10,*) obj%NodCoord(2,:)
            write(10,*) obj%NodCoord(3,:)
            write(10,*) obj%NodCoord(4,:)
            write(10,*) obj%NodCoord(1,:)
            close(10)
        else
            open(10,file=Name)
            write(10,*) obj%NodCoord(1,:),obj%NodCoord(2,:)-obj%NodCoord(1,:)
            write(10,*) obj%NodCoord(2,:),obj%NodCoord(3,:)-obj%NodCoord(2,:)
            write(10,*) obj%NodCoord(3,:),obj%NodCoord(4,:)-obj%NodCoord(3,:)
            write(10,*) obj%NodCoord(4,:),obj%NodCoord(1,:)-obj%NodCoord(4,:)
            close(10)
        endif
    else
        print *, "Rectangle> x1(:) = ",obj%NodCoord(1,:)
        print *, "Rectangle> x2(:) = ",obj%NodCoord(2,:)
        print *, "Rectangle> x3(:) = ",obj%NodCoord(3,:)
        print *, "Rectangle> x3(:) = ",obj%NodCoord(4,:)
    endif
end subroutine
!#########################################################


!#########################################################
function contactRectangle(obj,Rectangle,threshold) result(contact)
    class(Rectangle_),intent(in) :: obj
    class(Rectangle_),intent(in) :: Rectangle
    integer(int32),optional,intent(in) :: threshold
    logical :: contact, inside
    integer(int32) :: i,j,k,n,m,dim,inside_score,th
    real(real64)::dist1,dist2,dist3,x2d(2),x3d(3),x_max(3),x_min(3)

    th=input(default=1,option=threshold)

    ! Contact Rectangle to Rectangle
    dim = size(obj%NodCoord,2)
    if(dim==3)then
        ! get range
        do i=1,dim
            x_max(i)=maxval(Rectangle%NodCoord(:,i))
            x_min(i)=minval(Rectangle%NodCoord(:,i))
        enddo
        inside_score=0
        inside=.false.
        do i=1,size(obj%NodCoord,1)
            x3d(1:3)=obj%NodCoord(i,1:3)
            inside=InOrOut(x=x3d,xmax=x_max,xmin=x_min,DimNum=dim)
            if(inside .eqv. .true.)then
                inside_score=inside_score+1
            endif
        enddo
        if(inside_score >= th)then
            contact=.true.
        else
            contact=.false.
        endif
    else
        print *, "Please implement contactRectangle for",dim,"D"
        stop 
    endif
    

end function contactRectangle
!#########################################################


!#########################################################
subroutine InitTetrahedron(obj,NodCoord)
    class(Tetrahedron_),intent(inout) :: obj
    real(real64),intent(in) :: NodCoord(4,3)

    obj%NodCoord(:,:)=NodCoord(:,:)

end subroutine
!#########################################################

!#########################################################
subroutine getCircleTetrahedron(obj)
    class(Tetrahedron_),intent(inout) :: obj
    real(real64) :: a(3),b(3),c(3),d(3),e(3),f(3),g(3),s,t,r,N(3)
    real(real64) :: a_(3),b_(3),c_(3),d_(3),aA,aB,aC,aD,V,k
    
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