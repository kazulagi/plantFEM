module FactoryClass
    use FEMDomainClass
    use SoybeanClass
    implicit none

    type :: Factory_

    contains
        ! creator
        procedure,public :: cube => cube_Factory
        procedure,public :: sphere => sphere_Factory

        procedure,public :: soybean => soybean_Factory
        !

        ! editor
        procedure,pass :: move_forall_femdomain_Factory
        procedure,pass :: move_foreach_femdomain_Factory
        procedure,pass :: move_foreach_soybean_Factory
        generic :: move => move_forall_femdomain_Factory, move_foreach_femdomain_Factory,&
            move_foreach_soybean_Factory

        procedure,pass :: rotate_forall_Factory
        procedure,pass :: rotate_foreach_Factory
        generic :: rotate => rotate_forall_Factory, rotate_foreach_Factory

        procedure,pass :: resize_forall_Factory
        procedure,pass :: resize_foreach_Factory
        generic :: resize => resize_forall_Factory, resize_foreach_Factory
        
    end type

    interface operator(//)
        module procedure :: appendFEMDomainVector
    end interface
contains

function cube_Factory(this,division,n) result(cubes)
    class(Factory_),intent(in) :: this
    integer(int32),intent(in) :: division(1:3),n
    type(FEMDomain_) :: cubes(n)
    integer(int32) :: i
    
    !!!$OMP parallel do
    do i=1,n
        call cubes(i)%create("Cube3D",&
                x_num=division(1),&
                y_num=division(2),&
                z_num=division(3) &
            )
    enddo
    !!!$OMP end parallel do
end function
! ##################################################

function Sphere_Factory(this,division,n) result(Spheres)
    class(Factory_),intent(in) :: this
    integer(int32),intent(in) :: division(1:3),n
    type(FEMDomain_) :: Spheres(n)
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,n
        call Spheres(i)%create("Sphere3D",&
                x_num=division(1),&
                y_num=division(2),&
                z_num=division(3) &
            )
    enddo
    !!$OMP end parallel do
end function
! ##################################################
function Soybean_Factory(this,config,n) result(Soybeans)
    class(Factory_),intent(in) :: this
    character(*),intent(in) :: config
    integer(int32),intent(in) :: n
    type(Soybean_) :: Soybeans(n)
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,n
        call Soybeans(i)%create(config=config)
    enddo
    !!$OMP end parallel do
    
end function
! ##################################################
subroutine move_foreach_femdomain_Factory(this,objects,x,y,z) 
    class(Factory_),intent(in) :: this
    type(FEMDomain_),intent(inout) :: objects(:)
    real(real64),intent(in) :: x(:),y(:),z(:)
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,size(objects)
        call objects(i)%move(&
                x=x(i),&
                y=y(i),&
                z=z(i) &
            )
    enddo
    !!$OMP end parallel do
end subroutine

! ##################################################
subroutine move_forall_femdomain_Factory(this,objects,x,y,z) 
    class(Factory_),intent(in) :: this
    type(FEMDomain_),intent(inout) :: objects(:)
    real(real64),intent(in) :: x,y,z
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,size(objects)
        call objects(i)%move(&
                x=x,&
                y=y,&
                z=z &
            )
    enddo
    !!$OMP end parallel do
end subroutine
! ##################################################


! ##################################################
subroutine move_foreach_Soybean_Factory(this,objects,x,y,z) 
    class(Factory_),intent(in) :: this
    type(Soybean_),intent(inout) :: objects(:)
    real(real64),intent(in) :: x(:),y(:),z(:)
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,size(objects)
        call objects(i)%move(&
                x=x(i),&
                y=y(i),&
                z=z(i) &
            )
    enddo
    !!$OMP end parallel do
end subroutine

! ##################################################
subroutine move_forall_Soybean_Factory(this,objects,x,y,z) 
    class(Factory_),intent(in) :: this
    type(Soybean_),intent(inout) :: objects(:)
    real(real64),intent(in) :: x,y,z
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,size(objects)
        call objects(i)%move(&
                x=x,&
                y=y,&
                z=z &
            )
    enddo
    !!$OMP end parallel do
end subroutine
! ##################################################


! ##################################################
subroutine rotate_foreach_Factory(this,objects,x,y,z) 
    class(Factory_),intent(in) :: this
    type(FEMDomain_),intent(inout) :: objects(:)
    real(real64),intent(in) :: x(:),y(:),z(:)
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,size(objects)
        call objects(i)%rotate(&
                x=x(i),&
                y=y(i),&
                z=z(i) &
            )
    enddo
    !!$OMP end parallel do
end subroutine
! ##################################################
subroutine rotate_forall_Factory(this,objects,x,y,z) 
    class(Factory_),intent(in) :: this
    type(FEMDomain_),intent(inout) :: objects(:)
    real(real64),intent(in) :: x,y,z
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,size(objects)
        call objects(i)%rotate(&
                x=x,&
                y=y,&
                z=z &
            )
    enddo
    !!$OMP end parallel do
end subroutine
! ##################################################

! ##################################################
subroutine resize_foreach_Factory(this,objects,x,y,z) 
    class(Factory_),intent(in) :: this
    type(FEMDomain_),intent(inout) :: objects(:)
    real(real64),intent(in) :: x(:),y(:),z(:)
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,size(objects)
        call objects(i)%resize(&
                x=x(i),&
                y=y(i),&
                z=z(i) &
            )
    enddo
    !!$OMP end parallel do
end subroutine
! ##################################################
subroutine resize_forall_Factory(this,objects,x,y,z) 
    class(Factory_),intent(in) :: this
    type(FEMDomain_),intent(inout) :: objects(:)
    real(real64),optional,intent(in) :: x,y,z
    integer(int32) :: i
    
    !!$OMP parallel do
    do i=1,size(objects)
        call objects(i)%resize(&
                x=x,&
                y=y,&
                z=z &
            )
    enddo
    !!$OMP end parallel do
end subroutine
! ##################################################

function appendFEMDomainVector(dv1,dv2) result(ret)
    type(FEMDomain_),intent(in) :: dv1(:),dv2(:)
    type(FEMDomain_),allocatable :: ret(:)

    allocate(ret(size(dv1)+size(dv2) ))
    ret(1:size(dv1) ) = dv1(:)
    ret(size(dv1)+1: ) = dv2(:)

end function

end module
