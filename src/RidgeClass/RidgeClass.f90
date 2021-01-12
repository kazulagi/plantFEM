module RidgeClass
    use sim
    implicit none

    type ::Ridge_
        type(FEMDomain_) :: FEMDomain
    contains
        procedure, public :: create => createRidge
        procedure, public :: show => showRidge
    end type

contains

! ##########################################
subroutine createRidge(obj,x_num,y_num,z_num,x_len,y_len,z_len,top)
    class(Ridge_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_len,y_len,z_len,top
    real(real64)::hmin
    integer(int32),optional,intent(in) :: x_num,y_num,z_num
    call obj%FEMDomain%create(meshtype="Ridge3D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=z_len,thickness=y_len,&
    division=z_num,top=top)
    call obj%FEMDomain%rotate(x=3.14159265350d0/2.0d0)
    hmin=minval(obj%FEMDomain%Mesh%NodCoord(:,3) )
    call obj%FEMDomain%move(z=-hmin)
    hmin=minval(obj%FEMDomain%Mesh%NodCoord(:,2) )
    call obj%FEMDomain%move(y=-hmin)
end subroutine
! ##########################################

! ##########################################
subroutine showRidge(obj)
    class(Ridge_),intent(inout) :: obj
    call obj%FEMDomain%gmsh()
end subroutine showRidge
! ##########################################

end module    