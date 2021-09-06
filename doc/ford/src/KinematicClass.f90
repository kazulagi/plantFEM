module KinematicClass
    use, intrinsic :: iso_fortran_env
    use MathClass

    implicit none

contains

! #############################################
function RotationMatrix3D(rotx,roty,rotz,n,angle) result(Rmat)
    real(real64)::Rmat(3,3)
    real(real64),optional,intent(in)::rotx,roty,rotz,n(3),angle
    integer(int32) :: i
    Rmat(:,:)=0.0d0
    do i=1,3
        Rmat(i,i)=1.0d0
    enddo
    
    if(present(rotx) )then
        Rmat(2,2) = cos(rotx)
        Rmat(2,3) = -sin(rotx)
        Rmat(3,2) = sin(rotx)
        Rmat(3,3) = cos(rotx)
    endif

    if(present(rotx) )then
        Rmat(1,1) = cos(rotx)
        Rmat(1,3) = sin(rotx)
        Rmat(3,1) = -sin(rotx)
        Rmat(3,3) = cos(rotx)
    endif

    if(present(rotx) )then
        Rmat(2,2) = cos(rotx)
        Rmat(2,3) = -sin(rotx)
        Rmat(3,2) = sin(rotx)
        Rmat(3,3) = cos(rotx)
    endif

    if(present(n) .and. present(angle) )then
        ! Rodrigues' rotation formula
        Rmat(1,1)=cos(angle)+n(1)*n(1)*(1.0d0 - cos(angle) )
        Rmat(1,2)=n(1)*n(2)*(1.0d0 - cos(angle) )-n(3)*sin(angle)
        Rmat(1,3)=n(1)*n(3)*(1.0d0 + cos(angle) )-n(2)*sin(angle)
        Rmat(2,1)=n(2)*n(1)*(1.0d0 + cos(angle) )-n(3)*sin(angle)
        Rmat(2,2)=cos(angle)+n(2)*n(2)*(1.0d0 - cos(angle) )
        Rmat(2,3)=n(2)*n(3)*(1.0d0 - cos(angle) )-n(1)*sin(angle)
        Rmat(3,1)=n(3)*n(1)*(1.0d0 - cos(angle) )-n(2)*sin(angle)
        Rmat(3,2)=n(3)*n(2)*(1.0d0 + cos(angle) )-n(1)*sin(angle)
        Rmat(3,3)=cos(angle)+n(3)*n(3)*(1.0d0 - cos(angle) )
    endif

end function
! #############################################

! #############################################
function Rotation3D(vector,rotx,roty,rotz,n,angle) result(vec)
    real(real64)::vec(3)
    real(real64),intent(in)::vector(3)
    real(real64),optional,intent(in)::rotx,roty,rotz,n(3),angle

    vec(:) = matmul( RotationMatrix3D(rotx,roty,rotz,n,angle) ,vector)

end function
! #############################################

end module