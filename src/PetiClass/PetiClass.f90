module PetiClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use StemClass
    use FEMDomainClass

    implicit none

    type :: Peti_
        type(FEMDomain_)    ::  FEMDomain
        real(real64)             ::  Thickness,length,width
        real(real64)             ::  MaxThickness,Maxlength,Maxwidth
        real(real64)             ::  center_bottom(3),center_top(3)
        real(real64)             ::  radius_bottom(3),radius_top(3)
        real(real64)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer(int32)             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initPeti
        procedure, public :: export => exportPeti
    end type
contains


! ########################################
subroutine initPeti(obj,Thickness,length,width,MaxThickness,Maxlength,Maxwidth,rotx,roty,rotz,location)
    class(Peti_),intent(inout) :: obj
    real(real64),optional,intent(in)::  Thickness,length,width
    real(real64),optional,intent(in)::  MaxThickness,Maxlength,Maxwidth
    real(real64),optional,intent(in)::  rotx,roty,rotz,location(3)
    real(real64) :: loc(3)
    loc(:)=0.0d0
    if(present(location) )then
        loc(:)=location(:)
    endif
    obj%Thickness   = input(default=0.010d0,option= Thickness     )
    obj%length      = input(default=0.050d0,option= length      )
    obj%width       = input(default=0.010d0,option= width)

    obj%MaxThickness   = input(default=0.50d0  ,option= MaxThickness     )
    obj%Maxlength      = input(default=10.0d0  ,option= Maxlength      )
    obj%Maxwidth       = input(default=0.50d0  ,option= Maxwidth)

    obj%outer_normal_bottom(:)=0.0d0
    obj%outer_normal_bottom(1)=1.0d0
    obj%outer_normal_top(:)=0.0d0
    obj%outer_normal_top(1)=1.0d0
    ! rotate
    obj%outer_normal_Bottom(:) = Rotation3D(vector=obj%outer_normal_bottom,rotx=rotx,roty=roty,rotz=rotz)
    obj%outer_normal_top(:) = Rotation3D(vector=obj%outer_normal_top,rotx=rotx,roty=roty,rotz=rotz)

    obj%center_bottom(:)=loc(:)
    obj%center_top(:) = obj%center_bottom(:) + obj%length*obj%outer_normal_bottom(:)


end subroutine 
! ########################################


! ########################################
subroutine exportPeti(obj,FileName,PetiID)
    class(Peti_),intent(in)::obj
    character(*),intent(in) :: FileName
    integer(int32),optional,intent(inout) :: PetiID
    real(real64) :: radius
    
    radius=0.50d0*obj%width+0.50d0*obj%Thickness
    open(14,file=FileName)
    write(14,'(A)') "//+"
    write(14,'(A)') 'SetFactory("OpenCASCADE");'
    write(14,*) "Cylinder(",input(default=1,option=PetiID),") = {",&
    obj%center_bottom(1),",", obj%center_bottom(2),",", obj%center_bottom(3),",",&
    obj%center_top(1)-obj%center_bottom(1),",", obj%center_top(2)-obj%center_bottom(2),",",&
     obj%center_top(3)-obj%center_bottom(3),",",&
    radius,", 2*Pi};"
    close(14)
    PetiID=petiID+1


end subroutine
! ########################################




end module