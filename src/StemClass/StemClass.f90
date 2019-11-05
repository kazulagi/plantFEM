module StemClass
    use KinematicClass
    use FEMDomainClass
    implicit none
    
    type :: Stem_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  MaxThickness,Maxlength,Maxwidth
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initStem
        procedure, public :: export => exportStem
    end type
contains



! ########################################
subroutine initStem(obj,Thickness,length,width,MaxThickness,Maxlength,Maxwidth,rotx,roty,rotz,location)
    class(Stem_),intent(inout) :: obj
    real(8),optional,intent(in)::  Thickness,length,width
    real(8),optional,intent(in)::  MaxThickness,Maxlength,MaxWidth
    real(8),optional,intent(in)::  rotx,roty,rotz,location(3)
    real(8) :: loc(3)
    loc(:)=0.0d0
    if(present(location) )then
        loc(:)=location(:)
    endif

    obj%Thickness   = input(default=0.010d0,option= Thickness     )
    obj%length      = input(default=0.050d0,option= length      )
    obj%width       = input(default=0.010d0,option= width)

    obj%MaxThickness   = input(default=0.50d0  ,option=MaxThickness      )
    obj%Maxlength      = input(default=10.0d0  ,option=Maxlength       )
    obj%Maxwidth       = input(default=0.50d0  ,option=Maxwidth )

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
subroutine exportStem(obj,FileName,StemID)
    class(Stem_),intent(in)::obj
    character(*),intent(in) :: FileName
    integer,optional,intent(inout) :: StemID
    real(8) :: radius

    
    radius=0.50d0*obj%width+0.50d0*obj%Thickness
    
    open(13,file=FileName)
    write(13,'(A)') "//+"
    write(13,'(A)') 'SetFactory("OpenCASCADE");'
    write(13,*) "Cylinder(",input(default=1,option=StemID),") = {",&
    obj%center_bottom(1),",", obj%center_bottom(2),",", obj%center_bottom(3),",",&
    obj%center_top(1)-obj%center_bottom(1),",", obj%center_top(2)-obj%center_bottom(2),",",&
     obj%center_top(3)-obj%center_bottom(3),",",&
    radius,", 2*Pi};"
    close(13)
    StemID=StemID+1

end subroutine
! ########################################


end module