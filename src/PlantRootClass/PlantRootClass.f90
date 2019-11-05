module PlantRootClass

    use KinematicClass
    use FEMDomainClass
    use StemClass
    implicit none


    type :: PlantRoot_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  MaxThickness,Maxlength,Maxwidth
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
        type(PlantRoot_),pointer ::  pRoot
    contains
        procedure, public :: Init => initRoot
        procedure, public :: export => exportRoot
    end type
contains

! ########################################
subroutine initRoot(obj,PlantName,Stage,&
    Thickness,length,width,&
    MaxThickness,Maxlength,Maxwidth,location)
    class(PlantRoot_),intent(inout)::obj
    character(*),intent(in) :: PlantName
    character(2),intent(in) :: Stage
    real(8),optional,intent(in) :: Thickness,length,width,location(3)
    real(8),optional,intent(in) :: MaxThickness,Maxlength,Maxwidth
    real(8) :: loc(3)

    loc(:)=0.0d0
    if(present(location) )then
        loc(:)=location(:)
    endif
    if(trim(PlantName) == "soybean" .or. trim(PlantName) == "Soybean")then
        if(Stage == "VE")then
            

            ! initialize 
            obj%Thickness   = input(default=0.010d0,option= Thickness     )
            obj%length      = input(default=0.050d0,  option= length      )
            obj%width       = input(default=0.010d0,option= width)
        
            obj%MaxThickness   = input(default=0.50d0  ,option=MaxThickness      )
            obj%Maxlength      = input(default=10.0d0  ,option=Maxlength       )
            obj%Maxwidth       = input(default=0.50d0  ,option=Maxwidth )
        
        
            ! set direction of plumule
            obj%outer_normal_bottom(:)=0.0d0
            obj%outer_normal_bottom(1)=-1.0d0
            
            obj%outer_normal_top(:)=0.0d0
            obj%outer_normal_top(1)=-1.0d0
            
            obj%center_bottom(:)=loc(:)
            obj%center_top(:) = obj%center_bottom(:) + obj%length*obj%outer_normal_bottom(:)

            return
        else
            return
        endif
    endif
end subroutine
! ########################################



! ########################################
subroutine exportRoot(obj,FileName,RootID)
    class(PlantRoot_),intent(in)::obj
    character(*),intent(in) :: FileName
    integer,optional,intent(inout) :: RootID
    real(8) :: radius    
    radius=0.50d0*obj%width+0.50d0*obj%Thickness
    open(12,file=FileName)
    write(12,'(A)') "//+"
    write(12,'(A)') 'SetFactory("OpenCASCADE");'
    write(12,*) "Cylinder(",input(default=1,option=RootID),") = {",&
    obj%center_bottom(1),",", obj%center_bottom(2),",", obj%center_bottom(3),",",&
    obj%center_top(1)-obj%center_bottom(1),",", obj%center_top(2)-obj%center_bottom(2),",",&
     obj%center_top(3)-obj%center_bottom(3),",",&
    radius,", 2*Pi};"
    close(12)
    RootID=RootID+1
end subroutine
! ########################################



end module