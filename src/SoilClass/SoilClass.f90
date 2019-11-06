module SoilClass
    use SiCroF
    implicit none

    type :: Soil_
        type(FEMDomain_) :: FEMDomain

        real(8) :: depth
        real(8) :: length
        real(8) :: width
        real(8) :: x,y,z ! center coordinate
    contains
        procedure :: init => initSoil
        procedure :: export => exportSoil
    end type

contains

! ################################################################
subroutine initSoil(obj,depth,length,width,x,y,z)
    class(Soil_),intent(inout)::obj
    real(8),optional,intent(in):: depth,length,width,x,y,z ! cm

    obj%depth = input(default=-1.0d0,option=depth )
    obj%length= input(default=1.0d0,option=length)
    obj%width = input(default=1.0d0,option=width )
    obj%x     = input(default=0.0d0,option=x )
    obj%y     = input(default=0.0d0,option=y)
    obj%z     = input(default=0.0d0,option=z )

end subroutine
! ################################################################


! ################################################################
subroutine exportSoil(obj,FileName,format,objID)
    class(Soil_),intent(inout)::obj
    integer,optional,intent(inout) :: objID
    character(*),intent(in)::FileName
    character(*),optional,intent(in) :: format

    if(present(format) )then
        if(format==".geo" .or. format=="geo" )then
            open(15,file=FileName)
            write(15,'(A)') "//+"
            write(15,'(A)') 'SetFactory("OpenCASCADE");'
            write(15,*) "Box(",input(default=1,option=objID),") = {",&
            obj%x   ,",", obj%y  ,",", obj%z ,   ",",&
            obj%width                ,",", obj%length               ,",", obj%depth ,"};"
            close(15)
            objID=objID+1
        endif
    endif
end subroutine
! ################################################################

end module