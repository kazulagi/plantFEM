! updated 2019/1/19
module MaterialPropClass
    implicit none

    type :: MaterialProp_
        real(8),allocatable::MatPara(:,:)
        integer :: NumOfMatPara
        integer :: NumOfMaterial    
        
        character*40 :: MaterialType

        character*70 ErrorMsg
    contains
        procedure :: Init => initializeMaterial
        procedure :: Delete => DeallocateMaterialProp
    end type MaterialProp_

contains


!##################################################
subroutine DeallocateMaterialProp(obj)
    class(MaterialProp_),intent(inout)::obj

    if(allocated(obj%MatPara) )then
        deallocate(obj%MatPara)
    endif
    obj%ErrorMsg = "All objectes in MaterialPropClass are deallocated."

end subroutine DeallocateMaterialProp
!##################################################




!##################################################
subroutine initializeMaterial(obj,MaterialParameters)
    class(MaterialProp_),intent(inout)::obj
    real(8),allocatable,optional,intent(inout) :: MaterialParameters(:,:)
    if(.not.allocated(obj%MatPara))then
        obj%ErrorMsg="ERROR :: MaterialPropClass%Initialize >>.not.allocated(MatPara)"
        print *,"ERROR :: MaterialPropClass%Initialize >>.not.allocated(MatPara)"
    endif
    if(present(MaterialParameters) )then
        if(.not.allocated(MaterialParameters) )then
            print *, "ERROR :: initializeMaterial >> MaterialParameters(:,:) not allocated."
            return
        else
            if(allocated(obj%MatPara) )then
                deallocate(obj%MatPara)
            endif
            obj%NumOfMatPara = size(MaterialParameters,1)
            obj%NumOfMaterial = size(MaterialParameters,2)
            allocate( obj%MatPara(obj%NumOfMatPara,obj%NumOfMaterial  ) )
            obj%MatPara(:,:) = MaterialParameters(:,:)
        endif
        return
    endif

    if(.not.allocated(obj%MatPara) )then
        print *, "ERROR :: initializeMaterial >> please import obj%MatPara"
    endif
    obj%NumOfMatPara  = size(obj%MatPara,1)
    obj%NumOfMaterial = size(obj%MatPara,2)
end subroutine initializeMaterial
!##################################################



!##################################################
subroutine ImportMatPara(obj,mat_para)
    class(MaterialProp_),intent(inout)::obj
    real(8),intent(in)::mat_para(:,:)
    
    include "./ImportMatPara.f90"

end subroutine ImportMatPara
!##################################################


!##################################################
subroutine MergeMaterialProp(inobj1,inobj2,outobj)
    class(MaterialProp_),intent(in)::inobj1,inobj2
    class(MaterialProp_),intent(out)::outobj

    include "./MergeMaterialProp.f90"

end subroutine MergeMaterialProp
!##################################################



!##################################################
subroutine ShowMatPara(obj)
    class(MaterialProp_),intent(in)::obj

    include "./ShowMatPara.f90"

end subroutine ShowMatPara
!##################################################


end module MaterialPropClass