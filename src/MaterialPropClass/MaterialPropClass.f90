module MaterialPropClass
    use, intrinsic :: iso_fortran_env
    use MeshClass
    implicit none

    type :: MaterialProp_
        type(Mesh_) :: Mesh
        real(real64),allocatable:: meshPara(:,:)
        real(real64) ::x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
        integer(int32) :: Mcount
        integer(int32) :: layer


        real(real64),allocatable::MatPara(:,:)
        integer(int32) :: NumOfMatPara
        integer(int32) :: NumOfMaterial    
        
        character*40 :: MaterialType

        character*70 ErrorMsg
    contains
        procedure :: Init => initializeMaterial
        procedure :: Delete => DeallocateMaterialProp
        procedure :: create => createMaterialProp
        procedure :: set => setMaterialProp
        procedure :: gmsh => gmshMaterialProp
    end type MaterialProp_

contains


! ###########################################################################
subroutine createMaterialProp(obj,Category,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,&
    ParaValue,Layer)
    class(MaterialProp_),intent(inout) :: obj
    character(*),optional,intent(in) :: Category
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    real(real64),optional,intent(in) :: ParaValue
    integer(int32),optional,intent(in) :: Layer
    call obj%set(x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,z_max=z_max,&
        z_min=z_min,t_max=t_max,t_min=t_min,ParaValue=ParaValue,Layer=Layer)
        
end subroutine
! ###########################################################################

! ###########################################################################
subroutine setMaterialProp(obj,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,ParaValue,Layer)
    class(MaterialProp_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    real(real64),optional,intent(in) :: ParaValue
    integer(int32) :: i,j,n,m,valsize
    integer(int32),optional,intent(in) :: Layer
    
    obj%Layer=input(default=1, option=Layer)
    valsize=1
    if(.not. allocated(obj%mesh%NodCoord) )then
        allocate(obj%mesh%NodCoord(8,3))
        allocate(obj%mesh%ElemNod(1,8))
        allocate(obj%meshPara(1,valsize))
        obj%Mcount=1
    endif
    obj%x_max=input(default=10000000.0d0,option=x_max)
    obj%x_min=input(default=-10000000.0d0 ,option=x_min)
    obj%y_max=input(default=10000000.0d0,option=y_max)
    obj%y_min=input(default=-10000000.0d0 ,option=y_min)
    obj%z_max=input(default=10000000.0d0,option=z_max)
    obj%z_min=input(default=-10000000.0d0 ,option=z_min)
    obj%t_max=input(default=10000000.0d0,option=t_max)
    obj%t_min=input(default=0.0d0 ,option=t_min)

    if(obj%Mcount==1 )then
        obj%mesh%NodCoord(1,1) = obj%x_min
        obj%mesh%NodCoord(1,2) = obj%y_min
        obj%mesh%NodCoord(1,3) = obj%z_min

        obj%mesh%NodCoord(2,1) = obj%x_max
        obj%mesh%NodCoord(2,2) = obj%y_min
        obj%mesh%NodCoord(2,3) = obj%z_min

        obj%mesh%NodCoord(3,1) = obj%x_max
        obj%mesh%NodCoord(3,2) = obj%y_max
        obj%mesh%NodCoord(3,3) = obj%z_min

        obj%mesh%NodCoord(4,1) = obj%x_min
        obj%mesh%NodCoord(4,2) = obj%y_max
        obj%mesh%NodCoord(4,3) = obj%z_min

        obj%mesh%NodCoord(5,1) = obj%x_min
        obj%mesh%NodCoord(5,2) = obj%y_min
        obj%mesh%NodCoord(5,3) = obj%z_max

        obj%mesh%NodCoord(6,1) = obj%x_max
        obj%mesh%NodCoord(6,2) = obj%y_min
        obj%mesh%NodCoord(6,3) = obj%z_max

        obj%mesh%NodCoord(7,1) = obj%x_max
        obj%mesh%NodCoord(7,2) = obj%y_max
        obj%mesh%NodCoord(7,3) = obj%z_max

        obj%mesh%NodCoord(8,1) = obj%x_min
        obj%mesh%NodCoord(8,2) = obj%y_max
        obj%mesh%NodCoord(8,3) = obj%z_max


        do i=1,8
            obj%mesh%ElemNod(1,i)=i
        enddo
        obj%meshPara(1,1)=input(default=0.0d0,option=ParaValue)
        !if(present(Values) )then
        !    obj%meshPara(1,:)=values
        !endif
        obj%Mcount=obj%Mcount+1
    else
        n=size(obj%mesh%ElemNod,1)
        do i=1,8
            call extendArray(mat=obj%mesh%NodCoord,extend1stColumn=.true.)
        enddo
        call extendArray(mat=obj%meshPara,extend1stColumn=.true.)
        call extendArray(mat=obj%mesh%ElemNod,extend1stColumn=.true.)
        obj%Mcount=obj%Mcount+1
        obj%mesh%NodCoord( n*8 + 1,1) = obj%x_min
        obj%mesh%NodCoord( n*8 + 1,2) = obj%y_min
        obj%mesh%NodCoord( n*8 + 1,3) = obj%z_min
        
        obj%mesh%NodCoord( n*8 + 2,1) = obj%x_max
        obj%mesh%NodCoord( n*8 + 2,2) = obj%y_min
        obj%mesh%NodCoord( n*8 + 2,3) = obj%z_min

        obj%mesh%NodCoord( n*8 + 3,1) = obj%x_max
        obj%mesh%NodCoord( n*8 + 3,2) = obj%y_max
        obj%mesh%NodCoord( n*8 + 3,3) = obj%z_min

        obj%mesh%NodCoord( n*8 + 4,1) = obj%x_min
        obj%mesh%NodCoord( n*8 + 4,2) = obj%y_max
        obj%mesh%NodCoord( n*8 + 4,3) = obj%z_min

        obj%mesh%NodCoord( n*8 + 5,1) = obj%x_min
        obj%mesh%NodCoord( n*8 + 5,2) = obj%y_min
        obj%mesh%NodCoord( n*8 + 5,3) = obj%z_max

        obj%mesh%NodCoord( n*8 + 6,1) = obj%x_max
        obj%mesh%NodCoord( n*8 + 6,2) = obj%y_min
        obj%mesh%NodCoord( n*8 + 6,3) = obj%z_max

        obj%mesh%NodCoord( n*8 + 7,1) = obj%x_max
        obj%mesh%NodCoord( n*8 + 7,2) = obj%y_max
        obj%mesh%NodCoord( n*8 + 7,3) = obj%z_max

        obj%mesh%NodCoord( n*8 + 8,1) = obj%x_min
        obj%mesh%NodCoord( n*8 + 8,2) = obj%y_max
        obj%mesh%NodCoord( n*8 + 8,3) = obj%z_max
        
        obj%meshPara(1+n,1)=input(default=0.0d0,option=ParaValue)
        do i=1,8
            obj%mesh%ElemNod(1+n,i)=i+n*8
        enddo
        !if(present(Values) )then
        !    obj%meshPara(1+n,:)=values
        !endif


    endif
    
end subroutine setMaterialProp
! ###########################################################################



!###############################################
subroutine gmshMaterialProp(obj, Name, Tag)
    class(MaterialProp_),intent(inout) :: obj
    character(*),optional,intent(in) :: Name,Tag
    real(real64),allocatable :: ElemValue(:,:)
    integer(int32) :: i


    if( obj%Mesh%empty() .eqv. .true. )then
        print *, "ERROR gmshMaterialProp :: No Material. is installed. "
        return
    endif
    allocate(ElemValue(size(obj%MeshPara,1),1 ))
    do i=1,size(obj%MeshPara,2)
        ElemValue(:,1)=obj%MeshPara(:,i)
        if(present(tag) )then
            call obj%Mesh%gmsh(Name=Name//"Material",ElemValue=ElemValue,&
                OptionalContorName=trim(Tag))
        else
            call obj%Mesh%gmsh(Name=Name//"Material",ElemValue=ElemValue,&
                OptionalContorName="Material Value :: ID = "//trim(adjustl(fstring(i))))
        endif
    enddo
    deallocate(ElemValue)
        

end subroutine gmshMaterialProp
!###############################################


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
    real(real64),allocatable,optional,intent(inout) :: MaterialParameters(:,:)
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
    real(real64),intent(in)::mat_para(:,:)
    
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