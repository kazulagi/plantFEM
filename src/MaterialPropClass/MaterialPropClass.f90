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
        character(200) :: Name= " "


        real(real64),allocatable::MatPara(:,:)
        integer(int32) :: NumOfMatPara
        integer(int32) :: NumOfMaterial    
        
        character*40 :: MaterialType= " "

        character*70 :: ErrorMsg= " "
    contains
        procedure :: Init => initializeMaterial
        procedure :: export => exportMaterialProp
        procedure :: save => saveMaterialProp
        procedure :: Delete => DeallocateMaterialProp
        procedure :: create => createMaterialProp
        procedure :: set => setMaterialProp
        procedure :: gmsh => gmshMaterialProp
        procedure :: show => showMaterialProp
        procedure :: getValues => getValuesMaterialProp
        procedure :: remove => removeMaterialProp
        procedure :: open => openMaterialProp
        
    end type MaterialProp_

contains

! ###########################################################################
subroutine openMaterialProp(obj,path,name)
    class(MaterialProp_),intent(inout)::obj
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f

    if(present(name) )then
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","Material",".prop")
        call obj%Mesh%open(path=trim(path)//"/"//trim(adjustl(name)),name="Mesh")
        call openArray(f%fh,obj%meshPara)
        read(f%fh,*) obj%x_max
        read(f%fh,*) obj%x_min
        read(f%fh,*) obj%y_max
        read(f%fh,*) obj%y_min
        read(f%fh,*) obj%z_max
        read(f%fh,*) obj%z_min
        read(f%fh,*) obj%t_max
        read(f%fh,*) obj%t_min
        read(f%fh,*) obj%Mcount
        read(f%fh,*) obj%layer
        read(f%fh, '(A)' ) obj%Name
        call openArray(f%fh, obj%MatPara)
        read(f%fh,*) obj%NumOfMatPara
        read(f%fh,*) obj%NumOfMaterial    
        read(f%fh, '(A)') obj%MaterialType
        read(f%fh, '(A)') obj%ErrorMsg
        call f%close()
    else
        call execute_command_line("mkdir -p "//trim(path)//"/Material")
        call f%open(trim(path)//"/Material/","Material",".prop")
        call obj%Mesh%open(path=trim(path)//"/"//"Material",name="Mesh")
        call openArray(f%fh,obj%meshPara)
        read(f%fh,*) obj%x_max
        read(f%fh,*) obj%x_min
        read(f%fh,*) obj%y_max
        read(f%fh,*) obj%y_min
        read(f%fh,*) obj%z_max
        read(f%fh,*) obj%z_min
        read(f%fh,*) obj%t_max
        read(f%fh,*) obj%t_min
        read(f%fh,*) obj%Mcount
        read(f%fh,*) obj%layer
        read(f%fh, '(A)' ) obj%Name
        call openArray(f%fh, obj%MatPara)
        read(f%fh,*) obj%NumOfMatPara
        read(f%fh,*) obj%NumOfMaterial    
        read(f%fh, '(A)') obj%MaterialType
        read(f%fh, '(A)') obj%ErrorMsg
        call f%close()
    endif



end subroutine
! ###########################################################################

! ###########################################################################
subroutine removeMaterialProp(obj)
    class(MaterialProp_),intent(inout) :: obj

    call obj%Mesh%remove()
    if(allocated( obj%meshPara))then
        deallocate(obj%meshPara)
    endif

    obj%x_max=0.0d0
    obj%x_min=0.0d0
    obj%y_max=0.0d0
    obj%y_min=0.0d0
    obj%z_max=0.0d0
    obj%z_min=0.0d0
    obj%t_max=0.0d0
    obj%t_min=0.0d0
    obj%Mcount=1
    obj%layer=1
    obj%Name=" "


    if(allocated(obj%MatPara))then
        deallocate(obj%MatPara)
    endif

    obj%NumOfMatPara=1
    obj%NumOfMaterial    =1
    
    obj%MaterialType=" "
    obj%ErrorMsg=" "

end subroutine
! ###########################################################################

! ###########################################################################
subroutine createMaterialProp(obj,Name,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,&
    ParaValue,Layer)
    class(MaterialProp_),intent(inout) :: obj
    character(*),optional,intent(in) :: Name
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    real(real64),optional,intent(in) :: ParaValue
    integer(int32),optional,intent(in) :: Layer
    if(present(Name) )then
        obj%Name=trim(adjustl(name))
    else
        obj%Name="NoName"
    endif
    call obj%set(x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,z_max=z_max,&
        z_min=z_min,t_max=t_max,t_min=t_min,ParaValue=ParaValue,Layer=Layer)
        
end subroutine
! ###########################################################################


! ###########################################################################
subroutine saveMaterialProp(obj,path,name)
    class(MaterialProp_),intent(inout)::obj
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f

    if(present(name) )then
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)))
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","Material",".prop")
        call obj%Mesh%save(path=trim(path)//"/"//trim(adjustl(name)),name="Mesh")
        call writeArray(f%fh, obj%meshPara)
        write(f%fh,*) obj%x_max
        write(f%fh,*) obj%x_min
        write(f%fh,*) obj%y_max
        write(f%fh,*) obj%y_min
        write(f%fh,*) obj%z_max
        write(f%fh,*) obj%z_min
        write(f%fh,*) obj%t_max
        write(f%fh,*) obj%t_min
        write(f%fh,*) obj%Mcount
        write(f%fh,*) obj%layer
        write(f%fh, '(A)' ) trim(obj%Name)
        call writeArray(f%fh, obj%MatPara)
        write(f%fh,*) obj%NumOfMatPara
        write(f%fh,*) obj%NumOfMaterial    
        write(f%fh, '(A)') trim(obj%MaterialType)
        write(f%fh, '(A)') trim(obj%ErrorMsg)
        call f%close()
    else
        call execute_command_line("mkdir -p "//trim(path)//"/Material")
        call f%open(trim(path)//"/Material/","Material",".prop")
        call obj%Mesh%save(path=trim(path)//"/"//"Material",name="Mesh")
        call writeArray(f%fh, obj%meshPara)
        write(f%fh,*) obj%x_max
        write(f%fh,*) obj%x_min
        write(f%fh,*) obj%y_max
        write(f%fh,*) obj%y_min
        write(f%fh,*) obj%z_max
        write(f%fh,*) obj%z_min
        write(f%fh,*) obj%t_max
        write(f%fh,*) obj%t_min
        write(f%fh,*) obj%Mcount
        write(f%fh,*) obj%layer
        write(f%fh, '(A)' ) trim(obj%Name)
        call writeArray(f%fh, obj%MatPara)
        write(f%fh,*) obj%NumOfMatPara
        write(f%fh,*) obj%NumOfMaterial    
        write(f%fh, '(A)') trim(obj%MaterialType)
        write(f%fh, '(A)') trim(obj%ErrorMsg)
        call f%close()
    endif

end subroutine
! ###########################################################################


! ###########################################################################
subroutine exportMaterialProp(obj,restart,path)
    class(MaterialProp_),intent(inout)::obj
    logical,optional,intent(in) :: restart
    character(*),intent(in) :: path
    type(IO_) :: f

    if(present(restart) )then
        call execute_command_line("mkdir -p "//trim(path)//"/Material")
        call f%open(trim(path)//"/Material/","Material",".prop")
        call obj%Mesh%export(path=trim(path)//"/Material",restart=.true.)
        write(f%fh,*) obj%meshPara(:,:)
        write(f%fh,*) obj%x_max
        write(f%fh,*) obj%x_min
        write(f%fh,*) obj%y_max
        write(f%fh,*) obj%y_min
        write(f%fh,*) obj%z_max
        write(f%fh,*) obj%z_min
        write(f%fh,*) obj%t_max
        write(f%fh,*) obj%t_min
        write(f%fh,*) obj%Mcount
        write(f%fh,*) obj%layer
        write(f%fh, '(A)' ) trim(obj%Name)
        write(f%fh,*) obj%MatPara(:,:)
        write(f%fh,*) obj%NumOfMatPara
        write(f%fh,*) obj%NumOfMaterial    
        write(f%fh, '(A)') trim(obj%MaterialType)
        write(f%fh, '(A)') trim(obj%ErrorMsg)
        call f%close()
    endif

end subroutine
! ###########################################################################


! ###########################################################################
subroutine showMaterialProp(obj)
    class(MaterialProp_),intent(inout) :: obj
    integer(int32) :: i,j,n

    n=size(obj%Mesh%ElemNod,1)

    print *, trim(obj%Name)
    do i=1,n
        if(i==n)then
            print *, "  L _ Zone #",i,"Parameters : ",obj%meshPara(i,:)
        else
            print *, "  | - Zone #",i,"Parameters : ",obj%meshPara(i,:)
        endif
    enddo

end subroutine showMaterialProp
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
    
    if(allocated(obj%MatPara) )then
        deallocate(obj%MatPara)
    endif
    allocate(obj%MatPara(size(mat_para,1),size(mat_para,2) ) )
    obj%MatPara(:,:)=mat_para(:,:)

    obj%NumOfMatPara=size(mat_para,1)
    obj%NumOfMaterial=size(mat_para,2)

end subroutine ImportMatPara
!##################################################


!##################################################
subroutine MergeMaterialProp(inobj1,inobj2,outobj)
    class(MaterialProp_),intent(in)::inobj1,inobj2
    class(MaterialProp_),intent(out)::outobj
    integer num1,num2,num3,i


    ! ========= Merge nodes  ============
    num1=size(inobj1%MatPara,1)
    num2=size(inobj2%MatPara,1)
    num3=size(inobj2%MatPara,2)
    if(num3 /= size(inobj1%MatPara,1) )then
        outobj%ErrorMsg="MergeMaterialProp >> num3 /= inobj1%MatPara,1"
    endif
    allocate(outobj%MatPara(num1+num2, num3))
    do i=1,num1
        outobj%MatPara(i,:)=inobj1%MatPara(i,:)
    enddo
    do i=1,num2
        outobj%MatPara(i+num1,:)=inobj2%MatPara(i,:)
    enddo
    outobj%NumOfMatPara=inobj1%NumOfMatPara
    outobj%NumOfMaterial=inobj1%NumOfMaterial+inobj1%NumOfMaterial
    ! ========= Merge nodes  ============
    
    
end subroutine MergeMaterialProp
!##################################################



!##################################################
subroutine ShowMatPara(obj)
    class(MaterialProp_),intent(in)::obj

    integer i

    do i=1,size(obj%MatPara,1)
        print *, "obj%MatPara : ",obj%MatPara(i,:)
    enddo

end subroutine ShowMatPara
!##################################################

!##################################################
subroutine getValuesMaterialProp(obj,mesh,Values)
    class(MaterialProp_),intent(in):: obj
    type(Mesh_),intent(in) :: mesh
    type(Rectangle_) ::rect,mrect
    real(real64),allocatable,intent(inout) :: Values(:)
    integer(int32) :: i,j,k,l,n,dim_num,elem_num

    n=size(mesh%NodCoord,1)
    dim_num=size(mesh%NodCoord,2)
    elem_num=size(mesh%ElemNod,1)
    if(.not. allocated(Values) )then
        allocate(Values(elem_num) )
    elseif(size(Values) /= elem_num)then
        print *, "ERROR :: getValuesMaterialProp >> size(Values) /= elem_num"
        print *, size(Values),elem_num
        deallocate(Values)
        allocate(Values(elem_num) )
        Values(:)=0.0d0
        print *, size(Values),elem_num
    endif
    

    allocate(rect%NodCoord(size(obj%Mesh%ElemNod,2),size(obj%Mesh%NodCoord,2)) )
	allocate(mrect%NodCoord(size(obj%Mesh%ElemNod,2),size(obj%Mesh%NodCoord,2)) )
		
    do i=1,size(Mesh%ElemNod,1)
        do j=1,size(Mesh%ElemNod,2)
            rect%NodCoord(j,:)=Mesh%NodCoord(Mesh%ElemNod(i,j),:)
        enddo
        ! for all materials, check material parameters
		! for each zones, check in-out
        ! import nodal coordinate
        do k=1,size(obj%Mesh%ElemNod,1)
		    do l=1,size(obj%Mesh%ElemNod,2)
		    	n=obj%Mesh%ElemNod(k,l)
		    	mrect%NodCoord(l,:)=obj%Mesh%NodCoord(n,:)
		    enddo
		    ! check in-out
		    if(rect%contact(mrect) .eqv. .true. )then
		    	! in
		    	Values(i)=obj%meshPara(k,1)
		    else
		    	cycle
            endif
        enddo
    enddo

end subroutine getValuesMaterialProp
!##################################################
end module MaterialPropClass