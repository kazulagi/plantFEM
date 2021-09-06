module PhysicalFieldClass
    use ArrayClass
    use IOClass
    use LinearSolverClass
    implicit none


    type :: PhysicalField_
        character(len=:),allocatable,public:: name
        real(real64),allocatable ,public:: scalar(:)
        real(real64),allocatable ,public:: vector(:,:)
        real(real64),allocatable ,public:: tensor(:,:,:)
        integer(int32) :: attribute=0 ! node=1, element=2, gausspoint=3
        integer(int32) :: datastyle=0 !scalar=1, vector=2, tensor=3
    contains

        procedure,pass :: importPhysicalFieldScalar
        procedure,pass :: importPhysicalFieldVector
        procedure,pass :: importPhysicalFieldTensor
        generic,public :: import => importPhysicalFieldScalar,&
            importPhysicalFieldVector,&
            importPhysicalFieldTensor
        procedure :: clear => clearPhysicalField
        procedure :: init => clearPhysicalField
        procedure :: remove => clearPhysicalField
        procedure :: msh => mshPhysicalField

    end type
contains

subroutine importPhysicalFieldScalar(obj,scalar,name)
    class(PhysicalField_),intent(inout) :: obj
    real(real64),intent(in) :: scalar(:)
    character(*),intent(in) :: name

    obj % name   = "untitled"
    obj % scalar = scalar
    obj % name   = name

end subroutine

subroutine importPhysicalFieldVector(obj,vector,name)
    class(PhysicalField_),intent(inout) :: obj
    real(real64),intent(in) :: vector(:,:)
    character(*),intent(in) :: name

    obj % name   = "untitled"
    obj % vector = vector
    obj % name   = name
    
end subroutine


subroutine importPhysicalFieldtensor(obj,tensor,name)
    class(PhysicalField_),intent(inout) :: obj
    real(real64),intent(in) :: tensor(:,:,:)
    character(*),intent(in) :: name

    obj % name   = "untitled"
    obj % tensor = tensor
    obj % name   = name
    
end subroutine


subroutine clearPhysicalField(obj)
    class(PhysicalField_),intent(inout) :: obj

    if(allocated(obj%scalar) )then
        deallocate(obj%scalar)
    endif
    if(allocated(obj%vector) )then
        deallocate(obj%vector)
    endif
    if(allocated(obj%tensor) )then
        deallocate(obj%tensor)
    endif
    obj%name = "untitled"
end subroutine
! ########################################################

! ########################################################
! export physical field by gmsh (.msh)
subroutine mshPhysicalField(obj,name,caption)
    class(PhysicalField_),intent(inout) :: obj
    character(*),intent(in) :: name
    character(*),optional,intent(in) ::caption
    character(200) :: cap
    integer(int32) :: nb_scalar_points,nb_vector_points,nb_tensor_points,i,j
    real(real64),allocatable :: tensor(:,:), eigenVector(:,:)
    type(IO_) :: f
    ! doc: http://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format-version-2-_0028Legacy_0029
    if(present(caption) ) then
        cap=trim(caption)
    else
        cap="untitled"
    endif
    if(.not.allocated(obj%scalar) )then
        nb_scalar_points = 0
    else
        nb_scalar_points =size(obj%scalar,1)
    endif
    if(.not.allocated(obj%vector) )then
        nb_vector_points = 0
    else
        nb_vector_points =size(obj%vector,1)
    endif
    
    if(.not.allocated(obj%tensor) )then
        nb_tensor_points = 0
    else
        nb_tensor_points =size(obj%tensor,1)
    endif
    
    ! まずはスカラー
    if(nb_scalar_points/=0 .and. obj%attribute==1)then
        call f%open(trim(name)//"_node_scalar.msh" )
        write(f%fh,'(a)' ) "$MeshFormat"
        write(f%fh,'(a)' ) "2.2 0 8"
        write(f%fh,'(a)' ) "$EndMeshFormat"
        write(f%fh,'(a)' ) "$NodeData"
        write(f%fh,'(a)' ) "1" ! one string tag
        write(f%fh,'(a)' ) trim(cap) ! the name of the view
        write(f%fh,'(a)' ) "1" ! one real tag
        write(f%fh,'(a)' ) "0.0" ! time value
        write(f%fh,'(a)' ) "3"   ! three integer tag
        write(f%fh,'(a)' ) "0"   ! the timestep (starts from 0)
        write(f%fh,'(a)' ) "1"! 1-component (scalar) field
        write(f%fh,'(a)' ) trim(str(size(obj%scalar,1) ) )
        do i=1,size(obj%scalar,1)
            write(f%fh,'(a)' ) trim(str(i))//" "//trim(str(obj%scalar(i)))
        enddo
        write(f%fh,'(a)' ) "$EndNodeData"
        call f%close()
    endif
    ! 次にベクトル
    if(nb_vector_points/=0 .and. obj%attribute==1)then
        call f%open(trim(name)//"_node_vector.msh" )
        write(f%fh,'(a)' ) "$MeshFormat"
        write(f%fh,'(a)' ) "2.2 0 8"
        write(f%fh,'(a)' ) "$EndMeshFormat"
        write(f%fh,'(a)' ) "$NodeData"
        write(f%fh,'(a)' ) "1" ! one string tag
        write(f%fh,'(a)' ) trim(cap) ! the name of the view
        write(f%fh,'(a)' ) "1" ! one real tag
        write(f%fh,'(a)' ) "0.0" ! time value
        write(f%fh,'(a)' ) "3"   ! three integer tag
        write(f%fh,'(a)' ) "0"   ! the timestep (starts from 0)
        write(f%fh,'(a)' ) trim(str(size(obj%vector,2)))! n-component (vector) field
        write(f%fh,'(a)' ) trim(str(size(obj%vector,1) ) )
        do i=1,size(obj%vector,1)
            write(f%fh,'(a)',advance="no" ) trim(str(i))//" "
            do j=1,size(obj%vector,2)-1
                write(f%fh,'(a)',advance="no" ) trim(str(obj%vector(i,j)))//" "
            enddo
            j=size(obj%vector,2)
            write(f%fh,'(a)',advance="yes" ) trim(str(obj%vector(i,j)))
        enddo
        write(f%fh,'(a)' ) "$EndNodeData"
        call f%close()
    endif

    ! for element-data

    if(nb_scalar_points/=0 .and. obj%attribute==2)then
        call f%open(trim(name)//"_elem_scalar.msh" )
        write(f%fh,'(a)' ) "$MeshFormat"
        write(f%fh,'(a)' ) "2.2 0 8"
        write(f%fh,'(a)' ) "$EndMeshFormat"
        write(f%fh,'(a)' ) "$ElementData"
        write(f%fh,'(a)' ) "1" ! one string tag
        write(f%fh,'(a)' ) trim(cap) ! the name of the view
        write(f%fh,'(a)' ) "1" ! one real tag
        write(f%fh,'(a)' ) "0.0" ! time value
        write(f%fh,'(a)' ) "3"   ! three integer tag
        write(f%fh,'(a)' ) "0"   ! the timestep (starts from 0)
        write(f%fh,'(a)' ) "1"! 1-component (scalar) field
        write(f%fh,'(a)' ) trim(str(size(obj%scalar,1) ) )
        do i=1,size(obj%scalar,1)
            write(f%fh,'(a)' ) trim(str(i))//" "//trim(str(obj%scalar(i)))
        enddo
        write(f%fh,'(a)' ) "$EndElementData"
        call f%close()
    endif
    ! 次にベクトル
    if(nb_vector_points/=0 .and. obj%attribute==2)then
        call f%open(trim(name)//"_elem_vector.msh" )
        write(f%fh,'(a)' ) "$MeshFormat"
        write(f%fh,'(a)' ) "2.2 0 8"
        write(f%fh,'(a)' ) "$EndMeshFormat"
        write(f%fh,'(a)' ) "$ElementData"
        write(f%fh,'(a)' ) "1" ! one string tag
        write(f%fh,'(a)' ) trim(cap) ! the name of the view
        write(f%fh,'(a)' ) "1" ! one real tag
        write(f%fh,'(a)' ) "0.0" ! time value
        write(f%fh,'(a)' ) "3"   ! three integer tag
        write(f%fh,'(a)' ) "0"   ! the timestep (starts from 0)
        write(f%fh,'(a)' ) trim(str(size(obj%vector,2)))! n-component (vector) field
        write(f%fh,'(a)' ) trim(str(size(obj%vector,1) ) )
        do i=1,size(obj%vector,1)
            write(f%fh,'(a)',advance="no" ) trim(str(i))//" "
            do j=1,size(obj%vector,2)-1
                write(f%fh,'(a)',advance="no" ) trim(str(obj%vector(i,j)))//" "
            enddo
            j=size(obj%vector,2)
            write(f%fh,'(a)',advance="yes" ) trim(str(obj%vector(i,j)))
        enddo
        write(f%fh,'(a)' ) "$EndElementData"
        call f%close()
    endif

    ! テンソルの場合は実装中；
    if(nb_tensor_points/=0 .and. obj%attribute==3)then
        ! テンソルかつガウス点に定義（応力テンソルなど）
        ! 方針 >> 平均化して要素ごとに1つの主応力ベクトル図をかく
        i = size(obj%tensor,2)
        j = size(obj%tensor,3)
        if(i/=j)then
            print *, "mshFEMDomain >> ERROR >> size(obj%tensor,3)/=size(obj%tensor,2)"
            return
        endif
        allocate(tensor(i,j) )
        if(i==2)then
            call eigen_2d(tensor,eigenVector)
        elseif(i==3)then
            allocate(eigenVector(3,3))
            eigenVector(:,:) = eigen_3d(tensor)
        else
            print *, "ERROR :: mshPhysicalField for more dimension than 4-D>> not implemented yet."
            stop
        endif
    endif

end subroutine
! ########################################################

end module 