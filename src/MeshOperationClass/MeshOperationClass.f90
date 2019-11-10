! updated 2019/1/19
module MeshOperationClass
    use MathClass
    use ArrayOperationClass
    use ShapeFunctionClass
    use GeometryClass
    implicit none


    type:: Mesh_
        real(8),allocatable::NodCoord(:,:)
        real(8),allocatable::NodCoordInit(:,:)
        integer,allocatable::ElemNod(:,:)
        integer,allocatable::FacetElemNod(:,:)
        integer,allocatable::NextFacets(:,:)
        integer,allocatable::SurfaceLine2D(:)
        integer,allocatable::ElemMat(:)
        integer,allocatable::SubMeshNodFromTo(:,:)
        integer,allocatable::SubMeshElemFromTo(:,:)
        integer,allocatable::SubMeshSurfFromTo(:,:)

        !for Interfaces
        integer,allocatable::GlobalNodID(:)

        character*70::ElemType
        character*70 ErrorMsg
    contains
        procedure :: Init => InitializeMesh
        procedure :: Delete => DeallocateMesh
        procedure :: Copy => CopyMesh
        procedure :: import => importMeshObj 
        procedure :: ImportElemNod => ImportElemNod
        procedure :: ImportNodCoord => ImportNodCoord
        procedure :: ImportElemMat => ImportElemMat

        procedure :: resize => resizeMeshobj
        procedure :: GetFacetElement => GetFacetElement
        procedure :: GetSurface => GetSurface
        procedure :: GetInterface => GetInterface
        procedure :: GetInterfaceElemNod => GetInterfaceElemNod
        procedure :: GetBoundingBox     => GetBoundingBox
        procedure :: GetFacetElemInsideBox => GetFacetElemInsideBox
        procedure :: GetInterSectBox => GetInterSectBox
        procedure :: GetNextFacets => GetNextFacets
        procedure :: MergeMesh => MergeMesh
        procedure :: ExportElemNod => ExportElemNod
        procedure :: ExportNodCoord => ExportNodCoord
        procedure :: ExportSurface2D => ExportSurface2D
        procedure :: DisplayMesh => DisplayMesh 
        procedure :: ShowMesh => ShowMesh 
        procedure :: MeltingSkelton => MeltingSkeltonMesh 
        procedure :: getNumOfDomain => getNumOfDomainMesh
        procedure :: SortFacet    => SortFacetMesh 
        procedure :: Meshing    => MeshingMesh
        procedure :: getCircumscribedCircle => getCircumscribedCircleMesh
        procedure :: getCircumscribedTriangle => getCircumscribedTriangleMesh
        procedure :: removeCircumscribedTriangle => removeCircumscribedTriangleMesh
    
        procedure :: DelauneygetNewNode => DelauneygetNewNodeMesh 
        procedure :: DelauneygetNewTriangle => DelauneygetNewTriangleMesh 
        procedure :: DelauneyremoveOverlaps => DelauneyremoveOverlapsMesh 
        procedure :: RemoveFailedTriangle => RemoveFailedTriangleMesh
        procedure :: GetElemType => GetElemTypeMesh 
    end type Mesh_


    contains
    
!##################################################
subroutine DeallocateMesh(obj)
    class(Mesh_),intent(inout)::obj

    if( allocated(obj%NodCoord         ) ) deallocate(obj%NodCoord         )
    if( allocated(obj%ElemNod          ) ) deallocate(obj%ElemNod          )
    if( allocated(obj%FacetElemNod     ) ) deallocate(obj%FacetElemNod     )
    if( allocated(obj%SurfaceLine2D    ) ) deallocate(obj%SurfaceLine2D    )
    if( allocated(obj%ElemMat          ) ) deallocate(obj%ElemMat          )
    if( allocated(obj%SubMeshNodFromTo ) ) deallocate(obj%SubMeshNodFromTo )
    if( allocated(obj%SubMeshElemFromTo) ) deallocate(obj%SubMeshElemFromTo)
    if( allocated(obj%SubMeshSurfFromTo) ) deallocate(obj%SubMeshSurfFromTo)
    !obj%ErrorMsg="All allocatable entities are deallocated"
end subroutine DeallocateMesh
!##################################################


!##################################################
subroutine CopyMesh(obj,cobj,Minimum)
    class(Mesh_),intent(inout)::obj ! copied
    class(Mesh_),intent(inout)::cobj! original
    
    logical,optional,intent(in)::Minimum


    !real(8),allocatable::NodCoord(:,:)
    ! original >> obj, copy>> cobj
    

    call CopyArray(cobj%NodCoord,            obj%NodCoord)
    call CopyArray(cobj%ElemNod  ,           obj%ElemNod)
    
    call CopyArray(cobj%FacetElemNod  ,      obj%FacetElemNod)
    call CopyArray(cobj%ElemMat  ,           obj%ElemMat)
    
    if(present(Minimum) )then
        if(Minimum .eqv. .true.)then
            return
        endif
    endif
    
    
    call CopyArray(cobj%NodCoordInit  ,      obj%NodCoordInit)
    call CopyArray(cobj%NextFacets  ,        obj%NextFacets)
    call CopyArray(cobj%SurfaceLine2D  ,     obj%SurfaceLine2D)
    call CopyArray(cobj%GlobalNodID  ,       obj%GlobalNodID)
    call CopyArray(cobj%SubMeshNodFromTo  ,  obj%SubMeshNodFromTo)
    call CopyArray(cobj%SubMeshElemFromTo  , obj%SubMeshElemFromTo)
    call CopyArray(cobj%SubMeshSurfFromTo  , obj%SubMeshSurfFromTo)
    obj%ElemType   = cobj%ElemType
    obj%ErrorMsg   = cobj%ErrorMsg
    

    
    
end subroutine

!##################################################
subroutine InitializeMesh(obj,MaterialID,NoFacetMode,simple)
    class(Mesh_),intent(inout)::obj
    integer,optional,intent(in)::MaterialID
    logical,optional,intent(in)::NoFacetMode
    logical,optional,intent(in) :: simple


    integer i,j,n1,n2,ne

    if(present(simple) )then
        if(simple .eqv. .true. )then
            return
        endif
    endif
    
    if(.not.allocated(obj%NodCoord) )then
        obj%ErrorMsg="Caution :: Initialize >> .not.allocated(obj%NodCoord)"
        print *, obj%ErrorMsg 
        return
    endif
    n1=size(obj%NodCoord,1)
    if(allocated(obj%SubMeshNodFromTo ))then
        deallocate(obj%SubMeshNodFromTo)
    endif
    allocate(obj%SubMeshNodFromTo(1,3) )
    obj%SubMeshNodFromTo(1,1)=1
    obj%SubMeshNodFromTo(1,2)=1
    obj%SubMeshNodFromTo(1,3)=n1
    print *, "Mesh%Init() => Domain information (Nodes) is imported"


    if(.not.allocated(obj%ElemNod) )then
        obj%ErrorMsg="Caution :: Initialize >> .not.allocated(obj%ElemNod)"
        print *, obj%ErrorMsg 
        return
    endif
    n1=size(obj%ElemNod,1)
    ne=n1
    if(allocated(obj%SubMeshElemFromTo ))then
        deallocate(obj%SubMeshElemFromTo)
    endif
    allocate(obj%SubMeshElemFromTo(1,3) )
    obj%SubMeshElemFromTo(1,1)=1
    obj%SubMeshElemFromTo(1,2)=1
    obj%SubMeshElemFromTo(1,3)=n1
    print *, "Mesh%Init() => Domain information (Elements) is imported"


    if( allocated(obj%ElemMat) .and. size(obj%ElemMat)/=ne )then
        deallocate(obj%ElemMat)
    endif
    
    if(.not.allocated(obj%ElemMat)  )then
        obj%ErrorMsg="Caution :: Initialize >> .not.allocated(obj%ElemMat)"
        

        print *, obj%ErrorMsg 
        
        allocate(obj%ElemMat(ne) )
        if(present(MaterialID) )then
            obj%ElemMat=MaterialID
        else
            obj%ElemMat=1
        endif
    endif

    if(present(NoFacetMode) )then
        if(NoFacetMode .eqv. .true. )then
            return
        endif
    endif

    call GetFacetElement(obj)
    call GetSurface2D(obj)
    
    if(.not.allocated(obj%SurfaceLine2D) )then
        obj%ErrorMsg="Caution :: Initialize >> .not.allocated(obj%ESurfaceLine2D)"
        print *, obj%ErrorMsg 
        return
    endif
    
    n1=size(obj%SurfaceLine2D,1)
    if(allocated(obj%SubMeshSurfFromTo ))then
        deallocate(obj%SubMeshSurfFromTo)
    endif
    
    allocate(obj%SubMeshSurfFromTo(1,3) )
    obj%SubMeshSurfFromTo(1,1)=1
    obj%SubMeshSurfFromTo(1,2)=1
    obj%SubMeshSurfFromTo(1,3)=n1

end subroutine InitializeMesh
!##################################################


!##################################################
subroutine ImportElemNod(obj,elem_nod)
    class(Mesh_),intent(inout)::obj
    integer,intent(in)::elem_nod(:,:)
    
    include "./ImportElemNod.f90"

end subroutine ImportElemNod
!##################################################





!##################################################
subroutine ImportNodCoord(obj,nod_coord)
    class(Mesh_),intent(inout)::obj
    real(8),intent(in)::nod_coord(:,:)

    include "./ImportNodCoord.f90"

end subroutine ImportNodCoord
!##################################################




!##################################################
subroutine ImportElemMat(obj,elem_mat)
    class(Mesh_),intent(inout)::obj
    integer,intent(in)::elem_mat(:)

    include "./ImportElemMat.f90"

end subroutine ImportElemMat
!##################################################

subroutine resizeMeshobj(obj,x_rate,y_rate,z_rate)
    class(Mesh_),intent(inout) :: obj
	real(8),optional,intent(in) :: x_rate,y_rate,z_rate

    if(.not.allocated(obj%NodCoord) )then
        print *, "ERROR :: MeshClass resizeMeshObj >> no Nodal coordintates are not found."
        return
    endif

    if(present(x_rate) )then
        obj%NodCoord(:,1)=x_rate*obj%NodCoord(:,1)
    endif

    if(present(y_rate) )then
        obj%NodCoord(:,2)=y_rate*obj%NodCoord(:,2)
    endif

    if(present(z_rate) )then
        obj%NodCoord(:,3)=z_rate*obj%NodCoord(:,3)
    endif

end subroutine


!##################################################
subroutine importMeshObj(obj,FileName,extention,ElemType)
    class(Mesh_),intent(inout)::obj
    character(*),intent(in)::FileName,extention,ElemType
    character(200) :: MeshVersionFormatted,Dim,Vertices,Edges,Triangles
    character(200) :: Tetrahedra
    real(8) :: null_num_real
    integer :: dim_num,node_num,elem_num,elemnod_num,i,j
    integer :: edge_num,null_num_int,num_of_triangles
    integer :: num_of_Tetrahedra
    call obj%delete()

    if(trim(extention) == ".mesh")then
        open(17,file=FileName)
        read(17,*) MeshVersionFormatted,null_num_int
        read(17,*) Dim
        read(17,*) dim_num
        read(17,*) Vertices
        read(17,*) node_num
        allocate(obj%NodCoord(node_num,dim_num) )
        do i=1,node_num
            read(17,*) obj%NodCoord(i,1:dim_num)
        enddo
        print *, "MeshClass >> importMeshobj >> imported nod_coord"
        read(17,*) Edges
        read(17,*) edge_num
        do i=1,edge_num
            read(17,*) null_num_int
        enddo
        read(17,*) Triangles
        read(17,*) num_of_triangles
        if(trim(adjustl(ElemType))=="Triangles"  )then    
            allocate(obj%ElemNod(num_of_triangles,3) )
            print *, "MeshClass >> importMeshobj >> Reading ", trim(Triangles)
            do i=1,num_of_triangles
                read(17,*) obj%ElemNod(i,1:3)
            enddo
        else
            do i=1,num_of_triangles
                read(17,*) null_num_int
            enddo
        endif

        read(17,*) Tetrahedra
        read(17,*) num_of_Tetrahedra
        if(trim(adjustl(ElemType))=="Tetrahedra"  )then    
            allocate(obj%ElemNod(num_of_Tetrahedra,4) )
            print *, "MeshClass >> importMeshobj >> Reading ", trim(Tetrahedra)
            do i=1,num_of_Tetrahedra
                read(17,*) obj%ElemNod(i,1:4)
            enddo
        else
            do i=1,num_of_Tetrahedra
                read(17,*) null_num_int
            enddo
        endif


        close(17)  
        
    else
        print *, "Extention",extention
        print *, "MeshClass >> importMeshObj >> extention is not supprted now."
    endif

    print *, "MeshClass >> importMeshobj >> Mesh is successfully imported."
end subroutine
!##################################################


!##################################################
subroutine GetFacetElement(obj)
    class(Mesh_),intent(inout)::obj


    integer :: i,j,k,l,n
    integer :: NumOfElem,NumOfDim,NumNodePerElem
    integer :: id_1,id_2,id_3,id_4
    integer :: id_r1,id_r2,id_r3,id_r4
    integer,allocatable::id(:),idr(:)
    integer,allocatable::buffer(:,:)


    if(allocated(obj%FacetElemNod) )then
        deallocate(obj%FacetElemNod)
    endif
    NumOfElem = size(obj%ElemNod,1) 
    NumOfDim  = size(obj%NodCoord,2)
    NumNodePerElem = size(obj%ElemNod,2)

    If(NumOfDim < 2 .or. NumOfDim > 4 ) then
        obj%ErrorMsg = "ERROR::GetFaceElement.f90 >> NumOfDim = 2 or 3"
        return
    endif

    if(NumOfDim==2)then
        ! initialization only for linear_triangle&rectangular ::
        if(allocated(obj%FacetElemNod) ) then
            deallocate(obj%FacetElemNod)
        endif
        allocate(obj%FacetElemNod(NumOfElem*NumNodePerElem,2) )
        obj%FacetElemNod(:,:) = 0


        ! trial mode
        do i=1,NumOfElem
            do j=1,NumNodePerElem
                id_1=mod(j+NumNodePerElem   ,NumNodePerElem)
                id_2=mod(j+NumNodePerElem+1 ,NumNodePerElem)

                if(id_1==0)then
                    id_1=NumNodePerElem
                endif
                if(id_2==0)then
                    id_2=NumNodePerElem
                endif

                obj%FacetElemNod( NumNodePerElem*(i-1)+j,1) = obj%ElemNod(i,id_1)
                obj%FacetElemNod( NumNodePerElem*(i-1)+j,2) = obj%ElemNod(i,id_2)

            enddo
        enddo

        
        ! cut off overlapped facets
        do i=1,size(obj%FacetElemNod,1)-1
            if(obj%FacetElemNod(i,1)==-1 )then
                cycle
            endif
            do j=i+1,size(obj%FacetElemNod,1)
                if(obj%FacetElemNod(i,1) == obj%FacetElemNod(j,2) .and. &
                    obj%FacetElemNod(i,2) == obj%FacetElemNod(j,1) )then

                    obj%FacetElemNod(i,:)=-1
                    obj%FacetElemNod(j,:)=-1
                    exit
                endif

                if(obj%FacetElemNod(i,1)==-1 )then
                    exit
                endif
            enddo

        enddo

        

        allocate(buffer(size(obj%FacetElemNod,1),size(obj%FacetElemNod,2) ))

        buffer(:,:)=0
        j=0
        k=0
        do i=1,size(obj%FacetElemNod,1)
            if(obj%FacetElemNod(i,1)==-1)then
                    cycle
            else
                k=k+1
                buffer(k,:)=obj%FacetElemNod(i,:)
            endif
        enddo

        deallocate(obj%FacetElemNod)
        allocate(obj%FacetElemNod(k,2) )

        do i=1,size(obj%FacetElemNod,1)
            obj%FacetElemNod(i,:)=buffer(i,:)
        enddo

        
    elseif(NumOfDim==3 )then

        ! initialization only for  Hexahedral/tetrahedron::
        if(allocated(obj%FacetElemNod) ) then
            deallocate(obj%FacetElemNod)
        endif

        NumOfElem=size(obj%ElemNod,1)
        if(NumNodePerElem==4)then
            allocate(obj%FacetElemNod(NumOfElem*4,3),id(3),idr(3) )
        elseif(NumNodePerElem==8)then
            allocate(obj%FacetElemNod(NumOfElem*6,4),id(4),idr(4) )
        else
            stop "ERROR :: GetFacetElement :: only for  Hexahedral/tetrahedron #"
        endif
        obj%FacetElemNod(:,:) = 0



        ! trial mode
        do i=1,size(obj%ElemNod,1)
            if(NumNodePerElem==4)then
                obj%FacetElemNod(  (i-1)*4+1 ,1) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*4+1 ,2) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*4+1 ,3) = obj%ElemNod(i,3)

                obj%FacetElemNod(  (i-1)*4+2 ,1) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*4+2 ,2) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*4+2 ,3) = obj%ElemNod(i,4)

                obj%FacetElemNod(  (i-1)*4+3 ,1) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*4+3 ,2) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*4+3 ,3) = obj%ElemNod(i,4)

                obj%FacetElemNod(  (i-1)*4+4 ,1) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*4+4 ,2) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*4+4 ,3) = obj%ElemNod(i,4)

            elseif(NumNodePerElem==8)then
                obj%FacetElemNod(  (i-1)*6+1 ,1) = obj%ElemNod(i,4)
                obj%FacetElemNod(  (i-1)*6+1 ,2) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*6+1 ,3) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*6+1 ,4) = obj%ElemNod(i,1)

                obj%FacetElemNod(  (i-1)*6+2 ,1) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*6+2 ,2) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*6+2 ,3) = obj%ElemNod(i,6)
                obj%FacetElemNod(  (i-1)*6+2 ,4) = obj%ElemNod(i,5)

                obj%FacetElemNod(  (i-1)*6+3 ,1) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*6+3 ,2) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*6+3 ,3) = obj%ElemNod(i,7)
                obj%FacetElemNod(  (i-1)*6+3 ,4) = obj%ElemNod(i,6)

                obj%FacetElemNod(  (i-1)*6+4 ,1) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*6+4 ,2) = obj%ElemNod(i,4)
                obj%FacetElemNod(  (i-1)*6+4 ,3) = obj%ElemNod(i,8)
                obj%FacetElemNod(  (i-1)*6+4 ,4) = obj%ElemNod(i,7)

                obj%FacetElemNod(  (i-1)*6+5 ,1) = obj%ElemNod(i,4)
                obj%FacetElemNod(  (i-1)*6+5 ,2) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*6+5 ,3) = obj%ElemNod(i,5)
                obj%FacetElemNod(  (i-1)*6+5 ,4) = obj%ElemNod(i,8)

                obj%FacetElemNod(  (i-1)*6+6 ,1) = obj%ElemNod(i,5)
                obj%FacetElemNod(  (i-1)*6+6 ,2) = obj%ElemNod(i,6)
                obj%FacetElemNod(  (i-1)*6+6 ,3) = obj%ElemNod(i,7)
                obj%FacetElemNod(  (i-1)*6+6 ,4) = obj%ElemNod(i,8)

            else
                stop "ERROR :: GetFacetElement :: only for  Hexahedral/tetrahedron ##"
            endif
        enddo


        ! cut off overlapped facets
        do i=1,size(obj%FacetElemNod,1)-1
            if(obj%FacetElemNod(i,1)==-1 )then
                cycle
            endif
            do j=i+1,size(obj%FacetElemNod,1)

                if(size(obj%FacetElemNod,2)==3 .or. size(obj%FacetElemNod,2)==4 )then
                    id(:)=obj%FacetElemNod(i,:)
                    idr(:)=obj%FacetElemNod(j,:)
                    call heapsort(size(id) ,id)
                    call heapsort(size(idr) ,idr)
                    id_1=dot_product(id-idr,id-idr)

                    if(id_1==0)then
                        obj%FacetElemNod(i,:)=-1
                        obj%FacetElemNod(j,:)=-1
                    endif
                else
                    stop "ERROR :: GetFacetElement :: only for  Hexahedral/tetrahedron ##"
                endif


            enddo
        enddo

        allocate(buffer(size(obj%FacetElemNod,1),size(obj%FacetElemNod,2) ))

        buffer(:,:)=0
        j=0
        k=0
        do i=1,size(obj%FacetElemNod,1)
            if(obj%FacetElemNod(i,1)==-1)then
                cycle
            else
                k=k+1
                buffer(k,:)=obj%FacetElemNod(i,:)
            endif
        enddo


        deallocate(obj%FacetElemNod)
        allocate(obj%FacetElemNod(k, size(buffer,2) ) )

        do i=1,size(obj%FacetElemNod,1)
            obj%FacetElemNod(i,:)=buffer(i,:)
        enddo

    endif

end subroutine GetFacetElement
!##################################################



!##################################################
subroutine GetSurface2D(obj)
    class(Mesh_),intent(inout)::obj
    integer :: i,j,k,n
    integer :: NumOfElem,NumOfDim,NumNodePerElem
    integer :: id_1,id_2
    integer,allocatable::buffer(:,:)


    NumOfElem = size(obj%ElemNod,1) 
    NumOfDim  = size(obj%NodCoord,2)
    NumNodePerElem = size(obj%ElemNod,2)

    If(NumOfDim /= 2) then
        obj%ErrorMsg = "ERROR::GetFaceElement.f90 >> NumOfDim /= 2"
        return
    endif

    call GetFacetElement(obj)
    

    !initialize
    allocate(buffer(size(obj%FacetElemNod,1),size(obj%FacetElemNod,2)) )
    buffer(1,:)=obj%FacetElemNod(1,:)

    !buffer is arranged by clock-wize
    do i=1,size(obj%FacetElemNod,1)-1
        id_2=buffer(i,2)
        do j=1,size(obj%FacetElemNod,1)
            if(id_2==obj%FacetElemNod(j,1) )then
                buffer(i+1,:)=obj%FacetElemNod(j,:)
            else
                cycle
            endif
        enddo
    enddo
    if(allocated(obj%SurfaceLine2D) ) then
        deallocate(obj%SurfaceLine2D)
    endif
    allocate(obj%SurfaceLine2D(size(buffer,1) ) )
    do i=1,size(buffer,1)
        obj%SurfaceLine2D(size(buffer,1)-i+1)=buffer(i,1)
    enddo



    if(allocated(obj%SubMeshSurfFromTo))then
        deallocate(obj%SubMeshSurfFromTo)
    endif
    allocate(obj%SubMeshSurfFromTo(1,3 ))
    obj%SubMeshSurfFromTo(1,1)=1
    obj%SubMeshSurfFromTo(1,2)=1
    obj%SubMeshSurfFromTo(1,3)=size(obj%SurfaceLine2D,1)

end subroutine GetSurface2D
!##################################################






!##################################################
subroutine GetSurface(obj)
    class(Mesh_),intent(inout)::obj
    integer :: i,j,k,n
    integer :: NumOfElem,NumOfDim,NumNodePerElem
    integer :: id_1,id_2
    integer,allocatable::buffer(:,:)


    NumOfDim=size(obj%NodCoord,2)
    if(NumOfDim==2)then
        
        call GetSurface2D(obj)
    elseif(NumOfDim==3)then
        call GetFacetElement(obj)

        call GetNextFacets(obj)

    else
        stop "ERROR >> GetSurface >> NumOfDim== 2 or 3 "
    endif

    call obj%SortFacet()

end subroutine GetSurface
!##################################################



!##################################################
subroutine GetInterface(obj1,obj2,iface1,iface2,err)
    class(Mesh_),intent(inout)::obj1,obj2
    class(Mesh_),intent(inout)::iface1,iface2
    type(Mesh_) :: BBox1,BBox2,BBox
    integer,optional,intent(inout)::err
    integer :: i,j,n,ierr


    err =0
    ! GetSurface
    call GetSurface(obj1)
    call GetSurface(obj2)


    
    ! GetBoundingBox
    call GetBoundingBox(obj1,BBox1)
    call GetBoundingBox(obj2,BBox2)

    call GetInterSectBox(BBox1,BBox2,BBox)





    if(.not.allocated(BBox%NodCoord) .or. size(BBox%NodCoord,1)==0)then
        print *, "No interface"
        err = 1
        return
    endif

    call GetFacetElemInsideBox(obj1,BBox,iface1)
    call GetFacetElemInsideBox(obj2,BBox,iface2)


    call GetInterfaceElemNod(obj1,iface1)
    call GetInterfaceElemNod(obj2,iface2)



    


end subroutine
!##################################################




!##################################################
subroutine GetInterfaceElemNod(obj,iface)
    class(Mesh_),intent(in)::obj
    class(Mesh_),intent(inout)::iface

    integer :: i,j,n,felem_num,felemnod_num,dim_num
    integer,allocatable::node_id_list(:)

    if(allocated(iface%ElemNod) )then
        deallocate(iface%ElemNod)
    endif
    if(allocated(iface%NodCoord) )then
        deallocate(iface%NodCoord)
    endif
    if(allocated(iface%NodCoordInit) )then
        deallocate(iface%NodCoordInit)
    endif

    allocate(node_id_list(size(obj%NodCoord,1) ))
    node_id_list(:)=0


    ! check node_id_list
    dim_num=size(obj%NodCoord,2)
    felem_num=size(iface%FacetElemNod,1)
    felemnod_num=size(iface%FacetElemNod,2)
    do i=1,felem_num
        do j=1,felemnod_num
            node_id_list(iface%FacetElemNod(i,j))=1
        enddo
    enddo


    

    n=sum(node_id_list)
    if(allocated(iface%GlobalNodID) ) deallocate(iface%GlobalNodID)
    if(allocated(iface%NodCoord) ) deallocate(iface%NodCoord)
    if(allocated(iface%NodCoordInit) ) deallocate(iface%NodCoordInit)
    
    allocate( iface%GlobalNodID(n) )
    allocate( iface%NodCoord(n,dim_num ) )
    allocate( iface%NodCoordInit(n,dim_num ) )



    n=0
    do i=1,size(node_id_list)
        if(node_id_list(i)==1 )then
            n=n+1
            iface%GlobalNodID(n)=i
            iface%NodCoord(n,:)=obj%NodCoord(i,:)
        else
            cycle
        endif
    enddo
    allocate(iface%ElemNod(felem_num,felemnod_num  ) )
    do i=1,size(iface%ElemNod,1)
        do j=1,size(iface%ElemNod,2)
            iface%ElemNod(i,j)=SearchIDIntVec( iface%GlobalNodID, iface%FacetElemNod(i,j) )
        enddo
    enddo
    iface%NodCoordInit(:,:)=iface%NodCoord(:,:)


end subroutine
!##################################################




!##################################################
subroutine GetBoundingBox(obj,BBox)
    class(Mesh_),intent(in)::obj
    class(Mesh_),intent(inout)::BBox

    real(8),allocatable::max_coord(:),min_coord(:)
    integer :: dim_num,i


    dim_num=size(obj%NodCoord,2)
    allocate(max_coord(dim_num) )
    allocate(min_coord(dim_num) )

    do i=1,dim_num
        max_coord(i)=maxval(obj%NodCoord(:,i) )
        min_coord(i)=minval(obj%NodCoord(:,i) )
    enddo

    if(dim_num==2)then
        allocate(BBox%NodCoord(4,2) )
        allocate(BBox%ElemNod( 1,4) )
        do i=1,4
            BBox%ElemNod(1,i)=i
        enddo

        BBox%NodCoord(1,1)=min_coord(1) ; BBox%NodCoord(1,2)=min_coord(2) ;
        BBox%NodCoord(2,1)=max_coord(1) ; BBox%NodCoord(2,2)=min_coord(2) ;
        BBox%NodCoord(3,1)=max_coord(1) ; BBox%NodCoord(3,2)=max_coord(2) ;
        BBox%NodCoord(4,1)=min_coord(1) ; BBox%NodCoord(4,2)=max_coord(2) ;

    elseif(dim_num==3)then
        allocate(BBox%NodCoord(8,3) )
        allocate(BBox%ElemNod( 1,8) )
        do i=1,8
            BBox%ElemNod(1,i)=i
        enddo

        BBox%NodCoord(1,1)=min_coord(1) ; BBox%NodCoord(1,2)=min_coord(2) ; BBox%NodCoord(1,3)=min_coord(3) ;
        BBox%NodCoord(2,1)=max_coord(1) ; BBox%NodCoord(2,2)=min_coord(2) ; BBox%NodCoord(2,3)=min_coord(3) ;
        BBox%NodCoord(3,1)=max_coord(1) ; BBox%NodCoord(3,2)=max_coord(2) ; BBox%NodCoord(3,3)=min_coord(3) ;
        BBox%NodCoord(4,1)=min_coord(1) ; BBox%NodCoord(4,2)=max_coord(2) ; BBox%NodCoord(4,3)=min_coord(3) ;
        BBox%NodCoord(5,1)=min_coord(1) ; BBox%NodCoord(5,2)=min_coord(2) ; BBox%NodCoord(5,3)=max_coord(3) ;
        BBox%NodCoord(6,1)=max_coord(1) ; BBox%NodCoord(6,2)=min_coord(2) ; BBox%NodCoord(6,3)=max_coord(3) ;
        BBox%NodCoord(7,1)=max_coord(1) ; BBox%NodCoord(7,2)=max_coord(2) ; BBox%NodCoord(7,3)=max_coord(3) ;
        BBox%NodCoord(8,1)=min_coord(1) ; BBox%NodCoord(8,2)=max_coord(2) ; BBox%NodCoord(8,3)=max_coord(3) ;

    else
        stop "ERROR :: GetBoundingBox :: dim_num should be 2 or 3 "
    endif




end subroutine
!##################################################

!##################################################
subroutine GetFacetElemInsideBox(obj,BBox,iface)
    class(Mesh_),intent(in)::obj,BBox
    class(Mesh_),intent(inout)::iface
    integer i,j,n,dim_num,s_elem_num,count_s_elem_num,c_or_not,k,mm
    real(8) ::max_obj,max_bb,min_obj,min_bb

    dim_num=size(obj%NodCoord,2)
    s_elem_num=size(obj%FacetElemNod,1)
    count_s_elem_num=0
    do i=1,s_elem_num
        c_or_not=0
        do j=1, dim_num
            mm=0
            do k=1, size(obj%FacetElemNod,2)
                max_obj= obj%NodCoord( obj%FacetElemNod(i,k),j)
                max_bb = maxval(BBox%NodCoord(:,j))
                min_obj= obj%NodCoord( obj%FacetElemNod(i,k),j)
                min_bb = minval(BBox%NodCoord(:,j))

                if(max_obj <= max_bb .and. min_obj >= min_bb  )then
                    mm=mm+1
                endif
            enddo
            if(mm >= 1)then
                c_or_not=c_or_not+1
            endif
        enddo
        if(c_or_not==dim_num)then
            count_s_elem_num=count_s_elem_num+1
        endif
    enddo

    if(allocated(iface%FacetElemNod) ) deallocate(iface%FacetElemNod)
    allocate(iface%FacetElemNod(count_s_elem_num,size(obj%FacetElemNod,2) ) )
    count_s_elem_num=0
    do i=1,s_elem_num
        c_or_not=0
        do j=1, dim_num
            mm=0
            do k=1, size(obj%FacetElemNod,2)
                max_obj= obj%NodCoord( obj%FacetElemNod(i,k),j)
                max_bb = maxval(BBox%NodCoord(:,j))
                min_obj= obj%NodCoord( obj%FacetElemNod(i,k),j)
                min_bb = minval(BBox%NodCoord(:,j))

                if(max_obj <= max_bb .and. min_obj >= min_bb  )then
                    mm=mm+1
                endif
            enddo
            if(mm >= 1)then
                c_or_not=c_or_not+1
            endif
        enddo
        if(c_or_not==dim_num)then
            count_s_elem_num=count_s_elem_num+1
            iface%FacetElemNod(count_s_elem_num,:)=obj%FacetElemNod(i,:)
        endif
    enddo


end subroutine
!##################################################

!##################################################
subroutine GetInterSectBox(obj1,obj2,BBox)
    class(Mesh_),intent(in)::obj1,obj2
    class(Mesh_),intent(inout)::BBox

    real(8),allocatable::width1(:),width2(:),center1(:),center2(:),max_coord(:),min_coord(:)
    real(8) :: xmax_(2),xmin_(2)
    integer :: dim_num,i,j,c_or_not


    dim_num=size(obj1%NodCoord,2)
    
    if(dim_num==2)then
        if(allocated(BBox%NodCoord) ) deallocate(BBox%NodCoord)
        if(allocated(BBox%ElemNod) ) deallocate(BBox%ElemNod)
        allocate(BBox%NodCoord(4,2) )
        allocate(BBox%ElemNod( 1,4) )

    elseif(dim_num==3)then
        if(allocated(BBox%NodCoord) ) deallocate(BBox%NodCoord)
        if(allocated(BBox%ElemNod) ) deallocate(BBox%ElemNod)
        
        allocate(BBox%NodCoord(8,3) )
        allocate(BBox%ElemNod( 1,8) )
    else
        stop "ERROR :: GetBoundingBox :: dim_num should be 2 or 3 "
    endif

    

    allocate(center1(dim_num) )
    allocate(center2(dim_num) )
    center1(:)=0.0d0
    center2(:)=0.0d0
    allocate(width1(dim_num) )
    allocate(width2(dim_num) )
    
    allocate(max_coord(dim_num) )
    allocate(min_coord(dim_num) )

    do i=1,dim_num
        center1(i)=0.50d0*minval(obj1%NodCoord(:,i))+0.50d0*maxval(obj1%NodCoord(:,i))
        center2(i)=0.50d0*minval(obj2%NodCoord(:,i))+0.50d0*maxval(obj2%NodCoord(:,i))
        width1(i) = maxval(obj1%NodCoord(:,i)) - minval(obj1%NodCoord(:,i)) 
        width2(i) = maxval(obj2%NodCoord(:,i)) - minval(obj2%NodCoord(:,i)) 
    enddo

    ! Contact detection
    c_or_not=1
    do i=1,dim_num
        if(abs(center1(i)-center2(i))  <= 0.50d0*width1(i)+0.50d0*width2(i) )then
            cycle
        else
            c_or_not=c_or_not*0
        endif
    enddo


    if(c_or_not==0)then
        print *, "No contact ! GetInterSectBox "

        deallocate(BBox%NodCoord)
        deallocate(BBox%ElemNod)
        return
    else
        print *, "Contact ! GetInterSectBox "
    endif

    ! Cmputing Intersection Box 
    do i=1,dim_num
        xmax_(1)=maxval(obj1%NodCoord(:,i))
        xmax_(2)=maxval(obj2%NodCoord(:,i))
        xmin_(1)=minval(obj1%NodCoord(:,i))
        xmin_(2)=minval(obj2%NodCoord(:,i))

        max_coord(i)=minval(xmax_)
        min_coord(i)=maxval(xmin_)
    enddo


    if(dim_num==2)then
        do i=1,4
            BBox%ElemNod(1,i)=i
        enddo

        BBox%NodCoord(1,1)=min_coord(1) ; BBox%NodCoord(1,2)=min_coord(2) ;
        BBox%NodCoord(2,1)=max_coord(1) ; BBox%NodCoord(2,2)=min_coord(2) ;
        BBox%NodCoord(3,1)=max_coord(1) ; BBox%NodCoord(3,2)=max_coord(2) ;
        BBox%NodCoord(4,1)=min_coord(1) ; BBox%NodCoord(4,2)=max_coord(2) ;

    elseif(dim_num==3)then
        do i=1,8
            BBox%ElemNod(1,i)=i
        enddo

        BBox%NodCoord(1,1)=min_coord(1) ; BBox%NodCoord(1,2)=min_coord(2) ; BBox%NodCoord(1,3)=min_coord(3) ;
        BBox%NodCoord(2,1)=max_coord(1) ; BBox%NodCoord(2,2)=min_coord(2) ; BBox%NodCoord(2,3)=min_coord(3) ;
        BBox%NodCoord(3,1)=max_coord(1) ; BBox%NodCoord(3,2)=max_coord(2) ; BBox%NodCoord(3,3)=min_coord(3) ;
        BBox%NodCoord(4,1)=min_coord(1) ; BBox%NodCoord(4,2)=max_coord(2) ; BBox%NodCoord(4,3)=min_coord(3) ;
        BBox%NodCoord(5,1)=min_coord(1) ; BBox%NodCoord(5,2)=min_coord(2) ; BBox%NodCoord(5,3)=max_coord(3) ;
        BBox%NodCoord(6,1)=max_coord(1) ; BBox%NodCoord(6,2)=min_coord(2) ; BBox%NodCoord(6,3)=max_coord(3) ;
        BBox%NodCoord(7,1)=max_coord(1) ; BBox%NodCoord(7,2)=max_coord(2) ; BBox%NodCoord(7,3)=max_coord(3) ;
        BBox%NodCoord(8,1)=min_coord(1) ; BBox%NodCoord(8,2)=max_coord(2) ; BBox%NodCoord(8,3)=max_coord(3) ;

    else
        stop "ERROR :: GetBoundingBox :: dim_num should be 2 or 3 "
    endif


end subroutine
!##################################################







!##################################################
subroutine GetNextFacets(obj)
    class(Mesh_),intent(inout)::obj
    integer,allocatable::buffer(:)
    integer :: i,j,n,node_id,k,l,m
    
    if(allocated(obj%NextFacets) )then
        deallocate(obj%NextFacets)
    endif

    allocate(buffer(100) )
    allocate(obj%NextFacets(size(obj%FacetElemNod,1),size(obj%FacetElemNod,2)*100+1 ))

    obj%NextFacets(:,:)=-1

    do i=1,size(obj%FacetElemNod,1)
        
        buffer(:)=-1
        obj%NextFacets(i,1)=i
        buffer(1)=i
        n=2
        do j=1,size(obj%FacetElemNod,2)
            node_id=obj%FacetElemNod(i,j)
            do k=1,size(obj%FacetElemNod,1)
                if(k==j)then
                    cycle
                endif
                do l=1,size(obj%FacetElemNod,2)
                    if(n>size(obj%NextFacets,1))then
                        stop "Warning!! >> GetNextFacets >> n>size(obj%NextFacets,1)"
                    endif
                    if(obj%FacetElemNod(k,l)==node_id )then
                        buffer(n)=k
                        n=n+1
                    endif
                enddo 
            enddo
        enddo


        do j=1,size(buffer,1)
            do k=j+1,size(buffer,1)
                if(buffer(j)==buffer(k) )then
                    buffer(k)=-1
                endif
            enddo
        enddo


        n=1
        do j=1,size(buffer,1)
            if(buffer(j)>0 )then
                if(i>size(obj%NextFacets,1) .or. n>size(obj%NextFacets,2) )then
                    print *, "i , size(obj%NextFacets,1) : ",i,size(obj%NextFacets,1)
                    print *, "n, size(obj%NextFacets,2)  : ",n,size(obj%NextFacets,2)
                    stop "MeshOperationClass >> GetNextFacets >> invalid i,n"
                endif
                obj%NextFacets(i,n)=buffer(j)
                n=n+1
            else
                cycle
            endif
        enddo
    enddo


    


end subroutine
!##################################################









!##################################################
subroutine MergeMesh(inobj1,inobj2,outobj)
    class(Mesh_),intent(in) ::inobj1,inobj2
    class(Mesh_),intent(out)::outobj
    integer node_num1,num1,num2,num3
    integer i,j,k
    
    include "./MergeMesh.f90"
    
    

end subroutine MergeMesh
!##################################################



!##################################################
subroutine ExportElemNod(obj,elem_nod)
    class(Mesh_),intent(inout)::obj
    integer,allocatable,intent(inout)::elem_nod(:,:)
    
    include "./ExportElemNod.f90"

    
end subroutine ExportElemNod
!##################################################

!##################################################
subroutine ExportNodCoord(obj,nod_coord)
    class(Mesh_),intent(inout)::obj
    real(8),allocatable,intent(inout)::nod_coord(:,:)

    include "./ExportNodCoord.f90"

    

end subroutine ExportNodCoord
!##################################################



!##################################################
subroutine ExportSurface2D(obj,surface_nod)
    class(Mesh_),intent(inout)::obj
    integer,allocatable,intent(inout)::surface_nod(:)

    include "./ExportSurface2D.f90"

end subroutine ExportSurface2D
!##################################################


!##################################################
subroutine DisplayMesh(obj,OptionalFolderName,OptionalFormat)
    class(Mesh_),intent(inout)::obj
    character*70,optional,intent(in):: OptionalFolderName
    character*4,optional,intent(in) :: OptionalFormat
    
    include "./DisplayMesh.f90"
    
    
end subroutine DisplayMesh
!##################################################

!##################################################
subroutine ShowMesh(obj,FileHandle,OnlySurface) 
    class(Mesh_),intent(inout)::obj
    integer,optional,intent(in)::FileHandle
    logical,optional,intent(in)::OnlySurface
    logical :: no_fh
    integer :: i,j,fh,n,m,exp_mode
    
    
    
    if( present(FileHandle) )then
        fh=FileHandle
        no_fh = .false.
    else
        no_fh = .true.
    endif

        !call ShowArraySize(Obj%ElemNod)
        !call ShowArraySize(Obj%FacetElemNod)
        !print *, maxval(Obj%FacetElemNod),n
        !call ShowArraySize(obj%NodCoord)
    if(present(OnlySurface) )then
        if(OnlySurface .eqv. .true. )then
            n=size(obj%FacetElemNod,1)
            exp_mode=2
        else
            n=size(obj%ElemNod,1)
            exp_mode=1
        endif
    else
        n=size(obj%ElemNod,1)
        exp_mode=1
    endif


    if(exp_mode==1)then
        do i=1,n
            do j=1,size(Obj%ElemNod,2)
                if(no_fh .eqv. .true.)then
                    write(*,*) obj%NodCoord( Obj%ElemNod(i,j),: )
                    if(j==size(Obj%ElemNod,2)  )then
                        write(*,*) " "
                    endif
                else
                    write(fh,*) obj%NodCoord( Obj%ElemNod(i,j),: )
                    if(j==size(Obj%ElemNod,2)  )then
                        write(fh,*) " "
                    endif
                endif
            enddo
        enddo
    else

        do i=1,n
            do j=1,size(Obj%FacetElemNod,2)
                if(no_fh .eqv. .true.)then
                    write(*,*) obj%NodCoord( Obj%FacetElemNod(i,j),: )
                    if(j==size(Obj%FacetElemNod,2)  )then
                        write(*,*) " "
                    endif
                else
                    write(fh,*) obj%NodCoord( Obj%FacetElemNod(i,j),: )
                    if(j==size(Obj%FacetElemNod,2)  )then
                        write(fh,*) " "
                    endif
                endif
            enddo
        enddo

    endif

end subroutine
!##################################################



!##################################################
subroutine MeltingSkeltonMesh(obj,ItrTol)
    class(Mesh_),intent(inout)::obj
    type(Mesh_) :: MeltObj
    integer,optional,intent(in)::ItrTol
    
    integer :: itr,i,j,k,l,n,m,EndStep,dnum,dnum_init,nodeid,fnodeid

    ! ######## Caution #################
    ! IT gets a "skelton mesh" 
    ! "skelton mesh" is consists of the chain of elements, where all surface are facets
    ! you need to modify this code, since it may be incomplete and slow.
    ! ######## Caution #################

    if(present(ItrTol) )then
        EndStep=ItrTol
    else
        EndStep=10
    endif

    n=size(obj%ElemNod,1)
    m=size(obj%ElemNod,2)
    
    !call obj%Copy(MeltObj)
    call obj%GetSurface()

    call Meltobj%copy(obj,Minimum=.true.)
    dnum_init=obj%getNumOfDomain()
    do itr=1,EndStep
        
        call Meltobj%GetSurface()
        
        do i=1,size(Meltobj%ElemNod,1)
            do j=1, size(Meltobj%ElemNod,2)
                nodeid=Meltobj%ElemNod(i,j)
                if(nodeid <= 0 )then
                    cycle
                endif
                do k=1,size(Meltobj%FacetElemNod,1)
                    do l=1,size(Meltobj%FacetElemNod,2)
                        fnodeid= Meltobj%FacetElemNod(k,l)
                        if(fnodeid <= 0 )then
                            print *, "Caution :: Meltobj%FacetElemNod >> NodeID <= 0 exists"
                            exit
                        endif
                        if(nodeid==fnodeid)then
                            MeltObj%ElemNod(i,:)=-1
                            exit
                        endif
                    enddo

                enddo
            enddo

            dnum=Meltobj%getNumOfDomain()
            if(dnum/=dnum_init)then
                Meltobj%ElemNod(i,:)=obj%ElemNod(i,:)
            endif
        enddo

        !call showArray(Obj%NodCoord,MeltObj%ElemNod, FileHandle=50)
        
    enddo

    

end subroutine
!##################################################


!##################################################
function getNumOfDomainMesh(obj,ItrTol) result(dnum)
    class(Mesh_),intent(inout)::obj
    integer,optional,intent(in)::ItrTol
    integer,allocatable :: domain_id(:), domain_id_ref(:),node_id(:)

    integer :: itr,i,j,k,l,n,m,node,cnode,itrmax,dnum

    n=size(obj%ElemNod,1)
    m=size(obj%ElemNod,2)

    allocate(domain_id(n),domain_id_ref(n),node_id(m) )
    do i=1,n
        domain_id(i)=i
    enddo

    if(present(ItrTol))then
        itrmax=ItrTol
    else
        itrmax=100
    endif

    do itr=1,itrmax
        domain_id_ref(:)=domain_id(:)
        do i=1,n
            do j=1,m
                node=obj%ElemNod(i,j)
                do k=1,n
                    do l=1,m
                        cnode=obj%ElemNod(k,l)
                        if(node==cnode)then
                            domain_id(i)=domain_id(n)
                            exit
                        endif
                    enddo
                enddo
            enddo
        enddo
        if(dot_product(domain_id_ref-domain_id,domain_id_ref-domain_id) == 0 )then
            print *, "getNumOfDomainMesh >> converged"
            exit
        endif    

        if(itr==itrmax)then
            print *, "getNumOfDomainMesh >> Did not converge"
            return
        endif
    enddo

    domain_id_ref(:)=0
    do i=1,n
        do j=1,n
            if(domain_id(j)==i )then
                domain_id_ref(i)=1
            endif
        enddo
    enddo


    dnum=0
    do i=1,n
        dnum=dnum+domain_id_ref(i)
    enddo




end function
!##################################################


!##################################################
subroutine SortFacetMesh(obj)
    class(Mesh_),intent(inout)::obj

    integer :: i,j,n,m,a1,a2,id
    real(8),allocatable :: buf(:)
    ! SortFacet
    n=size(obj%NodCoord,2)
    if(n==2)then
        if(.not.allocated(obj%FacetElemNod) )then
            
            print *, "ERROR :: SortFacetMesh >> for 3D, now implementing "

            return
        endif

        allocate(buf(size(obj%FacetElemNod,2) ))
        do i=1,size(obj%FacetElemNod,1)-1
            a1=obj%FacetElemNod(i,2)
            do j=i+1,size(obj%FacetElemNod,1)
                a2=obj%FacetElemNod(j,1)
                if(a2==a1)then
                    id=j
                    exit
                endif
            enddo
            buf(:)=obj%FacetElemNod(i+1,:)
            obj%FacetElemNod(i+1,:)=obj%FacetElemNod(id,:)
            obj%FacetElemNod(id,:)=buf(:)
        enddo
    elseif(n==3)then
        print *, "ERROR :: SortFacetMesh >> for 3D, now implementing "
        return
    endif


end subroutine
!##################################################


!##################################################
subroutine MeshingMesh(obj,Mode,itr_tol)
    class(Mesh_),intent(inout)::obj
    type(triangle_)::tri
    type(circle_)::cir
    integer,optional,intent(in) :: Mode,itr_tol
    integer :: i,j,k,n,m,node_num,dim_num,dim_mode
    real(8),allocatable :: stage_range(:,:),triangle(:,:)
    integer,allocatable :: staged_node(:)
    real(8) :: centerx,centery,centerz,radius
    logical :: NoChange

    ! This method creates mesh-connectivity for the given nodal coordinates.
    ! Therefore, Mesh%NodCoord(:,:) should be filled preliminary.

    dim_mode=input(default=2,option=Mode)
    if(dim_mode==2)then
        if(.not. allocated(obj%NodCoord) )then
            print *, "ERROR :: MeshOperationClass MeshingMesh"
            print *, "This method creates mesh-connectivity for the given nodal coordinates."
            print *, "Therefore, Mesh%NodCoord(:,:) should be filled preliminary."
            return 
        endif
        print *, "Meshing sequence is started."

        node_num=size(obj%NodCoord,1)
        dim_num =size(obj%NodCoord,2)

        call obj%getCircumscribedTriangle(triangle)

        if(allocated(obj%ElemNod) )then
            deallocate(obj%ElemNod)
        endif
        allocate(obj%ElemNod(node_num*100,4) )
        allocate(staged_node(node_num+3))
        obj%ElemNod(:,:)=-1
        staged_node(:)=0
        staged_node(node_num+1)=1
        staged_node(node_num+2)=1
        staged_node(node_num+3)=1
        
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        obj%NodCoord(node_num+1,:)=triangle(1,:)
        obj%NodCoord(node_num+2,:)=triangle(2,:)
        obj%NodCoord(node_num+3,:)=triangle(3,:)
        
        
        do i=1,size(obj%NodCoord,1)
            ! Delauney triangulation for 2D
            print *, i,"/",size(obj%NodCoord,1)," :: ",dble(i)/dble(size(obj%NodCoord,1))*100,"% done."
            call obj%DelauneygetNewNode(i,staged_node,triangle)
        enddo


        ! Remove invalid triangle
        
        call obj%RemoveFailedTriangle()


        do k=1,size(obj%ElemNod,1)
            if(obj%ElemNod(k,1)<1)then
                cycle
            endif
            write(123,*) obj%NodCoord(obj%ElemNod(k,1),:),obj%NodCoord(obj%ElemNod(k,2),:)-obj%NodCoord(obj%ElemNod(k,1),:)
            write(123,*) obj%NodCoord(obj%ElemNod(k,2),:),obj%NodCoord(obj%ElemNod(k,3),:)-obj%NodCoord(obj%ElemNod(k,2),:)
            write(123,*) obj%NodCoord(obj%ElemNod(k,3),:),obj%NodCoord(obj%ElemNod(k,1),:)-obj%NodCoord(obj%ElemNod(k,3),:)
            writE(123,*) " "
        enddo 

        ! Flipping (swapping) ) algorithm
        do k=1,input(default=1000,option=itr_tol)
            call obj%DelauneyremoveOverlaps(NoChange=NoChange)
            if(NoChange .eqv. .true.)then
                exit
            else
                cycle
            endif
        enddo

        ! Remove circumscribed triangle
        call obj%removeCircumscribedTriangle()

        print *, "Meshing is successfully done based on Delauney 2D"


    elseif(dim_mode==3)then
        print *, "Now implementing"
    else
        print *, "ERROR :: MeshClass :: MeshingMesh :: Dimension = ",dim_mode
    endif
    

end subroutine
!##################################################


!##################################################
subroutine getCircumscribedCircleMesh(obj,centerx,centery,centerz,radius)
    class(Mesh_),intent(inout)::obj
    real(8),intent(out)::centerx,centery,centerz,radius
    real(8),allocatable::center(:)
    real(8) :: dist
    integer ::i
    
    allocate(center( size(obj%NodCoord,2 ) ) )
    ! get center corrdinate
    do i=1,size(center)
        center(i)=mean(obj%NodCoord(:,i) )
    enddo

    ! get radius
    radius =0.0d0
    do i=1,size(obj%NodCoord,1)
        dist=distance(obj%NodCoord(i,:),center)
        if(dist >= radius)then
            radius=dist
        else
            cycle
        endif
    enddo

    

    centerz=0.0d0
    centerx=center(1)
    if(size(center)>=2 )then
        centery=center(2)
    endif
    if(size(center)>=3 )then
        centerz=center(3)
    endif

end subroutine
!##################################################


!##################################################
subroutine getCircumscribedTriangleMesh(obj,triangle)
    class(Mesh_),intent(inout)::obj
    real(8),allocatable :: center(:)
    real(8),allocatable,intent(out) :: triangle(:,:)
    real(8) :: centerx,centery,centerz,radius,pi
    integer :: i

    pi=3.1415926d0

    allocate(triangle(3,size(obj%NodCoord,2) ))
    allocate(center(size(obj%NodCoord,2) ))

    call obj%getCircumscribedCircle(centerx,centery,centerz,radius)
    radius=radius*(1.20d0)
    center(1)=centerx
    center(2)=centery

    triangle(1,1)=center(1)+2.0d0*radius*cos(0.0d0);              triangle(1,2)=center(2)+2.0d0*radius*sin(0.0d0)
    triangle(2,1)=center(1)+2.0d0*radius*cos(2.0d0*pi/3.0d0);     triangle(2,2)=center(2)+2.0d0*radius*sin(2.0d0*pi/3.0d0)
    triangle(3,1)=center(1)+2.0d0*radius*cos(-2.0d0*pi/3.0d0);    triangle(3,2)=center(2)+2.0d0*radius*sin(-2.0d0*pi/3.0d0)



    if(size(center)==3 )then
        center(3)=centerz
        triangle(:,3)=0.0d0
    endif


end subroutine
!##################################################


!##################################################
subroutine DelauneygetNewNodeMesh(obj,node_id,staged_node,triangle)
    class(Mesh_),intent(inout)::obj
    integer,intent(in) :: node_id
    integer,intent(inout):: staged_node(:) ! if =1,staged.
    real(8),intent(inout)  :: triangle(:,:)
    real(8) :: avec(3),bvec(3),cvec(3),s,t
    integer :: triangle_node_id(3),new_node_id,i,j,n,point,cover_triangle


    ! add NewNode
    staged_node(node_id)=1

    ! if i==1, create 3 triangle
    if(node_id==1)then
        triangle_node_id(1)=size(obj%NodCoord,1)+1-3
        triangle_node_id(2)=size(obj%NodCoord,1)+2-3
        triangle_node_id(3)=size(obj%NodCoord,1)+3-3
        new_node_id=1
        call obj%DelauneygetNewTriangle(triangle_node_id,new_node_id)
        
    else
        ! detect cover triangle
        do i=1,size(obj%ElemNod,1)
            if(obj%ElemNod(i,1)<1 )then
                cycle
            else
                point = 0
                ! detect in-out
                avec(:)=0.0d0
                bvec(:)=0.0d0
                avec(1:2)   =   obj%NodCoord(obj%ElemNod(i,2),1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,1),1:2 )
                bvec(1:2)   =   obj%NodCoord(obj%ElemNod(i,3),1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,1),1:2 )
                cvec(1:2)   =   obj%NodCoord(node_id,1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,1),1:2 )
                if( (bvec(1)*avec(2)-bvec(2)*avec(1)) ==0.0d0)then
                    cycle
                endif
                s = (avec(2)*cvec(1)-avec(1)*cvec(2))/(bvec(1)*avec(2)-bvec(2)*avec(1))  
                t = (bvec(2)*cvec(1)-bvec(1)*cvec(2))/(avec(1)*bvec(2)-avec(2)*bvec(1))
                !print *, "s,t=",s,t
                if(0.0d0 <= s .and. s<=1.0d0 )then
                    if(0.0d0 <= t .and. t<=1.0d0 )then
                        ! hit!
                        point = point+1
                    else
                        cycle
                    endif    
                else
                    cycle
                endif

                ! detect in-out
                avec(:)=0.0d0
                bvec(:)=0.0d0
                avec(1:2)   =   obj%NodCoord(obj%ElemNod(i,1),1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,2),1:2 )
                bvec(1:2)   =   obj%NodCoord(obj%ElemNod(i,3),1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,2),1:2 )
                cvec(1:2)   =   obj%NodCoord(node_id,1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,2),1:2 )
                s = (avec(2)*cvec(1)-avec(1)*cvec(2))/(bvec(1)*avec(2)-bvec(2)*avec(1))  
                t = (bvec(2)*cvec(1)-bvec(1)*cvec(2))/(avec(1)*bvec(2)-avec(2)*bvec(1))
                !print *, "s,t=",s,t
                if(0.0d0 <= s .and. s<=1.0d0 )then
                    if(0.0d0 <= t .and. t<=1.0d0 )then
                        ! hit!
                        point = point+1
                    else
                        cycle
                    endif    
                else
                    cycle
                endif


                ! detect in-out
                avec(:)=0.0d0
                bvec(:)=0.0d0
                avec(1:2)   =   obj%NodCoord(obj%ElemNod(i,1),1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,3),1:2 )
                bvec(1:2)   =   obj%NodCoord(obj%ElemNod(i,2),1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,3),1:2 )
                cvec(1:2)   =   obj%NodCoord(node_id,1:2 )-&
                                obj%NodCoord(obj%ElemNod(i,3),1:2 )
                s = (avec(2)*cvec(1)-avec(1)*cvec(2))/(bvec(1)*avec(2)-bvec(2)*avec(1))  
                t = (bvec(2)*cvec(1)-bvec(1)*cvec(2))/(avec(1)*bvec(2)-avec(2)*bvec(1))
                !print *, "s,t=",s,t
                if(0.0d0 <= s .and. s<=1.0d0 )then
                    if(0.0d0 <= t .and. t<=1.0d0 )then
                        ! hit!
                        point = point+1
                    else
                        cycle
                    endif    
                else
                    cycle
                endif

                if(point==3)then
                    triangle_node_id(1)=obj%ElemNod(i,1)
                    triangle_node_id(2)=obj%ElemNod(i,2)
                    triangle_node_id(3)=obj%ElemNod(i,3)
                    cover_triangle=i
                    !print *, "hit!"
                endif
                

            endif
        enddo
        new_node_id=node_id
        call obj%DelauneygetNewTriangle(triangle_node_id,new_node_id)
        !print *,  "deleted triangle id =",cover_triangle-1
        call removeArray(obj%ElemNod,remove1stColumn=.true.,NextOf=cover_triangle-1)
        
        
    endif
    ! if staged_node(k)=1, it is staged.
    





end subroutine
!##################################################


!##################################################
subroutine DelauneygetNewTriangleMesh(obj,triangle_node_id,new_node_id)
    class(Mesh_),intent(inout)::obj
    integer,intent(in)::triangle_node_id(:),new_node_id
    integer :: last_elem_id,i

    last_elem_id=0
    
    do i=1,size(obj%ElemNod,1)
        if(obj%ElemNod(i,1) >= 1)then
            last_elem_id=last_elem_id+1
        else
            exit
        endif
    enddo
    !print *, "last_elem_id",last_elem_id

    ! current Element id = last_elem_id+1
    obj%ElemNod(last_elem_id+1,1)=triangle_node_id(1)
    obj%ElemNod(last_elem_id+1,2)=triangle_node_id(2)
    obj%ElemNod(last_elem_id+1,3)=new_node_id


    obj%ElemNod(last_elem_id+2,1)=triangle_node_id(2)
    obj%ElemNod(last_elem_id+2,2)=triangle_node_id(3)
    obj%ElemNod(last_elem_id+2,3)=new_node_id


    obj%ElemNod(last_elem_id+3,1)=triangle_node_id(3)
    obj%ElemNod(last_elem_id+3,2)=triangle_node_id(1)
    obj%ElemNod(last_elem_id+3,3)=new_node_id


end subroutine
!##################################################


!##################################################
subroutine DelauneyremoveOverlapsMesh(obj,step,NoChange)
    class(Mesh_),intent(inout)::obj
    type(Point_)::p1,p2,p3
    type(Triangle_)::t1
    type(Circle_)::c1
    integer,optional,intent(in) ::step
    logical,optional,intent(inout) :: NoChange
    real(8) :: center(2),a(2),b(2),c(2),node(2)
    real(8) :: x1,y1,x2,y2,x3,y3,radius,dist_tr
    integer :: i,j,n,k,l,nodeid_1,nodeid_2,nodeid_tr_1,nodeid_tr_2,point(3)
    integer :: elem_id, node_tr,nodeid_3,dot_1,count_num,countin,flip_node
    integer :: old_triangle_id_2,old_triangle_id_1,far_node,far_node_loc,rhs_node,lhs_node 
    integer :: far_node_tr,far_node_loc_tr,k_1,k_2

    count_num=0
    NoChange = .False.
    ! Fliping for a time
    do i=1,size(obj%ElemNod,1)
        if(obj%ElemNod(i,1)<1 )then
            cycle
        endif



        ! 外心を計算する　
        a(1:2)=obj%NodCoord( obj%ElemNod(i,1),1:2 )
        b(1:2)=obj%NodCoord( obj%ElemNod(i,2),1:2 )
        c(1:2)=obj%NodCoord( obj%ElemNod(i,3),1:2 )
        

        call p1%init(dim=2)
        call p2%init(dim=2)
        call p3%init(dim=2)

        call p1%set(x=a(1),y=a(2) )
        call p2%set(x=b(1),y=b(2) )
        call p3%set(x=c(1),y=c(2) )

        call t1%init(dim=2)

        call t1%setNode(point=p1,order=1)
        call t1%setNode(point=p2,order=2)
        call t1%setNode(point=p3,order=3)

        call t1%getCircle(type_of_circle="circumcenter",circle=c1)

        !print *, "c1%radius",c1%radius,c1%center
        ! 外心を計算する　→　内外判定へ！
        ! from i th triangle to the last triangle 
        countin=0
        do j=i,size(obj%ElemNod,1)
            if(i==j)then
                cycle
            endif
            if(minval((obj%ElemNod(j,1:3) ))<=0 )then
                cycle
            endif

            !print *, "same node::",countifsame(obj%ElemNod(i,1:3) ,obj%ElemNod(j,1:3))
            if(countifsame(obj%ElemNod(i,1:3) ,obj%ElemNod(j,1:3)) /=2 )then
                cycle
            endif
            do k=1,3
                dist_tr=distance(c1%center(1:2),obj%NodCoord(obj%ElemNod(j,k),1:2 ) )
                if(k==1)then
                    k_1 = 2
                    k_2 = 3
                elseif(k==2)then
                    k_1 = 3
                    k_2 = 1
                else
                    k_1 = 1
                    k_2 = 2
                endif

                if(dist_tr < c1%radius)then
                    ! inside
                    !print *, "inside"
                    countin=countin+1

                    ! FLIP at HERE
                    ! Triangles are generated anti-clockwize
                    flip_node=obj%ElemNod(j,k)
                    old_triangle_id_1=i
                    old_triangle_id_2=j
                    ! farhest node id : (1, 2 or 3)
                    far_node_loc = 1

                    if(    obj%ElemNod(j,k_1)==obj%ElemNod(old_triangle_id_1,2) .and. &
                        obj%ElemNod(j,k_2)==obj%ElemNod(old_triangle_id_1,1)  )then
                        far_node = obj%ElemNod(i,3)
                        lhs_node = obj%ElemNod(i,1)
                        rhs_node = obj%ElemNod(i,2)
                    elseif(obj%ElemNod(j,k_1)==obj%ElemNod(old_triangle_id_1,1) .and. &
                            obj%ElemNod(j,k_2)==obj%ElemNod(old_triangle_id_1,3)    )then
                        far_node = obj%ElemNod(i,2)
                        lhs_node = obj%ElemNod(i,3)
                        rhs_node = obj%ElemNod(i,1)
                    elseif( obj%ElemNod(j,k_1)==obj%ElemNod(old_triangle_id_1,3) .and. &
                            obj%ElemNod(j,k_2)==obj%ElemNod(old_triangle_id_1,2) )then
                        far_node = obj%ElemNod(i,1)
                        lhs_node = obj%ElemNod(i,2)
                        rhs_node = obj%ElemNod(i,3)
                    else
                        cycle
                    endif


                    !print *, "OLD :: ",obj%ElemNod(old_triangle_id_1,:),"|",obj%ElemNod(old_triangle_id_2,:)
                    
                    !open(134,file="before.txt",status="replace")
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_1,1),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_1,2),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_1,3),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_1,1),1:2)
                   
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_2,1),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_2,2),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_2,3),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_2,1),1:2)
                    !close(134)

                    obj%ElemNod(old_triangle_id_1,1)=flip_node
                    obj%ElemNod(old_triangle_id_1,2)=far_node
                    obj%ElemNod(old_triangle_id_1,3)=lhs_node

                    obj%ElemNod(old_triangle_id_2,1)=flip_node
                    obj%ElemNod(old_triangle_id_2,2)=rhs_node
                    obj%ElemNod(old_triangle_id_2,3)=far_node

                    
                    ! (1) detect shared line
                    ! (2) split shared line 

                    !print *, "NEW :: ",obj%ElemNod(old_triangle_id_1,:),"|",obj%ElemNod(old_triangle_id_2,:)

                    !open(134,file="after.txt",status="replace")
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_1,1),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_1,2),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_1,3),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_1,1),1:2)
                    !
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_2,1),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_2,2),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_2,3),1:2)
                    !write(134,*) obj%NodCoord(obj%ElemNod(old_triangle_id_2,1),1:2)
                    !close(134)
                    ! FLIP at HERE

                    return
                    
                else
                    ! outside
                    !print *, "outside"
                endif
            enddo
        enddo

    enddo



    NoChange = .true.
    !print *, "No flip-point is found."

end subroutine
!##################################################




!##################################################
subroutine RemoveFailedTriangleMesh(obj)
    class(Mesh_),intent(inout)::obj
    type(Point_)::p1,p2,p3
    type(Triangle_)::t1
    type(Circle_)::c1
    integer :: i,j,n,remove,k

    ! remove non-triangle element

    print *, "debug flag0"
    n=size(obj%ElemNod,1)
    do i=n,1,-1
        if(obj%ElemNod(i,1)<=0 )then
            print *, i
            call removeArray(obj%ElemNod,remove1stColumn=.true.,NextOf=i-1)
        else
            cycle
            !if(obj%ElemNod(i,1) == obj%ElemNod(i,2))then
            !    call removeArray(obj%ElemNod,remove1stColumn=.true.,NextOf=i-1)
            !elseif(obj%ElemNod(i,2) == obj%ElemNod(i,3))then
            !    call removeArray(obj%ElemNod,remove1stColumn=.true.,NextOf=i-1)
            !elseif(obj%ElemNod(i,3) == obj%ElemNod(i,1))then
            !    call removeArray(obj%ElemNod,remove1stColumn=.true.,NextOf=i-1)
            !else
            !    cycle
            !endif
        endif
    enddo

    print *, "debug flag1"
    ! remove overlapped triangle
    n=size(obj%ElemNod,1)
    k=1
    do i=1,n
        remove=( obj%ElemNod(k,1)-obj%ElemNod(k,2) )*&
        ( obj%ElemNod(k,2)-obj%ElemNod(k,3) )*&
        ( obj%ElemNod(k,3)-obj%ElemNod(k,1) )
        if( remove==0  )then
            call removeArray(obj%ElemNod,remove1stColumn=.true.,NextOf=k-1)
        else
            k=k+1
            cycle
        endif

    enddo

    print *, "debug flag2"
    
    n=size(obj%ElemNod,1)
    do i=1,n
        

        do j=1,i-1
            remove=countifsame(obj%ElemNod(i,1:3),obj%ElemNod(j,1:3) )
            !print *, "remove2 =",remove
            if(remove>=3 )then
                call removeArray(obj%ElemNod,remove1stColumn=.true.,NextOf=j-1)
            else
                cycle
            endif
        enddo
    enddo
    print *, "debug flag3"
end subroutine
!##################################################



!##################################################
subroutine removeCircumscribedTriangleMesh(obj)
    class(Mesh_),intent(inout)::obj
    integer :: i,j,k,l,n,tri_nodes(3),rmn

    do i=1,3
        tri_nodes(i)=size(obj%NodCoord,1)
        call removeArray(obj%NodCoord,remove1stColumn=.true.,NextOf=size(obj%NodCoord,1)-1 )
    enddo


    rmn=0

    n=size(obj%ElemNod,1)
    l=1
    do i=1,n
        k=countifsame(tri_nodes(1:3), obj%ElemNod(l,1:3) )
        !print *, k
        if(k/=0)then
            ! exist
            rmn=rmn+1
            call removeArray(obj%ElemNod,remove1stColumn=.true.,NextOf=l-1 )
        else
            l=l+1
            cycle
        endif
    enddo
    print *, rmn," elements are successfully removed."


    return
end subroutine
!##################################################


!##################################################
function GetElemTypeMesh(obj) result(ElemType)
    class(Mesh_),intent(inout)::obj
    type(ShapeFunction_)::sobj
    character*200 :: ElemType
    integer :: i,j,n,m

    n=size(obj%NodCoord,2)
    m=size(obj%ElemNod,2)

    call sobj%getType(NumOfDim=n,NumOfNodePerElem=m)

    ElemType=sobj%ElemType
    return

end function
!##################################################



end module MeshOperationClass