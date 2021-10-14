module MeshClass
    use std
    implicit none


    integer(int32) :: PF_GLYCINE_MAX = 1
    integer(int32) :: PF_GLYCINE_SOJA = 1
    integer(int32) :: PF_SOYBEAN = 1
    
    integer(int32) :: PF_MAIZE = 2
    

    type:: Mesh_
        ! Name
        character*200::FileName=" "
        ! Nodal coordinates
        real(real64),allocatable  ::NodCoord(:,:)
        ! Connectivity information for FE-mesh
        integer(int32),allocatable::ElemNod(:,:)
        ! Material IDs for Finite Elements
        integer(int32),allocatable::ElemMat(:)

        integer(int32),allocatable::MasterID(:)
        integer(int32),allocatable::SlaveID(:)
        integer(int32),allocatable::NTSMasterFacetID(:)
        real(real64),allocatable :: xi(:,:)

        ! optional data;
        real(real64),allocatable  ::NodCoordInit(:,:)
        integer(int32),allocatable::BottomElemID
        integer(int32),allocatable::TopElemID
        integer(int32),allocatable::FacetElemNod(:,:)
        integer(int32),allocatable::NextFacets(:,:)
        integer(int32),allocatable::SurfaceLine2D(:)
        integer(int32),allocatable::SubMeshNodFromTo(:,:)
        integer(int32),allocatable::SubMeshElemFromTo(:,:)
        integer(int32),allocatable::SubMeshSurfFromTo(:,:)


        integer(int32) :: surface=1

        !for Interfaces
        integer(int32),allocatable::GlobalNodID(:)

        character(len=36) :: uuid
        character*70::ElemType=" "
        character*70:: ErrorMsg=" "
        character*70:: meshtype

    contains
        procedure :: add => addMesh
        procedure :: addElements => addElementsMesh
        procedure :: adjustSphere => AdjustSphereMesh
        procedure :: adjustCylinder => AdjustCylinderMesh
        procedure :: assemble => assembleMesh
        procedure :: arrangeNodeOrder => arrangeNodeOrderMesh

        procedure :: copy => CopyMesh
        procedure :: cut => cutMesh
        procedure :: convertMeshType => ConvertMeshTypeMesh
        procedure :: convertTetraToHexa => convertTetraToHexaMesh 
        procedure :: convertTriangleToRectangular => convertTriangleToRectangularMesh 
        procedure :: create=>createMesh
        procedure :: check=>checkMesh
        procedure :: convert2Dto3D => Convert2Dto3DMesh
        procedure :: clean => cleanMesh
        procedure :: delete => DeallocateMesh
        procedure :: detectIface => detectIfaceMesh
        procedure :: displayMesh => DisplayMesh 
        procedure :: display => DisplayMesh 
        procedure :: divide => divideMesh
        procedure :: delauneygetNewNode => DelauneygetNewNodeMesh 
        procedure :: delauneygetNewNode3D => DelauneygetNewNode3DMesh 
        procedure :: delauneygetNewTriangle => DelauneygetNewTriangleMesh 
        procedure :: delauneyremoveOverlaps => DelauneyremoveOverlapsMesh 
        
        procedure :: export => exportMeshObj
        procedure :: exportElemNod => ExportElemNod
        procedure :: exportNodCoord => ExportNodCoord
        procedure :: exportSurface2D => ExportSurface2D
        procedure :: empty => emptyMesh
        procedure :: edit => editMesh
        
        procedure :: getCoordinate => getCoordinateMesh
        procedure :: getNodeIDinElement => getNodeIDinElementMesh
        procedure :: getFacetElement => GetFacetElement
        procedure :: getFacetNodeID => getFacetNodeIDMesh
        procedure :: getSurface => GetSurface
        procedure :: getInterface => GetInterface
        procedure :: getInterfaceElemNod => GetInterfaceElemNod
        procedure :: getBoundingBox     => GetBoundingBox
        procedure :: getFacetElemInsideBox => GetFacetElemInsideBox
        procedure :: getInterSectBox => GetInterSectBox
        procedure :: getNextFacets => GetNextFacets 
        procedure :: getElemType => GetElemTypeMesh 
        procedure :: getElement=> getElementMesh
        procedure :: getNumOfDomain => getNumOfDomainMesh
        procedure :: getCircumscribedCircle => getCircumscribedCircleMesh
        procedure :: getCircumscribedSphere => getCircumscribedSphereMesh
        procedure :: getCircumscribedTriangle => getCircumscribedTriangleMesh
        procedure :: getCircumscribedBox => getCircumscribedBoxMesh
        procedure :: getCircumscribedSphereOfTetra => getCircumscribedSphereOfTetraMesh

        procedure :: getNodeList => getNodeListMesh
        procedure :: getFacetList => getFacetListMesh
        procedure :: getElementList => getElementListMesh

        procedure :: getVolume => getVolumeMesh
        procedure :: getShapeFunction => getShapeFunctionMesh
        procedure :: getCenterCoordinate => getCenterCoordinateMesh
        procedure :: getNeighboringNode => getNeighboringNodeMesh
        procedure :: getNeighboringElement => getNeighboringElementMesh
        procedure :: ShapeFunction => getShapeFunctionMesh
        procedure :: gmsh => gmshMesh
        
        procedure :: import => importMeshObj 
        procedure :: importElemNod => ImportElemNod
        procedure :: importNodCoord => ImportNodCoord
        procedure :: importElemMat => ImportElemMat
        procedure :: init => InitializeMesh
        procedure :: InsideOfElement => InsideOfElementMesh
        
        procedure :: json => jsonMesh

        procedure :: length => lengthMesh
        procedure :: Laplacian => LaplacianMesh
        
        procedure :: mergeMesh => MergeMesh
        procedure :: meltingSkelton => MeltingSkeltonMesh 
        procedure :: meshing    => MeshingMesh

        procedure :: numElements => numElementsMesh
        procedure :: ne => numElementsMesh
        procedure :: numNodes => numNodesMesh
        procedure :: nn => numNodesMesh
        procedure :: numNodesForEachElement => numNodesForEachElementMesh
        procedure :: nne => numNodesForEachElementMesh
        procedure :: numDimension => numDimensionMesh
        procedure :: nd => numDimensionMesh
        procedure :: nearestElementID => nearestElementIDMesh
        procedure :: getNearestElementID => NearestElementIDMesh
        procedure :: getNearestNodeID => getNearestNodeIDMesh
        
        procedure :: HowManyDomain => HowManyDomainMesh
        

        procedure :: open => openMesh

        procedure :: position => positionMesh
        procedure :: position_x => position_xMesh
        procedure :: position_y => position_yMesh
        procedure :: position_z => position_zMesh

        procedure :: remove => removeMesh
        procedure :: removeCircumscribedTriangle => removeCircumscribedTriangleMesh
        procedure :: removeFailedTriangle => RemoveFailedTriangleMesh
        procedure :: removeOverlappedNode =>removeOverlappedNodeMesh
        procedure :: removeElements => removeElementsMesh
        procedure :: resize => resizeMeshobj
        procedure :: remesh => remeshMesh
        
        procedure :: save    => saveMesh 
        procedure :: sortFacet    => SortFacetMesh 
        procedure :: shift=>shiftMesh
        procedure :: showRange => showRangeMesh
        procedure :: showMesh => ShowMesh 
        procedure :: show => ShowMesh 
        

    end type Mesh_


    contains



! ##########################################################################
function getCoordinateMesh(obj,NodeID,onlyX,onlyY,OnlyZ) result(x)
    class(Mesh_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: NodeID
    real(real64),allocatable :: x(:)
    logical,optional,intent(in) :: onlyX,onlyY,OnlyZ
    integer(int32) :: n,m,itr,i,j

    if(.not.allocated(obj%nodcoord) )then
        print *, "getCoordinateMesh :: mesh is not allocated."
        return
    endif

    n = size(obj%nodcoord,1)
    m = size(obj%nodcoord,2)

    if(present(NodeID))then

        if(present(onlyX))then
            if(onlyX .eqv. .true.) then
                allocate(x(1) )
                x(1) = obj%nodcoord(NodeID,1)
                return
            endif
        endif
        if(present(onlyY))then
            if(onlyY .eqv. .true.) then
                allocate(x(1) )
                x(1) = obj%nodcoord(NodeID,2)
                return
            endif
        endif
        if(present(onlyZ))then
            if(onlyZ .eqv. .true.) then
                allocate(x(1) )
                x(1) = obj%nodcoord(NodeID,3)
                return
            endif
        endif

        allocate(x(m) )
        x(:) = obj%nodcoord(NodeID,:)
    else

        if(present(onlyX))then
            if(onlyX .eqv. .true.) then
                allocate(x(n) )
                x(:) = obj%nodcoord(:,1)
                return
            endif
        endif
        if(present(onlyY))then
            if(onlyY .eqv. .true.) then
                allocate(x(n) )
                x(:) = obj%nodcoord(:,2)
                return
            endif
        endif
        if(present(onlyZ))then
            if(onlyZ .eqv. .true.) then
                allocate(x(n) )
                x(:) = obj%nodcoord(:,3)
                return
            endif
        endif

        allocate(x(n*m) )

        itr=0
        do i=1,m
            do j=1,m    
                itr=itr+1
                x(itr) = obj%nodcoord(i,j)
            enddo
        enddo

    endif

end function
! ##########################################################################


! ##########################################################################
function getNodeIDinElementMesh(obj,ElementID) result(NodeIDList)
    class(Mesh_),intent(inout) :: obj
    integer(int32),intent(in) :: ElementID
    integer(int32),allocatable :: NodeIDList(:)
    integer(int32) :: m

    if(.not.allocated(obj%elemnod) )then
        print *, "ERROR :: getNodeIDinElementMesh :: mesh is NOT created."
        return
    endif

    m = size(obj%elemnod,2)
    allocate(NodeIDList(m) )
    NodeIDList(:) = obj%elemnod(ElementID,:)

end function
! ##########################################################################

! ####################################################################
    function detectIfaceMesh(obj,material1, material2) result(list)
        class(Mesh_) ,intent(inout) :: obj
        integer(int32),optional,intent(in) :: material1, material2
        integer(int32),allocatable :: list(:)
        integer(int32) :: itr, i,j,k,l,n,node_id,m

!        if(present(,material1) .and.  present(,material2))then
!            ! rip between material 1 and material 2
!            
!            if(material1 == material2)then
!                print *, "caution! cutmesh >> material1 == material2"
!                return
!            endif
!
!            ! detect interface
!            do i=1,size(obj%ElemNod,1)
!                if(obj%ElemMat(i) == material1 )then
!                    do j=i+1, size(obj%ElemNod,1)
!                        if(obj%ElemMat(j) == material2)then
!                            ! now , elem #i and #j touch interface
!                            ! let us record the interfacial nodes
!                            ! detect shared nodes
!                            do k=1,size(obj%ElemNod,2)
!                                do l=1,size(obj%ElemNod,2)
!                                    if(obj%ElemNod(i,k) == obj%ElemNod(j,l)  )then
!                                        node_id=obj%ElemNod(i,k)
!                                        call addlist(list,node_id)
!                                    endif
!                                enddo
!                            enddo
!                        else
!                            cycle
!                        endif
!                    enddo
!                else
!                    cycle
!                endif
!            enddo
!        endif
!
        
    end function
! ####################################################################


! ####################################################################
    subroutine cutMesh(obj,material1, material2)
        class(Mesh_) ,intent(inout) :: obj
        integer(int32),allocatable :: list(:)
        integer(int32),optional,intent(in) :: material1, material2
        integer(int32) :: itr, i,j,k,n
!        if(present(,material1) .and.  present(,material2))then
!            ! rip between material 1 and material 2
!            if(material1 == material2)then
!                print *, "caution! cutmesh >> material1 == material2"
!                return
!            endif
!
!            ! detect interface
!            list = obj%detectIface(material1, material2)            
!
!            ! add new nodes on interface
!            n=size(obj%NodCoord,1)
!            do i=1, size(list)
!                call extendArray(mat=obj%NodCoord,extend1stColumn=.true.)
!                obj%NodCoord(n+i,:)=obj%NodCoord(list(i),: )
!            enddo
!
!            ! change node_id
!            do i=1,size(obj%ElemNod,1)
!                if(obj%elemmat(i) == material1 )then
!                    do j=1,size(obj%ElemNod,2)
!                        do k=1,size(list)
!                            if( obj%ElemNod(i,j) == list(k) )then
!                                obj%ElemNod(i,j) = n+k
!                                exit
!                            endif
!                        enddo
!                    enddo
!                else
!                    cycle
!                endif
!            enddo
!
!        endif

        
    end subroutine
! ####################################################################

    function lengthMesh(obj) result(length)
        class(Mesh_) ,intent(in) :: obj
        real(real64) :: length(3)
        integer(int32) :: i

        length(:)=0.0d0
        do i=1,size(obj%NodCoord,2)
            length(i)=maxval(obj%NodCoord(:,i)) - minval(obj%NodCoord(:,i))
        enddo

    end function

! ####################################################################
    subroutine saveMesh(obj,path,name)
        class(Mesh_),intent(inout)::obj
        character(*),intent(in) :: path
        character(*),optional,intent(in) :: name
        type(IO_) :: f
        integer(int32) :: i,j,dim_num,n,m


        
        if(present(name) )then
            call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)))
            call f%open(trim(path)//"/"//trim(adjustl(name))//"/","Mesh",".prop")
            !call obj%gmsh(Name=trim(path)//"/"//trim(adjustl(name))//"/Mesh")
            !call obj%export(path=trim(path)//"/"//trim(adjustl(name))//"/",name="Mesh")
            !print *, trim(path)//"/"//trim(adjustl(name))//"/","Mesh",".prop"

        else
            call execute_command_line("mkdir -p "//trim(path)//"/Mesh")
            call f%open(trim(path)//"/Mesh/","Mesh",".prop")
            !call obj%gmsh(Name=trim(path)//"/Mesh/Mesh")
            !call obj%export(path=trim(path)//"/Mesh/",name="Mesh")
            !print *, trim(path)//"/Mesh/","Mesh",".prop"
        endif

    
        call writeArray(f%fh,obj%NodCoord)
    
        call writeArray(f%fh,obj%NodCoordInit)
        
        call writeArray(f%fh,obj%ElemNod)
        
        call writeArray(f%fh,obj%FacetElemNod)
        
        call writeArray(f%fh,obj%NextFacets)
    
        call writeArray(f%fh,obj%SurfaceLine2D)
        
        call writeArray(f%fh,obj%ElemMat)
        
        call writeArray(f%fh,obj%SubMeshNodFromTo)
        
        call writeArray(f%fh,obj%SubMeshElemFromTo)
        
        call writeArray(f%fh,obj%SubMeshSurfFromTo)
        
        call writeArray(f%fh,obj%GlobalNodID)
        
        write(f%fh,*) obj%surface
        
        write(f%fh,'(A)') trim(obj%FileName)
        write(f%fh,'(A)') trim(obj%ElemType)
        write(f%fh,'(A)') trim(obj%ErrorMsg)
        call f%close()        
    end subroutine


subroutine openMesh(obj,path,name)
    class(Mesh_),intent(inout)::obj
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f
    integer(int32) :: i,j,dim_num,n,m
    
    if(present(name) )then
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","Mesh",".prop")
    else
        call f%open(trim(path)//"/Mesh/","Mesh",".prop")
    endif
        


    call openArray(f%fh,obj%NodCoord)

    call openArray(f%fh,obj%NodCoordInit)
    
    call openArray(f%fh,obj%ElemNod)
    
    call openArray(f%fh,obj%FacetElemNod)
    
    call openArray(f%fh,obj%NextFacets)

    call openArray(f%fh,obj%SurfaceLine2D)
    
    call openArray(f%fh,obj%ElemMat)
    
    call openArray(f%fh,obj%SubMeshNodFromTo)
    
    call openArray(f%fh,obj%SubMeshElemFromTo)
    
    call openArray(f%fh,obj%SubMeshSurfFromTo)
    
    call openArray(f%fh,obj%GlobalNodID)
    
    read(f%fh,*) obj%surface
    
    read(f%fh,'(A)') obj%FileName
    read(f%fh,'(A)') obj%ElemType
    read(f%fh,'(A)') obj%ErrorMsg
    call f%close()        
end subroutine


subroutine removeMesh(obj,all,x_min,x_max,y_min,y_max,z_min,z_max)
    class(Mesh_),intent(inout)::obj
    logical,optional,intent(in) :: all
    logical :: removeall = .true.
    integer(int32),allocatable :: rm_node_list(:)
    integer(int32),allocatable :: newid_vs_oldid(:,:),elemnod(:,:)
    integer(int32),allocatable :: rm_elem_list(:),ElemMat(:)
    real(real64),allocatable :: nodcoord(:,:)
    integer(int32) :: i,j,k,n,totcount,oldid
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
    real(real64) :: xmin(3),xmax(3),x(3)
    logical :: tf
    type(IO_)::f

    if(present(all) )then
        removeall = all
    endif

    if(present(x_min) )then
        removeall = .false.
    endif
    if(present(x_max) )then
        removeall = .false.
    endif

    if(present(y_min) )then
        removeall = .false.
    endif
    if(present(y_max) )then
        removeall = .false.
    endif

    if(present(z_min) )then
        removeall = .false.
    endif
    if(present(z_max) )then
        removeall = .false.
    endif

    if(removeall .eqv. .true.)then
        if( allocated(obj%NodCoord         ) ) deallocate(obj%NodCoord         )
        if( allocated(obj%NodCoordInit     ) ) deallocate(obj%NodCoordInit     )
        if( allocated(obj%ElemNod          ) ) deallocate(obj%ElemNod          )
        if( allocated(obj%FacetElemNod     ) ) deallocate(obj%FacetElemNod     )
        if( allocated(obj%NextFacets       ) ) deallocate(obj%NextFacets       )
        if( allocated(obj%SurfaceLine2D    ) ) deallocate(obj%SurfaceLine2D    )
        if( allocated(obj%ElemMat          ) ) deallocate(obj%ElemMat          )
        if( allocated(obj%SubMeshNodFromTo ) ) deallocate(obj%SubMeshNodFromTo )
        if( allocated(obj%SubMeshElemFromTo) ) deallocate(obj%SubMeshElemFromTo)
        if( allocated(obj%SubMeshSurfFromTo) ) deallocate(obj%SubMeshSurfFromTo)
        if( allocated(obj%GlobalNodID      ) ) deallocate(obj%GlobalNodID      )
        
        obj%surface=1
        
        obj%FileName=" "
        obj%ElemType=" "
        obj%ErrorMsg=" "
        return
    endif

    ! remove only element
    if(obj%empty() .eqv. .true. )then
        print *, "ERROR obj%empty() .eqv. .true."
        stop
    endif

    ! initialization
    n = size(obj%NodCoord,1)
    allocate(rm_node_list(n) )
    rm_node_list(:)=0
    allocate(newid_vs_oldid(n,2) )
    newid_vs_oldid(:,:)=-1

    n = size(obj%ElemNod,1)
    allocate(rm_elem_list(n) )
    rm_elem_list(:)=0

    ! list-up all nodes which is to be removed.
    xmin(1)=input(default=-dble(1.0e+18),option=x_min)
    xmin(2)=input(default=-dble(1.0e+18),option=y_min)
    xmin(3)=input(default=-dble(1.0e+18),option=z_min)
    xmax(1)=input(default= dble(1.0e+18),option=x_max)
    xmax(2)=input(default= dble(1.0e+18),option=y_max)
    xmax(3)=input(default= dble(1.0e+18),option=z_max)
    
    totcount=0
    do i=1, size(rm_node_list)
        x(:)=0
        do j=1,size(obj%NodCoord,2)
            x(j)=obj%NodCoord(i,j)
        enddo
        tf = InOrOut(x=x,xmax=xmax,xmin=xmin,DimNum=3)    
        if(tf .eqv. .true.)then
            rm_node_list(i)=1 ! to be removed
            newid_vs_oldid(i,1) = -1 ! new
            newid_vs_oldid(i,1) = i ! old id
        else
            rm_node_list(i)=0 ! not to be removed
            totcount=totcount+1
            newid_vs_oldid(i,1) = totcount ! new
            newid_vs_oldid(i,1) = i ! old id
        endif
    enddo

    nodcoord = obj%nodcoord
    deallocate(obj%nodcoord)
    allocate(obj%nodcoord(totcount,size(nodcoord,2) ) )
    totcount=0
    do i=1,size(rm_node_list)
        if(rm_node_list(i)==1 )then
            cycle
        else
            totcount=totcount+1
            obj%nodcoord(totcount,:) = nodcoord(i,:)
        endif
    enddo
    
    ! new id への更新
    totcount=0
    do i=1,obj%numElements()
        do j=1,obj%numNodesForEachElement()

            do k=1,size(rm_node_list)
                if( rm_node_list(obj%elemnod(i,j)) == 1 )then
                    rm_elem_list(i)=1
                    exit
                endif
            enddo

        enddo
    enddo

    totcount=0
    do i=1,size(rm_elem_list)
        totcount=totcount+rm_elem_list(i)
    enddo

    elemnod = obj%elemnod
    deallocate(obj%elemnod)
    allocate(obj%elemnod(size(elemnod,1)-totcount,size(elemnod,2) ) )
    
    totcount=0
    do i=1,size(rm_elem_list)
        if(rm_elem_list( i )==1 )then
            cycle
        else
            totcount=totcount+1
            obj%elemnod(totcount,:) = elemnod(i,:)
        endif
    enddo

    do i=1,size(obj%elemnod,1)
        do j=1,size(obj%elemnod,2)
            totcount=0
            do k=1,obj%elemnod(i,j)-1
                totcount=totcount+rm_node_list(k)
            enddo
            obj%elemnod(i,j) = obj%elemnod(i,j) - totcount
        enddo
    enddo

    totcount=0
    do i=1,size(rm_elem_list)
        totcount=totcount+rm_elem_list(i)
    enddo

    if(.not. allocated(obj%elemmat) )then
        call print(".not. allocated(obj%elemmat) >> ignored!")
        return
    endif
    elemmat = obj%elemmat
    deallocate(obj%elemmat)
    allocate(obj%elemmat(size(elemmat)- totcount) )
    totcount=0
    do i=1,size(rm_elem_list)
        if(rm_elem_list(i)==1 )then
            cycle
        else
            totcount=totcount+1
            obj%elemmat(totcount) = elemmat(i)
        endif
    enddo


end subroutine

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
    class(Mesh_),intent(in)::cobj! original
    
    logical,optional,intent(in)::Minimum


    !real(real64),allocatable::NodCoord(:,:)
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
    integer(int32),optional,intent(in)::MaterialID
    logical,optional,intent(in)::NoFacetMode
    logical,optional,intent(in) :: simple


    integer(int32) i,j,n1,n2,ne

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
    !print *, "Mesh%Init() => Domain information (Nodes) is imported"


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
    !print *, "Mesh%Init() => Domain information (Elements) is imported"


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
    integer(int32),intent(in)::elem_nod(:,:)
    
    
    if(allocated(obj%ElemNod) )then
        deallocate(obj%ElemNod)
    endif
    allocate(obj%ElemNod(size(elem_nod,1),size(elem_nod,2) ) )
    obj%ElemNod(:,:)=elem_nod(:,:)

    
    if(allocated(obj%SubMeshElemFromTo))then
        deallocate(obj%SubMeshElemFromTo)
    endif
    allocate(obj%SubMeshElemFromTo(1,3 ))
    obj%SubMeshElemFromTo(1,1)=1
    obj%SubMeshElemFromTo(1,2)=1
    obj%SubMeshElemFromTo(1,3)=size(elem_nod,1)

end subroutine ImportElemNod
!##################################################





!##################################################
subroutine ImportNodCoord(obj,nod_coord)
    class(Mesh_),intent(inout)::obj
    real(real64),intent(in)::nod_coord(:,:)

    
    if(allocated(obj%NodCoord) )then
        deallocate(obj%NodCoord)
    endif
    allocate(obj%NodCoord(size(nod_coord,1),size(nod_coord,2) ) )
    obj%NodCoord(:,:)=nod_coord(:,:)

    if(allocated(obj%SubMeshNodFromTo))then
        deallocate(obj%SubMeshNodFromTo)
    endif
    allocate(obj%SubMeshNodFromTo(1,3 ))
    obj%SubMeshNodFromTo(1,1)=1
    obj%SubMeshNodFromTo(1,2)=1
    obj%SubMeshNodFromTo(1,3)=size(nod_coord,1)

end subroutine ImportNodCoord
!##################################################




!##################################################
subroutine ImportElemMat(obj,elem_mat)
    class(Mesh_),intent(inout)::obj
    integer(int32),intent(in)::elem_mat(:)

    if(allocated(obj%ElemMat) )then
        deallocate(obj%ElemMat)
    endif
    allocate(obj%ElemMat(size(elem_mat,1) ) )
    obj%ElemMat(:)=elem_mat(:)

end subroutine ImportElemMat
!##################################################

subroutine resizeMeshobj(obj,x_rate,y_rate,z_rate,x_len,y_len,z_len)
    class(Mesh_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_rate,y_rate,z_rate,x_len,y_len,z_len
    real(real64) :: rate, len


    ! 2021/09/24 >> Tried to paralelize this by OpenMP, but failed.
    ! do not use !$OMP parallel do

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

    if(present(x_len) )then
        len = maxval(obj%NodCoord(:,1) ) - minval(obj%NodCoord(:,1) )
        rate = x_len/len
        obj%NodCoord(:,1)=rate*obj%NodCoord(:,1)
    endif

    if(present(y_len) )then
        len = maxval(obj%NodCoord(:,2) ) - minval(obj%NodCoord(:,2) )
        rate = y_len/len
        obj%NodCoord(:,2)=rate*obj%NodCoord(:,2)
    endif

    if(present(z_len) )then
        len = maxval(obj%NodCoord(:,3) ) - minval(obj%NodCoord(:,3) )
        rate = z_len/len
        obj%NodCoord(:,3)=rate*obj%NodCoord(:,3)
    endif
end subroutine


!##################################################
subroutine importMeshObj(obj,FileName,extention,ElemType,Mesh)
    class(Mesh_),intent(inout)::obj
    type(Mesh_),optional,intent(in) :: Mesh
    type(IO_) :: f
    character(*),optional,intent(in)::FileName,extention,ElemType
    character(200) :: MeshVersionFormatted,Dim,Vertices,Edges,Triangles
    character(200) :: Tetrahedra,ex,ch
    real(real64) :: null_num_real
    integer(int32) :: dim_num,node_num,elem_num,elemnod_num,i,j
    integer(int32) :: edge_num,null_num_int,num_of_triangles
    integer(int32) :: num_of_Tetrahedra

    call obj%delete()
    if(present(Mesh) )then
        call obj%copy(Mesh)
        return
    endif

    if(present(FileName) )then
        ex=getext(FileName)
        if(  trim(ex)=="stl")then
            
            return
        endif
    endif

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
        !print *, "MeshClass >> importMeshobj >> imported nod_coord"
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
subroutine exportMeshObj(obj,restart,path,stl,scalar,vector,tensor,name)
    class(Mesh_),intent(inout)::obj
    real(real64),optional,intent(in) :: scalar(:),vector(:,:),tensor(:,:,:)
    logical,optional,intent(in) :: restart,stl
    character(*),optional,intent(in) :: path
    character(*),optional,intent(in) :: name
    character(200) :: fieldname
    type(IO_) :: f
    integer(int32) :: i,j,dim_num
	real(real64) :: x1(3),x2(3),x3(3),x,y,z
    
    if(present(name) )then
        fieldname=trim(adjustl(name))
    else
        fieldname="Mesh"
    endif

    if(size(obj%ElemNod,2)==2 )then

        call f%open(trim(fieldname)//".msh" )
        call f%write("$MeshFormat") 
        call f%write("2.2 0 8")
        call f%write("$EndMeshFormat\n")
        call f%write('$Nodes')
        write(f%fh,*) size(obj%NodCoord,1)
        do i=1,size(obj%NodCoord,1)
            write(f%fh,*) i,obj%NodCoord(i,:)
        enddo
        call f%write('$EndNodes')
        call f%write('$Elements')
        write(f%fh,*) size(obj%ElemNod,1)
        do i=1,size(obj%ElemNod,1)
            write(f%fh,*) i,"3 2 2 1",obj%ElemNod(i,:),obj%ElemNod(i,:)
        enddo
        call f%write('$EndElements')
        call f%close()
        
        return
    endif

    call execute_command_line("mkdir -p "//trim(path)//"/Mesh")

    if(obj%empty() .eqv. .true.)then
        return
    endif


    if(present(restart) )then
        call execute_command_line("mkdir -p "//trim(path)//"/Mesh")
        call f%open(trim(path)//"/Mesh/",trim(fieldname),".prop")
        
        call writeArray(f%fh,obj%NodCoord)

        call writeArray(f%fh,obj%NodCoordInit)
        
        call writeArray(f%fh,obj%ElemNod)
        
        call writeArray(f%fh,obj%FacetElemNod)
        
        call writeArray(f%fh,obj%NextFacets)

        call writeArray(f%fh,obj%SurfaceLine2D)
        
        call writeArray(f%fh,obj%ElemMat)
        
        call writeArray(f%fh,obj%SubMeshNodFromTo)
        
        call writeArray(f%fh,obj%SubMeshElemFromTo)
        
        call writeArray(f%fh,obj%SubMeshSurfFromTo)
        
        call writeArray(f%fh,obj%GlobalNodID)
        
        write(f%fh,*) obj%surface

        write(f%fh,'(A)') trim(obj%FileName)
        write(f%fh,'(A)') trim(obj%ElemType)
        write(f%fh,'(A)') trim(obj%ErrorMsg)
        call f%close()
        return
    endif

    ! export mesh 
    call f%open(trim(path)//"/Mesh/","Mesh",".vtk")
	write(f%fh,'(A)' ) "# vtk DataFile Version 2.0"
	write(f%fh,'(A)' ) "Cube example"
	write(f%fh,'(A)' ) "ASCII"
	write(f%fh,'(A)' ) "DATASET POLYDATA"
	write(f%fh,'(A)' ,advance="no") "POINTS "
	write(f%fh,'(i10)' ,advance="no")size(obj%NodCoord,1)
	write(f%fh,'(A)')" float"
    if( size(obj%NodCoord,2)==3 )then
	    do i=1,size(obj%NodCoord,1)
	    	do j=1,size(obj%NodCoord,2)
                if(j==size(obj%NodCoord,2))then
	    			write(f%fh,'(f20.8)' ) obj%NodCoord(i,j)
	    		else
	    			write(f%fh,'(f20.8)', advance="no" ) obj%NodCoord(i,j)
	    			write(f%fh,'(A)', advance="no" ) " "
	    		endif
	    	enddo
	    enddo
    elseif( size(obj%NodCoord,2)==2 )then
        do i=1,size(obj%NodCoord,1)
	    	do j=1,size(obj%NodCoord,2)
                if(j==size(obj%NodCoord,2))then
	    			write(f%fh,'(f20.8)', advance="no" ) obj%NodCoord(i,j)
	    			write(f%fh,'(A)', advance="no" ) " "
	    		endif
                write(f%fh,'(f20.8)' ) 0.0d0
	    	enddo
	    enddo
    elseif( size(obj%NodCoord,2)==1 )then
        do i=1,size(obj%NodCoord,1)
            do j=1,size(obj%NodCoord,2)
                if(j==size(obj%NodCoord,2))then
                    write(f%fh,'(f20.8)', advance="no" ) obj%NodCoord(i,j)
                    write(f%fh,'(A)', advance="no" ) " "
                endif
                write(f%fh,'(f20.8)' ) 0.0d0,0.0d0
            enddo
        enddo        
    else
        print *, "Mesh % vtk >> invalid space dimension",size(obj%NodCoord,2)
        stop
    endif

	write(f%fh,'(A)',advance="no")" POLYGONS "
	write(f%fh,'(i10)',advance="no") 6*size(obj%ElemNod,1)
	write(f%fh,'(A)',advance="no") " "
	write(f%fh,'(i10)') size(obj%ElemNod,1)*5*6
    write(f%fh,'(A)') "CELL_DATA 6"
    call f%close()
    

    ! export mesh with scalar
    if(present(scalar) )then

        call f%open(trim(path)//"/Mesh/",trim(fieldname),".vtk")
    	write(f%fh,'(A)' ) "# vtk DataFile Version 2.0"
    	write(f%fh,'(A)' ) "Cube example"
    	write(f%fh,'(A)' ) "ASCII"
    	write(f%fh,'(A)' ) "DATASET POLYDATA"
    	write(f%fh,'(A)' ,advance="no") "POINTS "
    	write(f%fh,'(i10)' ,advance="no")size(obj%NodCoord,1)
    	write(f%fh,'(A)')" float"
    	do i=1,size(obj%NodCoord,1)
    		do j=1,size(obj%NodCoord,2)
    			if(j==size(obj%NodCoord,2))then
    				write(f%fh,'(f20.8)' ) obj%NodCoord(i,j)
    			else
    				write(f%fh,'(f20.8)', advance="no" ) obj%NodCoord(i,j)
    				write(f%fh,'(A)', advance="no" ) " "
    			endif
    		enddo
    	enddo
    	write(f%fh,'(A)',advance="no")" POLYGONS "
    	write(f%fh,'(i10)',advance="no") 6*size(obj%ElemNod,1)
    	write(f%fh,'(A)',advance="no") " "
    	write(f%fh,'(i10)') size(obj%ElemNod,1)*5*6
    	

	    do i=1,size(obj%ElemNod,1)
	    	write(f%fh,'(A)',advance="no") "4 "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,1))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,2))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,3))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,4))
	    	write(f%fh,'(A)') " "
	    	write(f%fh,'(A)',advance="no") "4 "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,5))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,6))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,7))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,8))
	    	write(f%fh,'(A)') " "
	    	write(f%fh,'(A)',advance="no") "4 "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,1))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,2))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,6))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,5))
	    	write(f%fh,'(A)') " "
	    	write(f%fh,'(A)',advance="no") "4 "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,3))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,4))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,8))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,7))
	    	write(f%fh,'(A)') " "
	    	write(f%fh,'(A)',advance="no") "4 "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,1))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,5))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,8))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,4))
	    	write(f%fh,'(A)') " "
	    	write(f%fh,'(A)',advance="no") "4 "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,2))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,3))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,7))
	    	write(f%fh,'(A)',advance="no") " "
	    	write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,6))
	    	write(f%fh,'(A)') " "
        enddo
        
        call execute_command_line("mkdir -p "//trim(path)//"/Mesh")
        call f%open(trim(path)//"/Mesh/",trim(fieldname),".ply")
    	write(f%fh,'(A)')"ply"
    	write(f%fh,'(A)')"format ascii 1.0"
    	write(f%fh,'(A)',advance="no")"element vertex "
    	write(f%fh,'(i10)') size(obj%NodCoord,1)
    	write(f%fh,'(A)')"property float32 x"
    	write(f%fh,'(A)')"property float32 y"
    	write(f%fh,'(A)')"property float32 z"
    	write(f%fh,'(A)')"property uchar red"
    	write(f%fh,'(A)')"property uchar green"
    	write(f%fh,'(A)')"property uchar blue"
    	write(f%fh,'(A)',advance="no")"element face "
    	write(f%fh,'(i10)') size(obj%ElemNod,1)*6
    	write(f%fh,'(A)')"property list uint8 int32 vertex_indices"
    	write(f%fh,'(A)') "end_header"
    	do i=1,size(obj%NodCoord,1)
    		do j=1,size(obj%NodCoord,2)
    			if(j==size(obj%NodCoord,2))then
    				write(f%fh,'(f20.8)', advance="no"  ) obj%NodCoord(i,j)
    				write(f%fh,'(A)', advance="no" ) " "
    			else
    				write(f%fh,'(f20.8)', advance="no" ) obj%NodCoord(i,j)
    				write(f%fh,'(A)', advance="no" ) " "
    			endif
    		enddo
    		write(f%fh,'(A)', advance="no" ) " "
    		write(f%fh,'(i3)',advance="no") int(obj%NodCoord(i,1)*255.0d0/maxval(obj%NodCoord(:,1) ))
    		write(f%fh,'(A)', advance="no" ) " "
    		write(f%fh,'(i3)',advance="no") int(obj%NodCoord(i,2)*255.0d0/maxval(obj%NodCoord(:,2) ))
    		write(f%fh,'(A)', advance="no" ) " "
    		write(f%fh,'(i3)') int(obj%NodCoord(i,3)*255.0d0/maxval(obj%NodCoord(:,3) ))
        enddo
        do i=1,size(obj%ElemNod,1)
            write(f%fh,'(A)',advance="no") "4 "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,1))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,2))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,3))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,4))
            write(f%fh,'(A)') " "
            write(f%fh,'(A)',advance="no") "4 "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,5))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,6))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,7))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,8))
            write(f%fh,'(A)') " "
            write(f%fh,'(A)',advance="no") "4 "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,1))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,2))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,6))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,5))
            write(f%fh,'(A)') " "
            write(f%fh,'(A)',advance="no") "4 "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,3))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,4))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,8))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,7))
            write(f%fh,'(A)') " "
            write(f%fh,'(A)',advance="no") "4 "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,1))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,5))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,8))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,4))
            write(f%fh,'(A)') " "
            write(f%fh,'(A)',advance="no") "4 "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,2))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,3))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,7))
            write(f%fh,'(A)',advance="no") " "
            write(f%fh,'(i10)',advance="no") scalar(obj%ElemNod(i,6))
            write(f%fh,'(A)') " "
        enddo
        call f%close()



    endif
    write(f%fh,'(A)') "CELL_DATA 6"
    call f%close()
    


    call execute_command_line("mkdir -p "//trim(path)//"/Mesh")
    call f%open(trim(path)//"/Mesh/","Mesh",".ply")
	write(f%fh,'(A)')"ply"
	write(f%fh,'(A)')"format ascii 1.0"
	write(f%fh,'(A)',advance="no")"element vertex "
	write(f%fh,'(i10)') size(obj%NodCoord,1)
	write(f%fh,'(A)')"property float32 x"
	write(f%fh,'(A)')"property float32 y"
	write(f%fh,'(A)')"property float32 z"
	write(f%fh,'(A)')"property uchar red"
	write(f%fh,'(A)')"property uchar green"
	write(f%fh,'(A)')"property uchar blue"
	write(f%fh,'(A)',advance="no")"element face "
	write(f%fh,'(i10)') size(obj%ElemNod,1)*6
	write(f%fh,'(A)')"property list uint8 int32 vertex_indices"
	write(f%fh,'(A)') "end_header"
	do i=1,size(obj%NodCoord,1)
		do j=1,size(obj%NodCoord,2)
			if(j==size(obj%NodCoord,2))then
				write(f%fh,'(f20.8)', advance="no"  ) obj%NodCoord(i,j)
				write(f%fh,'(A)', advance="no" ) " "
			else
				write(f%fh,'(f20.8)', advance="no" ) obj%NodCoord(i,j)
				write(f%fh,'(A)', advance="no" ) " "
			endif
		enddo
		write(f%fh,'(A)', advance="no" ) " "
		write(f%fh,'(i3)',advance="no") int(obj%NodCoord(i,1)*255.0d0/maxval(obj%NodCoord(:,1) ))
		write(f%fh,'(A)', advance="no" ) " "
		write(f%fh,'(i3)',advance="no") int(obj%NodCoord(i,2)*255.0d0/maxval(obj%NodCoord(:,2) ))
		write(f%fh,'(A)', advance="no" ) " "
		write(f%fh,'(i3)') int(obj%NodCoord(i,3)*255.0d0/maxval(obj%NodCoord(:,3) ))
    enddo
    do i=1,size(obj%ElemNod,1)
        write(f%fh,'(A)',advance="no") "4 "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,1)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,2)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,3)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,4)-1
        write(f%fh,'(A)') " "
        write(f%fh,'(A)',advance="no") "4 "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,5)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,6)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,7)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,8)-1
        write(f%fh,'(A)') " "
        write(f%fh,'(A)',advance="no") "4 "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,1)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,2)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,6)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,5)-1
        write(f%fh,'(A)') " "
        write(f%fh,'(A)',advance="no") "4 "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,3)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,4)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,8)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,7)-1
        write(f%fh,'(A)') " "
        write(f%fh,'(A)',advance="no") "4 "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,1)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,5)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,8)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,4)-1
        write(f%fh,'(A)') " "
        write(f%fh,'(A)',advance="no") "4 "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,2)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,3)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,7)-1
        write(f%fh,'(A)',advance="no") " "
        write(f%fh,'(i10)',advance="no") obj%ElemNod(i,6)-1
        write(f%fh,'(A)') " "
    enddo
    call f%close()

    if(present(stl) )then
        call execute_command_line("mkdir -p "//trim(path)//"/Mesh")
        call f%open(trim(path)//"/Mesh/","Mesh",".stl")
        call obj%GetSurface()
	    dim_num = size(obj%NodCoord,2)
        if(dim_num/=3)then
            print *, "Sorry, Export stl is supported only for 3-D mesh"
            close(f%fh)
            return
        endif
        write(f%fh,'(A)') "solid "//trim(path)//"/Mesh"
        print *, "Number of facet is",size(obj%FacetElemNod,1)
        do i=1,size(obj%FacetElemNod,1)
            if(size(obj%FacetElemNod,2)==4  )then
                ! rectangular
                ! describe two triangular

                x1(:)=obj%NodCoord(obj%FacetElemNod(i,1),: ) 
                x2(:)=obj%NodCoord(obj%FacetElemNod(i,2),: )
                x3(:)=obj%NodCoord(obj%FacetElemNod(i,3),: )
                write(f%fh,'(A)') "facet normal 0.0 0.0 1.0"
                write(f%fh,'(A)') "outer loop"
                write(f%fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
                write(f%fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
                write(f%fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
                write(f%fh,'(A)') "endloop"
                write(f%fh,'(A)') "endfacet"
                x1(:)=obj%NodCoord(obj%FacetElemNod(i,1),: ) 
                x2(:)=obj%NodCoord(obj%FacetElemNod(i,3),: )
                x3(:)=obj%NodCoord(obj%FacetElemNod(i,4),: )
                write(f%fh,'(A)') "facet normal 0.0 0.0 1.0"
                write(f%fh,'(A)') "outer loop"
                write(f%fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
                write(f%fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
                write(f%fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
                write(f%fh,'(A)') "endloop"
                write(f%fh,'(A)') "endfacet"
            elseif(size(obj%FacetElemNod,2)==3  )then
                ! rectangular
                ! describe two triangular
                x1(:)=obj%NodCoord(obj%FacetElemNod(i,1),: ) 
                x2(:)=obj%NodCoord(obj%FacetElemNod(i,2),: )
                x3(:)=obj%NodCoord(obj%FacetElemNod(i,3),: )
                write(f%fh,'(A)') "facet normal 0.0 0.0 1.0"
                write(f%fh,'(A)') "outer loop"
                write(f%fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
                write(f%fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
                write(f%fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
                write(f%fh,'(A)') "endloop"
                write(f%fh,'(A)') "endfacet"

            else
                ! other
                print *, "Sorry, Export stl is supported only for rectangular mesh"
                return
                close(f%fh)
            endif
        enddo
        write(f%fh,'(A)') "endsolid "//trim(path)//"/Mesh"
        call f%close()
    endif


end subroutine
!##################################################

recursive subroutine GetFacetElementByDivideConquor(obj)
    class(Mesh_),intent(inout)::obj
    type(Mesh_) :: smallObj

    print *, "ERROR :: not implemented yet >> GetFacetElementByDivideConquor"
    stop

end subroutine


!##################################################
function getFacetNodeIDMesh(obj,ElementID) result(ret)
    class(Mesh_),intent(in) :: obj
    integer(int32),intent(in) :: ElementID
    integer(int32),allocatable :: ret(:,:),order(:,:)
    integeR(int32) :: i,j,n,elemid,k,dimnum,elemnodnum


    ! get element info
    dimnum = size(obj%nodcoord,2)
    elemnodnum = size(obj%elemnod,2)


    if(dimnum==3 .and. elemnodnum==4)then
        ! Tetra mesh
        allocate(ret(4,3) )
        allocate(order(4,3) )
        order(1,:) = [3, 2, 1]
        order(2,:) = [1, 2, 4]
        order(3,:) = [2, 3, 4]
        order(4,:) = [3, 1, 4]
        do k=1,4
            ret(k,1) = obj%elemnod( ElementID, order(k,1) )
            ret(k,2) = obj%elemnod( ElementID, order(k,2) )
            ret(k,3) = obj%elemnod( ElementID, order(k,3) )
        enddo
        return
    elseif(dimnum==3 .and. elemnodnum==8)then
        ! Tetra mesh
        allocate(ret(6,4) )
        allocate(order(6,4) )
        order(1,:) = [ 4, 3, 2, 1]
        order(2,:) = [ 1, 2, 6, 5]
        order(3,:) = [ 2, 3, 7, 6]
        order(4,:) = [ 3, 4, 8, 7]
        order(5,:) = [ 4, 1, 5, 8]
        order(6,:) = [ 5, 6, 7, 8]
        do k = 1,6
            ret(k,1) = obj%elemnod( ElementID, order(k,1) )
            ret(k,2) = obj%elemnod( ElementID, order(k,2) )
            ret(k,3) = obj%elemnod( ElementID, order(k,3) )
            ret(k,4) = obj%elemnod( ElementID, order(k,4) )
        enddo
        return
    endif


end function
!##################################################


!##################################################
subroutine GetFacetElement(obj)
    class(Mesh_),intent(inout)::obj
    logical,parameter :: fast=.true.

    integer(int32) :: i,j,k,l,n,m
    integer(int32) :: NumOfElem,NumOfDim,NumNodePerElem
    integer(int32) :: id_1,id_2,id_3,id_4,num_face,div_num
    integer(int32) :: id_r1,id_r2,id_r3,id_r4,diff,elementID_I,elementID_J
    integer(int32),allocatable::id(:),idr(:)
    integer(int32),allocatable::buffer(:,:),ElementGroup(:,:)
    real(real64):: dx(3),x(3)
    logical,allocatable :: overlap(:)

    ! From 1 -> 2 -> -> 3 -> 4, outer normal vector is obtained  


    if(allocated(obj%FacetElemNod) )then
        deallocate(obj%FacetElemNod)
    endif
    NumOfElem = size(obj%ElemNod,1) 
    NumOfDim  = size(obj%NodCoord,2)
    NumNodePerElem = size(obj%ElemNod,2)

    If(NumOfDim < 2 .or. NumOfDim > 4 ) then
        obj%ErrorMsg = "ERROR::GetFaceElement.f90 >> NumOfDim = 2 or 3"
        stop
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
            ! New algorithm
        if(fast)then
            allocate(ElementGroup(size(obj%elemnod,1),3) )
            !div_num = size(obj%elemnod,1)/200 + 1
            div_num=10
            dx(1) = (maxval(obj%nodcoord(:,1) ) - minval(obj%nodcoord(:,1) ))/dble(div_num)
            div_num=10
            dx(2) = (maxval(obj%nodcoord(:,2) ) - minval(obj%nodcoord(:,2) ))/dble(div_num)
            div_num=10
            dx(3) = (maxval(obj%nodcoord(:,3) ) - minval(obj%nodcoord(:,3) ))/dble(div_num)
            do i=1, size(obj%elemnod,1)
                x(1) = obj%nodcoord(obj%elemnod(i,1) ,1 )
                x(2) = obj%nodcoord(obj%elemnod(i,1) ,2 )
                x(3) = obj%nodcoord(obj%elemnod(i,1) ,3 )
                ElementGroup(i,1) = int((x(1) -minval(obj%nodcoord(:,1) ))/dx(1))
                ElementGroup(i,2) = int((x(2) -minval(obj%nodcoord(:,2) ))/dx(2))
                ElementGroup(i,3) = int((x(3) -minval(obj%nodcoord(:,3) ))/dx(3))
            enddo

            n = size(obj%ElemNod,1)
            NumNodePerElem = size(obj%ElemNod,2)
            
        
            if(NumNodePerElem==4)then
                num_face = 4
                allocate(obj%FacetElemNod(NumOfElem*4,3),id(3),idr(3) )
                do i=1,size(obj%ElemNod,1)
                    obj%FacetElemNod(  (i-1)*4+1 ,1) = obj%ElemNod(i,3)
                    obj%FacetElemNod(  (i-1)*4+1 ,2) = obj%ElemNod(i,2)
                    obj%FacetElemNod(  (i-1)*4+1 ,3) = obj%ElemNod(i,1)
                    
                    obj%FacetElemNod(  (i-1)*4+2 ,1) = obj%ElemNod(i,1)
                    obj%FacetElemNod(  (i-1)*4+2 ,2) = obj%ElemNod(i,2)
                    obj%FacetElemNod(  (i-1)*4+2 ,3) = obj%ElemNod(i,4)
                    
                    obj%FacetElemNod(  (i-1)*4+3 ,1) = obj%ElemNod(i,2)
                    obj%FacetElemNod(  (i-1)*4+3 ,2) = obj%ElemNod(i,3)
                    obj%FacetElemNod(  (i-1)*4+3 ,3) = obj%ElemNod(i,4)
                    
                    obj%FacetElemNod(  (i-1)*4+4 ,1) = obj%ElemNod(i,3)
                    obj%FacetElemNod(  (i-1)*4+4 ,2) = obj%ElemNod(i,1)
                    obj%FacetElemNod(  (i-1)*4+4 ,3) = obj%ElemNod(i,4)
                enddo
            elseif(NumNodePerElem==8)then
                num_face = 6
                allocate(obj%FacetElemNod(NumOfElem*6,4),id(4),idr(4) )
                do i=1,size(obj%ElemNod,1)
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
                enddo
            else
                stop "ERROR :: GetFacetElement :: only for  Hexahedral/tetrahedron ##"
            endif
            allocate(overlap(size(obj%FacetElemNod,1) ) )
            overlap(:) = .false.
            
            id = int( zeros(size(obj%FacetElemNod,2) )  )
            idr= int( zeros(size(obj%FacetElemNod,2) )  )

            ! Most time-consuming part
            elementID_I=0
            do i=1,size(overlap)-1
                if(mod(i-1,num_face)==0 )then
                    elementID_I = elementID_I + 1
                endif

                if(overlap(i) ) cycle
                ! 全然違うやつをすばやく弾きたい
                elementID_J = elementID_I
                do j=i+1,size(overlap)
                    if(mod(j-1,num_face)==0 )then
                        elementID_J = elementID_J + 1
                    endif
                    if( abs(ElementGroup(elementID_I,1)-ElementGroup(elementID_J,1))>=2 ) cycle
                    if( abs(ElementGroup(elementID_I,2)-ElementGroup(elementID_J,2))>=2 ) cycle
                    if( abs(ElementGroup(elementID_I,3)-ElementGroup(elementID_J,3))>=2 ) cycle

                    id = obj%FacetElemNod(i,:)
                    idr= obj%FacetElemNod(j,:)
                    if( sameAsGroup(id,idr) )then
                        overlap(i) = .true.
                        overlap(j) = .true.
                        exit
                    endif
                enddo
            enddo
            ! to here.
            
            j = 0
            do i=1,size(overlap)
                if(.not.overlap(i) )then
                    j = j+1
                endif    
            enddo
            buffer = obj%FacetElemNod
            obj%FacetElemNod = int(zeros( j,size(buffer,2) ) )
            j=0
            do i=1,size(overlap)
                if(.not.overlap(i) )then
                    j = j+1
                    obj%FacetElemNod(j,:) = buffer(i,:)
                endif    
            enddo
            return
        endif

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
    integer(int32) :: i,j,k,n
    integer(int32) :: NumOfElem,NumOfDim,NumNodePerElem
    integer(int32) :: id_1,id_2
    integer(int32),allocatable::buffer(:,:)


    NumOfElem = size(obj%ElemNod,1) 
    NumOfDim  = size(obj%NodCoord,2)
    NumNodePerElem = size(obj%ElemNod,2)

    If(NumOfDim /= 2) then
        obj%ErrorMsg = "ERROR::GetFaceElement.f90 >> NumOfDim /= 2"
        stop 
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
    integer(int32) :: i,j,k,n
    integer(int32) :: NumOfElem,NumOfDim,NumNodePerElem
    integer(int32) :: id_1,id_2
    integer(int32),allocatable::buffer(:,:)


    if(allocated(obj%FacetElemNod) ) then
        deallocate(obj%FacetElemNod)
    endif
    if(allocated(obj%NextFacets) ) then
        deallocate(obj%NextFacets)
    endif
    if(allocated(obj%SurfaceLine2D) ) then
        deallocate(obj%SurfaceLine2D)
    endif
!    if(allocated(obj%SubMeshNodFromTo) ) then
!        deallocate(obj%SubMeshNodFromTo)
!    endif
!    if(allocated(obj%SubMeshElemFromTo) ) then
!        deallocate(obj%SubMeshElemFromTo)
!    endif
    if(allocated(obj%SubMeshSurfFromTo) ) then
        deallocate(obj%SubMeshSurfFromTo)
    endif
        

    NumOfDim=size(obj%NodCoord,2)
    if(NumOfDim==2)then
        call GetSurface2D(obj)
        obj%surface=1
    elseif(NumOfDim==3)then
        call GetFacetElement(obj)

        call GetNextFacets(obj)
        obj%surface=1

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
    integer(int32),optional,intent(inout)::err
    integer(int32) :: i,j,n,ierr


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

    integer(int32) :: i,j,n,felem_num,felemnod_num,dim_num
    integer(int32),allocatable::node_id_list(:)

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

    real(real64),allocatable::max_coord(:),min_coord(:)
    integer(int32) :: dim_num,i


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
        allocate(BBox%ElemMat(1) )
        BBox%ElemMat(1)=1
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
        allocate(BBox%ElemMat(1) )
        BBox%ElemMat(1)=1

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
    integer(int32) i,j,n,dim_num,s_elem_num,count_s_elem_num,c_or_not,k,mm
    real(real64) ::max_obj,max_bb,min_obj,min_bb

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

    real(real64),allocatable::width1(:),width2(:),center1(:),center2(:),max_coord(:),min_coord(:),&
        x1_max(:),x1_min(:),x2_max(:),x2_min(:),center(:)
    real(real64) :: xmax_(2),xmin_(2)
    integer(int32) :: dim_num,i,j,c_or_not

    ! check contact
    dim_num=size(obj1%nodcoord,2)
    
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


!    ! Detect intersection by nodes
!    dim_num=size(obj1%NodCoord,2)
!    allocate(x1_max(dim_num),x2_max(dim_num),x1_min(dim_num),x2_min(dim_num),center(dim_num) )
!    do i=1,dim_num
!        x1_max(i) = maxval(obj1%nodcoord(:,i) )
!        x1_min(i) = minval(obj1%nodcoord(:,i) )
!        x2_max(i) = maxval(obj2%nodcoord(:,i) )
!        x2_min(i) = minval(obj2%nodcoord(:,i) )
!    enddo
!    center(:)  = 0.50d0*center1(:)+ 0.50d0*center1(:)
!
!    c_or_not = 0 ! default :: contact
!    do i=1,dim_num
!        if(center() )
!    enddo
!   
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
    integer(int32),allocatable::buffer(:)
    integer(int32) :: i,j,n,node_id,k,l,m
    
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
                    stop "MeshClass >> GetNextFacets >> invalid i,n"
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
subroutine addMesh(obj,mesh,from,length,rot_x,rot_y,rot_z,x,y,z,dx,dy,dz)
    class(Mesh_),intent(inout) :: obj
    class(Mesh_),optional,intent(inout)    :: mesh
    integer(int32),optional,intent(in) :: from
    real(real64),optional,intent(in) :: length,rot_x,rot_y,rot_z,x,y,z,dx,dy,dz
    integer(int32) :: NumOfElem,node_id,elem_id
    real(real64) :: n(3),rotmat(3,3),L

    if(obj%meshtype == "Root" .or. obj%meshtype == "root")then
        ! add node
        node_id = size(obj%nodcoord,1)
        elem_id=size(obj%elemnod,1)
        call extendArray(obj%nodcoord,extend1stColumn=.true.)
        call extendArray(obj%elemnod,extend1stColumn=.true.)

        n(:) = 0.0d0
        n(3) = -1.0d0

        if(present(rot_x) )then
		    rotmat(1,1)=1.0d0	;rotmat(1,2)=0.0d0		;rotmat(1,3)=0.0d0			;
		    rotmat(2,1)=0.0d0	;rotmat(2,2)=cos(rot_x)		;rotmat(2,3)=-sin(rot_x);
		    rotmat(3,1)=0.0d0	;rotmat(3,2)=sin(rot_x)		;rotmat(3,3)= cos(rot_x);
            n(:) = matmul(rotmat,n)
        endif
        if(present(rot_y) )then    
			rotmat(1,1)=cos(rot_y)	;rotmat(1,2)=0.0d0		;rotmat(1,3)=sin(rot_y)		;
			rotmat(2,1)=0.0d0	;rotmat(2,2)=1.0d0		;rotmat(2,3)=0.0d0		;
			rotmat(3,1)=-sin(rot_y)	;rotmat(3,2)=0.0d0		;rotmat(3,3)= cos(rot_y)    ;
            n(:) = matmul(rotmat,n)
        endif
        if(present(rot_z) )then
			rotmat(1,1)=cos(rot_z)	;rotmat(1,2)=-sin(rot_z)	;rotmat(1,3)=0.0d0		;
			rotmat(2,1)=sin(rot_z)	;rotmat(2,2)=cos(rot_z)		;rotmat(2,3)=0.0d0		;
			rotmat(3,1)=0.0d0	;rotmat(3,2)=0.0d0		;rotmat(3,3)=1.0d0 		;
            n(:) = matmul(rotmat,n)
        endif

        ! Or you can directly identify new node by coordinate
        n(1) = input(default=n(1), option=dx )
        n(2) = input(default=n(2), option=dy )
        n(3) = input(default=n(3), option=dz )


        L = input(default=1.0d0,option=length)
        if(present(from) )then
            obj%nodcoord(node_id+1,:) = obj%nodcoord(From,:) + L*n(:) 
            
            obj%nodcoord(node_id+1,1) = input(default=obj%nodcoord(node_id+1,1), option=x )
            obj%nodcoord(node_id+1,2) = input(default=obj%nodcoord(node_id+1,2), option=y )
            obj%nodcoord(node_id+1,3) = input(default=obj%nodcoord(node_id+1,3), option=z )
            obj%elemnod(elem_id+1,1)  = From
            obj%elemnod(elem_id+1,2:)  = node_id+1
        else
            obj%nodcoord(node_id+1,:) = obj%nodcoord(node_id,:) + L*n(:) 
            
            obj%nodcoord(node_id+1,1) = input(default=obj%nodcoord(node_id+1,1), option=x )
            obj%nodcoord(node_id+1,2) = input(default=obj%nodcoord(node_id+1,2), option=y )
            obj%nodcoord(node_id+1,3) = input(default=obj%nodcoord(node_id+1,3), option=z )
            obj%elemnod(elem_id+1,1)  = node_id
            obj%elemnod(elem_id+1,2:)  = node_id+1
        endif

        return
    endif

    NumOfElem=size(obj%ElemNod,1)
    call addarray(obj%NodCoord,mesh%NodCoord)
    call addarray(obj%ElemNod,mesh%ElemNod)
    call addarray(obj%ElemMat,mesh%ElemMat)
    obj%ElemNod(NumOfElem+1:,:)=obj%ElemNod(NumOfElem+1:,:)+size(obj%NodCoord,1)

end subroutine
!##################################################






!##################################################
subroutine MergeMesh(inobj1,inobj2,outobj)
    class(Mesh_),intent(in) ::inobj1,inobj2
    class(Mesh_),intent(out)::outobj
    integer(int32) node_num1,num1,num2,num3
    integer(int32) i,j,k
    
    !MergeObjects if the array is allocated.

    ! ========= Merge nodes  ============
    num1=size(inobj1%NodCoord,1)
    node_num1=num1
    num2=size(inobj2%NodCoord,1)
    num3=size(inobj2%NodCoord,2)
    if(num3 /= size(inobj1%NodCoord,1) )then
        outobj%ErrorMsg="MergeMesh >> num3 /= inobj1%NodCoord,1"
    endif

    allocate(outobj%NodCoord(num1+num2, num3))
    do i=1,num1
        outobj%NodCoord(i,:)=inobj1%NodCoord(i,:)
    enddo
    do i=1,num2
        outobj%NodCoord(i+num1,:)=inobj2%NodCoord(i,:)
    enddo

    ! update subdomain infomation
    if(allocated(inobj1%SubMeshNodFromTo) )then
        if(allocated(inobj2%SubMeshNodFromTo) )then
            if(allocated(outobj%SubMeshNodFromTo) )then
                deallocate(outobj%SubMeshNodFromTo)
            endif
            allocate(outobj%SubMeshNodFromTo(2,3) )
            outobj%SubMeshNodFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshNodFromTo(1,2)=1
            outobj%SubMeshNodFromTo(1,3)=num1
            
            outobj%SubMeshNodFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshNodFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshNodFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshNodFromTo) )then
                deallocate(outobj%SubMeshNodFromTo)
            endif
            allocate(outobj%SubMeshNodFromTo(2,3) )
            outobj%SubMeshNodFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshNodFromTo(1,2)=1
            outobj%SubMeshNodFromTo(1,3)=num1
            
            outobj%SubMeshNodFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshNodFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshNodFromTo(2,3)=num1+num2 !node id goes to
        endif    
    else
        if(allocated(inobj2%SubMeshNodFromTo) )then
            if(allocated(outobj%SubMeshNodFromTo) )then
                deallocate(outobj%SubMeshNodFromTo)
            endif
            allocate(outobj%SubMeshNodFromTo(2,3) )
            outobj%SubMeshNodFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshNodFromTo(1,2)=1
            outobj%SubMeshNodFromTo(1,3)=num1
            
            outobj%SubMeshNodFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshNodFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshNodFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshNodFromTo) )then
                deallocate(outobj%SubMeshNodFromTo)
            endif
            allocate(outobj%SubMeshNodFromTo(2,3) )
            outobj%SubMeshNodFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshNodFromTo(1,2)=1
            outobj%SubMeshNodFromTo(1,3)=num1
            
            outobj%SubMeshNodFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshNodFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshNodFromTo(2,3)=num1+num2 !node id goes to
        endif    
    endif
    ! ========= Merge nodes  ============
    


    ! ========= Merge elements  ============
    num1=size(inobj1%ElemNod,1)
    num2=size(inobj2%ElemNod,1)
    num3=size(inobj2%ElemNod,2)
    if(num3 /= size(inobj1%ElemNod,1) )then
        outobj%ErrorMsg="MergeMesh >> num3 /= inobj1%ElemNod,1"
    endif

    allocate(outobj%ElemNod(num1+num2, num3))
    do i=1,num1
        outobj%ElemNod(i,:)=inobj1%ElemNod(i,:)
    enddo
    do i=1,num2
        outobj%ElemNod(i+num1,:)=inobj2%ElemNod(i,:)+node_num1
    enddo
    ! update subdomain infomation
    if(allocated(inobj1%SubMeshElemFromTo) )then
        if(allocated(inobj2%SubMeshElemFromTo) )then
            if(allocated(outobj%SubMeshElemFromTo) )then
                deallocate(outobj%SubMeshElemFromTo)
            endif
            allocate(outobj%SubMeshElemFromTo(2,3) )
            outobj%SubMeshElemFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshElemFromTo(1,2)=1
            outobj%SubMeshElemFromTo(1,3)=num1
            
            outobj%SubMeshElemFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshElemFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshElemFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshElemFromTo) )then
                deallocate(outobj%SubMeshElemFromTo)
            endif
            allocate(outobj%SubMeshElemFromTo(2,3) )
            outobj%SubMeshElemFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshElemFromTo(1,2)=1
            outobj%SubMeshElemFromTo(1,3)=num1
            
            outobj%SubMeshElemFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshElemFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshElemFromTo(2,3)=num1+num2 !node id goes to
        endif    
    else
        if(allocated(inobj2%SubMeshElemFromTo) )then
            if(allocated(outobj%SubMeshElemFromTo) )then
                deallocate(outobj%SubMeshElemFromTo)
            endif
            allocate(outobj%SubMeshElemFromTo(2,3) )
            outobj%SubMeshElemFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshElemFromTo(1,2)=1
            outobj%SubMeshElemFromTo(1,3)=num1
            
            outobj%SubMeshElemFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshElemFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshElemFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshElemFromTo) )then
                deallocate(outobj%SubMeshElemFromTo)
            endif
            allocate(outobj%SubMeshElemFromTo(2,3) )
            outobj%SubMeshElemFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshElemFromTo(1,2)=1
            outobj%SubMeshElemFromTo(1,3)=num1
            
            outobj%SubMeshElemFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshElemFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshElemFromTo(2,3)=num1+num2 !node id goes to
        endif    
    endif
    ! ========= Merge elements  ============
    

    ! ========= Merge Facet Elements  ============
    num1=size(inobj1%FacetElemNod,1)
    num2=size(inobj2%FacetElemNod,1)
    num3=size(inobj2%FacetElemNod,2)
    if(num3 /= size(inobj1%FacetElemNod,1) )then
        outobj%ErrorMsg="MergeMesh >> num3 /= inobj1%ElemNod,1"
    endif

    allocate(outobj%FacetElemNod(num1+num2, num3))
    do i=1,num1
        outobj%FacetElemNod(i,:)=inobj1%FacetElemNod(i,:)
    enddo
    do i=1,num2
        outobj%FacetElemNod(i+num1,:)=inobj2%FacetElemNod(i,:)+node_num1
    enddo

    
    ! ========= Merge Facet Elements  ============


    ! ========= Merge surface elements  ============
    num1=size(inobj1%SurfaceLine2D,1)
    num2=size(inobj2%SurfaceLine2D,1)
    
    allocate(outobj%SurfaceLine2D(num1+num2))
    do i=1,num1
        outobj%SurfaceLine2D(i)=inobj1%SurfaceLine2D(i)
    enddo
    do i=1,num2
        outobj%SurfaceLine2D(i+num1)=inobj2%SurfaceLine2D(i)+node_num1
    enddo

    ! update subdomain infomation
    if(allocated(inobj1%SubMeshSurfFromTo) )then
        if(allocated(inobj2%SubMeshSurfFromTo) )then
            if(allocated(outobj%SubMeshSurfFromTo) )then
                deallocate(outobj%SubMeshSurfFromTo)
            endif
            allocate(outobj%SubMeshSurfFromTo(2,3) )
            outobj%SubMeshSurfFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshSurfFromTo(1,2)=1
            outobj%SubMeshSurfFromTo(1,3)=num1
            
            outobj%SubMeshSurfFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshSurfFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshSurfFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshSurfFromTo) )then
                deallocate(outobj%SubMeshSurfFromTo)
            endif
            allocate(outobj%SubMeshSurfFromTo(2,3) )
            outobj%SubMeshSurfFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshSurfFromTo(1,2)=1
            outobj%SubMeshSurfFromTo(1,3)=num1
            
            outobj%SubMeshSurfFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshSurfFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshSurfFromTo(2,3)=num1+num2 !node id goes to
        endif    
    else
        if(allocated(inobj2%SubMeshSurfFromTo) )then
            if(allocated(outobj%SubMeshSurfFromTo) )then
                deallocate(outobj%SubMeshSurfFromTo)
            endif
            allocate(outobj%SubMeshSurfFromTo(2,3) )
            outobj%SubMeshSurfFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshSurfFromTo(1,2)=1
            outobj%SubMeshSurfFromTo(1,3)=num1
            
            outobj%SubMeshSurfFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshSurfFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshSurfFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshSurfFromTo) )then
                deallocate(outobj%SubMeshSurfFromTo)
            endif
            allocate(outobj%SubMeshSurfFromTo(2,3) )
            outobj%SubMeshSurfFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshSurfFromTo(1,2)=1
            outobj%SubMeshSurfFromTo(1,3)=num1
            
            outobj%SubMeshSurfFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshSurfFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshSurfFromTo(2,3)=num1+num2 !node id goes to
        endif    
    endif
    ! ========= Merge surface elements  ============


    ! ========= Merge Material ID ==================
    num1=size(inobj1%ElemMat,1)
    num2=size(inobj2%ElemMat,1)
    if(num3 /= size(inobj1%ElemMat,1) )then
        outobj%ErrorMsg="MergeMesh >> num3 /= inobj1%ElemMat,1"
    endif

    allocate(outobj%ElemMat(num1+num2))
    do i=1,num1
        outobj%ElemMat(i)=inobj1%ElemMat(i)
    enddo
    do i=1,num2
        outobj%ElemMat(i+num1)=inobj2%ElemMat(i)+Maxval(inobj1%ElemMat)
    enddo
    ! ========= Merge Material ID ==================
    

end subroutine MergeMesh
!##################################################



!##################################################
subroutine ExportElemNod(obj,elem_nod)
    class(Mesh_),intent(inout)::obj
    integer(int32),allocatable,intent(inout)::elem_nod(:,:)
    if(allocated(elem_nod ))then
        deallocate(elem_nod)
    endif
    allocate(elem_nod(size(obj%ElemNod,1),size(obj%ElemNod,2) ) )
    elem_nod(:,:)=obj%ElemNod(:,:)
end subroutine ExportElemNod
!##################################################

!##################################################
subroutine ExportNodCoord(obj,nod_coord)
    class(Mesh_),intent(inout)::obj
    real(real64),allocatable,intent(inout)::nod_coord(:,:)

    if(allocated(nod_coord) )then
        deallocate(nod_coord)
    endif
    allocate(nod_coord(size(obj%NodCoord,1),size(obj%NodCoord,2) ) )
    nod_coord(:,:)=obj%NodCoord(:,:)
    

end subroutine ExportNodCoord
!##################################################



!##################################################
subroutine ExportSurface2D(obj,surface_nod)
    class(Mesh_),intent(inout)::obj
    integer(int32),allocatable,intent(inout)::surface_nod(:)

    if(allocated(surface_nod) )then
        deallocate(surface_nod)
    endif
    allocate(surface_nod(size(obj%SurfaceLine2D,1) ) )
    surface_nod(:)=obj%SurfaceLine2D(:)

end subroutine ExportSurface2D
!##################################################


!##################################################
subroutine DisplayMesh(obj,OptionalFolderName,OptionalFormat,FileHandle,Name)
    class(Mesh_),intent(inout)::obj
    character(*),optional,intent(in):: OptionalFolderName
    character(*),optional,intent(in) :: OptionalFormat,Name
    integer(int32),optional,intent(in) :: FileHandle
    integer(int32) :: fh
    character*70 DefaultFolderName
    character*70 FolderName
    character*76 command_mkdir 
    character*86 surfaceout
    integer i,j,node_ID,node_ID_next,k

    fh=input(default=10,option=FileHandle)
    if(present(Name) )then
        open(fh,file=Name)
        if(.not.allocated(obj%ElemNod) )then
            print *, "DisplayMesh :: Error >> mesh-connectivity is not allocated."
            return
        endif
        do i=1,size(obj%ElemNod,1)
            do j=1,size(obj%ElemNod,2)
                write(fh,*) obj%NodCoord(obj%ElemNod(i,j),:)
            enddo
            write(fh,*) obj%NodCoord(obj%ElemNod(i,1),:)
            write(fh,*) "  "
        enddo
        close(fh)
        return
    endif

    
if(present(OptionalFormat) )then
    if(trim(OptionalFormat)==".gp")then
        ! Export Mesh as .gp
        open(102,file="SurfaceLine2D.txt")
        ! Surface line
        do i=1,size(obj%SubMeshSurfFromTo,1)
            do j=obj%SubMeshSurfFromTo(i,2),obj%SubMeshSurfFromTo(i,3)-1
                node_ID     =obj%SurfaceLine2D(j)
                node_ID_next=obj%SurfaceLine2D(j+1)
                write(102,*) obj%NodCoord(node_ID,:),&
                    obj%NodCoord(node_ID_next,:)-obj%NodCoord(node_ID,:) 
            enddo
            node_ID     =obj%SurfaceLine2D(obj%SubMeshSurfFromTo(i,3))
            node_ID_next=obj%SurfaceLine2D(obj%SubMeshSurfFromTo(i,2))
            write(102,*) obj%NodCoord(node_ID,:),&
                obj%NodCoord(node_ID_next,:)-obj%NodCoord(node_ID,:) 
            
            write(102,*) "  "
        enddo
        close(102)
        open(102,file="SurfaceLine2D.gp")
        write(102,*) "plot 'SurfaceLine2D.txt' with vector "
        write(102,*) "pause -1"
        close(102)
        call execute_command_line("gnuplot SurfaceLine2D.gp")
    endif
endif
if(present(OptionalFormat) )then
    if(trim(OptionalFormat)==".gp")then
        ! Export Mesh as .gp
        open(102,file="ElemLine2D.txt")
        
        ! Elemace line
        do i=1,size(obj%SubMeshElemFromTo,1)
            do j=obj%SubMeshElemFromTo(i,2),obj%SubMeshElemFromTo(i,3)
                do k=1,size(obj%ElemNod,2)-1
                    write(102, * ) obj%NodCoord(obj%ElemNod(j,k),:),&
                        obj%NodCoord(obj%ElemNod(j,k+1),:)-obj%NodCoord(obj%ElemNod(j,k),:)
                enddo
                write(102,*) obj%NodCoord(obj%ElemNod(j,size(obj%ElemNod,2)),:),&
                    obj%NodCoord(obj%ElemNod(j,1),:)&
                    -obj%NodCoord(obj%ElemNod(j,size(obj%ElemNod,2)),:)
            enddo
            write(102,*) "  "
        enddo
        close(102)
        open(102,file="ElemLine2D.gp")
        write(102,*) "plot 'ElemLine2D.txt' with vector "
        write(102,*) "pause -1"
        close(102)
        call execute_command_line("gnuplot ElemLine2D.gp")

        return
    endif
endif



DefaultFolderName="DisplaySurface"
if(present(OptionalFolderName) )then
    FolderName=OptionalFolderName
else
    FolderName=DefaultFolderName
endif
command_mkdir ="mkdir -p " // trim(FolderName)
command_mkdir =trim(command_mkdir )

call execute_command_line(command_mkdir )
surfaceout=trim(FolderName)//"/surface_nod.txt"
surfaceout=trim(surfaceout)
open(100,file=surfaceout)

do i=1,size(obj%SurfaceLine2D,1)
    
    write(100,*) obj%NodCoord(obj%SurfaceLine2D(i),: )
enddo
close(100)

surfaceout=trim(FolderName)//"/surface_ids.txt"
surfaceout=trim(surfaceout)
open(100,file=surfaceout)

do i=1,size(obj%SurfaceLine2D,1)
    
    write(100,*) obj%NodCoord(obj%SurfaceLine2D(i),: )
enddo
close(100)

surfaceout=trim(FolderName)//"/element_nod.txt"
surfaceout=trim(surfaceout)
open(100,file=surfaceout)

do i=1,size(obj%SurfaceLine2D,1)
    
    write(100,*) obj%NodCoord(obj%SurfaceLine2D(i),: )
enddo
close(100)

    
end subroutine DisplayMesh
!##################################################

!##################################################
subroutine ShowMesh(obj,FileHandle,OnlySurface) 
    class(Mesh_),intent(inout)::obj
    integer(int32),optional,intent(in)::FileHandle
    logical,optional,intent(in)::OnlySurface
    logical :: no_fh
    integer(int32) :: i,j,fh,n,m,exp_mode
    
    
    
    if( present(FileHandle) )then
        fh=FileHandle
        no_fh = .false.
    else
        no_fh = .true.
    endif

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
    integer(int32),optional,intent(in)::ItrTol
    
    integer(int32) :: itr,i,j,k,l,n,m,EndStep,dnum,dnum_init,nodeid,fnodeid

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

        
    enddo

    

end subroutine
!##################################################


!##################################################
function getNumOfDomainMesh(obj,ItrTol) result(dnum)
    class(Mesh_),intent(inout)::obj
    integer(int32),optional,intent(in)::ItrTol
    integer(int32),allocatable :: domain_id(:), domain_id_ref(:),node_id(:)

    integer(int32) :: itr,i,j,k,l,n,m,node,cnode,itrmax,dnum

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

    integer(int32) :: i,j,n,m,a1,a2,id
    real(real64),allocatable :: buf(:)
    ! SortFacet
    n=size(obj%NodCoord,2)
    if(n==2)then
        if(.not.allocated(obj%FacetElemNod) )then
            
            !"  SortFacetMesh >> for 3D, now implementing "

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
        !print *, "ERROR :: SortFacetMesh >> for 3D, now implementing "
        return
    endif


end subroutine
!##################################################


!##################################################
subroutine MeshingMesh(obj,Mode,itr_tol,delaunay2d)
    class(Mesh_),intent(inout)::obj
    type(Mesh_) :: box
    type(triangle_)::tri
    type(circle_)::cir
    logical,optional,intent(in) :: delaunay2d
    integer(int32),optional,intent(in) :: Mode,itr_tol
    integer(int32) :: i,j,k,n,m,node_num,dim_num,dim_mode,itr
    real(real64),allocatable :: stage_range(:,:),triangle(:,:),nodcoord(:,:)
    integer(int32),allocatable :: staged_node(:),lapl_node(:),&
        neighbornode(:),ElementElementConnect(:,:),elemnod(:,:)
    real(real64) :: centerx,centery,centerz,radius
    logical :: NoChange

    ! This method creates mesh-connectivity for the given nodal coordinates.
    ! Therefore, Mesh%NodCoord(:,:) should be filled preliminary.

    dim_mode=input(default=2,option=Mode)
    if(dim_mode==2 .or. present(delaunay2d))then
        if(.not. allocated(obj%NodCoord) )then
            print *, "ERROR :: MeshClass MeshingMesh"
            print *, "This method creates mesh-connectivity for the given nodal coordinates."
            print *, "Therefore, Mesh%NodCoord(:,:) should be filled preliminary."
            return 
        endif
        print *, "Meshing sequence is started."
        if(.not. delaunay2d)then
            return
        endif

        node_num=size(obj%NodCoord,1)
        dim_num =size(obj%NodCoord,2)

        call obj%arrangeNodeOrder()
        call obj%getCircumscribedTriangle(triangle)

        if(allocated(obj%ElemNod) )then
            deallocate(obj%ElemNod)
        endif
        allocate(obj%ElemNod(node_num*2,3) )
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
            !write(123,*) obj%NodCoord(obj%ElemNod(k,1),:),obj%NodCoord(obj%ElemNod(k,2),:)-obj%NodCoord(obj%ElemNod(k,1),:)
            !write(123,*) obj%NodCoord(obj%ElemNod(k,2),:),obj%NodCoord(obj%ElemNod(k,3),:)-obj%NodCoord(obj%ElemNod(k,2),:)
            !write(123,*) obj%NodCoord(obj%ElemNod(k,3),:),obj%NodCoord(obj%ElemNod(k,1),:)-obj%NodCoord(obj%ElemNod(k,3),:)
            !writE(123,*) " "
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

        ! Laplacian method
        call obj%getSurface()

        call obj%Laplacian(itr_tol=itr_tol)
        print *, "Meshing is successfully done based on Delauney 2D"


    elseif(dim_mode==3)then
        ! divide mesh by delauney
        ! step #0: check data quality
        if(.not. allocated(obj%NodCoord) )then
            print *, "ERROR :: MeshClass MeshingMesh"
            print *, "This method creates mesh-connectivity for the given nodal coordinates."
            print *, "Therefore, Mesh%NodCoord(:,:) should be filled preliminary."
            return 
        endif
        print *, "Meshing sequence is started."

        node_num=size(obj%NodCoord,1)
        dim_num =size(obj%NodCoord,2)

        ! arrange node order from outer to inner.
        call obj%arrangeNodeOrder()

        ! step #1: get Curcumscribed Box
        call obj%getCircumscribedBox(box)

        ! prepare connectivity
        if(allocated(obj%ElemNod) )then
            deallocate(obj%ElemNod)
        endif
        

        obj%ElemNod = box%elemnod
        obj%ElemNod(:,:) =obj%ElemNod(:,:) + node_num 
!        staged_node(:)=0
!        staged_node(node_num+1)=1
!        staged_node(node_num+2)=1
!        staged_node(node_num+3)=1

        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        call ExtendArrayReal(obj%NodCoord,extend1stColumn=.true.,DefaultValue=0.0d0)
        obj%NodCoord(node_num+1,:)=box%NodCoord(1,:)
        obj%NodCoord(node_num+2,:)=box%NodCoord(2,:)
        obj%NodCoord(node_num+3,:)=box%NodCoord(3,:)
        obj%NodCoord(node_num+4,:)=box%NodCoord(4,:)
        obj%NodCoord(node_num+5,:)=box%NodCoord(5,:)
        obj%NodCoord(node_num+6,:)=box%NodCoord(6,:)
        obj%NodCoord(node_num+7,:)=box%NodCoord(7,:)
        obj%NodCoord(node_num+8,:)=box%NodCoord(8,:)

        do i=1,node_num
            ! Delauney triangulation for 2D
            print *, i,"/",size(obj%NodCoord,1)," :: ",dble(i)/dble(size(obj%NodCoord,1))*100,"% done."
            ! some bugs.
            call obj%DelauneygetNewNode3D(NodeID=i)
            print *, "Under debugging >> call obj%DelauneygetNewNode3D(NodeID=i)"
            if(i==1)then
                return
            endif
        enddo


        ! Remove outer box
        ! 最初に作った，全体を覆うスーパーボックスを取り除く


        ! 以上により，Delauney分割を完了する．

        print *, "Flipping algorithm is to be implemented."
        print *, "3D-Delaunay :: trial version. it may have some bugs."
        return
        
    else
        print *, "ERROR :: MeshClass :: MeshingMesh :: Dimension = ",dim_mode
    endif
    

end subroutine
!##################################################


subroutine LaplacianMesh(obj,itr_tol)
    class(Mesh_),intent(inout) ::  obj
    integer(int32),optional,intent(in) :: itr_tol
    integer(int32) :: i ,j, k, itr
    integer(int32),allocatable :: lapl_node(:),neighbornode(:)
    
    ! Laplacian method

    call obj%getSurface()
    lapl_node = int(zeros( size(obj%nodcoord,1) ) )
    do i=1,size(obj%SurfaceLine2D)
        lapl_node( obj%SurfaceLine2D(i) ) = -1
    enddo

    itr = input(default=10, option=itr_tol)
    do i=1,itr
        do j=1,size(lapl_node)
            if(lapl_node(j)==0 )then
                ! not boundary node => move node
                neighbornode = obj%getNeighboringNode(NodeId=j)
                obj%nodcoord(j,:) = 0.0d0
                do k=1,size(neighbornode)
                    obj%nodcoord( j ,:) =&
                    obj%nodcoord( j ,:) + &
                    1.0d0/dble(size(neighbornode))*obj%nodcoord( neighbornode(k) ,:) 
                enddo
            else
                cycle
            endif
        enddo
    enddo

end subroutine

!##################################################
subroutine getCircumscribedCircleMesh(obj,centerx,centery,centerz,radius)
    class(Mesh_),intent(inout)::obj
    real(real64),intent(out)::centerx,centery,centerz,radius
    real(real64),allocatable::center(:)
    real(real64) :: dist
    integer(int32) ::i
    
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

function getElementMesh(obj,ElementID) result(element)
    class(Mesh_),intent(in) :: obj
    type(Mesh_) :: element
    integer(int32),intent(in) :: ElementID
    integer(int32) :: i,j,n,m

    n = size(obj%nodcoord,2)
    m = size(obj%elemnod,2)

    allocate(element%nodcoord(m,n) )
    allocate(element%elemnod(1,m) )

    do i=1,m
        element%nodcoord(i,:) = obj%nodcoord(obj%elemnod(ElementID,i),:)
        element%elemnod(1,i) = i
    enddo

end function
!##################################################

subroutine getCircumscribedSphereOfTetraMesh(obj,center,radius)
    class(Mesh_),intent(in)::obj
    real(real64),intent(inout) :: center(3), radius
    real(real64) ::i, Matrix(3,3),N1(3),N2(3),N3(3),&
        a1(3),a2(3),a3(3),a4(3),M13(3),M14(3),M24(3),M12(3),p(3),Matrix_Inv(3,3)

    a1(:) = obj%nodcoord(1,:)
    a2(:) = obj%nodcoord(2,:)
    a3(:) = obj%nodcoord(3,:)
    a4(:) = obj%nodcoord(4,:)

    M13 = 0.50d0*a1 +  0.50d0*a3
    M14 = 0.50d0*a1 +  0.50d0*a4
    M12 = 0.50d0*a1 +  0.50d0*a2

    N1 = a1 - M13
    N2 = a1 - M14
    N3 = a1 - M12

    p(1) = dot_product(M13,N1)
    p(2) = dot_product(M14,N2)
    p(3) = dot_product(M12,N3)

    Matrix(1,:) = N1(:)
    Matrix(2,:) = N2(:)
    Matrix(3,:) = N3(:) 

    Matrix_Inv = inverse(Matrix)

    center = matmul(Matrix_Inv,p)

    radius = sqrt(dot_product(center-a1,center-a1) )

end subroutine


!##################################################
subroutine getCircumscribedSphereMesh(obj,centerx,centery,centerz,radius)
    class(Mesh_),intent(inout)::obj
    real(real64),intent(out)::centerx,centery,centerz,radius
    real(real64),allocatable::center(:)
    real(real64) :: dist
    integer(int32) ::i
    
    allocate(center( 3 ) )
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

    

    centerx=center(1)
    centery=center(2)
    centerz=center(3)
    

end subroutine
!##################################################



!##################################################
subroutine getCircumscribedTriangleMesh(obj,triangle)
    class(Mesh_),intent(inout)::obj
    real(real64),allocatable :: center(:)
    real(real64),allocatable,intent(out) :: triangle(:,:)
    real(real64) :: centerx,centery,centerz,radius,pi
    integer(int32) :: i

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
subroutine getCircumscribedBoxMesh(obj,Box)
    class(Mesh_),intent(inout)::obj
    type(Mesh_),intent(inout) :: box
    real(real64),allocatable :: center(:)
    real(real64) :: centerx,centery,centerz,radius,pi
    integer(int32) :: i

    pi=3.1415926d0

    allocate(Box%nodcoord(8,3 ))
    allocate(Box%elemnod(5,4))
    allocate(center(3))

    call obj%getCircumscribedSphere(centerx,centery,centerz,radius)
    
    radius=radius*(1.20d0)
    center(1)=centerx
    center(2)=centery
    center(3)=centerz

    Box%nodcoord(1,1)=centerx - radius ; Box%nodcoord(1,2)=centery - radius ; Box%nodcoord(1,3)=centerz - radius ; 
    Box%nodcoord(2,1)=centerx + radius ; Box%nodcoord(2,2)=centery - radius ; Box%nodcoord(2,3)=centerz - radius ; 
    Box%nodcoord(3,1)=centerx + radius ; Box%nodcoord(3,2)=centery + radius ; Box%nodcoord(3,3)=centerz - radius ; 
    Box%nodcoord(4,1)=centerx - radius ; Box%nodcoord(4,2)=centery + radius ; Box%nodcoord(4,3)=centerz - radius ; 
    Box%nodcoord(5,1)=centerx - radius ; Box%nodcoord(5,2)=centery - radius ; Box%nodcoord(5,3)=centerz + radius ; 
    Box%nodcoord(6,1)=centerx + radius ; Box%nodcoord(6,2)=centery - radius ; Box%nodcoord(6,3)=centerz + radius ; 
    Box%nodcoord(7,1)=centerx + radius ; Box%nodcoord(7,2)=centery + radius ; Box%nodcoord(7,3)=centerz + radius ; 
    Box%nodcoord(8,1)=centerx - radius ; Box%nodcoord(8,2)=centery + radius ; Box%nodcoord(8,3)=centerz + radius ; 

    ! Element-Node connectivity
    Box%elemnod(1,1)=1;Box%elemnod(1,2)=2;Box%elemnod(1,3)=4;Box%elemnod(1,4)=5;
    Box%elemnod(2,1)=2;Box%elemnod(2,2)=3;Box%elemnod(2,3)=4;Box%elemnod(2,4)=7;
    Box%elemnod(3,1)=5;Box%elemnod(3,2)=2;Box%elemnod(3,3)=7;Box%elemnod(3,4)=6;
    Box%elemnod(4,1)=5;Box%elemnod(4,2)=7;Box%elemnod(4,3)=4;Box%elemnod(4,4)=8;
    Box%elemnod(5,1)=2;Box%elemnod(5,2)=7;Box%elemnod(5,3)=4;Box%elemnod(5,4)=5;


end subroutine
!##################################################

subroutine DelauneygetNewNode3DMesh(obj,NodeID)
    class(Mesh_),intent(inout)::obj
    type(Mesh_) :: element
    integer(int32),intent(in) :: NodeID
    integer(int32) :: ElementID, ElemNum,i,itr,newElemID,j,currentID
    integer(int32),allocatable :: element_id_list(:),elemnod(:,:),&
    staged_element(:),newElem(:,:),facetNodeID(:,:),staged_facet_id(:)
    real(real64) :: x,y,z,radius,coord(3),center(3),dist,surf(3)

    type(IO_) :: f
    call f%open("debug.txt","w")

    x = obj%nodcoord(nodeid,1)
    y = obj%nodcoord(nodeid,2)
    z = obj%nodcoord(nodeid,3)
    coord(:) = obj%nodcoord(nodeid,:)
    ! search element which contains the node
    ElementID = -1
    do i=1, size(obj%ElemNod)
        if(obj%InsideOfElement(ElementID=i,x=x,y=y,z=z ) )then
            ElementID = i
            currentID = i
            exit
        else
            cycle
        endif
    enddo

    if(ElementID <=0)then
        print *, "ERROR ::DelauneygetNewNode3DMesh >> invalid nodal coordinate. "
        return
    endif

    ! flipping algorithm
    ! #1 check outer sphere for all neighbor elements
    ! ElementIDについて，接する全ての要素を探す
    element_id_list = obj%getNeighboringElement(ElementID,withSurfaceID=.true.,Interfaces=staged_facet_id)
    do i=1,size(element_id_list)/2
        element = obj%getElement(ElementID=element_id_list(i))
        call element%getCircumscribedSphereOfTetra(center,radius)
        dist = sqrt(dot_product(center-coord,center-coord) )
        if(dist <= radius)then
            if(.not.allocated(staged_element) )then
                staged_element = int(zeros(1) ) 
                staged_element(1) = ElementID
            endif
            call ExtendArrayIntVec(mat=staged_element)
            staged_element(size(staged_element) ) = element_id_list(i)
        else
            cycle
        endif
    enddo


    ! add elements in staged_elements
    i = 4-sum(staged_facet_id)
    allocate(newElem(sizE(staged_element)/2*3 + i ,4 ) )
    newElemID = 0
    do i=1, size(staged_element)/2
        facetNodeID = obj%getFacetNodeID(ElementID=(staged_element(i) ) )
        if( .not. allocated(facetNodeID) ) cycle
        if( size(facetNodeID)==0 ) cycle
        do j=1,4
            if( i + size(staged_element)/2 == j )then
                ! facet of original tetra
                cycle
            else
                newElemID = newElemID + 1
                newElem(newElemID,1) = facetNodeID(j,3)
                newElem(newElemID,2) = facetNodeID(j,2)
                newElem(newElemID,3) = facetNodeID(j,1)
                newElem(newElemID,4) = NodeID
            endif
        enddo
    enddo 

    facetNodeID = obj%getFacetNodeID(ElementID=currentID)
    call print(obj%elemnod(currentID,:) )
    call print(" ")
    call print(facetNodeID)
    call print(" ")
    call print(newElem)
    do i=1,sizE(staged_facet_id)
        if(staged_facet_id(i)==1 )then
            cycle
        else
            newElemID = newElemID + 1
            newElem(newElemID,1) = facetNodeID(i,3)
            newElem(newElemID,2) = facetNodeID(i,2)
            newElem(newElemID,3) = facetNodeID(i,1)
            newElem(newElemID,4) = NodeID
        endif
    enddo   

    call print(" ")
    call print(newElem)
    

    ! add elements
    call obj%addElements(connectivity = newElem)
    j = sizE(staged_element)/2
    call obj%removeElements(ElementIDs = staged_element(1:j) )    

    ! remove staged_element(:) and create new elements
    
end subroutine

!##################################################
subroutine DelauneygetNewNodeMesh(obj,node_id,staged_node,triangle,box)
    class(Mesh_),intent(inout)::obj
    type(Mesh_),optional,intent(in) :: box
    integer(int32),optional,intent(in) :: node_id
    integer(int32),optional,intent(inout):: staged_node(:) ! if =1,staged.
    real(real64),optional,intent(inout)  :: triangle(:,:)
    real(real64) :: avec(3),bvec(3),cvec(3),s,t
    integer(int32) :: triangle_node_id(3),new_node_id,i,j,n,point,cover_triangle


    if(size(obj%nodcoord,2)==3 .and. present(Node_id) )then
        
        call obj%DelauneygetNewNode3D(NodeID=node_id)
        return
    endif
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
    integer(int32),intent(in)::triangle_node_id(:),new_node_id
    integer(int32) :: last_elem_id,i

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
    integer(int32),optional,intent(in) ::step
    logical,optional,intent(inout) :: NoChange
    real(real64) :: center(2),a(2),b(2),c(2),node(2)
    real(real64) :: x1,y1,x2,y2,x3,y3,radius,dist_tr
    integer(int32) :: i,j,n,k,l,nodeid_1,nodeid_2,nodeid_tr_1,nodeid_tr_2,point(3)
    integer(int32) :: elem_id, node_tr,nodeid_3,dot_1,count_num,countin,flip_node
    integer(int32) :: old_triangle_id_2,old_triangle_id_1,far_node,far_node_loc,rhs_node,lhs_node 
    integer(int32) :: far_node_tr,far_node_loc_tr,k_1,k_2

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
    integer(int32) :: i,j,n,remove,k

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
    integer(int32) :: i,j,k,l,n,tri_nodes(3),rmn

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
    class(Mesh_),intent(in)::obj
    type(ShapeFunction_)::sobj
    character*200 :: ElemType
    integer(int32) :: i,j,n,m

    n=size(obj%NodCoord,2)
    m=size(obj%ElemNod,2)

    call sobj%getType(NumOfDim=n,NumOfNodePerElem=m)

    ElemType=sobj%ElemType
    return

end function
!##################################################



!##################################################
function getShapeFunctionMesh(obj, ElementID,GaussPointID,ReducedIntegration) result(sobj)
    class(Mesh_),intent(inout)::obj
    integer(int32),intent(in) :: GaussPointID, ElementID
    logical,optional,intent(in) :: ReducedIntegration
    type(ShapeFunction_)::sobj
    character*200 :: ElemType
    integer(int32) :: i,j,n,m,gpid,elemID


    gpid   = GaussPointID
    elemid = ElementID

    n=size(obj%NodCoord,2)
    m=size(obj%ElemNod,2)
    sobj%ReducedIntegration = input(default=.false.,option=ReducedIntegration)

    call sobj%getType(NumOfDim=n,NumOfNodePerElem=m)

    ! get shape functions
    call SetShapeFuncType(sobj)

    call getAllShapeFunc(sobj,elem_id=elemid,nod_coord=obj%NodCoord,elem_nod=obj%ElemNod,OptionalGpID=gpid)

    

end function
!##################################################

!##################################################
subroutine ConvertMeshTypeMesh(obj,Option)
    class(Mesh_),intent(inout) :: obj
    character(*),intent(in) :: Option

    if(Option=="TetraToHexa" .or. Option=="TetraToHex")then
        call obj%convertTetraToHexa()
    elseif(Option=="convertTriangleToRectangular" .or. Option=="TriangleToRectangule")then
        call obj%convertTriangleToRectangular()
    else
        print *, "Option :: ",Option,"is not valid, what if TetraToHexa ?"
    endif


end subroutine
!##################################################

!##################################################
subroutine convertTetraToHexaMesh(obj)
    class(Mesh_),intent(inout) :: obj
    integer(int32) :: i,node_num,elem_num,elemnod_num,incre_nod_num
    real(real64) :: incre_nod_num_real,x1(3),x2(3),x3(3),x4(3)
    real(real64) :: x12(3),x23(3),x31(3),x14(3),x24(3),x34(3)
    real(real64) :: x123(3),x234(3),x134(3),x124(3)
    real(real64) :: x1234(3),direct
    
    integer(int32),allocatable :: HexElemNod(:,:)
    real(real64),allocatable ::HexNodCoord(:,:)
    integer(int32) :: local_id(15),node_id
    
    ! converter for 3D
    node_num     = size(obj%NodCoord,1)
    elem_num    = size(obj%ElemNod,1)
    elemnod_num = size(obj%ElemNod,2)
    incre_nod_num=(4+6+1)*elem_num


    allocate(HexElemNod( elem_num*4,8) )
    allocate(HexNodCoord(node_num+incre_nod_num,3)  )

    HexNodCoord(1:node_num,1:3) = obj%NodCoord(1:node_num,1:3)
    ! increase ElemNod (connectivity)
    node_id=node_num
    do i=1, elem_num
        ! for each element
        node_id=node_id
        x1(:) = obj%NodCoord( obj%ElemNod(i,1) ,:) ! #1
        x2(:) = obj%NodCoord( obj%ElemNod(i,2) ,:) ! #2
        x3(:) = obj%NodCoord( obj%ElemNod(i,3) ,:) ! #3
        x4(:) = obj%NodCoord( obj%ElemNod(i,4) ,:) ! #4

        ! check order
        !direct=dot_product(cross_product(x2-x1,x3-x1),x4-x1)
        !if(direct<=0.0d0)then
        !    print *, "Elemid = ",i,"is invalid",direct
        !    stop "debug"
        !else
        !    print *, "Elemid = ",i,"is ok",direct
        !endif

        x12(:)= 0.50d0*x1(:) + 0.50d0*x2(:) ! #5
        x23(:)= 0.50d0*x2(:) + 0.50d0*x3(:) ! #6
        x31(:)= 0.50d0*x3(:) + 0.50d0*x1(:) ! #7
        x14(:)= 0.50d0*x1(:) + 0.50d0*x4(:) ! #8
        x24(:)= 0.50d0*x2(:) + 0.50d0*x4(:) ! #9
        x34(:)= 0.50d0*x3(:) + 0.50d0*x4(:) ! #10
        x123(:) = 1.0d0/3.0d0*x1(:)+1.0d0/3.0d0*x2(:)+1.0d0/3.0d0*x3(:) ! #11
        x234(:) = 1.0d0/3.0d0*x2(:)+1.0d0/3.0d0*x3(:)+1.0d0/3.0d0*x4(:) ! #12
        x134(:) = 1.0d0/3.0d0*x1(:)+1.0d0/3.0d0*x3(:)+1.0d0/3.0d0*x4(:) ! #13
        x124(:) = 1.0d0/3.0d0*x1(:)+1.0d0/3.0d0*x2(:)+1.0d0/3.0d0*x4(:) ! #14
        x1234(:)=x1(:)+x2(:)+x3(:)+x4(:)
        x1234(:)=0.250d0*x1234(:) ! #15
        local_id( 1) = obj%ElemNod(i,1)
        local_id( 2) = obj%ElemNod(i,2)
        local_id( 3) = obj%ElemNod(i,3)
        local_id( 4) = obj%ElemNod(i,4)
        local_id( 5) = node_id+ 1
        local_id( 6) = node_id+ 2
        local_id( 7) = node_id+ 3
        local_id( 8) = node_id+ 4
        local_id( 9) = node_id+ 5
        local_id(10) = node_id+ 6
        local_id(11) = node_id+ 7
        local_id(12) = node_id+ 8
        local_id(13) = node_id+ 9
        local_id(14) = node_id+10
        local_id(15) = node_id+11

        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x12(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x23(:) 
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x31(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x14(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x24(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x34(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x123(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x234(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x134(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x124(:)
        node_id = node_id + 1
        HexNodCoord(node_id,1:3) = x1234(:)

        ! assemble new element
        HexElemNod( (i-1)*4 + 1,1) = local_id(1 )
        HexElemNod( (i-1)*4 + 1,2) = local_id(5 ) 
        HexElemNod( (i-1)*4 + 1,3) = local_id(11) 
        HexElemNod( (i-1)*4 + 1,4) = local_id(7 ) 
        HexElemNod( (i-1)*4 + 1,5) = local_id(8 ) 
        HexElemNod( (i-1)*4 + 1,6) = local_id(14) 
        HexElemNod( (i-1)*4 + 1,7) = local_id(15) 
        HexElemNod( (i-1)*4 + 1,8) = local_id(13) 

        HexElemNod( (i-1)*4 + 2,1) = local_id(5 )
        HexElemNod( (i-1)*4 + 2,2) = local_id(2 ) 
        HexElemNod( (i-1)*4 + 2,3) = local_id(6 ) 
        HexElemNod( (i-1)*4 + 2,4) = local_id(11 ) 
        HexElemNod( (i-1)*4 + 2,5) = local_id(14 ) 
        HexElemNod( (i-1)*4 + 2,6) = local_id(9 ) 
        HexElemNod( (i-1)*4 + 2,7) = local_id(12 ) 
        HexElemNod( (i-1)*4 + 2,8) = local_id(15 ) 

        HexElemNod( (i-1)*4 + 3,1) = local_id(6 )
        HexElemNod( (i-1)*4 + 3,2) = local_id(3 ) 
        HexElemNod( (i-1)*4 + 3,3) = local_id(7 ) 
        HexElemNod( (i-1)*4 + 3,4) = local_id(11 ) 
        HexElemNod( (i-1)*4 + 3,5) = local_id(15 ) 
        HexElemNod( (i-1)*4 + 3,6) = local_id(12 ) 
        HexElemNod( (i-1)*4 + 3,7) = local_id(10 ) 
        HexElemNod( (i-1)*4 + 3,8) = local_id(13 ) 

        HexElemNod( (i-1)*4 + 4,1) = local_id(8 )
        HexElemNod( (i-1)*4 + 4,2) = local_id(14 ) 
        HexElemNod( (i-1)*4 + 4,3) = local_id(15 ) 
        HexElemNod( (i-1)*4 + 4,4) = local_id(13 ) 
        HexElemNod( (i-1)*4 + 4,5) = local_id(4 ) 
        HexElemNod( (i-1)*4 + 4,6) = local_id(9 ) 
        HexElemNod( (i-1)*4 + 4,7) = local_id(12 ) 
        HexElemNod( (i-1)*4 + 4,8) = local_id(10 ) 

    enddo

    deallocate(obj%NodCoord)
    deallocate(obj%ElemNod)
    allocate(obj%NodCoord(size(HexNodCoord,1),size(HexNodCoord,2)  ) )
    allocate(obj%ElemNod(size(HexElemNod,1) ,size(HexElemNod,2) ))
    obj%NodCoord(:,:)=HexNodCoord(:,:)
    obj%ElemNod      =HexElemNod(:,:)
    
    ! done, but overlaps exists
    call obj%removeOverlappedNode()


end subroutine
!##################################################


!##################################################
subroutine convertTriangleToRectangularMesh(obj)
    class(Mesh_),intent(inout) :: obj
    integer(int32) :: i,node_num,elem_num,elemnod_num,incre_nod_num
    real(real64) :: incre_nod_num_real,x1(2),x2(2),x3(2),x4(2)
    real(real64) :: x12(2),x23(2),x31(2)
    real(real64) :: x123(2)
    
    integer(int32),allocatable :: RectElemNod(:,:),before_after(:)
    real(real64),allocatable :: RectNodCoord(:,:)
    integer(int32) :: local_id(7),node_id
    
    ! converter for 3D
    node_num     = size(obj%NodCoord,1)
    elem_num    = size(obj%ElemNod,1)
    elemnod_num = size(obj%ElemNod,2)
    incre_nod_num=(4)*elem_num


    print *, "Triangle mesh to rectangular mesh"
    allocate(RectElemNod( elem_num*3,4) )
    allocate(RectNodCoord(node_num+incre_nod_num,2)  )

    RectNodCoord(1:node_num,1:2) = obj%NodCoord(1:node_num,1:2)
    ! increase ElemNod (connectivity)
    node_id=node_num
    do i=1, elem_num
        ! for each element
        node_id=node_id
        x1(1:2) = obj%NodCoord( obj%ElemNod(i,1) ,1:2) ! #1
        x2(1:2) = obj%NodCoord( obj%ElemNod(i,2) ,1:2) ! #2
        x3(1:2) = obj%NodCoord( obj%ElemNod(i,3) ,1:2) ! #3
        x12(1:2)= 0.50d0*x1(1:2) + 0.50d0*x2(1:2) ! #4
        x23(1:2)= 0.50d0*x2(1:2) + 0.50d0*x3(1:2) ! #5
        x31(1:2)= 0.50d0*x3(1:2) + 0.50d0*x1(1:2) ! #6
        x123(:)=x1(:)+x2(:)+x3(:)
        x123(:)=1.0d0/3.0d0*x123(:) ! #7

        local_id( 1) = obj%ElemNod(i,1)
        local_id( 2) = obj%ElemNod(i,2)
        local_id( 3) = obj%ElemNod(i,3)
        local_id( 4) = node_id+ 1
        local_id( 5) = node_id+ 2
        local_id( 6) = node_id+ 3
        local_id( 7) = node_id+ 4

        node_id = node_id + 1
        RectNodCoord(node_id,1:2) = x12(:)
        node_id = node_id + 1
        RectNodCoord(node_id,1:2) = x23(:) 
        node_id = node_id + 1
        RectNodCoord(node_id,1:2) = x31(:)
        node_id = node_id + 1
        RectNodCoord(node_id,1:2) = x123(:)
        
        ! assemble new element
        RectElemNod( (i-1)*3 + 1,1) = local_id(1 )
        RectElemNod( (i-1)*3 + 1,2) = local_id(4 ) 
        RectElemNod( (i-1)*3 + 1,3) = local_id(7) 
        RectElemNod( (i-1)*3 + 1,4) = local_id(6 ) 

        RectElemNod( (i-1)*3 + 2,1) = local_id(4 )
        RectElemNod( (i-1)*3 + 2,2) = local_id(2 ) 
        RectElemNod( (i-1)*3 + 2,3) = local_id(5 ) 
        RectElemNod( (i-1)*3 + 2,4) = local_id(7 )

        RectElemNod( (i-1)*3 + 3,1) = local_id(5 )
        RectElemNod( (i-1)*3 + 3,2) = local_id(3 ) 
        RectElemNod( (i-1)*3 + 3,3) = local_id(6 ) 
        RectElemNod( (i-1)*3 + 3,4) = local_id(7 )

    enddo

    
    deallocate(obj%NodCoord)
    deallocate(obj%ElemNod)
    allocate(obj%NodCoord(size(RectNodCoord,1),size(RectNodCoord,2)  ) )
    allocate(obj%ElemNod(size(RectElemNod,1) ,size(RectElemNod,2) ))
    obj%NodCoord(:,:)=RectNodCoord(:,:)
    obj%ElemNod      =RectElemNod(:,:)
    
    ! done, but overlaps exists
    
    call obj%removeOverlappedNode()



end subroutine
!##################################################

!##################################################
subroutine removeOverlappedNodeMesh(obj,tolerance)
    class(Mesh_),intent(inout)::obj
    real(real64),optional,intent(in) :: tolerance
    integer(int32),allocatable :: RectElemNod(:,:),checked(:),before_after(:)
    real(real64),allocatable :: New_NodCoord(:,:)
    integer(int32) :: i,j,k,dim_num,node_num,itr,elem_num,elemnod_num,l
    real(real64),allocatable :: x(:),x_tr(:)
    real(real64) :: error,tol
    
    if(present(tolerance) )then
        tol=tolerance
    else
        tol=1.0e-16
    endif
    dim_num=size(obj%NodCoord,2)
    node_num=size(obj%NodCoord,1)
    elem_num=size(obj%ElemNod,1)
    elemnod_num=size(obj%ElemNod,2)
    allocate( x(dim_num),x_tr(dim_num),checked(node_num ) )
    allocate(before_after(size(checked) ) )
    

    do i=1,node_num
        before_after(i)=i
    enddo


    
    checked(:)=0
    itr=0
    do i=1,node_num-1
        ! if already checked
        if(checked(i)>=1 )then
            cycle
        endif
        ! check about ith node
        x(:)=obj%NodCoord(i,:)
        
        

        do k=i+1,node_num
            ! if already checked
            if(checked(k)>=1 )then
                cycle
            endif

            x_tr(:)=obj%NodCoord(k,:)
            error = dot_product(x(:) -x_tr(:),x(:)-x_tr(:) )
            if(error < tol)then
                ! node id i and node id k are the same node
                ! use smaller id

                checked(k)=checked(k)+1
                before_after(k)=i
                
            else
                cycle
            endif
        enddo
    enddo



    k=0
    do i=1,size(checked)
        if(checked(i)>=1 )then
            cycle
        else
            k=k+1
            l=before_after(i)
            before_after(i)=k
            do j=i+1,node_num
                if(before_after(j)==l )then
                    before_after(j)=k
                endif
            enddo
        endif
    enddo
    allocate(New_NodCoord(k,dim_num ) )


    ! fix numbers
    do i=1,elem_num
        do j=1,elemnod_num
            obj%ElemNod(i,j)=before_after( obj%ElemNod(i,j) )
        enddo
    enddo

    ! then remove node_id==k check(k)==1
    k=0
    do i=1,node_num
        if(checked(i)>=1 )then
            cycle
        else
            k=k+1
            New_NodCoord(k,:)=obj%NodCoord(i,:)
        endif
    enddo

    deallocate(obj%NodCoord)
    allocate(obj%NodCoord( size(New_NodCoord,1), size(New_NodCoord,2)   ) )
    obj%NodCoord(:,:)=New_NodCoord(:,:)

end subroutine
!##################################################

!##################################################
subroutine AdjustSphereMesh(obj,rx,ry,rz,debug)
    class(Mesh_),intent(inout) :: obj
    type(Mesh_) :: mesh
    real(real64)   :: o(3),rate,x_cur(3),x_pres(3)
    real(real64),optional,intent(in)   :: rx,ry,rz
    real(real64)   :: r_x,r_y,r_z,dist,r_tr(3)
    integer(int32) :: i,ii,j,k,n,node_id,itr
    integer(int32),allocatable :: elem(:)
    logical,optional,intent(in) :: debug

    n=size(obj%ElemNod,1)
    
    call mesh%copy(obj)
    itr=0
    do 
        itr=itr+1
        o(1)=minval(mesh%NodCoord(:,1))+maxval(mesh%NodCoord(:,1))
        o(2)=minval(mesh%NodCoord(:,2))+maxval(mesh%NodCoord(:,2))
        o(3)=minval(mesh%NodCoord(:,3))+maxval(mesh%NodCoord(:,3))
        o(:)=0.50d0*o(:)
        
        if(allocated(elem) )then
            deallocate(elem)
        endif
        n=size(mesh%ElemNod,1)
        if(present(debug) )then
            print *, "itr :",itr,"Number of element",n
        endif
        
        if(n==0)then
            exit
        endif
        allocate(elem(n) )
        elem(:)=1
        call mesh%getSurface()

        
        do i=1,size(mesh%FacetElemNod,1)
            do j=1,size(mesh%FacetElemNod,2)
                node_id=mesh%FacetElemNod(i,j)
                if(i==1 .and. j==1)then
                    r_x=0.50d0*(mesh%NodCoord(node_id,1) - o(1) )
                    r_y=0.50d0*(mesh%NodCoord(node_id,2) - o(2) )
                    r_z=0.50d0*(mesh%NodCoord(node_id,3) - o(3) )
                    cycle
                else
                    r_tr(1)=0.50d0*(mesh%NodCoord(node_id,1) - o(1) )
                    r_tr(2)=0.50d0*(mesh%NodCoord(node_id,2) - o(2) )
                    r_tr(3)=0.50d0*(mesh%NodCoord(node_id,3) - o(3) )            
                endif
                if(r_x < r_tr(1))then
                    r_x=r_tr(1)
                endif
                if(r_y < r_tr(2))then
                    r_y=r_tr(2)
                endif
                if(r_z < r_tr(3))then
                    r_z=r_tr(3)
                endif
            enddo
        enddo
        print *, r_x,r_y,r_z
        
        do i=1,size(mesh%FacetElemNod,1)
            do j=1,size(mesh%FacetElemNod,2)
                node_id=mesh%FacetElemNod(i,j)
                
                x_cur(1:3)=obj%NodCoord(node_id,1:3)
                
                dist=distance(x_cur,o)

                x_pres(1)=o(1)+ r_x/dist*(x_cur(1) - o(1) )*2.0d0
                x_pres(2)=o(2)+ r_y/dist*(x_cur(2) - o(2) )*2.0d0
                x_pres(3)=o(3)+ r_z/dist*(x_cur(3) - o(3) )*2.0d0
                
                obj%NodCoord(node_id,1:3)=x_pres(1:3)
            enddo
        enddo
        ! remove facets
        elem(:)=1
        do i=1,size(mesh%ElemNod,1)
            do ii=1,size(mesh%ElemNod,2)
                do j=1,size(mesh%FacetElemNod,1)
                    do k=1,size(mesh%FacetElemNod,2)
                        node_id=mesh%FacetElemNod(j,k)
                        if(mesh%ElemNod(i,ii)==node_id )then
                            elem(i)=0
                            exit
                        endif
                    enddo
                enddo
            enddo
        enddo
        
        if(minval(elem)==1 )then
            print *, "ERROR :: AdjustSphereMesh minval(elem)==1"
            stop 
        endif
        if(maxval(elem)==0 )then
            print *, "converged"
            exit
        endif
        ! remove elems
        do i=size(elem),1,-1
            if(elem(i)==0 )then
                call removeArray(mat=mesh%ElemNod,remove1stColumn=.true.,NextOf=i-1)
            endif
        enddo
        !call showArray(mat=mesh%NodCoord,IndexArray=mesh%ElemNod,&
        !    Name=trim(adjustl( fstring(itr) ))//".txt")
    enddo
    
    
end subroutine AdjustSphereMesh
!##################################################

!##################################################
subroutine AdjustCylinderMesh(obj,rx,ry,rz,debug)
    class(Mesh_),intent(inout) :: obj
    type(Mesh_) :: mesh
    real(real64)   :: o(3),rate,x_cur(3),x_pres(3)
    real(real64),optional,intent(in)   :: rx,ry,rz
    real(real64)   :: r_x,r_y,r_z,dist,r_tr(3)
    integer(int32) :: i,ii,j,k,n,node_id,itr
    integer(int32),allocatable :: elem(:)
    logical,optional,intent(in) :: debug

    n=size(obj%ElemNod,1)
    
    call mesh%copy(obj)
    itr=0
    do 
        itr=itr+1
        o(1)=minval(mesh%NodCoord(:,1))+maxval(mesh%NodCoord(:,1))
        o(2)=minval(mesh%NodCoord(:,2))+maxval(mesh%NodCoord(:,2))
        o(3)=minval(mesh%NodCoord(:,3))+maxval(mesh%NodCoord(:,3))
        o(:)=0.50d0*o(:)
        
        if(allocated(elem) )then
            deallocate(elem)
        endif
        n=size(mesh%ElemNod,1)
        if(present(debug) )then
            print *, "itr :",itr,"Number of element",n
        endif
        
        if(n==0)then
            exit
        endif
        allocate(elem(n) )
        elem(:)=1
        call mesh%getSurface()

        
        do i=1,size(mesh%FacetElemNod,1)
            do j=1,size(mesh%FacetElemNod,2)
                node_id=mesh%FacetElemNod(i,j)
                if(i==1 .and. j==1)then
                    r_x=0.50d0*(mesh%NodCoord(node_id,1) - o(1) )
                    r_y=0.50d0*(mesh%NodCoord(node_id,2) - o(2) )
                    r_z=0.50d0*(mesh%NodCoord(node_id,3) - o(3) )
                    cycle
                else
                    r_tr(1)=0.50d0*(mesh%NodCoord(node_id,1) - o(1) )
                    r_tr(2)=0.50d0*(mesh%NodCoord(node_id,2) - o(2) )
                    r_tr(3)=0.50d0*(mesh%NodCoord(node_id,3) - o(3) )            
                endif
                if(r_x < r_tr(1))then
                    r_x=r_tr(1)
                endif
                if(r_y < r_tr(2))then
                    r_y=r_tr(2)
                endif
                if(r_z < r_tr(3))then
                    r_z=r_tr(3)
                endif
            enddo
        enddo
        
        do i=1,size(mesh%FacetElemNod,1)
            do j=1,size(mesh%FacetElemNod,2)
                node_id=mesh%FacetElemNod(i,j)
                
                x_cur(1:3)=obj%NodCoord(node_id,1:3)
                
                dist=distance(x_cur(1:3),o(1:3) )

                x_pres(1)=o(1)+ r_x/dist*(x_cur(1) - o(1) )*2.0d0
                x_pres(2)=o(2)+ r_y/dist*(x_cur(2) - o(2) )*2.0d0
                x_pres(3)=o(3)+ r_z/dist*(x_cur(3) - o(3) )*2.0d0
                
                obj%NodCoord(node_id,1:2)=x_pres(1:2)
            enddo
        enddo
        ! remove facets
        elem(:)=1
        do i=1,size(mesh%ElemNod,1)
            do ii=1,size(mesh%ElemNod,2)
                do j=1,size(mesh%FacetElemNod,1)
                    do k=1,size(mesh%FacetElemNod,2)
                        node_id=mesh%FacetElemNod(j,k)
                        if(mesh%ElemNod(i,ii)==node_id )then
                            elem(i)=0
                            exit
                        endif
                    enddo
                enddo
            enddo
        enddo
        
        if(minval(elem)==1 )then
            print *, "ERROR :: AdjustSphereMesh minval(elem)==1"
            stop 
        endif
        if(maxval(elem)==0 )then
            print *, "converged"
            exit
        endif
        ! remove elems
        do i=size(elem),1,-1
            if(elem(i)==0 )then
                call removeArray(mat=mesh%ElemNod,remove1stColumn=.true.,NextOf=i-1)
            endif
        enddo

        
        !call showArray(mat=mesh%NodCoord,IndexArray=mesh%ElemNod,&
        !    Name=trim(adjustl( fstring(itr) ))//".txt")
    enddo
    
    
end subroutine AdjustCylinderMesh
!##################################################

recursive subroutine createMesh(obj,meshtype,x_num,y_num,x_len,y_len,Le,Lh,Dr,thickness,&
    division,smooth,top,margin,inclineRate,shaperatio,master,slave,x,y,z,dx,dy,dz,coordinate,&
    species,SoyWidthRatio)
    class(Mesh_),intent(inout) :: obj
    type(Mesh_) :: mesh1,mesh2,interface1,interface2
    type(Mesh_),optional,intent(inout) :: master,slave
    type(IO_) :: f
    type(ShapeFunction_) :: shape
    character(*),optional,intent(in) :: meshtype
    logical,optional,intent(in) :: smooth
    integer(int32),optional,intent(in) :: x_num,y_num ! number of division
    integer(int32),optional,intent(in) :: division ! for 3D rectangular
    real(real64),optional,intent(in) :: x_len,y_len,Le,Lh,Dr,coordinate(:,:) ! length
    real(real64),optional,intent(in) :: thickness,inclineRate ! for 3D rectangular
    real(real64),optional,intent(in) :: top,margin ! for 3D rectangular
    real(real64),optional,intent(in) :: shaperatio ! for 3D leaf
    real(real64),optional,intent(in) :: x,y,z,dx,dy,dz
    integer(int32),optional,intent(in) :: species
    real(real64),optional,intent(in) :: SoyWidthRatio ! width ratio for side leaves of soybean

    integer(int32) :: i,j,n,m,xn,yn,smoothedge(8),ini,k,dim_num,node_num,elem_num
    real(real64)::lx,ly,sx,sy,a_val,radius,x_,y_,diflen,Lt,&
        unitx,unity,xm, ym,tp,rx,ry,zc,zl,zm,ysize,ox,oy,dist,rr
    logical :: validmeshtype=.false.
    type(Mesh_) :: BoundBox
    real(real64)::ymin,ymax,ratio,width,pi,xx,yy,xvec(3),x_max(3),&
        x_min(3),x_m_mid(3),x_s_mid(3),x1vec(3),x2vec(3),nvec(3),hvec(3)
    integer(int32),allocatable:: OutNodeID(:),OutElementID(:)
    logical :: inside
    real(real64):: dist_tr, dist_cur,z_,zval1,zval2,x_1(3),x_2(3)
    integer(int32) :: num_layer,itr,node1,node2,node3,node4,count,prev_node1
    integer(int32), allocatable :: elemnod(:,:)
    integer(int32) :: nearest_node_id,nearest_facet_id,node_id,elist(2),tri_excep,tri_excep_last
    integer(int32),allocatable :: checked(:),checked_node(:)
    real(real64),allocatable ::nodcoord(:,:)
    real(real64) :: ll,center(3),vector(3),e1(3),e2(3),e3(3),len_val
    real(real64) :: length,r,alpha,lin_curve_ratio,yy_,swratio
    
    
    
    lin_curve_ratio = 0.50d0
    pi = 3.1415926535d0
    ! this subroutine creates mesh
    obj%meshtype = meshtype

    if(obj%meshtype=="root" .or. obj%meshtype=="Root")then
        
        
        ! tree-like graph structure 
        call obj%remove(all=.true.)



        if(present(coordinate) )then
            
            itr = 0
            obj%nodcoord = coordinate

            ! assemble nodes to a mesh consisits of line elements
            call obj%assemble()

            if(.not. present(thickness) )then
                return
            endif
            
            width = input(default=1.0d0,option=thickness)
            elem_num = size(obj%elemnod,1)
            node_num = size(obj%nodcoord,1)
            dim_num = size(obj%nodcoord,2)
            allocate(nodcoord(node_num*4,dim_num))
            elemnod = obj%elemnod

            ! 4倍に増やしてつなげる
            nodcoord(1 :node_num  ,3) = obj%nodcoord(:,3) 
            nodcoord(1 :node_num  ,1) = obj%nodcoord(1 :node_num  ,1) - width*0.50d0
            nodcoord(1 :node_num  ,2) = obj%nodcoord(1 :node_num  ,2) - width*0.50d0
            
            nodcoord(node_num+1 :node_num*2  ,3) = obj%nodcoord(:,3) +width*0.10d0
            nodcoord(node_num+1 :node_num*2  ,1) = obj%nodcoord(1 :node_num  ,1) + width*0.50d0
            nodcoord(node_num+1 :node_num*2  ,2) = obj%nodcoord(1 :node_num  ,2) - width*0.50d0

            nodcoord(node_num*2+1 :node_num*3  ,3) = obj%nodcoord(:,3) +width*0.10d0
            nodcoord(node_num*2+1 :node_num*3  ,1) = obj%nodcoord(1 :node_num  ,1) + width*0.50d0
            nodcoord(node_num*2+1 :node_num*3  ,2) = obj%nodcoord(1 :node_num  ,2) + width*0.50d0

            nodcoord(node_num*3+1 :node_num*4  ,3) = obj%nodcoord(:,3) 
            nodcoord(node_num*3+1 :node_num*4  ,1) = obj%nodcoord(1 :node_num  ,1) - width*0.50d0
            nodcoord(node_num*3+1 :node_num*4  ,2) = obj%nodcoord(1 :node_num  ,2) + width*0.50d0

            do i=1,elem_num
                node1 = elemnod(i,1)
                node2 = elemnod(i,2)

                elemnod(i,1) = node1 + node_num*0
                elemnod(i,2) = node1 + node_num*1
                elemnod(i,3) = node1 + node_num*2
                elemnod(i,4) = node1 + node_num*3
                
                elemnod(i,5) = node2 + node_num*0
                elemnod(i,6) = node2 + node_num*1
                elemnod(i,7) = node2 + node_num*2
                elemnod(i,8) = node2 + node_num*3
            enddo

            obj%nodcoord = nodcoord
            obj%elemnod  = elemnod
            return


!            ! generate solid elements from line elements
!            elem_num = size(obj%elemnod,1)
!            allocate(nodcoord(elem_num*8,3))
!            allocate(elemnod(elem_num,8))
!            width = input(default=1.0d0,option=thickness)
!            ll = width/2.0d0
!            
!            e1(:)=0.0d0
!            e1(1)=1.0d0
!            
!            e2(:)=0.0d0
!            e2(2)=1.0d0
!            
!            e3(:)=0.0d0
!            e3(3)=1.0d0
!
!            ! O_________________O
!            ! |\                 \
!            ! | \        +        \ 
!            ! |  \      node2      \ 
!            ! |   O_________________O
!            ! O   |                 |
!            !  \  |    node1        |
!            !   \ |       +         |
!            !    \O_________________O
!            !
!            ! From +, create O 
!            do i=1,elem_num
!                node1 = obj%elemnod(i,1)
!                node2 = obj%elemnod(i,2)
!
!                if(obj%nodcoord(node1,3) > obj%nodcoord(node2,3) )then
!                    node1 = obj%elemnod(i,2)
!                    node2 = obj%elemnod(i,1)
!                endif
!                x_1(:) = obj%nodcoord(node1,:)
!                x_2(:) = obj%nodcoord(node2,:)
!                center(:) =0.50d0*(  x_2(:) + x_1(:) )
!                vector(:) = x_2(:) - x_1(:)
!                len_val = abs(vector(3) )
!
                ! vector の方向によって場合分け
!                if( abs(vector(1)) > abs(vector(2)) .and. abs(vector(1)) > abs(vector(3))  ) then
!                    ! x-domination
!                    nodcoord(8*i-7,1)=x_1(1) - ll ; nodcoord(8*i-7,2)=x_1(2) - ll ;nodcoord(8*i-7,3)= center(3) - ll;
!                    nodcoord(8*i-6,1)=x_1(1) + ll ; nodcoord(8*i-6,2)=x_1(2) - ll ;nodcoord(8*i-6,3)= center(3) - ll;
!                    nodcoord(8*i-5,1)=x_1(1) + ll ; nodcoord(8*i-5,2)=x_1(2) + ll ;nodcoord(8*i-5,3)= center(3) - ll;
!                    nodcoord(8*i-4,1)=x_1(1) - ll ; nodcoord(8*i-4,2)=x_1(2) + ll ;nodcoord(8*i-4,3)= center(3) - ll;
!                    nodcoord(8*i-3,1)=x_1(1) - ll ; nodcoord(8*i-3,2)=x_1(2) - ll ;nodcoord(8*i-3,3)= center(3) + ll;
!                    nodcoord(8*i-2,1)=x_1(1) + ll ; nodcoord(8*i-2,2)=x_1(2) - ll ;nodcoord(8*i-2,3)= center(3) + ll;
!                    nodcoord(8*i-1,1)=x_1(1) + ll ; nodcoord(8*i-1,2)=x_1(2) + ll ;nodcoord(8*i-1,3)= center(3) + ll;
!                    nodcoord(8*i  ,1)=x_1(1) - ll ; nodcoord(8*i  ,2)=x_1(2) + ll ;nodcoord(8*i  ,3)= center(3) + ll;
!                elseif( abs(vector(2)) > abs(vector(1)) .and. abs(vector(2)) > abs(vector(3))  ) then
!                    ! y-domination
!                    nodcoord(8*i-7,1)=x_1(1) - ll ; nodcoord(8*i-7,2)=x_1(2) - ll ;nodcoord(8*i-7,3)= center(3) - ll;
!                    nodcoord(8*i-6,1)=x_1(1) + ll ; nodcoord(8*i-6,2)=x_1(2) - ll ;nodcoord(8*i-6,3)= center(3) - ll;
!                    nodcoord(8*i-5,1)=x_1(1) + ll ; nodcoord(8*i-5,2)=x_1(2) + ll ;nodcoord(8*i-5,3)= center(3) - ll;
!                    nodcoord(8*i-4,1)=x_1(1) - ll ; nodcoord(8*i-4,2)=x_1(2) + ll ;nodcoord(8*i-4,3)= center(3) - ll;
!                    nodcoord(8*i-3,1)=x_1(1) - ll ; nodcoord(8*i-3,2)=x_1(2) - ll ;nodcoord(8*i-3,3)= center(3) + ll;
!                    nodcoord(8*i-2,1)=x_1(1) + ll ; nodcoord(8*i-2,2)=x_1(2) - ll ;nodcoord(8*i-2,3)= center(3) + ll;
!                    nodcoord(8*i-1,1)=x_1(1) + ll ; nodcoord(8*i-1,2)=x_1(2) + ll ;nodcoord(8*i-1,3)= center(3) + ll;
!                    nodcoord(8*i  ,1)=x_1(1) - ll ; nodcoord(8*i  ,2)=x_1(2) + ll ;nodcoord(8*i  ,3)= center(3) + ll;
!                elseif( abs(vector(3)) > abs(vector(1)) .and. abs(vector(3)) > abs(vector(2))  ) then
!                    ! z-domination
!                    nodcoord(8*i-7,1)=x_1(1) - ll ; nodcoord(8*i-7,2)=x_1(2) - ll ;nodcoord(8*i-7,3)= center(3) - ll;
!                    nodcoord(8*i-6,1)=x_1(1) + ll ; nodcoord(8*i-6,2)=x_1(2) - ll ;nodcoord(8*i-6,3)= center(3) - ll;
!                    nodcoord(8*i-5,1)=x_1(1) + ll ; nodcoord(8*i-5,2)=x_1(2) + ll ;nodcoord(8*i-5,3)= center(3) - ll;
!                    nodcoord(8*i-4,1)=x_1(1) - ll ; nodcoord(8*i-4,2)=x_1(2) + ll ;nodcoord(8*i-4,3)= center(3) - ll;
!                    nodcoord(8*i-3,1)=x_1(1) - ll ; nodcoord(8*i-3,2)=x_1(2) - ll ;nodcoord(8*i-3,3)= center(3) + ll;
!                    nodcoord(8*i-2,1)=x_1(1) + ll ; nodcoord(8*i-2,2)=x_1(2) - ll ;nodcoord(8*i-2,3)= center(3) + ll;
!                    nodcoord(8*i-1,1)=x_1(1) + ll ; nodcoord(8*i-1,2)=x_1(2) + ll ;nodcoord(8*i-1,3)= center(3) + ll;
!                    nodcoord(8*i  ,1)=x_1(1) - ll ; nodcoord(8*i  ,2)=x_1(2) + ll ;nodcoord(8*i  ,3)= center(3) + ll;
!                else
!                    ! same
!
!                endif        
!               
!                nodcoord(8*i-7,1)= center(1) - abs(vector(1))*0.50d0 
!                nodcoord(8*i-7,2)= center(2) - abs(vector(2))*0.50d0
!                nodcoord(8*i-7,3)= center(3) - abs(vector(3))*0.50d0
!
!                nodcoord(8*i-6,1)= center(1) + abs(vector(1))*0.50d0 
!                nodcoord(8*i-6,2)= center(2) - abs(vector(2))*0.50d0 
!                nodcoord(8*i-6,3)= center(3) - abs(vector(3))*0.50d0
!
!                nodcoord(8*i-5,1)= center(1) + abs(vector(1))*0.50d0 
!                nodcoord(8*i-5,2)= center(2) + abs(vector(2))*0.50d0 
!                nodcoord(8*i-5,3)= center(3) - abs(vector(3))*0.50d0
!
!                nodcoord(8*i-4,1)= center(1) - abs(vector(1))*0.50d0 
!                nodcoord(8*i-4,2)= center(2) + abs(vector(2))*0.50d0 
!                nodcoord(8*i-4,3)= center(3) - abs(vector(3))*0.50d0
!
!                nodcoord(8*i-3,1)= center(1) - abs(vector(1))*0.50d0 
!                nodcoord(8*i-3,2)= center(2) - abs(vector(2))*0.50d0 
!                nodcoord(8*i-3,3)= center(3) + abs(vector(3))*0.50d0
!
!                nodcoord(8*i-2,1)= center(1) + abs(vector(1))*0.50d0 
!                nodcoord(8*i-2,2)= center(2) - abs(vector(2))*0.50d0 
!                nodcoord(8*i-2,3)= center(3) + abs(vector(3))*0.50d0
!
!                nodcoord(8*i-1,1)= center(1) + abs(vector(1))*0.50d0 
!                nodcoord(8*i-1,2)= center(2) + abs(vector(2))*0.50d0 
!                nodcoord(8*i-1,3)= center(3) + abs(vector(3))*0.50d0
!
!                nodcoord(8*i  ,1)= center(1) - abs(vector(1))*0.50d0 
!                nodcoord(8*i  ,2)= center(2) + abs(vector(2))*0.50d0 
!                nodcoord(8*i  ,3)= center(3) + abs(vector(3))*0.50d0
!
!
!
!                elemnod(i,1) = 8*i-7
!                elemnod(i,2) = 8*i-6
!                elemnod(i,3) = 8*i-5
!                elemnod(i,4) = 8*i-4
!                elemnod(i,5) = 8*i-3
!                elemnod(i,6) = 8*i-2
!                elemnod(i,7) = 8*i-1
!                elemnod(i,8) = 8*i  
!            enddo
!
!            obj%nodcoord = nodcoord
!            obj%elemnod = elemnod
!
            

!            return

            !!!!!
!            allocate(obj%elemnod(size(obj%nodcoord,1)*2 ,8) )
!            do 
!                itr = itr + 1
!            
!                if(itr > size(obj%nodcoord,1) ) exit
!                x_ = obj%nodcoord(itr,1)
!                y_ = obj%nodcoord(itr,2)
!                z_ = obj%nodcoord(itr,3)
!                nearest_node_id = obj%getNearestNodeID(x=x_,y=y_,z=z_,except=itr)
!                obj%elemnod(2*itr-1,1) = itr
!                obj%elemnod(2*itr-1,2:) = nearest_node_id
!                elist(1)=itr
!                elist(2)=nearest_node_id
!                x_ = obj%nodcoord(itr,1)
!                y_ = obj%nodcoord(itr,2)
!                z_ = obj%nodcoord(itr,3)
!                nearest_node_id = obj%getNearestNodeID(x=x_,y=y_,z=z_,exceptlist=elist)
!                obj%elemnod(2*itr,1) = itr
!                obj%elemnod(2*itr,2:) = nearest_node_id
!            enddo
!        
!            ! remove overlap elements
!        
!            ! case 1:
!            ! A->A
!        
!            itr = 0
!            do i=1,size(obj%elemnod,1)
!                if(obj%elemnod(i,1) == obj%elemnod(i,2))then
!                    obj%elemnod(i,:) = 0
!                    itr=itr+1
!                endif
!            enddo
!        
!            ! A -> B
!            ! B <- A
!            do i=1,size(obj%elemnod,1)
!                if(obj%elemnod(i,1)==0 )then
!                    cycle
!                endif
!                node1 = obj%elemnod(i,1)
!                node2 = obj%elemnod(i,2)
!                do j=i+1,size(obj%elemnod,1)
!                    if(obj%elemnod(j,1)==0 )then
!                        cycle
!                    endif
!                    if(obj%elemnod(j,1) == node1 .and. &
!                        obj%elemnod(j,2) == node2)then
!                        obj%elemnod(j,:)=0
!                        itr=itr+1
!                    endif
!                    if(obj%elemnod(j,1) == node2 .and. &
!                        obj%elemnod(j,2) == node1)then
!                        obj%elemnod(j,:)=0
!                        itr=itr+1
!                    endif
!                enddo
!            enddo

        
            ! case 2
            ! D->A
            ! A->B
            ! B->C
            ! C->A
            ! >> triangle-exception
            ! Find cyclic graph

        !    node1 = 2
        !    node2 = 1
        !    node3 = 1
        !    node4 = 1
        !    allocate(checked(size(obj%elemnod,1)) )
        !    allocate(checked_node(size(obj%nodcoord,1)) )
        !    checked(:) = 0
        !    checked_node(:) = 0
        !    count=0
        !    do 
        !
        !        prev_node1=node1
        !        ! triangle-exception探索
        !        ! 通った要素はchecked=1
        !        tri_excep=0
        !
        !        do i=1,size(obj%elemnod,1)
        !            
        !            if(checked(i) == 1) then
        !                cycle
        !            endif
        !
        !            if(obj%elemnod(i,1) == 0 )then
        !                cycle
        !            endif
        !
        !            checked(i) = 1
        !
        !            checked_node(obj%elemnod(i,1))=1 
        !            checked_node(obj%elemnod(i,2))=1
        !            
        !            ! Find next node >> append
        !            if(obj%elemnod(i,1) == node1 .and. &
        !                obj%elemnod(i,2) /= node2)then
        !                node4 = node3
        !                node3 = node2
        !                node2 = node1 
        !                node1 = obj%elemnod(i,2)
        !                checked_node(node1) = 1
        !                checked_node(node2) = 1
        !                checked_node(node3) = 1
        !                checked_node(node4) = 1
        !                tri_excep=i
        !                checked(i) = 1
        !                exit
        !            elseif(obj%elemnod(i,2) == node1 .and. &
        !                obj%elemnod(i,1) /= node2)then
        !                node4 = node3
        !                node3 = node2
        !                node2 = node1 
        !                node1 = obj%elemnod(i,1)
        !                checked_node(node1) = 1
        !                checked_node(node2) = 1
        !                checked_node(node3) = 1
        !                checked_node(node4) = 1
        !                tri_excep=i
        !                checked(i) = 1
        !                exit
        !            else
        !                cycle
        !            endif
        !
        !        enddo
        !
        !
        !        print *, node1, node2,node3,node4
        !    
        !        print *, countif(Array=checked,Equal=.true.,value=0),"/",size(checked)
        !
        !
        !        if(prev_node1 == node1) then
        !            exit
        !        endif
        !
        !        if(countif(Array=checked,Equal=.true.,value=0) ==0  ) then
        !            exit
        !        endif
        !
        !
        !        if(tri_excep==0)then
        !            do j=1,size(checked)
        !                if(checked(j)==0 .and. obj%elemnod(j,1)/=0 )then
        !                    if(checked_node(obj%elemnod(j,1 ))==0 )then
        !                        node1 = obj%elemnod(j,1 )
        !                        
        !                        exit
        !                    elseif(checked_node(obj%elemnod(j,2 ))==0 )then
        !                        node1 = obj%elemnod(j,2 )
        !                        exit
        !                    else
        !                        cycle
        !                    endif
        !                endif
        !            enddo
        !        endif
        !
        !        if(node1==node4 .and. tri_excep/=0)then
        !            itr=itr+1
        !            ! triangle-exception
        !            obj%elemnod(tri_excep,:) = 0
        !            ! checkされてない中で最も接点番号が若いものから再スタート
        !            do j=1,size(checked)
        !                if(checked(j)==0 .and. obj%elemnod(j,1)/=0 )then
        !                    if(checked_node(obj%elemnod(j,1 ))==0 )then
        !                        node1 = obj%elemnod(j,1 )
        !                        
        !                        exit
        !                    elseif(checked_node(obj%elemnod(j,2 ))==0 )then
        !                        node1 = obj%elemnod(j,2 )
        !                        exit
        !                    else
        !                        cycle
        !                    endif
        !                    cycle
        !                endif
        !            enddo
        !        endif
        !
        !
        !    enddo
        !
        !    do i=1,size(obj%elemnod,1)
        !        node1 = obj%elemnod(i,1)
        !        node2 = obj%elemnod(i,2)
        !        do j=i+1,size(obj%elemnod,1)
        !            if(obj%elemnod(j,1) == node1 )then
        !                node3 = obj%elemnod(j,2)
        !            endif
        !            if(obj%elemnod(j,1) == node2 )then
        !                node3 = obj%elemnod(j,2)
        !            endif
        !        enddo
        !    enddo
        !
        !
        
!            elemnod = obj%elemnod
!            deallocate(obj%elemnod)
!            allocate(obj%elemnod(size(elemnod,1)-itr,8 ) )
!            obj%elemnod(:,:)=0
!            itr=0
!            do i=1,size(elemnod,1)
!                if(minval(elemnod(i,:))==0 )then
!                    cycle
!                else
!                    itr=itr+1
!                    obj%elemnod(itr,:) = elemnod(i,:)
!                endif
!            enddo
!            return
        endif
        ! initialize root
        !   o  (0,0,0)
        !   |
        !   |
        !   o  (0,0,-1)
        allocate(obj%nodcoord(2,3))
        obj%nodcoord(:,:) = 0.0d0
        obj%nodcoord(2,3) = -1.0d0

        obj%nodcoord(2,1) = input(default=obj%nodcoord(2,1), option=x)
        obj%nodcoord(2,2) = input(default=obj%nodcoord(2,2), option=y)
        obj%nodcoord(2,3) = input(default=obj%nodcoord(2,3), option=z)

        obj%nodcoord(2,1) = input(default=obj%nodcoord(2,1), option=dx)
        obj%nodcoord(2,2) = input(default=obj%nodcoord(2,2), option=dy)
        obj%nodcoord(2,3) = input(default=obj%nodcoord(2,3), option=dz)

        allocate(obj%elemnod(1,8))
        obj%elemnod(1,1) = 1
        obj%elemnod(1,2:8) = 2
    endif


    if(meshtype=="Node-To-Segment" .or. meshtype=="node-to-segment") then
        if(.not. present(master) )then
            call print("ERROR :: please input FEMDomain_-typed object to master")
        endif
        if(.not. present(slave) )then
            call print("ERROR :: please input FEMDomain_-typed object to slave")
        endif

        ! create Node-To-Node elements
        call obj%create(meshtype="Node-To-Node",master=master,slave=slave)

        ! get segment
        ! First, identify facet lists
        
        ! If surface is not obtained, get surface.
        if(.not. allocated(master%FacetElemNod) )then
            call master%getSurface()
        endif
        if(allocated(obj%NTSMasterFacetID) )then
            deallocate(obj%NTSMasterFacetID)
        endif
        allocate(obj%NTSMasterFacetID(size(obj%slaveID)) )

        do i=1,size(obj%SlaveID)
            !print *, slave%nodcoord(obj%SlaveID(i),1:3)
            ! get nearest facet
            ! ignore In/out :: find nearest segment for a node-to-segment pairing
            do j=1,size(master%FacetElemNod,1)
                center(:) = 0.0d0
                xvec(:) = slave%nodcoord(obj%SlaveID(i),1:3)
                do k = 1,size(master%FacetElemNod,2)
                    node_id = master%FacetElemNod(j,k)
                    center(:) = center(:) + master%nodcoord(node_id,:)
                enddo
                center(:) = 1.0d0/dble(size(master%FacetElemNod,2))*center(:)
                dist_tr = sqrt(dot_product(center-xvec,center-xvec))
                if(j==1)then
                    dist = dist_tr
                    nearest_facet_id = j
                else
                    if(dist_tr < dist)then
                        dist = dist_tr
                        nearest_facet_id = j
                    endif
                endif
            enddo
            obj%NTSMasterFacetID(i) = nearest_facet_id
        enddo
        if(allocated(obj%NodCoord) ) deallocate(obj%NodCoord)
        if(allocated(obj%ElemNod) ) deallocate(obj%ElemNod)
        if(allocated(obj%ElemMat) ) deallocate(obj%ElemMat)
        ! nodal coordinate >> slave1, master1, master2, ...
        allocate(obj%NodCoord(size(obj%slaveid)*(size(master%FacetElemNod,2)+1),3 ) )
        node_id = 0
        do i=1, size(obj%slaveID)
            node_id = node_id+1
            obj%NodCoord(node_id,:) = slave%nodcoord(obj%slaveID(i),:)
            do j=1,size(master%FacetElemNod,2)
                node_id = node_id+1
                obj%NodCoord(node_id,:) = &
                master%nodcoord( master%FacetElemNod(obj%NTSMasterFacetID(i),j),:)
            enddo 
        enddo

        allocate(obj%ElemNod(size(obj%slaveid),size(slave%ElemNod,2)) )
        node_id = 0
        do i=1,size(obj%ElemNod,1)
            do j=1,size(master%FacetElemNod,2)
                node_id = node_id + 1
                obj%elemnod(i,j:) = node_id
            enddo
        enddo
        allocate(obj%ElemMat(size(obj%slaveid) ) )
        obj%ElemMat(:) = 1

        !call print(obj%nodcoord)
        !call print(obj%elemnod)
        !stop

        ! get local coordinate (xi_1, xi_2)
        if(size(master%FacetElemNod,2) /=4)then
            ! if not 8-node isoparametric elements,
            call print("createMesh(NTS) >>  not 8-node isoparametric elements >> no xi-local codinate is created")
            call print("Not supported now.")
            return
        endif

        allocate(obj%xi(size(obj%ElemNod,1),2 ) )
        ! initialize shape function
        !call shape%init(ElemType="LinearRectangularGp4")
        do i=1,size(obj%elemnod,1)
            x1vec(:) = obj%nodcoord(obj%elemnod(i,4),:)-obj%nodcoord(obj%elemnod(i,3),:)
            x2vec(:) = obj%nodcoord(obj%elemnod(i,2),:)-obj%nodcoord(obj%elemnod(i,3),:)
            nvec(:) = cross_product(x1vec, x2vec)
            nvec(:) = 1.0d0/sqrt(dot_product(nvec,nvec) )*nvec(:)
            ! foot of the node
            xvec(:) = obj%nodcoord(obj%elemnod(i,1),:) - obj%nodcoord(obj%elemnod(i,3),:)
            hvec(:) = obj%nodcoord(obj%elemnod(i,1),:) - dot_product(xvec,nvec)*nvec(:)
            ! 4-node
            ! create shape function
            !call shape%getall()
            !do j=1,4
            !    call obj%GetAll(elem_id=1,nod_coord=NodCoord,elem_nod=ElemNod,OptionalGpID=j)
            !enddo
        enddo
        

    endif 

    if(meshtype=="Node-To-Node" .or. meshtype=="node-to-node") then
        call master%GetInterSectBox(slave,BoundBox)
        if( BoundBox%empty() .eqv. .true. ) then
            call print("No interface")
            return
        else
            call print("Contact interface detected.")
            ! get master and slave nodes
            ! Global search for master node by AABB algorithm (Bounding-Box method)
            dim_num = size(master%nodcoord,2) 
            node_num = size(master%nodcoord,1)
            allocate(OutNodeID(size(master%nodcoord,1) ) )
            OutNodeID(:) = 0
            do i=1,size(master%nodcoord,1)
                xvec(:) = 0.0d0
                x_max(:) = 0.0d0
                x_min(:) = 0.0d0
                xvec(1:size(master%nodcoord,2)) = master%nodcoord(i,1:size(master%nodcoord,2) )
    
                do j=1,size(BoundBox%NodCoord,2)
                    x_max(j) = maxval(BoundBox%NodCoord(:,j) )
                enddo
                do j=1,size(BoundBox%NodCoord,2)
                    x_min(j) = minval(BoundBox%NodCoord(:,j) )
                enddo
                ! Judge inside or not
                inside = InOrOut(x=xvec,xmax=x_max,xmin=x_min)
                if(inside .eqv. .false.)then
                    OutNodeID(i)=1    
                endif
            enddo

            call print("Interface node :: "//str(node_num - sum(OutNodeID))//"/"//str(node_num) )
            
            allocate(interface1%nodcoord(node_num - sum(OutNodeID) , dim_num ) )
            j=0
            do i=1,size(master%Nodcoord,1)
                if(OutNodeID(i)==1 )then
                    ! out >> ignore the node
                    cycle
                else
                    j=j+1
                    interface1%nodcoord(j,:) = master%Nodcoord(i,:)
                endif
            enddo

            allocate(OutElementID(size(master%elemnod,1) ) )
            k=0
            OutElementID(:) = 0
            do i=1,size(master%elemnod,1)
                do j=1,size(master%elemnod,2)
                    if(OutNodeID(master%elemnod(i,j) )==1)then
                        ! out element
                        k=k+1
                        OutElementID(i) = 1
                        exit
                    endif
                enddo
            enddo

            call print("Interface element :: "//str(size(master%elemnod,1) - k)//"/"//str(size(master%elemnod,1)) )
            allocate(interface1%elemnod(size(master%elemnod,1) - k ,size(master%elemnod,2) ) )
            k=0
            do i=1,size(OutElementID,1)
                if(OutElementID(i) == 1 )then
                    cycle
                else
                    k=k+1
                    do j=1,size(master%elemnod,2)
                        interface1%elemnod(k,j) = master%elemnod(i,j) - sum(OutNodeID(1:master%elemnod(i,j)-1 ))
                    enddo
                endif
            enddo


            deallocate(OutElementID)
            deallocate(OutNodeID)
            
            ! global search for slave
            dim_num = size(slave%nodcoord,2) 
            node_num = size(slave%nodcoord,1)
            allocate(OutNodeID(size(slave%nodcoord,1) ) )
            OutNodeID(:) = 0
            do i=1,size(slave%nodcoord,1)
                xvec(:) = 0.0d0
                x_max(:) = 0.0d0
                x_min(:) = 0.0d0
                xvec(1:size(slave%nodcoord,2)) = slave%nodcoord(i,1:size(slave%nodcoord,2) )
    
                do j=1,size(BoundBox%NodCoord,2)
                    x_max(j) = maxval(BoundBox%NodCoord(:,j) )
                enddo
                do j=1,size(BoundBox%NodCoord,2)
                    x_min(j) = minval(BoundBox%NodCoord(:,j) )
                enddo
                ! Judge inside or not
                inside = InOrOut(x=xvec,xmax=x_max,xmin=x_min)
                if(inside .eqv. .false.)then
                    OutNodeID(i)=1    
                endif
            enddo
            call print("Interface node :: "//str(node_num - sum(OutNodeID))//"/"//str(node_num) )
            allocate(interface2%nodcoord(node_num - sum(OutNodeID) , dim_num ) )
            j=0
            do i=1,size(slave%Nodcoord,1)
                if(OutNodeID(i)==1 )then
                    ! out >> ignore the node
                    cycle
                else
                    j=j+1
                    interface2%nodcoord(j,:) = slave%Nodcoord(i,:)
                endif
            enddo

            allocate(OutElementID(size(slave%elemnod,1) ) )
            k=0
            OutElementID(:) = 0
            do i=1,size(slave%elemnod,1)
                do j=1,size(slave%elemnod,2)
                    if(OutNodeID(slave%elemnod(i,j) )==1)then
                        ! out element
                        k=k+1
                        OutElementID(i) = 1
                        exit
                    endif
                enddo
            enddo

            call print("Interface element :: "//str(size(slave%elemnod,1) - k)//"/"//str(size(slave%elemnod,1)) )
            allocate(interface2%elemnod(size(slave%elemnod,1) - k ,size(slave%elemnod,2) ) )
            k=0
            do i=1,size(OutElementID,1)
                if(OutElementID(i) == 1 )then
                    cycle
                else
                    k=k+1
                    do j=1,size(slave%elemnod,2)
                        interface2%elemnod(k,j) = slave%elemnod(i,j) - sum(OutNodeID(1:slave%elemnod(i,j)-1 ))
                    enddo
                endif
            enddo


            deallocate(OutElementID)
            deallocate(OutNodeID)
            

            
            !obj%nodcoord = interface2%nodcoord
            !obj%elemnod = interface2%elemnod

!            ! again get boundary box
!            print *, maxval(interface1%nodcoord(:,1)), maxval(interface1%nodcoord(:,2)), maxval(interface1%nodcoord(:,3))
!            print *, minval(interface1%nodcoord(:,1)), minval(interface1%nodcoord(:,2)), minval(interface1%nodcoord(:,3))
!            print *, maxval(interface2%nodcoord(:,1)), maxval(interface2%nodcoord(:,2)), maxval(interface2%nodcoord(:,3))
!            print *, minval(interface2%nodcoord(:,1)), minval(interface2%nodcoord(:,2)), minval(interface2%nodcoord(:,3))
!            
!            call interface1%GetInterSectBox(interface2,BoundBox)
!            
!            call interface1%remove(x_max=minval(BoundBox%nodcoord(:,1)) )
!            call interface1%remove(y_max=minval(BoundBox%nodcoord(:,2)) )
!            call interface1%remove(z_max=minval(BoundBox%nodcoord(:,3)) )
!
!            call interface1%remove(x_min=maxval(BoundBox%nodcoord(:,1)) )
!            call interface1%remove(y_min=maxval(BoundBox%nodcoord(:,2)) )
!            call interface1%remove(z_min=maxval(BoundBox%nodcoord(:,3)) )
!            
!            call interface2%remove(x_max=minval(BoundBox%nodcoord(:,1)) )
!            call interface2%remove(y_max=minval(BoundBox%nodcoord(:,2)) )
!            call interface2%remove(z_max=minval(BoundBox%nodcoord(:,3)) )
!
!            call interface2%remove(x_min=maxval(BoundBox%nodcoord(:,1)) )
!            call interface2%remove(y_min=maxval(BoundBox%nodcoord(:,2)) )
!            call interface2%remove(z_min=maxval(BoundBox%nodcoord(:,3)) )
!            
!
!            

            call print("Global Search Done!")



            



            call print("local search >> ")

            ! pairing 
            ! link Node-To-Node
            allocate(obj%nodcoord(size(interface2%nodcoord,1)*2,size(interface2%nodcoord,2)  ) )
            node_num = size(interface2%nodcoord,1)
            ! =
            ! slave-node #1  x, y
            ! slave-node #2  x, y
            ! slave-node #3  x, y
            ! slave-node #4  x, y
            ! slave-node #5  x, y
            ! ...
            ! master-node #1  x, y
            ! master-node #2  x, y
            ! master-node #3  x, y
            ! master-node #4  x, y
            ! master-node #5  x, y
            ! ...
            
            allocate(obj%elemnod(size(interface2%nodcoord,1),  8 )) ! slave-node, master-node
            
            do j=1, size(interface2%nodcoord,1)! for each slave node

                ! どっちがmasterか気をつける。

                ! initialize
                obj%elemnod(j,1) = j  ! slave node
                obj%elemnod(j,2:8) = j+node_num  ! master node
                x_s_mid(1:dim_num) = interface2%nodcoord(j,1:dim_num)
                x_m_mid(1:dim_num) = interface1%nodcoord(1,1:dim_num)
                dist_cur = dsqrt(dot_product(x_m_mid- x_s_mid, x_m_mid-x_s_mid ) )

                ! get nearest master node
                obj%nodcoord(j,1:dim_num) = interface2%nodcoord(j,1:dim_num) ! slave node
                do i=1,size(interface1%nodcoord,1) ! for each master node
                    x_s_mid(:) = 0.0d0
                    x_s_mid(1:dim_num) = interface2%nodcoord(j,1:dim_num)
                    x_m_mid(:) = 0.0d0
                    x_m_mid(1:dim_num) = interface1%nodcoord(i,1:dim_num)
                    dist_tr = dsqrt(dot_product(x_m_mid- x_s_mid, x_m_mid-x_s_mid ) )
                    if(dist_tr <= dist_cur)then
                        dist_cur = dist_tr
                        obj%nodcoord(j+node_num,:) = interface1%nodcoord(i,:) ! master node
                    endif
                enddo

            enddo

            allocate(obj%masterID(node_num) )
            allocate(obj%slaveID(node_num) )

            ! search master ids
            do j=1,size(interface2%nodcoord,1)
                do i=1,size(master%nodcoord,1)
                    xvec(:) = 0.0d0
                    x_m_mid(:) = 0.0d0
                    xvec(1:dim_num) = obj%nodcoord(j+node_num,1:dim_num)
                    x_m_mid(1:dim_num) = master%nodcoord(i,1:dim_num)
                    dist_tr = dsqrt(dot_product(xvec-x_m_mid, xvec-x_m_mid ) )
                    if(dist_tr == 0.0d0)then
                        obj%masterID(j) = i
                        exit
                    endif
                enddo
            enddo
            
            ! search slave ids
            do j=1,size(interface2%nodcoord,1)
                do i=1,size(slave%nodcoord,1)
                    xvec(:) = 0.0d0
                    x_m_mid(:) = 0.0d0
                    xvec(1:dim_num) =  obj%nodcoord(j,1:dim_num)
                    x_m_mid(1:dim_num) = slave%nodcoord(i,1:dim_num)
                    dist_tr = dsqrt(dot_product(xvec-x_m_mid, xvec-x_m_mid ) )
                    if(dist_tr == 0.0d0)then
                        obj%slaveID(j) = i
                        exit
                    endif
                enddo
            enddo

        endif
        

        return
    endif

    if(meshtype=="Leaf3D")then
        validmeshtype=.true.
        call obj%create(meshtype="rectangular3D",x_num=x_num,&
        y_num=y_num,x_len=x_len,y_len=y_len,Le=Le,Lh=Lh,Dr=Dr,thickness=thickness,&
        division=division,smooth=smooth,top=top,margin=margin,inclineRate=inclineRate)
        obj%NodCoord(:,1) =obj%NodCoord(:,1) - (maxval(obj%NodCoord(:,1))-minval(obj%NodCoord(:,1)))*0.50d0
        obj%NodCoord(:,2) =obj%NodCoord(:,2)  - (maxval(obj%NodCoord(:,2))-minval(obj%NodCoord(:,2)))*0.50d0
        
        ! shape like this
        !
        !           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%  B
        !         %%                        %   %
        !        %%                    %      %%  
        !      %%                 %          %%    
        !     %%            %              %%      
        !     %%      %                  %%        
        !     %%                       %%          
        !   A   %%                  %%            
        !      <I> %%%%%%%%%%%%%%%%                               
        call obj%clean()

        if(present(species) )then
            if(species == PF_GLYCINE_MAX)then
                ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                ! TOMOBE model (Tomobe 2021, in prep.) 
                zm = minval(obj%NodCoord(:,3) )
                length =maxval(obj%NodCoord(:,3) )- minval(obj%NodCoord(:,3) )
                width = maxval(obj%NodCoord(:,1) )- minval(obj%NodCoord(:,1) )
                zl = maxval(obj%NodCoord(:,3) )- minval(obj%NodCoord(:,3) )

                swratio = input(default=0.50d0,option=SoyWidthRatio)
                if(swratio>=1.0d0 .or. swratio<=0.0d0 )then
                    print *, "ERROR  >> mesh%create(leaf3d, PF_SOYBEAN) >> invalid SoyWidthRatio ",SoyWidthRatio
                    stop 
                endif

                do i=1,size(obj%nodcoord,1)
                    xx = obj%nodcoord(i,3)
                    if(obj%NodCoord(i,1) <= (maxval(obj%NodCoord(:,1) ) + minval(obj%NodCoord(:,1)))*0.50d0  )then
                        alpha = swratio*width
                    else
                        alpha = (1.0d0-swratio)*width
                    endif
                    r      = (alpha**2 + (length - alpha)**2)/(2*alpha)*1.20d0 
                    if(xx <= 1.0d0/25.0d0*length)then
                        obj%NodCoord(i,1) = obj%NodCoord(i,1)*1.0d0/10.0d0
                        cycle
                    elseif(xx < alpha)then
                        yy = sqrt( alpha**2 - (xx-alpha)**2 )
                        yy_ = xx
                        yy = lin_curve_ratio*yy + (1.0d0-lin_curve_ratio)*yy_
                    else
                        yy_ = alpha + (-alpha)/(length-alpha)*(xx - alpha)
                        yy = alpha - r + sqrt(r**2 - (xx - alpha)**2 )
                        yy = lin_curve_ratio*yy + (1.0d0-lin_curve_ratio)*yy_
                    endif
                    yy = abs(yy)
                    obj%nodcoord(i,1) = obj%nodcoord(i,1)*(yy/alpha)
                enddo

                ! TOMOBE model (Tomobe 2021, in prep.) 
                ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            elseif(species == PF_MAIZE)then
                ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                ! TOMOBE model (Tomobe 2021, in prep.) 
                zm = minval(obj%NodCoord(:,3) )
                length =maxval(obj%NodCoord(:,3) )- minval(obj%NodCoord(:,3) )
                width = maxval(obj%NodCoord(:,1) )- minval(obj%NodCoord(:,1) )
                zl = maxval(obj%NodCoord(:,3) )- minval(obj%NodCoord(:,3) )

                swratio = input(default=0.50d0,option=SoyWidthRatio)
                if(swratio>=1.0d0 .or. swratio<=0.0d0 )then
                    print *, "ERROR  >> mesh%create(leaf3d, PF_SOYBEAN) >> invalid SoyWidthRatio ",SoyWidthRatio
                    stop 
                endif

                do i=1,size(obj%nodcoord,1)
                    xx = obj%nodcoord(i,3)
                    if(obj%NodCoord(i,1) <= (maxval(obj%NodCoord(:,1) ) + minval(obj%NodCoord(:,1)))*0.50d0  )then
                        alpha = swratio*width
                    else
                        alpha = (1.0d0-swratio)*width
                    endif
                    r      = (alpha**2 + (length - alpha)**2)/(2*alpha)*1.20d0 
                    if(xx <= 1.0d0/25.0d0*length)then
                        obj%NodCoord(i,1) = obj%NodCoord(i,1)*1.0d0/10.0d0
                        cycle
                    elseif(xx < alpha)then
                        yy = sqrt( alpha**2 - (xx-alpha)**2 )
                        yy_ = xx
                        yy = lin_curve_ratio*yy + (1.0d0-lin_curve_ratio)*yy_
                    else
                        yy_ = alpha + (-alpha)/(length-alpha)*(xx - alpha)
                        yy = alpha - r + sqrt(r**2 - (xx - alpha)**2 )
                        yy = lin_curve_ratio*yy + (1.0d0-lin_curve_ratio)*yy_
                    endif
                    yy = abs(yy)
                    obj%nodcoord(i,1) = obj%nodcoord(i,1)*(yy/alpha)
                enddo

                ! TOMOBE model (Tomobe 2021, in prep.) 
                ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            
            else
                print *, "[ERROR] Mesh%create =>  No such species as ",species
                stop
            endif
        else
            do i=1,size(obj%NodCoord,1)
                zc = obj%NodCoord(i,3)
                zm = minval(obj%NodCoord(:,3) )
                width = maxval(obj%NodCoord(:,1) )- minval(obj%NodCoord(:,1) )
                width = width/2.0d0
                zl = maxval(obj%NodCoord(:,3) )- minval(obj%NodCoord(:,3) )

                if(zc <= 1.0d0/20.0d0*zl)then
                    ratio = 1.0d0/10.0d0 
                elseif(1.0d0/20.0d0*zl < zc .and. zc <= zl*shaperatio )then
                    ratio = 1.0d0/10.0d0 + 0.90d0/(zl*shaperatio - 1.0d0/20.0d0*zl)*(zc - 1.0d0/20.0d0*zl)
                else
                    ratio = 1.0d0 -0.90d0/(zl - shaperatio*zl)*(zc - shaperatio*zl)
                endif

                obj%NodCoord(i,1) = obj%NodCoord(i,1)*ratio

            enddo
        endif
    endif

    if(meshtype=="HalfSphere3D")then
        validmeshtype=.true.
        call obj%create(meshtype="Sphere3D",x_num=x_num,y_num=y_num,x_len=x_len,&
        y_len=y_len,Le=Le,Lh=Lh,Dr=Dr,thickness=thickness,&
        division=division,smooth=smooth,top=top,margin=margin,inclineRate=inclineRate)

        ! remove half by x-z plane
        ysize = maxval(obj%NodCoord(:,2) ) - minval(obj%NodCoord(:,2) )
        call obj%remove(y_max=ysize/2.0d0-dble(1.0e-8))

    endif

    if(meshtype=="Bar1D" .or. meshtype=="bar1D")then
        ! need x_len, x_num
        validmeshtype=.true.
        if(allocated(obj%NodCoord)) deallocate(obj%NodCoord)
        if(allocated(obj%ElemNod)) deallocate(obj%ElemNod)
        if(allocated(obj%ElemMat)) deallocate(obj%ElemMat)
        
        n=input(default=10,option=x_num)
        allocate(obj%NodCoord(n+1,1) )
        allocate(obj%ElemNod(n,2) )
        allocate(obj%ElemMat(n) )
        
        lx=input(default=10.0d0,option=x_len)
        do i=1,n+1
            obj%NodCoord(i,1)=dble(i-1)*lx/n
            
        enddo
        do i=1,n
            obj%ElemNod(i,1)=i
            obj%ElemNod(i,2)=i+1
            obj%ElemMat(i)=1
        enddo

    endif

    if(meshtype=="rectangular3D" .or. meshtype=="Cube")then
        validmeshtype=.true.
        call obj%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len)
        call obj%Convert2Dto3D(Thickness=Thickness,division=division)
        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif

        ! create direction-data
        obj%BottomElemID = (x_num)*(y_num)/2
        obj%TopElemID    = (x_num)*(y_num)/2 + (x_num)*(y_num)*(division-1)

    endif


    if(meshtype=="Cube3D" .or.meshtype=="cube3D")then
        validmeshtype=.true.
        call obj%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len)
        call obj%Convert2Dto3D(Thickness=Thickness,division=division)
        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif

        ! create direction-data
        obj%BottomElemID = (x_num)*(y_num)/2
        obj%TopElemID    = (x_num)*(y_num)/2 + (x_num)*(y_num)*(division-1)

    endif


    if(meshtype=="Dam3D" )then
        validmeshtype=.true.
        call obj%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len)
        
        xm=0.50d0*maxval(obj%NodCoord(:,1) )+0.50d0*minval(obj%NodCoord(:,1) )
        ym=0.50d0*maxval(obj%NodCoord(:,2) )+0.50d0*minval(obj%NodCoord(:,2) )
        lx=maxval(obj%NodCoord(:,1) )- minval(obj%NodCoord(:,1) )
        ly=maxval(obj%NodCoord(:,2) )- minval(obj%NodCoord(:,2) )
        ymin=minval(obj%NodCoord(:,2))
        obj%NodCoord(:,1)=obj%NodCoord(:,1)-xm
        obj%NodCoord(:,2)=obj%NodCoord(:,2)-ymin
        tp = input(default=ly*1.50d0,option=top)

        if(top < ly)then
            print *, "ERROR createMesh >> top < ly"
            stop 
        endif
        do i=1,size(obj%NodCoord,1)
            ry = obj%NodCoord(i,2)
            rx = (top-ry)*lx*0.50d0/top
            obj%NodCoord(i,1) = obj%NodCoord(i,1)/(lx*0.50d0)*rx
        enddo
        ! add mesh
        call mesh1%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len)
        call mesh2%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len)
        ymax=maxval(mesh1%NodCoord(:,2) )
        mesh1%NodCoord(:,1)=mesh1%NodCoord(:,1)
        mesh1%NodCoord(:,2)=mesh1%NodCoord(:,2)-ymax
        mesh2%NodCoord(:,1)=mesh2%NodCoord(:,1)-2.0d0*xm
        mesh2%NodCoord(:,2)=mesh2%NodCoord(:,2)-ymax

        print *, "deo"
        call obj%add(mesh1)
        call obj%add(mesh2)
        print *, "deo"
        call showArray(obj%NodCoord,IndexArray=obj%ElemNod,Name="text.txt")
        print *, "ERROR :: Dam3D is not implemented yet."
        stop
        !call obj%removeOverlappedNode()
        call obj%Convert2Dto3D(Thickness=Thickness,division=division)
        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif
        return
    endif

    if(meshtype=="Trapezoid2D" .or. meshtype=="Ridge2D")then
        validmeshtype=.true.
        call obj%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len)
        
        xm=0.50d0*maxval(obj%NodCoord(:,1) )+0.50d0*minval(obj%NodCoord(:,1) )
        ym=0.50d0*maxval(obj%NodCoord(:,2) )+0.50d0*minval(obj%NodCoord(:,2) )
        lx=maxval(obj%NodCoord(:,1) )- minval(obj%NodCoord(:,1) )
        ly=maxval(obj%NodCoord(:,2) )- minval(obj%NodCoord(:,2) )
        obj%NodCoord(:,1)=obj%NodCoord(:,1)-xm
        tp = input(default=ly*1.50d0,option=top)
        if(top < ly)then
            print *, "ERROR createMesh >> top < ly"
            stop 
        endif
        do i=1,size(obj%NodCoord,1)
            ry = obj%NodCoord(i,2)
            rx = (top-ry)*lx*0.50d0/top
            obj%NodCoord(i,1) = obj%NodCoord(i,1)/(lx*0.50d0)*rx
        enddo
        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif
        return
    endif

    if(meshtype=="Trapezoid3D" .or. meshtype=="Ridge3D")then
        validmeshtype=.true.
        call obj%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len)
        
        xm=0.50d0*maxval(obj%NodCoord(:,1) )+0.50d0*minval(obj%NodCoord(:,1) )
        ym=0.50d0*maxval(obj%NodCoord(:,2) )+0.50d0*minval(obj%NodCoord(:,2) )
        lx=maxval(obj%NodCoord(:,1) )- minval(obj%NodCoord(:,1) )
        ly=maxval(obj%NodCoord(:,2) )- minval(obj%NodCoord(:,2) )
        obj%NodCoord(:,1)=obj%NodCoord(:,1)-xm
        tp = input(default=ly*1.50d0,option=top)
        if(top < ly)then
            print *, "ERROR createMesh >> top < ly"
            stop 
        endif
        do i=1,size(obj%NodCoord,1)
            ry = obj%NodCoord(i,2)
            rx = (top-ry)*lx*0.50d0/top
            obj%NodCoord(i,1) = obj%NodCoord(i,1)/(lx*0.50d0)*rx
        enddo

        call obj%Convert2Dto3D(Thickness=Thickness,division=division)
        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif
        return
    endif

    
    if(meshtype=="Sphere3D" .or. meshtype=="Sphere")then
        validmeshtype=.true.
        call obj%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=1.0d0,y_len=1.0d0)       
        call obj%Convert2Dto3D(Thickness=1.0d0,division=division)
        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif
        call obj%AdjustSphere(debug=.true.)
        call obj%clean()
        call obj%resize(x_rate=x_len,&
            y_rate=y_len,&
            z_rate=thickness)
        return
    endif


    if(meshtype=="HQSphere3D" .or. meshtype=="HQSphere")then
        validmeshtype=.true.
        call obj%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=1.0d0,y_len=1.0d0)       
        call obj%Convert2Dto3D(Thickness=1.0d0,division=division)

        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif
        
        call obj%AdjustSphere(debug=.true.)
        call obj%resize(x_rate=x_len,&
            y_rate=y_len,&
            z_rate=thickness)
        return
    endif

    if(meshtype=="Cylinder3D" .or. meshtype=="Cylinder")then
        validmeshtype=.true.
        call obj%create(meshtype="Circle2D",x_num=x_num,y_num=y_num,x_len=1.0d0,y_len=1.0d0)       
        call obj%Convert2Dto3D(Thickness=thickness,division=division)
        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif
        !call obj%adjustCylinder(debug=.true.)
        ! move unconnected nodes
        call obj%clean()
        call obj%resize(x_rate=2.0d0*x_len,&
            y_rate=2.0d0*y_len,&
            z_rate=thickness)
        return
    endif

    if(meshtype=="Circle2D" .or. meshtype=="Circle")then
        validmeshtype=.true.
        ! create mesh by scheme-circle method
        ! https://support.jpmandt.com/mesh/create-mesh/surface-create-mesh/scheme-circle/
        ! fraction:interval = 1:1
        xn = input(default=10,option=x_num/2+1)
        yn = input(default=10,option=y_num/2+1)
        ! x方向とy方向のうち、より分割数が多い方に合わせる
        if(xn <= ym)then
            xn = ym
        else
            yn = xn
        endif
        ! 正方形ができる。
        call obj%create(meshtype="rectangular2D",x_num=2*xn,y_num=2*yn,x_len=2.0d0,y_len=2.0d0)     

        obj%nodcoord(:,1)=obj%nodcoord(:,1)-1.0d0
        obj%nodcoord(:,2)=obj%nodcoord(:,2)-1.0d0
        
        ! 正方形を整形して、円とのコネクティビティを改善
        do i=1,size(obj%nodCoord,1)
            xx = obj%nodCoord(i,1)
            yy = obj%nodCoord(i,2)
            if(xx>=0.0d0 .and. yy>=0.0d0)then
                obj%nodCoord(i,1) = xx + xx*(sqrt(2.0d0)-1.0d0)*(1.0d0-yy)
                obj%nodCoord(i,2) = yy + yy*(sqrt(2.0d0)-1.0d0)*(1.0d0-xx)
            elseif(xx< 0.0d0 .and. yy>=0.0d0)then
                obj%nodCoord(i,1) = xx + xx*(sqrt(2.0d0)-1.0d0)*(1.0d0-yy)
                obj%nodCoord(i,2) = yy + yy*(sqrt(2.0d0)-1.0d0)*(1.0d0+xx)
            elseif(xx< 0.0d0 .and. yy< 0.0d0)then
                obj%nodCoord(i,1) = xx + xx*(sqrt(2.0d0)-1.0d0)*(1.0d0+yy)
                obj%nodCoord(i,2) = yy + yy*(sqrt(2.0d0)-1.0d0)*(1.0d0+xx)
            elseif(xx>=0.0d0 .and. yy< 0.0d0)then
                obj%nodCoord(i,1) = xx + xx*(sqrt(2.0d0)-1.0d0)*(1.0d0+yy)
                obj%nodCoord(i,2) = yy + yy*(sqrt(2.0d0)-1.0d0)*(1.0d0-xx)
            else
                print *, "ERROR :: createMesh >> circle error"
                stop 
            endif
        enddo
        if(present(meshtype) .and. validmeshtype .eqv. .false. )then
            print *, "createMesh%error :: no such mesh as ", trim(meshtype)
            return
        endif

        !obj%nodcoord(:,1)=obj%nodcoord(:,1)*0.650d0
        !obj%nodcoord(:,2)=obj%nodcoord(:,2)*0.650d0

        obj%nodcoord(:,1)=dble(2*xn-1)/dble(2*xn)*obj%nodcoord(:,1)/sqrt(2.0d0)
        obj%nodcoord(:,2)=dble(2*xn-1)/dble(2*xn)*obj%nodcoord(:,2)/sqrt(2.0d0)

        ! 外周メッシュ
        allocate(mesh1%nodcoord( (2*xn)* (2*xn)*4   ,size(obj%nodcoord,2) ) )
        
        do i=1, (2*xn) ! For each layer
            do j=1, (2*xn)*4
                mesh1%nodcoord( (i-1)* (2*xn)*4+ j,1) = (1.0d0 + dble(i)*(1.0d0/dble( (2*xn)) ) )&
                    *cos(2.0d0*pi/4.0d0/dble( (2*xn))*dble(j-1) )
                mesh1%nodcoord( (i-1)* (2*xn)*4+ j,2) = (1.0d0 + dble(i)*(1.0d0/dble( (2*xn)) ) )&
                    *sin(2.0d0*pi/4.0d0/dble( (2*xn))*dble(j-1) )
            enddo
        enddo

        !call print(mat=mesh1%nodcoord,name="circle.txt")
        !call print(mat=obj%nodcoord,name="cube.txt")

        ! 要素
        ! Starts from ElementID: (2*xn+1)*(2*xn+1)
        allocate(mesh1%elemnod(8*xn*(xn+1),4) )
        mesh1%elemnod(:,:)=0
        j=0
        do i=1,xn
            j=j+1
            mesh1%elemnod(j,1)= (2*xn+1)*(xn+i)
            mesh1%elemnod(j,2)= (2*xn+1)*(2*xn+1)+ j
            mesh1%elemnod(j,3)= (2*xn+1)*(2*xn+1)+ j+1
            mesh1%elemnod(j,4)= (2*xn+1)*(xn+i+1)
        enddo
        do i=1,2*xn
            j=j+1
            mesh1%elemnod(j,1)= (2*xn+1)*(2*xn+1)-i+1
            mesh1%elemnod(j,2)= (2*xn+1)*(2*xn+1)+ j
            mesh1%elemnod(j,3)= (2*xn+1)*(2*xn+1)+ j+1
            mesh1%elemnod(j,4)= (2*xn+1)*(2*xn+1)-i
        enddo
        do i=1,2*xn
            j=j+1
            mesh1%elemnod(j,1)= (2*xn+1)*(2*xn+1)-(2*xn+1)+1-(i-1)*(2*xn+1)
            mesh1%elemnod(j,2)= (2*xn+1)*(2*xn+1)+ j
            mesh1%elemnod(j,3)= (2*xn+1)*(2*xn+1)+ j+1
            mesh1%elemnod(j,4)= (2*xn+1)*(2*xn+1)-(2*xn+1)+1-(i)*(2*xn+1)
        enddo
        do i=1,2*xn
            j=j+1
            mesh1%elemnod(j,1)= i
            mesh1%elemnod(j,2)= (2*xn+1)*(2*xn+1)+ j
            mesh1%elemnod(j,3)= (2*xn+1)*(2*xn+1)+ j+1
            mesh1%elemnod(j,4)= i+1
        enddo
        do i=1,xn
            j=j+1
            mesh1%elemnod(j,1)= (2*xn+1)*i
            mesh1%elemnod(j,2)= (2*xn+1)*(2*xn+1)+ j
            mesh1%elemnod(j,3)= (2*xn+1)*(2*xn+1)+ j+1
            mesh1%elemnod(j,4)= (2*xn+1)*(i+1)
        enddo
        mesh1%elemnod(j,3)= (2*xn+1)*(2*xn+1) +1
        
        do i=1,xn
            ini=j+1
            do k=1,8*xn-1
                j=j+1
                mesh1%elemnod(j,1)= (2*xn+1)*(2*xn+1)+ j - 8*xn
                mesh1%elemnod(j,2)= (2*xn+1)*(2*xn+1)+ j
                mesh1%elemnod(j,3)= (2*xn+1)*(2*xn+1)+ j+1
                mesh1%elemnod(j,4)= (2*xn+1)*(2*xn+1)+ j+1 - 8*xn
            enddo
            j=j+1
            mesh1%elemnod(j,1)= (2*xn+1)*(2*xn+1)+ j - 8*xn
            mesh1%elemnod(j,2)= (2*xn+1)*(2*xn+1)+ j
            mesh1%elemnod(j,3)= (2*xn+1)*(2*xn+1)+ ini
            mesh1%elemnod(j,4)= (2*xn+1)*(2*xn+1)+ ini - 8*xn
        enddo
        !call print(mat=mesh1%elemnod,name="elem.txt")

        allocate(mesh2%nodcoord(size(obj%nodcoord,1)+size(mesh1%nodcoord,1),&
            size(obj%nodcoord,2)) )
        mesh2%nodcoord(1:size(obj%nodcoord,1),1:2)=obj%nodcoord(1:size(obj%nodcoord,1),1:2)
        mesh2%nodcoord(size(obj%nodcoord,1)+1:size(obj%nodcoord,1)+size(mesh1%nodcoord,1),1:2)&
            =mesh1%nodcoord(1:size(mesh1%nodcoord,1),1:2)
        allocate(mesh2%elemnod(size(obj%elemnod,1)+size(mesh1%elemnod,1),&
            size(obj%elemnod,2)) )
        mesh2%elemnod(1:size(obj%elemnod,1),1:4)=obj%elemnod(1:size(obj%elemnod,1),1:4)
        mesh2%elemnod(size(obj%elemnod,1)+1:size(obj%elemnod,1)+size(mesh1%elemnod,1),1:4)&
            =mesh1%elemnod(1:size(mesh1%elemnod,1),1:4)
        !call print(mat=mesh2%elemnod,name="elem2.txt")
        !call print(mat=mesh2%nodcoord,IndexArray=mesh2%elemnod,name="mesh2.txt")

        !call f%open("mesh2.txt")
        !do i=1,size(mesh2%elemnod,1)
        !    do j=1,size(mesh2%elemnod,2)
        !        write(f%fh,*) mesh2%nodcoord(mesh2%elemnod(i,j),:)
        !    enddo
        !    write(f%fh,*) mesh2%nodcoord(mesh2%elemnod(i,1),:)
        !    write(f%fh,*) " "
        !enddo
        !call f%close()

        allocate(mesh2%elemmat(size(mesh2%elemnod,1) ) )
        mesh2%elemmat(:)=1
        call obj%remove()
        obj%nodcoord = mesh2%nodcoord
        obj%elemnod = mesh2%elemnod
        obj%elemmat = mesh2%elemmat
        return
    endif

    if(meshtype=="rectangular2D" .or. meshtype=="Box2D")then
        xn=input(default=1,option=x_num)
        yn=input(default=1,option=y_num)
        lx=input(default=1.0d0,option=x_len)
        ly=input(default=1.0d0,option=y_len)
        unitx=lx/dble(xn)
        unity=ly/dble(yn)
        ! creating rectangular mesh
        allocate(obj%NodCoord( (xn+1)*(yn+1) , 2 ))
        allocate(obj%ElemNod( xn*yn,4) )
        allocate(obj%ElemMat(xn*yn) )
        n=0
        do j=1, yn+1
            do i=1, xn+1
                n=n+1
                obj%NodCoord(n,1)=lx/dble(xn)*dble(i-1)
                obj%NodCoord(n,2)=ly/dble(yn)*dble(j-1)
            enddo
        enddo

        if(present(smooth) )then
            if(smooth .eqv. .true.)then
            
                smoothedge(1)=1
                smoothedge(2)=xn+1
                smoothedge(3)=(xn+1)*yn + 1
                smoothedge(4)=(xn+1)*(yn+1)
                obj%NodCoord(smoothedge(1),1)=obj%NodCoord(smoothedge(1),1)+0.30d0*unitx
                obj%NodCoord(smoothedge(1),2)=obj%NodCoord(smoothedge(1),2)+0.30d0*unity
                obj%NodCoord(smoothedge(2),1)=obj%NodCoord(smoothedge(2),1)-0.30d0*unitx
                obj%NodCoord(smoothedge(2),2)=obj%NodCoord(smoothedge(2),2)+0.30d0*unity
                obj%NodCoord(smoothedge(3),1)=obj%NodCoord(smoothedge(3),1)+0.30d0*unitx
                obj%NodCoord(smoothedge(3),2)=obj%NodCoord(smoothedge(3),2)-0.30d0*unity
                obj%NodCoord(smoothedge(4),1)=obj%NodCoord(smoothedge(4),1)-0.30d0*unitx
                obj%NodCoord(smoothedge(4),2)=obj%NodCoord(smoothedge(4),2)-0.30d0*unity
            endif
        endif
        
        n=1
        obj%ElemNod(1,1)=1
        obj%ElemNod(1,2)=2
        obj%ElemNod(1,3)=yn+3
        obj%ElemNod(1,4)=yn+2
        if(xn>=2)then
            obj%ElemNod(2,1)=2
            obj%ElemNod(2,2)=3
            obj%ElemNod(2,3)=yn+4
            obj%ElemNod(2,4)=yn+3
        endif

        
        n=0
        do j=1, yn
            do i=1, xn
                n=n+1
                obj%ElemNod(n,1)=i + (j-1)*(xn+1)
                obj%ElemNod(n,2)=i+1 + (j-1)*(xn+1)
                obj%ElemNod(n,3)=xn+2+i+ (j-1)*(xn+1)
                obj%ElemNod(n,4)=xn+1+i + (j-1)*(xn+1)
                obj%ElemMat(n)=1
            enddo
        enddo


    endif

    if(meshtype=="Root2D")then
        xn=input(default=1,option=x_num)
        yn=input(default=1,option=y_num)
        lx=input(default=1.0d0,option=x_len)
        ly=input(default=1.0d0,option=y_len)
        ! creating rectangular mesh
        allocate(obj%NodCoord( (xn+1)*(yn+1) , 2 ))
        allocate(obj%ElemNod( xn*yn,4) )
        allocate(obj%ElemMat(xn*yn) )
        n=0
        do j=1, yn+1
            do i=1, xn+1
                n=n+1
                obj%NodCoord(n,1)=lx/dble(xn)*dble(i-1)
                obj%NodCoord(n,2)=ly/dble(yn)*dble(j-1)
            enddo
        enddo
        n=1
        obj%ElemNod(1,1)=1
        obj%ElemNod(1,2)=2
        obj%ElemNod(1,3)=yn+3
        obj%ElemNod(1,4)=yn+2
        if(xn>=2)then
            obj%ElemNod(2,1)=2
            obj%ElemNod(2,2)=3
            obj%ElemNod(2,3)=yn+4
            obj%ElemNod(2,4)=yn+3
        endif

        
        n=0
        do j=1, yn
            do i=1, xn
                n=n+1
                obj%ElemNod(n,1)=i + (j-1)*(xn+1)
                obj%ElemNod(n,2)=i+1 + (j-1)*(xn+1)
                obj%ElemNod(n,3)=xn+2+i+ (j-1)*(xn+1)
                obj%ElemNod(n,4)=xn+1+i + (j-1)*(xn+1)
                obj%ElemMat(n)=1
            enddo
        enddo

        !Lt : Length of root cap
        !Le : Length of enlongating-zone
        !Lh : Length of tail
        !Dr : Diameter of root

        ! first, shift to the origin
        call obj%shift(x=-lx*0.50d0)

        if(.not. present(Lh) )then
            print *, "createMesh >> ERROR >> Lh should be given."
        endif
        if(.not. present(Le) )then
            print *, "createMesh >> ERROR >> Lh should be given."
        endif
        ! get parabolic constant
        radius=0.50d0*lx
        a_val=Lh/radius/radius
        do i=1,xn+1
            do j=1,yn+1
                x_=obj%NodCoord(i+(xn+1)*(j-1) ,1)
                obj%NodCoord(i+(xn+1)*(j-1) ,2)=obj%NodCoord(i+(xn+1)*(j-1) ,2)&
                    *(ly - a_val*x_*x_ )/ly
                obj%NodCoord(i+(xn+1)*(j-1) ,2)=-obj%NodCoord(i+(xn+1)*(j-1) ,2)
                obj%NodCoord(i+(xn+1)*(j-1) ,1)=-obj%NodCoord(i+(xn+1)*(j-1) ,1)
            enddo
        enddo

        ! Set material IDs
        ! rootcap=1, enlongating zone =2, and others are 3
        obj%ElemMat(:)=3
        do i=1,size(obj%ElemMat,1)
            x_=obj%NodCoord(obj%ElemNod(i,1),2)+obj%NodCoord(obj%ElemNod(i,3),2)&
                +obj%NodCoord(obj%ElemNod(i,2),2)+obj%NodCoord(obj%ElemNod(i,4),2)
            x_=x_*0.250d0
            if(x_ >= -(y_len-Le-Lh) )then
                obj%ElemMat(i)=3
            elseif( -(y_len-Le-Lh) > x_ .and. x_ > -(y_len-Lh))then
                obj%ElemMat(i)=2
            else
                obj%ElemMat(i)=1
            endif
        enddo

        call obj%GetSurface()
    endif


    if(meshtype=="RootAndSoil2D")then
        xn=input(default=1,option=x_num)
        yn=input(default=1,option=y_num)
        lx=input(default=1.0d0,option=x_len)
        ly=input(default=1.0d0,option=y_len)
        ! creating rectangular mesh
        allocate(obj%NodCoord( (xn+1)*(yn+1) , 2 ))
        allocate(obj%ElemNod( xn*yn,4) )
        allocate(obj%ElemMat(xn*yn) )
        n=0
        do j=1, yn+1
            do i=1, xn+1
                n=n+1
                obj%NodCoord(n,1)=lx/dble(xn)*dble(i-1)
                obj%NodCoord(n,2)=ly/dble(yn)*dble(j-1)
            enddo
        enddo
        n=1
        obj%ElemNod(1,1)=1
        obj%ElemNod(1,2)=2
        obj%ElemNod(1,3)=yn+3
        obj%ElemNod(1,4)=yn+2
        if(xn>=2)then
            obj%ElemNod(2,1)=2
            obj%ElemNod(2,2)=3
            obj%ElemNod(2,3)=yn+4
            obj%ElemNod(2,4)=yn+3
        endif

        
        n=0
        do j=1, yn
            do i=1, xn
                n=n+1
                obj%ElemNod(n,1)=i + (j-1)*(xn+1)
                obj%ElemNod(n,2)=i+1 + (j-1)*(xn+1)
                obj%ElemNod(n,3)=xn+2+i+ (j-1)*(xn+1)
                obj%ElemNod(n,4)=xn+1+i + (j-1)*(xn+1)
                obj%ElemMat(n)=1
            enddo
        enddo

        !Lt : Length of root cap
        !Le : Length of enlongating-zone
        !Lh : Length of tail
        !Dr : Diameter of root

        ! first, shift to the origin
        call obj%shift(x=-lx*0.50d0)

        if(.not. present(Lh) )then
            print *, "createMesh >> ERROR >> Lh should be given."
        endif
        if(.not. present(Le) )then
            print *, "createMesh >> ERROR >> Lh should be given."
        endif
        ! get parabolic constant
        radius=0.50d0*lx
        a_val=Lh/radius/radius
        do i=1,xn+1
            do j=1,yn+1
                x_=obj%NodCoord(i+(xn+1)*(j-1) ,1)
                obj%NodCoord(i+(xn+1)*(j-1) ,2)=obj%NodCoord(i+(xn+1)*(j-1) ,2)&
                    *(ly - a_val*x_*x_ )/ly
                obj%NodCoord(i+(xn+1)*(j-1) ,2)=-obj%NodCoord(i+(xn+1)*(j-1) ,2)
                obj%NodCoord(i+(xn+1)*(j-1) ,1)=-obj%NodCoord(i+(xn+1)*(j-1) ,1)
            enddo
        enddo

        ! Set material IDs
        ! rootcap=1, enlongating zone =2, and others are 3
        obj%ElemMat(:)=3
        do i=1,size(obj%ElemMat,1)
            x_=obj%NodCoord(obj%ElemNod(i,1),2)+obj%NodCoord(obj%ElemNod(i,3),2)&
                +obj%NodCoord(obj%ElemNod(i,2),2)+obj%NodCoord(obj%ElemNod(i,4),2)
            x_=x_*0.250d0
            if(x_ >= -(y_len-Le-Lh) )then
                obj%ElemMat(i)=3
            elseif( -(y_len-Le-Lh) > x_ .and. x_ > -(y_len-Lh))then
                obj%ElemMat(i)=2
            else
                obj%ElemMat(i)=1
            endif
        enddo
        call obj%GetSurface()
    endif




end subroutine createMesh

!##################################################
subroutine Convert2Dto3DMesh(obj,Thickness,division,smooth)
    class(Mesh_),intent(inout)::obj
    real(real64),allocatable::buffer(:,:)
    real(real64),optional,intent(in)::Thickness
    integer(int32),optional,intent(in)::division
    logical,optional,intent(in) :: smooth
    real(real64) :: Tn
    integer(int32) :: i,j,n,m,NumOfLayer,numnod


    ! only for linear elements

    if(present(Thickness))then
        if(Thickness==0.0d0)then
            print *, "ERROR :: Convert2Dto3D >> Thickness = 0"
            return
        else
            Tn=Thickness
        endif
    else
        Tn=1.0d0
    endif

    if(present(division))then
        if(division==0)then
            print *, "ERROR :: Convert2Dto3D >> division = 0"
            return
        endif
        NumOfLayer=division
    else
        NumOfLayer=1
    endif

    numnod=size(obj%NodCoord,1)
    n=size(obj%NodCoord,1)
    m=size(obj%NodCoord,2)

    allocate(buffer(n*(NumOfLayer+1),3))

    do j=1,NumOfLayer+1
        do i=1,n
            buffer( n*(j-1) + i ,1:2) = obj%NodCoord(i,1:2)
            buffer( n*(j-1) + i ,3)   = Tn / dble(NumOfLayer)*dble(j-1)
        enddo
    enddo

    deallocate(obj%NodCoord)
    allocate(obj%NodCoord( size(buffer,1) ,size(buffer,2) ) )
    obj%NodCoord(:,:)=buffer(:,:)
    deallocate(buffer)


    ! ElemNod

    if(.not.allocated(obj%ElemNod) )then
        print *, "Caution :: Convert2Dto3D >> ElemNod is not allocated = 0"
        return
    endif
    n=size(obj%ElemNod,1)
    m=size(obj%ElemNod,2)

    allocate(buffer(n*NumOfLayer,m*2))

    do j=1,NumOfLayer
        do i=1,n
            buffer( n*(j-1)+i, 1:m      ) = obj%ElemNod(i,1:m)+numnod*(j-1)
            buffer( n*(j-1)+i, m+1:2*m  ) = obj%ElemNod(i,1:m)+numnod*(j)
        enddo
    enddo

    deallocate(obj%ElemNod)
    allocate(obj%ElemNod( size(buffer,1) ,size(buffer,2) ) )
    obj%ElemNod(:,:)=buffer(:,:)
    deallocate(buffer)

    ! ElemMat

    if(.not.allocated(obj%ElemMat) )then
        print *, "Caution :: Convert2Dto3D >> ElemMat is not allocated = 0"
        return
    endif

    allocate(buffer(n*NumOfLayer,1))

    do j=1,NumOfLayer
        do i=1,n
            buffer( n*(j-1)+i, 1      ) = obj%ElemMat(i)
        enddo
    enddo

    deallocate(obj%ElemMat)
    allocate(obj%ElemMat( size(buffer,1) ) )
    obj%ElemMat(:)=buffer(:,1)
    deallocate(buffer)
    
    

end subroutine
!##################################################


!##################################################
subroutine remeshMesh(obj,meshtype,x_num,y_num,x_len,y_len,Le,Lh,Dr,thickness,&
    division,smooth,top,margin,inclineRate,shaperatio,master,slave,x,y,z,dx,dy,dz,coordinate)
    class(Mesh_),intent(inout) :: obj
    type(Mesh_) :: mesh1,mesh2,interface1,interface2
    type(Mesh_),optional,intent(inout) :: master,slave
    type(IO_) :: f
    type(ShapeFunction_) :: shape
    character(*),optional,intent(in) :: meshtype
    logical,optional,intent(in) :: smooth
    integer(int32),optional,intent(in) :: x_num,y_num ! number of division
    integer(int32),optional,intent(in) :: division ! for 3D rectangular
    real(real64),optional,intent(in) :: x_len,y_len,Le,Lh,Dr,coordinate(:,:) ! length
    real(real64),optional,intent(in) :: thickness,inclineRate ! for 3D rectangular
    real(real64),optional,intent(in) :: top,margin ! for 3D rectangular
    real(real64),optional,intent(in) :: shaperatio ! for 3D leaf
    real(real64),optional,intent(in) :: x,y,z,dx,dy,dz
    
    integer(int32) :: i,j,n,m,xn,yn,smoothedge(8),ini,k,dim_num,node_num,elem_num
    real(real64)::lx,ly,sx,sy,a_val,radius,x_,y_,diflen,Lt,&
        unitx,unity,xm, ym,tp,rx,ry,zc,zl,zm,ysize,ox,oy,dist,rr
    logical :: validmeshtype=.false.
    type(Mesh_) :: BoundBox
    real(real64)::ymin,ymax,ratio,width,pi,xx,yy,xvec(3),x_max(3),&
        x_min(3),x_m_mid(3),x_s_mid(3),x1vec(3),x2vec(3),nvec(3),hvec(3)
    integer(int32),allocatable:: OutNodeID(:),OutElementID(:)
    logical :: inside
    real(real64):: dist_tr, dist_cur,z_,zval1,zval2,x_1(3),x_2(3)
    integer(int32) :: num_layer,itr,node1,node2,node3,node4,count,prev_node1
    integer(int32), allocatable :: elemnod(:,:)
    integer(int32) :: nearest_node_id,nearest_facet_id,node_id,elist(2),tri_excep,tri_excep_last
    integer(int32),allocatable :: checked(:),checked_node(:)
    real(real64),allocatable ::nodcoord(:,:)
    real(real64) :: ll,center(3),vector(3),e1(3),e2(3),e3(3),len_val
    
    ! remesh
    ! only for build-in meshtypes
    if(trim(obj%meshtype)=="")then
        print *, "ERROR :: remeshMesh >> only for build-in meshtypes, &
            so the object should have created by createMesh"
        return
    endif
    
    call mesh1%create(meshtype=meshtype,x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len,Le=Le,Lh=Lh,Dr=Dr,thickness=thickness,&
    division=division,smooth=smooth,top=top,margin=margin,inclineRate=inclineRate,shaperatio=shaperatio,master=master,&
    slave=slave,x=x,y=y,z=z,dx=dx,dy=dy,dz=dz,coordinate=coordinate)

    obj%nodcoord = mesh1%nodcoord
    obj%elemnod = mesh1%elemnod
    obj%elemmat = mesh1%elemmat


end subroutine
!##################################################



! ##############################################

subroutine shiftMesh(obj,x,y,z)
    class(Mesh_),intent(inout)::obj
    real(real64),optional,intent(in) :: x,y,z

    if(present(x) )then
        obj%NodCoord(:,1)=obj%NodCoord(:,1)+x
    endif
    if(present(y) )then
        obj%NodCoord(:,2)=obj%NodCoord(:,2)+y
    endif
    if(present(z) )then
        obj%NodCoord(:,3)=obj%NodCoord(:,3)+z
    endif
end subroutine shiftMesh
! ##############################################
subroutine checkMesh(obj)
    class(Mesh_),intent(inout)::obj
    integer(int32) :: i,j,n,m,a,b,c,k,l
    integer(int32),allocatable :: Elem(:)
    real(real64) :: x1(3),x2(3),x3(3),dp,normalvec(3)
    if(.not. allocated(obj%NodCoord))then
        print *, "Check-mesh :: ERROR >> nodal coordiate is empty"
        stop
    endif
    if(.not. allocated(obj%ElemNod))then
        print *, "Check-mesh :: ERROR >> Element-connectivity is empty"
        stop
    endif


    n=size(obj%ElemNod,2)
    m=size(obj%NodCoord,2)
    allocate(Elem(n))
    if(n==4 .and. m==2)then
        do i=1,size(obj%ElemNod,1)
            !check node-order
            Elem(1:n)=obj%ElemNod(i,1:n)
            x1(:)=0.0d0
            x2(:)=0.0d0
            x1(1:2)=obj%NodCoord(Elem(2),1:2)-obj%NodCoord(Elem(1),1:2)
            x2(1:2)=obj%NodCoord(Elem(4),1:2)-obj%NodCoord(Elem(1),1:2)
            x3(:)=cross_product(x1,x2)
            normalvec(:)=0.0d0
            normalvec(3)=1.0d0
            dp=dot_product(x3,normalvec)
            if(dp <= 0)then
                !print *, dp
                !print *, normalvec
                !print *, x3(:)
                !print *, x2(:)
                !print *, x1(:)
                !print *, elem(:)
                !print *, obj%NodCoord(Elem(1),1:2), obj%NodCoord(Elem(2),1)-obj%NodCoord(Elem(1),1),&
                !     obj%NodCoord(Elem(2),2)-obj%NodCoord(Elem(1),2)
                !print *, obj%NodCoord(Elem(2),1:2), obj%NodCoord(Elem(3),1)-obj%NodCoord(Elem(2),1),&
                !     obj%NodCoord(Elem(3),2)-obj%NodCoord(Elem(2),2)
                !print *, obj%NodCoord(Elem(3),1:2), obj%NodCoord(Elem(4),1)-obj%NodCoord(Elem(3),1),&
                !     obj%NodCoord(Elem(4),2)-obj%NodCoord(Elem(3),2)
                !print *, obj%NodCoord(Elem(4),1:2), obj%NodCoord(Elem(1),1)-obj%NodCoord(Elem(4),1),&
                !     obj%NodCoord(Elem(1),2)-obj%NodCoord(Elem(4),2)
                print *, "Check-mesh :: ERROR >> Order of the connectivity is wrong!"
                ! modify connectivity
                do j=1,n
                    obj%ElemNod(i,j)=elem(n-j+1)
                enddo
                print *, "Check-mesh :: OK >> ERROR is modified!"

            else
                cycle
            endif
             
        enddo
        print *, "Mesh-connectivity is OK"
    else
        print *, "Element type :: ",m,"dimensional",n,"node iso-parametric element"
        print *, "Check-mesh :: Sorry not implemented for such types of meshes."
        stop 
    endif

    ! surface-node connectivity check only for 4-node isopara
    call obj%GetSurface()
    return
    
    if(.not.allocated(obj%SurfaceLine2D) )then
        a=obj%SurfaceLine2D(1)
        b=obj%SurfaceLine2D(2)
        do i=1,size(obj%ElemNod,1)
            do j=1,size(obj%ElemNod,2)
                if(obj%ElemNod(i,j)==a)then
                    do k=1,size(obj%ElemNod,2)
                        if(b==obj%ElemNod(i,k) )then
                            if(j+1==k .or. j-3==k)then
                                print *, "Check-mesh :: invalid surface-mesh order"
                                stop 
                            endif
                        endif
                    enddo
                endif
            enddo
        enddo

    else

    endif






end subroutine checkMesh
! ##############################################

! #########################################################################################
subroutine gmshMesh(obj,OptionalContorName,OptionalAbb,OptionalStep,Name,withNeumannBC,withDirichletBC&
	,onlyNeumannBC,onlyDirichletBC,asMsh,withMaterial,ElemValue,timestep)
	class(Mesh_),intent(inout)::obj
	real(real64),allocatable::gp_value(:,:)
	integer(int32),optional,intent(in)::OptionalStep,timestep
    character,optional,intent(in):: OptionalAbb*6
    character(*),optional,intent(in):: OptionalContorName
    character(*),optional,intent(in)::Name
	logical,optional,intent(in)::withNeumannBC,withDirichletBC,onlyNeumannBC,onlyDirichletBC,asMsh,withMaterial
	real(real64),allocatable::x_double(:,:)
	real(real64),allocatable::x(:,:)
    integer(int32) i,j,k,l,step,fh,nodeid1,nodeid2
    real(real64),optional,intent(in) :: ElemValue(:,:)
	character filename0*11
	character filename*200
	character filetitle*6
	character command*200
	character:: mapname*30,abbmap*6
	


	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif

	if(present(OptionalStep) )then
		step=OptionalStep
    elseif(present(timeStep) )then
        step=timestep
    else
		step=1
	endif
	fh=123

	filetitle(1:6)=abbmap(1:6)
    
    if(.not.allocated(obj%ElemMat) )then
        allocate(obj%ElemMat(size(obj%ElemNod,1) ) )
        obj%ElemMat(:)=1
    endif

	!---------------------
	write (filename0, '("_", i6.6, ".pos")') step ! ここでファイル名を生成している
	if(present(Name) )then
		filename=filetitle//filename0
		
		!call execute_command_line(  "touch "//trim(adjustl(name))//trim(obj%FileName)//trim(filename) )
		print *, trim(adjustl(name))//trim(filename)
		open(fh,file=trim(adjustl(name))//trim(filename) )
		print *, "writing ",trim(adjustl(name))//trim(filename)," step>>",step
	else
		filename=filetitle//filename0
		!call execute_command_line(  "touch "//trim(obj%FileName)//trim(filename) )
		print *, trim(obj%FileName)//trim(filename)
		open(fh,file=trim(obj%FileName)//trim(filename) )
		print *, "writing ",trim(obj%FileName)//trim(filename)," step>>",step
	endif
	
	
	!---------------------
	if( size(obj%ElemNod,2)==4 .and. size(obj%NodCoord,2)==2 ) then
		allocate(x(4,3) )
		allocate(x_double(4,3) )
		x(:,:)=0.0d0
		x_double(:,:)=0.0d0
	elseif( size(obj%ElemNod,2)==8 .and. size(obj%NodCoord,2)==3 ) then
		allocate(x(8,3) )
		allocate(x_double(8,3) )
		x(:,:)=0.0d0
		x_double(:,:)=0.0d0
		
	endif

    allocate(gp_value( size(obj%ElemNod,1),size(obj%ElemNod,2) ))
    if(.not.allocated(obj%ElemMat))then
        allocate(obj%ElemMat(size(obj%ElemNod,1)) )
        obj%ElemMat(:)=1
    endif
	do i=1,size(obj%ElemNod,1)
		gp_value(i,:)=input(default=dble(obj%ElemMat(i)),option=ElemValue(i,1))
	enddo

	x(:,:)=0.0d0
	write(fh,*) 'View "',mapname,'" {'
	do i=1,size(gp_value,1)
		if( size(obj%ElemNod,2)==4 .and. size(obj%NodCoord,2)==2 ) then
			
			! 2-D, 4 noded, isoparametric elements with four gauss points 
			x_double(1,1:2)=obj%NodCoord(obj%ElemNod(i,1),1:2  )
			x_double(2,1:2)=0.50d0*obj%NodCoord(obj%ElemNod(i,1),1:2  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,2),1:2  )
			x_double(3,1:2)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:2  )+0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:2  )&
					+0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:2  )+0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%NodCoord(obj%ElemNod(i,4),1:2  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,1),1:2  )

			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
				

			x_double(1,1:2)=obj%NodCoord(obj%ElemNod(i,2),1:2  )
			x_double(2,1:2)=0.50d0*obj%NodCoord(obj%ElemNod(i,2),1:2  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,3),1:2  )
			x_double(3,1:2)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:2  )+0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:2  )&
					+0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:2  )+0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%NodCoord(obj%ElemNod(i,1),1:2  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,2),1:2  )

			
			x(:,:)=x_double(:,:) 

			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
				
			x_double(1,1:2)=obj%NodCoord(obj%ElemNod(i,3),1:2  )
			x_double(2,1:2)=0.50d0*obj%NodCoord(obj%ElemNod(i,3),1:2  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,4),1:2  )
			x_double(3,1:2)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:2  )+0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:2  )&
					+0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:2  )+0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%NodCoord(obj%ElemNod(i,2),1:2  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,3),1:2  )
			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				
			x_double(1,1:2)=obj%NodCoord(obj%ElemNod(i,4),1:2  )
			x_double(2,1:2)=0.50d0*obj%NodCoord(obj%ElemNod(i,4),1:2  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,1),1:2  )
			x_double(3,1:2)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:2  )+0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:2  )&
					+0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:2  )+0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%NodCoord(obj%ElemNod(i,3),1:2  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,4),1:2  )
			
			x(:,:)=x_double(:,:) 
			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			
		elseif(size(obj%ElemNod,2)==8 .and. size(obj%NodCoord,2)==3  ) then
			
			! 3-D, 8 noded, isoparametric elements with 8 gauss points
			! 1/8

			x_double(1,1:3)=obj%NodCoord(obj%ElemNod(i,1),1:3  )
			x_double(2,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,1), 1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )
			x_double(3,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.250d0*obj%NodCoord(obj%ElemNod(i,2), 1:3  )&
					+0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.250d0*obj%NodCoord(obj%ElemNod(i,4), 1:3  )
			x_double(4,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,4), 1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )

			x_double(5,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )

			x_double(6,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
					+0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )
			
			x_double(7,1:3)=0.1250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
					+0.1250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
					+0.1250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
					+0.1250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x_double(8,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
					+0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )
			
			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			
			! 2/8

			x_double(1,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,1), 1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )
			
			x_double(2,1:3)=obj%NodCoord(obj%ElemNod(i,2),1:3  )
			
			x_double(3,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,2), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )

			x_double(5,1:3)= 0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )

			x_double(6,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,2), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )

			
			x_double(7,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.1250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			
			
			! 3/8

			x_double(8,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%NodCoord(obj%ElemNod(i,3),1:3  )
			
			x_double(2,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,2), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )

			x_double(6,1:3)= 0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )

			x_double(7,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,3), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			
			x_double(4,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,4),1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				

			! 4/8

			x_double(6,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%NodCoord(obj%ElemNod(i,4),1:3  )
			
			x_double(7,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,4), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )

			x_double(8,1:3)= 0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )

			x_double(4,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,4), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )

			
			x_double(2,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,4),1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			



			! 5/8

			x_double(7,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(5,1:3)=obj%NodCoord(obj%ElemNod(i,5),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,5), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )

			x_double(4,1:3)= 0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )

			x_double(1,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,5), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.1250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			
			! 6/8

			x_double(8,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(6,1:3)=obj%NodCoord(obj%ElemNod(i,6),1:3  )
			
			x_double(5,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,5), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )

			x_double(3,1:3)= 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )

			x_double(2,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,6), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )

			
			x_double(7,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,6),1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(4,1:3)=0.1250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			

			
			! 7/8

			x_double(5,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%NodCoord(obj%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,7), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x_double(2,1:3)= 0.250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(3,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,3), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			
			x_double(6,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,6),1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(1,1:3)=0.1250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			

			

			
			! 8/8

			x_double(5,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%NodCoord(obj%ElemNod(i,8),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,7), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x_double(4,1:3)= 0.250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,4), 1:3  )+0.50d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%NodCoord(obj%ElemNod(i,5),1:3  ) + 0.50d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )

			x_double(1,1:3)=0.1250d0*obj%NodCoord(obj%ElemNod(i,1),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,3),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,5),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%NodCoord(obj%ElemNod(i,7),1:3  )+0.1250d0*obj%NodCoord(obj%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			




        else
            print *, " size(obj%ElemNod,2)==",size(obj%ElemNod,2)
            print *, ".and. size(obj%NodCoord,2)==",size(obj%NodCoord,2)
			stop "plot_contour >> now constructing"
		endif
	enddo
	write(fh,*) '};'
	close(fh)
 end subroutine
 !===========================================================================================

subroutine showRangeMesh(obj)
    class(Mesh_),intent(in) :: obj
    real(real64) :: x_max, x_min,y_max, y_min, z_max, z_min

    x_max = maxval(obj%NodCoord(:,1) )
    x_min = minval(obj%NodCoord(:,1) )
    y_max = maxval(obj%NodCoord(:,2) )
    y_min = minval(obj%NodCoord(:,2) )
    z_max = maxval(obj%NodCoord(:,3) )
    z_min = minval(obj%NodCoord(:,3) )
    print *, " x_max=", x_max, " x_min=", x_min,&
     " y_max=",y_max, " y_min=", y_min, &
     " z_max=", z_max, " z_min=", z_min

end subroutine
!===========================================================================================

function emptyMesh(obj) result(res)
    class(Mesh_),intent(in) :: obj
    logical :: res
    integer(int32) :: cn
    cn=0
    if(allocated(obj%NodCoord) )then
        cn=cn+1
    endif
    if(allocated(obj%ElemNod) )then
        cn=cn+1
    endif
    if(cn==0)then
        res=.true.
    else
        res=.false.
    endif
end function
! ################################################################################




! ################################################################################
function divideMesh(obj,n) result(meshes)
    class(Mesh_),intent(inout) :: obj
    class(Mesh_),allocatable :: meshes(:)
    integer(int32),intent(in) :: n
    integer(int32) :: i,j,k,l,m,mesh_num,loc_elem_num,elem_num,elem_type,dim_num
    integer(int32) :: cur_node_id,cur_elem_id,local_id,global_id,num_loc_node
    logical,allocatable :: selected(:)
    integer(int32),allocatable :: global_vs_local(:,:)
    integer(int32),allocatable :: buffer(:,:)
    logical :: tf

    if(n<2)then
        allocate(meshes(1))
        call meshes(1)%copy(obj)
        return
    endif
    ! divide mesh by the Greedy algorithm.

    mesh_num = input(default=2, option=n)
    
    allocate(meshes(mesh_num) )
    
    
    elem_num  = size(obj%ElemNod,1)
    elem_type = size(obj%ElemNod,2)

    allocate(selected(elem_num) )
    selected(:) = .false.
    loc_elem_num=int(elem_num/mesh_num)
    ! count number of mesh
    k=0
    do i=1,mesh_num
        if(i<=mod(elem_num,mesh_num) )then
            allocate( meshes(i)%ElemNod(loc_elem_num+1, elem_type) )
            allocate( meshes(i)%ElemMat(loc_elem_num+1           ) )
            meshes(i)%ElemMat(:)=1
        else
            allocate(meshes(i)%ElemNod(loc_elem_num,elem_type) )
            allocate( meshes(i)%ElemMat(loc_elem_num         ) )
            meshes(i)%ElemMat(:)=1
        endif
    enddo

    do i=1, size(meshes)
        print *, size(meshes(i)%ElemNod,1)
    enddo

    do i=1,size(meshes)
        do j=1,size(selected)
            if(selected(j) .eqv. .false. )then
                cur_elem_id=j
                exit
            endif
        enddo

        k=1
        meshes(i)%ElemNod(k,:) = obj%ElemNod(cur_elem_id,:)
        selected(cur_elem_id)=.true.
        ! search neighbor element
        do l=cur_elem_id+1, elem_num
            if(k==size(meshes(i)%ElemNod,1) )then
                exit
            endif

            if(selected(l) .eqv. .true. )then
                cycle
            endif
            
            m=countifsame(meshes(i)%ElemNod(k,:),obj%ElemNod(l,:)  )
            
            if( m<=0)then
                ! no contact
                cycle
            else
                ! contact
                k=k+1
                meshes(i)%ElemNod(k,:)=obj%ElemNod(l,:) 
                selected(l)=.true.
            endif
        enddo
    enddo


    
    
    local_id=0
    do i=1,size(meshes,1)
        allocate(global_vs_local(1,2) )
        do j=1,size(meshes(i)%ElemNod,1)
            do k=1,size(meshes(i)%ElemNod,2)
                global_id=meshes(i)%ElemNod(j,k)
                if(m==0)then
                    local_id=local_id+1
                    global_vs_local(1,1)=global_id ! global node id
                    global_vs_local(1,2)=local_id ! local node id
                else
                    tf=exist(vector=global_vs_local,val=global_id,columnid=1 )
                    if(tf .eqv. .true. )then
                        cycle
                    else
                        call extend(mat=global_vs_local)
                        local_id=local_id+1
                        global_vs_local(1,1)=global_id ! global node id
                        global_vs_local(1,2)=local_id ! local node id
                    endif
                endif
            enddo
        enddo

        ! change node-ids and allocate nodal-coordinat
        num_loc_node=size(global_vs_local)
        dim_num=size(obj%NodCoord,2)
        allocate(buffer(size(meshes(i)%ElemNod,1),size(meshes(i)%ElemNod,2)  ) )

        allocate(meshes(i)%NodCoord(num_loc_node,dim_num) )
        do j=1,size(global_vs_local,1)
            ! update node id
            meshes(i)%NodCoord(global_vs_local(j,2),:) = obj%NodCoord(global_vs_local(j,1),: )
            ! update elem_id
            do k=1,size(meshes(i)%ElemNod,1)
                do l=1,size(meshes(i)%ElemNod,2)
                    if( meshes(i)%ElemNod(k,l) == global_vs_local(j,1) )then
                        buffer(k,l) = global_vs_local(j,2)
                    endif
                enddo
            enddo
        enddo
        meshes(i)%ElemNod(:,:)=buffer(:,:)

        deallocate(global_vs_local)
        deallocate(buffer)
    enddo



end function
! ################################################################################


!#######################################################################################
function HowManyDomainMesh(obj) result(ret)
    class(Mesh_),intent(in) :: obj
    integer(int32) :: ret, i,j,itr,k,n
    integer(int32),allocatable :: domain_id(:)

!    if(obj%empty() .eqv. .true.)then
!        print *, "HowManyDomainMesh :: obj%empty() .eqv. .true."
!        return
!    endif
!
!    n=size(obj%NodCoord,1)
!    allocate(domain_id(n) )
!    domain_id(:)=-1
!    k=1
!    domain_id(1)=1
!    do 
!
!        if(minval(domain_id)/=-1 )then
!            exit
!        endif
!    enddo

end function
!#######################################################################################

!#######################################################################################
function getNodeListMesh(obj,BoundingBox,xmin,xmax,ymin,ymax,zmin,zmax) result(NodeList)
    class(Mesh_),intent(inout) :: obj
    type(Mesh_),optional,intent(inout) :: BoundingBox
    real(real64),optional,intent(in) :: xmin,xmax,ymin,ymax,zmin,zmax
    integer(int32),allocatable :: NodeList(:)
    integer(int32) :: i,j,n,num_of_node,m
    logical ,allocatable:: tf(:)
    real(real64),allocatable :: x(:),x_min(:),x_max(:)
    
    

    n=size(obj%NodCoord,1)
    m=size(obj%NodCoord,2)
    allocate( x(m),x_min(m),x_max(m),tf(n) )

    if(present(BoundingBox) )then
        num_of_node=0
        do i=1,n
            x(:)=obj%NodCoord(i,:)
            do j=1,m
                x_min(j)=minval(BoundingBox%NodCoord(:,j))
                x_max(j)=maxval(BoundingBox%NodCoord(:,j))
            enddo
            tf(i)=.false.
            tf(i) = InOrOut(x=x,xmax=x_max,xmin=x_min,DimNum=m)
        enddo
    else
        if(m==3)then
            x_min(1)=input(default=-dble(1.0e+18),option=xmin)
            x_min(2)=input(default=-dble(1.0e+18),option=ymin)
            x_min(3)=input(default=-dble(1.0e+18),option=zmin)

            x_max(1)=input(default= dble(1.0e+18),option=xmax)
            x_max(2)=input(default= dble(1.0e+18),option=ymax)
            x_max(3)=input(default= dble(1.0e+18),option=zmax)
        else
            print *, "Stop >> getNodeListMesh is supproted for 3D"
            stop
        endif
        num_of_node=0
        do i=1,n
            x(:)=obj%NodCoord(i,:)
            tf(i)=.false.
            tf(i) = InOrOut(x=x,xmax=x_max,xmin=x_min,DimNum=m)
        enddo
    endif

    n=countif(Vector=tf,tf=.true.)

    allocate(NodeList(n) )

    j=0
    do i=1,size(tf)
        if(tf(i) .eqv. .true. )then
            j=j+1
            NodeList(j) = i
        endif
    enddo

end function
!#######################################################################################


!#######################################################################################
function getFacetListMesh(obj,NodeID) result(FacetList)
    class(Mesh_),intent(inout) :: obj
    integer(int32),intent(in) :: NodeID
    integer(int32),allocatable :: FacetList(:,:) ! Node-ID =  FacetList(FacetID, LocalNodeID ) 
    integer(int32) :: i,j,k,l,count_id
    integer(int32) :: node_per_Facet = 4
    integer(int32),allocatable :: ElementList(:),NodeList(:,:),CountNodeList(:,:)

    ! Facetとってからcheckのほうが簡単


    
    ! search facets, in which a node is in
    ElementList = obj%getElementList(NodeID=NodeID)
    allocate(FacetList(size(ElementList),node_per_Facet ) )
    FacetList(:,:) = 0
    allocate(Nodelist(size(ElementList),size(obj%ElemNod,2) ) )
    allocate(CountNodelist(size(ElementList),size(obj%ElemNod,2) ) )
    CountNodelist(:,:) = 1
    ! get all nodes
    do i=1,size(ElementList)
        NodeList(i,:) = obj%ElemNod(ElementList(i),: )
    enddo
    do i=1,size(NodeList,1)
        do j=1,size(NodeList,2)
            do k=i+1,size(NodeList,1)
                do l=1,size(NodeList,2)
                    if(NodeList(i,j)==0) then
                        cycle
                    endif
                    if(NodeList(k,l)==0) then
                        cycle
                    endif
                    if(NodeList(i,j) == NodeList(k,l) )then
                        NodeList(k,l) = 0
                        CountNodeList(i,j) = CountNodeList(i,j) + 1
                    endif
                enddo
            enddo
        enddo   
    enddo
    do i=1,size(NodeList,1)
        do j=1,size(NodeList,2)
            if(CountNodeList(i,j)==0 .or. CountNodeList(i,j)==1)then
                NodeList(i,j) = 0
            endif
        enddo
    enddo
    call print(CountNodeList)
    call print("****")
    call print(NodeList)




end function
!#######################################################################################


!#######################################################################################
function getElementListMesh(obj,BoundingBox,xmin,xmax,ymin,ymax,zmin,zmax,NodeID) result(ElementList)
    class(Mesh_),intent(inout) :: obj
    type(Mesh_),optional,intent(inout) :: BoundingBox
    real(real64),optional,intent(in) :: xmin,xmax,ymin,ymax,zmin,zmax
    integer(int32),optional,intent(in) :: NodeID
    integer(int32),allocatable :: NodeList(:)
    integer(int32),allocatable :: ElementList(:)

    integer(int32) :: i,j,n,num_of_node,m,counter,k
    logical ,allocatable:: tf(:),exist
    real(real64),allocatable :: x(:),x_min(:),x_max(:)
    
    if(present(NodeID) )then
        if(obj%empty() .eqv. .true. )then
            call print("getElementListMesh >> obj%empty() .eqv. .true. ")
            allocate(ElementList(0))
            return
        endif
        n = 0
        do i=1,size(obj%elemnod,1)
            do j=1,size(obj%elemnod,2)
                if(obj%elemnod(i,j)==NodeID)then
                    n = n + 1
                    exit 
                endif
            enddo
        enddo
        allocate(ElementList(n) )
        n = 0
        do i=1,size(obj%elemnod,1)
            do j=1,size(obj%elemnod,2)
                if(obj%elemnod(i,j)==NodeID)then
                    n = n + 1
                    ElementList(n) = i
                    exit 
                endif
            enddo
        enddo
        return
    endif
    
    NodeList =  obj%getNodeList(BoundingBox,xmin,xmax,ymin,ymax,zmin,zmax)

    counter=0
    do i=1,size(obj%ElemNod,1)
        exist=.false.
        do j=1,size(obj%ElemNod,2)
            do k=1,size(NodeList,1)
                if( obj%ElemNod(i,j) == Nodelist(k) )then
                    exist=.true.
                    exit
                endif
            enddo    
            if(exist .eqv. .true.)then
                exit
            endif
        enddo
        if(exist .eqv. .true. )then
            counter=counter+1
        else
            cycle
        endif
    enddo
    allocate(ElementList(counter) )
    
    counter=0
    do i=1,size(obj%ElemNod,1)
        exist=.false.
        do j=1,size(obj%ElemNod,2)
            do k=1,size(NodeList,1)
                if( obj%ElemNod(i,j) == Nodelist(k) )then
                    exist=.true.
                    exit
                endif
            enddo    
            if(exist .eqv. .true.)then
                exit
            endif
        enddo
        if(exist .eqv. .true. )then
            counter=counter+1
            ElementList(counter) = i
        else
            cycle
        endif
    enddo
    

end function
!#######################################################################################


!#######################################################################################
function getVolumeMesh(obj) result(volume)
    class(Mesh_),intent(inout) :: obj
    real(real64),allocatable :: volume(:),eNodCoord(:,:)
    integer(int32) :: i,j,numelem, numelemnod,numnode,dimnum

    if(obj%empty() .eqv. .true. )then
        print *, "getVolumeMesh >> Mesh is empty."
        return
    endif

    numelem  = size(obj%ElemNod,1)
    numelemnod = size(obj%ElemNod,2)
    numnode = size(obj%NodCoord,1)
    dimnum  = size(obj%NodCoord,2)

    allocate( volume(numelem) )
    allocate(eNodCoord(numelemnod, dimnum) )

    if(numelemnod == 8)then
        do i=1,numelem
            do j=1, numelemnod
                eNodCoord(j,:)=obj%NodCoord( obj%ElemNod(i,j) ,:)
            enddo
        enddo
    else
        print *, "getVolumeMesh >> Not imlemented."
        stop
    endif



end function
!#######################################################################################



!#######################################################################################
function numElementsMesh(obj) result(ret)
    class(Mesh_),intent(in) :: obj
    integer(int32) :: ret

    if(obj%empty() .eqv. .true. )then
        ret = 0
        return
    endif
    ret = size(obj%ElemNod,1)
end function
!#######################################################################################


!#######################################################################################
function numNodesMesh(obj) result(ret)
    class(Mesh_),intent(in) :: obj
    integer(int32) :: ret

    if(obj%empty() .eqv. .true. )then
        ret = 0
        return
    endif
    ret = size(obj%NodCoord,1)
end function
!#######################################################################################


!#######################################################################################
function numNodesForEachElementMesh(obj) result(ret)
    class(Mesh_),intent(in) :: obj
    integer(int32) :: ret

    if(obj%empty() .eqv. .true. )then
        ret = 0
        return
    endif
    ret = size(obj%ElemNod,2)
end function
!#######################################################################################


!#######################################################################################
function numDimensionMesh(obj) result(ret)
    class(Mesh_),intent(in) :: obj
    integer(int32) :: ret

    if(obj%empty() .eqv. .true. )then
        ret = 0
        return
    endif
    ret = size(obj%NodCoord,2)
end function
!#######################################################################################
!#######################################################################################

subroutine jsonMesh(obj,name,fh,endl)
	class(Mesh_),intent(in) :: obj
	type(IO_) :: f
	integer(int32),optional,intent(in) :: fh
	character(*),optional,intent(in) :: name
    integer(int32) :: fileid,i,j
    logical,optional,intent(in) :: endl
	
	! export JSON file
	if(present(name) )then
		call f%open(name)
		fileid=f%fh
	else
		fileid=fh
	endif


    
    if(present(name) )then
        call f%write('{')
		write(fileid,*) '"name": "'//trim(name)//'",'
	endif
    write(fileid,*) '"mesh":{'
    
    if(allocated(obj%nodcoord) )then
        call json(array=obj%nodcoord,fh=fileid,name="NodCoord")
    endif
    if(allocated(obj%NodCoordInit) )then
        call json(array=obj%NodCoordInit,fh=fileid,name="NodCoordInit")
    endif
    if(allocated(obj%ElemNod) )then
        call json(array=obj%ElemNod,fh=fileid,name="ElemNod")
    endif
    if(allocated(obj%FacetElemNod) )then
        call json(array=obj%FacetElemNod,fh=fileid,name="FacetElemNod")
    endif
    if(allocated(obj%NextFacets) )then
        call json(array=obj%NextFacets,fh=fileid,name="NextFacets")
    endif
    if(allocated(obj%SurfaceLine2D) )then
        call json(array=obj%SurfaceLine2D,fh=fileid,name="SurfaceLine2D")
    endif
    if(allocated(obj%ElemMat) )then
        call json(array=obj%ElemMat,fh=fileid,name="ElemMat")
    endif
    if(allocated(obj%SubMeshNodFromTo) )then
        call json(array=obj%SubMeshNodFromTo,fh=fileid,name="SubMeshNodFromTo")
    endif
    if(allocated(obj%SubMeshElemFromTo) )then
        call json(array=obj%SubMeshElemFromTo,fh=fileid,name="SubMeshElemFromTo")
    endif
    if(allocated(obj%SubMeshSurfFromTo) )then
        call json(array=obj%SubMeshSurfFromTo,fh=fileid,name="SubMeshSurfFromTo")
    endif
    if(allocated(obj%GlobalNodID) )then
        call json(array=obj%GlobalNodID,fh=fileid,name="GlobalNodID")
    endif
    write(fileid,*) '"return_mesh":0'
    
!    integer(int32),allocatable::BottomElemID
!    integer(int32),allocatable::TopElemID
!    integer(int32) :: surface=1
!
!
!    character*200::FileName=" "
!    character*70::ElemType=" "
!    character*70:: ErrorMsg=" "
    


	

    if(present(endl) )then
        if(endl .eqv. .false.)then
            write(fileid,*) '},'
        else
            write(fileid,*) '}'
        endif
    else
        write(fileid,*) '}'
    endif

    if(present(name) )then
        
		call f%close()
	endif




end subroutine
!#######################################################################################
!#######################################################################################
subroutine cleanMesh(obj)
    class(Mesh_),intent(inout) :: obj
    integer(int32) :: i,j,n,num_dim
    integer(int32),allocatable :: removes(:)
    real(real64),allocatable :: nodcoord(:,:)

    allocate(removes( size(obj%nodcoord,1) ) )
    removes(:) = 1
    num_dim  =size(obj%nodcoord,2)
    do i=1,size(obj%ElemNod,1)
        do j=1, size(obj%ElemNod,2)
            removes(obj%ElemNod(i,j) ) = 0
        enddo
    enddo
    n = size(obj%nodcoord,1) - sum(removes)
    allocate(nodcoord(n,num_dim) )
    n=0
    do i=1,size(removes)
        if(removes(i)==0 )then
            n=n+1
            nodcoord(n,:) = obj%nodcoord(i,:)
        else
            cycle
        endif
    enddo
    obj%nodcoord = nodcoord
    do i=1,size(obj%elemnod,1)
        do j=1,size(obj%elemnod,2)
            
            obj%elemnod(i,j)=obj%elemnod(i,j)-sum(removes(1:obj%elemnod(i,j)) )
        enddo
    enddo

end subroutine
!#######################################################################################
!################################################################################
function nearestElementIDMesh(obj,x,y,z) result(ret)
    class(Mesh_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    real(real64),allocatable :: xcoord(:),nodcoord(:,:),xmin(:),xmax(:)
    integer(int32),allocatable :: element_id_list(:)
    logical,allocatable :: Inside(:)
    integer(int32) :: ret,dim_num,elem_num,node_num,i,j,nearest_node_id
    real(real64) :: r_val
    dim_num = size(obj%nodcoord,2)
    node_num = size(obj%nodcoord,1)
    elem_num = size(obj%elemnod,2)
    ret = -1 ! default
    
    allocate(xcoord(dim_num) )
    ! copy array
    if(dim_num==1)then
        xcoord(1) = x
    elseif(dim_num==2)then
        xcoord(1) = x
        xcoord(2) = y
    elseif(dim_num==3)then
        xcoord(1) = x
        xcoord(2) = y
        xcoord(3) = z
    endif
!    nodcoord = obj%nodcoord
!    do i=1,size(nodcoord,1)
!        nodcoord(i,:) = nodcoord(i,:) - xcoord(:)
!    enddo
    ! use heap sort

    ! if position is out of domain,
    ! return
    allocate(xmin(dim_num),xmax(dim_num) )

    do i=1,dim_num
        xmin(i) = minval(obj%nodcoord( :,i) )
        xmax(i) = maxval(obj%nodcoord( :,i) )
    enddo

    if(.not.InOrOut(xcoord,xmax,xmin,dim_num) )then  
        ret = -1
        !print *, "Caution! :: getNearestElementID :: out of domain"
        return
    endif


    nearest_node_id = obj%getNearestNodeID(x=x,y=y,z=z)
    element_id_list = obj%getElementList(NodeID=nearest_node_id)
    do j=1, size(element_id_list)
        if(obj%InsideOfElement(ElementID=element_id_list(j),x=x,y=y,z=z ) )then
            ret = element_id_list(j)
            return
        else
            cycle
        endif
    enddo

end function
!##################################################################################

!##################################################################################
function InsideOfElementMesh(obj,ElementID,x,y,z) result(Inside)
    class(Mesh_),intent(in) :: obj
    integer(int32),intent(in) :: ElementID
    real(real64),intent(in) :: x,y,z
    real(real64) :: a,b
    real(real64),allocatable :: ElemCoord(:,:),p1(:),p2(:),o1(:),o2(:),nvec(:)
    logical :: Inside
    integer(int32) :: i,j,cross_count,in_count,node_1,node_2,node_0,node_id,dim_num,nne

    inside = .false.

    ! detect Inside or not.
    dim_num = size(obj%nodcoord,2)
    nne =  size(obj%elemnod,2)
    allocate(ElemCoord( nne, dim_num ) )
    ElemCoord(:,:) = 0.0d0
    
    if(size(obj%elemnod,1) < ElementID )then
        print *, "ERROR :: InsideOfElementMesh >> size(obj%elemnod,1) < ElementID"
        Inside = .false.
        return
    endif

    do i=1,nne
        node_id = obj%elemnod(ElementID, i)
        elemcoord(i,:) = obj%nodcoord(node_id,:)
    enddo

    

    ! Question >>> 
    ! x,y,z is in elemcoord?
    if(size(obj%elemnod,2)==4 .and. size(obj%nodcoord,2)==2 )then
        ! Line-Crossing algorithm
        ! x ------> this side
        cross_count = 0
        allocate(p1(2), p2(2), o1(2), o2(2) )
        do i=1,4
            if(i==4)then    
                p1(:) = ElemCoord( 4 ,:)
                p2(:) = ElemCoord( 1 ,:)
            else
                p1(:) = ElemCoord( i ,:)
                p2(:) = ElemCoord( i+1 ,:)
            endif
            o1(1) = x
            o1(2) = y 
            ! p1, p2を通る直線の方程式
            a = (p2(2)-p1(2) )/( p2(1) - p1(1) )
            b = p2(2) - a * p2(1)
            
            ! y = o1(2) とy=ax+bとの交点のx座標
            ! x = (y-b)/a
            if(a==0)then
                if(b==y )then
                    if( abs(p1(1)-x)+abs(p2(1)-x) ==abs(p1(1)-p2(1) )  )then
                        ! on the line!
                        Inside = .true.
                        return
                    else
                        cycle
                    endif
                else
                    cycle
                endif
            else
                if( (y-b)/a >= x )then
                    cross_count=cross_count+1
                endif
            endif
        enddo    
        if(cross_count == 1)then
            ! inside
            Inside=.true.
        endif
        
    elseif (size(obj%elemnod,2)==8 .and. size(obj%nodcoord,2)==3 )then
        ! 内外判定
        ! Z = zで断面を切り、(x,y)のリストを作り、交差判定
        ! 3次元直線のZ=zにおける(x,y)を出す。>>ダメ

        ! 内積で、角度？
        in_count = 0
        Inside = .false.
        allocate(p1(3) )
        allocate(p2(3) )
        allocate(o1(3) )
        allocate(o2(3) )
        allocate(nvec(3) )

        !trial #1
        node_0 = 1
        node_1 = 4
        node_2 = 2
        
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        
        o1(1) = x
        o1(2) = y
        o1(3) = z

        !call print(elemcoord)

        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        !trial #2
        node_0 = 1
        node_1 = 2
        node_2 = 5
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        o1(1) = x
        o1(2) = y
        o1(3) = z
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        !trial #3
        node_0 = 1
        node_1 = 5
        node_2 = 4
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        o1(1) = x
        o1(2) = y
        o1(3) = z
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        !trial #4
        node_0 = 3
        node_1 = 7
        node_2 = 2
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        o1(1) = x
        o1(2) = y
        o1(3) = z
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        !trial #5
        node_0 = 7
        node_1 = 8
        node_2 = 6
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        o1(1) = x
        o1(2) = y
        o1(3) = z
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        !trial #6
        node_0 = 3
        node_1 = 4
        node_2 = 7
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        o1(1) = x
        o1(2) = y
        o1(3) = z
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        Inside = .true.
        return
    elseif(size(obj%elemnod,2)==4 .and. size(obj%nodcoord,2)==3 )then
        ! tetra element


        ! trial #1
        in_count = 0
        Inside = .false.
        allocate(p1(3) )
        allocate(p2(3) )
        allocate(o1(3) )
        allocate(o2(3) )
        allocate(nvec(3) )

        !trial #1
        node_0 = 3
        node_1 = 2
        node_2 = 1
        
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        
        o1(1) = x
        o1(2) = y
        o1(3) = z

        !call print(elemcoord)
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif


        !trial #2
        node_0 = 1
        node_1 = 2
        node_2 = 4
        
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        
        o1(1) = x
        o1(2) = y
        o1(3) = z

        !call print(elemcoord)
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        !trial #3
        node_0 = 1
        node_1 = 4
        node_2 = 3
        
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        
        o1(1) = x
        o1(2) = y
        o1(3) = z

        !call print(elemcoord)
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        !trial #4
        node_0 = 2
        node_1 = 3
        node_2 = 4
        
        p1(:) = elemcoord(node_1,:) - elemcoord(node_0,:)
        p2(:) = elemcoord(node_2,:) - elemcoord(node_0,:)
        
        o1(1) = x
        o1(2) = y
        o1(3) = z

        !call print(elemcoord)
        o1(:) = o1(:) - elemcoord(node_0,:)
        nvec = cross_product(p1,p2)
        if(dot_product(nvec,o1) > 0.0d0 )then
            ! outside
            Inside = .false.
            return
        endif

        Inside = .true.
        return

    else
        print *, "ERROR :: InsideOfElementMesh >> 4-node box or 8-node cube are acceptable."
        stop
    endif
    
end function
!##################################################################################


!##################################################################################
function getCenterCoordinateMesh(obj, elemid) result(ret)
    class(Mesh_),intent(in) :: obj
    integer(int32),intent(in) :: elemid
    integer(int32) :: dimnum,i
    real(real64),allocatable :: ret(:)

    if(obj%empty() .eqv. .true. )then
        print *, "ERROR :: mesh is empty"
        return
    endif
    dimnum = size(obj%nodcoord,2)

    allocate(ret(dimnum) )

    ret(:) = 0.0d0
    do i=1,size(obj%elemnod,2)
        ret(:)  = ret(:) + 1.0d0/dble(size(obj%elemnod,2))*obj%nodcoord(obj%elemnod(elemid,i),: )
    enddo

end function
!##################################################################################

function getNeighboringNodeMesh(obj,nodeid) result(ret)
    class(Mesh_),intent(inout) :: obj
    integer(int32),intent(in) :: nodeid
    integer(int32) :: dimnum,i,facetnum,elemnodnum,j,numnn
    integer(int32),allocatable :: ret(:),nodelist(:),elemnodtr(:)
    logical :: exists

    nodelist = int( zeros(size(obj%nodcoord,1) )  )
    elemnodtr= int( zeros(size(obj%elemnod ,2) )  )
    do i=1,size(obj%elemnod,1)
        elemnodtr = obj%elemnod(i,:)
        elemnodtr(:) = elemnodtr(:) - nodeid 
        elemnodtr(:) = abs(elemnodtr(:))
        if(minval(elemnodtr) == 0 )then
            do j=1,size(obj%elemnod,2)
                nodelist( obj%elemnod(i,j) ) = 1
            enddo
        endif
    enddo

    nodelist(nodeid) = 0

    ret = int(zeros(sum(nodelist) )  )
    j = 0
    do i=1,size(nodelist)
        if(nodelist(i)==1 )then
            j=j+1
            ret( j ) = i
        endif
    enddo


end function

!##################################################################################
function getNeighboringElementMesh(obj, elemid,withSurfaceID,interfaces) result(ret)
    class(Mesh_),intent(inout) :: obj
    integer(int32),intent(in) :: elemid
    integer(int32),allocatable,optional,intent(inout) :: interfaces(:)
    logical,optional,intent(in) :: withSurfaceID
    integer(int32) :: dimnum,i,facetnum,elemnodnum,j,n,k
    integer(int32),allocatable :: ret(:),nodelist(:),elemlist(:),id(:),idr(:),order(:,:)
    integer(int32),allocatable :: retbuf(:)
    logical :: exists

    if(obj%empty() .eqv. .true. )then
        print *, "ERROR :: mesh is empty"
        return
    endif

    ! get element info
    dimnum = size(obj%nodcoord,2)
    elemnodnum = size(obj%elemnod,2)


    if(dimnum==3 .and. elemnodnum==4)then
        ! Tetra mesh
        if(present(withSurfaceID) )then
            if(withSurfaceID)then
                allocate(ret(8) )
            else
                allocate(ret(4) )
            endif
        else
            allocate(ret(4) )
        endif

        if(present(interfaces) )then
            interfaces = int(zeros(4) )
        endif
        allocate(id(4) )
        allocate(idr(4) )
        allocate(order(4,3) )
        order(1,:) = [3, 2, 1]
        order(2,:) = [1, 2, 4]
        order(3,:) = [2, 3, 4]
        order(4,:) = [3, 1, 4]
        ret = -1
        n = 0
        do k = 1,4
            idr(1) = obj%elemnod( elemid, order(k,1) )
            idr(2) = obj%elemnod( elemid, order(k,2) )
            idr(3) = obj%elemnod( elemid, order(k,3) )
            do i=size(obj%elemnod,1),1,-1
                if(i==elemid) cycle
                do j=1,4
                    id(1) = obj%elemnod( i, order(j,1) )
                    id(2) = obj%elemnod( i, order(j,2) )
                    id(3) = obj%elemnod( i, order(j,3) )
                    if(sameAsGroup(id,idr) )then
                        if(present(interfaces) )then
                            interfaces(k)=1
                        endif
                        n=n+1
                        ret(n) = i
                        if(size(ret)==8 )then
                            ret(n+4) = j
                        endif
                        exit
                    endif
                enddo
                if(n==k)then
                    exit
                endif
            enddo
        enddo
        call searchAndRemove(vec=ret,leq=0)
        return
    elseif(dimnum==3 .and. elemnodnum==8)then
        ! Tetra mesh
        if(present(withSurfaceID) )then
            if(withSurfaceID)then
                allocate(ret(12) )
            else
                allocate(ret(6) )
            endif
        else
            allocate(ret(6) )
        endif

        if(present(interfaces) )then
            interfaces = int(zeros(6) )
        endif
        allocate(id(6) )
        allocate(idr(6) )
        allocate(order(6,4) )
        order(1,:) = [ 4, 3, 2, 1]
        order(2,:) = [ 1, 2, 6, 5]
        order(3,:) = [ 2, 3, 7, 6]
        order(4,:) = [ 3, 4, 8, 7]
        order(5,:) = [ 4, 1, 5, 8]
        order(6,:) = [ 5, 6, 7, 8]
        ret = -1
        n = 0
        do k = 1,6
            idr(1) = obj%elemnod( elemid, order(k,1) )
            idr(2) = obj%elemnod( elemid, order(k,2) )
            idr(3) = obj%elemnod( elemid, order(k,3) )
            idr(4) = obj%elemnod( elemid, order(k,4) )
            idr(5) = obj%elemnod( elemid, order(k,5) )
            idr(6) = obj%elemnod( elemid, order(k,6) )
            do i=size(obj%elemnod,1),1,-1
                if(i==elemid) cycle
                do j=1,6
                    id(1) = obj%elemnod( i, order(j,1) )
                    id(2) = obj%elemnod( i, order(j,2) )
                    id(3) = obj%elemnod( i, order(j,3) )
                    id(4) = obj%elemnod( i, order(j,4) )
                    id(5) = obj%elemnod( i, order(j,5) )
                    id(6) = obj%elemnod( i, order(j,6) )
                    
                    if(sameAsGroup(id,idr) )then
                        if(present(interfaces) )then
                            interfaces(k)=1
                        endif
                        n=n+1
                        ret(n) = i
                        if(size(ret)==12 )then
                            ret(n+6) = j
                        endif
                        exit
                    endif
                enddo
                if(n==k)then
                    exit
                endif
            enddo
        enddo
        call searchAndRemove(vec=ret,leq=0)
        return
    endif

    allocate(elemlist(size(obj%elemnod,1) ) )
    
    elemlist(:)  = 0
    
    allocate(nodelist(elemnodnum) )
    do i=1,size(obj%elemnod,2)
        nodelist(i)=obj%elemnod(elemid,i)
    enddo

    do i=1,size(obj%elemnod,1)
        exists = .false.
        do j=1,size(nodelist,1)
            if(existIntArray(vector=obj%elemnod,rowid=i,val=nodelist(j) ) .eqv. .true. )then
                exists = .true.
                exit
            else
                cycle
            endif
        enddo
        if(exists .eqv. .true.)then
            elemlist(i) = 1
        endif
    enddo
    allocate(ret(sum(elemlist) ))
    j=0
    do i=1,size(elemlist)
        if(elemlist(i)==1 )then
            j=j+1
            ret(j) = i
        endif
    enddo


end function
!##################################################################################

subroutine editMesh(obj,x,altitude)
    class(Mesh_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x(:),altitude(:)
    real(real64) :: coord(3),top,original_top
    integer(int32) :: i,j

    if(present(x) .and. present(altitude) )then
        ! from x(n) -> x(n+1), the altitute (z-coordinate) changes from al(n) -> al(n+1)
        original_top=maxval(obj%nodcoord(:,3)) 
        do i=1, size(obj%nodcoord,1)
            coord(:) =obj%nodcoord(i,:)
            if(coord(3) <= 0.0d0 )then
                ! only for above-ground part
                cycle
            endif
            do j=1,size(x)-1
                if(x(j) <= coord(1) .and. coord(1)<x(j+1) )then
                    top = (altitude(j+1)-altitude(j))/(x(j+1)-x(j) )*(coord(1)- x(j) ) +altitude(j) 
                    coord(3) = top/original_top*coord(3)
                    exit
                endif
                if(j==size(x)-1 .and. coord(1)==x(j+1) )then
                    top = (altitude(j+1)-altitude(j))/(x(j+1)-x(j) )*(coord(1)- x(j) ) +altitude(j) 
                    coord(3) = top/original_top*coord(3)
                    exit
                endif
            enddo
            obj%nodcoord(i,:) = coord(:) 
        enddo
    endif
    
end subroutine
! ##########################################################################



! ##########################################################################
function getNearestNodeIDMesh(obj,x,y,z,except,exceptlist) result(node_id)
    class(Mesh_),intent(inout) :: obj 
    real(real64),optional,intent(in) :: x,y,z ! coordinate
    integer(int32),optional,intent(in) :: except ! excepted node id
    integer(int32),optional,intent(in) :: exceptlist(:) ! excepted node id
    integer(int32) :: i,j,dim_num, node_num,node_id,except_id
    real(real64),allocatable :: xvec(:),xvec_tr(:),dist_cur, dist_tr

    node_num =size(obj%nodcoord,1) 
    dim_num = size(obj%nodcoord,2)
    except_id = input(default=0, option=except)
    
    allocate(xvec(dim_num),xvec_tr(dim_num) )
    xvec(:) = 0.0d0
    xvec(1) = input(default=0.0d0,option=x)
    xvec(2) = input(default=0.0d0,option=y)
    xvec(3) = input(default=0.0d0,option=z)
    xvec_tr(:) = 0.0d0
    
    node_id = 1
    xvec_tr(:) = obj%nodcoord(1,:)
    dist_cur = dot_product(xvec-xvec_tr,xvec-xvec_tr) 
    do i=1,node_num
        if(i == except_id)then
            cycle
        endif

        if(present(exceptlist) )then
            if(exist(exceptlist,i) .eqv. .true. )then
                cycle
            endif
        endif
        
        xvec_tr(:) = obj%nodcoord(i,:)
        dist_tr = dot_product(xvec-xvec_tr,xvec-xvec_tr)
        if(dist_tr < dist_cur)then
            node_id = i
            dist_cur = dist_tr
        endif
    enddo
end function
! ##########################################################################


! ##########################################################################
function positionMesh(obj,id) result(x)
    class(Mesh_),intent(in) :: obj
    integer(int32),intent(in) :: id ! node_id
    real(real64) :: x(3)
    integer(int32) :: dim_num,i

    dim_num = size(obj%nodcoord,2)
    do i=1,dim_num
        x(i) = obj%nodcoord(id,i)
    enddo
end function
! ##########################################################################

! ##########################################################################
function position_xMesh(obj,id) result(x)
    class(Mesh_),intent(in) :: obj
    integer(int32),intent(in) :: id ! node_id
    real(real64) :: x
    
    x = obj%nodcoord(id,1)

end function
! ##########################################################################

! ##########################################################################
function position_yMesh(obj,id) result(x)
    class(Mesh_),intent(in) :: obj
    integer(int32),intent(in) :: id ! node_id
    real(real64) :: x
    
    x = obj%nodcoord(id,2)

end function
! ##########################################################################

! ##########################################################################
function position_zMesh(obj,id) result(x)
    class(Mesh_),intent(in) :: obj
    integer(int32),intent(in) :: id ! node_id
    real(real64) :: x
    
    x = obj%nodcoord(id,3)

end function
! ##########################################################################


! ##########################################################################
recursive subroutine assembleMesh(obj)
    class(Mesh_),intent(inout) :: obj
    integer(int32),allocatable :: elemnod_1(:,:)
    integer(int32) :: i,j,itr,node_num,dim_num
    integer(int32) :: node1,node2,node3,node4,node_1and2(2)
    real(real64) :: coord(3),center(3),vec1(3),vec2(3)


    ! 点群3DCTから線要素を作成

    ! strategy#1
    ! 1. 最寄りと結合し線分へ
    ! 2. 線分の中心から最寄りの線分を探索
    ! 3. 発見した線分と結合
    
    ! 1. 最寄りと結合し線分へ
    node_num = size(obj%nodcoord,1)
    dim_num  = size(obj%nodcoord,2)

    if(dim_num/=3)then
        print *, "ERROR >> assembleMesh >> size(obj%nodcoord,1) should be 3"
        stop
    endif
    allocate(elemnod_1(2*node_num,2) )
    elemnod_1(:,:) = 0

    do i=1,node_num
        node1 = i
        coord(:) = obj%nodcoord(node1,:)
        
        node2 = obj%getNearestNodeID(&
            x=coord(1),&
            y=coord(2),&
            z=coord(3),&
            except=node1 &
            )

        node_1and2(1) = node1
        node_1and2(2) = node2
        node3 = obj%getNearestNodeID(&
            x=coord(1),&
            y=coord(2),&
            z=coord(3),&
            exceptlist=node_1and2 &
            )
        elemnod_1(i,1) = node1
        elemnod_1(i,2) = node2
        elemnod_1(i+node_num ,1) = node1
        elemnod_1(i+node_num, 2) = node3

        ! もし同じ方向だったら削除
        vec1(:) = obj%nodcoord(node3,:) - obj%nodcoord(node1,:)
        vec2(:) = obj%nodcoord(node2,:) - obj%nodcoord(node1,:)
        if( dot_product(vec1,vec2) > 0.0d0 )then
            elemnod_1(i+node_num ,1) = 0
            elemnod_1(i+node_num, 2) = 0
        endif

    enddo

    do i=1,size(elemnod_1,1)
        node1 = elemnod_1(i,1) 
        node2 = elemnod_1(i,2) 
        do j=i+1,size(elemnod_1,1)
            if(elemnod_1(j,1) == 0)then
                cycle
            endif

            if(elemnod_1(j,1) == node1 .and. &
            elemnod_1(j,2) == node2 )then
                elemnod_1(j,:) = 0
            endif

            if(elemnod_1(j,2) == node1 .and. &
            elemnod_1(j,1) == node2 )then
                elemnod_1(j,:) = 0
            endif
        enddo
    enddo

    itr=0
    do i=1,size(elemnod_1,1)
        if(elemnod_1(i,1)==0 )then
            itr=itr+1
        endif
    enddo

    if(allocated(obj%elemnod) ) then
        deallocate(obj%elemnod)
    endif

    ! remove 
    ! A ->B
    ! B <- A



    allocate(obj%elemnod(size(elemnod_1,1)-itr,8 ) )
    obj%elemnod(:,:)=0
    itr=0
    do i=1,size(elemnod_1,1)
        if(minval(elemnod_1(i,:))==0 )then
            cycle
        else
            itr=itr+1
            obj%elemnod(itr,1) = elemnod_1(i,1)
            obj%elemnod(itr,2:8) = elemnod_1(i,2)
        endif
    enddo


    if(minval(obj%elemnod) == 0 ) then
        print*, "ERROR :: assembleMesh minval(obj%elemnod) == 0 "
        stop
    endif


end subroutine
! ##########################################################################
subroutine arrangeNodeOrderMesh(obj,NumberOfLayer)
    class(Mesh_) ,intent(inout):: obj
    integer(int32),optional,intent(in) :: NumberOfLayer
    integer(int32),allocatable :: layer(:)
    real(real64),allocatable :: center(:),x(:),radius(:),nodcoord(:,:),nodeorder(:)
    real(real64) :: dr
    integer(int32) :: i,j,k,n,nl

    if(.not.allocated(obj%nodcoord) ) then
        print *, "ERROR :: no nodal coordinate was found."
        return
    endif

    ! arrange nodes from outer to center
    center = zeros(size(obj%nodcoord,2) )
    x = zeros(size(obj%nodcoord,2) )

    do i=1,size(center)
        center(i) = 1.0d0/dble(size(obj%nodcoord,1) ) *sum(obj%nodcoord(:,i) )
    enddo

    nodeorder = zeros(size(obj%nodcoord,1) )
    layer = int(zeros(size(obj%nodcoord,1) ))
    radius = zeros(size(obj%nodcoord,1) )
    
    nl = input(default=10,option=NumberOfLayer)

    do i=1,size(obj%nodcoord,1) 
        nodeorder(i) = dble(i)
        x(:) = obj%nodcoord(i,:)
        radius(i) = sqrt( dot_product(center-x,center-x) )
    enddo

    
    dr = maxval(radius)/dble(nl)

    do i=1,size(obj%nodcoord,1) 
        layer(i) = int(radius(i)/dr)
    enddo

    call heapsort(n=size(obj%nodcoord,1), array=layer, val=nodeorder)

    nodcoord = obj%nodcoord
    do i=1, sizE(nodeorder)
        obj%nodcoord(i,:) = nodcoord( int(nodeorder(i)),: )
    enddo

end subroutine


! ##########################################################################

subroutine addElementsMesh(obj,Connectivity)
    class(Mesh_),intent(inout) :: obj
    integer(int32),intent(in) :: connectivity(:,:)
    integer(int32),allocatable :: buf(:,:)
    integer(int32) :: n,m,i,newnum
    n = size(obj%elemnod,1)
    m = size(obj%elemnod,2)
    newnum = sizE(connectivity,1)
    if(m/=size(connectivity,2) )then
        print *, "ERROR ::addElementsMesh >>  size(obj%elemnod,2) /=  size(connectivity,2)"
        stop
    endif

    allocate(buf(n+newnum,m) )
    buf(1:n,:) = obj%elemnod(:,:)
    buf(n+1:,:) = connectivity(:,:)

    obj%elemnod = buf

end subroutine

! ##########################################################################

subroutine removeElementsMesh(obj,ElementIDs)
    class(Mesh_),intent(inout) :: obj
    integer(int32),intent(in) :: ElementIDs(:)
    integer(int32),allocatable :: buf(:,:)
    integer(int32) :: n,m,i,rmnum,itr
    n = size(obj%elemnod,1)
    m = size(obj%elemnod,2)
    rmnum = size(ElementIDs)
    allocate(buf(n-rmnum,m) )
    do i=1,rmnum
        obj%elemnod(ElementIDs(i),1) = -1
    enddo
    itr = 0
    do i=1,n
        if( obj%elemnod(i,1) == -1 )then
            cycle
        else
            itr=itr+1
            buf(itr,:) = obj%elemnod(i,:)
        endif
    enddo

    obj%elemnod = buf

end subroutine

! ##########################################################################


end module MeshClass