! updated 2019/1/19
module MeshClass
    use std
    implicit none


    type:: Mesh_
        real(real64),allocatable  ::NodCoord(:,:)
        real(real64),allocatable  ::NodCoordInit(:,:)
        integer(int32),allocatable::ElemNod(:,:)
        integer(int32),allocatable::FacetElemNod(:,:)
        integer(int32),allocatable::NextFacets(:,:)
        integer(int32),allocatable::SurfaceLine2D(:)
        integer(int32),allocatable::ElemMat(:)
        integer(int32),allocatable::SubMeshNodFromTo(:,:)
        integer(int32),allocatable::SubMeshElemFromTo(:,:)
        integer(int32),allocatable::SubMeshSurfFromTo(:,:)
        integer(int32) :: surface=1

        !for Interfaces
        integer(int32),allocatable::GlobalNodID(:)

        character*200::FileName=" "
        character*70::ElemType=" "
        character*70:: ErrorMsg=" "

    contains
        procedure :: add => addMesh
        procedure :: adjustSphere => AdjustSphereMesh

        procedure :: copy => CopyMesh
        procedure :: cut => cutMesh
        procedure :: convertMeshType => ConvertMeshTypeMesh
        procedure :: convertTetraToHexa => convertTetraToHexaMesh 
        procedure :: convertTriangleToRectangular => convertTriangleToRectangularMesh 
        procedure :: create=>createMesh
        procedure :: check=>checkMesh
        procedure :: convert2Dto3D => Convert2Dto3DMesh
        
        procedure :: delete => DeallocateMesh
        procedure :: detectIface => detectIfaceMesh
        procedure :: displayMesh => DisplayMesh 
        procedure :: display => DisplayMesh 
        procedure :: divide => divideMesh
        procedure :: delauneygetNewNode => DelauneygetNewNodeMesh 
        procedure :: delauneygetNewTriangle => DelauneygetNewTriangleMesh 
        procedure :: delauneyremoveOverlaps => DelauneyremoveOverlapsMesh 
        
        procedure :: export => exportMeshObj
        procedure :: exportElemNod => ExportElemNod
        procedure :: exportNodCoord => ExportNodCoord
        procedure :: exportSurface2D => ExportSurface2D
        procedure :: empty => emptyMesh
        
        procedure :: getFacetElement => GetFacetElement
        procedure :: getSurface => GetSurface
        procedure :: getInterface => GetInterface
        procedure :: getInterfaceElemNod => GetInterfaceElemNod
        procedure :: getBoundingBox     => GetBoundingBox
        procedure :: getFacetElemInsideBox => GetFacetElemInsideBox
        procedure :: getInterSectBox => GetInterSectBox
        procedure :: getNextFacets => GetNextFacets 
        procedure :: getElemType => GetElemTypeMesh 
        procedure :: getNumOfDomain => getNumOfDomainMesh
        procedure :: getCircumscribedCircle => getCircumscribedCircleMesh
        procedure :: getCircumscribedTriangle => getCircumscribedTriangleMesh
        procedure :: gmsh => gmshMesh
        
        procedure :: import => importMeshObj 
        procedure :: importElemNod => ImportElemNod
        procedure :: importNodCoord => ImportNodCoord
        procedure :: importElemMat => ImportElemMat
        procedure :: init => InitializeMesh
        
        procedure :: length => lengthMesh
        
        procedure :: mergeMesh => MergeMesh
        procedure :: meltingSkelton => MeltingSkeltonMesh 
        procedure :: meshing    => MeshingMesh

        procedure :: HowManyDomain => HowManyDomainMesh

        procedure :: open => openMesh

        procedure :: remove => removeMesh
        procedure :: removeCircumscribedTriangle => removeCircumscribedTriangleMesh
        procedure :: removeFailedTriangle => RemoveFailedTriangleMesh
        procedure :: removeOverlappedNode =>removeOverlappedNodeMesh
        procedure :: resize => resizeMeshobj
        
        procedure :: save    => saveMesh 
        procedure :: sortFacet    => SortFacetMesh 
        procedure :: shift=>shiftMesh
        procedure :: showRange => showRangeMesh
        procedure :: showMesh => ShowMesh 
        procedure :: show => ShowMesh 

        
    end type Mesh_


    contains

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
            call system("mkdir -p "//trim(path)//"/"//trim(adjustl(name)))
            call f%open(trim(path)//"/"//trim(adjustl(name))//"/","Mesh",".prop")
            !call obj%gmsh(Name=trim(path)//"/"//trim(adjustl(name))//"/Mesh")
            !call obj%export(path=trim(path)//"/"//trim(adjustl(name))//"/",name="Mesh")
            !print *, trim(path)//"/"//trim(adjustl(name))//"/","Mesh",".prop"

        else
            call system("mkdir -p "//trim(path)//"/Mesh")
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


subroutine removeMesh(obj)
    class(Mesh_),intent(inout)::obj

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
    
    include "./ImportElemNod.f90"

end subroutine ImportElemNod
!##################################################





!##################################################
subroutine ImportNodCoord(obj,nod_coord)
    class(Mesh_),intent(inout)::obj
    real(real64),intent(in)::nod_coord(:,:)

    include "./ImportNodCoord.f90"

end subroutine ImportNodCoord
!##################################################




!##################################################
subroutine ImportElemMat(obj,elem_mat)
    class(Mesh_),intent(inout)::obj
    integer(int32),intent(in)::elem_mat(:)

    include "./ImportElemMat.f90"

end subroutine ImportElemMat
!##################################################

subroutine resizeMeshobj(obj,x_rate,y_rate,z_rate)
    class(Mesh_),intent(inout) :: obj
	real(real64),optional,intent(in) :: x_rate,y_rate,z_rate

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
subroutine importMeshObj(obj,FileName,extention,ElemType,Mesh)
    class(Mesh_),intent(inout)::obj
    type(Mesh_),optional,intent(in) :: Mesh
    character(*),optional,intent(in)::FileName,extention,ElemType
    character(200) :: MeshVersionFormatted,Dim,Vertices,Edges,Triangles
    character(200) :: Tetrahedra
    real(real64) :: null_num_real
    integer(int32) :: dim_num,node_num,elem_num,elemnod_num,i,j
    integer(int32) :: edge_num,null_num_int,num_of_triangles
    integer(int32) :: num_of_Tetrahedra

    call obj%delete()
    if(present(Mesh) )then
        call obj%copy(Mesh)
        return
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
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    character(200) :: fieldname
    type(IO_) :: f
    integer(int32) :: i,j,dim_num
	real(real64) :: x1(3),x2(3),x3(3)
    
    if(present(name) )then
        fieldname=trim(adjustl(name))
    else
        fieldname="Mesh"
    endif

    call system("mkdir -p "//trim(path)//"/Mesh")

    if(obj%empty() .eqv. .true.)then
        return
    endif


    if(present(restart) )then
        call system("mkdir -p "//trim(path)//"/Mesh")
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
        
        call system("mkdir -p "//trim(path)//"/Mesh")
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
    


    call system("mkdir -p "//trim(path)//"/Mesh")
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
        call system("mkdir -p "//trim(path)//"/Mesh")
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

!##################################################
subroutine GetFacetElement(obj)
    class(Mesh_),intent(inout)::obj


    integer(int32) :: i,j,k,l,n
    integer(int32) :: NumOfElem,NumOfDim,NumNodePerElem
    integer(int32) :: id_1,id_2,id_3,id_4
    integer(int32) :: id_r1,id_r2,id_r3,id_r4
    integer(int32),allocatable::id(:),idr(:)
    integer(int32),allocatable::buffer(:,:)


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

    real(real64),allocatable::width1(:),width2(:),center1(:),center2(:),max_coord(:),min_coord(:)
    real(real64) :: xmax_(2),xmin_(2)
    integer(int32) :: dim_num,i,j,c_or_not


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
subroutine addMesh(obj,mesh)
    class(Mesh_),intent(inout) :: obj
    class(Mesh_),intent(inout)    :: mesh
    integer(int32) :: NumOfElem

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
    
    include "./ExportElemNod.f90"

    
end subroutine ExportElemNod
!##################################################

!##################################################
subroutine ExportNodCoord(obj,nod_coord)
    class(Mesh_),intent(inout)::obj
    real(real64),allocatable,intent(inout)::nod_coord(:,:)

    include "./ExportNodCoord.f90"

    

end subroutine ExportNodCoord
!##################################################



!##################################################
subroutine ExportSurface2D(obj,surface_nod)
    class(Mesh_),intent(inout)::obj
    integer(int32),allocatable,intent(inout)::surface_nod(:)

    include "./ExportSurface2D.f90"

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
        call system("gnuplot SurfaceLine2D.gp")
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
        call system("gnuplot ElemLine2D.gp")

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

call system(command_mkdir )
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
    integer(int32),optional,intent(in) :: Mode,itr_tol
    integer(int32) :: i,j,k,n,m,node_num,dim_num,dim_mode
    real(real64),allocatable :: stage_range(:,:),triangle(:,:)
    integer(int32),allocatable :: staged_node(:)
    real(real64) :: centerx,centery,centerz,radius
    logical :: NoChange

    ! This method creates mesh-connectivity for the given nodal coordinates.
    ! Therefore, Mesh%NodCoord(:,:) should be filled preliminary.

    dim_mode=input(default=2,option=Mode)
    if(dim_mode==2)then
        if(.not. allocated(obj%NodCoord) )then
            print *, "ERROR :: MeshClass MeshingMesh"
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
subroutine DelauneygetNewNodeMesh(obj,node_id,staged_node,triangle)
    class(Mesh_),intent(inout)::obj
    integer(int32),intent(in) :: node_id
    integer(int32),intent(inout):: staged_node(:) ! if =1,staged.
    real(real64),intent(inout)  :: triangle(:,:)
    real(real64) :: avec(3),bvec(3),cvec(3),s,t
    integer(int32) :: triangle_node_id(3),new_node_id,i,j,n,point,cover_triangle


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
    class(Mesh_),intent(inout)::obj
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

recursive subroutine createMesh(obj,meshtype,x_num,y_num,x_len,y_len,Le,Lh,Dr,thickness,&
    division,smooth,top,margin,inclineRate)
    class(Mesh_),intent(inout) :: obj
    type(Mesh_) :: mesh1,mesh2
    character(*),intent(in) :: meshtype
    logical,optional,intent(in) :: smooth
    integer(int32),optional,intent(in) :: x_num,y_num ! number of division
    integer(int32),optional,intent(in) :: division ! for 3D rectangular
    real(real64),optional,intent(in) :: x_len,y_len,Le,Lh,Dr ! length
    real(real64),optional,intent(in) :: thickness,inclineRate ! for 3D rectangular
    real(real64),optional,intent(in) :: top,margin ! for 3D rectangular
    integer(int32) :: i,j,n,m,xn,yn,smoothedge(8)
    real(real64)::lx,ly,sx,sy,a_val,radius,x_,y_,diflen,Lt,unitx,unity,xm, ym,tp,rx,ry
    real(real64)::ymin,ymax
    ! this subroutine creates mesh


    if(meshtype=="Bar1D" .or. meshtype=="bar1D")then
        ! need x_len, x_num
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
        call obj%create(meshtype="rectangular2D",x_num=x_num,y_num=y_num,x_len=x_len,y_len=y_len)
        call obj%Convert2Dto3D(Thickness=Thickness,division=division)
        if(.not.allocated(obj%ElemMat))then
            n=size(obj%ElemNod,1)
            allocate(obj%ElemMat(n) )
        endif
        return
    endif


    if(meshtype=="Dam3D" )then
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

    
    if(meshtype=="Sphere3D")then
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


    if(meshtype=="rectangular2D")then
        xn=input(default=1,option=x_num)
        yn=input(default=1,option=y_num)
        lx=input(default=1.0d0,option=x_len)
        ly=input(default=1.0d0,option=y_len)
        unitx=x_len/dble(xn)
        unity=y_len/dble(yn)
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
        obj%ElemNod(2,1)=2
        obj%ElemNod(2,2)=3
        obj%ElemNod(2,3)=yn+4
        obj%ElemNod(2,4)=yn+3

        
        n=0
        do j=1, yn
            do i=1, xn
                n=n+1
                obj%ElemNod(n,1)=i + (j-1)*(xn+1)
                obj%ElemNod(n,2)=i+1 + (j-1)*(xn+1)
                obj%ElemNod(n,3)=xn+2+i+ + (j-1)*(xn+1)
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
        obj%ElemNod(2,1)=2
        obj%ElemNod(2,2)=3
        obj%ElemNod(2,3)=yn+4
        obj%ElemNod(2,4)=yn+3

        
        n=0
        do j=1, yn
            do i=1, xn
                n=n+1
                obj%ElemNod(n,1)=i + (j-1)*(xn+1)
                obj%ElemNod(n,2)=i+1 + (j-1)*(xn+1)
                obj%ElemNod(n,3)=xn+2+i+ + (j-1)*(xn+1)
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
        obj%ElemNod(2,1)=2
        obj%ElemNod(2,2)=3
        obj%ElemNod(2,3)=yn+4
        obj%ElemNod(2,4)=yn+3

        
        n=0
        do j=1, yn
            do i=1, xn
                n=n+1
                obj%ElemNod(n,1)=i + (j-1)*(xn+1)
                obj%ElemNod(n,2)=i+1 + (j-1)*(xn+1)
                obj%ElemNod(n,3)=xn+2+i+ + (j-1)*(xn+1)
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
!##################################################`



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
		
		!call system(  "touch "//trim(adjustl(name))//trim(obj%FileName)//trim(filename) )
		print *, trim(adjustl(name))//trim(filename)
		open(fh,file=trim(adjustl(name))//trim(filename) )
		print *, "writing ",trim(adjustl(name))//trim(filename)," step>>",step
	else
		filename=filetitle//filename0
		!call system(  "touch "//trim(obj%FileName)//trim(filename) )
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
    integer(int32),optional,intent(in) :: n
    integer(int32) :: i,j,k,l,m,mesh_num,loc_elem_num,elem_num,elem_type,dim_num
    integer(int32) :: cur_node_id,cur_elem_id,local_id,global_id,num_loc_node
    logical,allocatable :: selected(:)
    integer(int32),allocatable :: global_vs_local(:,:)
    integer(int32),allocatable :: buffer(:,:)
    logical :: tf

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


! ################################################################################
! followings are exported from mpi_leaflow_1.0.0 @ 2020/06/20
subroutine mpi_greedy_division(infile,my_rank,petot,mpi_elem_nod,mpi_elem_nod_id,elem_num)
	
	integer i,j,k,n,mpi_elem_num,rem,elem_type,current_id
	integer itr,node_ID,l,m,ierr,min_nodeID,start_ID,end_ID
	integer,allocatable::elem_nod(:,:),mpi_elem_eat(:),recv_elem_eat(:),inside(:)
	integer,allocatable,intent(out)::mpi_elem_nod(:,:),mpi_elem_nod_id(:)
	integer,allocatable::iface(:),next_candidate(:)
	integer,intent(in)::my_rank,petot
	integer,intent(out)::elem_num
	character*17,intent(in)::infile
	character*17 outfile
	
	open(10,file=infile,status="old")
	read(10,*)elem_num,elem_type
	
	
	!--- allocate global connectivity --------------
	allocate(elem_nod(elem_num,elem_type),mpi_elem_eat(elem_num),recv_elem_eat(elem_num) )
	allocate(iface(elem_type))
	mpi_elem_eat(:)=0
	recv_elem_eat(:)=0
	call mpi_barrier(mpi_comm_world,ierr)
	!-----------------------------------------------
	
	
	
	!--- input global connectivity -----------------
	do i=1,elem_num
		read(10,*)elem_nod(i,1:elem_type)
	enddo
	!-----------------------------------------------
	close(10)
	
	
	
	!---- compute individual element number --------
	mpi_elem_num=int(elem_num/petot)
	rem=elem_num - mpi_elem_num*petot
	if(my_rank+1 <= rem)then
		mpi_elem_num=mpi_elem_num+1
	endif
	!-----------------------------------------------
	
	
	
	!------- allocate local connectivity -----------
	
	! Global node ID = mpi_nod_coord_id( Local node ID )
	allocate(mpi_elem_nod(mpi_elem_num,elem_type))
	allocate(mpi_elem_nod_id(mpi_elem_num))
	mpi_elem_nod(:,:)=0
	mpi_elem_nod_id(:)=0
	!-----------------------------------------------
	
	
	
	
	!>>>>>>>>>>>>>> Greedy's method >>>>>>>>>>>>>>>>
	
	!----- initialization -----------------
	itr = 0
	current_id=1
	
	!--------------------------------------
	
	
	
	!---- Greedy's method ----------------
	do i=1,petot
	

		if(my_rank+1 == i)then
			outfile="visual_parts.gp"
			if(i==1)then
				!open(20,file=outfile)
				!write(outfile,'("mpi_nod", i6.6, ".txt")') my_rank+1
				!write(20,*) "plot '",outfile,"'"
				!close(20)
			else
				!open(20,file=outfile,position="append")
				!write(outfile,'("mpi_nod", i6.6, ".txt")') my_rank+1
				!write(20,*) "replot '",outfile,"'"
				!close(20)		
			endif
			do j =1,size(mpi_elem_eat)
				if(mpi_elem_eat(j)==0)then
					min_nodeID=j
					exit
				endif
			enddo
			
			mpi_elem_nod(1,:)=elem_nod(min_nodeID,:)
			mpi_elem_nod_id(1)=min_nodeID
			current_id=current_id+1
			mpi_elem_eat(min_nodeID)=1
			start_ID=current_id-1
			end_ID=current_id-1
			
			do 
				
				do j=start_ID,end_ID
					do k=1,elem_type
						node_ID=mpi_elem_nod(j,k)
						
						!-- search and add ---------------
						do l=1,elem_num
							if(mpi_elem_eat(l)==1)then
								cycle
							else
								do m=1,elem_type
									if(node_ID==elem_nod(l,m))then
										mpi_elem_nod(current_id,:)=elem_nod(l,:)
										mpi_elem_nod_id(current_id)=l
										mpi_elem_eat(l)=1
										current_id=current_id+1
										exit
									endif
								enddo
								
							endif
							
							!-- check end ----
							if(current_id==size(mpi_elem_nod,1)+1)then
								exit
							endif			
							!-----------------	
							
							
							
						enddo
						!---------------------------------
						
						
						!-- check end ----
						if(current_id==size(mpi_elem_nod,1)+1)then
							exit
						endif			
						!-----------------	
					enddo
					
					
					!-- check end ----
					if(current_id==size(mpi_elem_nod,1)+1)then
						exit
					endif			
					!-----------------	
				enddo
				
				!-- check end ----
				if(current_id==size(mpi_elem_nod,1)+1)then
					
					print *, "my_rank is",my_rank,"proceeding is ",current_id-1,"/",size(mpi_elem_nod,1)	
					exit
				endif		
				
				!-----------------
				
				
				
				if(end_ID==current_id-1)then
					!-- no harvest ----
					do j =1,size(mpi_elem_eat)
						if(mpi_elem_eat(j)==0)then
							min_nodeID=j
							exit
						endif
					enddo
					
					mpi_elem_nod(current_id,1:elem_type)=elem_nod(j,1:elem_type)
					mpi_elem_nod_id(current_id)=j
					current_id=current_id+1
					mpi_elem_eat(min_nodeID)=1			
				endif	
				start_ID=end_ID+1	
				end_ID=current_id-1
							
				
			enddo
			
			
		endif
		call mpi_barrier(mpi_comm_world,ierr)
		
		!share mpi_elem_eat
		recv_elem_eat(:)=mpi_elem_eat(:)
		call mpi_allreduce(mpi_elem_eat(1),recv_elem_eat(1),elem_num,mpi_integer,mpi_max,mpi_comm_world,ierr)
		mpi_elem_eat(:)=recv_elem_eat(:)
		
	enddo
	
	write(outfile,'("mpi_ele", i6.6, ".txt")') my_rank+1
!	open(10,file=outfile)
!	write(10,*)size(mpi_elem_nod,1)
!	do i=1,size(mpi_elem_nod,1)
!		write(10,*)mpi_elem_nod(i,:)
!	enddo
!	close(10)
	
end subroutine mpi_greedy_division

!#######################################################################################

subroutine mpi_node_coord_read(infile_node,mpi_elem_nod,my_rank,petot,mpi_nod_coord,mpi_nod_coord_id,node_num)
	integer,intent(in)::mpi_elem_nod(:,:),my_rank,petot
	character*17,intent(in)::infile_node
	character*17 outfile
	
	integer i,j,k,l,count_nodes,node_ID,new0_or_old1,itr,dim_space,min_nodID
	integer,intent(out)::node_num
	integer,allocatable,intent(out)::mpi_nod_coord_id(:)
	real,allocatable::mpi_nod_coord_id_es(:),null_num(:)
	
	real(8),allocatable,intent(out)::mpi_nod_coord(:,:)
	
	
	!-- count number of nodes in the partition ----
	count_nodes=1
	allocate(mpi_nod_coord_id(size(mpi_elem_nod,1)*size(mpi_elem_nod,2)))
	mpi_nod_coord_id(:)=-1
	do i=1,size(mpi_elem_nod,1)
		do j=1,size(mpi_elem_nod,2)
			if(i*j==1)then
				mpi_nod_coord_id(1)=mpi_elem_nod(i,j)
				count_nodes=count_nodes+1
				cycle
			else
				node_ID=mpi_elem_nod(i,j)
				new0_or_old1=0
				do k=1,count_nodes-1
					if(mpi_nod_coord_id(k)==node_ID)then
						new0_or_old1=1
						exit
					else
						cycle
					endif
				enddo
				
				!-- judge new =0 or old =1--
				if(new0_or_old1==0)then
					mpi_nod_coord_id(count_nodes)=node_ID
					count_nodes=count_nodes+1
				endif
				!---------------------------
			
			endif
		enddo
	enddo
	count_nodes=count_nodes-1
	
	!----------------------------------------------
	
	
	
	
	
	
	
	
	!---- delete remains --------------------------
	allocate(mpi_nod_coord_id_es(count_nodes))
	do i=1,count_nodes
		mpi_nod_coord_id_es(i)=mpi_nod_coord_id(i)
	enddo
	
	deallocate(mpi_nod_coord_id)
	allocate(mpi_nod_coord_id(count_nodes))
	mpi_nod_coord_id(:)=mpi_nod_coord_id_es(:)
	
	!----------------------------------------------
	
	
	
	
	
	!--- sort mpi_nod_coord_id ------------------------
	do i=1,count_nodes
		
		do j=i,count_nodes
			if(mpi_nod_coord_id(j)==minval(mpi_nod_coord_id(i:count_nodes)))then
				min_nodID=mpi_nod_coord_id(j)
				mpi_nod_coord_id(j)=mpi_nod_coord_id(i)
				mpi_nod_coord_id(i)=min_nodID
				
				exit
			endif
		enddo
		
		
	enddo
	!mpi_nod_coord_id(:)=mpi_nod_coord_id_es(:)
	deallocate(mpi_nod_coord_id_es)
	
	!----------------------------------------------
	
	
	
	!---- read coordinates in the partition ----------
	open(10,file=infile_node,status="old")
	read(10,*)node_num,dim_space
	allocate(mpi_nod_coord(count_nodes,dim_space))
	allocate(null_num(dim_space))
	itr=1
	do i=1,node_num
		if(mpi_nod_coord_id(itr)==i)then
			read(10,*)mpi_nod_coord(itr,:)
			itr=itr+1
		else
			read(10,*) null_num(:)
		endif
	enddo
	close(10)
	!-------------------------------------------------
	
	
	!------ output local coordinates -----------------
!	write(outfile,'("mpi_nod", i6.6, ".txt")') my_rank+1
!	open(10,file=outfile)
!	do i=1,size(mpi_nod_coord,1)
!		write(10,*)mpi_nod_coord(i,:)
!	enddo
!	close(10)
	!-------------------------------------------------
	
	
	
end subroutine mpi_node_coord_read
!#######################################################################################
subroutine mpi_node_relation(mpi_nod_coord_id,my_rank,petot,node_num,mpi_nod_bound_num,mpi_nod_comm_ID)
	
	integer,intent(in)::mpi_nod_coord_id(:),my_rank,petot,node_num
	integer,allocatable,intent(out)::mpi_nod_bound_num(:),mpi_nod_comm_ID(:,:)
	integer,allocatable::common_flag_loc(:),common_flag_glo(:)
	integer i,j,local_num,global_num,loc_id,ierr,max_comm,itr
	character*17 outfile
	
	allocate(mpi_nod_bound_num(size(mpi_nod_coord_id)))
	
	
	
	!----- detect the number of overlapping for each node -------------------
	do i=1,node_num
		local_num=0
		do j=1,size(mpi_nod_coord_id)
			if(i==mpi_nod_coord_id(j))then
				local_num=1
				loc_id=j
				exit
			endif
		enddo
		global_num=0
		call mpi_allreduce(local_num,global_num,1,mpi_integer,mpi_sum,mpi_comm_world,ierr)
		if(local_num==1)then
			mpi_nod_bound_num(loc_id)=global_num
		endif
	enddo
	mpi_nod_bound_num(:)=mpi_nod_bound_num(:)-1
	!-------------------------------------------------------------------------
	
	
	
	
	!------ max. overlaps ------------------
	max_comm=maxval(mpi_nod_bound_num(:))
	!---------------------------------------
	
	
	
	
	
	!----- get pointer of common nodes to server ID ---------------------------
	allocate(mpi_nod_comm_ID(size(mpi_nod_coord_id),max_comm))
	allocate(common_flag_loc(petot))
	allocate(common_flag_glo(petot))
	mpi_nod_comm_ID(:,:)=0
	
	
	do i=1,node_num
		local_num=0
		do j=1,size(mpi_nod_coord_id)
			if(i==mpi_nod_coord_id(j))then
				local_num=1
				loc_id=j
				exit
			endif
		enddo
		
		global_num=0
		common_flag_glo(:)=0
		common_flag_loc(:)=0
		common_flag_loc(my_rank+1)=local_num
		
		call mpi_allreduce(common_flag_loc(1),common_flag_glo(1),petot,mpi_integer,mpi_sum,mpi_comm_world,ierr)
		
		if(local_num==1)then
			itr=0
			do j=1,petot
				if(common_flag_glo(j)==1 .and. j/=my_rank+1 )then
					itr=itr+1
					mpi_nod_comm_ID(loc_id,itr)=j
				endif
			enddo
		endif
	enddo	
	
	
	!--------------------------------------------------------------------------
	
	
	
	!------ output mpi node boundary numbers ---------
!	write(outfile,'("mpi_bou", i6.6, ".txt")') my_rank+1
!	open(10,file=outfile)
!	do i=1,size(mpi_nod_bound_num,1)
!		write(10,*)mpi_nod_bound_num(i),mpi_nod_comm_ID(i,:)
!	enddo
!	close(10)
	!-------------------------------------------------	
	
end subroutine mpi_node_relation
!#######################################################################################



subroutine mpi_read_mat_para(infile_mat,mpi_elem_nod,mpi_elem_nod_id,my_rank,petot,elem_num,mpi_elem_mat,mat_cons)
	
	integer i,j,mpi_elem_num,itr,null_8,mat_num,para_num,exist0_or_not1
	integer,intent(in)::mpi_elem_nod(:,:),mpi_elem_nod_id(:),my_rank,petot,elem_num
	integer,allocatable,intent(out)::mpi_elem_mat(:)
	real(8),allocatable,intent(out)::mat_cons(:,:)
	character*17,intent(in)::infile_mat
	character*17 outfile
	
	mpi_elem_num=size(mpi_elem_nod,1)
	allocate(mpi_elem_mat(mpi_elem_num))
	
	open(10,file=infile_mat,status="old")
	do i=1,elem_num
		exist0_or_not1=1
		do j=1,mpi_elem_num
			if(i==mpi_elem_nod_id(j))then
				read(10,*)mpi_elem_mat(j)
				itr=itr+1
				exist0_or_not1=0
				exit
			endif
		enddo
		if(exist0_or_not1==1)then
			read(10,*)null_8
		endif
	enddo
	
	read(10,*)mat_num,para_num
	allocate(mat_cons(mat_num,para_num))

	do i=1,mat_num
		read(10,*)mat_cons(i,1:para_num)
	enddo
	close(10)
	!------ output mpi node boundary numbers ---------
!	write(outfile,'("mpi_mat", i6.6, ".txt")') my_rank+1
!	open(20,file=outfile)
!	do i=1,mpi_elem_num
!		write(20,*)mpi_elem_mat(i),mat_cons( mpi_elem_mat(i) ,:)
!	enddo
!	close(20)
	!-------------------------------------------------			
end subroutine mpi_read_mat_para
!#######################################################################################
subroutine mpi_read_bound_cond(infile_bound,my_rank,mpi_nod_coord_id,&
		mpi_n_bc_nod,mpi_d_bc_nod,mpi_n_bc_val,mpi_d_bc_val)
	
	real(8) read_real
	integer i,j,file_id,n,n_bc_num,d_bc_num,itr,exist0_or_not1,read_int
	integer,intent(in)::my_rank,mpi_nod_coord_id(:)
	character*17,intent(in)::infile_bound
	character*17 outfile
	
	integer,allocatable::mpi_n_bc_nod_es(:),mpi_d_bc_nod_es(:)
	real(8),allocatable::mpi_n_bc_val_es(:),mpi_d_bc_val_es(:)
	
	integer,allocatable,intent(out)::mpi_n_bc_nod(:),mpi_d_bc_nod(:)
	real(8),allocatable,intent(out)::mpi_n_bc_val(:),mpi_d_bc_val(:)
	
	file_id=my_rank+1000
	open(file_id,file=infile_bound,status="old")
	
	
	
	!---- Dirichlet boundary conditions -------------------------
	read(file_id,*) d_bc_num
	allocate(mpi_d_bc_nod_es(d_bc_num),mpi_d_bc_val_es(d_bc_num))
	itr=1
	do i=1,d_bc_num
		read(file_id,*) read_int,read_real
		exist0_or_not1=1
		do j=1,size(mpi_nod_coord_id)
			if(mpi_nod_coord_id(j)==read_int)then
				exist0_or_not1=0
				exit
			endif
		enddo
		if(exist0_or_not1==0)then
			mpi_d_bc_nod_es(itr)=read_int
			mpi_d_bc_val_es(itr)=read_real
			itr=itr+1
		endif
	enddo
	
	if( d_bc_num==1)then
		print *, "Error:mpi_fem_lib.f90 L505 >> no Dirichlet B.C."
		 stop 
	endif
	allocate(mpi_d_bc_nod(itr-1),mpi_d_bc_val(itr-1))
	
	do i=1,itr-1
		mpi_d_bc_nod(i)=mpi_d_bc_nod_es(i)
		mpi_d_bc_val(i)=mpi_d_bc_val_es(i)
		!print *, "d_bc",mpi_d_bc_nod(i),mpi_d_bc_val(i)
	enddo
	
	
	!------------------------------------------------------------
	
	
	
	
	!---- Neumann boundary conditions ----------------------------
	read(file_id,*) n_bc_num

        
	if(n_bc_num==0)then
		close(file_id)
	else
		allocate(mpi_n_bc_nod_es(n_bc_num),mpi_n_bc_val_es(n_bc_num))
		
		itr=1
		
		do i=1,n_bc_num
			read(file_id,*) read_int,read_real
			exist0_or_not1=1
			do j=1,size(mpi_nod_coord_id)
				if(mpi_nod_coord_id(j)==read_int)then
					exist0_or_not1=0
					exit
				endif
			enddo
			if(exist0_or_not1==0)then
				mpi_n_bc_nod_es(itr)=read_int
				mpi_n_bc_val_es(itr)=read_real
				itr=itr+1
			endif
		enddo

		if(itr/=1)then
			allocate(mpi_n_bc_nod(itr-1),mpi_n_bc_val(itr-1))
			do i=1,itr-1
				mpi_n_bc_nod(i)=mpi_n_bc_nod_es(i)
				mpi_n_bc_val(i)=mpi_n_bc_val_es(i)
			enddo
		endif
	endif
	!------------------------------------------------------------	
	close(file_id)
	
	
	!------ output mpi node boundary numbers ---------
!	write(outfile,'("mpi_bcc", i6.6, ".txt")') my_rank+1
!	open(20,file=outfile)
!	write(20,*)"Dirichlet B.C. ::"
!	do i=1,size(mpi_d_bc_nod)
!		write(20,*)mpi_d_bc_nod(i),mpi_d_bc_val(i)
!	enddo
!	write(20,*)"Neumann B.C. ::"
!	do i=1,size(mpi_n_bc_nod)
!		write(20,*)mpi_n_bc_nod(i),mpi_n_bc_val(i)
!	enddo
!	
!	close(20)
	!-------------------------------------------------		
	
	
end subroutine mpi_read_bound_cond
!#######################################################################################
subroutine mpi_read_control_p(infile_control,my_rank,itr_max,tol)
	
	integer,intent(in)::my_rank
	integer,intent(out)::itr_max
	real(8),intent(out)::tol
	character*17,intent(in):: infile_control
	
	open(10,file=infile_control)
	read(10,*)itr_max,tol
	close(10)

end subroutine mpi_read_control_p
!#######################################################################################

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

end module MeshClass