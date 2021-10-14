character*70 DefaultFolderName
character*70 FolderName
character*76 command_mkdir
character*86 surfaceout
integer i,j,node_ID,node_ID_next,k

    
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
command_mkdir="mkdir " // trim(FolderName)
command_mkdir=trim(command_mkdir)

call execute_command_line(command_mkdir)
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
