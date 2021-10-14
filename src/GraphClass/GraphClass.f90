module GraphClass
    use VertexClass
    use IOClass
    use MathClass
    use RandomClass
    implicit none


    type :: Graph_
        ! Group of G(V, E),
        ! where V is vertex, E is edge

        ! adjacency matrix
        integer(int32),allocatable :: AdjacencyMatrix(:,:)

        ! vertex info
        type(Vertex_),allocatable :: Vertex(:)

        ! global info
        integer(int32),allocatable :: Global_ID(:)

        integer(int32) :: NumOfVertex=0
        
    contains
        procedure, public :: add => addGraph ! add vertex or edge
        procedure, public :: update => updateGraph ! update vertex or edge
        procedure, public :: show => showGraph
        procedure, public :: remove => removeGraph
        procedure, public :: sync => syncGraph
    end type
contains

! ######################################
subroutine removeGraph(obj,onlyVertex)
    class(Graph_),intent(inout) :: obj
    logical,optional,intent(in) :: onlyVertex

    deallocate(obj%vertex)

    if(present(onlyVertex) )then
        if(onlyVertex.eqv. .true.)then
            return
        endif
    endif

    deallocate(obj%AdjacencyMatrix)
    obj%NumOfVertex=0

end subroutine
! ######################################

! ######################################
subroutine addGraph(obj,vertex,from, to,between,and)
    class(Graph_),intent(inout) :: obj
    type(Vertex_),optional,intent(inout) :: vertex
    type(Vertex_),allocatable :: vlist(:)
    integer(int32),optional,intent(in) :: from, to,between,and
    integer(int32) :: i

    
    if(present(vertex) )then
        obj%NumOfVertex=obj%NumOfVertex+1
        vertex%ID = obj%NumOfVertex
    endif

    if(present(vertex) )then
        if(.not. allocated(obj%Vertex) )then
            allocate(obj%Vertex(1))
            obj%Vertex(1) = vertex%copy()
            if(allocated(obj%AdjacencyMatrix) ) deallocate(obj%AdjacencyMatrix)
            allocate(obj%AdjacencyMatrix(1,1) )
            obj%AdjacencyMatrix(1,1)=0
        else
            allocate(vlist(size(obj%Vertex) ) )
            do i=1,size(obj%Vertex,1 )
                vlist(i) = obj%Vertex(i)%copy()
            enddo
            deallocate(obj%Vertex)
            allocate(obj%Vertex(size(vlist,1)+1 )  )
            do i=1,size(vlist,1)
                obj%Vertex(i) = vlist(i)%copy()
            enddo
            obj%Vertex( size(vlist,1) + 1 ) = vertex%copy()
            call extend(mat=obj%AdjacencyMatrix,extend1stColumn=.true.,DefaultValue=0)
            call extend(mat=obj%AdjacencyMatrix,extend2ndColumn=.true.,DefaultValue=0)
        endif
    endif

    if(present(from) .and. present(to) )then
        obj%AdjacencyMatrix(from, to) =  1
        obj%AdjacencyMatrix(to, from) = -1
    endif

    if(present(between) .and. present(and) )then
        obj%AdjacencyMatrix(between, and) =  1
        obj%AdjacencyMatrix(and, between) =  1
    endif


end subroutine
! ######################################


! ######################################
subroutine updateGraph(obj,ID,vertex,from, to,between,and)
    class(Graph_),intent(inout) :: obj
    type(Vertex_),optional,intent(inout) :: vertex
    type(Vertex_),allocatable :: vlist(:)
    integer(int32),intent(in) :: ID
    integer(int32),optional,intent(in) :: from, to, between,and
    integer(int32) :: i

    if(ID > size(obj%vertex) )then
        print *, "ERROR :: updateGraph >> please add vertex before update."
        stop
    else
        obj%Vertex(ID) = vertex%copy()
    endif

    
    if(present(from) .and. present(to) )then
        obj%AdjacencyMatrix(from, to) =  1
        obj%AdjacencyMatrix(to, from) = -1
    endif

    if(present(between) .and. present(and) )then
        obj%AdjacencyMatrix(between, and) =  1
        obj%AdjacencyMatrix(and, between) =  1
    endif

end subroutine
! ######################################


! ######################################
subroutine showGraph(obj,withname)
    class(Graph_),intent(in) :: obj
    logical,optional,intent(in)::withname
    type(IO_) :: f
    character(200) :: command
    
    integer(int32) :: id,n,i,j

    n=size(obj%vertex)
    call f%open("./","vertex",".txt")
    do i=1,n
        call f%write(str(obj%vertex(i)%x)//" "//str(obj%vertex(i)%y)//" "//str(obj%vertex(i)%z)  )
    enddo
    call f%close()

    call f%open("./","showGraph",".gp")
    

    if(present(withname) )then
        if(withname .eqv. .False.)then
            do i=1,n
                command = "set label 'vertex:"//trim(str(i))//"' at "//str(obj%vertex(i)%x)//","//str(obj%vertex(i)%y)
                call f%write(trim(command) )        
            enddo
        else
            do i=1,n
                command = "set label 'ID:"//trim(str(i))//" Name: "//trim(obj%vertex(i)%name)&
                //"' at "//str(obj%vertex(i)%x)//","//str(obj%vertex(i)%y)
                call f%write(trim(command) )        
            enddo
        endif
    else
        do i=1,n
            command = "set label 'ID:"//trim(str(i))//" Name: "//trim(obj%vertex(i)%name)&
            //"' at "//str(obj%vertex(i)%x)//","//str(obj%vertex(i)%y)
            call f%write(trim(command) )        
        enddo
    endif

    id=0
    do i=1,n
        do j=1,n
            if(obj%AdjacencyMatrix (i,j) > 0)then
                id=id+1
                command = "set arrow "//trim(str(id))//" head filled from "&
                //str(obj%vertex(i)%x)//","//str(obj%vertex(i)%y)//" to "&
                //str(obj%vertex(j)%x)//","//str(obj%vertex(j)%y)
                call f%write(trim(command) )
                
            elseif(obj%AdjacencyMatrix (i,j) < 0)then
                id=id+1
                command = "set arrow "//trim(str(id))//" head filled from "&
                //str(obj%vertex(j)%x)//","//str(obj%vertex(j)%y)//" to "&
                //str(obj%vertex(i)%x)//","//str(obj%vertex(i)%y)
                call f%write(trim(command) )
            else
                cycle
            endif
        enddo
    enddo
    call f%write("unset key")
    call f%write("plot './vertex.txt'")
    call f%write("pause -1")
    call f%close()

    call execute_command_line("gnuplot ./showGraph.gp")
end subroutine
! ######################################


! ######################################
subroutine syncGraph(obj,AdjacencyMatrix)
    class(Graph_),intent(inout) :: obj
    integer(int32),intent(in)::AdjacencyMatrix(:,:)
    integer(int32) :: i,j,buf(2)

    do i=1,size(AdjacencyMatrix,1)
        do j=1,size(AdjacencyMatrix,2)
            if(AdjacencyMatrix(i,j) == 0)then
                cycle
            endif

            if(AdjacencyMatrix(i,j)*obj%AdjacencyMatrix(i,j) < 0 )then
                obj%AdjacencyMatrix(i,j) = 1
                obj%AdjacencyMatrix(j,i) = 1
            endif
            if(AdjacencyMatrix(i,j)*obj%AdjacencyMatrix(i,j) > 0 )then
                cycle
            endif
            if(AdjacencyMatrix(i,j)*obj%AdjacencyMatrix(i,j) == 0 )then
                obj%AdjacencyMatrix(i,j) = obj%AdjacencyMatrix(i,j) + AdjacencyMatrix(i,j)
            endif
            
        enddo
    enddo

end subroutine
! ######################################


end module GraphClass