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

        integer(int32) :: NumOfVertex=0
        
    contains
        procedure, public :: add => addGraph ! add vertex or edge
        procedure, public :: update => updateGraph ! update vertex or edge
        procedure, public :: show => showGraph
    end type
contains

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
                command = "set arrow "//trim(str(id))//" from "//str(obj%vertex(i)%x)//","//str(obj%vertex(i)%y)//" to "&
                //str(obj%vertex(j)%x)//","//str(obj%vertex(j)%y)
                call f%write(trim(command) )
                
            elseif(obj%AdjacencyMatrix (i,j) < 0)then
                id=id+1
                command = "set arrow "//trim(str(id))//" from "//str(obj%vertex(j)%x)//","//str(obj%vertex(j)%y)//" to "&
                //str(obj%vertex(i)%x)//","//str(obj%vertex(i)%y)
                call f%write(trim(command) )
            else
                cycle
            endif
        enddo
    enddo
    call f%write("plot './vertex.txt'")
    call f%write("pause -1")
    call f%close()

    call system("gnuplot ./showGraph.gp")
end subroutine
! ######################################

end module GraphClass