module WebserverClass
    use TimeClass
    use TermClass
    use MathClass
    use PhysicsClass
    use IOClass
    use KinematicClass
    use RandomClass
    use ArrayClass
    use VertexClass
    use GraphClass
    use CSVClass
    use VectorClass
    use EquationClass
    !use MPIClass
    use DictionaryClass
    use OpenMPClass
    use LinearSolverClass
    use GeometryClass
    use TreeClass
    use ShapeFunctionClass
    use RouteOptimization
    use STLClass
    implicit none

    type :: Webserver_
        character(:),allocatable :: lang!="en"
        character(:),allocatable :: charset!="utf-8"
        character(:),allocatable :: Title!="untitled"
        character(:),allocatable :: FileName!="index.html"
        character(:),allocatable :: body!="<h1>Hello! from plantFEM</h1>"
        character(:),allocatable :: arrayTitleX! = "Title X"
        character(:),allocatable :: arrayTitleY! = "Title Y"
        character(:),allocatable :: vectorTitleX! = "Title X"
        character(:),allocatable :: vectorTitleY! = "Title Y"
        integer(int32) :: port = 1234
        real(real64),allocatable :: array(:,:)
        real(real64),allocatable :: vector(:)
        logical :: arrayExists=.False.
        logical :: vectorExists=.False.
    contains
        procedure, public :: init => initWebserver
        procedure, public :: update => updateWebserver
        !procedure, public :: end => endWebserver
    end type
contains

subroutine initWebserver(obj)
    class(Webserver_),intent(inout) :: obj
    obj% lang="en"
    obj% charset="utf-8"
    obj% Title="untitled"
    obj% FileName="index.html"
    obj% body="<h1>Hello! from plantFEM</h1>"
    obj% arrayTitleX = "Title X"
    obj% arrayTitleY = "Title Y"
    obj% vectorTitleX = "Title X"
    obj% vectorTitleY = "Title Y"
    
    call execute_command_line("python3 -m http.server "//str(obj%port)//" &")

end subroutine

subroutine updateWebserver(obj)
    class(Webserver_),intent(inout) :: obj
    type(IO_) :: f
    integer(int32) :: line,i,col,j

    call f%open(obj%FileName )
    call f%write("<!DOCTYPE html>")
    call f%write('<html lang="'//obj%lang//'">')
    call f%write('<head>')
    !call f%write('<meta http-equiv="refresh" content="2; URL=">')
    call f%write('<meta charset="'//obj%charset//'">')
    call f%write('<title>')
    call f%write(obj%title)
    call f%write('</title>')
    call f%write('</head>')
    call f%write('<body>')
    call f%write(obj%body)
    if(allocated(obj%array) )then
        line=size(obj%array,1)
        col=size(obj%array,2)
        call f%write('<table border="1">')
        call f%write('<tr>')
        !call f%write('<td rowspan="1">')
        !call f%write(" ") 
        !call f%write('</td>')
        call f%write('<td rowspan="'//str(line+1)//'">')
        call f%write(obj%arrayTitleY) 
        call f%write('</td>')

        !call f%write('<th>')
        !call f%write(' ')
        !call f%write('</th>')
        call f%write('<th colspan="'//str(col)//'">')
        call f%write(obj%arrayTitleX )
        call f%write('</th>')
        call f%write('</tr>')
        do i=1,line
            call f%write('<tr>')
            do j=1,col
                call f%write('<th>')
                call f%write(str(obj%array(i,j) ) )
                call f%write('</th>')
            enddo
            call f%write('</tr>')
        enddo
        call f%write('</table>')
    endif
    call f%write('</body>')
    call f%write('</html>')
    call f%close()

end subroutine


!subroutine endWebserver(obj)
!    class(Webserver_),intent(inout) :: obj
!
!    call execute_command_line("python3 -m http.server "//str(obj%port)//" &")
!
!end subroutine

end module