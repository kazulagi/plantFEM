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
        character(5) :: lang="en"
        character(5) :: charset="utf-8"
        character(200) :: Title="untitled"
        character(200) :: FileName="index.html"
        character(200) :: body="<h1>Hello! from plantFEM</h1>"
        character(200) :: arrayTitleX = "Title X"
        character(200) :: arrayTitleY = "Title Y"
        character(200) :: vectorTitleX = "Title X"
        character(200) :: vectorTitleY = "Title Y"
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

    call execute_command_line("python3 -m http.server "//trim(str(obj%port))//" &")

end subroutine

subroutine updateWebserver(obj)
    class(Webserver_),intent(inout) :: obj
    type(IO_) :: f
    integer(int32) :: line,i,col,j

    call f%open(trim(obj%FileName) )
    call f%write("<!DOCTYPE html>")
    call f%write('<html lang="'//trim(obj%lang)//'">')
    call f%write('<head>')
    !call f%write('<meta http-equiv="refresh" content="2; URL=">')
    call f%write('<meta charset="'//trim(obj%charset)//'">')
    call f%write('<title>')
    call f%write(trim(obj%title))
    call f%write('</title>')
    call f%write('</head>')
    call f%write('<body>')
    call f%write(trim(obj%body))
    if(allocated(obj%array) )then
        line=size(obj%array,1)
        col=size(obj%array,2)
        call f%write('<table border="1">')
        call f%write('<tr>')
        !call f%write('<td rowspan="1">')
        !call f%write(" ") 
        !call f%write('</td>')
        call f%write('<td rowspan="'//str(line+1)//'">')
        call f%write(trim(obj%arrayTitleY )) 
        call f%write('</td>')

        !call f%write('<th>')
        !call f%write(' ')
        !call f%write('</th>')
        call f%write('<th colspan="'//str(col)//'">')
        call f%write(trim(obj%arrayTitleX) )
        call f%write('</th>')
        call f%write('</tr>')
        do i=1,line
            call f%write('<tr>')
            do j=1,col
                call f%write('<th>')
                call f%write(trim(str(obj%array(i,j) )) )
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
!    call execute_command_line("python3 -m http.server "//trim(str(obj%port))//" &")
!
!end subroutine

end module