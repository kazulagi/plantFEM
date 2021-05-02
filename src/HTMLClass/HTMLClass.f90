module HTMLClass
    use IOClass
    implicit none

    ! show info as HTML file (index.html)
    type :: HTML_
        type(IO_) :: file
        character(len=10) :: mode = "text"! text or 3D
        character(len=200) :: title = "plantFEM"
        integer(int32) :: fh
    contains
        procedure, public :: init => initHTML
        procedure, public :: add => addHTML
        procedure, public :: show => showHTML
    end type
contains

subroutine initHTML(obj)
    class(HTML_),intent(inout) :: obj

    if(obj%mode == "text")then
        ! view text
        call obj%file%open("index.html",'w')
        call obj%file%write("<html>")
        
        ! ----------- header ----------- 
        call obj%file%write("<head>")
        call obj%file%write('<meta http-equiv="refresh" content="2; URL=">')
        call obj%file%write("<title>")
        call obj%file%write(obj%title)
        call obj%file%write("</title>")
        call obj%file%write("</head>")
        ! ----------- header -----------

        ! ----------- body -----------
        call obj%file%write("<body>")


    elseif(obj%mode == "3D")then
        ! vew 3D
        call obj%file%open("index.html",'w')
        call obj%file%write("<html>")
        
        ! ----------- header ----------- 
        call obj%file%write("<head>")

        call obj%file%write('<script src="https://threejs.org/build/three.js"></script> ')
        
        call obj%file%write("</head>")
        ! ----------- header -----------

        ! ----------- body -----------
        call obj%file%write("<body>")
        call obj%file%write("Sorry, now implementing.")
        !call obj%file%write('<script type="text/javascript" src="src/HTMLClass/plantfem.js"></script>')
        
    else
        print *, "The mode ",trim(obj%mode), " is not supported. (only text or 3D)"
    endif

end subroutine
!====================================================



!====================================================
subroutine addHTML(obj,content)
    class(HTML_) ,intent(inout) :: obj
    character(*) ,intent(in) :: content

    call obj%file%write(content)
    call obj%file%write("<br>")

end subroutine
!====================================================



!====================================================
subroutine showHTML(obj)
    class(HTML_) ,intent(inout) :: obj

    call obj%file%write("</body>")
    ! ----------- body -----------
    
    call obj%file%write("</html>")
    call obj%file%close()
end subroutine
!====================================================





end module HTMLClass