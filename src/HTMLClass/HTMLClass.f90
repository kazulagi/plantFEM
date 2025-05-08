module HTMLClass
   !! This module creates HTML file.
   use IOClass
   implicit none

   
   type :: HTML_
      !! It shows info as HTML file (index.html)
      
      type(IO_) :: file
      !> File instance
      character(len=10) :: mode = "text"! text or 3D
      !> Text file or 3D file
      character(len=200) :: title = "plantFEM"
      !> Title of the page
      integer(int32) :: fh
      !> file handle
   contains
      !> initialize the HTML file
      procedure, public :: init => initHTML
      !> Add content in the HTML file
      procedure, public :: add => addHTML
      !> show the HTML file
      procedure, public :: show => showHTML
   end type
contains

   subroutine initHTML(obj)
      class(HTML_), intent(inout) :: obj

      if (obj%mode == "text") then
         ! view text
         call obj%file%open("index.html", 'w')
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

      elseif (obj%mode == "3D") then
         ! vew 3D
         call obj%file%open("index.html", 'w')
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
         print *, "The mode ", obj%mode, " is not supported. (only text or 3D)"
      end if

   end subroutine
!====================================================

!====================================================
   subroutine addHTML(obj, content)
      class(HTML_), intent(inout) :: obj
      character(*), intent(in) :: content

      call obj%file%write(content)
      call obj%file%write("<br>")

   end subroutine
!====================================================

!====================================================
   subroutine showHTML(obj)
      class(HTML_), intent(inout) :: obj

      call obj%file%write("</body>")
      ! ----------- body -----------

      call obj%file%write("</html>")
      call obj%file%close()
   end subroutine
!====================================================

end module HTMLClass
