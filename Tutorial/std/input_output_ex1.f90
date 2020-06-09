program main  
        use std
        implicit none 
        
        type(MPI_) :: mpid
        type(IO_) :: f

        call mpid%start()
        
        call f%open("../","hello"//trim(str(mpid%Myrank)),".txt" )
        call f%write("Hello from", mpid%Myrank)
        call f%close()

        call mpid%end()
       
end program
        
