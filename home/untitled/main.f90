program main
    use std
    implicit none
   
    type(IO_) :: f
   
    call f%open("./","hello",".csv")
    call f%write("Hello, world")
    call f%write(str(100)//"hello")
    call f%write(trim(str(100))//"hello")
    write(f%fh,*) 100.0d0
    call f%close()
   
end program main