program main
    use plantFEM
    implicit none

    type(IO_) :: f
    real(real64)::oarray(2,2)
    real(real64)::vector(2)
    integer(int32)::iarray(2,2)

    oarray(:,:) = 1.0d0
    iarray(:,:) = 10
    vector(:)=123

    call f%open("test.json")
    write(f%fh,*) "{"
    call json(array=vector,fh=f%fh,name="vec1")
    call json(array=oarray,fh=f%fh,name="obj1")
    call json(array=iarray,fh=f%fh,name="obj2",endl=.true.)
    write(f%fh,*) "}"
    call f%close()


end program main