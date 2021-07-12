
subroutine hello()
    implicit none

    print *, "Hello NVIDIA HPC SDK"

    call sub1<<<3,3>>>()

end subroutine hello