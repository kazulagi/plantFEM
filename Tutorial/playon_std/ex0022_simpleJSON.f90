program main
    use plantFEM
    implicit none

    type(IO_) :: file
    real(real64) :: t1,t2
    integer(int32) :: i

    call cpu_time(t1)
    call file%open("output.json")
    call file%write("{")
    call file%write("'msg':'good day',")
    call cpu_time(t2)
    do i=1,100
        call cpu_time(t2)
        call file%write("'cpu-time':"//str(t2-t1)//",")
    enddo
    call file%write("'return':0")
    call file%write("}")
    call file%close()

end program main

