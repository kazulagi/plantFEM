use plantfem
implicit none


type(IO_) :: f
type(String_) :: filename
integer(int32) :: hour, min, sec

! create sequential files
do hour=0,12
    do min=0,59,10 ! every 10 min
        do sec=0,59,10 ! every 10 sec
            filename = zfill(hour,2)//"_"//zfill(min,2)//"_"//zfill(sec,2)//".txt"
            call print(filename)

            call f%open(filename,"w")
                call f%write(hour)
                call f%write(min)
                call f%write(sec)
            call f%close()
        enddo
    enddo
enddo

end