program main
    use plantFEM
    implicit none

    integer(int32),parameter :: num_para = 1000

    type(MPI_) :: mpid
    type(IO_) :: f
    type(Soybean_) :: soy(num_para)
    integer(int32) :: i

    call mpid%start()
    call f%open("SoyVolume_rank_"//str(mpid%myrank)//".txt","w")
    do i=1,num_para
        call soy(i)%init(config="Tutorial/obj/realSoybeanConfig.json")
        call f%write(soy(i)%getVolume(leaf=.true.,stem=.true.))
        call f%flush()
    enddo
    call f%close()
    call mpid%end()

end program main