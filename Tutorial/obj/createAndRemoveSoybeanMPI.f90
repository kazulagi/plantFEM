program main
    use SoybeanClass
    implicit none

    integer(int32),parameter :: num_para = 10

    type(MPI_) :: mpid
    type(IO_) :: f
    type(Soybean_) :: soy
    integer(int32) :: i

    call mpid%start()
    call f%open("SoyVolume_rank_"//str(mpid%myrank)//".txt","w")
    do i=1,num_para
        call soy%init(config="Tutorial/obj/realSoybeanConfig.json")
        call f%write(soy%getVolume(leaf=.true.,stem=.true.))
        call f%flush()
        call soy%remove()
    enddo
    call f%close()
    call mpid%end()

end program main