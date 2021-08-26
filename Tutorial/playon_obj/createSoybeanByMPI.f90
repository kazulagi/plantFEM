program main
    use plantFEM
    implicit none

    type(MPI_) :: mpid
    type(IO_) :: f
    type(Soybean_) :: soy
    integer(int32) :: i

    call mpid%start()
    call f%open("SoyVolume_rank_"//str(mpid%myrank)//".txt","w")
    do i=1,1000
        call soy%init(config="Tutorial/playon_obj/realSoybeanConfig.json")
        call f%write(soy%getVolume(leaf=.true.,stem=.true.))
    enddo
    call f%close()
    call mpid%end()

end program main