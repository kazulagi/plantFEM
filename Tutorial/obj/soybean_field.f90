program main
    use plantFEM
    implicit none

    type(MPI_) :: mpid
    type(Soybean_) :: soy(10,3)
    integer(int32) :: i, j
    real(real64) :: x, y

    call mpid%start()
    do j=1,3
        i=mpid%myrank+1
        call soy(i,j)%init(config="Tutorial/obj/realSoybeanConfig.json") 
        x = dble(i-1)*0.200d0
        y = dble(j-1)*0.650d0
        call soy(i,j)%move(x=x,y=y )
        call soy(i,j)%msh(name="soy_"//trim(str(i))//"_"//trim(str(j)))
    enddo
    call mpid%end()
end program main