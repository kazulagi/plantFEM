program main
    use plantFEM
    implicit none

    type(Soybean_) :: soy(5)
    type(MPI_)     :: mpid
    integer(int32) :: i,j

    call mpid%start()
    i = mpid%myrank + 1
    do j=1,5
        call soy(j)%init("soyconfig")
        ! 条間75cm, 株間15cm
        call soy(j)%move(x=dble(i-1)*0.750d0,y=dble(j-1)*0.150d0 )
        ! 描画
        call soy(j)%gmsh("soy"//trim(str(i))//"_"//trim(str(j)))
    enddo
    call mpid%end()

end program main