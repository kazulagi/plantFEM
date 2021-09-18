program main
    use plantFEM
    implicit none

    type(Soybean_),allocatable :: soy(:,:)
    type(Soil_) ::soil
    type(MPI_) :: mpid
    character(:),allocatable :: filename
    integer(int32):: i,j

    call mpid%start()
    i = mpid%myrank+1
    allocate(soy(mpid%petot,20) )
    do j=1,20
      call soy(i,j)%init(config="Tutorial/obj/realSoybeanConfig.json") 
      !call soy%stl(name="soy")
      filename = "soy_" // trim(str(i))//"_"//trim(str(j))
      call soy(i,j)%move(x=dble(i-1)*0.750d0,y=dble(j-1)*0.15d0 )
      call soy(i,j)%msh(name=filename)
    enddo

    !call soy%json(name="soy")

    call soil%create(x_num=3,y_num=3,z_num=1)
    call soil%resize(x=3.0d0, y=3.0d0, z=1.0d0)
    call soil%msh(name="soil")
    call mpid%end()
    
end program main
