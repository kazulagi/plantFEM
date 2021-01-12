program main
  use plantFEM
  implicit none

  type(Soybean_) :: soy(1)
  type(Light_) :: sun
  type(Air_) :: air
  integer(int32) :: i,j

  call sun%init()
  call air%init()

  do i=1,1
    call soy(i)%init()
    !call soy(i)%gmsh("test"//trim(str(i))//"_000sec")
    call soy(i)%leaf(1)%rotate(x=radian(40),y=radian(40),z=radian(40) )
    call soy(i)%leaf(2)%rotate(x=radian(-40),y=radian(-40),z=radian(-40) )
    print *, "Growth start!"
    do j=1,3
        print *, "day ", j
        call soy(i)%grow(dt=60.0d0*60.0d0*8.0d0, light=sun, air=air)
        call soy(i)%leaf(1)%femdomain%ply(name="test2")
        call soy(i)%gmsh("test"//trim(str(i))//"_100sec")
    enddo
  enddo

end program main
