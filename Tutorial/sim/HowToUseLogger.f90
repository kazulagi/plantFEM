program main
  use LoggerClass
  use fem
  implicit none

  type(FEMDomain_) :: cube
  type(Logger_) :: Bob
  type(Math_) :: math
  integer(int32) :: i

  ! create an object
  call cube%create("Cube3D")

  ! set channels
  call Bob%set("Node3_x",cube%mesh%nodcoord(3,1))
  call Bob%set("Node12_y",cube%mesh%nodcoord(12,2))
  call Bob%set("Node9_z",cube%mesh%nodcoord(3,3))
  
  ! start logger
  call Bob%start()
  do i=1,100
    call cube%move(x=sin(dble(i)/2.0d0/Math%PI ) )
    call cube%rotate(x=sin(dble(i)/2.0d0/Math%PI ) )
    call Bob%save()
  enddo
  call Bob%reset()
  do i=1,100
    call cube%rotate(x=sin(dble(i)/2.0d0/Math%PI ) )
    call Bob%save()
  enddo
end program main