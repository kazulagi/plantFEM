use plantfem

implicit none

integer, parameter :: np = 10
type(Soybean_) :: soy(np,np)
real(real64) :: biomass(np,np)
integer(int32) :: i,j,k

!$OMP parallel do private(i,j)
do i=1,np
  do j=1,np
    call soy(i,j)%init(config="Tutorial/obj/realSoybeanConfig_mini.json") 
    do k=1,2
        call soy(i,j)%grow(dt=1.0d0,simple=.true.)
    enddo
    biomass(i,j) = soy(i,j)%getBiomass()
    call soy(i,j)%move(x=dble(i-1)*0.40,y=dble(j-1)*0.14) 
    call soy(i,j)%stl(name="soy_"//str(i)//"_"//str(j) )
  enddo
enddo
!$OMP end parallel do

!call soil%create(x_num=3,y_num=3,z_num=1)
!call soil%resize(x=3.0d0, y=3.0d0, z=1.0d0)
!call soil%msh(name="soil")
call print(biomass)
!writing soy_2_2_leaf18_000001.stl step>>           1
!1.2474712525392341E-003   1.1998353796776593E-003
!1.2671755205095057E-003   1.2480182046246107E-003
!0.0049625004
call print(sum(biomass))

end