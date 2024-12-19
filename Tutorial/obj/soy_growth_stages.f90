use SoybeanClass
implicit none

type(Soybean_) :: soy(13)
type(Random_) :: random
type(Math_) :: math

call soy(1)%init(radius=[0.60d0,0.70d0,0.50d0]/100.0d0,division=[10,10,10])
call soy(1)%move(x = dble(1)*20.0d0/100.0d0)
call soy(1)%vtk("soy_"+zfill(1,4),single_file=True)

do i_i=2,size(soy)
    call soy(i_i)%init(config="soy_"+str(i_i)+".json")
    call soy(i_i)%move(x = dble(i_i)*20.0d0/100.0d0)
    call soy(i_i)%vtk("soy_"+zfill(i_i,4),single_file=True)
enddo

end