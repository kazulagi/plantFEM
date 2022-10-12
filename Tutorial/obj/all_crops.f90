use RiceClass
use SoybeanClass
use MaizeClass
implicit none


type(Rice_) :: rice(15)
type(Soybean_) :: soybean
type(Maize_)   :: maize
type(Random_) :: random
type(Math_)   :: math

do i_i=1,15
    call rice(i_i)%create("Tutorial/obj/rice.json")
    call rice(i_i)%move(&
        x=random%gauss(mu=0.0d0,sigma=0.010d0),&
        y=random%gauss(mu=0.0d0,sigma=0.010d0) )
    call rice(i_i)%rotate(&
        z=math%pi*random%random())
    call rice(i_i)%vtk("rice"+zfill(i_i,5),single_file=.true.)
enddo

call soybean%create("Tutorial/obj/soy.json")
call soybean%move(x=0.50d0)
call soybean%vtk("soybean",single_file=.true.)


call maize%create("Tutorial/obj/maize.json")
call maize%move(x=-0.50d0)
call maize%vtk("maize",single_file=.true.)

end