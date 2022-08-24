use RiceClass

implicit none


type(Rice_) :: rice(15)
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

end