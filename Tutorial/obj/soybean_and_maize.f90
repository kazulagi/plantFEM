use SoybeanClass
use MaizeClass
implicit none

integer(int32),parameter :: n = 5
integer(int32),parameter :: m = 15

type(soybean_) :: soybean

type(Maize_) :: maize

do i_i=1,n
    do j_j=1,m
        call soybean%create(config="Tutorial/obj/soy.json")
        call soybean%remove(root=.true.)
        call soybean%move(y=(2*i_i)*0.75d0,x=(j_j-1)*0.15d0 )
        call soybean%vtk("soybean"+zfill(i_i,5)+"_"+zfill(j_j,5),single_file=.true.)
        call soybean%remove()
    enddo
enddo

do i_i=1,n
    do j_j=1,m
        call maize%create(config="Tutorial/obj/maize.json")
        call maize%move(y=(2*i_i-1)*0.75d0,x=(j_j-1)*0.15d0 )
        call maize%vtk("maize"+zfill(i_i,5)+"_"+zfill(j_j,5),single_file=.true.)
        call maize%remove()
    enddo
enddo



end