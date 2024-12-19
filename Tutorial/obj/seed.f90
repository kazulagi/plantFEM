use SoybeanClass
implicit none

type(Soybean_),allocatable :: soy(:,:)
type(Random_) :: random
type(Math_) :: math

allocate(soy(10,100))
do i_i=1,7
    do j_j=1,20
        call soy(i_i,j_j)%init(radius=[0.60d0,0.70d0,0.50d0]/100.0d0,division=[10,10,10])
        call soy(i_i,j_j)%move(x=0.70d0*(i_i-1),y=0.050d0*(j_j-1))
        call soy(i_i,j_j)%rotate(x=2*math%PI*random%random(),y=2*math%PI*random%random(),z=2*math%PI*random%random())
        call soy(i_i,j_j)%vtk("soy_seed"+zfill(i_i,4)+"_"+zfill(j_j,4),single_file=True)
    enddo
enddo

end