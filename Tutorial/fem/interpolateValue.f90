use FEMDomainClass
use LoggerClass
implicit none


type(FEMDomain_) :: cube
real(real64),allocatable :: Uz(:),your_position(:)

call cube%create("Cube3D")
call cube%resize(x=50.0d0,y=40.0d0,z=30.0d0)
Uz = linspace([1.0d0,dble(cube%nn())],cube%nn() )

! position :: xyz >> 
your_position = [30.0d0,30.0d0,30.0d0]

! interpolated value
do i_i=1,10
    print *, "getValue", cube%getValue(scalar_field=Uz,position=your_position)
    your_position = your_position - 1.0d0
enddo

end