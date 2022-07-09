use FEMDomainClass
use LoggerClass
implicit none


type(FEMDomain_) :: cube
type(Logger_)    :: logger
real(real64),allocatable :: Uz(:),your_logger_position(:)

call cube%create("Cube3D")
call cube%resize(x=50.0d0,y=40.0d0,z=30.0d0)
Uz = linspace([1.0d0,dble(cube%nn())],cube%nn() )

! logger position :: xyz >> 
your_logger_position = [30.0d0,30.0d0,30.0d0]

call Logger%set(femdomain=cube,position=your_logger_position,dataset=Uz,name="Uz")
call logger%vtk("logger")

call logger%start()

call logger%save(t=dble(i_i) )
call cube%vtk("Uz",scalar=Uz)

end