! simple growth model
use SoybeanClass
implicit none

type(Soybean_)   :: soy
integer(int32)   :: i

call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 

! Loop over timesteps
do i=1,int(day(unit="hour") )
    ! Write your algorithm @here
enddo

end