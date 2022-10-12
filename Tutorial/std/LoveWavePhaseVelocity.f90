use IOClass
use ElasticityClass
implicit none

type(Elasticity_) :: elast
type(IO_) :: f
real(real64),allocatable :: phase_velocity(:,:)
integer(int32) :: Mode

print *, "Order of mode (fundamental = 0) :: "
read(*,*) Mode

call f%open("Love_Phase_Velocity_"+zfill(Mode,4)+".txt","w")
phase_velocity = elast%to_LoveWavePhaseVelocity(&
    Density = [1.90d0, 1.80d0],&
    Vs = [250.0d0, 150.0d0], &
    H = 19.80d0, &
    division = 50, &
    Mode = Mode)
call f%write( phase_velocity  )
call f%close()
call f%plot(option = "with lines")

end