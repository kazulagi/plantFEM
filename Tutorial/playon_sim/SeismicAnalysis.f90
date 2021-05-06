use sim
implicit none

type(SeismicAnalysis_) :: seismic
type(FEMDomain_),target :: cube
real(real64) :: wave(100,2)
integer(int32) :: i

! create Domain
call cube%create(meshtype="Cube3D")

! create Wave
do i=1,size(wave,1)
    wave(i,1) = dble(i-1)
    wave(i,2) = sin(radian(10.0d0*dble(i-1)))
enddo

! set domain
seismic%femdomain => cube
! set wave
seismic%wave = wave

! run simulation
call seismic%run(dt=0.010d0,timestep=10)

end