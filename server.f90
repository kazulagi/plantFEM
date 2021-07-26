use sim
implicit none

type(SeismicAnalysis_) :: seismic
type(FEMDomain_),target :: cube
real(real64) :: wave(100,2)
integer(int32) :: i

! create Domain
call cube%create(meshtype="Cube3D",z_num=30)
call cube%resize(z=30.0d0)

! create Wave
do i=1,size(wave,1)
    wave(i,1) = dble(i-1)
    if(i<=5.0d0)then
        wave(i,2) = 10.0d0
    else
        wave(i,2) = 0.0d0
    endif
enddo


! set domain
seismic%femdomain => cube
! set wave
seismic%wave = wave

! run simulation
call seismic%init()
call seismic%loadWave(z_max=1.0d0,direction="x",wavetype=WAVE_DISP)
call seismic%run(timestep=20)

end