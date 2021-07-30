use sim
implicit none

type(SeismicAnalysis_) :: seismic(100)
type(FEMDomain_),target :: cube,original
type(IO_) :: f,response
type(Math_) :: math
real(real64),allocatable :: disp_z(:,:)
real(real64) :: wave(1000,2),T,Duration,dt
integer(int32) :: i,j,cases


call response%open("T_A.txt")
! create Domain
call cube%create(meshtype="Cube3D",x_num=2,y_num=2,z_num=20)
call cube%resize(x=1.0d0,y=1.0d0,z=5.0d0)
call cube%move(z=-5.0d0)

! Change T = 0.1, 0.2, 0.3 ... 1 (sec.)
T = 0.000d0

do cases=1,100
    ! create Wave
    T = T + 0.0500d0 ! sec.
    Duration = T * 10.0d0 ! sec.
    dt = Duration/dble(size(wave,1))
    
    wave(:,:) = 0.0d0
    do i=1,size(wave,1)
        wave(i,1) = dt*dble(i-1)
        wave(i,2) = sin(2.0d0*math%pi/T*wave(i,1) )
    enddo

    original = cube

    call f%open("wave.txt","w")
    call f%write(wave)
    call f%close()
    call f%plot("wave.txt","w l")


    ! set domain
    seismic(cases)%femdomain => cube
    ! set wave
    seismic(cases)%wave = wave

    ! run simulation
    call seismic(cases)%init()
    call seismic(cases)%fixDisplacement(z_max = -4.99d0,direction="x")
    call seismic(cases)%fixDisplacement(z_max = -4.99d0,direction="y")
    call seismic(cases)%fixDisplacement(z_max = -4.99d0,direction="z")

    call seismic(cases)%fixDisplacement(y_max = 0.0d0,direction="y")
    call seismic(cases)%fixDisplacement(y_min = 1.0d0,direction="y")

    call seismic(cases)%fixDisplacement(direction="z")

    call seismic(cases)%femdomain%vtk("mesh")

    call seismic(cases)%loadWave(z_max=-4.50d0,direction="x",wavetype=WAVE_ACCEL)

    call seismic(cases)%run(timestep=999,AccelLimit=10.0d0**8)

    print *, maxval(seismic(cases)%U)

    seismic(cases)%femdomain%mesh%nodcoord = seismic(cases)%femdomain%mesh%nodcoord + (10.0d0**6)*&
        reshape(seismic(cases)%U, seismic(cases)%femdomain%nn(),seismic(cases)%femdomain%nd() )
    call seismic(cases)%femdomain%vtk("x10")

    call response%write(T,seismic(cases)%maxA(1) )
    call response%flush()

enddo


end