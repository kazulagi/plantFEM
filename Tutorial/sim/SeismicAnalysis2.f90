use LoggerClass
use SeismicAnalysisClass
implicit none

type(SeismicAnalysis_) :: seismic
type(FEMDomain_),target :: cube
type(IO_) :: f
type(Random_) :: random
type(Logger_) :: logger

real(real64),allocatable :: disp_z(:,:),accel(:)
real(real64) :: wave(200*20,2),T,Duration,dt
real(real64),allocatable :: period(:)
integer(int32) :: i,j,cases,stack_id,num_of_cases

! create Domain
call cube%create(meshtype="Cube3D",x_num=20,y_num=20,z_num=20)
call cube%resize(x=200.0d0,y=200.0d0,z=50.0d0)
call cube%move(z=-50.0d0)

! create & check wave
Duration = 20.0d0
dt = Duration/dble(size(wave,1))!/10.0d0
wave(:,:) = 0.0d0
do i=1,size(wave,1)
    wave(i,1) = dt*dble(i)
    if(i<10 .or. i> size(wave,1)/2 )cycle
    wave(i,2) = random%gauss(mu=0.0d0,sigma=1.0d0)
enddo
call f%open("input_wave.txt" )
call f%write(wave )
call f%close()

! set domain
seismic%femdomain => cube
! set wave
seismic%wave = wave
seismic%dt = dt

! setting up simulator
call seismic%init()

! absorbing boundary
seismic%boundary_dumping_ratio = 1000.0d0
call seismic%absorbingBoundary(x_max =   0.0d0,direction="x")
call seismic%absorbingBoundary(x_min = 200.0d0,direction="x")
call seismic%absorbingBoundary(y_max =   0.0d0,direction="y")
call seismic%absorbingBoundary(y_min = 200.0d0,direction="y")
call seismic%absorbingBoundary(z_max = -50.0d0,direction="z")

! export initial configuration
call seismic%femdomain%vtk("mesh_init_T"+str(T) )
! load wave
call seismic%loadWave(&
    x_min=0.0d0,x_max=15.0d0,&
    y_min=0.0d0,y_max=15.0d0,&
    z_max=-50.0d0+3.0d0,direction="x",wavetype=WAVE_ACCEL)
! set Material Parameter
seismic%Density(:)      = 17000.0d0 !(N/m/m/m)
seismic%YoungModulus(:) = 25372853.0 !(N/m/m) Vs=121 m/s

seismic%YoungModulus = cube%getScalarField(&
    xr=[-10000.0d0,10000.0d0], &
    yr=[-10000.0d0,10000.0d0], &
    zr=[-10000.0d0,-20.0d0], &
    default=seismic%YoungModulus,&
    entryvalue= 2495520000.0d0) !(N/m/m) Vs1200 m/s)
seismic%PoissonRatio(:) = 0.330d0 
! initialize values
seismic%a = 0.0d0
seismic%v = 0.0d0
seismic%u = 0.0d0
! time loop
do i=1,size(wave,1)
    ! update config
    call seismic%run(timestep=[i,i+1])
    ! export results
    accel = zeros(seismic%femdomain%nn())
    do j=1,seismic%femdomain%nn()
        accel(j) = seismic%a( (j-1)*3 + 1  )! x-direction
    enddo
    call seismic%femdomain%vtk("result_ax_step_"+str(i),scalar=accel )
    do j=1,seismic%femdomain%nn()
        accel(j) = seismic%a( (j-1)*3 + 2  )! y-direction
    enddo
    call seismic%femdomain%vtk("result_ay_step_"+str(i),scalar=accel )
    do j=1,seismic%femdomain%nn()
        accel(j) = seismic%a( (j-1)*3 + 3  )! z-direction
    enddo
    call seismic%femdomain%vtk("result_az_step_"+str(i),scalar=accel )
enddo
! end
call cube%remove()
call seismic%remove()
end