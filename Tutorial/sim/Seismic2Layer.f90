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
integer(int32) :: i,j,channel_id
character(:),allocatable :: channel_name
integer(int32),allocatable :: logger_point_id(:)


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
seismic%YoungModulus(:) = 700000000.0d0 !(N/m/m) Vs=121 m/s


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


! Switch-ON data-logger
!(1) initialize with number of channels
call logger%init(MAX_CHANNEL_NUM=12)
logger_point_id = zeros(4)
logger_point_id(1) = cube%getNearestNodeID(x=10.0d0,y=10.0d0,z=  0.0d0)
logger_point_id(2) = cube%getNearestNodeID(x=10.0d0,y=10.0d0,z=-20.0d0)
logger_point_id(3) = cube%getNearestNodeID(x=150.0d0,y=150.0d0,z=0.0d0)
logger_point_id(4) = cube%getNearestNodeID(x=150.0d0,y=150.0d0,z=-20.0d0)

!(2) set number of channels
channel_id = 0
do i=1,size(logger_point_id)
    do j=1,3
        channel_id = channel_id + 1
        channel_name = "Point_"+str(i)+"_Direction_"+str(j)
        call logger%set(&
            channel_name=channel_name,&
            channel_value=seismic%A( (logger_point_id(i)-1)*3 + j) ,&
            channel_id=channel_id)
    enddo
enddo

! H/V spectre:
! BSSA 1964 Harkrider Surface wave in ...
! Arai and Tokimatsu BSSA, 2004, S-wave velocity profiling

! start data-logger
call logger%start()
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

    call logger%save(t=dble(i)*seismic%dt )

enddo
! end
call cube%remove()
call seismic%remove()
end