use sim
implicit none

type(SeismicAnalysis_) :: seismic
type(FEMDomain_),target :: cube,original
type(IO_) :: f,response,history_A,history_V,history_U,input_wave
type(Math_) :: math
real(real64),allocatable :: disp_z(:,:)
real(real64) :: wave(200,2),T,Duration,dt
integer(int32) :: i,j,cases,stack_id,num_of_cases

num_of_cases = 4



! create Domain
call cube%create(meshtype="Cube3D",x_num=4,y_num=4,z_num=20)
call cube%resize(x=1.0d0,y=1.0d0,z=5.0d0)
call cube%move(z=-5.0d0)


! create Wave
T = 0.300d0
Duration = T * 10.0d0 ! sec.
dt = Duration/dble(size(wave,1))!/10.0d0

wave(:,:) = 0.0d0
do i=1,size(wave,1)
    wave(i,1) = dt*dble(i)
    wave(i,2) = sin(2.0d0*math%pi/T*wave(i,1) )
enddo
call input_wave%open("input_wave.txt" )
call input_wave%write(wave )
call input_wave%close()
original = cube

! set domain
seismic%femdomain => cube
! set wave
seismic%wave = wave
seismic%dt = dt
! run simulation
call seismic%init()
!call seismic%fixDisplacement(z_max = -4.99d0,direction="x")
call seismic%fixDisplacement(z_max = -4.99d0,direction="y")
call seismic%fixDisplacement(z_max = -4.99d0,direction="z")
call seismic%fixDisplacement(y_max = 0.0d0,direction="y")
call seismic%fixDisplacement(y_min = 1.0d0,direction="y")
call seismic%fixDisplacement(direction="z")
call seismic%femdomain%vtk("mesh_init" )
call seismic%loadWave(z_max=-4.50d0,direction="x",wavetype=WAVE_ACCEL)


seismic%Density(:)      = 17000.0d0 !(N/m/m/m)
seismic%PoissonRatio(:) = 0.330d0 
seismic%YoungModulus(:) = 25372853.0 !(N/m/m) Vs=121 m/s

!seismic%alpha = 0.0d0
seismic%a = 0.0d0
seismic%v = 0.0d0
seismic%u = 0.0d0

do i=1,199
    !seismic%beta = 0.0d0
    call history_A%open("history_A"//str(i)//".txt","w")
    call history_V%open("history_V"//str(i)//".txt","w")
    call history_U%open("history_U"//str(i)//".txt","w")
    
    call seismic%run(timestep=[i,i+1],AccelLimit=10.0d0**8)

    do j=1,seismic%femdomain%nn()
        if(seismic%femdomain%position_x(j)/=0.0d0) cycle
        if(seismic%femdomain%position_y(j)/=0.0d0) cycle
        write(history_A%fh,*) real(dble(i)*dt),&
        seismic%femdomain%position_z(j), real(seismic%A( (j-1)*3 + 1  ))
    enddo
    do j=1,seismic%femdomain%nn()
        if(seismic%femdomain%position_x(j)/=0.0d0) cycle
        if(seismic%femdomain%position_y(j)/=0.0d0) cycle
        write(history_V%fh,*) real(dble(i)*dt),&
        seismic%femdomain%position_z(j), real(seismic%V( (j-1)*3 + 1  ))
    enddo
    do j=1,seismic%femdomain%nn()
        if(seismic%femdomain%position_x(j)/=0.0d0) cycle
        if(seismic%femdomain%position_y(j)/=0.0d0) cycle
        write(history_U%fh,*) real(dble(i)*dt),&
        seismic%femdomain%position_z(j), real(seismic%U( (j-1)*3 + 1  ))
    enddo
    
    call history_A%close()
    call history_V%close()
    call history_U%close()
enddo
!print *, maxval(seismic%U)
seismic%femdomain%mesh%nodcoord = seismic%femdomain%mesh%nodcoord + (10.0d0**0)*&
    reshape(seismic%a, seismic%femdomain%nn(),seismic%femdomain%nd() )
call seismic%femdomain%vtk("result")
!call response%open("T_A"//str(cases)//".txt")
!call response%write(T,seismic%maxA(1))
!call response%close()

end