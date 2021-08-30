use sim
implicit none

type(SeismicAnalysis_) :: seismic(100)
type(FEMDomain_),target :: cube,original
type(IO_) :: f,response,history_A,history_V,history_U,input_wave
type(Math_) :: math
type(MPI_) :: mpid
real(real64),allocatable :: disp_z(:,:)
real(real64) :: wave(1000,2),T,Duration,dt
integer(int32) :: i,j,cases,stack_id,num_of_cases

num_of_cases = 4

call mpid%start()
call mpid%createStack(num_of_cases)


! create Domain
call cube%create(meshtype="Cube3D",x_num=2,y_num=2,z_num=20)
call cube%resize(x=1.0d0,y=1.0d0,z=5.0d0)
call cube%move(z=-5.0d0)

! Change T = 0.1, 0.2, 0.3 ... 1 (sec.)

do stack_id=1,size(mpid%localstack)
    cases = mpid%localstack(stack_id)
    ! create Wave
    T = 0.010d0*cases+0.50d0 ! sec.
    Duration = T * 10.0d0 ! sec.
    dt = Duration/dble(size(wave,1))/10.0d0
    
    wave(:,:) = 0.0d0
    do i=1,size(wave,1)
        wave(i,1) = dt*dble(i)
        wave(i,2) = sin(2.0d0*math%pi/T*wave(i,1) )
    enddo
    call input_wave%open("input_wave_"//str(cases)//".txt" )
    call input_wave%write(wave)
    call input_wave%close()
    original = cube
    

    ! set domain
    seismic(cases)%femdomain => cube
    ! set wave
    seismic(cases)%wave = wave
    seismic(cases)%dt = dt
    ! run simulation
    call seismic(cases)%init()
    call seismic(cases)%fixDisplacement(z_max = -4.99d0,direction="x")
    call seismic(cases)%fixDisplacement(z_max = -4.99d0,direction="y")
    call seismic(cases)%fixDisplacement(z_max = -4.99d0,direction="z")

    call seismic(cases)%fixDisplacement(y_max = 0.0d0,direction="y")
    call seismic(cases)%fixDisplacement(y_min = 1.0d0,direction="y")

    call seismic(cases)%fixDisplacement(direction="z")

    call seismic(cases)%femdomain%vtk("mesh"//str(cases)//"_" )

    call seismic(cases)%loadWave(z_max=-4.50d0,direction="x",wavetype=WAVE_ACCEL)

    seismic(cases)%Density(:)      = 17000.0d0 !(N/m/m/m)
    seismic(cases)%YoungModulus(:) = 7000000.0d0 !(N/m/m)
    seismic(cases)%PoissonRatio(:) = 0.40d0 

    !seismic(cases)%alpha = 0.0d0
    !seismic(cases)%beta = 0.0d0

    call history_A%open("history_A"//str(cases)//".txt","w")
    call history_V%open("history_V"//str(cases)//".txt","w")
    call history_U%open("history_U"//str(cases)//".txt","w")
    
    do i=1,999
        call seismic(cases)%run(timestep=[i,i+1],AccelLimit=10.0d0**8)
        write(history_A%fh,*) real(dble(i)*dt), real(seismic(cases)%A( size(seismic(cases)%A)-2 ))
        write(history_V%fh,*) real(dble(i)*dt), real(seismic(cases)%V( size(seismic(cases)%V)-2 ))
        write(history_U%fh,*) real(dble(i)*dt), real(seismic(cases)%U( size(seismic(cases)%U)-2 )    )
        call history_A%flush()
        call history_V%flush()
        call history_U%flush()
    enddo
    call history_A%close()
    call history_V%close()
    call history_U%close()

    print *, maxval(seismic(cases)%U)


    seismic(cases)%femdomain%mesh%nodcoord = seismic(cases)%femdomain%mesh%nodcoord + (10.0d0**6)*&
        reshape(seismic(cases)%U, seismic(cases)%femdomain%nn(),seismic(cases)%femdomain%nd() )

    call seismic(cases)%femdomain%vtk("x10"//str(cases)//"_")

    call response%open("T_A"//str(cases)//".txt")
    call response%write(T,seismic(cases)%maxA(1))
    call response%close()

enddo

call mpid%end()

end