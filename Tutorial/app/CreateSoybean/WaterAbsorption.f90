program main
    use FarmClass
    implicit none
    type(Farm_):: SoybeanField
    integer :: i,timestep
    real(8) :: total_time,temp,time_interval
    print *, "Input time duration :: (hour.)"
    read(*,*) total_time
    print *, "Time-steps"
    read(*,*) timestep
    print *, "Temperature :: deg."
    read(*,*) temp
    time_interval=total_time/dble(timestep)*60.0d0
    call SoybeanField%sowing(crop_name="soybean",single=.true.,Variety="Tachinagaha")
    do i=1,timestep
        call SoybeanField%grow(crop_name="soybean",dt=time_interval,temp=temp+273.150d0)
        call SoybeanField%export(FileName="/home/haruka/test/soybean",withSTL=.true.,withMesh=.true.,TimeStep=i)
    enddo
end program 