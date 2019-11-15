program main
    use FarmClass
    implicit none

    type(Farm_):: SoybeanField
    integer :: i,timestep
    real(8) :: total_time,temp

    print *, "Input time duration :: (hour.)"
    read(*,*) total_time
    print *, "Time-steps"
    read(*,*) timestep
    print *, "Temperature :: deg."
    read(*,*) temp
    
    call SoybeanField%sowing(crop_name="soybean",single=.true.,Variety="Tachinagaha")
    do i=1,timestep
        write(60,*) total_time*1.0d0/dble(timestep)*60.0d0*dble(i)," (sec.) ",SoybeanField%Soybean(1,1)%Seed%Width1, " mm"
        call SoybeanField%grow(dt=total_time/dble(timestep)*60.0d0,temp=temp+273.150d0,crop_name="soybean" )
        call SoybeanField%export(FileName="/home/haruka/test/soybean",withSTL=.true.,withMesh=.true.,TimeStep=i)
    enddo

end program 