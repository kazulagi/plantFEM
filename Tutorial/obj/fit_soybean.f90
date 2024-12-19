program main
    use SoybeanClass
    implicit none

    type(Soybean_) :: soy,last_soy
    type(FEMDomain_) :: cube
    real(real64),allocatable :: stem_length_list(:)
    real(real64),allocatable :: stem_angle_list(:,:)
    real(real64) :: target_height = 0.90d0
    real(real64) :: target_width = 0.70d0
    integer(int32) :: itr
    real(real64) :: sigma
    type(Random_) :: random
    type(IO_) :: f



    call soy%init(config="Tutorial/obj/soy.json") 
    call soy%vtk(name="soy",single_file=.true.)
    

    call cube%create("Cube3D",&
        x_axis=[soy%x_min(),0.0d0,soy%x_max()],&
        y_axis=[soy%y_min(),0.0d0,soy%y_max()],&
        z_axis=[soy%z_min(),0.0d0,target_height])
    call cube%vtk("bbox")
    

    stem_length_list = soy%get_stem_length_list()
    stem_angle_list  = soy%get_stem_angle_list()
    call f%open("opt_history.txt","w")
    last_soy = soy
    
    sigma = 1.0d0/1000.0d0 
    itr = 0
    do i_i=1,10000
        stem_length_list = abs(stem_length_list + random%gauss(mu=0.0d0,sigma=sigma,n=size(stem_angle_list,1)))
        !stem_angle_list  = stem_angle_list  +  random%gauss(mu=0.0d0,sigma=radian(0.00d0),&
        !    n=size(stem_angle_list,1),m=size(stem_angle_list,2))
        
        call soy%set_stem_length_by_list(stem_length_list)
        !call soy%set_stem_angle_by_list(stem_angle_list)
        
        !sigma = sigma*0.99
        if (abs(target_height - soy%z_max()) < abs(target_height - last_soy%z_max()))then
            if (abs(target_width - (soy%x_max()-soy%x_min()) ) &
                < abs(target_width - (last_soy%x_max()-last_soy%x_min()) ))then
                last_soy = soy
                itr = itr + 1
                print *, i_i,soy%height()
                
                call f%write(i_i,soy%height())
                call f%flush()
                call soy%vtk(name="soy_"+zfill(itr,4),single_file=.true.)
            endif
        endif
    enddo
    sigma = 1.0d0/1000.0d0 
    do i_i=1,10000
        stem_length_list = abs(stem_length_list + random%gauss(mu=0.0d0,sigma=sigma,n=size(stem_angle_list,1)))
        !stem_angle_list  = stem_angle_list  +  random%gauss(mu=0.0d0,sigma=radian(0.00d0),&
        !    n=size(stem_angle_list,1),m=size(stem_angle_list,2))
        
        call soy%set_stem_length_by_list(stem_length_list)
        !call soy%set_stem_angle_by_list(stem_angle_list)
        
        !sigma = sigma*0.99
        if (abs(target_height - soy%z_max()) < abs(target_height - last_soy%z_max()))then
            if (abs(target_width - (soy%x_max()-soy%x_min()) ) &
                < abs(target_width - (last_soy%x_max()-last_soy%x_min()) ))then
                last_soy = soy
                itr = itr + 1
                print *, i_i,soy%height()
                
                call f%write(i_i,soy%height())
                call f%flush()
                call soy%vtk(name="soy_"+zfill(itr,4),single_file=.true.)
            endif
        endif
    enddo
    call soy%vtk(name="opt_soy",single_file=.true.)
    call f%close()
end program main