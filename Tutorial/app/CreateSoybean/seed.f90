program main
    use obj
    implicit none

    type(Seed_) :: soy
    type(IO_) :: f
    integer :: i

    ! create seed
    call soy%create(MeshType="Sphere3D",x_num=12,y_num=11,z_num=10,x_len=9.0d0, y_len=8.0d0,z_len=6.50d0,&
    Permiability=1.0d0,a_Psi=2.0d0, a_P=3.0d0, theta_eq=0.80d0, Psi_eq=0.20d0, a_E=1.0d0, a_v=1.0d0, E_eq=1.0d0, v_eq=1.0d0)
    
    do i=1,100
        ! create environment
        call soy%env(disp_x=0.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
            z_max=1.0d0,z_min=-1.0d0)
        call soy%env(disp_y=0.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
            z_max=1.0d0,z_min=-1.0d0)
        call soy%env(disp_z=0.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
            z_max=1.0d0,z_min=-1.0d0)
        call soy%env(disp_z=0.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
            z_max=1.0d0,z_min=-1.0d0)
        call soy%env(disp_z=0.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
            z_max=9.1d0,z_min=8.9d0)

        call soy%env(WaterContent=1.0d0,x_max=5.0d0,x_min=-1.0d0,y_max=100.0d0,y_min=-100.0d0,&
            z_max=10.0d0,z_min=-10.0d0)
        
        call soy%grow(timestep=100,dt=100.0d0,Display=.true.,nr_tol=0.010d0,restart=.false.)

    enddo
    
!    ! start growth
!    
!    
!
!    call soy%save("../","seed000")
!
!    call f%open("../","seed000/output/seed_length.txt")
!    write(f%fh, *) soy%length()
!    call f%close()
    

!    ! ####################
!    call soy%grow(timestep=10,dt=100.0d0,Display=.true.,nr_tol=0.010d0,restart=.true.)
!
!    call soy%save("../","seed001")
!    
!    call f%open("../","seed001/output/seed_length.txt")
!    write(f%fh, *) soy%length()
!    call f%close()
!    

    ! ####################
    call soy%grow(timestep=2,dt=100.0d0,Display=.true.,nr_tol=0.010d0,restart=.false.)

    call soy%save("../","seed002")
    !call soy%load("../","seed002")
    call showArraysize(soy%seedDomain%DiffusionEq%UnknownValue)
    
    
    !call soy%grow(timestep=2,dt=100.0d0,Display=.true.,nr_tol=0.010d0,restart=.true.)
    !call f%open("../","seed002/output/seed_length.txt")
    !write(f%fh, *) soy%length()
    !call f%close()
    !call soy%open("../","seed")
    
    
end program main