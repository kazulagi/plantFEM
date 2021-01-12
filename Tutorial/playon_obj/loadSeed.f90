program main
    use obj
    implicit none
    type(Seed_) :: soy
    call soy%load("../","seed002")
    call soy%env(disp_z=10.0d0,x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
        z_max=100.0d0,z_min=-100.0d0)
    call soy%grow(timestep=3,dt=100.0d0,Display=.true.,nr_tol=0.010d0,restart=.true.)
    call soy%save("../","seed003")
end program
