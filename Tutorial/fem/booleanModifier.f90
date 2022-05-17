program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: soil,plate,box
    real(real64),allocatable :: PorePressure(:),Velocity(:,:)




    call soil%create("Cube3D",x_num=100,y_num=100,z_num=100)
    call soil%resize(x=10.0d0,y=10.0d0,z=10.0d0)

    call plate%create("Cube3D",x_num=3,y_num=10,z_num=10)
    call plate%resize(x=0.70d0,y=10.0d0,z=10.0d0)
    call plate%move(x=5.0d0,z=2.00d0)

    call box%create("Cube3D",x_num=3,y_num=3,z_num=3)
    call box%resize(x=5.0d0,y=10.0d0,z=5.0d0)
    call box%move(x=5.50d0,z=5.00d0)
    call box%vtk("box")

    call soil%boolean(object=plate,difference=.True.)
    call soil%boolean(object=box,difference=.True.)
    
    call plate%vtk("plate")
    call box%vtk("box")
    call soil%vtk("cut")

end program main