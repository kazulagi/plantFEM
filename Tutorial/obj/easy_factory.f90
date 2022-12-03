program main
    use FactoryClass
    use SceneClass
    implicit none

    type(Factory_) :: factory
    type(Scene_)   :: scene
    type(Random_)  :: random
    type(FEMDomain_),allocatable  :: objects(:)
    type(Soybean_)    :: soybeans(10)

    objects   = factory%cube(division=[10,10,10],n=10)
    objects = objects  // factory%sphere(division=[10,10,10],n=10)


    call factory%resize(&
        objects,&
        x=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects)),&
        y=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects)),&
        z=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects)) &
        )
    
    call factory%move(&
        objects,&
        x=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects) ),&
        y=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects) ),&
        z=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects) ) &
        )
    
    
    call factory%rotate(&
        objects,&
        x=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects)),&
        y=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects)),&
        z=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(objects)) &
        )
    
    soybeans = factory%soybean(config="Tutorial/obj/soy.json",n=10)
    
    call factory%move(&
        objects=soybeans,&
        x=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(soybeans) ),&
        y=random%gauss(mu=0.0d0,sigma=1.0d0,n=size(soybeans) ),&
        z=zeros(size(soybeans)) &
        )


    call scene%add(objects)
    call scene%add(soybeans)
    call scene%vtk("hello")

end program



