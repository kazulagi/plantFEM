program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: root2d, rootRandom(2)
    type(Random_) :: random
    integer(int32) :: i
    real(real64) :: length, rot_x,x,y,z

    ! case #1 :: create root by rule
    call root2d%create(meshtype="root")
    length=1.0d0
    rot_x = 0.0d0
    do i=1,2999
        rot_x = rot_x + 0.010d0
        call root2d%add(Length=length,rot_x=rot_x)
        length=length*0.9990d0
    enddo


    call root2d%add(From=100,Length=length,rot_x=rot_x)
    length=1.0d0
    rot_x = 0.0d0
    do i=1,1000
        rot_x = rot_x + 0.010d0
        call root2d%add(Length=length,rot_x=rot_x)
        length=length*0.9990d0
    enddo


    call root2d%add(From=300,Length=length,rot_x=rot_x)
    length=1.0d0
    rot_x = 0.0d0
    do i=1,1000
        rot_x = rot_x + 0.010d0
        call root2d%add(Length=length,rot_z=rot_x)
        length=length*0.9990d0
    enddo




    call root2d%add(From=1000,Length=length,rot_z=rot_x)
    length=1.0d0
    do i=1,1000
        rot_x = rot_x + 0.010d0
        call root2d%add(Length=length,rot_z=rot_x)
        length=length*0.9990d0
    enddo

    call root2d%msh("root")

    call random%init()


    ! case #2 :: randomly generated 2d root
    x = random%gauss(mu=0.0d0,sigma=1.0d0)
    y = 0.0d0
    z = random%gauss(mu=0.0d0,sigma=1.0d0)
    call rootRandom(1)%create(meshtype="root",x=x,y=y,z=z)
    do i=1,1000
        x = random%gauss(mu=0.0d0,sigma=1.0d0)
        y = 0.0d0
        z = random%gauss(mu=0.0d0,sigma=1.0d0)
        call rootRandom(1)%add(x=x,y=y,z=z)
    enddo
    call rootRandom(1)%msh("random-2d")

    ! case #3 :: randomly generated 2d root
    x = random%gauss(mu=0.0d0,sigma=1.0d0)
    y = random%gauss(mu=0.0d0,sigma=1.0d0)
    z = random%gauss(mu=0.0d0,sigma=1.0d0)
    call rootRandom(2)%create(meshtype="root",x=x,y=y,z=z)
    do i=1,1000
        x = random%gauss(mu=0.0d0,sigma=1.0d0)
        y = random%gauss(mu=0.0d0,sigma=1.0d0)
        z = random%gauss(mu=0.0d0,sigma=1.0d0)
        call rootRandom(2)%add(x=x,y=y,z=z)
    enddo
    call rootRandom(2)%msh("random-3d_x")

    ! case #4 :: randomly generated 2d root (give dx, dy and dz)
    x = random%gauss(mu=0.0d0,sigma=1.0d0)
    y = random%gauss(mu=0.0d0,sigma=1.0d0)
    z = random%gauss(mu=0.0d0,sigma=1.0d0)
    call rootRandom(2)%create(meshtype="root",dx=x,dy=y,dz=z)
    do i=1,1000
        x = random%gauss(mu=0.0d0,sigma=1.0d0)
        y = random%gauss(mu=0.0d0,sigma=1.0d0)
        z = random%gauss(mu=0.0d0,sigma=1.0d0)
        call rootRandom(2)%add(dx=x,dy=y,dz=z)
    enddo
    call rootRandom(2)%msh("random-3d_dx")
 
end program main
