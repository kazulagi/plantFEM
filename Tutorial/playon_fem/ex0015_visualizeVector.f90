program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: space
    type(Random_) :: random
    real(real64) :: center(3),dist,coord(3),direction(3)
    !　走性係数（走性/ランダム比）
    real(real64),parameter :: a=0.010d0
    integer(int32) :: i,j

    call space%create(meshtype="Cube")
    ! cm!
    call space%resize(x=5.0d0,y=5.0d0,z=3.0d0)

    call space%msh("space")

    call space%addLayer(name="nematode",attribute="Nodal",datastyle="Vector")
    
    !中心
    do i=1, 10

        center(1) = 2.50d0   
        center(2) = 2.50d0  
        center(3) = 3.00d0 - 3.0d0*dble(i-1)/10.0d0
        call random%fill(space%physicalfield(1)%Vector)
        space%physicalfield(1)%Vector(:,:) = space%physicalfield(1)%Vector(:,:) - 0.50d0


        ! ランダムに動く＋中心に向けて動く
        do j=1,space%nn()
            coord(:) = space%mesh%nodcoord(j,:) 
            direction(:) = center(:) - coord(:)
            direction(:) = 1.0d0/sqrt(dot_product(direction,direction))*direction(:)*a
            
            
            space%physicalfield(1)%Vector(j,:) = space%physicalfield(1)%Vector(j,:)+direction(:)
        enddo
        
        ! びっくりしてうごくだけ

        !do j=1,space%nn()
        !    coord(:) = space%mesh%nodcoord(j,:) 
        !    dist = sqrt(dot_product(coord-center,coord-center))
        !    if(dist==0.0d0)then
        !        space%physicalfield(1)%Vector(j,:) = 0.0d0*space%physicalfield(1)%Vector(j,:)
        !    else
        !        space%physicalfield(1)%Vector(j,:) = 1.0d0/dist*space%physicalfield(1)%Vector(j,:)
        !    endif
        !    
        !enddo

        call space%msh(name="nematode",Vector=space%physicalfield(1)%Vector,step=i)
    enddo
    

end program main
