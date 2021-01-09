program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: domain
    type(Random_) :: random
    real(real64),allocatable :: StressTensor(:,:,:),symtensor(:,:),tensor(:,:)
    integer(int32) :: i


    call domain%create(meshtype="rectangular2D",x_num=10, y_num=10)

    ! create StressTensorField
    StressTensor = random%cube(domain%nn(),2,2 )
    do i=1,domain%nn()
        symtensor = random%matrix(2,2)
        symtensor = symtensor + transpose(symtensor) 
        StressTensor(i,1:2,1:2) = symtensor(1:2,1:2) 
    enddo
   
    !call eigen_2d(symtensor,tensor)
    !call print(tensor)
    !return

    call domain%addlayer(name="CauchyStress",tensor=StressTensor)
    call domain%showLayer()
    call domain%msh(name="mesh")
    print *, domain%PhysicalField(1)%name
    call domain%msh(name="eigens",fieldname="CauchyStress")

end program main
