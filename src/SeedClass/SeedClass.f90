module SeedClass
    use MathClass
    use LsystemClass
    use FEMDomainClass
    implicit none

    type :: Seed_
        integer :: num_of_seed ! num of seed
        real(8) :: mass ! seed mass g/cm^3
        real(8) :: water_content ! seed water_content %
        real(8) :: radius ! seed radius (cm)
        real(8) :: location(3) ! seed location (x,y,z)
    contains
        procedure :: init => initSeed 
        procedure :: export => exportSeed
    end type
contains
!########################################################
subroutine initSeed(obj,mass,water_content,radius,location,x,y,z)
    class(Seed_),intent(inout) :: obj
    real(8),optional,intent(in) :: mass,water_content,radius,location(3),x,y,z
    real(8) :: loc(3)
    loc(:)=0.0d0
    if(present(location) )then
        loc(:) = location(:)
    endif 
    obj%num_of_seed = 1
    obj%mass = input(default=1.0d0,option=mass)
    obj%water_content = input(default=12.0d0,option=water_content)
    obj%radius = input(default=0.30d0,option=radius)
    obj%location(:) = loc(:)
    obj%location(1) = obj%location(1)+input(default=0.0d0,option=x)
    obj%location(2) = obj%location(2)+input(default=0.0d0,option=y)
    obj%location(3) = obj%location(3)+input(default=0.0d0+obj%radius,option=z)
    

end subroutine
!########################################################



!########################################################
subroutine exportSeed(obj,FileName,SeedID)
    class(Seed_),intent(in) :: obj
    character(*),intent(in) :: FileName
    integer,optional,intent(in) :: SeedID
    
    open(10,file=FileName)
    write(10,'(A)') "//+"
    write(10,'(A)') 'SetFactory("OpenCASCADE");'
    write(10,*) "Sphere(",input(default=1,option=SeedID),") = {",&
    obj%location(1),",", obj%location(2),",", obj%location(3),",",&
    obj%radius,", -Pi/2, Pi/2, 2*Pi};"
    close(10)
end subroutine
!########################################################


end module SeedClass