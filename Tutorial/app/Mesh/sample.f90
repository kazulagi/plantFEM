program main
    use SiCroF
    implicit none

    integer i
    integer timestep
    type(FEMDomain_) :: hydrological_field,Mechanical_field


    call hydrological_field%init()
    call Mechanical_field%init()


    timestep=1000
    do i=1, timestep
        call hydrological_field%update()
        call Mechanical_field%update()
    enddo
    
end program main