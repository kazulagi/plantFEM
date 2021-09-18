program main
    use MPIClass
    use FEMDomainClass
    use ContactMechanicsClass

    implicit none

    type(MPI_)      ::mpidata
    type(FEMDomain_),target::obj1,obj2
    type(FEMIface_),target::ifobj
    type(ContactMechanics_) ::cobj

    call mpidata%start()
    call obj1%Import(OptionalProjectName="finitedtest1")
    call obj2%Import(OptionalProjectName="finitedtest2")
    
    call obj2%move(x=-0.25d0,y=0.0d0,z=-2.90d0)
    !call obj2%rotate(x=-1.0d0,y=0.0d0,z=-2.90d0)
    
    call ifobj%init(2)
    call ifobj%setFEMDomain(obj1,Name="finitedtest")
    call ifobj%setFEMDomain(obj2,Name="finitedtest2")
    call ifobj%GetFEMIface()

    call obj1%GmshPlotMesh(Name="1st")
    call obj2%GmshPlotMesh(Name="2nd")

    call cobj%deploy(ifobj)
    call cobj%setPenaltyParameter( dble(10.0e+004) )
    call cobj%updateContactStress()

    call mpidata%end()
end program