program main
    use SimulatorClass
    
    type(MPI_) :: mpidata
    type(field_) :: world
    
    call mpidata%start()
    
    print *, "Hello!"
    
    call mpidata%end()
    
end program main