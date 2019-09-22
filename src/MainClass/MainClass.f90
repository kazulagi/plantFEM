program main
    use SimulatorClass
    implicit none
    type(MPI_)              :: MPIData
    type(Field_),target     :: world
    integer                 :: TotalStep=40
    real(8)                 :: time=1.0d0
    
    call MPIData%Start()

    call world%Import()
    call Simulator(world,OptionalStep=TotalStep,OptionalTime=time)
    call world%Export()
    
    call MPIData%End()
    
end program 