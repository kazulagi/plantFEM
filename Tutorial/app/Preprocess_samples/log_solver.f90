program main
   use SimulatorClass
   implicit none
   type(MPI_)              :: MPIData
   type(Field_),target     :: world
   integer                 :: TotalStep=10
   real(8)                 :: time=60.0
   call MPIData%Start()
   call world%Import(OptionalDomainListName="Tutorial/InputData/Domainlist.txt",&
   OptionalIfaceListName="Tutorial/InputData/Ifacelist.txt")
   call Simulator(world,OptionalStep=TotalStep,OptionalTime=time,SolverType="GaussJordan"  )
   call world%Export()
   call MPIData%End()
end program main
