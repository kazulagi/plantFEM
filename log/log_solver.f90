program main
   use SimulatorClass
   implicit none
   type(MPI_)              :: MPIData
   type(Field_),target     :: world
   integer                 :: TotalStep=100
   real(8)                 :: time=1.0
   call MPIData%Start()
   call world%Import(OptionalDomainListName="Tutorial/InputData/Domainlist_test.txt",&
   OptionalIfaceListName="Tutorial/InputData/Ifacelist.txt")
   call Simulator(world,OptionalStep=TotalStep,OptionalTime=time,SolverType="GaussJordan"  )
   call world%Export()
   call MPIData%End()
end program main
