program main
   use SimulatorClass
   implicit none
   type(MPI_)              :: MPIData
   type(Field_),target     :: world
   integer                 :: TotalStep=10
   real(8)                 :: time=1.0
   call MPIData%Start()
   call world%Import(OptionalDomainListName="Debug_domainlist.txt",&
   OptionalIfaceListName="Debug_Ifacelist.txt")
   call Simulator(world,OptionalStep=TotalStep,OptionalTime=time)
   call world%Export()
   call MPIData%End()
end program main
