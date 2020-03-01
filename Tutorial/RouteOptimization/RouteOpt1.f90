program main
    use iso_fortran_env
    use RouteOptimization
    use MPIClass
    implicit none

    type(MPI_) :: mpid
    type(RouteOptimization_) :: obj
    integer(int32) :: np

    ! start mpi
    call mpid%start()
    ! get points from file
    call obj%import()
    ! optimize route by solving a TS problem.
    print *, "Number of points is : "
    read(*,*) np
    call obj%run(SolverName="TSP_enum_greedy_roop",NumOfPoints=np)
    ! export route
    call obj%export()
    !close mpi
    call mpid%end()

end program main