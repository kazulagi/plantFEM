program main
    use RouteOptimization
    use MPIClass
    implicit none

    type(MPI_) :: mpid
    type(RouteOptimization_) :: obj

    ! start mpi
    call mpid%start()
    ! get points from file
    call obj%import("/home/haruka/Dropbox/Paper/ProjNakamori/coord1.txt")
    ! optimize route by solving a TS problem.
    call obj%run(SolverName="TSP_enum_greedy_roop",NumOfPoints=59)
    ! export route
    call obj%export("/home/haruka/Dropbox/Paper/ProjNakamori")
    !close mpi
    call mpid%end()

end program main