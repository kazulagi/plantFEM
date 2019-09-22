program main
    use SiCroF

    implicit none
    type(MPI_)              :: MPIData
    type(Field_),target     :: world
    integer                 :: TotalStep=40
    real(8)                 :: time=1.0d0
    call MPIData%Start()

    call world%Import()
    call world%show()    
    call MPIData%end()

end program main
