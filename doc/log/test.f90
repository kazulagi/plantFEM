program main
    use SiCroF
    implicit none

    type(MPI_) :: mpid

    call mpid%start()
    print *, "My np is", mpid%petot
    print *, "My rank is", mpid%MyRank
    call mpid%end()

end program