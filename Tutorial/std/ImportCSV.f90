program main
    use std
    implicit none

    type(CSV_) :: f

    call f%import("../","test",".csv")

end program