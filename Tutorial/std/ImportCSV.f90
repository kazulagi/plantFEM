program main
    use std
    implicit none

    type(CSV_) :: f

    call f%import("../","test",".csv")
    call f%export("../","test2",".csv")

end program