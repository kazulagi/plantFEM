program main
    use plantFEM
    implicit none

    real(real64) :: x(3) = [1.0d0 , 2.0d0 , 3.0d0]

    call print(x)

end program main

! ==
!
! import numpy as np
! x = np.array([1.0, 2.0, 3.0])
! print(x)

