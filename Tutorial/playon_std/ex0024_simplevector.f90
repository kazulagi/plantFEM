program main
    use plantFEM
    implicit none

    real(real64) :: OA(2),OB(2),OC(2)
    real(real64) :: AB(2),CA(2),AC(2),BA(2)

    OA(1)=3.0d0; OA(2)=8.0d0
    OB(1)=-1.0d0; OB(2)=2.0d0
    OC(1)=3.00d0; OC(2)=-20.0d0
    AB = OB - OA
    BA = OA - OB
    CA = OA - OC
    AC = OC - OA
    print *, 1, 2.0d0/3.0d0*OA + 2.0d0*BA
    print *, 2,CA + 2.0d0*OB
    print *, 3,5.0d0*CA + 2.0d0*AB
    print *, 4,5.0d0*CA + 2.0d0*AB + 2.0d0*AC
    print *, 5,AB + 2.0d0*OB
    print *, 6,OA + 2.0d0*OB + 2.0d0*OC
    print *, 7,2.0d0*OA + 2.0d0*AB
    print *, "Student's answers"

    print *, 56.00d0/19.0d0*OA+ 46.0d0/19.0d0*OB !+ 1.0d0*OC

    
end program main
