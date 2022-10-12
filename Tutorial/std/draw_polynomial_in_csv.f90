program main
    use plantfem
    implicit none

    !type(FEMDomain_) :: soil
    !type(SeismicAnalysis_) :: seis
    type(Dictionary_) :: df
    integer(int32) :: i

    df = dict()
    call df%update("x","2 x^2 - x + 1")
    do i=-10000,10000
        call df%update(str(i/1000.0d0), polynomial(x=i/1000.0d0,params=[1.0d0, 2.0d0, -100.0d0, 1.0d0] ) )
    enddo
    call df%to_csv("test")

end program