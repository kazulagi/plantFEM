program main
    use SiCroF

    type(Random_) :: random
    type(FEMDomain_) :: femdomain
    integer(int32) :: i
    
    call random%init()
    allocate(femdomain%Mesh%nodCoord(100,3) )
    do i=1,100
        femdomain%Mesh%nodCoord(i,1) = random%random()
        femdomain%Mesh%nodCoord(i,2) = random%random()
        femdomain%Mesh%nodCoord(i,3) = random%random()
    enddo


    call showarray(Mat=femdomain%Mesh%nodCoord,Name="test.txt")
    
end program
