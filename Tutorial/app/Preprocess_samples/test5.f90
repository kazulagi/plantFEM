program main
    use PreprocessingClass

    type(Preprocessing_)::omega
    type(MPI_)::mpidata
    real(8),allocatable :: nod_coord(:,:)


    call mpidata%start()
    call omega%Init(Default=.true.)  ! Constractor
    call omega%ImportPictureName(Name="debug/scandata/case1GM.png")
    call omega%ImportPictureName(Name="EXAMPLES/green.png")
    call omega%GetPixcelSize(MPIData)
    
    call omega%SetColor(28,255,255)
    call omega%SetColor(0,255,0)
    call omega%GetPixcelByRGB(MPIData,err=10,onlycoord=.true.)
    call omega%importPixcelAsNode(interval=100)

    call showArray(omega%FEMDomain%Mesh%NodCoord,Name="test.txt")
    call omega%Meshing()
    call mpidata%end()

end program main