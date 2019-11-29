program main
    use mpi
    use MPIClass
    use PreprocessingClass
    implicit none

    type(Preprocessing_) :: obj
    type(MPI_) :: MPIData
    character*200 :: project,soil,boundarycondition



    project = "/home/haruka/Dropbox/PlantSoil/parametric_study/incre_length_only_m/"
    soil    = "/home/haruka/Dropbox/PlantSoil/parametric_study/incre_length_only_m/root_length_soil.svg.png"

    
    call MPIData%Start()
    call obj%getScfFromImage(project=trim(project),ElemType="LinearRectangularGp4",MPIData=MPIData,&
        R=0,G=128,B=0,scalex=16.0d0,scaley=9.0d0,Soilfile=soil,sR=255,SG=0,sB=0,SolverName="FiniteDeform_")
    call MPIData%end()
end program