program Preprocessing
    use TermClass
    use DictionaryClass
    use PreprocessingClass
    use PostProcessingClass

    implicit none
    
    type(Dictionary_)       :: InfileList
    type(MPI_)              :: MPIData
    type(PreProcessing_)    :: leaf
    character * 200         :: name,name1,name2,name3,name4,ElemType,SolverName
    call MPIData%Start()

    !!############### Get mesh from images ######################
    !!############### Get mesh from images ######################
    ElemType = "LinearRectangularGp4"
    name1="./your_leaf_picture.png"
    name2="./your_leaf_picture.png"
    name3="./your_leaf_picture.png"
    name4="./your_leaf_picture.png"

    ! load file names
    call InfileList%Init(4) ! Constractor
    call InfileList%Input(1,name1)
    call InfileList%Input(2,name2)
    call InfileList%Input(3,name3)
    call InfileList%Input(4,name4)

    name = InfileList%Get(MPIData%MyRank+1)
    print *, "My_rank : ",MPIData%MyRank,"InfileName : ",trim(name)

    ! Get Pixcel
    call leaf%Init(Default=.true.)  ! Constractor
    call leaf%ImportPictureName(name)
    call leaf%GetPixcelSize(MPIData)
    call leaf%SetColor(0,128,0)
    !call leaf%SetColor(255,142,28)
    
    call leaf%GetPixcelByRGB(MPIData,err=5,onlycoord=.true.)
    ! Get Outline
    call leaf%GetSurfaceNode(MPIData)
    call leaf%AssembleSurfaceElement(MPIData,dim=2,threshold=10,DelRange=10)
    ! Reduce Number of Surface Nodes
    call leaf%ReduceSize(MPIData,interval=10)
    ! Convert SurfaceNod to .geo
    call leaf%ExportGeoFile(MPIData)
    ! Run Gmsh to convert .geo to .msh
    call leaf%ConvertGeo2Msh(MPIData)
    call leaf%ConvertGeo2Inp(MPIData)
    call leaf%ConvertGeo2Mesh(MPIData)
    !!############### Get mesh from images ######################
    !!############### Get mesh from images ######################

    !!############### Convert Mesh Type ######################
    !!############### Convert Mesh Type ######################
    !!############### Convert Mesh Type ######################
    !call MPIData%getLapTime(comment="GetleafMesh")
    ! Convert .msh to .scf
    call leaf%ConvertMesh2Scf(MPIData,ElementType=ElemType)

    call leaf%FEMDomain%checkconnectivity(fix=.true.)
    call leaf%Convert3Dto2D()

    !call leaf%Convert2Dto3D(Thickness=0.10d0,division=4)
    
    call leaf%SetScale(scalex=7.10d0,scaley=113.350d0)
    !!############### Convert Mesh Type ######################
    !!############### Convert Mesh Type ######################
    !!############### Convert Mesh Type ######################
    SolverName="DiffusionEq_"
    call leaf%SetSolver(InSolverType=SolverName)
    call leaf%SetUp(NoFacetMode=.true.)
    call leaf%Reverse()
    call leaf%Convert2Dto3D(Thickness=0.250d0,division=4)
    call leaf%showMesh(Name="leaf")
    !!############### Setup Material Info ######################
    !!############### Setup Material Info ######################
    !!############### Setup Material Info ######################

    call leaf%SetMatPara(MaterialID=1,ParameterID=1,Val=0.00010d0)
    call leaf%SetMatID( MaterialID=1)

    !!############### Setup Material Info ######################
    !!############### Setup Material Info ######################
    !!############### Setup Material Info ######################
    
    

    !!############### Setup Boundary Condition ######################
    !!############### Setup Boundary Condition ######################
    !!############### Setup Boundary Condition ######################
    call leaf%SetSizeOfBC(Dirichlet=.true. , NumOfValue=1)
    call leaf%SetBC(Dirichlet=.true., ymax=-1.0d0, ymin=-25.0d0,  val=10.0d0,val_id=1)
    call leaf%SetBC(Dirichlet=.true., ymax=-100.0d0,             val=40.0d0,val_id=1)
    call leaf%SetSizeOfBC(Neumann=.true. , NumOfValue=3)
    !call leaf%SetBC(Neumann=.true., zmax=0.0d0, val=-1.0d0,val_id=1)
    !call leaf%SetBC(Neumann=.true., zmax=0.0d0, val=-1.0d0,val_id=2)
    !call leaf%SetBC(Neumann=.true., zmax=0.0d0, val=-1.0d0,val_id=3)
    
    
    call leaf%SetSizeOfBC(Initial=.true. , NumOfValue=1)
    !call leaf%SetBC(Initial=.true.,   val=20.0d0, val_id=1)
    call leaf%SetBC(Initial=.true., ymax=-2.6d0,  ymin=-30.20d0, val=30.0d0,val_id=1)
    call leaf%SetBC(Initial=.true., ymax=-30.20d0,ymin=-58.20d0, val=20.0d0,val_id=1)
    call leaf%SetBC(Initial=.true., ymax=-58.20d0,               val=10.0d0,val_id=1)
    
    call leaf%SetControlPara(OptionalItrTol=100,OptionalTimestep=100,OptionalSimMode=1)
        
    !!############### Setup Boundary Condition ######################
    !!############### Setup Boundary Condition ######################
    !!############### Setup Boundary Condition ######################

    ! Export Object
    call leaf%Export(Name="Tutorial/InputData/grass_leaf")
    call leaf%showMesh(Name="leaf_", withNeumannBC=.true.) 
    
    call MPIData%End()

end program 
