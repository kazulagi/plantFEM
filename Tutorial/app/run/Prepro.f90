program Preprocessing
    use TermClass
    use DictionaryClass
    use PreprocessingClass
    use PostProcessingClass

    implicit none
    
    type(Dictionary_)       :: InfileList
    type(MPI_)              :: MPIData
    type(PreProcessing_)    :: Root,Soil
    type(Term_)             :: term
    integer :: ans
    character * 200         :: name,name1,name2,name3,name4,ElemType,SolverName
    call MPIData%Start()
    call term%Init()

    ElemType = "LinearRectangularGp4"
    name1="debug/scandata/case1GM.png"
    name2="debug/scandata/case2GM.png"
    name3="debug/scandata/case3GM.png"
    name4="debug/scandata/case4GM.png"

    ! load file names
    call InfileList%Init(4) ! Constractor
    call InfileList%Input(1,name1)
    call InfileList%Input(2,name2)
    call InfileList%Input(3,name3)
    call InfileList%Input(4,name4)

    name = InfileList%Get(MPIData%MyRank+1)
    print *, "My_rank : ",MPIData%MyRank,"InfileName : ",trim(name)

    name = "inputdata.jpg"
    ! Get Pixcel
    call Root%Init(Default=.true.)  ! Constractor
    call Root%ImportPictureName(name)
    call Root%GetPixcelSize(MPIData)
    call Root%SetColor(28,255,255)
    !call Root%SetColor(255,142,28)
    call Root%GetPixcelByRGB(MPIData,err=10,onlycoord=.true.)

    !call MPIData%getLapTime(comment="GetRGB")

    ! Get Outline
    call Root%GetSurfaceNode(MPIData)
    !call MPIData%getLapTime(comment="GetSurfaceNode")

    call Root%AssembleSurfaceElement(MPIData,dim=2,threshold=10,DelRange=10)
    !call MPIData%getLapTime(comment="AssembleSurfaceElement")
    

    ! Reduce Number of Surface Nodes
    call Root%ReduceSize(MPIData,interval=10)


    ! Convert SurfaceNod to .geo
    call Root%ExportGeoFile(MPIData)



    ! Run Gmsh to convert .geo to .msh
    call Root%ConvertGeo2Msh(MPIData)
    call Root%ConvertGeo2Inp(MPIData)
    call Root%ConvertGeo2Mesh(MPIData)
    
   
    !call MPIData%getLapTime(comment="GetRootMesh")
    ! Convert .msh to .scf
    !call Root%ConvertMsh2Scf(MPIData,ElementType=ElemType)
    call Root%ConvertMesh2Scf(MPIData,ElementType=ElemType)
    call Root%Convert3Dto2D()
    !call Root%Convert2Dto3D(division=4)
    call Root%SetScale(scalex=18.0d0,scaley=26.0d0)
    
    SolverName="FiniteDeform_"
    call Root%SetSolver(InSolverType=SolverName)
    call Root%SetUp(NoFacetMode=.true.)

    ! Setup Material ID
    call Root%SetMatPara(NumOfMaterial=2,Val=1.0d0)
    call Root%SetMatPara(NumOfMaterial=2,Val=50.0d0)
    call Root%SetMatID( xmin=2.0d0 ,xmax=40.0d0,              MaterialID=2)
    call Root%SetMatID( xmax=32.0d0,ymax=18.0d0,  MaterialID=3)
    call Root%Reverse()



    call Soil%SetEntity(Rectangle=.true.,Xsize=75.0d0, Ysize=40.0d0,zsize=10.0d0,xloc=-80.0d0,yloc=-41.0d0)
    call Soil%Boolean(Root)
    call Soil%ExportGeoFile(MPIData,Name="Soil")
    call Soil%ConvertGeo2Mesh(MPIData,Name="Soil")
    call Soil%ConvertMesh2Scf(MPIData,ElementType=ElemType,Name="Soil")
    call Soil%Convert3Dto2D()
    
    call Soil%Convert2Dto3D(Thickness=0.20d0,division=4)
    call Root%Convert2Dto3D(Thickness=0.20d0,division=4)
    call Soil%showMesh(Name="Soil")
    call Root%showMesh(Name="Root")


    call MPIData%End()
    stop "Prerfect"

    SolverName="FiniteDeform_"
    call Soil%SetSolver(InSolverType=SolverName)
    call Soil%SetUp(NoFacetMode=.true.)
    call Root%SetSolver(InSolverType=SolverName)
    call Root%SetUp(NoFacetMode=.true.)


    ! Setup Material ID
    call Soil%SetMatPara(NumOfMaterial=3,Val=1.0d0)
    call Soil%SetMatID( ymax=-20.0d0,                 MaterialID=2)
    call Soil%SetMatID( ymin=-20.0d0,   ymax=-10.0d0, MaterialID=2)
    call Soil%SetMatID( ymin = -10.0d0, ymax=0.0d0,   MaterialID=3)
    
    ! Setup Material ID
    call Root%SetMatPara(NumOfMaterial=2,Val=1.0d0)
    call Root%SetMatPara(NumOfMaterial=2,Val=50.0d0)
    call Root%SetMatID( xmin=20.0d0,              MaterialID=2)
    call Root%SetMatID( xmax=15.0d0,ymax=-15.0d0,  MaterialID=3)
    call MPIData%getLapTime(comment="GetSoilMesh")


    call Soil%showMesh(Name="Soil")
    call MPIData%getLapTime(comment="Export Soil Mesh")
    
    call Root%showMesh(Name="Root")
    call MPIData%getLapTime(comment="Export Root Mesh")
    
    call MPIData%showLapTime()
    
    

    ! Setup Boundary Condition
    
    call Root%SetSizeOfBC(Dirichlet=.true. , NumOfValue=4)
    
    call Root%SetBC(Dirichlet=.true., xmin=0.0d0,  val=-0.4d0,val_id=1)
    !call Root%SetBC(Dirichlet=.true., xmin=50.0d0,  val=10.0d0,val_id=2)
    !call Root%SetBC(Dirichlet=.true., ymax= 2.0d0,  val=12.0d0,val_id=3)
    !call Root%SetBC(Dirichlet=.true., ymin= 9.0d0,  val=90.0d0,val_id=4)
    
    call Root%SetSizeOfBC(Neumann=.true. , NumOfValue=2)
    call Root%SetBC(Neumann=.true., xmax=0.0d0, val=0.0d0,val_id=1)
    !call Root%SetBC(Neumann=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    !call Root%SetBC(Neumann=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    !call Root%SetBC(Neumann=.true., ymin=9.0d0,  val=90.0d0,val_id=4)
    
    call Root%SetSizeOfBC(Initial=.true. , NumOfValue=2)
    call Root%SetBC(Initial=.true., xmax=0.0d0, val=0.0d0,val_id=1)
    !call Root%SetBC(Initial=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    !call Root%SetBC(Initial=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    !call Root%SetBC(Initial=.true., ymin=9.0d0,  val=90.0d0,val_id=4)

    call Root%SetControlPara(OptionalTol=1.0d0,OptionalItrTol=100,OptionalTimestep=100,OptionalSimMode=1)
    
    

    ! Setup Boundary Condition
    call Soil%SetSizeOfBC(Dirichlet=.true. , NumOfValue=3)
    call Soil%SetBC(Dirichlet=.true., xmax=-78.0d0, val=0.0d0 ,val_id=1)
    call Soil%SetBC(Dirichlet=.true., xmin=-4.80d0, val=10.0d0,val_id=1)
    call Soil%SetBC(Dirichlet=.true., ymax=-40.0d0,  val=12.0d0,val_id=2)
    
    call Soil%SetSizeOfBC(Neumann=.true. , NumOfValue=2)
    call Soil%SetBC(Neumann=.true., ymax=-40.0d0, val=0.0d0,val_id=1)
    !call Soil%SetBC(Neumann=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    !call Soil%SetBC(Neumann=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    !call Soil%SetBC(Neumann=.true., ymin=9.0d0,  val=90.0d0,val_id=4)
    
    call Soil%SetSizeOfBC(Initial=.true. , NumOfValue=2)
    call Soil%SetBC(Initial=.true., xmax=0.0d0, val=0.0d0,val_id=1)
    !call Soil%SetBC(Initial=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    !call Soil%SetBC(Initial=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    !call Soil%SetBC(Initial=.true., ymin=9.0d0,  val=90.0d0,val_id=4)

    call Soil%SetControlPara(OptionalTol=1.0d0,OptionalItrTol=100,OptionalTimestep=100,OptionalSimMode=1)
    

    ! Export Object
    call Root%Export(MPIData,Name="Root")
    call Soil%Export(MPIData,Name="Soil")

    call MPIData%End()

end program 