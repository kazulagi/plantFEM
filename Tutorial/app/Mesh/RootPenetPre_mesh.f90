program Preprocessing
    use TermClass
    use DictionaryClass
    use PreprocessingClass
    use PostProcessingClass

    implicit none
    
    type(Dictionary_)       :: InfileList
    type(MPI_)              :: MPIData
    type(PreProcessing_)    :: leaf,Soil
    type(Term_)             :: term
    integer :: ans
    character * 200         :: name,name1,name2,name3,name4,ElemType,SolverName
    call MPIData%Start()
    call term%Init()

    ElemType = "LinearRectangularGp4"

    name1="/home/haruka/Dropbox/Paper/JourExpBot2020/"

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
    
    !call MPIData%getLapTime(comment="GetRGB")

    ! Get Outline
    call leaf%GetSurfaceNode(MPIData)
    !call MPIData%getLapTime(comment="GetSurfaceNode")

    call leaf%AssembleSurfaceElement(MPIData,dim=2,threshold=10,DelRange=10)
    !call MPIData%getLapTime(comment="AssembleSurfaceElement")
    

    ! Reduce Number of Surface Nodes
    call leaf%ReduceSize(MPIData,interval=10)


    ! Convert SurfaceNod to .geo
    call leaf%ExportGeoFile(MPIData)



    ! Run Gmsh to convert .geo to .msh
    call leaf%ConvertGeo2Msh(MPIData)
    call leaf%ConvertGeo2Inp(MPIData)
    call leaf%ConvertGeo2Mesh(MPIData)
    
   
    !call MPIData%getLapTime(comment="GetleafMesh")
    ! Convert .msh to .scf
    !call leaf%ConvertMsh2Scf(MPIData,ElementType=ElemType)
    call leaf%ConvertMesh2Scf(MPIData,ElementType=ElemType)
    call leaf%Convert3Dto2D()
    !call leaf%Convert2Dto3D(Thickness=0.10d0,division=4)
    
    call leaf%SetScale(scalex=7.10d0,scaley=113.350d0)


    SolverName="diffusion_"
    call leaf%SetSolver(InSolverType=SolverName)
    call leaf%SetUp(NoFacetMode=.true.)

    ! Setup Material ID
    call leaf%SetMatPara(NumOfMaterial=2,Val=1.0d0)
    call leaf%SetMatPara(NumOfMaterial=2,Val=50.0d0)
    call leaf%SetMatID( xmin=2.0d0 ,xmax=40.0d0,              MaterialID=2)
    call leaf%SetMatID( xmax=32.0d0,ymax=18.0d0,  MaterialID=3)
    call leaf%Reverse()



!    call Soil%SetEntity(Rectangle=.true.,Xsize=75.0d0, Ysize=40.0d0,zsize=10.0d0,xloc=-80.0d0,yloc=-41.0d0)
!    call Soil%Boolean(leaf)
!    call Soil%ExportGeoFile(MPIData,Name="Soil")
!    call Soil%ConvertGeo2Mesh(MPIData,Name="Soil")
!    call Soil%ConvertMesh2Scf(MPIData,ElementType=ElemType,Name="Soil")
!    call Soil%Convert3Dto2D()
    
!    call Soil%Convert2Dto3D(Thickness=0.20d0,division=4)
    call leaf%Convert2Dto3D(Thickness=0.250d0,division=4)
!    call Soil%showMesh(Name="Soil")
    call leaf%showMesh(Name="leaf")

     call Soil%SetSolver(InSolverType=SolverName)
!    call Soil%SetUp(NoFacetMode=.true.)
    !call leaf%SetSolver(InSolverType=SolverName)
   !call leaf%SetUp(NoFacetMode=.true.)

    
    ! Setup Material ID
!    call Soil%SetMatPara(NumOfMaterial=3,Val=1.0d0)
!    call Soil%SetMatID( ymax=-20.0d0,                 MaterialID=2)
!    call Soil%SetMatID( ymin=-20.0d0,   ymax=-10.0d0, MaterialID=2)
!    call Soil%SetMatID( ymin = -10.0d0, ymax=0.0d0,   MaterialID=3)
    
    ! Setup Material ID
    call leaf%SetMatPara(NumOfMaterial=2,Val=1.0d0)
    call leaf%SetMatPara(NumOfMaterial=2,Val=50.0d0)
    call leaf%SetMatID( xmin=20.0d0,              MaterialID=2)
    call leaf%SetMatID( xmax=15.0d0,ymax=-15.0d0,  MaterialID=3)
    call MPIData%getLapTime(comment="GetSoilMesh")


!    call Soil%showMesh(Name="Soil")
    call MPIData%getLapTime(comment="Export Soil Mesh")
    
    call MPIData%getLapTime(comment="Export leaf Mesh")
    
    call MPIData%showLapTime()
    
    

    ! Setup Boundary Condition
    
    call leaf%SetSizeOfBC(Dirichlet=.true. , NumOfValue=1)
    
    call leaf%showMesh(Name="leaf")
    
    return

    call leaf%SetBC(Dirichlet=.true., xmin=-2.0d0,  val=-0.4d0,val_id=1)
    !call leaf%SetBC(Dirichlet=.true., xmin=50.0d0,  val=10.0d0,val_id=2)
    !call leaf%SetBC(Dirichlet=.true., ymax= 2.0d0,  val=12.0d0,val_id=3)
    !call leaf%SetBC(Dirichlet=.true., ymin= 9.0d0,  val=90.0d0,val_id=4)
    

    call leaf%SetSizeOfBC(Neumann=.true. , NumOfValue=3)
    call leaf%SetBC(Neumann=.true., zmax=0.0d0, val=-1.0d0,val_id=1)
    call leaf%SetBC(Neumann=.true., zmax=0.0d0, val=-1.0d0,val_id=2)
    call leaf%SetBC(Neumann=.true., zmax=0.0d0, val=-1.0d0,val_id=3)
    !call leaf%SetBC(Neumann=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    !call leaf%SetBC(Neumann=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    !call leaf%SetBC(Neumann=.true., ymin=9.0d0,  val=90.0d0,val_id=4)

    call leaf%SetSizeOfBC(Initial=.true. , NumOfValue=1)
    call leaf%SetBC(Initial=.true., xmin=-2.0d0,                 val=0.0d0,val_id=1)
    call leaf%SetBC(Initial=.true., xmax=-2.0d0,  xmin=-30.20d0, val=30.0d0,val_id=1)
    call leaf%SetBC(Initial=.true., xmax=-30.20d0,xmin=-58.20d0, val=20.0d0,val_id=1)
    call leaf%SetBC(Initial=.true., xmax=-58.20d0,               val=10.0d0,val_id=1)
    !call leaf%SetBC(Initial=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    !call leaf%SetBC(Initial=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    !call leaf%SetBC(Initial=.true., ymin=9.0d0,  val=90.0d0,val_id=4)

    call leaf%SetControlPara(OptionalTol=1.0d0,OptionalItrTol=100,OptionalTimestep=100,OptionalSimMode=1)
    
    
!    ! Setup Boundary Condition
!    call Soil%SetSizeOfBC(Dirichlet=.true. , NumOfValue=3)
!    call Soil%SetBC(Dirichlet=.true., xmax=-78.0d0, val=0.0d0 ,val_id=1)
!    call Soil%SetBC(Dirichlet=.true., xmin=-4.80d0, val=10.0d0,val_id=1)
!    call Soil%SetBC(Dirichlet=.true., ymax=-40.0d0,  val=12.0d0,val_id=2)
!    
!    call Soil%SetSizeOfBC(Neumann=.true. , NumOfValue=2)
!    call Soil%SetBC(Neumann=.true., ymax=-40.0d0, val=0.0d0,val_id=1)
!    !call Soil%SetBC(Neumann=.true., xmin=50.0d0, val=10.0d0,val_id=2)
!    !call Soil%SetBC(Neumann=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
!    !call Soil%SetBC(Neumann=.true., ymin=9.0d0,  val=90.0d0,val_id=4)
!    
!    call Soil%SetSizeOfBC(Initial=.true. , NumOfValue=2)
!    call Soil%SetBC(Initial=.true., xmax=0.0d0, val=0.0d0,val_id=1)
!    !call Soil%SetBC(Initial=.true., xmin=50.0d0, val=10.0d0,val_id=2)
!    !call Soil%SetBC(Initial=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
!    !call Soil%SetBC(Initial=.true., ymin=9.0d0,  val=90.0d0,val_id=4)
!
!    call Soil%SetControlPara(OptionalTol=1.0d0,OptionalItrTol=100,OptionalTimestep=100,OptionalSimMode=1)
    
      
    ! Export Object
    call leaf%Export(MPIData,Name="leaf")
!    call Soil%Export(MPIData,Name="Soil")

    call MPIData%End()

end program 