program prepro
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
   character * 200 :: name,ElemType,SolverName
   call MPIData%Start()
   call term%Init()
   ElemType = 'LinearRectangularGp4'
   
   call InfileList%Init(1)
   call InfileList%Input(1,'Tutorial/LeafImageData/grass_traced.png')
   name = InfileList%Get(MPIData%MyRank+1)
   print *, "My_rank : ",MPIData%MyRank,"InfileName : ",trim(name)
   call Root%Init(Default=.true.)
   call Root%ImportPictureName(name)
   call Root%GetPixcelSize(MPIData)
   call Root%SetColor(0,128,0)
   call Root%ConvertMesh2Scf(MPIData,ElementType=ElemType)
   call Root%Convert3Dto2D()
   call Root%SetScale(scalex=7.0d0,scaley=113.0d0)
   SolverName="DiffusionEq_"
   call Root%SetSolver(InSolverType=SolverName)
   call Root%SetUp(NoFacetMode=.true.)
   call Root%Reverse()
   call Root%Convert2Dto3D(Thickness=0.250d0,division=4)
   call Root%SetMatPara(MaterialID=1,parameterID=1,Val=1.0d0)
   call Root%SetMatID(MaterialID=1,Xmin=-1000000000000.00d0,&
Xmax=1000000000000.00d0,&
Ymin=-1000000000000.00d0,&
Ymax=1000000000000.00d0,&
Zmin=-1000000000000.00d0,&
Zmax=1000000000000.00d0,&
Tmin=-1000000000000.00d0,&
Tmax=1000000000000.00d0)
   call Root%SetSizeOfBC(Dirichlet=.true.,NumOfValue=1)
   call Root%SetBC(Dirichlet=.true., val=0.00d0,val_id=1,Xmin=-1000000000000.00d0,&
Xmax=1000000000000.00d0,&
Ymin=-25.00d0,&
Ymax=-1.00d0,&
Zmin=-1000000000000.00d0,&
Zmax=1000000000000.00d0,&
Tmin=-1000000000000.00d0,&
Tmax=1000000000000.00d0)
   call Root%SetControlPara(OptionalItrTol=100,OptionalTimestep=100,OptionalSimMode=1)
   call Root%Export(Name="Tutorial/LeafImageData/test")
   
   
   
   
   
   
   call MPIData%End()
end program prepro
