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
   
   call InfileList%Init(4)
   call InfileList%Input(1,'debug/scandata/case1GM.png')
   call InfileList%Input(2,'debug/scandata/case2GM.png')
   call InfileList%Input(3,'debug/scandata/case4GM.png')
   call InfileList%Input(4,'debug/scandata/case3GM.png')
   name = InfileList%Get(MPIData%MyRank+1)
   print *, "My_rank : ",MPIData%MyRank,"InfileName : ",trim(name)
   call Root%Init(Default=.true.)
   call Root%ImportPictureName(name)
   call Root%GetPixcelSize(MPIData)
   call Root%SetColor(28,255,255)
   call Root%GetPixcelByRGB(MPIData,err=10,onlycoord=.true.)
   call Root%GetSurfaceNode(MPIData)
   call Root%AssembleSurfaceElement(MPIData,dim=2,threshold=10,DelRange=10)
   call Root%ReduceSize(MPIData,interval=10)
   call Root%ConvertGeo2Msh(MPIData)
   call Root%ConvertGeo2Inp(MPIData)
   call Root%ConvertGeo2Mesh(MPIData)
   call Root%ConvertMesh2Scf(MPIData,ElementType=ElemType)
   call Root%Convert3Dto2D()
   call Root%SetScale(scalex=00d0,scaley=00d0)
   SolverName="FiniteDeform_"
   call Root%SetSolver(InSolverType=SolverName)
   call Root%SetUp(NoFacetMode=.true.)
   
   
   
   
   
   
   
   
   call MPIData%End()
end program prepro
