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
   call Root%GetPixcelByRGB(MPIData,err=5,onlycoord=.true.)
   call Root%GetSurfaceNode(MPIData)
   call Root%AssembleSurfaceElement(MPIData,dim=2,threshold=5,DelRange=5)
   call Root%ReduceSize(MPIData,interval=10)
   call Root%ExportGeoFile(MPIData)
   
   
   
   
   
   
   
   
   call MPIData%End()
end program prepro
