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
   
