## Format of .scf file
Dtype ["domain" or "interface"] char * 200   
Dtype [e.g. "FiniteDeform_" or "DiffusionEq_"] char * 200   
NumberOfDomain [in which the .scf contains] Int*4   
NodeID_start NodeID_end (for 1st domain) Int*4       
NodeID_start NodeID_end (for 2nd domain) Int*4       
...   
NumberOfElement (oof 1st domain) Int*4    
NumberOfElement (oof 2nd domain) Int*4   
...   
NumberofNode dimension Int*4   
x y z (for 1st node) Real*8   
x y z (for 2nd node) Real*8   
...   
NumberOfElement Int*4      numberOfElementPerElement Int*4   
type [LinearHexahedralGp8/LinearRectangularGp4]  Char     
NodeId NodeId NodeId NodeId ... Real*8   
NodeId NodeId NodeId NodeId ... Real*8      
NodeId NodeId NodeId NodeId ... Real*8      
...   

MaterialTypeID 1 Int*4   
MaterialTypeID 2 Int*4   
MaterialTypeID 3 Int*4   
   
...   
   
NumberOfMaterial Int*4     NumberOfMaterialParameterForEveryMaterials  Int*4   
MatPara MatPara MatPara MatPara ... Int*4   
MatPara MatPara MatPara MatPara ... Int*4   
MatPara MatPara MatPara MatPara ... Int*4   
...

DirichletBoundaryDimension Int*4   
DBNumberForDim1 DBNumberForDim2 ... Int*4   
DB_Dim1_NodeID1 Int*4   
DB_Dim1_NodeID2 Int*4   
DB_Dim1_NodeID3 Int*4   
...   
DB_Dim1_Value1 Real*8   
DB_Dim1_Value2 Real*8   
DB_Dim1_Value3 Real*8   
...   
DB_Dim2_NodeID1 Int*4   
DB_Dim2_NodeID2 Int*4   
DB_Dim2_NodeID3 Int*4   
...   
DB_Dim2_Value1 Real*8   
DB_Dim2_Value2 Real*8   
DB_Dim2_Value3 Real*8   
...   
DB_Dim3_NodeID1 Int*4   
DB_Dim3_NodeID2 Int*4   
DB_Dim3_NodeID3 Int*4   
...   
DB_Dim3_Value1 Real*8   
DB_Dim3_Value2 Real*8   
DB_Dim3_Value3 Real*8   
...   

NeumannBoundaryDimension Int*4   
NBNumberForDim1 NBNumberForDim2 ... Int*4   
NB_Dim1_NodeID1 Int*4   
NB_Dim1_NodeID2 Int*4   
NB_Dim1_NodeID3 Int*4   
...   
NB_Dim1_Value1 Real*8   
NB_Dim1_Value2 Real*8   
NB_Dim1_Value3 Real*8   
...   
NB_Dim2_NodeID1 Int*4   
NB_Dim2_NodeID2 Int*4   
NB_Dim2_NodeID3 Int*4   
...   
NB_Dim2_Value1 Real*8   
NB_Dim2_Value2 Real*8   
NB_Dim2_Value3 Real*8   
...   
NB_Dim3_NodeID1 Int*4   
NB_Dim3_NodeID2 Int*4   
NB_Dim3_NodeID3 Int*4   
...   
NB_Dim3_Value1 Real*8   
NB_Dim3_Value2 Real*8   
NB_Dim3_Value3 Real*8   
...   


Initial(Time)BoundaryDimensionForNode Int*4   
TBoundaryNumber Int*4    
TB_Dim1_NodeID1 Int*4    
TB_Dim1_NodeID2 Int*4    
TB_Dim1_NodeID3 Int*4    
...   
TB_Dim1_Value1 TB_Dim1_Value1 TB_Dim1_Value1 TB_Dim1_Value1 Real*8   
TB_Dim1_Value2 TB_Dim1_Value2 TB_Dim1_Value2 TB_Dim1_Value2 Real*8   
TB_Dim1_Value3 TB_Dim1_Value3 TB_Dim1_Value3 TB_Dim1_Value3 Real*8   


NumberOfInitialBoundaryDimensionForElement Int*4   
NumberOfGaussPoint Int*4   
NumberOfValuePerGaussPoint Int*4   
TB_Dim1_ElementID1 Int*4   
TB_Dim1_ElementID2 Int*4   
TB_Dim1_ElementID3 Int*4   
...   
TB_Dim1_Value1 Real*8    
TB_Dim1_Value1 Real*8    
TB_Dim1_Value1 Real*8    
...   
TB_Dim1_Value2 Real*8    
TB_Dim1_Value2 Real*8    
TB_Dim1_Value2 Real*8    
...   
TB_Dim1_Value3 Real*8    
TB_Dim1_Value3 Real*8    
TB_Dim1_Value3 Real*8    
...   

AnalysisType Int*4 ItrTol Int*4    Step Int*4   