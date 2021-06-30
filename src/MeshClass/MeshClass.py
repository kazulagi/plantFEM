import numpy as np

class Mesh(object):
        # Name
        FileName=" "
        # Nodal coordinates
        NodCoord = np.array([])
        # Connectivity information for FE-mesh
        ElemNod = np.array([],dtype=np.int32)
        # Material IDs for Finite Elements
        ElemMat = np.array([],dtype=np.int32)

        MasterID = np.array([],dtype=np.int32)
        SlaveID = np.array([],dtype=np.int32)
        NTSMasterFacetID = np.array([],dtype=np.int32)
        xi = np.array([],dtype=np.float)

        # optional data;
        NodCoordInit = np.array([])
        BottomElemID = np.array([],dtype=np.int32)
        TopElemID = np.array([],dtype=np.int32)
        FacetElemNod = np.array([],dtype=np.int32)
        NextFacets = np.array([],dtype=np.int32)
        SurfaceLine2D(:) = np.array([],dtype=np.int32)
        SubMeshNodFromTo = np.array([],dtype=np.int32)
        SubMeshElemFromTo = np.array([],dtype=np.int32)
        SubMeshSurfFromTo = np.array([],dtype=np.int32)


        integer(int32) :: surface=1

        #for Interfaces
        GlobalNodID(:) = np.array([],dtype=np.int32)

        character(len=36) :: uuid
        character*70::ElemType=" "
        character*70:: ErrorMsg=" "
        character*70:: meshtype
    
    def __init__(self):
        
    def create(this,meshtype="None",x_num,y_num,x_len,y_len,Le,Lh,Dr,thickness, \
        division,smooth,top,margin,inclineRate,shaperatio,master,slave,x,y,z,dx,dy,dz,coordinate):
        
        if meshtype.lower()=="root" :
            # initialize
            this = Mesh()
            
            
            
        
        