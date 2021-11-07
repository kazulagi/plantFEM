import numpy as np
import pyvista as pv

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
    SubMeshNodFromTo = np.array([],dtype=np.int32)
    SubMeshElemFromTo = np.array([],dtype=np.int32)
    SubMeshSurfFromTo = np.array([],dtype=np.int32)
    surface=1

    #for Interfaces
    uuid = "None"
    ElemType="None"
    ErrorMsg="None"
    meshtype="None"
    
    def __init__(self):
        print("initialized")
    
    def read(self,filename):
        self.vtkdata = pv.read(filename)
        
        self.NodCoord = self.vtkdata.points
        self.ElemNod  = self.vtkdata.cells
        ncell         = self.vtkdata.n_cells
        
        vsize = self.ElemNod.shape
        
        self.ElemNod = self.ElemNod.reshape(int(ncell),int(int(vsize[0])/int(ncell)))
        self.ElemNod = np.delete(self.ElemNod,0,1)
        
        self.ElemMat = np.zeros(self.ElemNod.shape[0] )
            

    def plot(self):
        self.vtkdata.plot()
        #self['new array'] = np.full(self.vtkdata, 5)
        
    
    def save(self,filename,binary=True):
        self.vtkdata.save(filename,binary)
        #self['new array'] = np.full(self.vtkdata, 5)
    def create(this,meshtype="None",x_num,y_num,x_len,y_len,Le,Lh,Dr,thickness, \
        division,smooth,top,margin,inclineRate,shaperatio,master,\
        slave,x,y,z,dx,dy,dz,coordinate):
        if meshtype.lower()=="Bar1D" :
    #        # initialize
    #        this = Mesh()
