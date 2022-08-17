from ctypes import sizeof
import os
import numpy as np




class femdomain:
    
    def __init__(self,meshtype="None",x_num=10,y_num=10,z_num=10,x=1.0,y=1.0,z=1.0):
        self.meshtype = meshtype
        self.nodcoord = []
        self.elemnod = []
        if meshtype == "None":
            return
        elif meshtype == "Cube":
            x_axis=np.linspace(0.0,x,num=x_num+1)
            y_axis=np.linspace(0.0,y,num=y_num+1)
            z_axis=np.linspace(0.0,z,num=z_num+1)
            
            self.createCube(x_axis=x_axis,y_axis=y_axis,z_axis=z_axis)

    def x(self):
        return self.nodcoord[:,0]

    def createCube(self,x_axis,y_axis,z_axis):
        
        x_num = x_axis.size-1
        y_num = y_axis.size-1
        z_num = z_axis.size-1
        xn = x_num
        yn = y_num
        zn = z_num

        self.nodcoord = np.zeros( ((xn+1)*(yn+1)*(zn+1),3) )
        n = 0
        for z_idx in range(zn+1):
            for y_idx in range(yn+1):
                for x_idx in range(xn+1):
                    self.nodcoord[n,0] = x_axis[x_idx]
                    self.nodcoord[n,1] = y_axis[y_idx]
                    self.nodcoord[n,2] = z_axis[z_idx]
                    n = n + 1

        self.elemnod = np.zeros( ((xn)*(yn)*(zn),8),dtype=int )
        n = 0
        for z_idx in range(zn):
            for y_idx in range(yn):
                for x_idx in range(xn):
                    self.elemnod[n,0] = x_idx     + y_idx*(xn+1)
                    self.elemnod[n,1] = x_idx + 1 + y_idx*(xn+1)
                    self.elemnod[n,2] = xn + 2 + x_idx + y_idx*(xn+1)
                    self.elemnod[n,3] = xn + 1 + x_idx + y_idx*(xn+1)
                    self.elemnod[n,4] = x_idx     + y_idx*(xn+1) + (z_idx+1)*(xn+1)*(yn+1)
                    self.elemnod[n,5] = x_idx + 1 + y_idx*(xn+1) + (z_idx+1)*(xn+1)*(yn+1)
                    self.elemnod[n,6] = xn + 2 + x_idx + y_idx*(xn+1) + (z_idx+1)*(xn+1)*(yn+1)
                    self.elemnod[n,7] = xn + 1 + x_idx + y_idx*(xn+1) + (z_idx+1)*(xn+1)*(yn+1)
                    n = n + 1
    def nn(self):
        return self.nodcoord.shape[0]

    def nd(self):
        return self.nodcoord.shape[1]

    def ne(self):
        return self.elemnod.shape[0]

    def nne(self):
        return self.elemnod.shape[1]

    def vtk(self, name):
        if  self.elemnod.shape[1] == 1 :
            self.VTK_CELL_TYPE=1 # point
        elif self.nodcoord.shape[1] ==2 and self.elemnod.shape[1]==3:
            self.VTK_CELL_TYPE=5 # triangle
        elif self.nodcoord.shape[1] ==2 and self.elemnod.shape[1]==4:
            self.VTK_CELL_TYPE=9 # square
        elif self.nodcoord.shape[1] ==3 and self.elemnod.shape[1]==4:
            self.VTK_CELL_TYPE=10 # 4-node triangle
        elif self.nodcoord.shape[1] ==3 and self.elemnod.shape[1]==8:
            self.VTK_CELL_TYPE=12 # 8-node box
        else:
            print("VTKFEMDomain >> ERROR :: Nothing is exported.")
            return 
        vtk_file = open(name + ".vtk", "w")
        vtk_file.write("# vtk DataFile Version 2.0 \n")
        vtk_file.write(name + "\n")
        vtk_file.write("ASCII \n")
        vtk_file.write("DATASET UNSTRUCTURED_GRID\n")
        vtk_file.write("POINTS "+str( self.nn() )+" float\n")
        
        for i in range(self.nn()):
            for j in range(self.nd()-1):
                vtk_file.write(str(self.nodcoord[i,j]) + " ")
            vtk_file.write(str(self.nodcoord[i,self.nd()-1] ) + "\n" )
    	
        vtk_file.write("CELLS "+str(self.ne())+" "+str(self.ne()* (self.nne()+1) ) + "\n")
        for i in range( self.ne()):
            num_node = self.nne()
            if self.VTK_CELL_TYPE==1:
                num_node = 1
            elif self.VTK_CELL_TYPE==5:
                num_node = 3
            elif self.VTK_CELL_TYPE==9:
                num_node = 4
            elif self.VTK_CELL_TYPE==10:
                num_node = 4
            elif self.VTK_CELL_TYPE==12:
                num_node = 8
            elif self.VTK_CELL_TYPE==13:
                num_node = 6
            elif self.VTK_CELL_TYPE==14:
                num_node = 4
    		
            vtk_file.write( str(num_node ) + " ")
            for j in range( num_node-1):
                vtk_file.write( str(self.elemnod[i,j])+" ")
            vtk_file.write( str(self.elemnod[i, num_node-1] ) + "\n")
    	
        vtk_file.write("CELL_TYPES "+str(self.ne() ) + "\n" )
        for i in range( self.ne()):
            vtk_file.write(str(self.VTK_CELL_TYPE)  + "\n" )
        vtk_file.close()
    
    def size(self,dim_num):
        if dim_num == 0:
            return max(cube.nodcoord[:,0]) - min(cube.nodcoord[:,0])
        elif dim_num == 1:
            return max(cube.nodcoord[:,1]) - min(cube.nodcoord[:,1])
        elif dim_num == 2:
            return max(cube.nodcoord[:,2]) - min(cube.nodcoord[:,2])
        
    def resize(self,x=0,y=0,z=0):
        x_len = max(cube.nodcoord[:,0]) - min(cube.nodcoord[:,0])
        y_len = max(cube.nodcoord[:,1]) - min(cube.nodcoord[:,1])
        z_len = max(cube.nodcoord[:,2]) - min(cube.nodcoord[:,2])

        if x != 0.0:
            cube.nodcoord[:,0] = x/x_len*cube.nodcoord[:,0]
        if y != 0.0:
            cube.nodcoord[:,1] = y/y_len*cube.nodcoord[:,1]
        if z != 0.0:
            cube.nodcoord[:,2] = z/z_len*cube.nodcoord[:,2]

if __name__ == "__main__":
    cube = femdomain(meshtype="Cube",x_num=100,y_num=10,z_num=10)
    cube.resize(x=10)
    cube.vtk("mesh")