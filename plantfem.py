import os
from src.MeshClass.MeshClass import Mesh
#import src.FEMDomainClass.FEMDomainClass


# functions:

class plantfem:

    def __init__(self,name="untitled"):
        self.scriptname = name
        self.script = open(self.scriptname+".f90","w")
        self.script.write("use plantfem"+"\n")
        self.script.write("implicit none"+"\n")
        self.definition = ""

    def install(self):
        os.system("python3 install.py")
        
    def hello(self,message="hello"):
        self.script.write("call print('"+message+"')\n")

    
    def soybean(self,config="Tutorial/playon_obj/realSoybeanConfig.json ",name="nameless_soybean",x=0.0,y=0.0,z=0.0):
        self.soyname = name
        self.soyconfig = config
        
        self.definition = self.definition + "\n"
        self.definition = self.definition + "type(Soybean_) :: "+self.soyname+"\n"
        
        self.script.write("call "+self.soyname+" "+"%"+" init(config='"+self.soyconfig+"')\n" )
        self.script.write("call "+self.soyname+" "+"%"+" vtk('"+self.soyname+"')\n" )
    
        self.script.write("call "+self.soyname+" "+"%"+" move(x="+str(x)+"d0)\n" )
        self.script.write("call "+self.soyname+" "+"%"+" move(y="+str(y)+"d0)\n" )
        self.script.write("call "+self.soyname+" "+"%"+" move(z="+str(z)+"d0)\n" )
        
    def soil(self,name="nameless_soil",x=0.0,y=0.0,z=0.0):
        self.soilname = name
        
        self.definition = self.definition + "\n"
        self.definition = self.definition + "type(Soil_) :: "+self.soilname+"\n"
        
        self.script.write("call "+self.soilname+" "+"%"+" init()\n" )
        self.script.write("call "+self.soilname+" "+"%"+" vtk('"+self.soilname+"')\n" )
        
        self.script.write("call "+self.soilname+" "+"%"+" move(x="+str(x)+"d0)\n" )
        self.script.write("call "+self.soilname+" "+"%"+" move(y="+str(y)+"d0)\n" )
        self.script.write("call "+self.soilname+" "+"%"+" move(z="+str(z)+"d0)\n" )
        
    def run(self):
        #self.script.write("end")
        self.script.close()
        
        
        with open(self.scriptname+".f90")as f:
            data = f.readlines()

        #3行目に挿入
        data.insert(2, self.definition)

        #元のファイルに書き込み
        with open(self.scriptname+".f90", mode='w')as f:
            f.writelines(data)
        
        os.system("plantfem "+str(self.scriptname)+".f90")



# Regacy codes >>>>
#class script:
#    name = "server.f90"
#    variable_dict =  {}
#    
#    
#
#class soybean:
#    config = "Tutorial/playon_obj/realSoybeanConfig.json"
#    name = "soy"
#    code = ""
#    variable_dict = {}
#    script_list  = []
#    script = "server.f90"
#    def __init__(self, name="soy"):
#        self.name = name
#        
#    def create(self, config="Tutorial/playon_obj/realSoybeanConfig.json"):
#        self.config = config
#        
#        # add variable
#        self.variable_dict[self.name]="Soybean_"
#        # add code
#        self.code = self.code + "\n call "+ self.name+"%init(config='"+self.config+"')"
#        print("Config-file : "+  self.config)
#    
#    def msh(self, name="untitled"):
#        self.code = self.code + "\n call "+ self.name+"%msh(name='"+name+"')"
#    
#    def stl(self, name="untitled"):
#        self.code = self.code + "\n call "+ self.name+"%stl(name='"+name+"')"
#    
#    def json(self, name="untitled"):
#        self.code = self.code + "\n call "+ self.name+"%json(name='"+name+"')"
#    
#    def run(self,path="./plantfem"):
#        f = open(self.script, "w")
#        
#        f.write("program main\n")
#        f.write("use plantfem\n")
#        f.write("implicit none\n")
#        
#        # write variables
#        for key, value in self.variable_dict.items():   
#            f.write("type("+value+") :: "+key+"\n")
#        
#        # write code
#        f.write(self.code+"\n")
#        
#        # end program
#        f.write("end program")
#        
#        f.close()
#        print("Your path to plantfem is : " + path)
#        if os.path.exists(path):
#            os.system("mpif90 "+path+"/inc/*o server.f90 -fopenmp -fopenacc -g -fcheck=all  -fintrinsic-modules-path "+path+"/inc/")
#            os.system("mpirun --allow-run-as-root ./a.out ")
#        else:
#            print("ERROR ::  plantfem is not installed.")
#            Y_or_No = input("Do you want to install plantfem? Y/n")
#            if(Y_or_No == "Y" or Y_or_No == "y"):
#                os.system("git clone https://github.com/kazulagi/plantfem.git")
#                cdir = os.getcwd()
#                os.chdir("./plantfem")
#                os.system("python3 setup/setup.py")
#                os.system("./plantfem install")
#                os.chdir(cdir)
#                os.system("mpif90 "+path+"/inc/*o server.f90 -fopenmp -fopenacc -g -fcheck=all  -fintrinsic-modules-path "+path+"/inc/")
#                os.system("mpirun --allow-run-as-root ./a.out ")
#                
#            else:
#                print("Aborted. Please install plantfem by")
#                print("     git clone https://github.com/kazulagi/plantfem.git")
#                print("     cd ./plantfem")
#                print("     python3 setup/setup.py")
#                print("     ./plantfem install")
#
#                
#            
#        
#    