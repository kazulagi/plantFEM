import os
#import src.FEMDomainClass.FEMDomainClass


# functions:
def install():
    os.system("python3 install.py")

def run(script="run"):
    os.system("plantfem "+script)
    return 0


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
#                os.system("python3 setup.py")
#                os.system("./plantfem install")
#                os.chdir(cdir)
#                os.system("mpif90 "+path+"/inc/*o server.f90 -fopenmp -fopenacc -g -fcheck=all  -fintrinsic-modules-path "+path+"/inc/")
#                os.system("mpirun --allow-run-as-root ./a.out ")
#                
#            else:
#                print("Aborted. Please install plantfem by")
#                print("     git clone https://github.com/kazulagi/plantfem.git")
#                print("     cd ./plantfem")
#                print("     python3 setup.py")
#                print("     ./plantfem install")
#
#                
#            
#        
#    