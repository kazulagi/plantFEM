# prior to this, 
# please install plantfem
import os
import sys

class plantfem:
    # Is plantFEM installed?
    ret = os.system("plantfem hello")
    if int(ret) != 0:
        print("Library package of plantFEM is not installed.")
        ans = input("Can I install the library? [y/N]")
        if ans == "y" or ans == "Y":
            # install
            os.system("git clone https://github.com/kazulagi/plantFEM.git && cd plantFEM && python3 install.py")

        

    # create a Light object
    def Light(self,config):
        return Light(light_angle=90.0,light_direction=180.0)

    # create a Soybean object
    def Soybean(self,config):
        return Soybean(config)

    


# define operatable soybean object
# 
import os
import uuid
import numpy as np

class Soybean():
    
    def __init__(self,config):
        self.__config = config
        self.__uuid   = str(uuid.uuid4() )
        self.__fortran_script_name = str(self.__uuid)+".f90"
        self.__fortran_script = open(self.__fortran_script_name,"w")
        self.__fortran_script.write("use plantfem \n")
        self.__fortran_script.write("implicit none\n")
        
        self.__fortran_script.write("type(Soybean_) :: soybean\n")
        self.__fortran_script.write("type(Light_) :: light\n")
        self.__fortran_script.write("real(real64),allocatable :: ppfd(:)\n")
        self.__fortran_script.write("type(IO_) :: f\n")

        self.__fortran_script.write("call light%"+"init()\n")
        self.__fortran_script.write("call soybean % " + "init(config='"+self.__config +"')\n")
        
    # Soybean methods
    def ppfd(self,light,transparency=0.10):
        
        
        self.__fortran_script.write("light%"+"angles(1)="+str(light.direction())+"d0\n")
        self.__fortran_script.write("light%"+"angles(2)="+str(light.angle())+"d0\n")
        
        self.__fortran_script.write("ppfd = soybean % " + "getPPFD(light=light,transparency="+str(transparency)+"d0)\n")
        self.__fortran_script.write("call f%"+"open('ppfd_"+str(self.__uuid)+".txt', 'w')\n")
        self.__fortran_script.write("call f%"+"write(ppfd)\n")
        self.__fortran_script.write("call f%"+"close()\n")
        return 'ppfd_'+str(self.__uuid)+'.txt'

    def vtk(self,name="untitled",single_file=True,scalar_field="None"):
        self.__name = name
        if single_file:
            export_as_single_file="True"
        else:
            export_as_single_file="False"

        if scalar_field == "None":
            # export only geometry
            self.__fortran_script.write("call soybean % vtk('"+name+"',single_file="+str(export_as_single_file)+")\n")
        else :
            field_type = scalar_field.split("_")
            field_type = field_type[0]
            field_type = str(field_type)
            self.__fortran_script.write("call soybean % vtk('"+name+"',single_file="+str(export_as_single_file)+"&\n")
            self.__fortran_script.write("    ,scalar_field="+field_type+")\n")

    def run(self,num_process=1):
        self.__fortran_script.write("end")
        self.__fortran_script.close()
        self.__num_process = num_process
        command = 'mv '+str(self.__fortran_script_name)+' server.f90'
        print(command)
        os.system('mv '+str(self.__fortran_script_name)+' server.f90')
        os.system('plantfem build')
        if num_process==1:
            os.system("./server.out")
        else:
            os.system('mpirun -np '+str(self.__num_process)+" ./server.out")

    def __del__(self):
        self.run()




class Light():

    def __init__(self,angle=90.0,direction=180.0):
        self.__light_angle = angle
        self.__light_direction = direction
    
    def angle(self):
        return self.__light_angle 

    def direction(self):
        return self.__light_direction 


