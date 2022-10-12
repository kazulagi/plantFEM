# define operatable soybean object
# 
import os
import uuid
import numpy as np
from ..LightClass.LightClass import Light

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





