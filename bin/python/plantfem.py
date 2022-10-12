import os


class soybean:
    config = "Tutorial/playon_obj/realSoybeanConfig.json"
    name = "untitled"
    code ="program main\n use plantFEM\n implicit none\n type(Soybean_) :: soy\n"
    script = "server.f90"
    
    def create(self, config="Tutorial/playon_obj/realSoybeanConfig.json"):
        self.config = config
        self.code = self.code + "\n call soy%init(config='"+self.config+"')"
        print("Config-file : "+  self.config)
    
    def msh(self, name="untitled"):
        self.name = name
        self.code = self.code + "\n call soy%msh(name='"+self.name+"')"
        print("Export >> "+self.name+".msh ")
        
    def run(self):
        self.code = self.code + "\n end program"
        f = open(self.script, "w")
        f.write(self.code)
        f.close()
        #os.system("./plantfem run")

        
    