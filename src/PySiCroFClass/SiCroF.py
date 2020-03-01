import os
import sys
import platform
import datetime

dt_now = datetime.datetime.now()
print("-------------------")
print("Imported at ")
print(dt_now)
print("-------------------")
print("Detecting OS type...")
pf=platform.system()
if pf == 'Windows':
    print("OS : Windows")
    print("Now installing...")
    #os.system("install.bat")
    print("Please use Windows Subsystem Linux(WSL) ")
    print("Successfully Installed!!")
elif pf == "Darwin":
    print("OS : macOS")
    print("Now installing...")
    #os.system("sh ./setup/setup_macOS")
    #os.system("sh ./install/install_macOS")
    print("Successfully Installed!!")
elif pf == "Linux":
    print("OS : Linux")
    print("Now installing...")
    #os.system("sh ./setup/setup")
    #os.system("sh ./install/install")
    print("Successfully Installed!!")
else:
    print("OS : Unknown ")

inc=".\inc\ArrayOperationClass.o  .\inc\BoundaryConditionClass.o  .\inc\ConstitutiveModelClass.o  .\inc\ContactMechanicsClass.o  .\inc\ControlParameterClass.o  .\inc\DictionaryClass.o  .\inc\DiffusionEquationClass.o  .\inc\FEMDomainClass.o  .\inc\FEMIfaceClass.o  .\inc\FieldClass.o  .\inc\FiniteDeformationClass.o  .\inc\LinearSolverClass.o  .\inc\MPIClass.o  .\inc\MaterialPropClass.o  .\inc\MathClass.o  .\inc\MeshOperationClass.o  .\inc\MultiDiffDeformClass.o  .\inc\MultiPhysicsClass.o  .\inc\OpenMPClass.o  .\inc\PostProcessingClass.o  .\inc\PreProcessingClass.o  .\inc\ShapeFunctionClass.o  .\inc\SiCroFClass.o  .\inc\SimulatorClass.o  .\inc\SpaceTimeDeformClass.o  .\inc\TermClass.o  .\inc\TreeClass.o "

class routeOpt:
    def __init__(self):
        print("Initialized.")
    
    def getOptimizedRoute(self,FieldList,OptRoute):
        prog=open("getOptimizedRoute.f90","w")
        prog.write("program main\n")
        prog.write("  use RouteOptimization")
        prog.write("  use MPIClass")
        prog.write("  implicit none")
        prog.write("  type(MPI_) :: mpid")
        prog.write("  type(RouteOptimization_) :: obj")
        prog.write("  ! start mpi")
        prog.write("  call mpid%"+"start()")
        prog.write("  ! get points from file")
        prog.write("  call obj%"+"import('Tutorial/RouteOptimization/coord1.txt')")
        prog.write("  ! optimize route by solving a TS problem.")
        prog.write('  call obj%'+'run(SolverName="TSP_enum_greedy_roop",NumOfPoints=59)')
        prog.write('  call obj%'+'run(SolverName="TSP_enum_greedy_roop",NumOfPoints=59)')
        prog.write('  ! export route')
        prog.write('  call obj%'+'export(Repository="Tutorial/RouteOptimization")')
        prog.write('  !close mpi')
        prog.write('  call mpid%end()')
        prog.write('end program main  ')
        prog.close()

        print("OS : Linux")
        os.system("mpif90 inc/*.o getOptimizedRoute.f90\n")
        print("Optimizing route...")
        os.system("mpirun ./a.out\n")
        print("Successfully done!")

    

class prepro:

    def __init__(self):
        self.state=int(1)
        self.ObjectList=list()
        self.RGBList=list()
        self.ScaleXYZList=list()
        
        self.rotation=list()
        self.shift=list()
        self.Domain=list()

        # Material Parameters
        self.MaterialID=list()
        self.MatParaParameterID=list()
        self.MatParaVal=list()


        # Material ID for Elements
        self.MatID=list()
        self.MatIDRange=list()

        # D-Boundary Condition for node
        self.DBCRange=list()
        self.DBCVal=list()
        self.DBCvalid=list()
        
        # N-Boundary Condition for node
        self.NBCRange=list()
        self.NBCVal=list()
        self.NBCvalid=list()
        
        # T-Boundary Condition for node
        self.TBCRange=list()
        self.TBCVal=list()
        self.TBCvalid=list()
        
        self.NumOfDBCValue=1
        self.NumOfNBCValue=1
        self.NumOfTBCValue=1

        self.Thickness=1.0
        self.division=1

        self.PixelError=5
        self.ReduceNodeInterval=10
        self.ProjName="log_prepro"
        self.SolverName="FiniteDeform_"
        self.DomainName="untitled"

        self.RGBList.append(0)
        self.RGBList.append(0)
        self.RGBList.append(0)

        self.ScaleXYZList.append(1.00)
        self.ScaleXYZList.append(1.00)
        self.ScaleXYZList.append(1.00)


    def update(self,blender="on"):
        
        if blender=="on":
            path=os.getcwd()
            print(path)
            
        else :
            return
    
    def importDomain(self,Name):
        self.DomainName=Name
        os.system("cp "+Name+".scf"+" buf_"+Name+".scf")
        print("cp "+Name+".scf"+" buf_"+Name+".scf")
        f=open("buf_"+str(self.DomainName+"_import.f90"),"w")
        f.write("program import\n")
        f.write("   use SiCroF\n")
        f.write("   implicit none\n")
        f.write("   type(FEMDomain_)::obj\n")
        f.write('   call obj%'+'import(OptionalProjectName="buf_'+self.DomainName+'")\n')
        f.write("   call obj%"+"GmshPlotMesh(Name='"+self.DomainName+"',withNeumannBC=.true.,withDirichletBC=.true.)\n")
        f.write("   call obj%"+"export(OptionalProjectName='"+"buf_"+self.DomainName+"')\n")
        f.write("end program \n")
        f.close()

        if pf=="Windows":
            os.system("gfortran -o a.exe "+inc+" buf_"+str(self.DomainName+"_import.f90") )
            os.system("a.exe\n")
            
        elif pf=="Darwin":
            os.system("mpif90 inc/*.o "+"buf_"+str(self.DomainName+"_import.f90") )
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(self.DomainName+"_import.f90"))
        
        elif pf=="Linux":
            os.system("mpif90 inc/*.o "+"buf_"+str(self.DomainName+"_import.f90") +" ")
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(self.DomainName+"_import.f90"))
        
        else:
            print("OS : Unknown ")

    def importImg(self,Name):
        self.DomainName=Name
        os.system("cp "+Name+".scf"+" buf_"+Name+".scf")
        print("cp "+Name+".scf"+" buf_"+Name+".scf")
        f=open("buf_"+str(self.DomainName+"_import.f90"),"w")
        f.write("program import\n")
        f.write("   use SiCroF\n")
        f.write("   implicit none\n")
        f.write("   type(FEMDomain_)::obj\n")
        f.write('   call obj%'+'import(OptionalProjectName="buf_'+self.DomainName+'")\n')
        f.write("   call obj%"+"GmshPlotMesh(Name='"+self.DomainName+"',withNeumannBC=.true.,withDirichletBC=.true.)\n")
        f.write("   call obj%"+"export(OptionalProjectName='"+"buf_"+self.DomainName+"')\n")
        f.write("end program \n")
        f.close()

        if pf=="Windows":
            os.system("gfortran -o a.exe "+inc+" buf_"+str(self.DomainName+"_import.f90") )
            os.system("a.exe\n")
            
        elif pf=="Darwin":
            os.system("mpif90 inc/*.o "+"buf_"+str(self.DomainName+"_import.f90") )
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(self.DomainName+"_import.f90"))
        
        elif pf=="Linux":
            os.system("mpif90 inc/*.o "+"buf_"+str(self.DomainName+"_import.f90") +" ")
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(self.DomainName+"_import.f90"))
        
        else:
            print("OS : Unknown ")
        
    
    def setProjName(self, ProjName):
        self.ProjName=ProjName

    def setNumOfCore(self, NumCore):
        print("NumOfCore : "+str(NumCore))
        self.NumCore=int(NumCore)

    def addObject(self, path):
        print("ObjectID : "+str(len(self.ObjectList))+" ObjectFilePath : "+path)
        self.ObjectList.append(path)
        print("Now, total number of objects is : "+str(len(self.ObjectList)))

    def removeObject(self, path):
        print("ObjectFilePath : "+path+" is removed.")
        self.ObjectList.remove(path)
        print("Now, total number of objects is : "+str(len(self.ObjectList)))

    def setRGBColor(self, R=0,G=0,B=0):
        self.RGBList[0]=int(R)
        self.RGBList[1]=int(G)
        self.RGBList[2]=int(B)

    def setThickness(self, Thickness=1.0):
        self.Thickness=Thickness

    def set3DDivision(self, division=1.0):
        self.division=division

    def setScaleXYZ(self, X=0,Y=0,Z=0):
        self.ScaleXYZList[0]=float(X)
        self.ScaleXYZList[1]=float(Y)
        self.ScaleXYZList[2]=float(Z)

    def rotate(self, X=0.0,Y=0.0,Z=0.0):
        f=open("buf_"+str(self.DomainName+"_rotate.f90"),"w")
        f.write("program rotate\n")
        f.write("   use SiCroF\n")
        f.write("   implicit none\n")
        f.write("   type(FEMDomain_)::obj\n")
        f.write('   call obj%'+'import(OptionalProjectName="buf_'+self.DomainName+'")\n')
        f.write("   call obj%"+"rotate(x="+str(float(X))+"0d0, y="+str(float(Y))+"0d0, z="+str(float(Z))+"0d0)\n")
        f.write("   call obj%"+"GmshPlotMesh(Name='"+"buf_"+self.DomainName+"',withNeumannBC=.true.,withDirichletBC=.true.)\n")
        f.write("   call obj%"+"export(OptionalProjectName='"+"buf_"+self.DomainName+"')\n")
        f.write("end program \n")
        f.close()



        if pf=="Windows":
            os.system("gfortran -o a.exe "+inc+" buf_"+str(self.DomainName+"_rotate.f90") )
            os.system("a.exe\n")
            
        elif pf=="Darwin":
            os.system("mpif90 inc/*.o "+"buf_"+str(self.DomainName+"_rotate.f90") )
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(self.DomainName+"_rotate.f90"))
        
        elif pf=="Linux":
            os.system("mpif90 inc/*.o "+"buf_"+str(self.DomainName+"_rotate.f90") +" ")
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(self.DomainName+"_rotate.f90"))
        
        else:
            print("OS : Unknown ")


    def move(self, X=0.0,Y=0.0,Z=0.0):
        f=open("buf_"+str(self.DomainName+"_move.f90"),"w")
        f.write("program move\n")
        f.write("   use SiCroF\n")
        f.write("   implicit none\n")
        f.write("   type(FEMDomain_)::obj\n")
        f.write('   call obj%'+'import(OptionalProjectName="buf_'+self.DomainName+'")\n')
        f.write("   call obj%"+"move(x="+str(float(X))+"0d0, y="+str(float(Y))+"0d0, z="+str(float(Z))+"0d0)\n")
        f.write("   call obj%"+"GmshPlotMesh(Name='"+"buf_"+self.DomainName+"',withNeumannBC=.true.,withDirichletBC=.true.)\n")
        f.write("   call obj%"+"export(OptionalProjectName='"+"buf_"+self.DomainName+"')\n")
        f.write("end program \n")
        f.close()



        if pf=="Windows":

            os.system("gfortran -o a.exe "+inc+" buf_"+str(self.DomainName+"_move.f90"))
            os.system("a.exe\n")
           
        elif pf=="Darwin":

            os.system("mpif90 inc/*.o "+"buf_"+str(self.DomainName+"_move.f90") )
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(self.DomainName+"_move.f90"))
        
        elif pf=="Linux":

            os.system("mpif90 inc/*.o "+"buf_"+str(self.DomainName+"_move.f90") +" ")
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(self.DomainName+"_move.f90"))
        
        else:
            print("OS : Unknown ")


    def save(self):
        os.system("cp "+self.DomainName+".scf "+self.DomainName+"~"+".scf")
        os.system("cp "+" buf_"+self.DomainName+".scf "+self.DomainName+".scf")
    
    def copy(self,Name=""):
        os.system("cp "+self.DomainName+".scf "+Name+".scf")
        os.system("cp "+" buf_"+self.DomainName+".scf "+Name+"~.scf")

    def saveas(self, Name=""):

        self.copy(Name)
        f=open(str(Name+"_saveas.f90"),"w")
        f.write("program saveas\n")
        f.write("   use SiCroF\n")
        f.write("   implicit none\n")
        f.write("   type(FEMDomain_)::obj\n")
        f.write('   call obj%'+'import(OptionalProjectName="'+self.DomainName+'")\n')
        f.write("   call obj%"+"GmshPlotMesh(Name='"+Name+"',withNeumannBC=.true.,withDirichletBC=.true.)\n")
        f.write("   call obj%"+"export(OptionalProjectName='"+Name+"')\n")
        f.write("end program \n")
        f.close()


        if pf=="Windows":
            os.system("gfortran -o a.exe "+inc+" "+str(Name+"_saveas.f90"))
            os.system("a.exe\n")
            
        elif pf=="Darwin":
            os.system("mpif90 inc/*.o "+str(Name+"_saveas.f90") )
            os.system("./a.out\n")
            os.system("rm -f "+str(Name+"_saveas.f90"))
        
        
        elif pf=="Linux":
            os.system("mpif90 inc/*.o "+str(Name+"_saveas.f90") +" ")
            os.system("./a.out\n")
            os.system("rm -f "+str(Name+"_saveas.f90"))
        
        
        else:
            print("OS : Unknown ")

    def undo(self):
        os.system("cp "+self.DomainName+"~.scf "+self.DomainName+".scf")
        os.system("cp "+self.DomainName+"~.scf buf_"+self.DomainName+".scf")

    def setPixelError(self, PixelError):
        self.PixelError=PixelError

    def setSolver(self, SolverName):
        self.SolverName=SolverName
        if SolverName=="DiffusionEq_" :
            self.NumOfDBCValue=1
            self.NumOfNBCValue=3
            self.NumOfTBCValue=1
        elif SolverName=="FiniteDeform_":
            self.NumOfDBCValue=3
            self.NumOfNBCValue=3
            self.NumOfTBCValue=20 #????
        else:
            print("ERROR setSolver:: no such solver as "+SolverName)
            
        # Now, SolverName=DiffusionEq_ or FiniteDeform_

    def setMatPara(self,MaterialID=1,MatParaParameterID=1,MatParaVal=1.0):
        self.MaterialID.append(MaterialID)
        self.MatParaParameterID.append(MatParaParameterID)
        self.MatParaVal.append(MatParaVal)

    def setMatID(self,MatID=1,Xmin=-1.0e+12,Xmax=1.0e+12,Ymin=-1.0e+12,Ymax=1.0e+12,Zmin=-1.0e+12,Zmax=1.0e+12,Tmin=-1.0e+12,Tmax=1.0e+12):
        range_of_coord=[Xmin,Xmax,Ymin,Ymax,Zmin,Zmax,Tmin,Tmax]
        self.MatID.append(MatID)
        self.MatIDRange.append(range_of_coord)

    def setDBC(self,DBCvalid=1,DBCVal=1.0,Xmin=-1.0e+12,Xmax=1.0e+12,Ymin=-1.0e+12,Ymax=1.0e+12,Zmin=-1.0e+12,Zmax=1.0e+12,Tmin=-1.0e+12,Tmax=1.0e+12):
        range_of_coord=[Xmin,Xmax,Ymin,Ymax,Zmin,Zmax,Tmin,Tmax]
        
        # D-Boundary Condition for node
        self.DBCRange.append(range_of_coord)
        self.DBCVal.append(DBCVal)
        self.DBCvalid.append(DBCvalid)
    
    def setNBC(self,NBCvalid=1,NBCVal=1.0,Xmin=-1.0e+12,Xmax=1.0e+12,Ymin=-1.0e+12,Ymax=1.0e+12,Zmin=-1.0e+12,Zmax=1.0e+12,Tmin=-1.0e+12,Tmax=1.0e+12):
        range_of_coord=[Xmin,Xmax,Ymin,Ymax,Zmin,Zmax,Tmin,Tmax]
        
        # D-Boundary Condition for node
        self.NBCRange.append(range_of_coord)
        self.NBCVal.append(NBCVal)
        self.NBCvalid.append(NBCvalid)
    
    def setTBC(self,TBCvalid=1,TBCVal=1.0,Xmin=-1.0e+12,Xmax=1.0e+12,Ymin=-1.0e+12,Ymax=1.0e+12,Zmin=-1.0e+12,Zmax=1.0e+12,Tmin=-1.0e+12,Tmax=1.0e+12):
        range_of_coord=[Xmin,Xmax,Ymin,Ymax,Zmin,Zmax,Tmin,Tmax]
        
        # D-Boundary Condition for node
        self.TBCRange.append(range_of_coord)
        self.TBCVal.append(TBCVal)
        self.TBCvalid.append(TBCvalid)
        
        
    def setReduceNodeInterval(self, ReduceNodeInterval):
        self.ReduceNodeInterval=ReduceNodeInterval

    def exportDomainAsScf(self,FileName=""):
        self.FileName=FileName

    def exportDomainAsSTL(self,FileName=""):
        Name=FileName+self.DomainName
        print("Exporting "+Name+".stl >>> ")
        
        os.system   ("cp "+self.DomainName+".scf "+Name+".scf")
        f=open("buf_"+str(Name+"_exportasstl.f90"),"w")
        f.write("program exportasstl\n")
        f.write("   use SiCroF\n")
        f.write("   implicit none\n")
        f.write("   type(FEMDomain_)::obj\n")
        f.write('   call obj%'+'import(OptionalProjectName="'+Name+'")\n')
        f.write("   call obj%"+"export(OptionalFileFormat='.stl' ,OptionalProjectName='"+Name+"')\n")
        f.write("end program \n")
        f.close()


        if pf=="Windows":
            os.system("gfortran -o a.exe "+inc+" buf_"+str(Name+"_exportasstl.f90") )
            os.system("a.exe\n")
            
        
        elif pf=="Darwin":
            os.system("mpif90 inc/*.o "+"buf_"+str(Name+"_exportasstl.f90") )
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(Name+"_exportasstl.f90"))

        
        elif pf=="Linux":
            os.system("mpif90 inc/*.o "+"buf_"+str(Name+"_exportasstl.f90") +" ")
            os.system("./a.out\n")
            os.system("rm -f "+"buf_"+str(Name+"_exportasstl.f90"))

        
        else:
            print("OS : Unknown ")

    def exportFortranScript(self):
        f=open(str(self.ProjName+"_1.f90"),"w")
        f.write("program prepro\n")
        f.write("   use TermClass\n")
        f.write("   use DictionaryClass\n")
        f.write("   use PreprocessingClass\n")
        f.write("   use PostProcessingClass\n")
        f.write("   implicit none\n")
        
        f.write("   type(Dictionary_)       :: InfileList   \n")
        f.write("   type(MPI_)              :: MPIData  \n")
        f.write("   type(PreProcessing_)    :: Root,Soil \n")
        f.write("   type(Term_)             :: term \n")

        f.write("   integer :: ans\n")
        f.write("   character * 200 :: name,ElemType,SolverName\n")
        

        f.write("   call MPIData%Start()\n")
        f.write("   call term%Init()\n")
        
        f.write("   ElemType = 'LinearRectangularGp4'\n")
        f.write('   \n')
        

        f.write("   call InfileList%Init("+str(len(self.ObjectList))+")\n")
        
        for i in range(len(self.ObjectList) ):
            f.write("   call InfileList%Input("+str(i+1)+",'"+self.ObjectList[i]+"')\n")


        f.write("   name = InfileList%"+"Get(MPIData%MyRank+1)\n")
        f.write('   print *, "My_rank : ",MPIData%MyRank,"InfileName : ",trim(name)\n')
        f.write('   call Root%Init(Default=.true.)\n')
        f.write('   call Root%ImportPictureName(name)\n')
        f.write('   call Root%'+'GetPixcelSize(MPIData)\n')
        f.write('   call Root%SetColor('+str(self.RGBList[0])+',')
        f.write(str(self.RGBList[1])+','+str(self.RGBList[2])+')\n')
        f.write('   call Root%'+'GetPixcelByRGB(MPIData,err='+str(self.PixelError)+',onlycoord=.true.)\n')
        f.write('   call Root%'+'GetSurfaceNode(MPIData)\n')
        f.write('   call Root%AssembleSurfaceElement(MPIData,dim=2,threshold='+str(self.PixelError)+',DelRange='+str(self.PixelError)+')\n')
        f.write('   call Root%ReduceSize(MPIData,interval='+str(self.ReduceNodeInterval)+')\n')
        
        f.write('   call Root%'+'ExportGeoFile(MPIData)\n')

        #f.write('   call Root%ConvertGeo2Msh(MPIData)\n')
        #f.write('   call Root%ConvertGeo2Inp(MPIData)\n')
        #f.write('   call Root%ConvertGeo2Mesh(MPIData)\n')
        #f.write('   call Root%ConvertMesh2Scf(MPIData,ElementType=ElemType)\n')
        #f.write('   call Root%Convert3Dto2D()\n')
        #f.write('   call Root%SetScale(scalex='+str(self.ScaleXYZList[0])+'0d0,scaley='+str(self.ScaleXYZList[1])+'0d0)\n')    
        #f.write('   SolverName="'+self.SolverName+'"\n')
        #f.write('   call Root%SetSolver(InSolverType=SolverName)\n')
        #f.write('   call Root%SetUp(NoFacetMode=.true.)\n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')

        f.write("   call MPIData%"+"End()\n")
        f.write("end program prepro\n")
        f.close()
        print("Successfully exported : "+str(self.ProjName+"_1.f90"))



        f=open(str(self.ProjName+"_2.f90"),"w")
        f.write("program prepro\n")
        f.write("   use TermClass\n")
        f.write("   use DictionaryClass\n")
        f.write("   use PreprocessingClass\n")
        f.write("   use PostProcessingClass\n")
        f.write("   implicit none\n")
        
        f.write("   type(Dictionary_)       :: InfileList   \n")
        f.write("   type(MPI_)              :: MPIData  \n")
        f.write("   type(PreProcessing_)    :: Root,Soil \n")
        f.write("   type(Term_)             :: term \n")

        f.write("   integer :: ans\n")
        f.write("   character * 200 :: name,ElemType,SolverName\n")
        

        f.write("   call MPIData%Start()\n")
        f.write("   call term%Init()\n")
        
        f.write("   ElemType = 'LinearRectangularGp4'\n")
        f.write('   \n')
        

        f.write("   call InfileList%Init("+str(len(self.ObjectList))+")\n")
        
        for i in range(len(self.ObjectList) ):
            f.write("   call InfileList%Input("+str(i+1)+",'"+str(self.ObjectList[i])+"')\n")


        f.write("   name = InfileList%"+"Get(MPIData%MyRank+1)\n")
        f.write('   print *, "My_rank : ",MPIData%MyRank,"InfileName : ",trim(name)\n')
        f.write('   call Root%Init(Default=.true.)\n')
        f.write('   call Root%ImportPictureName(name)\n')
        f.write('   call Root%'+'GetPixcelSize(MPIData)\n')
        f.write('   call Root%SetColor('+str(self.RGBList[0])+',')
        f.write(str(self.RGBList[1])+','+str(self.RGBList[2])+')\n')
        #f.write('   call Root%'+'GetPixcelByRGB(MPIData,err='+str(self.PixelError)+',onlycoord=.true.)\n')
        #f.write('   call Root%'+'GetSurfaceNode(MPIData)\n')
        #f.write('   call Root%AssembleSurfaceElement(MPIData,dim=2,threshold='+str(self.PixelError)+',DelRange='+str(self.PixelError)+')\n')
        #f.write('   call Root%ReduceSize(MPIData,interval='+str(self.ReduceNodeInterval)+')\n')
        
        #f.write('   call Root%'+'ExportGeoFile(MPIData)\n')

        #f.write('   call Root%ConvertGeo2Msh(MPIData)\n')
        #f.write('   call Root%ConvertGeo2Inp(MPIData)\n')
        #f.write('   call Root%ConvertGeo2Mesh(MPIData)\n')
        f.write('   call Root%ConvertMesh2Scf(MPIData,ElementType=ElemType)\n')
        f.write('   call Root%'+'FEMDomain%'+'checkconnectivity(fix=.true.)\n')
        f.write('   call Root%Convert3Dto2D()\n')
        f.write('   call Root%SetScale(scalex='+str(self.ScaleXYZList[0])+'0d0,scaley='+str(self.ScaleXYZList[1])+'0d0)\n')    
        f.write('   SolverName="'+str(self.SolverName)+'"\n')
        f.write('   call Root%SetSolver(InSolverType=SolverName)\n')
        f.write('   call Root%SetUp(NoFacetMode=.true.)\n')
        f.write('   call Root%Reverse()\n')
        f.write('   call Root%Convert2Dto3D(Thickness='+str(self.Thickness)+'0d0,division='+str(self.division)+')\n')
        
        for i in range(len(self.MaterialID)-1 ):
            f.write("   call Root%SetMatPara(MaterialID="+str(self.MaterialID[i])+",")
            f.write("parameterID="+str(self.MatParaParameterID[i] )+",")
            f.write("Val="+str(self.MatParaVal[i])+"0d0)\n") 

        for i in range(len(self.MatID) ):
            range_of_coord= self.MatIDRange[i]
            f.write("   call Root%SetMatID(MaterialID="+str(self.MatID[i])+",")
            f.write("Xmin="+str(range_of_coord[0])+"0d0,&\n")
            f.write("Xmax="+str(range_of_coord[1])+"0d0,&\n")
            f.write("Ymin="+str(range_of_coord[2])+"0d0,&\n")
            f.write("Ymax="+str(range_of_coord[3])+"0d0,&\n")
            f.write("Zmin="+str(range_of_coord[4])+"0d0,&\n")
            f.write("Zmax="+str(range_of_coord[5])+"0d0,&\n")
            f.write("Tmin="+str(range_of_coord[6])+"0d0,&\n")
            f.write("Tmax="+str(range_of_coord[7])+"0d0")
            f.write(")\n") 

        for i in range(len(self.DBCVal) ):
            
            
            if i==0:
                f.write("   call Root%SetSizeOfBC(Dirichlet=.true.,NumOfValue="+str(self.NumOfDBCValue) +")\n")
            
            range_of_coord= self.DBCRange[i]
            
            f.write("   call Root%SetBC(Dirichlet=.true., val="+str(self.DBCVal[i])+"0d0,")
            f.write("val_id="+str(self.DBCvalid[i])+",")
            f.write("Xmin="+str(range_of_coord[0])+"0d0,&\n")
            f.write("Xmax="+str(range_of_coord[1])+"0d0,&\n")
            f.write("Ymin="+str(range_of_coord[2])+"0d0,&\n")
            f.write("Ymax="+str(range_of_coord[3])+"0d0,&\n")
            f.write("Zmin="+str(range_of_coord[4])+"0d0,&\n")
            f.write("Zmax="+str(range_of_coord[5])+"0d0,&\n")
            f.write("Tmin="+str(range_of_coord[6])+"0d0,&\n")
            f.write("Tmax="+str(range_of_coord[7])+"0d0")
            f.write(")\n") 
        
        for i in range(len(self.NBCVal) ):
            if i==0:
                f.write("   call Root%SetSizeOfBC(Neumann=.true.,NumOfValue="+str(self.NumOfNBCValue) +")\n")
            range_of_coord= self.NBCRange[i]
            f.write("   call Root%SetBC(Neumann=.true., val="+str(self.NBCVal[i])+"0d0,")
            f.write("val_id="+str(self.NBCvalid[i])+",")
            f.write("Xmin="+str(range_of_coord[0])+"0d0,&\n")
            f.write("Xmax="+str(range_of_coord[1])+"0d0,&\n")
            f.write("Ymin="+str(range_of_coord[2])+"0d0,&\n")
            f.write("Ymax="+str(range_of_coord[3])+"0d0,&\n")
            f.write("Zmin="+str(range_of_coord[4])+"0d0,&\n")
            f.write("Zmax="+str(range_of_coord[5])+"0d0,&\n")
            f.write("Tmin="+str(range_of_coord[6])+"0d0,&\n")
            f.write("Tmax="+str(range_of_coord[7])+"0d0")
            f.write(")\n")

        for i in range(len(self.TBCVal) ):
            if i==0:
                f.write("   call Root%SetSizeOfBC(Initial=.true.,NumOfValue="+str(self.NumOfTBCValue) +")\n")
            range_of_coord= self.TBCRange[i]
            f.write("   call Root%SetBC(Initial=.true., val="+str(self.TBCVal[i])+"0d0,")
            f.write("val_id="+str(self.DBCvalid[i])+",")
            f.write("Xmin="+str(range_of_coord[0])+"0d0,&\n")
            f.write("Xmax="+str(range_of_coord[1])+"0d0,&\n")
            f.write("Ymin="+str(range_of_coord[2])+"0d0,&\n")
            f.write("Ymax="+str(range_of_coord[3])+"0d0,&\n")
            f.write("Zmin="+str(range_of_coord[4])+"0d0,&\n")
            f.write("Zmax="+str(range_of_coord[5])+"0d0,&\n")
            f.write("Tmin="+str(range_of_coord[6])+"0d0,&\n")
            f.write("Tmax="+str(range_of_coord[7])+"0d0")
            f.write(")\n")
            
        #for i in range(len(self.MaterialList) ):
        #    f.write("   call Root%SetMatPara(MaterialID="+str(i+1)+",ParameterID="+self.MaterialList[i]+"')\n")

        #f.write('   call Root%SetMatPara(MaterialID=.true.)\n')
        #f.write('   call Root%SetUp(NoFacetMode=.true.)\n')
        f.write('   call Root%SetControlPara(OptionalItrTol=100,OptionalTimestep=100,OptionalSimMode=1)\n')
        
        f.write('   call Root%'+'Export(Name="'+str(self.FileName)+'")\n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')
        f.write('   \n')

        f.write("   call MPIData%"+"End()\n")
        f.write("end program prepro\n")
        f.close()
        print("Successfully exported : "+str(self.ProjName+"_2.f90"))


    def run(self,option="None"):

        if option=="None" or option=="none":
            pf=platform.system()
            if os.name=="nt":
                print("OS : Windows")
                self.exportFortranScript()
                os.system("gfortran -o a.exe " + inc +" " +str(self.ProjName+"_1.f90") )
                os.system("a.exe\n")
                os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                os.system("gfortran -o a.exe "+ inc +" " +str(self.ProjName+"_2.f90") )
                print("Running preprocessing...")
                os.system( "a.exe\n")
                print("Successfully done!")
            elif os.name=="posix":
                if pf == "Darwin":
                    print("OS : Mac")
                    self.exportFortranScript()
                    os.system("mpif90 inc/*.o "+str(self.ProjName+"_1.f90") +" ")
                    os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                    os.system("mpif90 inc/*.o "+str(self.ProjName+"_2.f90")+" ")
                    print("Running preprocessing...")
                    os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    print("Successfully done!")
                elif pf == "Linux":
                    print("OS : Linux")
                    self.exportFortranScript()
                    os.system("mpif90 inc/*.o "+str(self.ProjName+"_1.f90") +" ")
                    os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                    os.system("mpif90 inc/*.o "+str(self.ProjName+"_2.f90")+" ")
                    print("Running preprocessing...")
                    os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    print("Successfully done!")
                else:
                    print("No OS is identified")    
            else:
                print("No OS is identified")
        elif option=="OnlyMesh" or option=="Onlymesh" or option=="onlyMesh" or option=="onlymesh" :
            pf=platform.system()
            if os.name=="nt":
                print("OS : Windows")
                self.exportFortranScript()
                os.system("gfortran -o a.exe " + inc +" " +str(self.ProjName+"_1.f90") )
                os.system("a.exe\n")
                #os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                #os.system("gfortran -o a.exe "+ inc +" " +str(self.ProjName+"_2.f90") )
                print("Running preprocessing...")
                os.system( "a.exe\n")
                print("Successfully done!")
            elif os.name=="posix":
                if pf == "Darwin":
                    print("OS : Mac")
                    self.exportFortranScript()
                    os.system("mpif90 inc/*.o "+str(self.ProjName+"_1.f90") +" ")
                    os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    #os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                    #os.system("mpif90 inc/*.o "+str(self.ProjName+"_2.f90")+" ")
                    print("Running preprocessing...")
                    #os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    print("Successfully done!")
                elif pf == "Linux":
                    print("OS : Linux")
                    self.exportFortranScript()
                    os.system("mpif90 inc/*.o "+str(self.ProjName+"_1.f90") +" ")
                    os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    #os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                    #os.system("mpif90 inc/*.o "+str(self.ProjName+"_2.f90")+" ")
                    print("Running preprocessing...")
                    #os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    print("Successfully done!")
                else:
                    print("No OS is identified")    
            else:
                print("No OS is identified")
        elif option=="withoutMesh" or option=="Withoutesh" or option=="WithoutMesh" or option=="withoutmesh":
            pf=platform.system()
            if os.name=="nt":
                print("OS : Windows")
                self.exportFortranScript()
                #os.system("gfortran -o a.exe " + inc +" " +str(self.ProjName+"_1.f90") )
                #os.system("a.exe\n")
                #os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                os.system("gfortran -o a.exe "+ inc +" " +str(self.ProjName+"_2.f90") )
                print("Running preprocessing...")
                os.system( "a.exe\n")
                print("Successfully done!")
            elif os.name=="posix":
                if pf == "Darwin":
                    print("OS : Mac")
                    self.exportFortranScript()
                    #os.system("mpif90 inc/*.o "+str(self.ProjName+"_1.f90") +" ")
                    #os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    #os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                    os.system("mpif90 inc/*.o "+str(self.ProjName+"_2.f90")+" ")
                    print("Running preprocessing...")
                    os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    print("Successfully done!")
                elif pf == "Linux":
                    print("OS : Linux")
                    self.exportFortranScript()
                    #os.system("mpif90 inc/*.o "+str(self.ProjName+"_1.f90") +" ")
                    #os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    #os.system('gmsh *.geo -2 -algo del2d -clmin 100 -format mesh')
                    os.system("mpif90 inc/*.o "+str(self.ProjName+"_2.f90")+" ")
                    print("Running preprocessing...")
                    os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                    print("Successfully done!")
                else:
                    print("No OS is identified")    
            else:
                print("No OS is identified")
            
        #os.system("rm "+str(self.ProjName+"_1.f90"))


class solver:

    def __init__(self):
        self.state=int(1)
        self.NumCore=1
        self.TimeStep=1
        self.SimulationTime=1.00
        self.ProjName="log_solver"
        self.SolverType="GaussJordan"
    

    def importDomains(self, domainlistname):
        print("DomainList : "+domainlistname+"\n")
        self.NumOfDomain=0
        self.DomainList=domainlistname
    
    def importInterfaces(self, Ifacelistname):
        print("IfaceList : "+Ifacelistname+"\n")
        self.NumOfIface=0
        self.IfaceList=Ifacelistname


    def setProjName(self, ProjName):
        self.ProjName=ProjName
    
    def setNumOfCore(self, NumCore):
        print("NumOfCore : "+str(NumCore))
        self.NumCore=int(NumCore)
    
    
    def setSimulationTime(self, SimulationTime):
        print("SimulationTime : "+str(SimulationTime)+"sec\n")
        self.SimulationTime=float(SimulationTime)

    def setTimeStep(self, TimeStep):
        print("TimeStep : "+str(TimeStep)+" steps\n")
        self.TimeStep=int(TimeStep)
    
    def setLinearSolver(self,SolverName):
        print("LinearSolverType : "+str(SolverName)+"\n")
        self.SolverType=SolverName
    
    def exportFortranScript(self):
        f=open(str(self.ProjName+".f90"),"w")
        f.write("program main\n")
        f.write("   use SimulatorClass\n")
        f.write("   implicit none\n")
        f.write("   type(MPI_)              :: MPIData\n")
        f.write("   type(Field_),target     :: world\n")
        f.write("   integer                 :: TotalStep="+str(self.TimeStep)+"\n" )
        f.write("   real(8)                 :: time="+str(self.SimulationTime)+"\n")
        f.write("   call MPIData%Start()\n")
        f.write('   call world%Import(OptionalDomainListName="'+str(self.DomainList)+'",&\n')
        f.write('   OptionalIfaceListName="'+str(self.IfaceList)+'")\n')
        f.write('   call Simulator(world,OptionalStep=TotalStep,OptionalTime=time,'+'SolverType="'+ str(self.SolverType) +'"  )\n')
        f.write("   call world%"+"Export()\n")
        f.write("   call MPIData%"+"End()\n")
        f.write("end program main\n")
        f.close()
        print("Successfully exported : "+str(self.ProjName+".f90"))

    def run(self):

        pf=platform.system()
        if os.name=="nt":
            self.exportFortranScript()
            os.system("gfortran -o a.exe "+ inc +" " +str(self.ProjName+".f90"))
            os.system("a.exe\n")
            os.system("rm "+str(self.ProjName+".f90"))

        elif os.name=="posix":
            if pf == "Darwin":
                print("OS : Mac")
                self.exportFortranScript()
                os.system("mpif90 inc/*.o "+str(self.ProjName+".f90"))
                print("Now Running Simulation...")
                os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                print("Simulation is done!")
                print("Please Download ***.pos files to check results")
                
                os.system("rm "+str(self.ProjName+".f90"))

            elif pf == "Linux":
                self.exportFortranScript()
                os.system("mpif90 inc/*.o "+str(self.ProjName+".f90")+" ")
                print("Now Running Simulation...")
                os.system("mpirun -np "+str(self.NumCore)+" ./a.out\n")
                print("Simulation is done!")
                print("Please Download ***.pos files to check results")
                #os.system("rm "+str(self.ProjName+".f90"))
            else:
                print("No OS is identified")                
        else:
            print("No OS is identified")

        
class pospro:
    def __init__(self):
        self.state=int(1)
        self.ProjName="pospro"



class SiCroF(prepro,solver,pospro):

    def __init__(self):
        self.ProjName="log_SiCroF"
        self.prepro=prepro()
        self.solver=solver()
        self.pospro=pospro()
    
    def install(self):
        print("Detecting OS type...")
        pf=platform.system()
        if pf == 'Windows':
            print("OS : Windows")
            print("Now installing...")
            os.system("install.bat")
            print("Successfully Installed!!")
        elif pf == "Darwin":
            print("OS : macOS")
            print("Now installing...")
            os.system("sh ./install/install_macOS")
            print("Successfully Installed!!")
        elif pf == "Linux":
            print("OS : Linux")
            print("Now installing...")
            os.system("sh ./install/install")
            print("Successfully Installed!!")
        else:
            print("OS : Unknown ")
        
    


