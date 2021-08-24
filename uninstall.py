import os
import sys
import platform


print("Detecting OS type...")
pf=platform.system()
if pf == 'Windows':
    print("OS : Windows")
    print("Now installing...")
    os.system("install.bat")
    print("Please use Windows Subsystem Linux(WSL) ")
    print("Successfully unInstalled!!")
elif pf == "Darwin":
    print("OS : macOS")
    print("Now installing...")
    #os.system("sh ./setup/setup_macOS")
    os.system("sh "+str(os.path.abspath("./"))+"/bin/compress")
    os.system("unlink /usr/local/bin/plantfem")
    os.system("sudo unlink /usr/local/bin/soja")
    os.system("unset PF_HOME")
    os.system("*.so")
    print("Successfully Uninstalled!!")
elif pf == "Linux":
    print("OS : Linux")
    print("Now installing...")
    #os.system("sh ./setup/setup")
    os.system("sh "+str(os.path.abspath("./"))+"/bin/compress")
    os.system("sudo unlink /usr/local/bin/plantfem")
    os.system("sudo unlink /usr/local/bin/soja")
    os.system("unset PF_HOME")
    print("Successfully Uninstalled!!")
else:
    print("OS : Unknown ")