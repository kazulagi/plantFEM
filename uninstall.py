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
    os.system("sudo /opt/plantfem/bin/compress")
    os.system("sudo unlink /usr/local/bin/plantfem")
    os.system("sudo unlink /usr/local/bin/soja")
    os.system("sudo unlink /opt/plantfem")
    os.system("*.so")
    print("Successfully Uninstalled!!")
elif pf == "Linux":
    print("OS : Linux")
    print("Now installing...")
    #os.system("sh ./setup/setup")
    os.system("sh /opt/plantfem/bin/compress")
    os.system("sudo unlink /usr/local/bin/plantfem")
    os.system("sudo unlink /usr/local/bin/soja")
    os.system("sudo unlink /opt/plantfem")
    print("Successfully Uninstalled!!")
else:
    print("OS : Unknown ")