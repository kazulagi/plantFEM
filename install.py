import os
import sys
import platform


print("Detecting OS type...")
pf=platform.system()
if pf == 'Windows':
    print("OS : Windows")
    print("Now installing...")
    os.system("install/install.bat")
    print("Please use Windows Subsystem Linux(WSL) ")
    print("Successfully Installed!!")
elif pf == "Darwin":
    print("OS : macOS")
    print("Now installing...")
    #os.system("sh ./setup/setup_macOS")
    os.system("python3 "+str(os.path.abspath("./"))+"/setup/setup.py")
    os.system("sh "+str(os.path.abspath("./"))+"/install/install")
    os.system("sudo ln -si $PWD /opt/plantfem")
    os.system("sudo ln -si $PWD/plantfem /usr/local/bin/plantfem")
    #print("[Next!]  Please type")
    #print("export PATH=$PATH:$PWD")
    #print("export PATH=$PATH:$PWD/bin")
    #print("and press ENTER")
    #os.system("ln -si "+str(os.path.abspath("./"))+"/plantfem /usr/local/bin")
    #os.system("sudo ln -si "+str(os.path.abspath("./"))+"/bin/soja.sh /usr/local/bin/soja")
    print("Successfully Installed!!")
elif pf == "Linux":
    print("OS : Linux")
    print("Now installing...")
    #os.system("sh ./setup/setup")
    os.system("python3 "+str(os.path.abspath("./"))+"/setup/setup.py")
    os.system("sh "+str(os.path.abspath("./"))+"/install/install")
    os.system("sudo ln -si "+str(os.path.abspath("./"))+"/plantfem /usr/local/bin/plantfem")
    os.system("sudo ln -si "+str(os.path.abspath("./"))+"/bin/soja.sh /usr/local/bin/soja")
    #os.system("sudo ln -si "+str(os.path.abspath("./"))+"/bin/plantfem_run.py /usr/local/bin/plantfem_run.py")
    #os.system("sudo ln -si "+str(os.path.abspath("./"))+"/Interactive/plantfem_run /usr/local/bin/plantfem_run")
    os.system("sudo ln -si $PWD /opt/plantfem")
    print("Successfully Installed!!")
else:
    print("OS : Unknown ")