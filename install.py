import os
import platform

def install_plantFEM():
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

        if os.path.exists("/opt/plantfem/inc/obj.o"):
            print("plantFEM is already built.")
        else:
            os.system("sh "+str(os.path.abspath("./"))+"/install/install")

        if os.path.exists("/opt/plantfem"):
            print("plantFEM is already installed.")
        else:
            os.system("sudo ln -si $PWD /opt/plantfem")


        if os.path.exists("/usr/local/bin/plantfem"):
            print("plantFEM is already installed.")
        else:
            os.system("sudo ln -si $PWD/plantfem /usr/local/bin/plantfem")

        if os.path.exists("/usr/local/bin/plantfem/soja"):
            print("soja (package manager of plantFEM) is already installed.")
        else:
            os.system("sudo ln -si "+str(os.path.abspath("./"))+"/bin/soja.sh /usr/local/bin/soja")

        os.system("pip3 install -U plantfem")
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
        if os.path.exists("/opt/plantfem/inc/obj.o"):
            print("plantFEM is already built.")
        else:
            os.system("sh "+str(os.path.abspath("./"))+"/install/install")

        if os.path.exists("/usr/local/bin/plantfem"):
            print("plantFEM is already installed.")
        else:
            os.system("sudo ln -si "+str(os.path.abspath("./"))+"/plantfem /usr/local/bin/plantfem")

        if os.path.exists("/usr/local/bin/soja"):
            print("soja (package manager of plantFEM) is already installed.")
        else:
            os.system("sudo ln -si "+str(os.path.abspath("./"))+"/bin/soja.sh /usr/local/bin/soja")
        #os.system("sudo ln -si "+str(os.path.abspath("./"))+"/bin/plantfem_run.py /usr/local/bin/plantfem_run.py")
        #os.system("sudo ln -si "+str(os.path.abspath("./"))+"/Interactive/plantfem_run /usr/local/bin/plantfem_run")
        os.system("sudo ln -si $PWD /opt/plantfem")
        os.system("rm -f plantFEM")

        #os.system("sudo pip3 install -U plantfem")
        print("Successfully Installed!!")
    else:
        print("OS : Unknown ")
    
if __name__ == "__main__":
    install_plantFEM()