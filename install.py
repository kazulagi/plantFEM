import os
import sys
import platform

def install_plantFEM():
    # selecting compiler type::
    # Default: mpif90 (gfortran)
    build_script = "/install/install"
    args = sys.argv
    for i in range(len(args) ):
        if "--compiler=" in args[i]:
            compiler = str(args[i]).replace("--compiler=","")
            if "intel" in compiler:
                build_script = "/install/install_ifort"
                break
            if "ifx" in compiler:
                build_script = "/install/install_ifort"
                break
            if "ifort" in compiler:
                build_script = "/install/install_ifort"
                break

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
            os.system("sh "+str(os.path.abspath("./"))+build_script)

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
            os.system("sh "+str(os.path.abspath("./"))+build_script)
        
        if not os.path.isfile("inc/obj.o"):
            print("[COMPILE ERROR]")
            return

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
        if os.path.isfile("inc/obj.o"):
            print("Successfully Installed!!")
        else:
            print("[COMPILE ERROR]")
    else:
        print("OS : Unknown ")
    
if __name__ == "__main__":
    install_plantFEM()