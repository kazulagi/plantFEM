import os
import sys
import platform


print("Detecting OS type...")
pf=platform.system()
if pf == 'Windows':
    print("OS : Windows")
    print("Please use Windows Subsystem Linux(WSL) ")
    os.system("./Interactive/SiCroF.bat")
elif pf == "Darwin":
    print("OS : macOS")
    os.system("sh ./Interactive/SiCroF_macOS")
elif pf == "Linux":
    print("OS : Linux")
    os.system("sh ./Interactive/SiCroF")
else:
    print("OS : Unknown ")