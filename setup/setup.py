import os
import sys
import platform

#import setuptools
#from setuptools import setup, find_packages
# 
#setuptools.setup(
#    name="plantfem",
#    version="1.0",
#    author="kazulagi",
#    author_email="kazulagi@gmail.com",
#    description="plantfem as a python package",
#    long_description="plantfem as a python package",
#    long_description_content_type="text/markdown",
#    url="https://github.com/kazulagi/plantfem",
#    packages=find_packages(where='./'), 
#    classifiers=[
#        "Programming Language :: Python :: 3.6.0",
#        "License :: OSI Approved :: MIT License",
#        "Operating System :: OS Independent",   
#    ]
#)

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
    os.system("sh ./setup/setup_macOS")
    os.system("sh ./bin/setpath.sh")
    print("Successfully Installed!!")
elif pf == "Linux":
    print("OS : Linux")
    print("Now installing...")
    os.system("sh ./setup/setup")
    os.system("sh ./bin/setpath.sh")
    print("Successfully Installed!!")
else:
    print("OS : Unknown ")
