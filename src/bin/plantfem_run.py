#!/bin/python3

import os
import sys
import platform
import argparse
import time

#!/usr/bin/env python
# -*- coding:utf-8 -*-
import argparse

# coding=utf-8


def yes_no_input():
    while True:
        #return True
        choice = input("Do you install plantFEM? 'yes' or 'no' [Y/n]: ").lower()
        if choice in ['y', 'ye', 'yes']:
            return True
        elif choice in ['n', 'no']:
            return False


def get_args():
    psr = argparse.ArgumentParser()
    psr.add_argument('-m', '--mode', help='use other compiler instead of mpif90')
    #psr.add_argument('-v', '--version', help='use other compiler instead of mpif90')
    psr.add_argument('-s', '--script', help='script for mpif90')
    psr.add_argument('-np', '--num_of_process', help='number of process mpif90')
    psr.add_argument('-f', '--filename', help='IO-file name')
    return psr.parse_args()

if __name__ == '__main__':
    args = get_args()
    
    #print(args.mode)
    #print(args.bravo)
    #print(args.charlie)
    #print(args.delta)
    expath=os.path.expandvars(".")
    ofiles = os.path.exists("/opt/plantfem/inc/obj.o") 

    
    if str(args.script) == "install":
        ofiles = True

    if ofiles == False:
        if yes_no_input():
            os.system("python3 /opt/plantfem/install.py")
            print("[ok] plantFEM has been installed.")
        else:
            print("[Caution] plantFEM is not installed yet.\n")
            time.sleep(1)
    else:
        print("[ok] plantFEM has been installed.")

    print("Detecting OS type...")
    pf=platform.system()
    if pf == 'Windows':
        print("OS : Windows")
        print("Please use Windows Subsystem Linux(WSL) ")
        os.system("./Interactive/plantfem.bat")
    elif pf == "Darwin":
        print("OS : macOS")
        #os.system("sh ./Interactive/plantfem_macOS")
        aout = os.path.exists("a.out")
        #print(aout)
        if aout == True:
            os.system("rm ./a.out")

        if args.script is None:
            print("Interactive Mode :: \n")
            print("\n")
            if args.mode == "gfortran":
                os.system("/opt/plantfem/Interactive/plantfem_gfortran ")
            else:
                os.system("/opt/plantfem/Interactive/plantfem ")
        #else :
        #    os.system("sh /opt/plantfem/Interactive/plantfem_run_macOS " + str(args.script))
        else :
            if args.filename is None:
                os.system("/opt/plantfem/Interactive/plantfem_run " + str(args.script))
            else:
                os.system("/opt/plantfem/Interactive/plantfem_run " + str(args.script)+" " + str(args.filename) )
        
    elif pf == "Linux":
        print("OS : Linux")
        aout = os.path.exists("a.out")
        #print(aout)

        if aout == True:
            os.system("rm ./a.out")

        if args.script is None:
            print("Interactive Mode :: \n")
            print("\n")
            if args.mode == "gfortran":
                os.system("/opt/plantfem/Interactive/plantfem_gfortran ")
            else:
                os.system("/opt/plantfem/Interactive/plantfem ")
        else :
            if args.filename is None:
                os.system("/opt/plantfem/Interactive/plantfem_run " + str(args.script))
            else:
                os.system("/opt/plantfem/Interactive/plantfem_run " + str(args.script)+" " + str(args.filename) )
    else:
        print("OS : Unknown ")