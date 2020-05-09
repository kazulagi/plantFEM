import os
import sys
import platform
import argparse

#!/usr/bin/env python
# -*- coding:utf-8 -*-
import argparse

'''
引数の定義
-a, -bは必須
-c, -dは任意
'''
def get_args():
    psr = argparse.ArgumentParser()
    psr.add_argument('-m', '--mode', help='use other compiler instead of mpif90')
    #psr.add_argument('-b', '--bravo', required=True, help='A explanation for arg called b')
    #psr.add_argument('-c', '--charlie', help='A explanation for arg called c')
    #psr.add_argument('-d', '--delta', help='A explanation for arg called d')

    return psr.parse_args()

if __name__ == '__main__':
    args = get_args()
    #print(args.mode)
    #print(args.bravo)
    #print(args.charlie)
    #print(args.delta)


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
        if args.mode == "gfortran":
            os.system("sh ./Interactive/SiCroF_gfortran")
        else:
            os.system("sh ./Interactive/SiCroF")
    else:
        print("OS : Unknown ")