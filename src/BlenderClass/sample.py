import bpy
import os
import sys

bl_info = {
    "name": "Do nothing addon",
    "author": "Haruka",
    "version": (2, 0),
    "blender": (2, 80, 0),
    "location": "",
    "description": "sample",
    "warning": "",
    "support": "TESTING",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Object"
}




def register():
    os.chdir("/home/haruka/plantFEM")
    os.getcwd()
    exec(open("./install.py").read() )
    import src.PyplantFEMClass.plantFEM
    sys.path.append("/home/haruka/plantFEM")
    print("hello!")
    print("hello!")
    print("hello!")
    print("hello!")
    print("hello!")
    print("hello!")
    print("hello!")
    print("hello!")


def unregister():
    print("done")
    print("done")
    print("done")
    print("done")
    print("done")
    print("done")
    print("done")


if __name__ == "__main__":
    register()