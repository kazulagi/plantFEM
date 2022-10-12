#!/usr/bin/env python3
# -*- coding: utf8 -*-
# tkinterのインポート
import tkinter as tk
import tkinter.ttk as ttk
from tkinter import filedialog
from tkinter import messagebox
import subprocess
import matplotlib
matplotlib.use("TkAgg")
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk
from matplotlib.figure import Figure
import matplotlib.animation as animation
import tkinter.messagebox
from tkinter import *
import tkinter as tk
import numpy as np
import vtk
from vtk.util import numpy_support
import threading

def run_server_out():
    subprocess.run("./server.out",shell=True)

#lorenz
def vtk_viewer(name):
    filename = name
    reader = vtk.vtkUnstructuredGridReader()
    reader.SetFileName(filename)
    reader.Update()
    #cell data から point data変換
    cell2point = vtk.vtkCellDataToPointData()
    cell2point.SetInputData(reader.GetOutput())
    cell2point.Update()


def button_add():
    def lorenz(x, y, z, s=10, r=5, b=28):
        '''
        Given:
           x, y, z: a point of interest in three dimensional space
           s, r, b: parameters defining the lorenz attractor
        Returns:
           x_dot, y_dot, z_dot: values of the lorenz attractor's partial
               derivatives at the point x, y, z
        '''
        x_dot = s*(y - x)
        y_dot = r*x - y - x*z
        z_dot = x*y - b*z
        return x_dot, y_dot, z_dot

    dt = 0.01
    num_steps = 10000 

     # Need one more for the initial values
    xs = np.empty(num_steps + 1)
    ys = np.empty(num_steps + 1)
    zs = np.empty(num_steps + 1)

     # Set initial values
    xs[0], ys[0], zs[0] = (0., 1., 1.05)

     # Step through "time", calculating the partial derivatives at the current point
    # and using them to estimate the next point
    for i in range(num_steps):
        x_dot, y_dot, z_dot = lorenz(xs[i], ys[i], zs[i])
        xs[i + 1] = xs[i] + (x_dot * dt)
        ys[i + 1] = ys[i] + (y_dot * dt)
        zs[i + 1] = zs[i] + (z_dot * dt)

    # Plot
    fig = Figure()

    canvas = FigureCanvasTkAgg(fig, root)
    canvas.draw()

    ax = fig.add_subplot(111, projection="3d")
    ax.plot(xs, ys, zs, lw=0.5)
    ax.set_xlabel("X Axis")
    ax.set_ylabel("Y Axis")
    ax.set_zlabel("Z Axis")
    ax.set_title("Lorenz Attractor")

    toolbar = NavigationToolbar2Tk(canvas, root)
    toolbar.update()

    canvas.get_tk_widget().pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=1)

def fileopen():
    typ = [('plantFEM script file','*.f90')] 
    dir = '.'
    fle = filedialog.askopenfilename(filetypes = typ, initialdir = dir) 

    print(fle)

def filebuild():

    typ = [('plantFEM script file','*.f90')] 
    dir = '.'
    fle = filedialog.askopenfilename(filetypes = typ, initialdir = dir) 
    
    msg.set("compiling " + fle)

    subprocess.run("plantfem build "+fle,shell=True)
    f = open("compile_log.txt","r")
    ret = f.readline()
    #ret = os.system("plantfem build "+fle)
    if(int(ret)==0 ):
        msg.set("[ok] Successfully compiled!")
    else:
        msg.set("[ERROR] Compile failed!")
    f.close()


def filebuild_and_run():

    typ = [('plantFEM script file','*.f90')] 
    dir = '.'
    fle = filedialog.askopenfilename(filetypes = typ, initialdir = dir) 
    
    msg.set("compiling " + fle)

    subprocess.run("plantfem build "+fle,shell=True)

    f = open("compile_log.txt","r")
    ret = f.readline()
    #ret = os.system("plantfem build "+fle)
    if(int(ret)==0 ):
        msg.set("[ok] Successfully compiled!")
    else:
        msg.set("[ERROR] Compile failed!")
        return
    f.close()
    
    msg.set("Running " + fle)
    

    thread1 = threading.Thread(target=run_server_out())
    thread1.start()
    
    msg.set("[ok] Successfully Executed " + fle)
    
        
    #ret = subprocess.call("plantfem build "+fle)
    #print(ret)

# rootメインウィンドウの設定
root = tk.Tk()
root.title("Frame")
root.attributes('-fullscreen',True)

# toolbarの設定
frame_top = tk.Frame(root, pady=5, padx=5, relief=tk.RAISED, bd=2)

button1 = tk.Button(frame_top, text='Open',command=fileopen)
button2 = tk.Button(frame_top, text='Build',command=filebuild )
button3 = tk.Button(frame_top, text='Run',command=filebuild_and_run )
button4 = tk.Button(frame_top, text='Close',command=exit)

button1.pack(side=tk.LEFT)
button2.pack(side=tk.LEFT, padx=5)
button3.pack(side=tk.LEFT, padx=10)
button4.pack(side=tk.LEFT, padx=15)

frame_top.pack(fill=tk.X)

# 左カラム
frame_left = tk.Frame(root, pady=5, padx=5, relief=tk.RAISED, bd=1, bg="white")
button1_left = tk.Button(frame_left, text="Func1", width=10)
button2_left = tk.Button(frame_left, text="Func2", width=10)

# 右カラム
frame_right = tk.Frame(root, pady=5, padx=5)
global msg 
msg = tk.StringVar()
msg.set('Welcome to plantFEM 22.10 (prototype)')
msglabel = tk.Label(frame_right, textvariable=msg)
#msglabel.pack()

# ウィジェットの配置
frame_left.pack(side=tk.LEFT, fill=tk.Y)
frame_right.pack(side=tk.LEFT, fill=tk.Y)

button1_left.pack()
button2_left.pack(pady=5)



button_add()


root.mainloop()