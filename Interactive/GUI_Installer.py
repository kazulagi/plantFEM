import tkinter
import tkinter.messagebox as mb
import os
from install import install_plantFEM
import time
import asyncio

def installer_button(widget):
    widget.place_forget() # canvas(widget)を隠す                          
    canvas2 = tkinter.Canvas(background="#eca", width=400, height=400)
    canvas2.place(x=0, y=0)
    label2 = tkinter.Label(canvas2, text="Installed plantFEM 22.04")
    label2.place(x=200, y=150, anchor=tkinter.CENTER)
    button = tkinter.Button(canvas1, text=">> Install", command=lambda:transition_button(canvas2)) # 遷移ボタン
    button.place(x=200, y=250, anchor=tkinter.CENTER) 
    button_work("Installing plantFEM 22.04 ...")
    label3 = tkinter.Label(canvas2, text="Installing plantFEM 22.04...")
    label3.place(x=200, y=150, anchor=tkinter.CENTER)
    return
    

def transition_button(widget):
    widget.place_forget() # canvas(widget)を隠す                          
    canvas3 = tkinter.Canvas(background="#eca", width=400, height=400)
    canvas3.place(x=0, y=0)
    
    ret = install_plantFEM()
    
    label2 = tkinter.Label(canvas3, text="Installed!")
    label2.place(x=200, y=150, anchor=tkinter.CENTER)
    
    button = tkinter.Button(canvas3, text=">> Finish", command=exit ) # 遷移ボタン                                                
    button.place(x=200, y=250, anchor=tkinter.CENTER)
    
    return

def button_work(text_A):
 
    mb.showinfo("info", text_A) #メッセージボックスで"show_info_A"と表示する。
# ウィンドウの作成                                                              
window = tkinter.Tk()
window.geometry("400x400")
window.eval('tk::PlaceWindow . center')
window.title("Screen Transition")

# 遷移前の画面の作成                                                            
canvas1 = tkinter.Canvas(background="#cea", width=400, height=400)
canvas1.place(x=0, y=0) # キャンバス                                            
label1 = tkinter.Label(canvas1, text="plantFEM 22.04 installer") # テキスト       
label1.place(x=200, y=150, anchor=tkinter.CENTER)
button = tkinter.Button(canvas1, text=">> Install", command=lambda:installer_button(canvas1)) # 遷移ボタン                                                
button.place(x=200, y=250, anchor=tkinter.CENTER)

button = tkinter.Button(canvas1, text=">> Install", command=lambda:transition_button(canvas1)) # 遷移ボタン                                                
button.place(x=200, y=250, anchor=tkinter.CENTER)

window.mainloop()