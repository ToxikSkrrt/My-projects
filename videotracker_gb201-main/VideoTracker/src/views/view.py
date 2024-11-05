import tkinter as tk
import PIL.Image, PIL.ImageTk
from tkinter import *
import cv2
import tkinter.ttk as ttk
import models.Video as modelV

class View(tk.Frame):

    def __init__(self, parent):
        super().__init__(parent)
        self.__parent = parent
            
        self.progressbar = ttk.Progressbar(self.__parent, orient='horizontal', mode='determinate', length=250)
        self.progressbar.pack(pady=20)
        
        self.video_lbl = Label(self.__parent, width='50', height = '20', font = ('Arial', 25), bg='grey')
        self.video_lbl.pack(side=TOP, padx=5, pady=5)
        
        self.pauseVideo_btn = Button(self.__parent, text ='Play | Pause', bg="green")
        self.pauseVideo_btn.pack(side=TOP, padx=5, pady=5)
        
        self.firstFrame_btn = Button(self.__parent, text = 'First Frame', bg= "grey")
        self.firstFrame_btn.pack(side=LEFT, padx=5, pady=5)
        
        self.frameByFrameLeft_btn = Button(self.__parent, text ='<', bg="grey")
        self.frameByFrameLeft_btn.pack(side=LEFT, padx=5, pady=5)
        
        self.lastFrame_btn = Button(self.__parent, text = 'Last Frame', bg= "grey")
        self.lastFrame_btn.pack(side=RIGHT, padx=5, pady=5)
        
        self.frameByFrameRight_btn = Button(self.__parent, text ='>', bg="grey")
        self.frameByFrameRight_btn.pack(side=RIGHT, padx=5, pady=5)
    
    def setController(self, controller):
        self.controller = controller
        
    def get_parent(self):
        return self.__parent


    