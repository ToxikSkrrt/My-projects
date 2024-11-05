import tkinter as tk
import cv2
import numpy as np
from tkinter.filedialog import *
import sys
sys.path.append("..")
from models.Point_FileRepo import *
from models.Video import *
from views.view import *

class Controller:
    
    def __init__(self, modelF, modelV, modelP, view):
        self.__modelF = modelF
        self.__modelV = modelV
        self.__modelP = modelP
        self.__view = view
        
        self.__view.pauseVideo_btn.config(command = self.pauseVideo)
        self.__view.frameByFrameLeft_btn.config(command = self.frameByFrameLeft)
        self.__view.frameByFrameRight_btn.config(command = self.frameByFrameRight)
        self.__view.firstFrame_btn.config(command = self.firstFrame)
        self.__view.lastFrame_btn.config(command = self.lastFrame)
        
        menubar = Menu(self.__view.get_parent())
        self.__view.get_parent().config(menu=menubar)

        fileMenu = Menu(menubar, tearoff = 0)
        fileMenu.add_command(label="Open Video", command = self.openVid)
        fileMenu.add_command(label="Export", command = self.export)
        fileMenu.add_command(label="Read video", command = self.pauseVideo)
        menubar.add_cascade(label="File", menu=fileMenu)
        
        toolsMenu = Menu(menubar, tearoff = 0)
        toolsMenu.add_command(label="Pointing Mode", command = self.pointingMode)
        toolsMenu.add_command(label="Points", command = self.getPoints)
        toolsMenu.add_command(label="Add Points (temp)", command = self.addPoints)
        menubar.add_cascade(label="Tools", menu=toolsMenu)

        exitMenu = Menu(menubar, tearoff = 0)
        fileMenu.add_command(label="Exit", command=self.onExit)

        self.__view.get_parent().bind("<Ctrl><O>", self.openVid())
        self.__view.get_parent().bind("<Ctrl><S>", self.saveFile())

    def openVid(self):
        file = askopenfile(mode ='r', filetypes =[('Video Files', '*.mp4')])
        if file is not None:
            self.__modelV.open_file(file.name)
            self.__modelV.play_video()
    
    def saveFile(self):
        files = [('CSV Files', '*.csv'), ('Text Files', '*.txt'), ('All Files', '*.*')]
        file = asksaveasfile(filetypes = files)
        return file.name
    
    def export(self):
        dataTimes = [0.0, 0.5]
        dataPoints = [Point(0, 1), Point(2.1, 2)]
        file_path = self.saveFile()
        self.__modelF.exportDataToCSV(dataTimes, dataPoints, file_path)
            
    def pointingMode(self):
        self.__modelP.setPointMode()
        self.__modelP.pointingMode()
    
    def getPoints(self):
        print(self.__modelP.getPoints())
        
    def addPoints(self):
        if len(self.__modelP.getPoints()) == 0:
            self.__modelP.addPointsV1(1, 2)
            print(f'Ajout du point n°1 de coordonnées x = {self.__modelP.getPoints()[0].xGet()}, y = {self.__modelP.getPoints()[0].yGet()}')
        elif len(self.__modelP.getPoints()) == 1:
            self.__modelP.addPointsV1(2, 4)
            print(f'Ajout du point n°2 de coordonnées x = {self.__modelP.getPoints()[1].xGet()}, y = {self.__modelP.getPoints()[1].yGet()}')
        elif len(self.__modelP.getPoints()) == 2:
            self.__modelP.addPointsV1(3, 6)
            print(f'Ajout du point n°3 de coordonnées x = {self.__modelP.getPoints()[2].xGet()}, y = {self.__modelP.getPoints()[2].yGet()}')
        elif len(self.__modelP.getPoints()) == 3:
            self.__modelP.addPointsV1(4, 8)
            print(f'Ajout du point n°4 de coordonnées x = {self.__modelP.getPoints()[3].xGet()}, y = {self.__modelP.getPoints()[3].yGet()}')
        elif len(self.__modelP.getPoints()) == 4:
            self.__modelP.addPointsV1(5, 10)
            print(f'Ajout du point n°5 de coordonnées x = {self.__modelP.getPoints()[4].xGet()}, y = {self.__modelP.getPoints()[4].yGet()}')
    
    def pauseVideo(self):
        self.__modelV.pause_video()
        
    def frameByFrameLeft(self):
        self.__modelV.FrameByFrame('left')
        
    def frameByFrameRight(self):
        self.__modelV.FrameByFrame('right')
        
    def firstFrame(self):
        self.__modelV.set_frame(1)
    
    def lastFrame(self):
        self.__modelV.set_frame("Last")
        
    def progressBar(self):
        progress_cap = self.__modelV.get_vid_length()
        if self.__view.progressbar['value'] < progress_cap:
            self.__view.progressbar['value'] = self.__modelV.get_vid_state()[0]

    def onExit(self):
        self.__view.get_parent().destroy()
