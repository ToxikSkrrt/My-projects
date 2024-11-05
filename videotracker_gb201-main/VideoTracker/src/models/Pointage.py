from models.Point_FileRepo import Point
import matplotlib as mpl
import numpy as np
import cv2
import sys


class Pointage:
    
    def __init__(self, parent):
        self.__parent = parent
        self.__points = []
        self.__origin = (0, 0)
        self.__pointMode = False
    
    def setOrigin(self, x, y):
        self.__origin = (x, y)
        
    def getPoints(self):
        return self.__points
    
    def getPointMode(self):
        return self.__pointMode
    
    def setPointMode(self):
        self.__pointMode = True
        
    def addPointsV1(self, x, y):
        self.__points += [Point(x, y)]
        
    def addPoints(self, event, x, y, flags, params):
        if event == cv2.EVENT_LBUTTONDOWN:
            self.__points += [Point(x, y)]
        if event == cv2.EVENT_RBUTTONDOWN:
            self.__pointMode = False
    
    def pointingMode(self):
        if self.__pointMode:
            try:
                cv2.setMouseCallback(self.__parent, self.addPoints)
                cv2.waitkey(1)
            except TypeError:
                pass