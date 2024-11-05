from controllers.Controller import Controller
import sys
from models.Point_FileRepo import FileRepo, Point
from models.Video import Video
from models.Pointage import Pointage
from views.view import View
import tkinter as tk
import tkinter.ttk as ttk

class Application(tk.Tk):

    def __init__(self):

        super().__init__()
        self.title('Video Tracker')
        # create a view and place it on the root window
        view = View(self)
        # create a video model
        video = Video(view.video_lbl)
        filerepo = FileRepo()
        pointage = Pointage(view.video_lbl)
        # create a controller
        controller = Controller(filerepo, video, pointage, view)

        # set the controller to view
        view.setController(controller)

if __name__ == '__main__':
    app = Application()
    app.mainloop()