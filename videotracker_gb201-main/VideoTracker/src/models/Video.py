from tkinter import *
from tkinter import messagebox
import PIL.Image, PIL.ImageTk
import cv2
import sys
sys.path.append("..")
from controllers.Controller import *

class Video:

    def __init__(self, window):
        self.window = window
        self.canvas = Canvas(window)
        self.canvas.pack()
        self.delay = 1   # ms
        self.fps = 60
        self.end = False

    def open_file(self, file_name):
        self.pause = True
        self.filename = file_name
        self.cap = cv2.VideoCapture(self.filename)
        ret, firstFrame = self.get_frame()
        self.width = self.cap.get(cv2.CAP_PROP_FRAME_WIDTH)
        self.height = self.cap.get(cv2.CAP_PROP_FRAME_HEIGHT)
        self.canvas.config(width = self.width, height = self.height)
        self.__length = int(self.cap.get(cv2.CAP_PROP_FRAME_COUNT))
        self.fps = self.cap.get(cv2.CAP_PROP_FPS)

    # get only one frame    
    def get_frame(self):   
        try:
            if self.cap.isOpened():
                ret, frame = self.cap.read()
                return (ret, cv2.cvtColor(frame, cv2.COLOR_BGR2RGB))
        except:
            self.end = True
            self.pause
            messagebox.showerror(title='Alert', message='End of the video.')
            
    def get_vid_length(self):
        return self.__length
    
    def get_vid_state(self):
        return [self.cap.get(cv2.CAP_PROP_POS_FRAMES), self.pause]
    
    def set_frame(self, frame_id):
        self.end = False
        if self.pause == False:
            self.pause = True
        if frame_id == "Last":
            frame_id = self.get_vid_length() - 2
        self.cap = cv2.VideoCapture(self.filename)
        self.cap.set(1, frame_id)
        ret, frame = self.cap.read()
        self.window.after(self.delay, self.play_video)


    def play_video(self):
        # Get a frame from the video source, and go to the next frame automatically
        ret, frame = self.get_frame()
        if ret:
            self.photo = PIL.ImageTk.PhotoImage(image = PIL.Image.fromarray(frame))
            self.canvas.create_image(0, 0, image = self.photo, anchor = NW)
        if not self.pause:
            self.window.after(self.delay, self.play_video)
            
    def pause_video(self):
        if self.end == True:
            self.set_frame(1)
        if self.pause:
            self.pause = False
            self.window.after(self.delay, self.play_video)
        else:
            self.pause = True
        
    def FrameByFrame(self, side):
        if self.pause:
            if side == 'left':
                self.pause = False
                self.window.after(self.delay, self.play_video)
                self.pause = True
            elif side == 'right':
                self.pause = False
                self.window.after(self.delay, self.play_video)
                self.pause = True

    # Release the video source when the object is destroyed
    def __del__(self):
        if self.cap.isOpened():
            self.cap.release()
