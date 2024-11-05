import os

class Point:

    def __init__(self, x : float, y : float):
        self.x = x
        self.y = y
  
    def xGet(self):
        return self.x

    def yGet(self):
        return self.y

    def xSet(self, x_value : float):
        self.x = x_value

    def ySet(self, y_value : float):
        self.y = y_value


class FileRepo:
  
    def __init__(self):
        pass

    def exportDataToString(self, dataTimes : list, dataPoints : list, delim = ',') -> str:
        file = open(fileName, "w")
        if len(dataPoints) == len(dataTimes):
            for i in range(len(dataPoints)):
                Time = dataTimes[i]
                X_Coord = dataPoints[i].xGet()
                Y_Coord = dataPoints[i].yGet()
                file.write(f'time = {Time}, x = {X_Coord}, y = {Y_Coord};\n')
        file.close()


    def exportDataToCSV(self, dataTimes : list, dataPoints : list, fileName : str, delim = ',') -> None:
        if len(dataPoints) == len(dataTimes):
            file = open(fileName, "w")
            for i in range(len(dataPoints)):
                Time = dataTimes[i]
                X_Coord = dataPoints[i].xGet()
                Y_Coord = dataPoints[i].yGet()
                file.write(f'{Time}{delim}{X_Coord}{delim}{Y_Coord}\n')
            file.close()
