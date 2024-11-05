from Point_FileRepo import Point

class Scale:

    def __init__(self, pointA, pointB, scaleValue):
        self.__pointA = pointA
        self.__pointB = pointB
        self.__scale = scaleValue

    def getXLength(self):
        if self.__pointB.xGet() - self.__pointA.xGet() < 0:
            return -(self.__pointB.xGet() - self.__pointA.xGet())
        return self.__pointB.xGet() - self.__pointA.xGet()

    def getYLength(self):
        if self.__pointB.yGet() - self.__pointA.yGet() < 0:
            return -(self.__pointB.yGet() - self.__pointA.yGet())
        return self.__pointB.yGet() - self.__pointA.yGet()

    def getScaleLength(self):
        return self.getXScale() + self.getYScale()

    def setScaleValue(self, newScale):
        self.__scale = newScale
    
    def convertLength(self, startPos, endPos):
        # Must be of same length type
        if endPos.xGet() - startPos.xGet() < 0:
            length_X = -(endPos.xGet() - startPos.xGet())
        else:
            length_X = endPos.xGet() - startPos.xGet()
        if endPos.yGet() - startPos.yGet() < 0:
            length_Y = -(endPos.yGet() - startPos.yGet())
        else:
            length_Y = endPos.yGet() - startPos.yGet()

        length_final = length_Y + length_X
        scaled = (length_final/self.getScaleLength() * self.__scale)
        return scaled