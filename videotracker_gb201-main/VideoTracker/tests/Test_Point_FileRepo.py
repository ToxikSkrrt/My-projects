import sys
sys.path.append("..")
from src.models.Point_FileRepo import *
import unittest
import filecmp


class TestPoint_FileRepo(unittest.TestCase):
  
    def setUp(self):
        self.__point1 = Point(0, 0)
        self.__point2 = Point(-1, -2)
        self.__point3 = Point(1, 2)
        self.__point4 = Point(1.111, 2.222)
        self.__point5 = Point(-1.111, -2.222)
        
        self.__points = [self.__point1, self.__point2, self.__point3, self.__point4, self.__point5]
        self.__timeTable = [0.0, 1.0, 2.0, 3.0, 4.0]
        
        self.__fileUnittestCreation = FileRepo('')
        self.__fileUnittestCreation.exportDataToCSV(self.__timeTable, self.__points, 'CSV_Unittest.csv', ',')
        self.__fileUnittestCreation.exportDataToTXT(self.__timeTable, self.__points, 'TXT_Unittest.txt')
        
        self.__str = self.__fileUnittestCreation.exportDataToString(self.__timeTable, self.__points)
        self.__strToTXT = open('STR_Unittest.txt', 'w')
        self.__strToTXT.write(self.__str)
        self.__strToTXT.close()
        
    def test_XGet(self):
        self.assertTrue(self.__point1.xGet() == 0)
        self.assertTrue(self.__point2.xGet() == -1)
        self.assertTrue(self.__point3.xGet() == 1)
        self.assertTrue(self.__point4.xGet() == 1.111)
        self.assertTrue(self.__point5.xGet() == -1.111)
        
    def test_YGet(self):
        self.assertTrue(self.__point1.yGet() == 0)
        self.assertTrue(self.__point2.yGet() == -2)
        self.assertTrue(self.__point3.yGet() == 2)
        self.assertTrue(self.__point4.yGet() == 2.222)
        self.assertTrue(self.__point5.yGet() == -2.222)
        
    def test_XSet(self):
        self.__point1.xSet(self.__point2.xGet())
        self.assertTrue(self.__point1.xGet() == -1)
        self.__point2.xSet(self.__point3.xGet())
        self.assertTrue(self.__point2.xGet() == 1)
        self.__point3.xSet(self.__point4.xGet())
        self.assertTrue(self.__point3.xGet() == 1.111)
        self.__point4.xSet(self.__point5.xGet())
        self.assertTrue(self.__point4.xGet() == -1.111)
    
    def test_YSet(self):
        self.__point1.ySet(self.__point2.yGet())
        self.assertTrue(self.__point1.yGet() == -2)
        self.__point2.ySet(self.__point3.yGet())
        self.assertTrue(self.__point2.yGet() == 2)
        self.__point3.ySet(self.__point4.yGet())
        self.assertTrue(self.__point3.yGet() == 2.222)
        self.__point4.ySet(self.__point5.yGet())
        self.assertTrue(self.__point4.yGet() == -2.222)
        
#    def test_ExportDataToCSV(self):
   #     self.assertTrue(filecmp.cmp('myCSV.csv', 'CSV_Unittest.csv'))
        
    def test_ExportDataToStringAndTXT(self):
        self.assertTrue(filecmp.cmp('TXT_Unittest.txt', 'STR_Unittest.txt'))
        
    
if __name__ == '__main__':
    unittest.main()
