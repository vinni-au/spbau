#!/usr/bin/env python
from panda3d.core import Vec3

class FigureType:
    L = 0
    T = 1
    I = 2
    Z = 3
    D = 4
    J = 5
    S = 6

class Figure:
    s = [[0 for j in xrange(3)] for i in xrange(4)]
    
    def __init__(self, Type, size_x, size_y, size_z):
        self.type = Type
        sz = size_z - 1
        sx = size_x / 2
        sy = size_y / 2
        
        if Type == FigureType.L:
            for i in xrange(3):
                self.s[i] = [sx - 1 + i, sy, sz]
            self.s[3] = [sx + 1, sy + 1, sz]
            
        if Type == FigureType.T:
            for i in xrange(3):
                self.s[i] = [sx - 1 + i, sy, sz]
            self.s[3] = [sx, sy + 1, sz]
            
        if Type == FigureType.I:
            for i in xrange(4):
                self.s[i] = [sx, sy + i - 1, sz]
                
        if Type == FigureType.Z:
            self.s[0] = [sx - 1, sy + 1, sz]
            self.s[1] = [sx, sy + 1, sz]
            self.s[2] = [sx, sy, sz]
            self.s[3] = [sx + 1, sy, sz]
            
        if Type == FigureType.D:
            for i in xrange(2):
                for j in xrange(2):
                    self.s[2*i + j] = [sx + i, sy + j, sz]            
        
        if Type == FigureType.J:
            for i in xrange(3):
                self.s[i] = [sx - 1 + i, sy, sz]
            self.s[3] = [sx - 1, sy + 1, sz]
        
        if Type == FigureType.S:
            self.s[0] = [sx - 1, sy, sz]
            self.s[1] = [sx, sy, sz]
            self.s[2] = [sx, sy + 1, sz]
            self.s[3] = [sx + 1, sy + 1, sz]
            
    def findCenterXY(self):
        return self.findCenterHelper(0, 1)
    
    def findCenterYZ(self):
        return self.findCenterHelper(1, 2)
    
    def findCenterXZ(self):
        return self.findCenterHelper(0, 2)
    
    def findCenterHelper(self, i1, i2):
        max1, max2 = 0, 0
        min1, min2 = 500, 500
        for i in xrange(4):
            max1 = max(max1, self.s[i][i1])
            max2 = max(max2, self.s[i][i2])
            min1 = min(min1, self.s[i][i1])
            min2 = min(min2, self.s[i][i2])
        if min1 == max1 and min2 == max2:
            return 0, 0
        size1 = max1 - min1 + 1
        size2 = max2 - min2 + 1
        center1 = min1 + size1 / 2
        center2 = min2 + size2 / 2
        return center1, center2