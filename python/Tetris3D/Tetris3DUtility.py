#!/usr/bin/env python
from panda3d.core import lookAt
from panda3d.core import GeomVertexFormat, GeomVertexData
from panda3d.core import Geom, GeomTriangles, GeomVertexWriter
from panda3d.core import Texture, GeomNode
from panda3d.core import Vec3, Vec4, Point3

class Tetris3DUtility():
    def __init__(self):
        pass
    
    def myNormalize(self, vec):
        vec.normalize()
        return vec
    
    def makeSquareC(self, x1, y1, z1, x2, y2, z2, color = Vec4(1, 1, 1, 1)):
        format=GeomVertexFormat.getV3n3cpt2()
        vdata=GeomVertexData('square', format, Geom.UHDynamic)
    
        vertex = GeomVertexWriter(vdata, 'vertex')
        normal = GeomVertexWriter(vdata, 'normal')
        colorW = GeomVertexWriter(vdata, 'color')
        texcoord = GeomVertexWriter(vdata, 'texcoord')
    
        if x1 != x2:
            vertex.addData3f(x1, y1, z1)
            vertex.addData3f(x2, y1, z1)
            vertex.addData3f(x2, y2, z2)
            vertex.addData3f(x1, y2, z2)
    
            normal.addData3f(self.myNormalize(Vec3(2*x1-1, 2*y1-1, 2*z1-1)))
            normal.addData3f(self.myNormalize(Vec3(2*x2-1, 2*y1-1, 2*z1-1)))
            normal.addData3f(self.myNormalize(Vec3(2*x2-1, 2*y2-1, 2*z2-1)))
            normal.addData3f(self.myNormalize(Vec3(2*x1-1, 2*y2-1, 2*z2-1)))		
        else:
            vertex.addData3f(x1, y1, z1)
            vertex.addData3f(x2, y2, z1)
            vertex.addData3f(x2, y2, z2)
            vertex.addData3f(x1, y1, z2)
    
            normal.addData3f(self.myNormalize(Vec3(2*x1-1, 2*y1-1, 2*z1-1)))
            normal.addData3f(self.myNormalize(Vec3(2*x2-1, 2*y2-1, 2*z1-1)))
            normal.addData3f(self.myNormalize(Vec3(2*x2-1, 2*y2-1, 2*z2-1)))
            normal.addData3f(self.myNormalize(Vec3(2*x1-1, 2*y1-1, 2*z2-1)))
    
        colorW.addData4f(color)
        colorW.addData4f(color)
        colorW.addData4f(color)
        colorW.addData4f(color)
    
        texcoord.addData2f(0.0, 1.0)
        texcoord.addData2f(0.0, 0.0)
        texcoord.addData2f(1.0, 0.0)
        texcoord.addData2f(1.0, 1.0)
    
        tri1=GeomTriangles(Geom.UHDynamic)
        tri2=GeomTriangles(Geom.UHDynamic)
    
        tri1.addVertex(0)
        tri1.addVertex(1)
        tri1.addVertex(3)
    
        tri2.addConsecutiveVertices(1,3)
    
        tri1.closePrimitive()
        tri2.closePrimitive()
    
        square=Geom(vdata)
        square.addPrimitive(tri1)
        square.addPrimitive(tri2)
    
        return square
    
    def makeSquare(self, point1, point2, color = Vec4(1, 1, 1, 1)):
        return self.makeSquareC(point1.getX(), point1.getY(), point1.getZ(), point2.getX(), point2.getY(), point2.getZ(), color)
    
    def makeCubeNode(self, center, radius, color = Vec4(1, 1, 1, 1)):
        x = center.getX()
        y = center.getY()
        z = center.getZ()
        o = radius/2.0 #offset
        
        squares = [
            self.makeSquare(Vec3(x-o, y-o, z-o), Vec3(x+o, y-o, z+o), color), #(-1,-1,-1, 1,-1, 1)
            self.makeSquare(Vec3(x-o, y+o, z-o), Vec3(x+o, y+o, z+o), color), #(-1, 1,-1, 1, 1, 1)
            self.makeSquare(Vec3(x-o, y+o, z+o), Vec3(x+o, y-o, z+o), color), #(-1, 1, 1, 1,-1, 1)
            self.makeSquare(Vec3(x-o, y+o, z-o), Vec3(x+o, y-o, z-o), color), #(-1, 1,-1, 1,-1,-1)
            self.makeSquare(Vec3(x-o, y-o, z-o), Vec3(x-o, y+o, z+o), color), #(-1,-1,-1,-1, 1, 1)
            self.makeSquare(Vec3(x+o, y-o, z-o), Vec3(x+o, y+o, z+o), color)  #( 1,-1,-1, 1, 1, 1)
        ]
        
        node = GeomNode('cube')
        for square in squares:
            node.addGeom(square)
        return node
    
    def makeBoxNode(self, BLTCorner, width, height, depth, color = Vec4(1, 1, 1, 1)):
        x = BLTCorner.getX()
        y = BLTCorner.getY()
        z = BLTCorner.getZ()

        squares = [    
            self.makeSquare(BLTCorner, Vec3(x + width, y + height, z), color),
            self.makeSquare(BLTCorner, Vec3(x + width, y, z + depth), color),
            self.makeSquare(BLTCorner, Vec3(x, y + height, z + depth), color),
            self.makeSquare(Vec3(x, y + height, z), Vec3(x + width, y + height, z + depth), color),
            self.makeSquare(Vec3(x + width, y, z), Vec3(x + width, y + height, z + depth), color),
            self.makeSquare(Vec3(x, y, z + depth), Vec3(x + width, y + height, z + depth), color)
        ]
        
        node = GeomNode('box')
        for square in squares:
            node.addGeom(square)
        return node
    
    def makeBox(self, BLTCorner, width, height, depth, color1 = Vec4(1,1,1,1), color2 = Vec4(0,0,0,1)):
        x = BLTCorner.getX()
        y = BLTCorner.getY()
        z = BLTCorner.getZ()
        
        squares = []
        for i in xrange(width):
            for j in xrange(height):
                squares.append(self.makeSquare(Vec3(x+i, y+j, z), Vec3(x+i+1, y+j+1, z), color1 if (i+j)%2 == 1 else color2))
            for k in xrange(depth):
                squares.append(self.makeSquare(Vec3(x+i, y, z+k),Vec3(x+i+1, y, z+k+1), color1 if (i+k)%2 == 0 else color2))
                squares.append(self.makeSquare(Vec3(x+i, y+height, z+k), Vec3(x+i+1, y+height, z+k+1), color1 if (i+k)%2==1 else color2))
        
        for j in xrange(height):
            for k in xrange(depth):
                squares.append(self.makeSquare(Vec3(x, y+j, z+k), Vec3(x, y+1+j, z+1+k), color1 if (j+k)%2 == 0 else color2))
                squares.append(self.makeSquare(Vec3(x+width, y+j, z+k), Vec3(x+width, y+1+j, z+1+k), color1 if (j+k)%2 == 1 else color2))
        
        node = GeomNode('box')
        for square in squares:
            node.addGeom(square)
        return node
    
    def makeFigureNode(self, data, color = Vec4(1.0, 1.0, 1.0, 1.0)):
        dummyNode = GeomNode("figure")
        nodes = [self.makeCubeNode(Vec3(i, j, 0), 1, color) for i in xrange(0, 4) for j in xrange(0, 4) if data[i][j] == 1]
        for node in nodes:
            dummyNode.addChild(node)
        return dummyNode    