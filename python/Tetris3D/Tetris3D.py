#!/usr/bin/env python
import sys
from math import pi, sin, cos
from direct.showbase.ShowBase import ShowBase
from direct.task import Task
from panda3d.core import Texture, GeomNode, Vec4, Vec3
from panda3d.core import WindowProperties, NodePath, PandaNode, LightRampAttrib, TextNode
from panda3d.core import PointLight, AmbientLight
from panda3d.core import TransparencyAttrib, Point3
from direct.filter.CommonFilters import CommonFilters
from Tetris3DUtility import Tetris3DUtility
from Figure import Figure, FigureType
from pprint import pprint
from random import randint

class State:
    MainMenu = 0
    Falling = 1
    GameOver = 2
    Generating = 3

class Tetris3DApp(ShowBase):
    box_x = 6
    box_y = 6
    box_z = 10
    utility = Tetris3DUtility()
    heightmap = [[0 for j in xrange(box_y)] for i in xrange(box_x)]
    fielddata = [[[0 for k in xrange(box_x)] for j in xrange(box_y)] for i in xrange(box_z)]
    fielddataupdated = False
    state = State.Generating
    layercolors = [Vec4(k*0.1, 1.0 - k*0.1, 0.5 + k*0.05, 1.0) if k % 2 == 0 else Vec4(1.0 - k*0.1, 0.0 + k*0.1, 1.0 - k*0.05, 1.0) for k in xrange(box_z)]
    figure = None
    text = TextNode("center text")
    text.setText("Game Over!")
    
    def __init__(self):
        ShowBase.__init__(self)
        props = WindowProperties()
        props.setTitle("Tetris3D by vinni-au")
        props.setSize(600, 600)
        self.win.requestProperties(props)       

        self.textNode = aspect2d.attachNewNode(self.text)
        self.textNode.setX(-0.7)
        self.textNode.setScale(0.25)
        self.textNode.setColor(Vec4(1.0, 0.0, 0.0, 1.0))
        self.textNode.hide()
    
        boxnode = self.utility.makeBox(Vec3(-3,-3,0), self.box_x, self.box_y, self.box_z)        
            
        self.box = self.render.attachNewNode(boxnode)
        self.box.setPos(0, 0, 0)
        self.box.setTwoSided(True)
        self.box.setTransparency(TransparencyAttrib.MAlpha)
        self.box.setAlphaScale(0.9)
        
        self.dFieldnode = self.render.attachNewNode("dummyField")
        self.dFigureNode = self.render.attachNewNode("dummyFigure")

        self.initCamera()
        self.initLights()
        self.initKeys()

        self.mainLoopTask = self.taskMgr.doMethodLater(1.3, self.mainLoop, "mainLoop")
            
    def exitRequest(self):
        sys.exit(0)
        
    def checkFigureData(self, s):
        for i in xrange(4):
            x = s[i][0]
            y = s[i][1]
            z = s[i][2]
            if x < 0 or x >= self.box_x or y < 0 or y >= self.box_y or z < 0 or z >= self.box_z:
                return False            
            if self.fielddata[z][x][y] == 1:
                return False
        return True
    
    def turnCW(self):
        self.turnHelper(0, 1, 2, 1, -1)
            
    def turnCCW(self):
        self.turnHelper(0, 1, 2, -1, 1)
        
    def turnForward(self):
        self.turnHelper(1, 2, 0, -1, 1)
        
    def turnBackward(self):
        self.turnHelper(1, 2, 0, 1, -1)
        
    def turnLeft(self):
        self.turnHelper(0, 2, 1, -1, 1)
        
    def turnRight(self):
        self.turnHelper(0, 2, 1, 1, -1)
        
    def initKeys(self):
        self.accept("escape", self.exitRequest)
        self.accept("arrow_up", self.moveUp)
        self.accept("arrow_down", self.moveDown)
        self.accept("arrow_right", self.moveRight)
        self.accept("arrow_left", self.moveLeft)
        self.accept("z", self.turnCCW)
        self.accept("x", self.turnCW)
        self.accept("w", self.turnForward)
        self.accept("s", self.turnBackward)
        self.accept("a", self.turnLeft)
        self.accept("d", self.turnRight)
        
    def initCamera(self):
        base.disableMouse()
        self.camera.setPos(0, 0, 21)
        self.camera.lookAt(self.render)
        
    def mainLoop(self, task):
        if self.state == State.Generating:
            self.figure = Figure(randint(0, 6), self.box_x, self.box_y, self.box_z)            
            self.state = State.Falling
            self.drawCurrentFigure()

        if self.state == State.Falling:
            for i in xrange(4):
                self.figure.s[i][2] = self.figure.s[i][2] - 1
            for i in xrange(4):
                x = self.figure.s[i][0]
                y = self.figure.s[i][1]
                if self.heightmap[x][y] >= self.figure.s[i][2]:
                    self.state = State.Generating
                    self.fixFigure()
                    self.figure = None
                    break
            self.drawCurrentFigure()
        
        if self.state == State.GameOver:
            self.textNode.show()
            return Task.done            
        
        self.drawFigures()
        return Task.again
    
    def fixFigure(self):
        tocheck = []
        for i in xrange(4):
            x, y, z = self.figure.s[i][0], self.figure.s[i][1], self.figure.s[i][2]
            tocheck.append(z)
            self.fielddata[z][x][y] = 1
            self.heightmap[x][y] = z + 1
            if self.heightmap[x][y] == self.box_z - 1:
                self.state = State.GameOver
        self.eliminate(tocheck)
        self.fielddataupdated = True
        
    def eliminate(self, tocheck):
        rowsEliminated = 0
        for z in tocheck:
            needEliminate = True
            for x in xrange(self.box_x):
                for y in xrange(self.box_y):
                    if self.fielddata[z][x][y] == 0:
                        needEliminate = False
                        break
            if needEliminate:
                for x in xrange(self.box_x):
                    for y in xrange(self.box_y):
                        if self.heightmap[x][y] >= z:
                            self.heightmap[x][y] = max(self.heightmap[x][y] - 1, 0)
                        while self.fielddata[self.heightmap[x][y] - 1][x][y] == 0 and self.heightmap[x][y] > 0:
                            self.heightmap[x][y] = self.heightmap[x][y] - 1
                for i in xrange(z, self.box_z - 1):
                    self.fielddata[i] = self.fielddata[i+1]
        for i in xrange(rowsEliminated):
            self.fielddata.append([[0 for k in xrange(box_x)] for j in xrange(box_y)])
    
    def drawFigures(self):
        if self.fielddataupdated == True:
            self.dFieldnode.removeNode()
            self.dFieldnode = self.render.attachNewNode("fieldNode")
            for i in xrange(self.box_x):
                for j in xrange(self.box_y):
                    for k in xrange(self.box_z):
                        if self.fielddata[k][i][j] == 1:
                            newNode = self.utility.makeCubeNode(Vec3(-2.5 + i,-2.5 + j,k), 1, self.layercolors[k])        
                            self.dFieldnode.attachNewNode(newNode)
            self.dFieldnode.setTwoSided(True)
            self.dFieldnode.setTransparency(TransparencyAttrib.MAlpha)
            self.dFieldnode.setAlphaScale(0.65)
            self.fielddataupdated = False
        
    def drawCurrentFigure(self):
        if self.figure != None:
            self.dFigureNode.removeNode()
            self.dFigureNode = self.render.attachNewNode("figureNode")
            for i in xrange(4):
                newNode = self.utility.makeCubeNode(self.figureToVec3(i), 1, Vec4(0.4, 0.9, 0.2, 1.0))
                self.dFigureNode.attachNewNode(newNode)
            self.dFigureNode.setTwoSided(True)
            self.dFigureNode.setTransparency(TransparencyAttrib.MAlpha)
            self.dFigureNode.setAlphaScale(0.6)
        else:
            self.dFigureNode.removeNode()
        
    def figureToVec3(self, i):
        return Vec3(-2.5 + self.figure.s[i][0], -2.5 + self.figure.s[i][1], 0.5 + self.figure.s[i][2])
        
    def initLights(self):
        plightnode = PointLight("point light")
        plightnode.setAttenuation(Vec3(0,0,1))
        plightnode.setPoint(Point3(4,4,-1))
        self.plight = render.attachNewNode(plightnode)
        alightnode = AmbientLight("ambient light")
        alightnode.setColor(Vec4(1.0,1.0,1.0,1))
        self.alight = render.attachNewNode(alightnode)
        self.alight.setPos(0,0,1)
        self.render.setLight(self.alight)
        self.render.setLight(self.plight)
        
    def turnHelper(self, i1, i2, i3, sign1, sign2):
        if self.figure != None:
            center1, center2 = self.figure.findCenterHelper(i1, i2)
            s = [[0 for j in xrange(3)] for i in xrange(4)]
            for i in xrange(4):
                s[i][i3] = self.figure.s[i][i3]
                s[i][i1] = center1 + sign1 * (self.figure.s[i][i2] - center2)
                s[i][i2] = center2 + sign2 * (self.figure.s[i][i1] - center1)
            if self.checkFigureData(s):
                self.figure.s = s
                self.drawCurrentFigure()
        
    def moveUp(self):
        if self.figure != None:
            for i in xrange(4):
                if self.figure.s[i][1] >= self.box_y - 1:
                    return
            for i in xrange(4):
                self.figure.s[i][1] = self.figure.s[i][1] + 1
            self.drawCurrentFigure()
        
    def moveDown(self):
        if self.figure != None:
            for i in xrange(4):
                if self.figure.s[i][1] <= 0:
                    return
            for i in xrange(4):
                self.figure.s[i][1] = self.figure.s[i][1] - 1
            self.drawCurrentFigure()
        
    def moveLeft(self):
        if self.figure != None:
            for i in xrange(4):
                if self.figure.s[i][0] <= 0:
                    return
            for i in xrange(4):
                self.figure.s[i][0] = self.figure.s[i][0] - 1
            self.drawCurrentFigure()
        
    def moveRight(self):
        if self.figure != None:
            for i in xrange(4):
                if self.figure.s[i][0] >= self.box_x - 1:
                    return
            for i in xrange(4):
                self.figure.s[i][0] = self.figure.s[i][0] + 1
            self.drawCurrentFigure()

app = Tetris3DApp()
app.run()