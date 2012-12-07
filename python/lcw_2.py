#!/usr/bin/env python

import xml.etree.ElementTree as ET

class Tag():
    def __init__(self, tag):
        self.name = tag
        self.children = []
        
    def __str__(self):
        return self.name
    
    def treeiterator(self):
        if self.name != "":
            print "<" + self.name + ">",
        for child in self.children:
            child.treeiterator()
        if self.name != "":
            print "</" + self.name + ">",
            
    def __repr__(self):
        if self.name != "":
            print "<" + self.name + ">",
        for child in self.children:
            print child,
        if self.name != "":
            print "</" + self.name + ">"

def append(tag, node):
    tag.children.append(Tag(node.tag))
    for child in node:
        append(tag, child)
    

tree = ET.parse("sample.xml")
root = tree.getroot()
#for child in root:
#    print child.tag
tag = Tag("")
append(tag, root)
tag.treeiterator()