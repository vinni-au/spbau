#!/usr/bin/env python

class Config():
    def __init__(self, file):
        self.filename = file
        self.file = open(file)
        self.config = {}
        for str in self.file.readlines():
            line = str.split("=")
            if len(line) > 1:
                if (len(line[0]) > 0):
                    self.config[line[0]] = line[1].strip()

class Patternizer():
    def __init__(self, file, config):
        self.filename = file
        self.file = open(file)
        self.config = config
        
    def produce(self):
        for str in self.file.readlines():
            for pat in config.config.keys():
                str = str.replace("{" + pat + "}", config.config[pat])
            print str,


config = Config("config.txt")
patternizer = Patternizer("pattern.txt", config)
patternizer.produce()
