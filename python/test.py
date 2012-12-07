#!/usr/bin/env python

class Recursive():
    def rec(self, i):
        if i < 0:
            return
        print "(",
        self.rec(i-1)
        print ")",
    
r = Recursive()
r.rec(3)