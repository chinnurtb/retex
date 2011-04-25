#!/bin/env python

""" Parses LaTeX formulae using PlasTeX """ 

from plasTeX import TeXFragment, TeXDocument
from plasTeX.DOM import Node
from plasTeX.TeX import TeX
import hashlib

### parse tree formatter ###

class Formatter(object):
    
    def __init__(self):
        self.elements = []
        
    def __str__(self):
        return " ".join(self.elements)

    def hash(self):
        md5 = hashlib.md5()
        for element in self.elements:
            if element.lstrip(' '):
                md5.update(element)
        return md5.digest()
  
    def formatNode(self, node):
        if node.nodeType == Node.TEXT_NODE:
            self.elements.append(unicode(node))
        else:
            macro = unicode(node.nodeName)
            if macro.startswith("#"):
                pass # these are used for implicit nodes?
            elif macro.startswith("active::"):
                self.elements.append(macro.lstrip("active::"))
            else:
                self.elements.append("\\" + macro)
            self.formatNodeChildren(node)

    def formatNodeChildren(self, node):
        # see if we have any attributes to format
        if node.hasAttributes():
            for key, value in node.attributes.items():
                # if the key is 'self' these nodes are the same as the child nodes
                # if the key is '*modifier*' we dont care about it
                if key == 'self' or key == '*modifier*':
                    pass
                elif type(value) is TeXFragment:
                    self.openBracket()
                    for child in value.childNodes:
                        self.formatNode(child)
                    self.closeBracket()
                elif type(value) is Node:
                    self.openBracket()
                    self.formatNode(value)
                    self.closeBracket()
                else:
                    pass

        # format child nodes
        if node.childNodes:
            self.openBracket()
            for child in node.childNodes:
                self.formatNode(child)
            self.closeBracket()

    def openBracket(self):
        self.elements.append("{")

    def closeBracket(self):
        self.elements.append("}")

def format(string):
    tex = TeX()
    # tex.disableLogging()
    tex.input(string)
    syntax_tree = tex.parse()

    formatter = Formatter()
    formatter.formatNode(syntax_tree)
    return formatter

### interaction with an erlang port  ###

import sys

def requests():
    line = sys.stdin.readline()
    while line:
        yield line
        line = sys.stdin.readline()

def main():
    command = sys.argv[1]
    for request in requests():
        if command == 'format':
            response = str(format(request))
        elif command == 'hash':
            response = format(request).hash()
        sys.stdout.write("%s\n" % response)
        sys.stdout.flush()

if __name__ == "__main__":
    main()
