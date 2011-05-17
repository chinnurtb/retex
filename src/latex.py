#!/usr/bin/python

""" Parses LaTeX formulae using PlasTeX """ 

from plasTeX import TeXFragment, TeXDocument
from plasTeX.DOM import Node
from plasTeX.TeX import TeX
import hashlib

### parse tree extractor ###

class Leaf(object):
    def __init__(self, name):
        self.name = name

    def update_hash(self, hash):
        hash.update(self.name)

    def __str__(self):
        return self.name

class Branch(object):
    def __init__(self, name):
        self.name = name
        self.children = []

    def add_child(self, child):
        if not(child.name.isspace()):
            self.children.append(child)

    def update_hash(self, hash):
        hash.update(self.name)
        for child in self.children:
            child.update_hash(hash)

    def __str__(self):
        return "(%s)" % " ".join([self.name] + map(str,self.children))

def plastex_node_to_ast(node):
    if node.nodeType == Node.TEXT_NODE:
        return Leaf(unicode(node))
    else:
        branch = Branch(unicode(node.nodeName))

        # see if we have any attributes to extract
        if node.hasAttributes():
            for key, value in node.attributes.items():
                # if the key is 'self' these nodes are the same as the child nodes
                # if the key is '*modifier*' we dont care about it
                if key == 'self' or key == '*modifier*':
                    pass
                elif type(value) is TeXFragment:
                    for child in value.childNodes:
                        branch.add_child(plastex_node_to_ast(child))
                elif type(value) is Node:
                    branch.add_child(plastex_node_to_ast(value))
                else:
                    pass

        # extract child nodes
        if node.childNodes:
            for child in node.childNodes:
                branch.add_child(plastex_node_to_ast(child))

        return branch

def latex_to_ast(latex):
    tex = TeX()
    # tex.disableLogging()
    tex.input(latex)
    plastex_node = tex.parse()
    return plastex_node_to_ast(plastex_node)

def hash_ast(ast):
    md5 = hashlib.md5()
    ast.update_hash(md5)
    return md5.digest()

### interaction with an erlang port  ###

import os, sys
import struct

# ErlangPort by thanos vassilakis
class ErlangPort(object):
    PACK = '>H'
    def __init__(self):
        self._in = sys.stdin
        self._out = sys.stdout
        
    def recv(self):
        buf = self._in.read(2)
        (sz,) = struct.unpack(self.PACK, buf)
        return self._in.read(sz)
        
    def send(self, what):
        sz = len(what)
        buf = struct.pack(self.PACK, sz)
        self._out.write(buf)
        self._out.write(what)
        self._out.flush()

def main():
    port = ErlangPort()
    request = port.recv()
    while request:
        r, w = os.pipe()
        pid = os.fork()
        if pid:
            # parent
            os.close(w)
            r = os.fdopen(r)
            response = r.read()
            port.send(response) 
            os.kill(pid, 9) # kill child
            request = port.recv()
        else:
            # child
            os.close(r)
            w = os.fdopen(w, 'w')
            response = hash_ast(latex_to_ast(request)) 
            w.write(response)
            w.close()
            sys.exit(0)

if __name__ == "__main__":
    main()
