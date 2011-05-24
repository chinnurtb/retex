#!/usr/bin/python

""" Parses LaTeX formulae using PlasTeX """ 

from plasTeX import TeXFragment, TeXDocument
from plasTeX.DOM import Node
from plasTeX.TeX import TeX
import hashlib

import resource
import select
import os, sys
import struct
import pwd

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

def jail():
    os.nice(20)
    resource.setrlimit(resource.RLIMIT_CPU, (5, 5)) # max 5s cpu time
    resource.setrlimit(resource.RLIMIT_AS, (100*1024*1024, 100*1024*1024)) # max 100MB address space

    user = pwd.getpwnam('retex')
    uid, gid = user[2], user[3]

    os.chroot('/retex-jail')

    os.setregid(gid, gid)
    os.setreuid(uid, uid)

def spawn_worker(request):
    r, w = os.pipe()
    pid = os.fork()
    if pid:
        # parent
        os.close(w)
        r = os.fdopen(r)
        return r, pid
    else:
        # child
        jail()
        os.close(r)
        w = os.fdopen(w, 'w')
        rid = request[0:4]
        latex = request[4:]
        response = hash_ast(latex_to_ast(latex))
        w.write(rid)
        w.write(response)
        w.close()
        sys.exit(0)

def main():
    port = ErlangPort()
    pids = {}
    # resource.setrlimit(resource.RLIMIT_NPROC, (100, 100)) # max 100 children
    while True:
        (files, _, _) = select.select(pids.keys() + [sys.stdin], [], [])
        for f in files:
            if f is sys.stdin:
                # new request
                request = port.recv()
                new_f, pid = spawn_worker(request)
                files.append(new_f)
                pids[new_f] = pid 
            else:
                # response to previous request
                port.send(f.read())
                pid = pids[f]
                os.kill(pid, 9)
                os.waitpid(pid, 0)
                del pids[f]

if __name__ == "__main__":
    main()
