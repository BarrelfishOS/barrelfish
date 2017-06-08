#!/usr/bin/env python

"""
Generates a capability dependency graph from a Hamlet file.
"""

import argparse
import re

import graphviz as gv


desc = 'generate a capability dependency graph from a Hamlet file'
parser = argparse.ArgumentParser(description=desc)
parser.add_argument('-f', '--file', type=str, help='name of Hamlet file',
                    default='caps.hl')
parser.add_argument('-i', '--img', type=str, help='name of png file without extension',
                    default='caps')
args = parser.parse_args()

source_file = open(args.file, 'r').readlines()
pattern = re.compile("cap(.)*") 
graph = gv.Graph(format='png')

for line in source_file:

    if pattern.match(line):

        tokens = line.split(" ")
        cap_name = tokens[1]

        assert tokens[0] == 'cap'
        assert cap_name[0].isupper()

        if 'abstract' in tokens:
            graph.node(cap_name, color='red')
        else:
            graph.node(cap_name)

        if 'from_self' in tokens:
            graph.edge(cap_name, cap_name)

        if 'from' in tokens:
            ndx = tokens.index('from')
            parent_cap = tokens[ndx + 1]
            graph.edge(cap_name, parent_cap)

graph.render(filename=args.img)
