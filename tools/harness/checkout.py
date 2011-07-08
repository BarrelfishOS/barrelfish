##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

try:
    from mercurial import hg, ui, node
    mercurial_module = True
except ImportError:
    mercurial_module = False

class Checkout:
    '''Checkout class:
    Maintain information about revision number, and directory locations
    '''

    def __init__(self, base_dir):
        # Set parameters
        self.base_dir = base_dir
        if mercurial_module:
            self.repo = hg.repository(ui.ui(), base_dir)

    def get_base_dir(self):
        return self.base_dir

    def describe(self):
        if not mercurial_module:
            return '(mercurial module not available)'

        # identify the parents of the working revision
        context = self.repo[None]
        parents = context.parents()
        s = ', '.join(map(lambda p: node.short(p.node()), parents))
        if context.files() or context.deleted():
            s += ' with local changes'
        else:
            s += ' unmodified'
        return s
