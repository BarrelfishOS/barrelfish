##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import os
import tests
from common import TestCommon
from results import PassFailResult

#module /acreto/x86_64/sbin/gm_pr spawnflags=2 /nfs/soc-LiveJournal1.bin 20 nfs://10.110.4.4/mnt/local/nfs/acreto/

@tests.add_test
class GreenMarl_PageRank(TestCommon):
    '''Green Marl: Page Rank Test'''
    name = "GreenMarl_PageRank"

    def get_modules(self, build, machine):
        modules = super(GreenMarl_PageRank, self).get_modules(build, machine)
        if 'SHL__NUM_CORES' in os.environ :
            ncores = os.environ['SHL__NUM_CORES']
        else:
            ncores = machine.get_ncores()
        
        modules.add_module("gm_pr", ["spawnflags=2",
                                     "/nfs/soc-LiveJournal1.bin",
                                     ncores,
                                     "nfs://10.110.4.4/mnt/local/nfs/acreto/"])

        modules.add_module("e1000n", ["auto", "noirq"])
        modules.add_module("NGD_mng", ["auto"])
        modules.add_module("netd", ["auto"])
        modules.add_module("/shl__settings.lua", ["nospawn"])
        return modules

    def get_finish_string(self):
        return "XXXXXXXXXX GM DONE XXXXXXXXXXXXXX"

    def boot(self, *args):
        super(GreenMarl_PageRank, self).boot(*args)
#        self.set_timeout(NFS_TIMEOUT)

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        passed = False;
        for line in rawiter:
            if line.startswith(self.get_finish_string()) :
                passed = True;
                break;
            elif line.startswith("Aborted") :
                passed = False;
                break;
        return PassFailResult(passed)

    def process_data(self, testdir, rawiter):
        passed = False
        for line in rawiter:
            if line.startswith(self.get_finish_string()):
               passed = True
               break
        return PassFailResult(passed)

@tests.add_test
class GreenMarl_TriangleCounting(TestCommon):
    '''Green Marl: Triangle Counting Test'''
    name = "GreenMarl_TriangleCounting"

    def get_modules(self, build, machine):
        modules = super(GreenMarl_TriangleCounting, self).get_modules(build, machine)
        # TODO: nfs location and number of cores...
        modules.add_module("gm_pr", ["spawnflags=2",
                                     "/nfs/soc-LiveJournal1.bin",
                                     machine.get_ncores(),
                                     "nfs://10.110.4.4/mnt/local/nfs/acreto/"])

        modules.add_module("e1000n", ["auto", "noirq"])
        modules.add_module("NGD_mng", ["auto"])
        modules.add_module("netd", ["auto"])
        
        return modules

    def get_finish_string(self):
        return "XXXXXXXXXX GM DONE XXXXXXXXXXXXXX"

    def boot(self, *args):
        super(GreenMarl_PageRank, self).boot(*args)

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)