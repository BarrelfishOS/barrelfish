
##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import tests
import debug
from common import TestCommon
from results import PassFailResult

class LibosTestMemtestMulti(TestCommon):
    '''memory allocation functionality on all cores using different library OS'''

    def setup(self, build, machine, testdir):
        super(LibosTestMemtestMulti, self).setup(build, machine, testdir)

        # XXX: track number of cores booted and seen for is_finished()
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(LibosTestMemtestMulti, self).get_modules(build, machine)
        self._libos_variant = self.name.split("_",2)[2]
        modules.add_module("memtest_%s" % self._libos_variant,
                           ["core=0-%d" % (machine.get_ncores()-1)])
        return modules

    def is_finished(self, line):
        # XXX: count number of times we have seen the finish string
        if line.startswith("memtest passed successfully!"):
            self._nseen += 1
        return self._nseen == self._ncores or super(LibosTestMemtestMulti, self).is_finished(line)

    def process_data(self, testdir, rawiter):
        nspawned = nseen = 0
        for line in rawiter:
            if re.match(r'.*pawning .*memtest_%s on core' % self._libos_variant, line):
                nspawned += 1
            if line.startswith("memtest passed successfully!"):
                nseen += 1
        return PassFailResult(nspawned > 0 and nspawned == nseen)

@tests.add_test
class LibbfPmapArrayTest(LibosTestMemtestMulti):
    name = "libos_memtest_pmap_array"

@tests.add_test
class LibbfPmapListTest(LibosTestMemtestMulti):
    name = "libos_memtest_pmap_list"

@tests.add_test
class LibbfPmapArrayMcnTest(LibosTestMemtestMulti):
    name = "libos_memtest_pmap_array_mcn"

@tests.add_test
class LibbfPmapListMcnTest(LibosTestMemtestMulti):
    name = "libos_memtest_pmap_list_mcn"
