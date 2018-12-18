##########################################################################
# Copyright (c) 2018, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
from common import TestCommon
from results import PassFailResult

@tests.add_test
class MallocTest(TestCommon):
    '''basic malloc functionality on a single core'''
    name = "malloctest"

    def get_modules(self, build, machine):
        modules = super(MallocTest, self).get_modules(build, machine)
        modules.add_module("malloctest")
        return modules

    def get_finish_string(self):
        return "malloctest done."

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)
