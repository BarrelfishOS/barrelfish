##########################################################################
# Copyright (c) 2009 - 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
from common import TestCommon
from results import PassFailResult, PassFailMultiResult

@tests.add_test
class NkmTest(TestCommon):
    '''memory kernel interface tests'''
    name = "nkmtest"

    def get_modules(self, build, machine):
        modules = super(NkmTest, self).get_modules(build, machine)
        modules.add_module("nkmtest_all")
        return modules

    def get_finish_string(self):
        return "nkmtest_all: all tests passed"

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)

@tests.add_test
class NkmTestMapOffset(TestCommon):
    '''test that kernel map interface does check source frame size'''
    name = "nkmtest_map_offset"

    def get_modules(self, build, machine):
        modules = super(NkmTestMapOffset, self).get_modules(build, machine)
        modules.add_module("nkmtest_map_offset")
        return modules

    def get_finish_string(self):
        return "nkmtest_map_offset: "

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        errors = []
        for line in rawiter:
            if line.startswith("nkmtest_map_offset: FAILURE"):
                errors.append(line)
        return PassFailMultiResult(self.name, errors)
