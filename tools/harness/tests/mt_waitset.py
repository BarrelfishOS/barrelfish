##########################################################################
# Copyright (c) 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import tests
from common import TestCommon
from results import PassFailResult
import logging, sys

@tests.add_test
class MultithreadedWaitsetTest(TestCommon):
    '''multithreaded waitset functionality'''
    name = "mt_waitset"

    def get_modules(self, build, machine):
        modules = super(MultithreadedWaitsetTest, self).get_modules(build, machine)
        modules.add_module("mt_waitset", ["10", "10", "100000", "8"])
        return modules

    def is_finished(self, line):
        return "Test PASSED" in line or "Test FAILED" in line

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if "Test PASSED" in line:
                PassFailResult(True)
        PassFailResult(False)
