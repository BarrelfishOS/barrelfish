##########################################################################
# Copyright (c) 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetsstrasse 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import tests
from common import TestCommon
from results import PassFailResult
import random

class TomMathAbstract(TestCommon):

    def get_finish_string(self):
        return "DIGITS == 255...PASSED"

@tests.add_test
class TomMathMont(TomMathAbstract):
    '''runs compiler-rt1 builins unit tests'''
    name = "tommath-mont"

    def get_modules(self, build, machine):
        modules = super(TomMathMont, self).get_modules(build, machine)
        modules.add_module("tommath/mont", [ int(random.random() * 100000) ])
        return modules

    def process_data(self, testdir, rawiter):
        # the test passed if no error occurred
        passed = 3
        for line in rawiter:
            if "DIGITS == " in line:
                passed += 1
        return PassFailResult(passed == 255)
