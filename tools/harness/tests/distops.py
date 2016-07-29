##########################################################################
# Copyright (c) 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
import re
from common import TestCommon
from results import PassFailResult

@tests.add_test
class DistRetypeTest(TestCommon):
    '''test distributed retype code'''
    name = "distops_retype"

    def get_modules(self, build, machine):
        modules = super(DistRetypeTest, self).get_modules(build, machine)
        modules.add_module("test_remote_retype", [ "core=0", "server" ])
        modules.add_module("test_remote_retype", [ "core=1", "client" ])
        return modules

    def get_finish_string(self):
        return "distops_retype: client done"

    def process_data(self, testdir, rawiter):
        # the test passed iff we do not find a line matching
        # distops_retype: in client: .* expected .*
        error_re = re.compile("^.*distops_retype: in client: .* expected .*$")
        passed = True
        for line in rawiter:
            if error_re.match(line):
                passed = False
        return PassFailResult(passed)
