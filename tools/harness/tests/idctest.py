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
from common import TestCommon
from results import PassFailResult

MATCH_RE = "rx_caps 69 \[Frame cap[^\]]*\] \[x86_64 Page table at [^\]]*\]"

class IdcTestCommon(TestCommon):
    ''' Execute IDC test '''
    name = "idc"

    def get_finish_string(self):
        return "client all done!"

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if re.search(MATCH_RE, line):
                return PassFailResult(True)
        return PassFailResult(False)

@tests.add_test
class IdcTestLocal(IdcTestCommon):
    ''' Execute IDC test. Client/server on same core'''
    name = "idc_local"

    def get_modules(self, build, machine):
        modules = super(IdcTestLocal, self).get_modules(build, machine)
        modules.add_module("idctest",
                ["core=%d" % machine.get_coreids()[0], "server"])
        modules.add_module("idctest",
                ["core=%d" % machine.get_coreids()[0], "client"])
        return modules

@tests.add_test
class IdcTestCross(IdcTestCommon):
    ''' Execute IDC test. Client/server on different core'''
    name = "idc_cross"

    def get_modules(self, build, machine):
        modules = super(IdcTestCross, self).get_modules(build, machine)
        modules.add_module("idctest",
                ["core=%d" % machine.get_coreids()[0], "server"])
        modules.add_module("idctest",
                ["core=%d" % machine.get_coreids()[1], "client"])
        return modules
