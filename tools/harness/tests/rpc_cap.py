##########################################################################
# Copyright (c) 2009, ETH Zurich.
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

class RpcCapTestCommon(TestCommon):
    def get_finish_string(self):
        return "TEST PASSED"

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)

@tests.add_test
class RpcCapTestLocal(RpcCapTestCommon):
    ''' test cap transfer using RPC. Client/server on same core '''
    name = "rpc_cap_local"

    def get_modules(self, build, machine):
        modules = super(RpcCapTestLocal, self).get_modules(build, machine)
        modules.add_module("rpc_cap_test",
                ["core=%d" % machine.get_coreids()[0], "server"])
        modules.add_module("rpc_cap_test",
                ["core=%d" % machine.get_coreids()[0], "client"])
        return modules



@tests.add_test
class RpcCapTestCross(RpcCapTestCommon):
    ''' test cap transfer using RPC. Cross-core '''
    name = "rpc_cap_cross"

    def get_modules(self, build, machine):
        if machine.get_ncores() < 2:
            raise Exception("Machine must have at least 2 cores")

        modules = super(RpcCapTestCross, self).get_modules(build, machine)
        modules.add_module("rpc_cap_test",
                ["core=%d" % machine.get_coreids()[0], "client"])
        modules.add_module("rpc_cap_test",
                ["core=%d" % machine.get_coreids()[1], "server"])
        return modules
