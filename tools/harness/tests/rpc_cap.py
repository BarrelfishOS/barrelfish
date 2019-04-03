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

    def get_modules(self, build, machine):
        modules = super(RpcCapTestCommon, self).get_modules(build, machine)
        ccid = self.get_client_coreid(machine)

        modules.add_module("rpc_cap_test",
                ["core=%d" % machine.get_coreids()[0], "server"])
        modules.add_module("rpc_cap_test",
                ["core=%d" % ccid, "client", "id=0"])
        modules.add_module("rpc_cap_test",
                ["core=%d" % ccid, "client", "id=1"])
        modules.add_module("rpc_cap_test",
                ["core=%d" % ccid, "client", "id=2"])
        return modules

@tests.add_test
class RpcCapTestLocal(RpcCapTestCommon):
    ''' test cap transfer using RPC. Client/server on same core '''
    name = "rpc_cap_local"

    def get_client_coreid(self, machine):
        return machine.get_coreids()[0]


@tests.add_test
class RpcCapTestCross(RpcCapTestCommon):
    ''' test cap transfer using RPC. Cross-core '''
    name = "rpc_cap_cross"

    def get_client_coreid(self, machine):
        if machine.get_ncores() < 2:
            raise Exception("Machine must have at least 2 cores")
        return machine.get_coreids()[1]

