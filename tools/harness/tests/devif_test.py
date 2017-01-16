##########################################################################
# Copyright (c) 2017, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re, datetime
import debug, tests
from common import TestCommon, TimeoutError
from results import RowResults, PassFailResult

TEST_TIMEOUT = datetime.timedelta(minutes=8)


class DevifTests(TestCommon):

    def __init__(self, options):
        super(DevifTests, self).__init__(options)

    def get_module_name(self):
        return "devif_test"

    def boot(self, *args):
        super(DevifTests, self).boot(*args)
        self.set_timeout(TEST_TIMEOUT)

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("sfn5122f", ["auto", "function=0"])
        modules.add_module("devif_idc", ["core=1"])
        modules.add_module(self.get_module_name(), ["core=0", self.OP])

        return modules

    def is_finished(self, line):
        return line.startswith("SUCCESS")

    def process_data(self, testdir, rawiter):
        passed = False
        for line in rawiter:
            if "SUCCESS" in line:
                passed = True
        return PassFailResult(passed)

@tests.add_test
class DevifNetTxTest(DevifTests):
    ''' Devif Net TX Test'''
    name = "devif_net_tx_test"
    OP = "net_tx"

@tests.add_test
class DevifNetRxTest(DevifTests):
    ''' Devif Net RX Test'''
    name = "devif_net_rx_test"
    OP = "net_rx"

@tests.add_test
class DevifIdcTest(DevifTests):
    ''' Devif IDC Test'''
    name = "devif_idc_test"
    OP = "idc"
