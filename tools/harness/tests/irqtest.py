##########################################################################
# Copyright (c) 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################
import datetime
import re
import tests
from common import TestCommon
from results import PassFailResult

#IRQTEST_TIMEOUT = datetime.timedelta(minutes=5)

@tests.add_test
class IRQTest(TestCommon):
    '''PCI IRQ test'''
    name = "irqtest"
    
    def get_modules(self, build, machine):
        modules = super(IRQTest, self).get_modules(build, machine)
        # This makes kaluga start the irqtest binary for e1000 cards
        modules.add_module_arg("kaluga","add_device_db=device_db_irqtest")
        modules.add_module("e1000n_irqtest", ["auto"])
        return modules

    def is_finished(self, line):
        return line.startswith("TEST ") 
    
    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if line.startswith("TEST SUCCESS"):
                return PassFailResult(True)
        return PassFailResult(False)
