##########################################################################
# Copyright (c) 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################
import datetime
import re
import tests
from common import TestCommon
from results import PassFailResult

#IRQTEST_TIMEOUT = datetime.timedelta(minutes=5)

class IRQTestCommon(TestCommon):
    '''PCI IRQ test'''
    
    def get_modules(self, build, machine):
        modules = super(IRQTestCommon, self).get_modules(build, machine)
        # This makes kaluga start the irqtest binary for e1000 cards
        modules.add_module("e1000n_irqtest", ["auto"])
        return modules

    def get_finish_string(self):
        return "TEST "
    
    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if line.startswith("TEST SUCCESS"):
                return PassFailResult(True)
        return PassFailResult(False)

@tests.add_test
class IRQTestLegacy(IRQTestCommon):
    '''PCI Legacy IRQ test'''
    name = "irqtestlegacy"
    
    def get_modules(self, build, machine):
        modules = super(IRQTestLegacy, self).get_modules(build, machine)
        # This makes kaluga start the irqtest binary for e1000 cards
        modules.add_module_arg("kaluga","add_device_db=device_db_irqtest_legacy")
        return modules

    def get_finish_string(self):
        return "TEST "
    
    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if line.startswith("TEST SUCCESS"):
                return PassFailResult(True)
        return PassFailResult(False)

@tests.add_test
class IRQTestMSIX(IRQTestCommon):
    '''PCI MSIX IRQ test'''
    name = "irqtestmsix"
    
    def get_modules(self, build, machine):
        modules = super(IRQTestMSIX, self).get_modules(build, machine)
        # This makes kaluga start the irqtest binary for e1000 cards
        modules.add_module_arg("kaluga","add_device_db=device_db_irqtest_msix")
        return modules
