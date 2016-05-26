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

    def get_machine_irqtest_args(self, machine):
        '''For a given machine, return the paramaters passed to irqtest.
        It should contain of the e1000 PCI device id, and if there are multiple
        cards, also the PCI function.'''
        mn = machine.get_machine_name()
        if mn.startswith("sbrinz"):
            return ["deviceid=0x1079", "function=0"]
        elif mn == "gruyere":
            return ["deviceid=0x1076"]
        elif mn == "appenzeller":
            return ["deviceid=0x10d3"]
        elif mn.startswith("nos"):
            return ["deviceid=0x107d"]
        elif mn.startswith("tomme"):
            return ["deviceid=0x10a7", "function=0"]
        else:
            raise Exception("Machine %s not supported" % mn)
        
    
    def get_modules(self, build, machine):
        modules = super(IRQTest, self).get_modules(build, machine)
        modules.add_module("irqtest",
                ["core=%d" % machine.get_coreids()[1]] + self.get_machine_irqtest_args(machine))
        return modules

    def is_finished(self, line):
        return line.startswith("TEST ") 
    
    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if line.startswith("TEST SUCCESS"):
                return PassFailResult(True)
        return PassFailResult(False)
