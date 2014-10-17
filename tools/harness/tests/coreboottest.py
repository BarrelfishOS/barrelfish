# -*- coding: utf-8 -*-

##########################################################################
# Copyright (c) 2014, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitätstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
from common import TestCommon, MAX_BOOT_ATTEMPTS, BOOT_TIMEOUT_LINE_RETRY, BOOT_TIMEOUT_LINE_FAIL
import pexpect, tempfile

class InteractiveTest(TestCommon):

    def interact(child):
        # Implement interaction with console
        pass

    def collect_data(self, machine):
        fh = machine.get_output()
        
        if not self.machine.get_boot_timeout():
            tt = 180
        else:
            tt = self.machine.get_boot_timeout()
        debug.verbose("Timeout set to %s", tt)

        child = pexpect.fdexpect.fdspawn(fh, timeout=tt)
        child.logfile = tempfile.NamedTemporaryFile(deleted=False)
        
        while self.boot_attempts < MAX_BOOT_ATTEMPTS:
            index = child.expect("Barrelfish CPU driver starting", 
                                 pexpect.TIMEOUT, pexpect.EOF)
            if index == 0:
                self.boot_phase = False
                break
            if index == 1:
                child.logfile.write(BOOT_TIMEOUT_LINE_RETRY)
                self.reboot(machine)
            if index == 2:
                child.logfile.write(BOOT_TIMEOUT_LINE_FAIL)

        if not self.boot_phase:
            self.interact(child)

        return child.logfile.readlines()

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        self.boot(machine, modules)
        return self.collect_data(machine)

@tests.add_test
class StopParkTest(InteractiveTest):
    '''Stop core and park OSNode test'''

    name = 'stop_park'

    def get_modules(self, build, machine):
        modules = super(StopParkTest, self).get_modules(build, machine)
        modules.add_module("x86boot")
        return modules

    def interact(child):
        child.expect(">")
        child.sendline("x86boot stop 1")

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            debug.verbose(line)
        return PassFailResult(True)

#@tests.add_test
class UpdateKernelTest(InteractiveTest):
    pass
