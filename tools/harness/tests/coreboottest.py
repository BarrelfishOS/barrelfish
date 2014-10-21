# -*- coding: utf-8 -*-

##########################################################################
# Copyright (c) 2014, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitätstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests, debug
from common import InteractiveTest
from results import PassFailResult

@tests.add_test
class StopCoreTest(InteractiveTest):
    '''Stop a core.'''

    name = 'stop_core'

    def interact(self):
        self.wait_for_fish()

        debug.verbose("Stopping core 1.")
        self.console.sendline("x86boot stop 1")
        debug.verbose("Wait until core is down.")
        self.console.expect("Power it down...")
        
        self.wait_for_prompt()
        time.sleep(5)

    def process_data(self, testdir, rawiter):
        passed = True
        for line in rawiter:
            if "user page fault in" in line:
                passed = False
                break

        return PassFailResult(passed)


@tests.add_test
class UpdateKernelTest(InteractiveTest):
    '''Update a kernel on a core.'''

    name = 'update_kernel'

    def interact(self):
        self.wait_for_fish()

        debug.verbose("Stopping core 1.")
        self.console.sendline("x86boot stop 1")
        debug.verbose("Wait until core is down.")
        self.console.expect("Power it down...")

        self.wait_for_prompt()
        self.console.sendline("x86boot up 1")
        self.console.expect("Core 1 up")

        self.wait_for_prompt()
        time.sleep(5)

    def process_data(self, testdir, rawiter):
        passed = True
        for line in rawiter:
            if "user page fault in" in line:
                passed = False
                break

        return PassFailResult(passed)


@tests.add_test
class ParkKernelTest(InteractiveTest):
    '''Park an OSNode on a core.'''

    name = 'park_osnode'

    def interact(self):
        self.wait_for_fish()

        debug.verbose("Stopping core 1.")
        self.console.sendline("x86boot stop 1")
        self.wait_for_prompt()
        self.console.sendline("x86boot give 1 2")
        time.sleep(5)

    def process_data(self, testdir, rawiter):
        passed = True
        for line in rawiter:
            if "user page fault in" in line:
                passed = False
                break

        return PassFailResult(passed)

