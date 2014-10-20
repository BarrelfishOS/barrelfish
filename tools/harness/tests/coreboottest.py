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
class StopTest(InteractiveTest):
    '''Stop core test'''

    name = 'stop_park'

    def interact(self):
        self.wait_for_fish()

        debug.verbose("Stopping core 1.")
        self.console.sendline("x86boot stop 1")

        try:
            debug.verbose("Wait until core is down.")
            self.console.expect("Power it down...")
        except:
            raise

        time.sleep(3)

    def process_data(self, testdir, rawiter):
        passed = True
        for line in rawiter:
            if "user page fault in" in line:
                passed = False
                break

        return PassFailResult(passed)


@tests.add_test
class RestartTest(InteractiveTest):
    '''Restart a core test'''

    name = 'stop_park'

    def interact(self):
        try:
            debug.verbose("Waiting for fish.")
            self.console.expect("fish v0.2 -- pleased to meet you!")
        except:
            raise

        debug.verbose("Stopping core 1.")
        self.console.sendline("x86boot stop 1")

        try:
            debug.verbose("Wait until core is down.")
            self.console.expect("Power it down...")
        except:
            raise

        time.sleep(3)

    def process_data(self, testdir, rawiter):
        passed = True
        for line in rawiter:
            if "user page fault in" in line:
                passed = False
                break

        return PassFailResult(passed)

#@tests.add_test
class UpdateKernelTest(InteractiveTest):
    pass
