# -*- coding: utf-8 -*-

##########################################################################
# Copyright (c) 2014, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitätstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests, debug, time, pexpect
from common import InteractiveTest

START_CPU_DRIVER = "Barrelfish CPU driver starting"

@tests.add_test
class StopCoreTest(InteractiveTest):
    '''Stop a core.'''

    name = 'stop_core'

    def get_modules(self, build, machine):
        modules = super(StopCoreTest, self).get_modules(build, machine)
        modules.add_module("periodicprint", args=["core=2"])
        return modules

    def interact(self):
        self.wait_for_fish()

        time.sleep(5)
        self.core = 2
        debug.verbose("Stopping core %s." % self.core)
        self.console.sendline("x86boot stop %s" % self.core)
        
        # Stop core
        debug.verbose("Wait until core is down.")
        self.console.expect("Core %s stopped." % self.core)
        self.wait_for_prompt()

        # Make sure app is no longer running
        i = self.console.expect(["On core %s" % self.core, pexpect.TIMEOUT], timeout=10)
        if i == 0:
            raise Exception("periodicprint still running, did we not shut-down the core?")


@tests.add_test
class UpdateKernelTest(InteractiveTest):
    '''Update a kernel on a core.'''

    name = 'update_kernel'

    def get_modules(self, build, machine):
        modules = super(UpdateKernelTest, self).get_modules(build, machine)
        modules.add_module("periodicprint", args=["core=2"])
        return modules

    def interact(self):
        self.core = 2
        self.wait_for_fish()

        # Reboot core
        self.console.sendline("x86boot update %s" % self.core)
        self.console.expect(START_CPU_DRIVER)
        self.wait_for_prompt()

        # Make sure app is still running
        self.console.expect("On core %s" % self.core)


@tests.add_test
class ParkOSNodeTest(InteractiveTest):
    '''Park an OSNode on a core.'''
    name = 'park_osnode'

    def get_modules(self, build, machine):
        modules = super(ParkOSNodeTest, self).get_modules(build, machine)
        modules.add_module("periodicprint", args=["core=2"])
        return modules

    def interact(self):
        self.wait_for_fish()
        
        self.core = 2
        self.target_core = 3

        self.console.expect("On core %s" % self.core)

        # Stop
        debug.verbose("Stopping core %s." % self.core)
        self.console.sendline("x86boot stop %s" % self.core)
        self.wait_for_prompt()

        # Park
        debug.verbose("Transfer OSNode from %s to %s." % (self.core, self.target_core))
        self.console.sendline("x86boot give %s %s" % (self.core, self.target_core))
        self.wait_for_prompt()

        self.console.expect("On core %s" % self.target_core)


@tests.add_test
class ListKCBTest(InteractiveTest):
    '''List all KCBs.'''
    name = 'list_kcb_cores'

    def interact(self):
        self.wait_for_fish()
        self.console.sendline("x86boot lskcb")
        self.console.expect("KCB 1:")
        self.wait_for_prompt()

        self.console.sendline("x86boot lscpu")
        self.console.expect("CPU 0:")
        self.wait_for_prompt()


@tests.add_test
class ParkRebootTest(InteractiveTest):
    '''Park OSNode and move it back.'''
    name = 'park_boot'

    def get_modules(self, build, machine):
        modules = super(ParkRebootTest, self).get_modules(build, machine)
        modules.add_module("periodicprint", args=["core=1"])
        return modules

    def interact(self):
        self.wait_for_fish()
        
        self.core = 1
        self.parking_core = 2

        self.console.expect("On core %s" % self.core)
        self.console.expect("On core %s" % self.core)

        # Stop
        debug.verbose("Stopping core %s." % self.core)
        self.console.sendline("x86boot stop %s" % self.core)
        self.wait_for_prompt()

        # Park
        debug.verbose("Transfer OSNode from %s to %s." % (self.core, self.parking_core))
        self.console.sendline("x86boot give %s %s" % (self.core, self.parking_core))
        self.wait_for_prompt()
        self.console.expect("On core %s" % self.parking_core)
        self.console.expect("On core %s" % self.parking_core)

        # Remove KCB on parking core
        debug.verbose("Remove KCB on parking core %s." % (self.parking_core))
        self.console.sendline("x86boot rmkcb %s" % (self.core))
        self.wait_for_prompt()

        # Reboot home core with kcb
        debug.verbose("Reboot core %s." % (self.core))
        self.console.sendline("x86boot boot -m %s" % (self.core))
        self.wait_for_prompt() 
        self.console.expect("On core %s" % self.core)
        self.console.expect("On core %s" % self.core)
