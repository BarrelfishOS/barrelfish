# -*- coding: utf-8 -*-

##########################################################################
# Copyright (c) 2014, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests, debug, pexpect
from common import InteractiveTest
from results import PassFailResult

START_CPU_DRIVER = "Barrelfish CPU driver starting"

class CoreCtrlTest(InteractiveTest):
    '''Framework for coreboot test'''

    def get_modules(self, build, machine):
        modules = super(CoreCtrlTest, self).get_modules(build, machine)
        if machine.get_ncores() > 2:
            self.core = 2
        else:
            self.core = 1
        return modules

@tests.add_test
class StopCoreTest(CoreCtrlTest):
    '''Stop a core.'''

    name = 'stop_core'

    def get_modules(self, build, machine):
        modules = super(StopCoreTest, self).get_modules(build, machine)
        modules.add_module("periodicprint", args=["core=%d" % self.core ])
        return modules

    def interact(self):
        self.wait_for_fish()

        # wait for app
        self.console.expect("On core %s" % self.core)

        debug.verbose("Stopping core %s." % self.core)
        self.console.sendline("corectrl stop %s\n" % self.core)

        # Stop core
        debug.verbose("Wait until core is down.")
        self.console.expect("Core %s stopped." % self.core)
        # cannot wait for prompt here, as new cleanup routine will wait for
        # answer from monitor on stopped core.
        #self.wait_for_prompt()

        debug.verbose("making sure that core is down.")
        # Make sure app is no longer running
        i = self.console.expect(["On core %s" % self.core, pexpect.TIMEOUT], timeout=10)
        debug.verbose("got %d from last expect" % i)
        if i == 0:
            raise Exception("periodicprint still running, did we not shut-down the core?")

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if ("Core %s stopped." % self.core) in line:
                return PassFailResult(True)

        return PassFailResult(False)


@tests.add_test
class UpdateKernelTest(CoreCtrlTest):
    '''Update a kernel on a core.'''

    name = 'update_kernel'

    def get_modules(self, build, machine):
        modules = super(UpdateKernelTest, self).get_modules(build, machine)
        modules.add_module("periodicprint", args=["core=%d" % self.core])
        return modules

    def interact(self):
        self.wait_for_fish()

        # wait for app
        self.console.expect("On core %s" % self.core)

        # Reboot core
        self.console.sendline("corectrl update %s\n" % self.core)
        self.console.expect(START_CPU_DRIVER)
        self.wait_for_prompt()

        # Make sure app is still running
        self.console.expect("On core %s" % self.core)

    def process_data(self, testdir, rawiter):
        output_count = 0
        for line in rawiter:
            if ("On core %s" % self.core) in line:
                output_count = output_count + 1

        return PassFailResult(output_count > 1)


@tests.add_test
class ParkOSNodeTest(CoreCtrlTest):
    '''Park an OSNode on a core.'''
    name = 'park_osnode'

    def get_modules(self, build, machine):
        modules = super(ParkOSNodeTest, self).get_modules(build, machine)
        if machine.get_ncores() > 3:
            self.target_core = 3
        else:
            self.target_core = 0
        modules.add_module("periodicprint", args=["core=%d" % self.core])
        return modules

    def interact(self):
        self.wait_for_fish()

        self.console.expect("On core %s" % self.core)

        # Park
        debug.verbose("Park OSNode from %s on %s." % (self.core, self.target_core))
        self.console.sendline("corectrl park %s %s\n" % (self.core, self.target_core))
        self.wait_for_prompt()

        self.console.expect("On core %s" % self.target_core)

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if ("On core %s" % self.target_core) in line:
                return PassFailResult(True)
        return PassFailResult(False)


@tests.add_test
class ListKCBTest(CoreCtrlTest):
    '''List all KCBs.'''
    name = 'list_kcb_cores'

    def interact(self):
        self.wait_for_fish()
        debug.verbose("Running corectrl lskcb")
        self.console.sendline("corectrl lskcb")
        self.console.expect("KCB 0:")
        self.wait_for_prompt()

        debug.verbose("Running corectrl lscpu")
        self.console.sendline("corectrl lscpu\n")
        self.console.expect("CPU 0:")
        self.wait_for_prompt()

    def process_data(self, testdir, rawiter):
        found_kcb_output = False
        found_cpu_output = False
        for line in rawiter:
            if "KCB 0:" in line:
                found_kcb_output = True
            if "CPU 0:" in line:
                found_cpu_output = True
        return PassFailResult(found_kcb_output and found_cpu_output)


@tests.add_test
class ParkRebootTest(CoreCtrlTest):
    '''Park OSNode and move it back.'''
    name = 'park_boot'

    def get_modules(self, build, machine):
        modules = super(ParkRebootTest, self).get_modules(build, machine)
        self.core = 1
        if machine.get_ncores() <= 2:
            self.parking_core = 0
        else:
            self.parking_core = 2
        modules.add_module("periodicprint", args=["core=%d" % self.core])
        return modules

    def interact(self):
        self.wait_for_fish()

        self.console.expect("On core %s" % self.core)
        self.console.expect("On core %s" % self.core)

        # Park
        debug.verbose("Park KCB %s on core %s." % (self.core, self.parking_core))
        self.console.sendline("corectrl park %s %s\n" % (self.core, self.parking_core))
        self.wait_for_prompt()

        self.console.expect("On core %s" % self.parking_core)
        self.console.expect("On core %s" % self.parking_core)

        # Unpark
        debug.verbose("Unpark KCB %s from core %s." % (self.core, self.parking_core))
        self.console.sendline("corectrl unpark %s\n" % (self.core))
        self.wait_for_prompt()

        # Reboot home core with kcb
        self.console.expect("On core %s" % self.core)
        self.console.expect("On core %s" % self.core)

    def process_data(self, testdir, rawiter):
        output_count = 0
        parked_count = 0

        for line in rawiter:
            if ("On core %s" % self.parking_core) in line:
                parked_count = parked_count + 1
            if ("On core %s" % self.core) in line:
                output_count = output_count + 1

        return PassFailResult(parked_count >= 2 and output_count >= 4)
