##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
from common import TestCommon
from results import PassFailResult

@tests.add_test
class BootTest(TestCommon):
    '''Simple test that checks if the machine boots into grub/hagfish'''
    name = "boottest"
    grub_boot = "GNU GRUB  version 0.97-os.3"
    hagfish_boot = "Hagfish UEFI loader starting"

    def setup(self, build, machine, testdir):
        # Don't build BF
        machine.lock()
        machine.setup()

    def run(self, build, machine, testdir):
        self.reboot(machine)
        # Ignore the boot phase
        self.boot_phase = False
        return self.collect_data(machine)

    def cleanup(self, machine):
        machine.shutdown()
        machine.unlock()

    def is_finished(self, line):
        # Exit test when we get an assertion failure or an abort, rather than
        # waiting for timeout
        return self.grub_boot in line or \
               self.hagfish_boot in line or \
               line.startswith("Assertion failed on core") or \
               line.startswith("Aborted")

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if line.find(self.grub_boot) > -1 or \
               line.find(self.hagfish_boot) > -1:
                return PassFailResult(True)
        return PassFailResult(False)

