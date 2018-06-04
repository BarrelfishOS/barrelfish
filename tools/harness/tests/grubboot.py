##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
from common import TestCommon
from results import PassFailResult

@tests.add_test
class GrubBootTest(TestCommon):
    '''Simple test that checks if the machine boots into grub'''
    name = "grubboot"

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

    def get_finish_string(self):
        return "GNU GRUB"

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if line.find("GNU GRUB  version 0.97-os.3") > -1:
                return PassFailResult(True)
        return PassFailResult(False)

