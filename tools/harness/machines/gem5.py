##########################################################################
# Copyright (c) 2012-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

# Quirks:
# * this is only running in single-core mode, since bootarm=0 is
#    used in above mentioned menu.lst

import os, signal, tempfile, subprocess, shutil, time
import debug, machines
from machines import ARMSimulatorBase, MachineFactory, ARMSimulatorOperations

GEM5_PATH = '/home/netos/tools/gem5/gem5-stable-1804'
# gem5 takes quite a while to come up. If we return right away,
# telnet will be opened too early and fails to connect
#
# SG, 2016-10-07: If this is too high, however, and we have an
# early-boot bug gem5 will exit before telnet connects, and we do
# not get the gem5 output at all
GEM5_START_TIMEOUT = 1 # in seconds

class Gem5MachineBase(ARMSimulatorBase):
    imagename = "armv7_a15ve_gem5_image"

    def __init__(self, options, operations, **kwargs):
        super(Gem5MachineBase, self).__init__(options, operations, **kwargs)

    def get_buildall_target(self):
        return "VExpressEMM-A15"

    def get_boot_timeout(self):
        # we set this to 10 mins since gem5 is very slow
        return 600

    def get_test_timeout(self):
        # give gem5 tests enough time to complete: skb initialization takes
        # about 10 minutes, so set timeout to 25 minutes.
        # RH, 2018-08-08 newer version of gem5 is even slower ...
        # increased to 50 mins
        return 50 * 60

class Gem5MachineBaseOperations(ARMSimulatorOperations):

    def __init__(self, machine):
        super(Gem5MachineBaseOperations, self).__init__(machine)
        self.simulator_start_timeout = GEM5_START_TIMEOUT
        # menu.lst template for gem5 is special
        # XXX: current template does not work because gem5 coreboot NYI
        self.menulst_template = "menu.lst.armv7_a15ve_gem5"

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            debug.verbose('creating temporary directory for Gem5 files')
            self.tftp_dir = tempfile.mkdtemp(prefix='harness_gem5_')
            debug.verbose('Gem5 install directory is %s' % self.tftp_dir)
        return self.tftp_dir

    def reboot(self):
        self._kill_child()
        cmd = self._get_cmdline()
        self.telnet_port = 3456
        debug.verbose('starting "%s" in gem5.py:reboot' % ' '.join(cmd))
        devnull = open('/dev/null', 'w')
        # remove ubuntu chroot from environment to make sure gem5 finds the
        # right shared libraries
        env = dict(os.environ)
        if 'LD_LIBRARY_PATH' in env:
            del env['LD_LIBRARY_PATH']

        self.child = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=devnull, env=env)
        time.sleep(GEM5_START_TIMEOUT)

class Gem5MachineARM(Gem5MachineBase):

    def __init__(self, options, operations, **kwargs):
        super(Gem5MachineARM, self).__init__(options, operations, **kwargs)

    def get_bootarch(self):
        return 'armv7'

    def get_platform(self):
        return 'a15ve'

class Gem5MachineARMOperations(Gem5MachineBaseOperations):

    def set_bootmodules(self, modules):
        # write menu.lst in build directory
        debug.verbose("writing menu.lst in build directory")
        menulst_fullpath = os.path.join(self._machine.options.builds[0].build_dir,
                "platforms", "arm", self.menulst_template)
        debug.verbose("writing menu.lst in build directory: %s" %
                menulst_fullpath)
        self._machine._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        debug.verbose("building proper gem5 image")
        debug.checkcmd(["make", self._machine.imagename],
                cwd=self._machine.options.builds[0].build_dir)


# SK: did not test this yet, but should work
# @machines.add_machine
# class Gem5MachineARMSingleCore(Gem5MachineARM):
#     name = 'gem5_arm_1'

#     def get_ncores(self):
#         return 1

#     def _get_cmdline(self):
#         script_path = os.path.join(self.options.sourcedir, 'tools/arm_gem5', 'gem5script.py')
#         return (['gem5.fast', script_path, '--kernel=%s'%self.kernel_img, '--n=%s'%self.get_ncores()]
#                 + GEM5_CACHES_ENABLE)


class Gem5MachineARMSingleCore(Gem5MachineARM):
    name = 'armv7_gem5'

    def __init__(self, options, **kwargs):
        super(Gem5MachineARMSingleCore, self).__init__(options, Gem5MachineARMSingleCoreOperations(self), **kwargs)


class Gem5MachineARMSingleCoreOperations(Gem5MachineARMOperations):

    def _get_cmdline(self):
        self.get_free_port()
        script_path = \
            os.path.join(self._machine.options.sourcedir, 'tools/arm_gem5',
                         'boot_gem5.sh')
        return ([script_path, 'VExpress_EMM', self._machine.kernel_img, GEM5_PATH,
                 str(self.telnet_port)])

MachineFactory.addMachine(Gem5MachineARMSingleCore.name, Gem5MachineARMSingleCore,
                          bootarch="armv7",
                          platform="a15ve")
