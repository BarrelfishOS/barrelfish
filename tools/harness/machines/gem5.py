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
from machines import ARMSimulatorBase

GEM5_PATH = '/home/netos/tools/gem5/gem5-stable'
# gem5 takes quite a while to come up. If we return right away,
# telnet will be opened too early and fails to connect
GEM5_START_TIMEOUT = 5 # in seconds

class Gem5MachineBase(ARMSimulatorBase):
    imagename = "armv7_a15ve_image"

    def __init__(self, options):
        super(Gem5MachineBase, self).__init__(options)
        self.child = None
        self.telnet = None
        self.tftp_dir = None
        self.options = options
        self.simulator_start_timeout = GEM5_START_TIMEOUT
        # menu.lst template for gem5 is special
        self.menulst_template += "_gem5"

    def get_buildall_target(self):
        return "VExpressEMM-A15"

    def get_boot_timeout(self):
        # we set this to 10 mins since gem5 is very slow
        return 600

    def get_test_timeout(self):
        # give gem5 tests enough time to complete: skb initialization takes
        # about 10 minutes, so set timeout to 25 minutes.
        return 25 * 60

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            debug.verbose('creating temporary directory for Gem5 files')
            self.tftp_dir = tempfile.mkdtemp(prefix='harness_gem5_')
            debug.verbose('Gem5 install directory is %s' % self.tftp_dir)
        return self.tftp_dir

    def reboot(self):
        self._kill_child()
        cmd = self._get_cmdline()
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
    def get_bootarch(self):
        return 'armv7'

    def get_platform(self):
        return 'a15ve'

    def set_bootmodules(self, modules):
        # write menu.lst in build directory
        debug.verbose("writing menu.lst in build directory")
        menulst_fullpath = os.path.join(self.options.buildbase,
                "platforms", "arm", "menu.lst.armv7_a15ve")
        debug.verbose("writing menu.lst in build directory: %s" %
                menulst_fullpath)
        self._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        debug.verbose("building proper gem5 image")
        debug.checkcmd(["make", self.imagename], cwd=self.options.buildbase)


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

@machines.add_machine
class Gem5MachineARMSingleCore(Gem5MachineARM):
    name = 'armv7_gem5'

    def get_bootarch(self):
        return "armv7"

    def get_platform(self):
        return 'a15ve'

    def get_ncores(self):
        return 1

    def get_cores_per_socket(self):
        return 1

    def _get_cmdline(self):
        self.get_free_port()
        script_path = \
            os.path.join(self.options.sourcedir, 'tools/arm_gem5',
                         'boot_gem5.sh')
        return ([script_path, 'VExpress_EMM', self.kernel_img, GEM5_PATH,
                 str(self.telnet_port)])
