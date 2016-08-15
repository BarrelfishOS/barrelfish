##########################################################################
# Copyright (c) 2012-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, tempfile, subprocess, time
import debug, machines
from machines import ARMSimulatorBase

FVP_PATH = '/home/netos/tools/DS-5_v5.24.0/bin'
FVP_LICENSE = '8224@sgv-license-01.ethz.ch'
FVP_START_TIMEOUT = 5 # in seconds

class FVPMachineBase(ARMSimulatorBase):
    imagename = "armv7_a9ve_image"

    def __init__(self, options):
        super(FVPMachineBase, self).__init__(options)
        self.child = None
        self.telnet = None
        self.tftp_dir = None
        self.options = options
        self.simulator_start_timeout = FVP_START_TIMEOUT

    def get_buildall_target(self):
        return "VExpressEMM-A9"

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            debug.verbose('Creating temporary directory for FVP files')
            self.tftp_dir = tempfile.mkdtemp(prefix='harness_fvp_')
            debug.verbose('FVP install directory is %s' % self.tftp_dir)
        return self.tftp_dir

    def reboot(self):
        self._kill_child()
        cmd = self._get_cmdline()
        debug.verbose('starting "%s" in FVP:reboot' % ' '.join(cmd))
        devnull = open('/dev/null', 'w')
        env = dict(os.environ)
        env['ARMLMD_LICENSE_FILE'] = FVP_LICENSE
        self.child = \
            subprocess.Popen(cmd, stdout=subprocess.PIPE,
                             stderr=devnull, env=env)
        time.sleep(FVP_START_TIMEOUT)

class FVPMachineARMv7(FVPMachineBase):
    def get_bootarch(self):
        return 'armv7'

    def get_platform(self):
        return 'a9ve'

    def set_bootmodules(self, modules):
        # write menu.lst in build directory
        debug.verbose("writing menu.lst in build directory")
        menulst_fullpath = os.path.join(self.options.buildbase,
                "platforms", "arm", "menu.lst.armv7_a9ve")
        debug.verbose("writing menu.lst in build directory: %s" %
                menulst_fullpath)
        self._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        debug.verbose("building proper FVP image")
        debug.checkcmd(["make", self.imagename], cwd=self.options.buildbase)

@machines.add_machine
class FVPMachineARMv7SingleCore(FVPMachineARMv7):
    name = 'armv7_fvp'

    def get_ncores(self):
        return 1

    def get_cores_per_socket(self):
        return 1

    def _get_cmdline(self):
        self.get_free_port()

        return [os.path.join(FVP_PATH, "FVP_VE_Cortex-A9x1"), 
                # Don't try to pop an LCD window up
                "-C", "motherboard.vis.disable_visualisation=1",
                # Don't start a telnet xterm
                "-C", "motherboard.terminal_0.start_telnet=0",
                "-C", "motherboard.terminal_0.start_port=%d"%self.telnet_port,
                self.kernel_img]
