##########################################################################
# Copyright (c) 2012-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, tempfile, subprocess, time
import debug
from machines import ARMSimulatorBase, MachineFactory, ARMSimulatorOperations
import efiimage

FVP_PATH = '/home/netos/tools/DS-5/bin'

# Get the prebuilt binary snapshots (fvp-uefi.zip) from:
# https://community.arm.com/docs/DOC-10952
FVP_UEFI_BL1="/home/netos/tools/fvp-uefi/bl1.bin"
FVP_UEFI_FIP="/home/netos/tools/fvp-uefi/fip.bin"
FVP_LICENSE = '8224@sgv-license-01.ethz.ch'
FVP_START_TIMEOUT = 2 # in seconds

class FVPMachineBase(ARMSimulatorBase):
    imagename = "armv7_a9ve_1_image"

    def __init__(self, options, operations, **kwargs):
        if operations is None:
            operations = FVPMachineBaseOperations(self)
        super(FVPMachineBase, self).__init__(options, operations, **kwargs)
        self.simulator_start_timeout = FVP_START_TIMEOUT

    def get_buildall_target(self):
        return "VExpressEMM-A9"

class FVPMachineBaseOperations(ARMSimulatorOperations):

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
        env = dict(os.environ)
        env['ARMLMD_LICENSE_FILE'] = FVP_LICENSE
        self.child = \
            subprocess.Popen(cmd, stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT, env=env)
        #time.sleep(FVP_START_TIMEOUT)


class FVPMachineARMv7(FVPMachineBase):

    def __init__(self, options, operations, **kwargs):
        super(FVPMachineARMv7, self).__init__(options, operations, **kwargs)

    def set_bootmodules(self, modules):
        # write menu.lst in build directory
        debug.verbose("writing menu.lst in build directory")
        menulst_fullpath = os.path.join(self.options.builds[0].build_dir,
                "platforms", "arm", "menu.lst.armv7_a9ve_1")
        debug.verbose("writing menu.lst in build directory: %s" %
                menulst_fullpath)
        self._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        debug.verbose("building proper FVP image")
        debug.checkcmd(["make", self.imagename],
                cwd=self.options.builds[0].build_dir)

class FVPMachineARMv7NCores(FVPMachineARMv7):

    def __init__(self, options, **kwargs):
        super(FVPMachineARMv7NCores, self).__init__(options, FVPMachineARMv7NCoresOperations(self), **kwargs)

class FVPMachineARMv7NCoresOperations(FVPMachineBaseOperations):

    def _get_cmdline(self):
        self.get_free_port()

        # TODO: it may be feasible to use "-C motherboard.pl011_uart0.out_file=-"
        # instead of telnet to grab the stdout of the model. Cf.
        # http://infocenter.arm.com/help/topic/com.arm.doc.dui0848b/jka1396274340548.html
        return [os.path.join(FVP_PATH, "FVP_VE_Cortex-A9x" + str(self._machine.get_ncores())),
                # Don't try to pop an LCD window up
                "-C", "motherboard.vis.disable_visualisation=1",
                # Don't start a telnet xterm
                "-C", "motherboard.terminal_0.start_telnet=0",
                "-C", "motherboard.terminal_0.start_port=%d" % self.telnet_port,
                self._machine.kernel_img]

# Single core machine
MachineFactory.addMachine("armv7_fvp", FVPMachineARMv7NCores,
                          bootarch="armv7",
                          platform="a9ve")

# Quad-core machine
MachineFactory.addMachine('armv7_fvp_4', FVPMachineARMv7NCores,
                          bootarch="armv7",
                          platform="a9ve",
                          ncores=4)


class FVPMachineEFI(FVPMachineBase):
    imagename = "armv8_efi"

    def __init__(self, options, simulator=None, **kwargs):
        super(FVPMachineEFI, self).__init__(options, FVPMachineEFIOperations(self), **kwargs)
        assert(simulator)
        self.simulator = simulator

    def set_bootmodules(self, modules):
        # write menu.lst in build directory
        debug.verbose("writing menu.lst in build directory")
        menulst_fullpath = os.path.join(self.options.builds[0].build_dir,
                "platforms", "arm", "menu.lst.armv8_base")
        debug.verbose("writing menu.lst in build directory: %s" %
                menulst_fullpath)
        self._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)

#        debug.checkcmd(["make"] + modules.get_build_targets(), cwd=self.options.builds[0].build_dir)

        debug.verbose("building proper FVP image")
        efi = efiimage.EFIImage(self.kernel_img, 200)
        efi.create()
        for module in modules.get_build_targets():
            efi.addFile(os.path.join(self.options.builds[0].build_dir, module), module)
        efi.writeFile("startup.nsh", "Hagfish.efi hagfish.cfg")
        efi.addFile("/home/netos/tftpboot/Hagfish.efi", "Hagfish.efi")
        efi.addFile(menulst_fullpath, "hagfish.cfg")

    def get_buildall_target(self):
        # XXX: this is a misnomer in hake for the a57v platform
        return "FVP"

class FVPMachineEFIOperations(FVPMachineBaseOperations):


    def get_output(self):
        return self.child.stdout

    def _get_cmdline(self):
        self.get_free_port()

        return [os.path.join(FVP_PATH, self._machine.simulator),
                # Don't try to pop an LCD window up
                "-C", "bp.vis.disable_visualisation=1",
                # Don't start a telnet xterm
                "-C", "bp.terminal_0.start_telnet=0",
                "-C", "bp.terminal_1.start_telnet=0",
                "-C", "bp.secureflashloader.fname=%s" % FVP_UEFI_BL1,
                "-C", "bp.flashloader0.fname=%s" % FVP_UEFI_FIP,
                "-C", "bp.mmc.p_mmc_file=%s" % self._machine.kernel_img,
                "-C", "bp.pl011_uart0.unbuffered_output=1",
                # This has to be the last parameter because otherwise the command
                # passed to the OS has incorrect parameters. Don't know why
                # MH 11/2016
                "-C", "bp.pl011_uart0.out_file=-",
                ]

MachineFactory.addMachine('armv8_fvp_base', FVPMachineEFI,
                          bootarch='armv8',
                          platform='a57_fvp',
                          boot_timeout=60,
                          simulator="FVP_Base_AEMv8A")

MachineFactory.addMachine('armv8_fvp_a57x1', FVPMachineEFI,
                          bootarch='armv8',
                          boot_driver = 'boot_armv8_generic',
                          platform='a57_fvp',
                          boot_timeout=60,
                          simulator="FVP_Base_Cortex-A57x1")

MachineFactory.addMachine('armv8_fvp_a57x2_a53x4', FVPMachineEFI,
                          bootarch='armv8',
                          boot_driver = 'boot_armv8_generic',
                          platform='a57_fvp',
                          boot_timeout=60,
                          simulator="FVP_Base_Cortex-A57x2-A53x4")
