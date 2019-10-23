##########################################################################
# Copyright (c) 2014-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import debug, eth_machinedata
import subprocess, os, socket, sys, shutil, tempfile, pty
from machines import ARMMachineBase, MachineFactory, MachineOperations
from machines.eth import ETHBaseMachineOperations
import glob, barrelfish

# XXX: the rack-mount imx8x machines are ETH-specific
TOOLS_PATH='/home/netos/tools/bin'
RACKBOOT=os.path.join(TOOLS_PATH, 'rackboot.sh')
RACKPOWER=os.path.join(TOOLS_PATH, 'rackpower')

class IMX8XMachine(ARMMachineBase):
    '''Machine to run tests on locally attached IMX8X board.'''
    name = 'imx8x_local'
    imagename = "armv8_imx8x_image.efi"

    def __init__(self, options, **kwargs):
        super(IMX8XMachine, self).__init__(options, IMX8XOperations(self), **kwargs)
        self.menulst_template = "menu.lst.armv8_imx8x"

    def default_bootmodules(self):
        m = barrelfish.BootModules(self, prefix="armv8/sbin/")
        m.set_boot_driver("boot_armv8_generic")
        m.set_cpu_driver("cpu_imx8x")
        m.add_module("init")
        m.add_module("mem_serv")
        m.add_module("monitor")
        m.add_module("ramfsd", ["boot"])
        m.add_module("skb", ["boot"])
        m.add_module("kaluga", ["boot"])
        m.add_module("spawnd", ["boot"])
        m.add_module("proc_mgmt", ["boot"])
        m.add_module("/eclipseclp_ramfs.cpio.gz", ["nospawn"])
        m.add_module("/skb_ramfs.cpio.gz", ["nospawn"])
        return m

    def get_boot_driver(self):
        # bootdriver  /armv8/sbin/boot_armv8_generic
        return "boot_armv8_generic"

    def setup(self, builddir=None):
        pass

class IMX8XOperations(MachineOperations):

    def __init__(self, machine):
        super(IMX8XOperations, self).__init__(machine)
        self.picocom = None
        self.tftp_dir = None
        self.masterfd = None

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            self.tftp_dir = tempfile.mkdtemp(prefix="imx8x_")
        return self.tftp_dir

    def set_bootmodules(self, modules):
        menulst_fullpath = os.path.join(self._machine.options.builds[0].build_dir,
                "platforms", "arm", self._machine.menulst_template)
        self._machine._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        debug.verbose("building proper imx8x image")
        debug.checkcmd(["make", self._machine.imagename],
                cwd=self._machine.options.builds[0].build_dir)

    def __usbboot(self):
        debug.verbose("Usbbooting imx8x")
        debug.verbose("build dir: %s" % self._machine.options.builds[0].build_dir)
        debug.checkcmd(["make", "usbboot_imx8x"],
                cwd=self._machine.options.builds[0].build_dir)

    def lock(self):
        pass

    def unlock(self):
        pass

    def reboot(self):
        self.__usbboot()

    def shutdown(self):
        '''shutdown: close picocom'''
        # FIXME: sending C-A C-X to close picocom does not seem to work
        #if self.masterfd is not None:
        #    debug.verbose("Sending C-A C-X to picocom")
        #    os.write(self.masterfd, "\x01\x24")
        if self.picocom is not None:
            debug.verbose("Killing picocom")
            self.picocom.kill()
        self.picocom = None
        self.masterfd = None

    def get_output(self):
        '''Use picocom to get output. This replicates part of
        ETHMachine.lock()'''
        (self.masterfd, slavefd) = pty.openpty()
        ttydevs = glob.glob("/dev/ttyColibri*")
        if len(ttydevs) != 1:
            print("Can't get output. Found %d /dev/ttyColibri*" % len(ttydevs))
            print("Make sure you have exactly one colibri connected and")
            print("udev rules installed")
            raise Exception("/dev/ttyColibri* is not unique")
                  
        self.picocom = subprocess.Popen(
                ["picocom", "-b", "115200", ttydevs[0]],
                close_fds=True, stdout=slavefd, stdin=slavefd)
        os.close(slavefd)
        self.console_out = os.fdopen(self.masterfd, 'rb', 0)
        return self.console_out


class ETHRackIMX8XMachine(ARMMachineBase):
    _machines = eth_machinedata.galaboards
    imagename = "armv8_imx8x_image.efi"

    def __init__(self, options, **kwargs):
        super(ETHRackIMX8XMachine, self).__init__(options, ETHRackIMX8XMachineOperations(self), **kwargs)
        self.menulst_template = "menu.lst.armv8_imx8x"

    def setup(self, builddir=None):
        pass

    def get_platform(self):
        return 'imx8x'

class ETHRackIMX8XMachineOperations(ETHBaseMachineOperations):
    def __init__(self, machine):
        super(ETHRackIMX8XMachineOperations, self).__init__(machine)
        self._tftp_dir = None
        self.targe_name = None

    def __get_gala_num(self):
        return int(self.name[4:])

    def set_bootmodules(self, modules):
        menulst_fullpath = os.path.join(self._machine.options.builds[0].build_dir,
                "platforms", "arm", self._machine.menulst_template)
        self._machine._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        source_name = os.path.join(self._machine.options.builds[0].build_dir, self._machine.imagename)
        self.target_name = os.path.join(self.get_tftp_dir(), self._machine.imagename)
        debug.verbose("building proper imx8x image")
        debug.checkcmd(["make", self._machine.imagename],
                cwd=self._machine.options.builds[0].build_dir)
        #debug.verbose("copying %s to %s" % (source_name, self.target_name))
        #shutil.copyfile(source_name, self.target_name)
        #self.__chmod_ar(self.target_name)

    def __usbboot(self):
        print("!!!! IN REBOOT")
        debug.checkcmd([
            "tools/imx8x/bf-boot.sh",
            "--bf", self.imagename,
            "--board", self.__get_gala_num()])

    def __rackpower(self, arg):
        try:
            debug.checkcmd([RACKPOWER, arg, self._machine.get_machine_name()])
        except subprocess.CalledProcessError:
            debug.warning("rackpower %s %s failed" %
                          (arg, self._machine.get_machine_name()))

    def reboot(self):
        self.__usbboot()

    def shutdown(self):
        self.__rackpower('-d')

#for pb in ETHRackIMX8XMachine._machines:
#    class TmpMachine(ETHRackIMX8XMachine):
#        name = pb
#    MachineFactory.addMachine(pb, TmpMachine, **ETHRackIMX8XMachine._machines[pb])

MachineFactory.addMachine("imx8x_local", IMX8XMachine,
                          bootarch='armv8',
                          platform='imx8x',
                          ncores=4)
