##########################################################################
# Copyright (c) 2014-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import debug, eth_machinedata
import subprocess, os, socket, sys, shutil, tempfile, pty, getpass
from machines import ARMMachineBase, MachineFactory, MachineOperations
from machines.eth import ETHMachineOperations
import glob, barrelfish

# XXX: the rack-mount colibri machines are ETH-specific
TOOLS_PATH='/home/netos/tools/bin'
RACKBOOT=os.path.join(TOOLS_PATH, 'rackboot.sh')
RACKPOWER=os.path.join(TOOLS_PATH, 'rackpower')
BFBOOT='/home/netos/tools/imx8x-boot/bf-boot.sh'

class ColibriMachine(ARMMachineBase):
    '''Machine to run tests on locally attached Imx8x/Colibri board.'''
    name = 'colibri_local'
    imagename = "armv8_imx8x_image.efi"

    def __init__(self, options, ops, **kwargs):
        super(ColibriMachine, self).__init__(options, ops, **kwargs)
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
        m.add_module("startd", ["boot"])
        m.add_module("proc_mgmt", ["boot"])
        m.add_module("/eclipseclp_ramfs.cpio.gz", ["nospawn"])
        m.add_module("/skb_ramfs.cpio.gz", ["nospawn"])
        m.add_module("corectrl", ["auto"])
        m.add_module("serial_lpuart", ["auto"])
        m.add_module("pl390_dist", ["auto"])
        m.add_module("int_route", ["auto"])
        return m

    def get_boot_driver(self):
        # bootdriver  /armv8/sbin/boot_armv8_generic
        return "boot_armv8_generic"

    def setup(self, builddir=None):
        pass

class ColibriLocalMachine(ColibriMachine):
    '''Machine to run tests on locally attached Imx8x/Colibri board.'''
    name = 'colibri_local'
    def __init__(self, options, **kwargs):
        super(ColibriLocalMachine, self).__init__(options, ColibriMachineOperations(self), **kwargs)

class ColibriMachineOperations(MachineOperations):

    def __init__(self, machine):
        super(ColibriMachineOperations, self).__init__(machine)
        self.picocom = None
        self.tftp_dir = None
        self.masterfd = None

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            self.tftp_dir = tempfile.mkdtemp(prefix="imx8x_")
        return self.tftp_dir

    def set_bootmodules(self, modules):
        self._colibri_devtty()
        menulst_fullpath = os.path.join(self._machine.options.builds[0].build_dir,
                "platforms", "arm", self._machine.menulst_template)
        self._machine._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        debug.verbose("building proper imx8x image")
        debug.checkcmd(["make", self._machine.imagename],
                cwd=self._machine.options.builds[0].build_dir)

    def __usbboot(self):
        self._colibri_devtty()
        debug.verbose("Usbbooting imx8x")
        debug.verbose("build dir: %s" % self._machine.options.builds[0].build_dir)
        debug.checkcmd(["make", "usbboot_imx8x"],
                cwd=self._machine.options.builds[0].build_dir)

    def lock(self):
        pass

    def unlock(self):
        pass

    def _colibri_devtty(self):
        """Check that there is only one colibri attached, if so, returns
        the ttyDevice path. Exception otherwise"""
        ttydevs = glob.glob("/dev/ttyColibri*")
        if len(ttydevs) != 1:
            print("Can't get output. Found %d /dev/ttyColibri*" % len(ttydevs))
            print("Make sure you have exactly one colibri connected and")
            print("udev rules installed")
            raise Exception("/dev/ttyColibri* is not unique.")
        return ttydevs[0]


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
        ttydev = self._colibri_devtty()
        self.picocom = subprocess.Popen(
                ["picocom", "-b", "115200", ttydev],
                close_fds=True, stdout=slavefd, stdin=slavefd)
        os.close(slavefd)
        self.console_out = os.fdopen(self.masterfd, 'rb', 0)
        return self.console_out


class ETHRackColibriMachine(ColibriMachine):
    _machines = eth_machinedata.colibriboards
    imagename = "armv8_imx8x_image.efi"

    def __init__(self, options, **kwargs):
        super(ETHRackColibriMachine, self).__init__(options, ETHRackColibriMachineOperations(self), **kwargs)
        self.menulst_template = "menu.lst.armv8_imx8x"

    def setup(self, builddir=None):
        pass

    def get_platform(self):
        return 'imx8x'

#class ETHRackColibriMachineOperations(ETHBaseMachineOperations):
class ETHRackColibriMachineOperations(ETHMachineOperations):
    def __init__(self, machine):
        super(ETHRackColibriMachineOperations, self).__init__(machine)
        self._tftp_dir = None
        self.targe_name = None
        self.name = machine.name

    def __get_colibri_num(self):
        return str(int(self.name[7:]))

    def __chmod_ar(self, file):
        '''make file/directory readable by all'''
        import stat
        extra = stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH
        if os.path.isdir(file):
            extra |= stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
        os.chmod(file, os.stat(file).st_mode | extra)

    def set_bootmodules(self, modules):
        menulst_fullpath = os.path.join(self._machine.options.builds[0].build_dir,
                "platforms", "arm", self._machine.menulst_template)
        self._machine._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        source_name = os.path.join(self._machine.options.builds[0].build_dir, self._machine.imagename)
        self.target_name = os.path.join(self.get_tftp_dir(), self._machine.imagename)
        debug.verbose("building proper imx8x image")
        debug.checkcmd(["make", self._machine.imagename],
                cwd=self._machine.options.builds[0].build_dir)
        debug.verbose("copying %s to %s" % (self._machine.imagename, self.target_name))
        shutil.copyfile(source_name, self.target_name)
        self.__chmod_ar(self.target_name)

    def __usbboot(self):
        debug.checkcmd([
            BFBOOT,
            "--bf", self.target_name,
            "--board", self.__get_colibri_num()])

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

for pb in ETHRackColibriMachine._machines:
    class TmpMachine(ETHRackColibriMachine):
        name = pb
    MachineFactory.addMachine(pb, TmpMachine, **ETHRackColibriMachine._machines[pb])

MachineFactory.addMachine("colibri_local", ColibriLocalMachine,
                          bootarch='armv8',
                          platform='imx8x',
                          ncores=4)
