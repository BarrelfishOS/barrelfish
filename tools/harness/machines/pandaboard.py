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

# XXX: the rack-mount pandaboard machines are ETH-specific
PANDA_ROOT='/mnt/emmentaler1_nfs/pandaboot'
PANDA_BOOT_HOST='masterpanda.in.barrelfish.org'
PANDA_PORT=10000
TOOLS_PATH='/home/netos/tools/bin'
RACKBOOT=os.path.join(TOOLS_PATH, 'rackboot.sh')
RACKPOWER=os.path.join(TOOLS_PATH, 'rackpower')


class PandaboardMachine(ARMMachineBase):
    '''Machine to run tests on locally attached pandaboard. Assumes your
    pandaboard's serial port is attached to /dev/ttyUSB0'''
    name = 'panda_local'
    imagename = "armv7_omap44xx_image"

    def __init__(self, options, **kwargs):
        super(PandaboardMachine, self).__init__(options, PandaboardOperations(self), **kwargs)
        self.menulst_template = "menu.lst.armv7_omap44xx"

    def setup(self, builddir=None):
        pass

    def get_buildall_target(self):
        return "PandaboardES"

class PandaboardOperations(MachineOperations):

    def __init__(self, machine):
        super(PandaboardOperations, self).__init__(machine)
        self.picocom = None
        self.tftp_dir = None
        self.masterfd = None

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            self.tftp_dir = tempfile.mkdtemp(prefix="panda_")
        return self.tftp_dir

    def set_bootmodules(self, modules):
        menulst_fullpath = os.path.join(self._machine.options.builds[0].build_dir,
                "platforms", "arm", self._machine.menulst_template)
        self._machine._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        debug.verbose("building proper pandaboard image")
        debug.checkcmd(["make", self._machine.imagename],
                cwd=self._machine.options.builds[0].build_dir)

    def __usbboot(self):
        debug.verbose("Usbbooting pandaboard; press reset")
        debug.verbose("build dir: %s" % self._machine.options.builds[0].build_dir)
        debug.checkcmd(["make", "usbboot_panda"],
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
            try:
                os.unlink("/var/lock/LCK..ttyUSB0")
            except OSError:
                pass
        self.picocom = None
        self.masterfd = None

    def get_output(self):
        '''Use picocom to get output. This replicates part of
        ETHMachine.lock()'''
        (self.masterfd, slavefd) = pty.openpty()
        self.picocom = subprocess.Popen(
                ["picocom", "-b", "115200", "/dev/ttyUSB0"],
                close_fds=True, stdout=slavefd, stdin=slavefd)
        os.close(slavefd)
        self.console_out = os.fdopen(self.masterfd, 'rb', 0)
        return self.console_out


class ETHRackPandaboardMachine(ARMMachineBase):
    _machines = eth_machinedata.pandaboards
    imagename = "armv7_omap44xx_image"

    def __init__(self, options, **kwargs):
        super(ETHRackPandaboardMachine, self).__init__(options, ETHRackPandaboardMachineOperations(self), **kwargs)
        self.menulst_template = "menu.lst.armv7_omap44xx"

    def setup(self, builddir=None):
        pass

    # pandaboard specifics
    def get_platform(self):
        return 'omap44xx'

    def get_buildall_target(self):
        return "PandaboardES"

class ETHRackPandaboardMachineOperations(ETHBaseMachineOperations):

    def __init__(self, machine):
        super(ETHRackPandaboardMachineOperations, self).__init__(machine)
        self._tftp_dir = None
        self.targe_name = None

    def __chmod_ar(self, file):
        '''make file/directory readable by all'''
        import stat
        extra = stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH
        if os.path.isdir(file):
            extra |= stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
        os.chmod(file, os.stat(file).st_mode | extra)

    def get_tftp_dir(self):
        if self._tftp_dir is None:
            self._tftp_dir = tempfile.mkdtemp(dir=PANDA_ROOT, prefix="%s_" % self._machine.getName())
            self.__chmod_ar(self._tftp_dir)
        return self._tftp_dir

    def set_bootmodules(self, modules):
        menulst_fullpath = os.path.join(self._machine.options.builds[0].build_dir,
                "platforms", "arm", self._machine.menulst_template)
        self._machine._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        source_name = os.path.join(self._machine.options.builds[0].build_dir, self._machine.imagename)
        self.target_name = os.path.join(self.get_tftp_dir(), self._machine.imagename)
        debug.verbose("building proper pandaboard image")
        debug.checkcmd(["make", self._machine.imagename],
                cwd=self._machine.options.builds[0].build_dir)
        debug.verbose("copying %s to %s" % (source_name, self.target_name))
        shutil.copyfile(source_name, self.target_name)
        self.__chmod_ar(self.target_name)

    def __usbboot(self):
        pandanum = self._machine.get_machine_name()[5:]
        imagename = os.path.relpath(self.target_name, PANDA_ROOT)
        # send "boot PANDANUM pandaboot/$tempdir/IMAGE_NAME" to
        # masterpanda:10000
        debug.verbose("sending boot command for pandaboard %s; pandaboard_image %s" % (pandanum, imagename))
        masterpanda_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        masterpanda_sock.connect((PANDA_BOOT_HOST, PANDA_PORT))
        masterpanda_sock.send('boot %s pandaboot/%s\n' %
                (pandanum, imagename))
        masterpanda_sock.shutdown(socket.SHUT_WR)
        while True:
            data = masterpanda_sock.recv(1024)
            os.write(sys.stdout.fileno(), data)
            if data == "":
                break

    def _get_console_status(self):
        # for Pandaboards we cannot do console -i <machine> so we grab full -i
        # output and find relevant line here
        proc = subprocess.Popen(["console", "-i"], stdout=subprocess.PIPE)
        output = proc.communicate()[0]
        assert(proc.returncode == 0)
        output = map(str.strip, output.split("\n"))
        return filter(lambda l: l.startswith(self._machine.get_machine_name()), output)[0]

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

for pb in ETHRackPandaboardMachine._machines:
    class TmpMachine(ETHRackPandaboardMachine):
        name = pb
    MachineFactory.addMachine(pb, TmpMachine, **ETHRackPandaboardMachine._machines[pb])

MachineFactory.addMachine("panda_local", PandaboardMachine,
                          bootarch='armv7',
                          platform='omap44xx',
                          ncores=2)
