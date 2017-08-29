##########################################################################
# Copyright (c) 2009-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, getpass, subprocess, socket, pty
import debug, eth_machinedata
from machines import Machine, MachineLockedError, MachineFactory,\
    MachineOperations

from subprocess_timeout import wait_or_terminate

TFTP_PATH='/home/netos/tftpboot'
TOOLS_PATH='/home/netos/tools/bin'
RACKBOOT=os.path.join(TOOLS_PATH, 'rackboot.sh')
RACKPOWER=os.path.join(TOOLS_PATH, 'rackpower')

class ETHBaseMachine(Machine):
    _machines = None

    def __init__(self, options,
                 operations,
                 serial_binary='serial_pc16550d',
                 **kwargs):

        super(ETHBaseMachine, self).__init__(options, operations,
                                             serial_binary=serial_binary,
                                             **kwargs)

    def get_perfcount_type(self):
        return self._perfcount_type

class ETHBaseMachineOperations(MachineOperations):

    def __init__(self, machine):
        super(ETHBaseMachineOperations, self).__init__(machine)
        self.lockprocess = None
        self.masterfd = None

    def _get_console_status(self):
        raise NotImplementedError

    def lock(self):
        """Use conserver to lock the machine."""

        # find out current status of console
        cstate = self._get_console_status()
        # check that nobody else has it open for writing
        myuser = getpass.getuser()
        parts = cstate.strip().split(':')
        conname, child, contype, details, users, state = parts[:6]
        if users:
            for userinfo in users.split(','):
                mode, username, host, port = userinfo.split('@')[:4]
                if 'w' in mode and username != myuser:
                    raise MachineLockedError # Machine is not free

        # run a console in the background to 'hold' the lock and read output
        debug.verbose('starting "console %s"' % self._machine.get_machine_name())
        # run on a PTY to work around terminal mangling code in console
        (self.masterfd, slavefd) = pty.openpty()
        self.lockprocess = subprocess.Popen(["console", self._machine.get_machine_name()],
                                            close_fds=True,
                                            stdout=slavefd, stdin=slavefd)
        os.close(slavefd)
        # XXX: open in binary mode with no buffering
        # otherwise select.select() may block when there is data in the buffer
        self.console_out = os.fdopen(self.masterfd, 'rb', 0)

    def unlock(self):
        if self.lockprocess is None:
            return # noop
        debug.verbose('quitting console process (%d)' % self.lockprocess.pid)
        # os.kill(self.lockprocess.pid, signal.SIGTERM)
        os.write(self.masterfd, "\x05c.")
        wait_or_terminate(self.lockprocess)
        self.lockprocess = None
        self.masterfd = None

    # this expects a pexpect object for `consolectrl`
    def force_write(self, consolectrl):
        try:
            consolectrl.send('\x05cf')
        except:
            print "Unable to force write control through consolectrl, trying masterfd"
            os.write(self.masterfd, "\x05cf")

    def get_output(self):
        return self.console_out


class ETHMachine(ETHBaseMachine):
    _machines = eth_machinedata.machines

    def __init__(self, options, **kwargs):
        super(ETHMachine, self).__init__(options, ETHMachineOperations(self), **kwargs)

    def get_buildall_target(self):
        if 'buildall_target' in self._machines[self.name]:
            return self._machines[self.name]['buildall_target']
        return self.get_bootarch().upper() + "_Full"

    def get_xphi_ncores(self):
        if 'xphi_ncores' in self._machines[self.name] :
            return self._machines[self.name]['xphi_ncores']
        else :
            return 0

    def get_xphi_ncards(self):
        if 'xphi_ncards' in self._machines[self.name] :
            return self._machines[self.name]['xphi_ncards']
        else :
            return 0

    def get_xphi_ram_gb(self):
        if 'xphi_ram_gb' in self._machines[self.name] :
            return self._machines[self.name]['xphi_ram_gb']
        else :
            return 0

    def get_xphi_tickrate(self):
        if 'xphi_tickrate' in self._machines[self.name] :
            return self._machines[self.name]['xphi_tickrate']
        else :
            return 0

    def get_hostname(self):
        return self.get_machine_name() + '.in.barrelfish.org'

    def get_ip(self):
        return socket.gethostbyname(self.get_hostname())

class ETHMachineOperations(ETHBaseMachineOperations):

    def __init__(self, machine):
        super(ETHMachineOperations, self).__init__(machine)

    def get_tftp_dir(self):
        user = getpass.getuser()
        return os.path.join(TFTP_PATH, user, self._machine.name + "_harness")

    def get_tftp_subdir(self):
        user = getpass.getuser()
        return os.path.join(user, self._machine.name + "_harness")

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        with open(path, 'w') as f:
            f.write(data)


    def _get_menu_lst_name(self):
        if self._machine.get_bootarch() == "armv8":
            return "hagfish.cfg"
        else:
            return "menu.lst"

    def _set_menu_lst(self, relpath):
        ip_menu_name = os.path.join(TFTP_PATH, self._get_menu_lst_name() + "." + self._machine.get_ip())
        debug.verbose('relinking %s to %s' % (ip_menu_name, relpath))
        os.remove(ip_menu_name)
        os.symlink(relpath, ip_menu_name)

    def set_bootmodules(self, modules):
        fullpath = os.path.join(self.get_tftp_dir(), self._get_menu_lst_name())
        relpath = os.path.relpath(fullpath, TFTP_PATH)
        tftppath = '/' + os.path.relpath(self.get_tftp_dir(), TFTP_PATH)
        self._write_menu_lst(modules.get_menu_data(tftppath), fullpath)
        self._set_menu_lst(relpath)

    def _get_console_status(self):
        debug.verbose('executing "console -i %s" to check state' %
                      self._machine.get_machine_name())
        proc = subprocess.Popen(["console", "-i", self._machine.get_machine_name()],
                                stdout=subprocess.PIPE)
        line = proc.communicate()[0]
        assert(proc.returncode == 0)
        return line

    def __rackboot(self, args):
        debug.checkcmd([RACKBOOT] + args + [self._machine.get_machine_name()])

    def setup(self):
        if self._machine.get_bootarch() == "armv8":
            self.__rackboot(["-b", "-H", "-n"])
        else:
            self.__rackboot(["-b", "-n"])

    def __rackpower(self, arg):
        try:
            debug.checkcmd([RACKPOWER, arg, self._machine.get_machine_name()])
        except subprocess.CalledProcessError:
            debug.warning("rackpower %s %s failed" %
                          (arg, self._machine.get_machine_name()))

    def reboot(self):
        self.__rackpower('-r')

    def shutdown(self):
        self.__rackpower('-d')


for n in sorted(ETHMachine._machines.keys()):
    class TmpMachine(ETHMachine):
        name = n
    MachineFactory.addMachine(n, TmpMachine, **ETHMachine._machines[n])
