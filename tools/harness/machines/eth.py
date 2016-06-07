##########################################################################
# Copyright (c) 2009-2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import sys, os, signal, time, getpass, subprocess, socket, pty
import debug, machines, eth_machinedata
from machines import Machine, MachineLockedError

TFTP_PATH='/home/netos/tftpboot'
TOOLS_PATH='/home/netos/tools/bin'
RACKBOOT=os.path.join(TOOLS_PATH, 'rackboot.sh')
RACKPOWER=os.path.join(TOOLS_PATH, 'rackpower')

class ETHMachine(Machine):
    _eth_machines = eth_machinedata.machines

    def __init__(self, options):
        super(ETHMachine, self).__init__(options)
        self.lockprocess = None
        self.masterfd = None

    def get_bootarch(self):
        b = self._eth_machines[self.name]['bootarch']
        assert(b in self.get_buildarchs())
        return b

    def get_buildall_target(self):
        return self.get_bootarch().upper() + "_Full"

    def get_machine_name(self):
        return self._eth_machines[self.name]['machine_name']

    def get_buildarchs(self):
        return self._eth_machines[self.name]['buildarchs']

    def get_ncores(self):
        return self._eth_machines[self.name]['ncores']

    def get_xphi_ncores(self):
        if 'xphi_ncores' in self._eth_machines[self.name] :
            return self._eth_machines[self.name]['xphi_ncores']
        else :
            return 0

    def get_xphi_ncards(self):
        if 'xphi_ncards' in self._eth_machines[self.name] :
            return self._eth_machines[self.name]['xphi_ncards']
        else :
            return 0

    def get_xphi_ram_gb(self):
        if 'xphi_ram_gb' in self._eth_machines[self.name] :
            return self._eth_machines[self.name]['xphi_ram_gb']
        else :
            return 0

    def get_cores_per_socket(self):
        return self._eth_machines[self.name]['cores_per_socket']

    def get_tickrate(self):
        return self._eth_machines[self.name]['tickrate']

    def get_xphi_tickrate(self):
        if 'xphi_tickrate' in self._eth_machines[self.name] :
            return self._eth_machines[self.name]['xphi_tickrate']
        else :
            return 0

    def get_perfcount_type(self):
        return self._eth_machines[self.name]['perfcount_type']

    def get_kernel_args(self):
        return self._eth_machines[self.name].get('kernel_args')

    def get_boot_timeout(self):
        return self._eth_machines[self.name].get('boot_timeout')

    def get_hostname(self):
        return self.get_machine_name() + '.in.barrelfish.org'

    def get_ip(self):
        return socket.gethostbyname(self.get_hostname())

    def get_tftp_dir(self):
        user = getpass.getuser()
        return os.path.join(TFTP_PATH, user, self.name + "_harness")

    def get_tftp_subdir(self):
        user = getpass.getuser()
        return os.path.join(user, self.name + "_harness")            

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        f = open(path, 'w')
        f.write(data)
        f.close()

    def _set_menu_lst(self, relpath):
        ip_menu_name = os.path.join(TFTP_PATH, "menu.lst." + self.get_ip())
        debug.verbose('relinking %s to %s' % (ip_menu_name, relpath))
        os.remove(ip_menu_name)
        os.symlink(relpath, ip_menu_name)

    def set_bootmodules(self, modules):
        fullpath = os.path.join(self.get_tftp_dir(), 'menu.lst')
        relpath = os.path.relpath(fullpath, TFTP_PATH)
        tftppath = '/' + os.path.relpath(self.get_tftp_dir(), TFTP_PATH)
        self._write_menu_lst(modules.get_menu_data(tftppath), fullpath)
        self._set_menu_lst(relpath)

    def lock(self):
        """Use conserver to lock the machine."""

        # find out current status of console
        debug.verbose('executing "console -i %s" to check state' %
                      self.get_machine_name())
        proc = subprocess.Popen(["console", "-i", self.get_machine_name()],
                                stdout=subprocess.PIPE)
        line = proc.communicate()[0]
        assert(proc.returncode == 0)

        # check that nobody else has it open for writing
        myuser = getpass.getuser()
        parts = line.strip().split(':')
        conname, child, contype, details, users, state = parts[:6]
        if users:
            for userinfo in users.split(','):
                mode, username, host, port = userinfo.split('@')[:4]
                if 'w' in mode and username != myuser:
                    raise MachineLockedError # Machine is not free

        # run a console in the background to 'hold' the lock and read output
        debug.verbose('starting "console %s"' % self.get_machine_name())
        # run on a PTY to work around terminal mangling code in console
        (self.masterfd, slavefd) = pty.openpty()
        self.lockprocess = subprocess.Popen(["console", self.get_machine_name()],
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
        self.lockprocess.wait()
        self.lockprocess = None
        self.masterfd = None

    # this expects a pexpect object for `consolectrl`
    def force_write(self, consolectrl):
        try:
            consolectrl.sendcontrol('e')
            consolectrl.send('cf')
        except:
            pass

    def __rackboot(self, args):
        debug.checkcmd([RACKBOOT] + args + [self.get_machine_name()])

    def setup(self):
        self.__rackboot(["-b", "-n"])

    def __rackpower(self, arg):
        try:
            debug.checkcmd([RACKPOWER, arg, self.get_machine_name()])
        except subprocess.CalledProcessError:
            debug.warning("rackpower %s %s failed" %
                          (arg, self.get_machine_name()))

    def reboot(self):
        self.__rackpower('-r')

    def shutdown(self):
        self.__rackpower('-d')

    def get_output(self):
        return self.console_out


for n in sorted(ETHMachine._eth_machines.keys()):
    class TmpMachine(ETHMachine):
        name = n
    machines.add_machine(TmpMachine)
