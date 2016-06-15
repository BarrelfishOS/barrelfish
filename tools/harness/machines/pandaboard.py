##########################################################################
# Copyright (c) 2014, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import debug, machines, eth_machinedata
import subprocess, os, socket, sys, shutil, tempfile
from eth import ETHBaseMachine

PANDA_ROOT='/mnt/local/nfs/pandaboot'
PANDA_BOOT_HOST='masterpanda.in.barrelfish.org'
PANDA_PORT=10000
TOOLS_PATH='/home/netos/tools/bin'
RACKBOOT=os.path.join(TOOLS_PATH, 'rackboot.sh')
RACKPOWER=os.path.join(TOOLS_PATH, 'rackpower')

class ETHRackPandaboardMachine(ETHBaseMachine):
    _machines = eth_machinedata.pandaboards

    def __init__(self, options):
        super(ETHRackPandaboardMachine, self).__init__(options)
        self._tftp_dir = None

    # pandaboard specifics
    def get_platform(self):
        return 'omap44xx'

    def get_buildall_target(self):
        return "PandaboardES"

    def get_bootline(self):
        # XXX: this should really not be necessary, check what is messing up
        # terminal
        return "Dump of device omap44xx_id"

    def __chmod_ar(self, file):
        '''make file/directory readable by all'''
        import stat
        extra = stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH
        if os.path.isdir(file):
            extra |= stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
        os.chmod(file, os.stat(file).st_mode | extra)

    def get_tftp_dir(self):
        if self._tftp_dir is None:
            self._tftp_dir = tempfile.mkdtemp(dir=PANDA_ROOT, prefix="%s_" % self.name)
            self.__chmod_ar(self._tftp_dir)
        return self._tftp_dir

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        f = open(path, 'w')
        f.write(data)
        # TODO: provide mmap properly somehwere (machine data?)
        f.write("mmap map 0x80000000 0x40000000 1\n")
        f.close()

    def set_bootmodules(self, modules):
        menulst_fullpath = os.path.join(self.builddir,
                "platforms", "arm", "menu.lst.pandaboard")
        self._write_menu_lst(modules.get_menu_data("/"), menulst_fullpath)
        source_name = os.path.join(self.builddir, "pandaboard_image")
        self.target_name = os.path.join(self.get_tftp_dir(), "pandaboard_image")
        debug.verbose("building proper pandaboard image")
        debug.checkcmd(["make", "pandaboard_image"], cwd=self.builddir)
        debug.verbose("copying %s to %s" % (source_name, self.target_name))
        shutil.copyfile(source_name, self.target_name)
        self.__chmod_ar(self.target_name)

    def __usbboot(self):
        pandanum = self.get_machine_name()[5:]
        imagename = os.path.relpath(self.target_name, PANDA_ROOT)
        # send "boot PANDANUM pandaboot/$tempdir/pandaboard_image" to
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
        return filter(lambda l: l.startswith(self.get_machine_name()), output)[0]

    def setup(self, builddir=None):
        self.builddir = builddir

    def __rackpower(self, arg):
        try:
            debug.checkcmd([RACKPOWER, arg, self.get_machine_name()])
        except subprocess.CalledProcessError:
            debug.warning("rackpower %s %s failed" %
                          (arg, self.get_machine_name()))

    def reboot(self):
        self.__usbboot()

    def shutdown(self):
        self.__rackpower('-d')

for pb in ETHRackPandaboardMachine._machines:
    class TmpMachine(ETHRackPandaboardMachine):
        name = pb
    machines.add_machine(TmpMachine)
