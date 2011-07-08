##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, signal, tempfile, subprocess, shutil
import debug, machines
from machines import Machine

SLAVECODE_PATH = "kernel/arch/beehive/slavecode.mem" # relative to source tree
BSIM_CMD = 'Bsimimg'
BSIM_ARGS= '-noaddrcheck -ibase=1000 -datarota=2'.split()
BSIM_IMG = 'spliced.img' # in "tftp" dir
BSPLICE_CMD = 'Bsplice'
BSIM_NCORES = 2

@machines.add_machine
class BSim(Machine):
    '''Beehive simulator'''
    name = 'bsim'

    def __init__(self, options):
        super(BSim, self).__init__(options)
        self.child = None
        self.tftp_dir = None
        self.options = options

    def get_bootarch(self):
        return "beehive"

    def get_ncores(self):
        return BSIM_NCORES

    def get_coreids(self):
        return range(2, 2 + self.get_ncores())

    def get_tickrate(self):
        return None

    def get_boot_timeout(self):
        # time limit for running a sim test
        # FIXME: ideally this should somehow be expressed in CPU time / cycles
        return 6 * 60

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            debug.verbose('creating temporary directory for Bsim TFTP files')
            self.tftp_dir = tempfile.mkdtemp(prefix='harness_sim_')
            debug.verbose('Bsim TFTP directory is %s' % self.tftp_dir)
        return self.tftp_dir

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        f = open(path, 'w')
        f.write(data)
        f.close()

    def _run_bsplice(self):
        tftp_dir = self.get_tftp_dir()
        outpath = os.path.join(tftp_dir, BSIM_IMG)
        inpath = os.path.join(tftp_dir, 'menu.lst')
        debug.checkcmd([BSPLICE_CMD, '-o', outpath, '-i', inpath])

    def set_bootmodules(self, modules):
        path = os.path.join(self.get_tftp_dir(), 'menu.lst')
        # XXX: Bsplice unsurprisingly doesn't like 'modulenounzip'
        menudata = modules.get_menu_data('/').replace("modulenounzip", "module")
        self._write_menu_lst(menudata, path)
        self._run_bsplice()

    def lock(self):
        pass

    def unlock(self):
        pass

    def setup(self):
        pass

    def _get_cmdline(self):
        tftp_dir = self.get_tftp_dir()
        return ([BSIM_CMD,
                 "-slave=" + os.path.join(self.options.sourcedir, SLAVECODE_PATH),
                 "-ncores=%d" % (self.get_ncores() + 1)] # +1 for hypervisor
                + BSIM_ARGS + [os.path.join(tftp_dir, BSIM_IMG)])

    def _kill_child(self):
        # terminate child if running
        if self.child:
            os.kill(self.child.pid, signal.SIGTERM)
            self.child.wait()
            self.child = None

    def reboot(self):
        self._kill_child()
        cmd = self._get_cmdline()
        debug.verbose('starting "%s"' % ' '.join(cmd))
        devnull = open(os.devnull, 'r')
        self.child = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=devnull)
        devnull.close()

    def shutdown(self):
        self._kill_child()
        # try to cleanup tftp tree if needed
        if self.tftp_dir and os.path.isdir(self.tftp_dir):
            shutil.rmtree(self.tftp_dir, ignore_errors=True)
        self.tftp_dir = None

    def get_output(self):
        return self.child.stdout
