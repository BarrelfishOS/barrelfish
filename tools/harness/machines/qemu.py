##########################################################################
# Copyright (c) 2009, 2010, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, signal, tempfile, subprocess, shutil
import debug, machines
from machines import Machine

QEMU_SCRIPT_PATH = 'tools/qemu-wrapper.sh' # relative to source tree
GRUB_IMAGE_PATH = 'tools/grub-qemu.img' # relative to source tree
QEMU_CMD_X64 = 'qemu-system-x86_64'
QEMU_CMD_X32 = 'qemu-system-i386'
QEMU_CMD_ARM = 'qemu-system-arm'
QEMU_ARGS_GENERIC = '-nographic -no-reboot'.split()
QEMU_ARGS_X64 = '-net nic,model=ne2k_pci -net user -m 3084'.split()
QEMU_ARGS_X32 = '-net nic,model=ne2k_pci -net user -m 512'.split()

class QEMUMachineBase(Machine):
    def __init__(self, options):
        super(QEMUMachineBase, self).__init__(options)
        self.child = None
        self.tftp_dir = None
        self.options = options

    def get_coreids(self):
        return range(0, self.get_ncores())

    def get_tickrate(self):
        return None

    def get_buildall_target(self):
        return self.get_bootarch().upper() + "_Full"

    def get_boot_timeout(self):
        # shorter time limit for running a qemu test
        # FIXME: ideally this should somehow be expressed in CPU time / cycles
        return 60

    def get_machine_name(self):
        return self.name

    def force_write(self, consolectrl):
        pass

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            debug.verbose('creating temporary directory for QEMU TFTP files')
            self.tftp_dir = tempfile.mkdtemp(prefix='harness_qemu_')
            debug.verbose('QEMU TFTP directory is %s' % self.tftp_dir)
        return self.tftp_dir

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        f = open(path, 'w')
        f.write(data)
        f.close()

    def set_bootmodules(self, modules):
        path = os.path.join(self.get_tftp_dir(), 'menu.lst')
        self._write_menu_lst(modules.get_menu_data('/'), path)

    def lock(self):
        pass

    def unlock(self):
        pass

    def setup(self, builddir=None):
        self.builddir = builddir

    def _get_cmdline(self):
        raise NotImplementedError

    def _kill_child(self):
        # terminate child if running
        if self.child:
            os.kill(self.child.pid, signal.SIGTERM)
            self.child.wait()
            self.child = None
        self.masterfd = None

    def reboot(self):
        self._kill_child()
        cmd = self._get_cmdline()
        debug.verbose('starting "%s"' % ' '.join(cmd))
        import pty
        (self.masterfd, slavefd) = pty.openpty()
        self.child = subprocess.Popen(cmd, close_fds=True,
                                      stdout=slavefd,
                                      stdin=slavefd)
        os.close(slavefd)
        # open in binary mode w/o buffering
        self.qemu_out = os.fdopen(self.masterfd, 'rb', 0)

    def shutdown(self):
        self._kill_child()
        # try to cleanup tftp tree if needed
        if self.tftp_dir and os.path.isdir(self.tftp_dir):
            shutil.rmtree(self.tftp_dir, ignore_errors=True)
        self.tftp_dir = None

    def get_output(self):
        return self.qemu_out

class QEMUMachineX64(QEMUMachineBase):
    def _get_cmdline(self):
        qemu_wrapper = os.path.join(self.options.sourcedir, QEMU_SCRIPT_PATH)
        menu_lst = os.path.join(self.get_tftp_dir(), 'menu.lst')
        return [ qemu_wrapper, "--menu", menu_lst, "--arch", "x86_64",
                "--smp", "%s" % self.get_ncores() ]

    def set_bootmodules(self, modules):
        path = os.path.join(self.get_tftp_dir(), 'menu.lst')
        self._write_menu_lst(modules.get_menu_data('/', self.get_tftp_dir()), path)

    def get_bootarch(self):
        return "x86_64"

class QEMUMachineX32(QEMUMachineBase):
    def _get_cmdline(self):
        grub_image = os.path.join(self.options.sourcedir, GRUB_IMAGE_PATH)
        s = '-smp %d -fda %s -tftp %s' % (self.get_ncores(), grub_image,
                                          self.get_tftp_dir())
        return [QEMU_CMD_X32] + QEMU_ARGS_GENERIC + QEMU_ARGS_X32 + s.split()

    def get_bootarch(self):
        return "x86_32"

@machines.add_machine
class QEMUMachineUniproc(QEMUMachineX64):
    '''Uniprocessor x86_64 QEMU'''
    name = 'qemu1'

    def get_ncores(self):
        return 1

@machines.add_machine
class QEMUMachineMultiproc(QEMUMachineX64):
    '''Multiprocessor x86_64 QEMU (4 CPUs)'''
    name = 'qemu4'

    def get_boot_timeout(self):
        # 4core qemu needs a bit longer to boot
        return 240

    def get_test_timeout(self):
        # give gem5 tests enough time to complete
        # 20 mins
        return 10 * 60

    def get_ncores(self):
        return 4

@machines.add_machine
class QEMUMachineX32Uniproc(QEMUMachineX32):
    '''Uniprocessor x86_32 QEMU'''
    name = 'qemu1-32'

    def get_ncores(self):
        return 1

@machines.add_machine
class QEMUMachineX32Multiproc(QEMUMachineX32):
    '''Multiprocessor x86_32 QEMU (4 CPUs)'''
    name = 'qemu4-32'

    def get_ncores(self):
        return 4

@machines.add_machine
class QEMUMachineARMv7Uniproc(QEMUMachineBase):
    '''Uniprocessor ARMv7 QEMU'''
    name = 'qemu_armv7'

    imagename = "armv7_a15ve_image"

    def get_ncores(self):
        return 1

    def get_bootarch(self):
        return "armv7"

    def get_platform(self):
        return 'a15ve'

    def set_bootmodules(self, modules):
        # store path to kernel for _get_cmdline to use
        self.kernel_img = os.path.join(self.options.buildbase,
                                       self.options.builds[0].name,
                                       self.imagename)
        # write menu.lst
        debug.verbose("Writing menu.lst in build directory.")
        menulst_fullpath = os.path.join(self.builddir,
                "platforms", "arm", "menu.lst.armv7_a15ve")
        self._write_menu_lst(modules.get_menu_data('/'), menulst_fullpath)
        with open(menulst_fullpath, 'a') as m:
            m.write("mmap map 0x80000000  0x20000000 1")

        # produce ROM image
        debug.verbose("Building QEMU image.")
        debug.checkcmd(["make", self.imagename], cwd=self.builddir)

    def _get_cmdline(self):
        qemu_wrapper = os.path.join(self.options.sourcedir, QEMU_SCRIPT_PATH)

        return ([qemu_wrapper, '--arch', 'a15ve', '--image', self.kernel_img])
