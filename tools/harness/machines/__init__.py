##########################################################################
# Copyright (c) 2009-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, debug, signal, shutil, time

class Machine(object):
    name = None # should be overridden

    def __init__(self, options):
        pass

    def get_bootarch(self):
        """Return the architecture for booting and base system services."""
        return "x86_64" # old default!

    def get_buildarchs(self):
        """Return the architectures that must be enabled in hake for this machine."""
        return [self.get_bootarch()]

    def get_buildall_target(self):
        """Return a valid make target to build default set of modules
        (previously 'all')"""
        raise NotImplementedError

    def get_ncores(self):
        """Returns absolute number of cores."""
        raise NotImplementedError

    def get_coreids(self):
        """Returns the list of core IDs."""
        return range(0, self.get_ncores()) # default behaviour for x86

    # XXX: REMOVE ME. used only by lwip_loopback bench
    def get_cores_per_socket(self):
        """Returns number of cores per socket."""
        raise NotImplementedError

    def get_tickrate(self):
        """Returns clock rate in MHz."""
        raise NotImplementedError

    def get_perfcount_type(self):
        """Returns a string ('amd0f', 'amd10', or 'intel'), or None if unknown"""
        return None

    def get_kernel_args(self):
        """Returns list of machine-specific arguments to add to the kernel command-line"""
        return []

    def get_pci_args(self):
        """Returns list of machine-specific arguments to add to the PCI command-line"""
        return []

    def get_eth0(self):
        """Returns machine-specific bus:dev:fun for connected network interface of machine"""
        # 0xff for all three elements should preserve kaluga default behaviour
        return (0xff, 0xff, 0xff)

    def get_serial_binary(self):
        """Returns a machine-specific binary name for the serial driver
        (fallback if not implemented is the kernel serial driver)"""
        return "serial_kernel"

    def get_boot_timeout(self):
        """Returns a machine-specific timeout (in seconds), or None for the default"""
        return None

    def get_test_timeout(self):
        """Returns a machine-specific timeout (in seconds), or None for the default"""
        return None

    def get_tftp_dir(self):
        """Return a unique TFTP boot directory for this machine."""
        raise NotImplementedError

    def set_bootmodules(self, modules):
        """Set the machine to boot from the given module data."""
        raise NotImplementedError

    def lock(self):
        """Lock the machine to prevent concurrent access."""
        raise NotImplementedError

    def unlock(self):
        """Unlock an already-locked machine."""
        raise NotImplementedError

    def setup(self):
        """Prepare a locked machine to be booted."""
        raise NotImplementedError

    def reboot(self):
        """Reboot (or boot) the machine."""
        raise NotImplementedError

    def shutdown(self):
        """Shut down/switch off the machine."""
        raise NotImplementedError

    def get_output(self):
        """Returns a file object to the output of a locked machine."""
        raise NotImplementedError

class MachineLockedError(Exception):
    """May be raised by lock() when the machine is locked by another user."""
    pass

class ARMMachineBase(Machine):
    def __init__(self, options):
        super(ARMMachineBase, self).__init__(options)
        self.options = options
        self.menulst = None
        self.mmap = None
        self.kernel_args = None
        self.menulst_template = "menu.lst." + self.get_bootarch() + "_" + \
                                self.get_platform() + ("_%d" % self.get_ncores())
        self._set_kernel_image()

    def _get_template_menu_lst(self):
        """Read menu lst in source tree"""
        if self.menulst is None:
            template_menulst = os.path.join(self.options.sourcedir, "hake",
                    self.menulst_template)
            with open(template_menulst) as f:
                self.menulst = f.readlines()

        return self.menulst

    def _set_kernel_image(self):
        if self.options.existingbuild:
            self.kernel_img = os.path.join(self.options.existingbuild, self.imagename)
        else:
            self.kernel_img = os.path.join(self.options.buildbase,
                                self.options.builds[0].name,
                                self.imagename)

    def get_kernel_args(self):
        if self.kernel_args is None:
            for line in self._get_template_menu_lst():
                if line.startswith("kernel"):
                    _, _, args = line.strip().split(" ", 2)
                    self.kernel_args = args.split(" ")
        return self.kernel_args

    def _get_mmap(self):
        """Grab MMAP data from menu lst in source tree"""
        if self.mmap is None:
            self.mmap = []
            for line in self._get_template_menu_lst():
                if line.startswith("mmap"):
                    self.mmap.append(line)

        debug.debug("got MMAP:\n  %s" % "  ".join(self.mmap))
        return self.mmap

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        f = open(path, 'w')
        f.write(data)
        for line in self._get_mmap():
            f.write(line)
        f.close()

class ARMSimulatorBase(ARMMachineBase):
    def __init__(self, options):
        super(ARMSimulatorBase, self).__init__(options)
        self.child = None
        self.telnet = None
        self.tftp_dir = None
        self.simulator_start_timeout = 5 # seconds

    def setup(self):
        pass

    def get_coreids(self):
        return range(0, self.get_ncores())

    def get_tickrate(self):
        return None

    def get_boot_timeout(self):
        """Default boot timeout for ARM simulators: 2min"""
        return 120

    def get_test_timeout(self):
        """Default test timeout for ARM simulators: 10min"""
        return 10 * 60

    def get_machine_name(self):
        return self.name

    def get_bootarch(self):
        raise NotImplementedError

    def get_platform(self):
        raise NotImplementedError

    def force_write(self, consolectrl):
        pass

    def lock(self):
        pass

    def unlock(self):
        pass

    def get_free_port(self):
        import socket
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind(('', 0))
        # extract port from addrinfo
        self.telnet_port = s.getsockname()[1]
        s.close()

    def _get_cmdline(self):
        raise NotImplementedError

    def _kill_child(self):
        # terminate child if running
        if self.child:
            try:
                os.kill(self.child.pid, signal.SIGTERM)
            except OSError, e:
                debug.verbose("Caught OSError trying to kill child: %r" % e)
            except Exception, e:
                debug.verbose("Caught exception trying to kill child: %r" % e)
            try:
                self.child.wait()
            except Exception, e:
                debug.verbose(
                    "Caught exception while waiting for child: %r" % e)
            self.child = None

    def shutdown(self):
        debug.verbose('Simulator:shutdown requested');
        debug.verbose('terminating simulator')
        if not self.child is None:
            try:
                self.child.terminate()
            except OSError, e:
                debug.verbose("Error when trying to terminate simulator: %r" % e)
        debug.verbose('closing telnet connection')
        if not self.telnet is None:
            self.output.close()
            self.telnet.close()
        # try to cleanup tftp tree if needed
        if self.tftp_dir and os.path.isdir(self.tftp_dir):
            shutil.rmtree(self.tftp_dir, ignore_errors=True)
        self.tftp_dir = None

    def get_output(self):
        # wait a bit to give the simulator time to listen for a telnet connection
        if self.child.poll() != None: # Check if child is down
            print 'Simulator is down, return code is %d' % self.child.returncode
            return None
        # use telnetlib
        import telnetlib
        self.telnet_connected = False
        while not self.telnet_connected:
            try:
                self.telnet = telnetlib.Telnet("localhost", self.telnet_port)
                self.telnet_connected = True
                self.output = self.telnet.get_socket().makefile()
            except IOError, e:
                errno, msg = e
                if errno != 111: # connection refused
                    debug.error("telnet: %s [%d]" % (msg, errno))
                else:
                    self.telnet_connected = False
            time.sleep(self.simulator_start_timeout)

        return self.output


all_machines = []

def add_machine(machine):
    all_machines.append(machine)
    return machine

# Assume that QEMU, FVP, pandaboard and Gem5 work everywhere if invoked
import qemu
import gem5
import fvp
import pandaboard

# Other site-specific modules will be loaded by the siteconfig module
