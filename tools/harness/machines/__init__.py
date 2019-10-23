##########################################################################
# Copyright (c) 2009-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, debug, signal, shutil, time
import barrelfish

class Machine(object):

    @classmethod
    def validateArgs(cls, kwargs):
        try:
            kwargs['bootarch']
        except KeyError as e:
            raise TypeError("Missing key %s" % e.args[0])

    def __init__(self, options, operations,
                 bootarch=None,
                 machine_name=None,
                 boot_timeout=360,
                 platform=None,
                 buildarchs=None,
                 ncores=1,
                 cores_per_socket=None,
                 kernel_args=[],
                 serial_binary="serial_kernel",
                 pci_args=[],
                 acpi_args=[],
                 eth0=(0xff, 0xff, 0xff),
                 perfcount_type=None,
                 boot_driver = None,
                 tickrate = 0,
                 uboot = False,
                 **kwargs):

        self._name = "(unknown)"
        self.options = options
        self._operations = operations

        self._bootarch = bootarch

        self._machine_name = machine_name

        if buildarchs is None:
            buildarchs = [bootarch]
        self._build_archs = buildarchs
        assert(bootarch in buildarchs)

        self._ncores = ncores

        if cores_per_socket is None:
            cores_per_socket = ncores
        self._cores_per_socket = cores_per_socket

        self._kernel_args = kernel_args

        self._boot_driver = boot_driver

        self._serial_binary = serial_binary

        self._boot_timeout = boot_timeout

        self._platform = platform

        self._pci_args = pci_args

        self._acpi_args = acpi_args

        self._eth0 = eth0

        self._perfcount_type = perfcount_type

        self._tick_rate = tickrate
        self._uboot = uboot

        if bool(kwargs):
            debug.warning("Machine base class does not understand the " +
                    "following machine arguments: %s" % str(kwargs))

    def get_machine_name(self):
        return self._machine_name

    def get_bootarch(self):
        """Return the architecture for booting and base system services."""
        return self._bootarch

    def get_buildarchs(self):
        """Return the architectures that must be enabled in hake for this machine."""
        return self._build_archs

    def get_buildall_target(self):
        """Return a valid make target to build default set of modules
        (previously 'all')"""
        raise NotImplementedError

    def get_ncores(self):
        """Returns absolute number of cores."""
        return self._ncores

    def get_coreids(self):
        """Returns the list of core IDs."""
        return range(0, self.get_ncores()) # default behaviour for x86

    # XXX: REMOVE ME. used only by lwip_loopback bench
    def get_cores_per_socket(self):
        """Returns number of cores per socket."""
        return self._cores_per_socket

    def get_tickrate(self):
        """Returns clock rate in MHz."""
        raise NotImplementedError

    def get_perfcount_type(self):
        """Returns a string ('amd0f', 'amd10', or 'intel'), or None if unknown"""
        return self._perfcount_type

    def get_kernel_args(self):
        """Returns list of machine-specific arguments to add to the kernel command-line"""
        return self._kernel_args

    def get_boot_driver(self):
        """Returns list of machine-specific arguments to add to the kernel command-line"""
        return self._boot_driver

    def get_pci_args(self):
        """Returns list of machine-specific arguments to add to the PCI command-line"""
        return self._pci_args

    def get_acpi_args(self):
        """Returns list of machine-specific arguments to add to the ACPI command-line"""
        return self._acpi_args

    def get_platform(self):
        """Returns machine-specific platform specifier"""
        return self._platform

    def get_eth0(self):
        """Returns machine-specific bus:dev:fun for connected network interface of machine"""
        # 0xff for all three elements should preserve kaluga default behaviour
        return self._eth0

    def get_serial_binary(self):
        """Returns a machine-specific binary name for the serial driver
        (fallback if not implemented is the kernel serial driver)"""
        return self._serial_binary

    def get_boot_timeout(self):
        """Returns a machine-specific timeout (in seconds), or None for the default"""
        return self._boot_timeout

    def get_test_timeout(self):
        """Returns a machine-specific timeout (in seconds), or None for the default"""
        return None

    def get_tftp_dir(self):
        """Return a unique TFTP boot directory for this machine."""
        print("DEPRECATED get_tftp_dir")
        return self._operations.get_tftp_dir()

    def set_bootmodules(self, modules):
        """Set the machine to boot from the given module data."""
        print("DEPRECATED set_bootmodules")
        return self._operations.set_bootmodules(modules)

    def lock(self):
        """Lock the machine to prevent concurrent access."""
        print("DEPRECATED lock")
        return self._operations.lock()

    def unlock(self):
        """Unlock an already-locked machine."""
        print("DEPRECATED unlock")
        return self._operations.unlock()


    def setup(self):
        """Prepare a locked machine to be booted."""
        print("DEPRECATED setup")
        return self._operations.setup()

    def reboot(self):
        """Reboot (or boot) the machine."""
        print("DEPRECATED reboot")
        return self._operations.reboot()

    def shutdown(self):
        """Shut down/switch off the machine."""
        print("DEPRECATED shutdown")
        return self._operations.shutdown()

    def get_output(self):
        """Returns a file object to the output of a locked machine."""
        print("DEPRECATED get_output")
        return self._operations.get_output()

    def force_write(self, consolectrl):
        print("DEPRECATED force_write")
        return self._operations.force_write(consolectrl)

    def getName(self):
        return self._name

    def setName(self, name):
        self._name = name

    def default_bootmodules(self):
        """Returns the default boot module configuration for the given machine."""
        # FIXME: clean up / separate platform-specific logic

        machine = self
        a = machine.get_bootarch()

        # set the kernel: elver on x86_64
        if a == "x86_64":
            kernel = "elver"
        elif a == "armv7" or a == "armv8":
            kernel = "cpu_%s" % machine.get_platform()
        else:
            kernel = "cpu"

        m = barrelfish.BootModules(machine, prefix=("%s/sbin/" % a), kernel=kernel)
        m.add_kernel_args(machine.get_kernel_args())
        # default for all barrelfish archs
        # hack: cpu driver is not called "cpu" for ARMv7 builds
        if a == "armv7" :
            m.add_module("cpu_%s" % machine.get_platform(), machine.get_kernel_args())
        elif a == "armv8" :
            # remove kernel
            m.set_kernel(None)
            # add cpu driver
            m.set_cpu_driver(kernel, machine.get_kernel_args())
            # add boot driver
            m.set_boot_driver(machine.get_boot_driver())
        else :
            m.add_module("cpu", machine.get_kernel_args())

        m.add_module("init")
        m.add_module("mem_serv")
        m.add_module("monitor")
        m.add_module("ramfsd", ["boot"])
        m.add_module("skb", ["boot"])
        m.add_module("proc_mgmt", ["boot"])
        m.add_module("spawnd", ["boot"])
        m.add_module("startd", ["boot"])
        m.add_module("/eclipseclp_ramfs.cpio.gz", ["nospawn"])
        m.add_module("/skb_ramfs.cpio.gz", ["nospawn"])
        m.add_module("corectrl", ["auto"])

        # armv8
        if a == "armv8" :
            if not machine._uboot: # no ACPI on U-Boot
                m.add_module("acpi", ["boot"])
            m.add_module("kaluga", ["boot"])

        # SKB and PCI are x86-only for the moment
        if a == "x86_64" or a == "x86_32":
            # Add acpi with machine-specific extra-arguments
            m.add_module("acpi", ["boot"] + machine.get_acpi_args())
            m.add_module("routing_setup", ["boot"])

            # Add pci with machine-specific extra-arguments
            m.add_module("pci", ["auto"] + machine.get_pci_args())

            # Add kaluga with machine-specific bus:dev:fun triplet for eth0
            # interface
            m.add_module("kaluga",
                    ["boot", "eth0=%d:%d:%d" % machine.get_eth0()])

        # coreboot should work on armv7 now
        if a == "armv7":
            m.add_module("kaluga", machine.get_kaluga_args())
            m.add_module("driverdomain_pl390", ["auto"])
            m.add_module("serial_kernel", ["auto"])
            m.add_module("serial_pl011",  ["auto"])
            m.add_module("int_route", [])

        return m

class MachineOperations(object):

    def __init__(self, machine):
        self._machine = machine

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

    def force_write(self, consolectrl):
        raise NotImplementedError

class MachineLockedError(Exception):
    """May be raised by lock() when the machine is locked by another user."""
    pass

class ARMMachineBase(Machine):

    @classmethod
    def validateArgs(cls, kwargs):
        super(ARMMachineBase, cls).validateArgs(kwargs)
        try:
            kwargs['platform']
        except KeyError as e:
            raise TypeError("Missing key %s" % e.args[0])

    def __init__(self, options, operations, **kwargs):
        super(ARMMachineBase, self).__init__(options, operations, **kwargs)
        self.menulst = None
        self.mmap = None
        self.kernel_args = None
        self.kaluga_args = None
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
                if line.startswith("kernel") or line.startswith("cpudriver"):
                    pts = line.strip().split(" ", 2)
                    if len(pts) == 3:
                        self.kernel_args = pts[-1].split(" ")
                    else:
                        self.kernel_args = []
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

    def get_kaluga_args(self):
        if self.kaluga_args is None:
            for line in self._get_template_menu_lst():
                if 'kaluga' in line:
                    _,_,args = line.strip().split(' ', 2)
                    self.kaluga_args = args.split(' ')
                    break
        return self.kaluga_args

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        f = open(path, 'w')
        f.write(data)
        for line in self._get_mmap():
            f.write(line)
        f.close()

class ARMSimulatorBase(ARMMachineBase):

    def __init__(self, options, operations,
                 boot_timeout=20, **kwargs):
        super(ARMSimulatorBase, self).__init__(options, operations,
                boot_timeout=boot_timeout,
                **kwargs)

    def get_tickrate(self):
        return None

    def get_test_timeout(self):
        """Default test timeout for ARM simulators: 10min"""
        return 10 * 60

    def get_machine_name(self):
        return self.name

class ARMSimulatorOperations(MachineOperations):

    def __init__(self, machine):
        super(ARMSimulatorOperations, self).__init__(machine)
        self.child = None
        self.telnet = None
        self.tftp_dir = None
        self.simulator_start_timeout = 5 # seconds

    def setup(self):
        pass

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
        if self.telnet is not None:
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
                # RH 08.08.2018 The gem5 test seems to have problems with using localhost
                # instead of 127.0.0.1
                self.telnet = telnetlib.Telnet("127.0.0.1", self.telnet_port)
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

class MachineFactory:

    machineFactories = {}

    def __init__(self, name, machineClass, kwargs):
        self._class = machineClass
        self._kwargs = kwargs
        self._name = name

    @classmethod
    def addMachine(cls, name, machineClass, **kwargs):
        cls.machineFactories[name] = MachineFactory(name, machineClass, kwargs)
        machineClass.validateArgs(kwargs)

    def getName(self):
        """Get the name of the machine produced by this factory."""
        return self._name

    def createMachine(self, options):
        """Create a new machine instance."""
        try:
            machine = self._class(options, **self._kwargs)
        except TypeError as e:
            print("Machine class %s failed to instantiate: %s" % (str(self._class), str(e)))
            raise TypeError(e)
        machine.setName(self._name)
        return machine

    @classmethod
    def createMachineByName(cls, name, options):
        """Create a new machine instance."""
        return cls.machineFactories[name].createMachine(options)

# Assume that QEMU, FVP, pandaboard and Gem5 work everywhere if invoked
import qemu
import gem5
import fvp
import pandaboard
import imx8x

# Other site-specific modules will be loaded by the siteconfig module
