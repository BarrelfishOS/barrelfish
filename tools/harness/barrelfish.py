##########################################################################
# Copyright (c) 2009, 2010, 2011, 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os.path

class BootModules(object):
    """Modules to boot (ie. the menu.lst file)"""

    def __init__(self, machine, prefix, kernel):
        self.hypervisor = None
        self.prefix = prefix
        self.kernel = os.path.join(prefix, kernel)
        self.kernelArgs = []
        self.modules = {}
        self.machine = machine

    def set_kernel(self, kernel, args=[]):
        self.kernel = kernel
        self.kernelArgs = args

    def add_kernel_arg(self, arg):
        self.kernelArgs.append(arg)

    def set_hypervisor(self, h):
        self.hypervisor = h

    # does modulespec describe modulename?
    def _module_matches(self, modulename, modulespec):
        if '/' in modulespec: # if the spec contains a path, they must be the same
            return modulespec == modulename
        else: # otherwise, we look at the last part of the name only
            return modulespec == modulename.rsplit('/',1)[-1]

    def add_module(self, module, args=[]):

        # Support for build targets with / in their name (e.g. examples/xmpl-spawn)
        module = module.replace('$BUILD', self.prefix)

        if module.startswith('/'):
            module = module[1:]
        else:
            assert self.kernel
            module = os.path.join(self.prefix, module)
        self.modules[module] = args

    def add_module_arg(self, modulename, arg):
        for (mod, args) in self.modules.items():
            if self._module_matches(mod, modulename):
                args.append(arg)

    def get_menu_data(self, path, root="(nd)"):
        assert(self.kernel[0])
        r = "timeout 0\n"
        r += "title Harness image\n"
        r += "root %s\n" % root
        if self.hypervisor:
            r += "hypervisor %s\n" % os.path.join(path, self.prefix, self.hypervisor)
        r += "kernel %s %s\n" % (
                os.path.join(path, self.kernel), " ".join(map(str, self.kernelArgs)))
        for (module, args) in self.modules.iteritems():
            r += "modulenounzip %s %s\n" % (os.path.join(path, module), " ".join(map(str, args)))
        return r

    # what targets do we need to build/install to run this test?
    def get_build_targets(self):
        ret = list(set([self.kernel] + self.modules.keys()))

        if self.hypervisor:
            ret.append(self.hypervisor)

        return ret

def default_bootmodules(build, machine):
    """Returns the default boot module configuration for the given machine."""
    # FIXME: clean up / separate platform-specific logic

    a = machine.get_bootarch()
    m = BootModules(machine)

    # set the kernel: elver on x86_64
    if a == "x86_64":
        m.set_kernel("%s/sbin/elver" % a, machine.get_kernel_args())
    elif a == "armv7" or a == "armv8":
        m.set_kernel("%s/sbin/cpu_%s" % (a, machine.get_platform()), machine.get_kernel_args())
    else:
        m.set_kernel("%s/sbin/cpu" % a, machine.get_kernel_args())

    # default for all barrelfish archs
    # hack: cpu driver is not called "cpu" for ARMv7 builds
    if a == "armv7" or a == "armv8":
        m.add_module("%s/sbin/cpu_%s" % (a, machine.get_platform()), machine.get_kernel_args())
    else:
        m.add_module("%s/sbin/cpu" % a, machine.get_kernel_args())

    m.add_module("%s/sbin/init" % a)
    m.add_module("%s/sbin/mem_serv" % a)
    m.add_module("%s/sbin/monitor" % a)
    m.add_module("%s/sbin/ramfsd" % a, ["boot"])
    m.add_module("%s/sbin/skb" % a, ["boot"])
    m.add_module("%s/sbin/spawnd" % a, ["boot"])
    m.add_module("%s/sbin/startd" % a, ["boot"])
    m.add_module("/eclipseclp_ramfs.cpio.gz", ["nospawn"])
    m.add_module("/skb_ramfs.cpio.gz", ["nospawn"])

    # armv8
    if a == "armv8" :
        m.add_module("%s/sbin/acpi" % a, ["boot"])

    # SKB and PCI are x86-only for the moment
    if a == "x86_64" or a == "x86_32":
        m.add_module("%s/sbin/acpi" % a, ["boot"])
        m.add_module("%s/sbin/routing_setup" %a, ["boot"])
        m.add_module("%s/sbin/corectrl" % a, ["auto"])

        # Add pci with machine-specific extra-arguments
        m.add_module("%s/sbin/pci" % a, ["auto"] + machine.get_pci_args())

        # Add kaluga with machine-specific bus:dev:fun triplet for eth0
        # interface
        m.add_module("%s/sbin/kaluga" % a,
                ["boot", "eth0=%d:%d:%d" % machine.get_eth0()])

    # coreboot should work on armv7 now
    if a == "armv7":
        m.add_module("corectrl", ["auto"])
        m.add_module("kaluga", machine.get_kaluga_args())
    return m
