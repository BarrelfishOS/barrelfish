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
