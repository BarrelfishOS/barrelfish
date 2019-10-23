##########################################################################
# Copyright (c) 2009, 2010, 2011, 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os.path

class Module(object):
    def __init__(self, module, args):
        self.module = module
        self.args = args

class BootModules(object):
    """Modules to boot (ie. the menu.lst file)"""

    def __init__(self, machine, prefix, kernel=None):
        self.hypervisor = None
        self.prefix = prefix

        self.kernel = None
        if not kernel is None:
            self.kernel = os.path.join(prefix, kernel)
        self.kernelArgs = []
        self.cpu_driver = None
        self.boot_driver = None
        self.boot_driver_args = []
        self.modules = []
        self.machine = machine

    def set_kernel(self, kernel, args=[]):
        if kernel == None:
            self.kernel = None
        else: 
            self.kernel = os.path.join(self.prefix, kernel)
        if args is None:
            args = []
        self.kernelArgs = args

    def add_kernel_args(self, args):
        if args:
            self.kernelArgs.extend(args)

    def set_cpu_driver(self, cpu_driver, args=[]):
        if cpu_driver == None :
            self.cpu_driver = None
            self.kernelArgs = []
        else :
            self.cpu_driver = os.path.join(self.prefix, cpu_driver)
            if args is None:
                args = []
            self.kernelArgs = args

    def set_boot_driver(self, boot_driver,  args=[]):
        if boot_driver == None :
            self.boot_driver = None
            self.boot_driver_args = []
        else :
            self.boot_driver = os.path.join(self.prefix, boot_driver);
            if args is None:
                args = []
            self.boot_driver_args = args
    
    def set_boot_driver_args(self, args):
        self.boot_driver_args.extend(args)

    def set_hypervisor(self, h):
        self.hypervisor = h

    # does modulespec describe modulename?
    def _module_matches(self, modulename, modulespec):
        if '/' in modulespec: # if the spec contains a path, they must be the same
            return modulespec == modulename
        else: # otherwise, we look at the last part of the name only
            return modulespec == modulename.rsplit('/',1)[-1]

    def add_module(self, module, args=[]):
        if module.startswith('/'):
            # absolute path are converted to relative and left as-is
            module = module[1:]
        else:
            # relative paths are prepended with the prefix
            module = os.path.join(self.prefix, module)
        mod = Module(module, args)
        self.modules.append(mod)
        return mod

    def add_module_arg(self, modulename, arg):
        for mod in self.modules:
            if self._module_matches(mod.module, modulename):
                mod.args.append(arg)

    def get_menu_data(self, path, root="(nd)"):
        assert(self.kernel != None 
                or (self.boot_driver != None and self.cpu_driver != None))
        r = "timeout 0\n"
        r += "title Harness image\n"
        r += "root %s\n" % root
        if self.boot_driver :
            r += "bootdriver %s %s\n" % (
                os.path.join(path, self.boot_driver), " ".join(self.boot_driver_args))
        if self.cpu_driver :
            r += "cpudriver %s %s\n" % (
                os.path.join(path, self.cpu_driver), " ".join(self.kernelArgs))
        if self.hypervisor:
            r += "hypervisor %s\n" % os.path.join(path, self.prefix, self.hypervisor)
        
        if self.kernel :
            r += "kernel %s %s\n" % (
                os.path.join(path, self.kernel), " ".join(self.kernelArgs))
        for module in self.modules:
            r += "modulenounzip %s %s\n" % (os.path.join(path, module.module), " ".join(map(str, module.args)))
            
        return r

    # what targets do we need to build/install to run this test?
    def get_build_targets(self):
        ret = list(set([ module.module for module in self.modules] ))
        
        if self.kernel :
            ret.append(self.kernel)
        if self.cpu_driver :
            ret.append(self.cpu_driver)
        if self.boot_driver : 
            ret.append(self.boot_driver)
        if self.hypervisor:
            ret.append(self.hypervisor)

        return ret
