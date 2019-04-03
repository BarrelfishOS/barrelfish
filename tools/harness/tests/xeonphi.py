##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

# MAKEOPTS=-j12 ./scalebench.py -m xeon_phi_1 -B /mnt/local/" + machine.get_tftp_subdir() + "/barrelfish/build -t xeon_phi_spawn /mnt/local/" + machine.get_tftp_subdir() + "/barrelfish/ /mnt/local/" + machine.get_tftp_subdir() + "/harness-results/ -v


import re, os, sys, debug, shutil, datetime

import tests
from common import TestCommon
from barrelfish import BootModules
from results import PassFailResult

#module /" + machine.get_tftp_subdir() + "/x86_64/sbin/xeon_phi_mgr

@tests.add_test
class XeonPhi_Boot_Test(TestCommon):
    '''Xeon Phi Spawn test'''
    name = "xeon_phi_boot"
    nphi = 2;

    def set_xeon_phi_bootmodules(self, build_dir, machine):
        fullpath = os.path.join(machine.get_tftp_dir(), 'menu.lst.k1om')
        f = open(fullpath, 'w')

        tftpdir = machine._operations.get_tftp_subdir()

        f.write("title   Barrelfish \n")
        f.write("root    (nd) \n")
        f.write("kernel  /" + tftpdir + "/k1om/sbin/weever\n")
        f.write("module  /" + tftpdir + "/k1om/sbin/cpu loglevel=3 \n")
        f.write("module  /" + tftpdir + "/k1om/sbin/init\n")

        # Domains spawned by init
        f.write("module  /" + tftpdir + "/k1om/sbin/mem_serv\n")
        f.write("module  /" + tftpdir + "/k1om/sbin/monitor\n")

        # Special boot time domains spawned by monitor
        f.write("module  /" + tftpdir + "/k1om/sbin/ramfsd boot \n")
        f.write("module  /" + tftpdir + "/k1om/sbin/skb boot \n")
        f.write("module  /" + tftpdir + "/k1om/sbin/xeon_phi boot \n")
        f.write("module  /" + tftpdir + "/k1om/sbin/spawnd boot \n")
        f.write("module  /" + tftpdir + "/k1om/sbin/startd boot \n")
        f.write("module  /" + tftpdir + "/k1om/sbin/proc_mgmt boot \n")
        # drivers
        f.write("module  /" + tftpdir + "/k1om/sbin/corectrl auto \n")

        # GDDR Memory we have 6GB on our Xeon PHi
        f.write("mmap map 0x0000000000 0x00FEE00000 1 \n")
        # memory hole for the APIC and the flash rom 
        f.write("mmap map 0x00FEE00000 0x120000 3 \n")
        f.write("mmap map 0x0100000000 0x80000000 1 \n")
        # put additional modules herei
        f.write("\n")
        f.close()


    def setup(self, build, machine, testdir) :
        super(XeonPhi_Boot_Test, self).setup(build, machine, testdir)

        # setup menu.lst.k1om
        menulst = os.path.join(machine.get_tftp_dir(), "menu.lst.k1om")
        if (os.path.isfile(menulst)) :
            os.remove(menulst)
        self.set_xeon_phi_bootmodules(build.build_dir, machine)
#        self.nphi = machine.get_xphi_ncards()
        

    def get_build_targets(self, build, machine):
        targets = super(XeonPhi_Boot_Test, self).get_build_targets(build, machine)
        targets.append('k1om/sbin/weever')
        targets.append('k1om/sbin/cpu')
        targets.append('k1om/sbin/init')
        targets.append('k1om/sbin/mem_serv')
        targets.append('k1om/sbin/monitor')
        targets.append('k1om/sbin/ramfsd')
        targets.append('k1om/sbin/xeon_phi')
        targets.append('k1om/sbin/spawnd')
        targets.append("k1om/sbin/skb")
        targets.append('k1om/sbin/startd')
        targets.append('k1om/sbin/proc_mgmt')
        targets.append('k1om/sbin/corectrl')
        return targets

    


    def cleanup(self, machine):
        # remove the xeon phi image directory
        menulst = os.path.join(machine.get_tftp_dir(), "menu.lst.k1om")
        if (os.path.isfile(menulst)) :
            os.remove(menulst)
        super(XeonPhi_Boot_Test, self).cleanup(machine)        

    def get_modules(self, build, machine):
        modules = super(XeonPhi_Boot_Test, self).get_modules(build, machine)
        tftpdir = machine._operations.get_tftp_subdir()
        modules.add_module("xeon_phi_mgr", [""])
        modules.add_module("xeon_phi", ["auto", 
                                        "--tftp=tftp://10.110.4.4:69",
                                        "--modlist=/" + tftpdir + "/menu.lst.k1om"])
        modules.add_module("e1000n", ["auto"])
        modules.add_module("net_sockets_server", ["nospawn"])
        modules.add_module("dma_mgr", [""])

        return modules

    def get_finish_string(self):
        return "Xeon Phi operational: xeon_phi." + str(self.nphi - 1) + ".ready"

    def boot(self, *args):
        super(XeonPhi_Boot_Test, self).boot(*args)
#        self.set_timeout(NFS_TIMEOUT)

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        print "PROCESS DATA"
        passed=False
        for line in rawiter:
            m = re.search("Xeon Phi operational: xeon_phi." + str(self.nphi - 1) + ".ready", line)
            if m:
                passed=True
                
        return PassFailResult(passed)
