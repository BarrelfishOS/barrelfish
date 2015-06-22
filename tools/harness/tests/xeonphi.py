##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

# MAKEOPTS=-j12 ./scalebench.py -m xeon_phi_1 -B /mnt/local/acreto/barrelfish/build -t xeon_phi_spawn /mnt/local/acreto/barrelfish/ /mnt/local/acreto/harness-results/ -v


import re, os, sys, debug, shutil, datetime

import tests
from common import TestCommon
from barrelfish import BootModules
from results import PassFailResult

XEON_PHI_IMAGE_NFS="nfs://10.110.4.4/mnt/local/nfs/barrelfish/xeon_phi/"
XEON_PHI_IMAGE_LOCATION="/mnt/local/nfs/barrelfish/xeon_phi/"
#module /acreto/x86_64/sbin/xeon_phi_mgr

@tests.add_test
class XeonPhi_Spawn_test(TestCommon):
    '''Xeon Phi Spawn test'''
    name = "xeon_phi_spawn"

    image_dir=XEON_PHI_IMAGE_NFS
    nfs_dir=XEON_PHI_IMAGE_LOCATION
    nphi = 2

    def set_xeon_phi_bootmodules(self, build_dir, machine):
        fullpath = os.path.join(build_dir, 'menu.lst.k1om')
        f = open(fullpath, 'a')
        f.write("\n")
        # put additional modules here
        f.close()


    def setup(self, build, machine, testdir) :
       
        # setup xeon phi nfs directory

        timestamp = datetime.datetime.now().strftime('%Y%m%d%H%M%S')
        run_name = "harness_" + str(timestamp)
        self.image_dir = os.path.join(XEON_PHI_IMAGE_LOCATION, run_name)
        self.nfs_dir=os.path.join(XEON_PHI_IMAGE_NFS, run_name)

        debug.verbose('creating image directory' + self.image_dir)
        os.makedirs(self.image_dir)

        # setup menu.lst.k1om
        menulst = os.path.join(build.build_dir, "menu.lst.k1om")
        if (os.path.isfile(menulst)) :
            os.remove(menulst)
        debug.checkcmd(["make", "menu.lst.k1om"], cwd=build.build_dir)
        self.set_xeon_phi_bootmodules(build.build_dir, machine)

        debug.checkcmd(["make", "k1om/sbin/weever"], cwd=build.build_dir)

        super(XeonPhi_Spawn_test, self).setup(build, machine, testdir)

    def run(self, build, machine, testdir):
        xphi_img = os.path.join(build.build_dir, 'k1om/xeon_phi_multiboot')

        debug.verbose('copying xeon phi <' + xphi_img + '>image to ' + self.image_dir)
        shutil.copy(xphi_img, self.image_dir)

        xphi_img = os.path.join(build.build_dir, 'k1om/sbin/weever')

        debug.verbose('copying xeon phi <' + xphi_img + '>image to ' + self.image_dir)
        shutil.copy(xphi_img, self.image_dir)
        
        return super(XeonPhi_Spawn_test, self).run(build, machine, testdir)

    def cleanup(self, machine):
        # remove the xeon phi image directory
        debug.verbose('removing ' + self.image_dir)
        shutil.rmtree(self.image_dir, ignore_errors=True)
        super(XeonPhi_Spawn_test, self).cleanup(machine)        

    def get_modules(self, build, machine):
        modules = super(XeonPhi_Spawn_test, self).get_modules(build, machine)
        modules.add_module("xeon_phi_mgr", [""])
        modules.add_module("xeon_phi", ["auto", 
                                        "--nfs=" + self.nfs_dir])
        modules.add_module("e1000n", ["auto", "noirq"])
        modules.add_module("NGD_mng", ["auto"])
        modules.add_module("netd", ["auto"])
        modules.add_module("dma_mgr", [""])

        return modules

    def is_finished(self, line):
        #m = re.search("Xeon Phi operational: xeon_phi.([0-9]).ready", line)
        m = re.search("Xeon Phi operational: xeon_phi.1.ready", line)
        if m :
            return True
        else :
            return False        

    def get_finish_string(self):
        return "Xeon Phi operational: xeon_phi.([0-9]).ready"

    def boot(self, *args):
        super(XeonPhi_Spawn_test, self).boot(*args)
#        self.set_timeout(NFS_TIMEOUT)

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        passed = False;
        for line in rawiter:
            m = re.search("Xeon Phi operational: xeon_phi.1.ready", line)
            if m :
                passed = True
               
        return PassFailResult(passed)
