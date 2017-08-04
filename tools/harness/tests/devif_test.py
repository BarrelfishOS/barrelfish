##########################################################################
# Copyright (c) 2017, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re, datetime
import debug, tests
import subprocess
import os
import socket, struct
from common import TestCommon, TimeoutError
from results import RowResults, PassFailResult

TEST_TIMEOUT = datetime.timedelta(minutes=8)

class DevifTests(TestCommon):

    def __init__(self, options):
        super(DevifTests, self).__init__(options)

    def get_module_name(self):
        return "devif_test"

    def boot(self, *args):
        super(DevifTests, self).boot(*args)
        self.set_timeout(TEST_TIMEOUT)
       

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("e10k", ["auto", "function=0"])
        modules.add_module("sfn5122f", ["auto", "function=0"])
        modules.add_module("devif_idc", ["core=1"])

        f = os.popen('ifconfig eno2 | grep "inet\ addr" | cut -d: -f2 | cut -d" " -f1')
        ip_string = f.read()
        packedIP = socket.inet_aton(ip_string)
        src_ip = struct.unpack("!L", packedIP)[0]

        if self.CARD == "sfn5122f":
            dst_string = subprocess.check_output('nslookup %s-sf.in.barrelfish.org | sed -n 5p | awk -F\" \" \'{print $NF}\'' % machine.name, shell=True)
        else:
            dst_string = subprocess.check_output('nslookup %s-e10k.in.barrelfish.org | sed -n 5p | awk -F\" \" \'{print $NF}\'' % machine.name, shell=True)

        packedIP = socket.inet_aton(dst_string)
        dst_ip = struct.unpack("!L", packedIP)[0]

        modules.add_module(self.get_module_name(), ["core=2", self.OP, src_ip, dst_ip, self.CARD])
        return modules

    def get_finish_string(self):
        return "SUCCESS"

    def process_line(self, line):
        m = re.match(r'# IP Addr (\d+\.\d+\.\d+\.\d+)', line)
        if m:
            self.loop = subprocess.Popen('while :; do echo -n "Data Data Data" | nc -4u -q1 %s 7; done' % m.group(1), shell=True)


    def process_data(self, testdir, rawiter):
        passed = False
        for line in rawiter:
            if "SUCCESS" in line:
                passed = True
        self.loop.kill()
        return PassFailResult(passed)

@tests.add_test
class DevifNetTxSF(DevifTests):
    ''' Devif Net TX Test'''
    name = "devif_nettx_sf"
    OP = "net_tx"
    CARD = "sfn5122f"

@tests.add_test
class DevifNetTxE10k(DevifTests):
    ''' Devif Net TX Test'''
    name = "devif_nettx_e10k"
    OP = "net_tx"
    CARD = "e10k"

@tests.add_test
class DevifNetRxSF(DevifTests):
    ''' Devif Net RX Test'''
    name = "devif_netrx_sf"
    OP = "net_rx"
    CARD = "sfn5122f"

@tests.add_test
class DevifNetRxE10k(DevifTests):
    ''' Devif Net RX Test'''
    name = "devif_netrx_e10k"
    OP = "net_rx"
    CARD = "e10k"
    f = os.popen('ifconfig eno2 | grep "inet\ addr" | cut -d: -f2 | cut -d" " -f1')
    ip_string = f.read()
    packedIP = socket.inet_aton(ip_string)
    src_ip = struct.unpack("!L", packedIP)[0]

@tests.add_test
class DevifIdcTest(DevifTests):
    ''' Devif IDC Test'''
    name = "devif_idc_test"
    OP = "idc"
    CARD = "none"
    src_ip = 0

@tests.add_test
class DevifDebug(DevifTests):
    ''' Devif Debug Backend Test'''
    name = "devif_debug"

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("devif_idc", ["core=1"])
        modules.add_module("devif_debug_test")

        return modules

