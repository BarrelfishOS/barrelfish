##########################################################################
# Copyright (c) 2017, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re, datetime
import debug, tests
import subprocess
import os
import socket, struct, fcntl
import thread
from common import TestCommon, TimeoutError
from results import RowResults, PassFailResult

TEST_TIMEOUT = datetime.timedelta(minutes=240)

mac = {'babybel1': 130587495626, 
       'babybel2': 130587510022,
       'babybel3': 130587512798,
       'babybel4': 130589790232,
       'ziger2': 65817495764,
       'ziger1': 116527143012, }

# Fallback if gethostip does not work
ip = {'babybel1': 174982272, 
       'babybel2': 174982270,
       'babybel3': 174982271,
       'ziger2': 174982183,
       'ziger1': 174982183, }


class DevifTests(TestCommon):

    def __init__(self, options):
        super(DevifTests, self).__init__(options)

    def get_module_name(self):
        return "devif_test"

    def boot(self, *args):
        super(DevifTests, self).boot(*args)
        self.set_timeout(TEST_TIMEOUT)

    def get_decimal_ip(self, hostname):
        try:
            iphex = subprocess.check_output('gethostip -x %s' % hostname, shell=True)
            return '%d' % int(iphex, 16)
        except:
            return ip[hostname.split('-')[0]]
      
    def get_local_mac(self, ifname):
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        info = fcntl.ioctl(s.fileno(), 0x8927,  struct.pack('256s', ifname[:15]))
        hexmac = ''.join(['%02x' % ord(char) for char in info[18:24]])
        return '%d' % int(hexmac, 16)

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("e1000n", ["auto"])
        modules.add_module("net_sockets_server", ["nospawn"])
        modules.add_module("devif_idc", ["core=1"])
        modules.add_module("e10k", ["auto", "function=0"])

        hostname = '%s.in.barrelfish.org' % subprocess.check_output('hostname -s', shell=True).rstrip()
        src_ip = self.get_decimal_ip(hostname)

        if 'ziger2' in machine.name:
            modules.add_module("sfn5122f", ["auto", "function=0"])
            dst_ip = self.get_decimal_ip('%s-sf.in.barrelfish.org' % machine.name)
        else:
            dst_ip = self.get_decimal_ip('%s-e10k.in.barrelfish.org' % machine.name)

        modules.add_module(self.get_module_name(), ["core=2", self.OP, src_ip, dst_ip, self.CARD])
        return modules

    def get_finish_string(self):
        return "SUCCESS"


    def thread_func (self, dummy, dummy2):
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        while True:
            s.sendto("Data Data Data", (self.ip, 7))
            
    def start_loop(self):
        self.thread = thread.start_new_thread(self.thread_func, (self, 0))

    def process_line(self, line):
        m = re.match(r'# IP Addr (\d+\.\d+\.\d+\.\d+)', line)
        if m:
            self.start_loop()
            self.ip = m.group(1)


    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if "SUCCESS" in line:
                return PassFailResult(True)

        return PassFailResult(False)


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
    CARD = "e10k:8086:10fb:0006:0000:0000"


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

@tests.add_test
class DevifIdcTest(DevifTests):
    ''' Devif IDC Test'''
    name = "devif_idc_test"
    OP = "idc"
    CARD = "none"

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("devif_idc", ["core=1"])
        modules.add_module(self.get_module_name(), ["core=2", self.OP, 0, 0, "none"])
        return modules


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

@tests.add_test
class DevifDebug(DevifTests):
    ''' Devif Benchmark'''
    name = "devif_bench"

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("e1000n", ["auto"])
        modules.add_module("net_sockets_server", ["nospawn"])
        modules.add_module("devif_bench", ["core=2", machine.name])

        return modules


@tests.add_test
class DevifDebug(DevifTests):
    ''' Devif Benchmark'''
    name = "devif_bench_stack"

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("e1000n", ["auto"])
        modules.add_module("net_sockets_server", ["nospawn"])
        modules.add_module("devif_bench_stack", ["core=2", machine.name])

        return modules

@tests.add_test
class DevifUDP(DevifTests):
    ''' Devif UDP Backend Test'''
    name = "devif_udp"
    data = ("Data Data Data Data")

    def get_module_name(self):
        return "devif_udp"

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("net_sockets_server", ["nospawn"])
        hostname = '%s.in.barrelfish.org' % subprocess.check_output('hostname -s', shell=True).rstrip()
        dst_ip = self.get_decimal_ip(hostname)
        dst_mac = self.get_local_mac('eno2')

        if ('ziger2' in machine.name):
        #if ('ziger2' in machine.name) or ('babybel2' in machine.name):
            if 'ziger2' in machine.name:
                src_ip = self.get_decimal_ip('%s-sf.in.barrelfish.org' % machine.name)
                modules.add_module("sfn5122f", ["auto", "function=0"])
                self.cardname = "sfn5122f"
            else:
                src_ip = self.get_decimal_ip('%s-sf.in.barrelfish.org' % machine.name)
                modules.add_module("sfn5122f", ["auto", "function=0"])
                self.cardname = "sfn5122f"
        else:
            modules.add_module("e10k", ["auto", "function=0"])
            src_ip = self.get_decimal_ip('%s-e10k.in.barrelfish.org' % machine.name)
            self.cardname = "e10k:8086:10fb:0006:0000:0000"

        modules.add_module(self.get_module_name(), ["core=2", dst_ip, dst_mac, 20000, 20000, self.cardname])
        return modules

    def thread_func (self, dummy, dummy2):
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        while True:
            s.sendto(self.data, (self.ip, 20000))

    def start_loop(self):
        self.thread = thread.start_new_thread(self.thread_func, (self, 0))

    def process_line(self, line):
        m = re.match(r'# IP Addr (\d+\.\d+\.\d+\.\d+)', line)
        if m:
            self.ip = m.group(1)

        m1 = re.match(r'Testing receiving UDP packets', line)
        if m1:
            self.start_loop()

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if "SUCCESS" in line:
                return PassFailResult(True)

        return PassFailResult(False)


class DevifDebug(DevifTests):
    ''' Devif Benchmark'''
    name = "devif_bench_stack"

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(DevifTests, self).get_modules(build, machine)
        modules.add_module("e1000n", ["auto"])
        modules.add_module("net_sockets_server", ["nospawn"])
        modules.add_module("devif_bench_stack", ["core=2", machine.name])

        return modules

#@tests.add_test
#class DevifUPDecho(DevifUDP):
#    ''' Devif Debug Backend Test'''
#    name = "devif_udp_echo"
#
#    def get_module_name(self):
#        return "devif_echo"



