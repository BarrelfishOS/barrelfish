##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import getpass
import siteconfig

# FIXME: Check why exactly ikq03 does not want to run httperf
LOADGEN_HOSTS = ['emmentaler%d.ethz.ch' % n for n in range(1,4)]
#LOADGEN_HOSTS = ['ikq0%d.ethz.ch' % n for n in range(4,5)]

class ETH(siteconfig.BaseSite):
    # site-specific configuration variables for ETH
    WEBSERVER_NFS_HOST = 'emmentaler.in.barrelfish.org'
    NFS_SERVER_HOST = 'tomme1.in.barrelfish.org'
    WEBSERVER_NFS_PATH_FULL = '/mnt/local/nfs/barrelfish_website'
    WEBSERVER_NFS_TEST_PATH = '/mnt/local/nfs/pravin/website/bftest_crash'
    WEBSERVER_VM_PATH = '/mnt/local/nfs/harness'
    WEBSERVER_1G_PATH = '/mnt/local/nfs/harness_nfs'
    WEBSERVER_1G_FILE = 'G1.file'
#    WEBSERVER_NFS_PATH = WEBSERVER_NFS_TEST_PATH
    WEBSERVER_NFS_PATH = WEBSERVER_NFS_PATH_FULL
    WEBSERVER_LOCAL_PATH = WEBSERVER_NFS_PATH
    HTTPERF_PATH = '/home/netos/tools/i686-pc-linux-gnu/bin/httperf'
    HTTPERF_MAXCLIENTS = len(LOADGEN_HOSTS * 2) # max number of load generators
    IPBENCH_PATH = '/home/netos/tools/ipbench/bin/ipbench.py'
    IPBENCHD_PATH = '/home/netos/tools/ipbench/bin/ipbenchd.py'
    SSH_ARGS='-x -o StrictHostKeyChecking=no -o ControlPath=none'
    HAGFISH_LOCATION='/home/netos/tftpboot/Hagfish.efi'

    def __init__(self):
        self._loadgen_hosts = LOADGEN_HOSTS

    def get_load_generator(self):
        # take the first host, but put it on the back in case we
        # need more clients than available hosts (ie. rotate the list)
        host = self._loadgen_hosts.pop(0)
        self._loadgen_hosts.append(host)
        return getpass.getuser(), host

siteconfig.site = ETH()

# also cause the ETH machines to be loaded/initialised
import machines.eth
