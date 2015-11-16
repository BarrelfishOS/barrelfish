# Copyright (c) 2012 ARM Limited
# All rights reserved.
#
# The license below extends only to copyright in the software and shall
# not be construed as granting a license to any other intellectual
# property including but not limited to intellectual property relating
# to a hardware implementation of the functionality of the software
# licensed hereunder.  You may use the software subject to the license
# terms below provided that you ensure that this notice is replicated
# unmodified and in its entirety in all distributions of the software,
# modified or unmodified, in source code or in binary form.
#
# Copyright (c) 2006-2007 The Regents of The University of Michigan
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met: redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer;
# redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution;
# neither the name of the copyright holders nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# Authors: Lisa Hsu

from m5.objects import *

# Base implementations of L1, L2, IO and TLB-walker caches. There are
# used in the regressions and also as base components in the
# system-configuration scripts. The values are meant to serve as a
# starting point, and specific parameters can be overridden in the
# specific instantiations.

class L1Cache(BaseCache):
    assoc = 2
    hit_latency = 1
    response_latency = 1
    mshrs = 10
    tgts_per_mshr = 20
    is_top_level = True

class L2Cache(BaseCache):
    assoc = 8
    hit_latency = 10
    response_latency = 10
    mshrs = 20
    tgts_per_mshr = 12
    write_buffers = 8

class IOCache(BaseCache):
    assoc = 8
    hit_latency = 10
    response_latency = 10
    mshrs = 20
    size = '1kB'
    tgts_per_mshr = 12
    forward_snoops = False
    is_top_level = True

class PageTableWalkerCache(BaseCache):
    assoc = 2
    hit_latency = 1
    response_latency = 1
    mshrs = 10
    size = '1kB'
    tgts_per_mshr = 12
    is_top_level = True
