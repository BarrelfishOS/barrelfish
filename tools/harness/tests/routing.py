##########################################################################
# Copyright (c) 2009, 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import debug, tests
from common import TestCommon, TimeoutError
from results import RawResults

class RouteCommon(TestCommon):
    def get_modules(self, build, machine):
        modules = super(RouteCommon, self).get_modules(build, machine)
        modules.add_module(self.get_module_name())
        return modules

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        ncores = machine.get_ncores()

        if ncores == 4:
            iterations = 3
        elif ncores == 16:
            iterations = 16
        elif ncores == 24:
            iterations = 9
        elif ncores == 32:
            iterations = 11

        for i in range(1, iterations):
            debug.log('running %s iteration %d/%d' % (self.name, i, iterations - 1))
            modules.reset_module(self.get_module_name(), ["core=1", str(i)])
            self.boot(machine, modules)
            for line in self.collect_data(machine):
                yield line

    def process_data(self, testdir, rawiter):
        results = RawResults('radix')
        times = []
        radix = None
        for line in rawiter:
            m = re.match("radix is (\d+)", line)
            if m:
                if times:
                    results.add_group(radix, times)
                radix = int(m.group(1))
                times = []
                continue

            m = re.match("\d+ total (\d+)", line)
            if m:
                assert(radix is not None)
                times.append(int(m.group(1)))

        if len(times) != 0:
            results.add_group(radix, times)
        return results

@tests.add_test
class RadixRoute(RouteCommon):
    ''' Tests performance of various routing trees '''
    name = "radix_route"

    def get_module_name(self):
        return "radix_route_bench"

@tests.add_test
class BarrierRoute(RouteCommon):
    ''' Tests performance of barriers on the routing library '''
    name = "barrier_bench"

    def get_module_name(self):
        return "barrier_bench"
