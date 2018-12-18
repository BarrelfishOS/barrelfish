import tests, debug
from common import TestCommon
from results import PassFailResult, RowResults
import sys, re, numpy, os, datetime


class AppelLiBench(TestCommon):
    '''Benchmark GC primitives with Appel Li benchmark'''

    bench_core = 4

    def setup(self, build, machine, testdir):
        # gracefully handle machines with < 5 cores
        if machine.get_ncores() < self.bench_core:
            self.bench_core = machine.get_ncores() - 1

    def get_finish_string(self):
        return "appel_li: done"

    def process_data(self, testdir, rawiter):
        debug.verbose(">> processing data")
        # the test passed if we can produce results
        results = RowResults(['op', 'cycles/iter', '#iters'])
        # and assertions are disabled
        valid = False
        for line in rawiter:
            if line.startswith('Operating system bug'):
                valid = False
                break
            if 'ASSERTIONS DISABLED' in line:
                valid = True
            #if line.startswith(self.get_finish_string()):
            #    break
            if line.startswith("appel_li:"):
                # found data line: <op cycles/iter #iters>
                elems = line.strip().split(':')
                if len(elems) < 3:
                    continue
                _, op, data = elems
                if ' ' not in data:
                    continue
                cyc, _, count, _ = data.strip().split(' ')
                results.add_row([op.strip(), cyc, count])
        if not valid:
            return PassFailResult(False)
        return results

@tests.add_test
class AppelLiDefault(AppelLiBench):
    name = "appel_li_bench_default"

    def get_modules(self, build, machine):
        modules = super(AppelLiDefault, self).get_modules(build, machine)
        modules.add_module("benchmarks/mem_appel", ["core=%d" % self.bench_core])
        return modules

@tests.add_test
class AppelLiDefaultDI(AppelLiBench):
    name = "appel_li_bench_default_di"

    def get_modules(self, build, machine):
        modules = super(AppelLiDefaultDI, self).get_modules(build, machine)
        modules.add_module("benchmarks/mem_appel_di", [["core=%d" % self.bench_core]])
        return modules

@tests.add_test
class AppelLiFull(AppelLiBench):
    name = "appel_li_bench_full"

    def get_modules(self, build, machine):
        modules = super(AppelLiFull, self).get_modules(build, machine)
        modules.add_module("benchmarks/mem_appel_ff", ["core=%d" % self.bench_core])
        return modules

@tests.add_test
class AppelLiFullDI(AppelLiBench):
    name = "appel_li_bench_full_di"

    def get_modules(self, build, machine):
        modules = super(AppelLiFullDI, self).get_modules(build, machine)
        modules.add_module("benchmarks/mem_appel_di_ff", ["core=%d" % self.bench_core])
        return modules

@tests.add_test
class AppelLiSelective(AppelLiBench):
    name = "appel_li_bench_selective"

    def get_modules(self, build, machine):
        modules = super(AppelLiSelective, self).get_modules(build, machine)
        modules.add_module("benchmarks/mem_appel_sf", ["core=%d" % self.bench_core])
        return modules

@tests.add_test
class AppelLiSelectiveDI(AppelLiBench):
    name = "appel_li_bench_selective_di"

    def get_modules(self, build, machine):
        modules = super(AppelLiSelectiveDI, self).get_modules(build, machine)
        modules.add_module("benchmarks/mem_appel_di_sf", ["core=%d" % self.bench_core])
        return modules

@tests.add_test
class AppelLiSelectiveHint(AppelLiBench):
    name = "appel_li_bench_selective_hint"

    def get_modules(self, build, machine):
        modules = super(AppelLiSelectiveHint, self).get_modules(build, machine)
        modules.add_module("benchmarks/mem_appel_sfh", ["core=%d" % self.bench_core])
        return modules

@tests.add_test
class AppelLiSelectiveHintDI(AppelLiBench):
    name = "appel_li_bench_selective_hint_di"

    def get_modules(self, build, machine):
        modules = super(AppelLiSelectiveHintDI, self).get_modules(build, machine)
        modules.add_module("benchmarks/mem_appel_di_sfh", ["core=%d" % self.bench_core])
        return modules
