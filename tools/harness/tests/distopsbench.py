import tests, debug
from common import TestCommon
from results import PassFailResult, RawResults
import sys, re, numpy, os, datetime

import matplotlib.pyplot as plt

OPERATIONHEADER = re.compile("^# Benchmarking ([A-Z 0-9]+): nodes=(\d+)$")
DATAHEADER = re.compile("^# node (\d+): tsc_per_us = (\d+); numcopies = (\d+).*")
DATALINE = re.compile("^\d+$")

def tsc_to_us(ts, tscperus):
    return ts / float(tscperus)

class DistopsPlot(object):
    def __init__(self, machine):
        self.nodecount = -1
        self.nodedata = dict()
        self.tscperus = dict()
        self.operation = "n/a"
        self.machine = machine

    def read_data(self, rawiter):
        currnode = -1
        currcopies = -1
        passed = False

        for line in rawiter:
            line = line.strip()
            if line == "# Benchmark done!":
                passed = True
            ophead = OPERATIONHEADER.match(line)
            if ophead is not None:
                self.operation = ophead.group(1).lower()
                self.nodecount = int(ophead.group(2))
                debug.verbose("Found starting line; nodecount=%d" % self.nodecount)
                for x in xrange(1, self.nodecount+1):
                    self.nodedata[x] = dict()

            if self.nodecount > 0:
                header = DATAHEADER.match(line)
                if header is not None:
                    debug.verbose("found meta line")
                    currnode = int(header.group(1))
                    tscperus = int(header.group(2))
                    currcopies = int(header.group(3))
                    debug.verbose("node = %d, tsc_per_us = %d, copies = %d" % \
                            (currnode, tscperus, currcopies))
                    if currnode not in self.tscperus.keys():
                        self.tscperus[currnode] = tscperus
                    self.nodedata[currnode][currcopies] = []
                data = DATALINE.match(line)
                if data is not None and currnode > 0:
                    self.nodedata[currnode][currcopies].append(int(line))
        return passed

    def _finalize_plot(self, outfile, xlabel, ylabel, ylim, fix_xticks=False, draw_legend=False):
        plt.xlabel(xlabel if xlabel is not None else '#capabilities on node (over base set of capabilities)')
        plt.ylabel(ylabel if ylabel is not None else r'latency in $\mu s$')
        if draw_legend:
            plt.legend(loc='lower right')
        plt.title("%s latency (%s)" % (self.operation, self.machine))
        if fix_xticks:
            ax = plt.gca()
            assert(1 in self.nodedata.keys())
            xlabels = plt.setp(ax, xticklabels=sorted(self.nodedata[1].keys()))
            plt.setp(xlabels, rotation=90)
        # TODO: think about using numpy.percentile to figure out sensible ylim
        if ylim is not None:
            ax.set_ylim(ylim)
        plt.tight_layout()
        plt.savefig('%s.pdf' % outfile, format='pdf')

    def boxplot(self, outfile, all_nodes=False, xlabel=None, ylabel=None, ylim=None):
        fig = plt.figure()
        # grab first node id with data
        startn = sorted(self.tscperus.keys())[0]
        # count #nodes with data
        nodecount = len(self.tscperus.keys())
        assert(nodecount >= 1)
        if not all_nodes:
            nodecount=1
        for nodeid in xrange(startn, startn+nodecount):
            nodedata = self.nodedata[nodeid]
            if nodeid not in self.tscperus.keys():
                continue;
            node_tscperus = self.tscperus[nodeid]
            plotdata = []
            ploterr = []
            for cnt in sorted(nodedata.keys()):
                usvals = map(lambda t: tsc_to_us(t, node_tscperus), nodedata[cnt])
                plotdata.append(usvals)
            plt.boxplot(plotdata)
        self._finalize_plot(outfile, xlabel, ylabel, ylim, fix_xticks=True)


    def plot(self, outfile, xlabel=None, ylabel=None, ylim=None):
        sym = [ '', 'x', 'o', '*' ]
        fig = plt.figure()
        nodedata = self.nodedata
        for nodeid in xrange(1, self.nodecount+1):
            if nodeid not in self.tscperus.keys():
                continue
            node_tscperus = self.tscperus[nodeid]
            plotdata = []
            ploterr = []
            for cnt in sorted(nodedata[nodeid].keys()):
                usvals = map(lambda t: tsc_to_us(t, node_tscperus), nodedata[nodeid][cnt])
                plotdata.append(numpy.mean(usvals))
                ploterr.append(numpy.std(usvals))
            plt.errorbar(sorted(nodedata[nodeid].keys()), plotdata, yerr=ploterr, label='core %d' % nodeid)

        #plt.xscale('log', basex=2)
        self._finalize_plot(outfile, xlabel, ylabel, ylim, draw_legend=True)

    def get_raw_results(self, name):
        rr = dict()
        for nodeid in self.nodedata.keys():
            if nodeid not in self.tscperus.keys():
                # skip nodes that aren't running benchmark for some cases
                continue
            r = RawResults('mdbsize', name="%s_node%d" % (name, nodeid))
            for d in self.nodedata[nodeid].keys():
                r.add_group(d, self.nodedata[nodeid][d])
            rr[nodeid] = r
        return rr

class DistopsBench(TestCommon):
    '''Base class for common code for distops benchmarks'''
    def __init__(self, options):
        super(DistopsBench, self).__init__(options)
        self.xlabel = None
        self.ylabel = None
        self.plot_ylim = None
        self.boxplot_ylim = None
        self.boxplot_all_nodes = False

    def get_finish_string(self):
        return "# Benchmark done!"

    def process_data(self, testdir, rawiter):
        debug.verbose("Processing data for %s" % self.name)
        plot = DistopsPlot(self.machine)
        debug.verbose(">>> Reading data for plotting")
        passed = plot.read_data(rawiter)
        plotf = "%s/boxplot_%s" % (testdir, self.name)
        debug.verbose(">>> Saving boxplot to %s.pdf" % plotf)
        plot.boxplot(plotf, all_nodes=self.boxplot_all_nodes,
                     xlabel=self.xlabel, ylabel=self.ylabel,
                     ylim=self.boxplot_ylim)
        plotf = "%s/plot_%s" % (testdir, self.name)
        debug.verbose(">>> Saving plot to %s.pdf" % plotf)
        plot.plot(plotf, ylabel=self.ylabel, ylim=self.plot_ylim)
        rr = plot.get_raw_results(self.name)
        return [ rr[k] for k in sorted(rr.keys()) ]

@tests.add_test
class DistopsBenchDeleteForeign(DistopsBench):
    '''Benchmark latency of deleting foreign copy of capability'''
    name = 'distops_bench_delete_foreign'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchDeleteForeign, self).get_modules(build, machine)
        modules.add_module("bench_delete_foreign_copy",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_delete_foreign_copy",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchDeleteLast(DistopsBench):
    '''Benchmark latency of deleting last copy of capability'''
    name = 'distops_bench_delete_last'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchDeleteLast, self).get_modules(build, machine)
        modules.add_module("bench_delete_last_copy",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_delete_last_copy",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchDeleteLastRemote(DistopsBench):
    '''Benchmark latency of deleting last local copy of capability with remote copies'''
    name = 'distops_bench_delete_last_remote'

    def __init__(self, options):
        super(DistopsBenchDeleteLastRemote, self).__init__(options)
        # make boxplot useful
        self.boxplot_ylim = [0, 100]

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchDeleteLastRemote, self).get_modules(build, machine)
        modules.add_module("bench_delete_last_copy_remote",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_delete_last_copy_remote",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchRevokeNoRemote(DistopsBench):
    '''Benchmark latency of revoking capability with no remote relations'''
    name = 'distops_bench_revoke_no_remote'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchRevokeNoRemote, self).get_modules(build, machine)
        modules.add_module("bench_revoke_no_remote",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_revoke_no_remote",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchRevokeRemoteCopy(DistopsBench):
    '''Benchmark latency of revoking foreign copy of capability'''
    name = 'distops_bench_revoke_remote_copy'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchRevokeRemoteCopy, self).get_modules(build, machine)
        modules.add_module("bench_revoke_remote_copy",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_revoke_remote_copy",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchRevokeWithRemoteCopies(DistopsBench):
    '''Benchmark latency of revoking a capability with remote copies/descendants'''
    name = 'distops_bench_revoke_with_remote_copies'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchRevokeWithRemoteCopies, self).get_modules(build, machine)
        modules.add_module("bench_revoke_with_remote_copies",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_revoke_with_remote_copies",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchRetypeNoRemote(DistopsBench):
    '''Benchmark latency of retyping capability with no remote relations'''
    name = 'distops_bench_retype_no_remote'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchRetypeNoRemote, self).get_modules(build, machine)
        modules.add_module("bench_retype_no_remote",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_retype_no_remote",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchRetypeWithLocalDescs(DistopsBench):
    '''Benchmark latency of retyping capability with local descendants but no remote relations'''
    name = 'distops_bench_retype_with_local_descs'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchRetypeWithLocalDescs, self).get_modules(build, machine)
        modules.add_module("bench_retype_w_local_descendants",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_retype_w_local_descendants",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchRetypeWithRemoteCopies(DistopsBench):
    '''Benchmark latency of retyping capability with remote copies'''
    name = 'distops_bench_retype_with_local_descs'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchRetypeWithRemoteCopies, self).get_modules(build, machine)
        modules.add_module("bench_retype_with_remote_copies",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_retype_with_remote_copies",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchNoopInvocation(DistopsBench):
    '''Benchmark latency of noop invocation'''
    name = 'distops_bench_noop_invocation'

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchNoopInvocation, self).get_modules(build, machine)
        modules.add_module("bench_noop_invocation",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_noop_invocation",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchDeleteCNodeLast(DistopsBench):
    '''Benchmark latency of deleting last copy of cnode with 4 slots occupied'''
    name = 'distops_bench_delete_cnode_last'

    def __init__(self, options):
        super(DistopsBenchDeleteCNodeLast, self).__init__(options)
        # make boxplot useful
        self.boxplot_ylim = [0, 100]

    def setup(self, build, machine, testdir):
        super(DistopsBenchDeleteCNodeLast, self).setup(build, machine, testdir)
        # set timeout for this test to 10min
        self.test_timeout_delta = datetime.timedelta(seconds=20*60)

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchDeleteCNodeLast, self).get_modules(build, machine)
        modules.add_module("bench_delete_cnode_last_copy",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_delete_cnode_last_copy",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules

@tests.add_test
class DistopsBenchDeleteCNodeVary(DistopsBench):
    '''Benchmark latency of deleting cnode with varying number of slots occupied'''
    name = 'distops_bench_delete_cnode_slots_occupied'

    def __init__(self, options):
        super(DistopsBenchDeleteCNodeVary, self).__init__(options)
        # make boxplot useful
        self.boxplot_ylim = [0, 2000]

    def setup(self, build, machine, testdir):
        super(DistopsBenchDeleteCNodeVary, self).setup(build, machine, testdir)
        # XXX: figure out timeout for this test. 24h might not be enough...
        self.test_timeout_delta = datetime.timedelta(seconds=86400)

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBenchDeleteCNodeVary, self).get_modules(build, machine)
        modules.add_module("bench_delete_cnode_last_copy_2",
                           ["core=0", "mgmt", "%d" % (machine.get_ncores()-1)])
        modules.add_module("bench_delete_cnode_last_copy_2",
                           ["core=1-%d" % (machine.get_ncores()-1), "node"])
        return modules
