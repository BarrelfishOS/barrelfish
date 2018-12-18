import tests, debug
from common import TestCommon
from results import PassFailResult, RowResults
import sys, re, numpy, os, datetime

has_mpl = True
try:
    import matplotlib.pyplot as plt
except:
    has_mpl = False


OPERATIONHEADER = re.compile("^# Benchmarking ([A-Z 0-9]+): nodes=(\d+).*$")
DATAHEADER = re.compile("^# node (\d+): tsc_per_us = (\d+); numcopies = (\d+).*")
DATALINE = re.compile("^\d+$")

def tsc_to_us(ts, tscperus):
    return ts / float(tscperus)

class DistopsPlot(object):
    def __init__(self):
        self.nodecount = -1
        self.nodedata = dict()
        self.tscperus = dict()
        self.operation = "n/a"

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
                    if currnode not in self.nodedata.keys():
                        self.nodedata[currnode] = dict()
                    self.nodedata[currnode][currcopies] = []
                data = DATALINE.match(line)
                if data is not None and currnode > 0:
                    self.nodedata[currnode][currcopies].append(int(line))
        return passed

    def _finalize_plot(self, outfile, xlabel, ylabel, ylim, fix_xticks=False, draw_legend=False):
        if has_mpl:
            plt.xlabel(xlabel if xlabel is not None else '#capabilities on node (over base set of capabilities)')
            plt.ylabel(ylabel if ylabel is not None else r'latency in $\mu s$')
            if draw_legend:
                plt.legend(loc='lower right')
            if fix_xticks:
                ax = plt.gca()
                xlabels = plt.setp(ax,
                        xticklabels=sorted(self.nodedata[self.nodedata.keys()[0]].keys()))
                plt.setp(xlabels, rotation=90)
            # TODO: think about using numpy.percentile to figure out sensible ylim
            if ylim is not None:
                ax.set_ylim(ylim)
            plt.tight_layout()
            debug.verbose(">>> Saving plot to %s.pdf" % outfile)
            plt.savefig('%s.pdf' % outfile, format='pdf')

    def boxplot(self, outfile, all_nodes=False, xlabel=None, ylabel=None, ylim=None):
        if has_mpl:
            # count #nodes with data
            nodecount = len(self.tscperus.keys())
            assert(nodecount >= 1)
            if not all_nodes:
                nodecount=1
            count = 0
            for nodeid in self.tscperus.keys():
                fig = plt.figure()
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
                self._finalize_plot("%s_node%d" % (outfile, nodeid), xlabel, ylabel, ylim, fix_xticks=True)
                count=count+1
                if count == nodecount:
                    break


    def plot(self, outfile, xlabel=None, ylabel=None, ylim=None):
        if has_mpl:
            sym = [ '', 'x', 'o', '*' ]
            fig = plt.figure()
            nodedata = self.nodedata
            for nodeid in nodedata.keys():
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

    def get_row_results(self, name):
        results = RowResults(['nodeid', 'mdbsize', 'oplatency'])
        for nodeid in sorted(self.nodedata.keys()):
            if nodeid not in self.tscperus.keys():
                # skip nodes that aren't running benchmark for some cases
                continue
            for d in sorted(self.nodedata[nodeid].keys()):
                for v in self.nodedata[nodeid][d]:
                    results.add_row([nodeid, d, v])
        return results

class DistopsBench(TestCommon):
    name = None
    binary_name = None

    '''Base class for common code for distops benchmarks'''
    def __init__(self, options):
        super(DistopsBench, self).__init__(options)
        self.xlabel = None
        self.ylabel = None
        self.plot_ylim = None
        self.boxplot_ylim = None
        self.boxplot_all_nodes = True

    def get_finish_string(self):
        return "# Benchmark done!"

    def process_data(self, testdir, rawiter):
        debug.verbose("Processing data for %s" % self.name)
        plot = DistopsPlot()
        debug.verbose(">>> Reading data for plotting")
        passed = plot.read_data(rawiter)
        plotf = "%s/boxplot_%s" % (testdir, self.name)
        plot.boxplot(plotf, all_nodes=self.boxplot_all_nodes,
                     xlabel=self.xlabel, ylabel=self.ylabel,
                     ylim=self.boxplot_ylim)
        plotf = "%s/plot_%s" % (testdir, self.name)
        plot.plot(plotf, ylabel=self.ylabel, ylim=self.plot_ylim)
        return plot.get_row_results(self.name)

    def get_modules(self, build, machine):
        assert(self.binary_name is not None)
        modules = super(DistopsBench, self).get_modules(build, machine)
        modules.add_module(self.binary_name,
                           ["core=0", "mgmt", "%d" % 3])
        modules.add_module(self.binary_name, ["core=1-2", "node"])
        modules.add_module(self.binary_name,
                ["core=%d" % (machine.get_ncores()-1), "node"])
        return modules


@tests.add_test
class DistopsBenchDeleteForeign(DistopsBench):
    '''Benchmark latency of deleting foreign copy of capability'''
    name = 'bench_distops_delete_foreign'
    binary_name = "bench_delete_foreign_copy"

@tests.add_test
class DistopsBenchDeleteLocal(DistopsBench):
    '''Benchmark latency of deleting local copy of capability'''
    name = 'bench_distops_delete_local'
    binary_name = "bench_delete_local_copy"

@tests.add_test
class DistopsBenchDeleteLast(DistopsBench):
    '''Benchmark latency of deleting last copy of capability'''
    name = 'bench_distops_delete_last'
    binary_name = "bench_delete_last_copy"

@tests.add_test
class DistopsBenchDeleteLastRemote(DistopsBench):
    '''Benchmark latency of deleting last local copy of capability with remote copies'''
    name = 'bench_distops_delete_last_remote'
    binary_name = "bench_delete_last_copy_remote"

    def __init__(self, options):
        super(DistopsBenchDeleteLastRemote, self).__init__(options)
        # make boxplot useful
        self.boxplot_ylim = [0, 100]

@tests.add_test
class DistopsBenchRevokeNoRemote(DistopsBench):
    '''Benchmark latency of revoking capability with no remote relations'''
    name = 'bench_distops_revoke_no_remote'
    binary_name = "bench_revoke_no_remote"

    def setup(self, build, machine, testdir):
        super(DistopsBenchRevokeNoRemote, self).setup(build, machine, testdir)
        # set timeout for this test to 20min
        self.test_timeout_delta = datetime.timedelta(seconds=20*60)

    # Use standalone version of benchmark, as distributed version has some bug
    def get_modules(self, build, machine):
        modules = super(DistopsBench, self).get_modules(build, machine)
        modules.add_module("distops_standalone_runner",
                           ["core=0", "bench_revoke_no_remote_standalone" ] +
                            ("%d %d %d %d" % (3, 1, 2, machine.get_ncores()-1)).split(" "))
        modules.add_module("bench_revoke_no_remote_standalone", ["nospawn"])
        return modules

@tests.add_test
class DistopsBenchRevokeRemoteCopy(DistopsBench):
    '''Benchmark latency of revoking foreign copy of capability'''
    name = 'bench_distops_revoke_remote_copy'
    binary_name = "bench_revoke_remote_copy"

@tests.add_test
class DistopsBenchRevokeWithRemoteCopies(DistopsBench):
    '''Benchmark latency of revoking a capability with remote copies/descendants'''
    name = 'bench_distops_revoke_with_remote_copies'
    binary_name = "bench_revoke_with_remote_copies"

@tests.add_test
class DistopsBenchRetypeNoRemote(DistopsBench):
    '''Benchmark latency of retyping capability with no remote relations'''
    name = 'bench_distops_retype_no_remote'
    binary_name = "bench_retype_no_remote"

@tests.add_test
class DistopsBenchRetypeWithLocalDescs(DistopsBench):
    '''Benchmark latency of retyping capability with local descendants but no remote relations'''
    name = 'bench_distops_retype_with_local_descs'
    binary_name = "bench_retype_w_local_descendants"

@tests.add_test
class DistopsBenchRetypeWithRemoteCopies(DistopsBench):
    '''Benchmark latency of retyping capability with remote copies'''
    name = 'bench_distops_retype_with_remote_copies'
    binary_name = "bench_retype_with_remote_copies"

@tests.add_test
class DistopsBenchNoopInvocation(DistopsBench):
    '''Benchmark latency of noop invocation'''
    name = 'bench_distops_noop_invocation'
    binary_name = "bench_noop_invocation"

@tests.add_test
class DistopsBenchDeleteCNodeLast(DistopsBench):
    '''Benchmark latency of deleting last copy of cnode with 4 slots occupied'''
    name = 'bench_distops_delete_cnode_last'
    binary_name = "bench_delete_cnode_last_copy"

    def __init__(self, options):
        super(DistopsBenchDeleteCNodeLast, self).__init__(options)
        # make boxplot useful
        self.boxplot_ylim = [0, 100]

    def setup(self, build, machine, testdir):
        super(DistopsBenchDeleteCNodeLast, self).setup(build, machine, testdir)
        # set timeout for this test to 20min
        self.test_timeout_delta = datetime.timedelta(seconds=20*60)

@tests.add_test
class DistopsBenchDeleteCNodeVary(DistopsBench):
    '''Benchmark latency of deleting cnode with varying number of slots occupied'''
    name = 'bench_distops_delete_cnode_slots_occupied'
    # not setting binary_name, as we're using a custom get_modules() for this
    # test

    def __init__(self, options):
        super(DistopsBenchDeleteCNodeVary, self).__init__(options)
        # make boxplot useful
        self.boxplot_ylim = [0, 1000]
        self.xlabel = "#occupied slots in CNode"

    def setup(self, build, machine, testdir):
        super(DistopsBenchDeleteCNodeVary, self).setup(build, machine, testdir)
        # XXX: figure out timeout for this test. 24h might not be enough...
        self.test_timeout_delta = datetime.timedelta(seconds=86400)

    def get_modules(self, build, machine):
        modules = super(DistopsBench, self).get_modules(build, machine)
        modules.add_module("bench_delete_cnode_last_copy_2",
                           ["core=0", "mgmt", "1"])
        modules.add_module("bench_delete_cnode_last_copy_2",
                           ["core=1", "node"])
        return modules
