import tests, debug
from common import TestCommon
from results import PassFailResult, RowResults
import sys, re, numpy, os, copy

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
            debug.verbose(">>> nodedata[1].keys = %r" % self.nodedata[1].keys())
            xlabels = plt.setp(ax, xticklabels=sorted(self.nodedata[1].keys()))
            plt.setp(xlabels, rotation=90)
        # TODO: think about using numpy.percentile to figure out sensible ylim
        if ylim is not None:
            ax.set_ylim(ylim)
        plt.tight_layout()
        plt.savefig('%s.pdf' % outfile, format='pdf')

    def boxplot(self, outfile, all_nodes=False, xlabel=None, ylabel=None, ylim=None):
        fig = plt.figure()
        nodecount = self.nodecount
        if not all_nodes:
            nodecount=1
        for nodeid in xrange(1, nodecount+1):
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

@tests.add_test
class DistopsBenchDeleteForeign(TestCommon):
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

    def get_finish_string(self):
        return "# Benchmark done!"

    def process_data(self, testdir, rawiter):
        debug.verbose("Processing data for %s" % self.name)
        plot = DistopsPlot(self.machine)
        debug.verbose(">>> Reading data for plotting")
        passed = plot.read_data(rawiter)
        plotf = "%s/boxplot_%s" % (testdir, self.name)
        debug.verbose(">>> Saving boxplot to %s.pdf" % plotf)
        plot.boxplot(plotf)
        plotf = "%s/plot_%s" % (testdir, self.name)
        debug.verbose(">>> Saving plot to %s.pdf" % plotf)
        plot.plot(plotf)
        return PassFailResult(passed)
