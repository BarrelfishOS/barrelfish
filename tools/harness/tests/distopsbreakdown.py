import tests, debug
from common import TestCommon
from results import RowResults
import sys, os, socket

import siteconfig

class OpBreakdown(object):
    def __init__(self, start_ev, expected_end_evname, evcmp):
        self._start_ts = start_ev._timestamp
        self._seqnum = start_ev._arg
        self._events = [ start_ev ]
        self._expected_end_evname = expected_end_evname
        self._sealed = False
        self._event_comparator = evcmp

    @property
    def seqnum(self):
        return self._seqnum

    @property
    def last_event(self):
        # this should not be necessary...
        #l = sorted(self._events, cmp=cmp_events_by_timestamp, reverse=True)[0]
        return self._events[-1]

    def append_event(self, ev):
        if self._sealed:
            print "Trying to append event to sealed breakdown"
            return
        self._events.append(ev)
        if ev._evname == self._expected_end_evname:
            self._events = sorted(self._events, cmp=self._event_comparator)
            self._sealed = True


    def generate_breakdown_data(self):
        '''Generator that produces a tuple (seqnum,eventname,tscoff,core,evarg) for each
        chunk of work done inside the monitor for a delete call'''
        for e in self._events:
            yield (self._seqnum, e._evname, e._timestamp - self._start_ts, e._coreid, e._arg)

    def compute_overall_latency(self):
        return self.last_event()._timestamp - self._start_ts

    def __str__(self):
        evdata = [ (ev._coreid, ev._evname, ev._timestamp) for ev in self._events ]
        return "Breakdown { startts %d, seqnum %d, events=%r }" \
                % (self._start_ts, self._seqnum, evdata)

class DistopsBreakdownTraceParser(object):

    def __init__(self, pyaquariumpath, tracedefs):
        if pyaquariumpath not in sys.path:
            sys.path.append(pyaquariumpath)
        import aquarium
        self.aq = aquarium.Aquarium(tracedefs)
        import trace_parser
        self.evcmp = trace_parser.cmp_events_by_timestamp

    def _event_filter(self, ev):
        name = ev.subsys.get_name()
        return name == "capops" or name == "kernel_capops"

    def parse_trace_file(self, tracef):
        self.trace = self.aq.load_trace(tracef)

    def process_trace(self, start_evname, end_evname):
        t = self.trace

        evtypes = self.aq.get_event_types()
        overall_del_lats=dict()
        found_start = False

        # XXX: this is not very nice, as we're flattening a partial order by hand
        # here in order to make queries about event ordering to skip partially
        # recorded inner trace points that don't carry the sequence number yet :)
        #
        # 2017-11-01: this seems to be unnecessary, as trace points are
        # recorded in-order anyway.
        # event_order = [ 'user_delete_call', 'delete_enter', 'try_delete',
        #         'has_copies', 'cleanup_copy', 'cleanup_last', 'unmap_capability',
        #         'mdb_remove', 'mdb_rebalance', 'mdb_update_end', 'create_ram',
        #         'create_ram_lmp', 'delete_done', 'user_delete_resp' ]

        # current breakdown object indexed by coreid
        currbreak = dict()
        # list of complete breakdown objects indexed by coreid
        breakdowns = dict()

        # we seem to get better results without presorting events by
        # timestamp, so we leave that out for now; events should be sorted by
        # (coreid, timestamp) anyway.
        for e in [ e for e in t._events if self._event_filter(e) ]:
            # find START signal
            if e._evname == "start":
                found_start = True
            if not found_start:
                # ignore events before start event
                continue

            # start_evname is signalling start of new operation.
            if e._evname == start_evname:
                currbreak[e._coreid] = OpBreakdown(e, end_evname, self.evcmp)
                if e._coreid not in breakdowns.keys():
                    breakdowns[e._coreid] = []

            # end_evname is signalling end of operation.
            # just skip end_evname events for which we're not tracking a breakdown
            elif e._evname == end_evname and e._coreid in currbreak.keys():
                if e._arg != currbreak[e._coreid].seqnum:
                    print "[core %d] got end event with seqnum %d, last start event was %d" \
                            % (e._coreid, e._arg, currbreak[e._coreid].seqnum)
                    print "skipping this set of trace points"
                else:
                    currbreak[e._coreid].append_event(e)
                    breakdowns[e._coreid].append(currbreak[e._coreid])
                    # remove breakdown object for e._coreid from currbreak dict,
                    # so other code can check whether we're in the middle of a breakdown
                    # by checking whether the coreid is in the keyset of the dict
                    del currbreak[e._coreid]
            elif e._coreid in currbreak.keys():
                currbreak[e._coreid].append_event(e)

        return breakdowns

class DistopsBreakdown(TestCommon):
    name = None
    binary_name = None
    start_evname = None
    end_evname = None

    '''Base class for common code for distops breakdown benchmarks'''
    def __init__(self, options):
        assert(self.name is not None)
        super(DistopsBreakdown, self).__init__(options)
        self.nfsip = socket.gethostbyname(siteconfig.get('WEBSERVER_NFS_HOST'))
        self.nfspath = siteconfig.get('BFSCOPE_NFS_TRACE_DIR')
        self.tracefile = "%s.trace" % self.name
        print options
        self.builddir = options.buildbase
        self.buildarch = None
        if options.machines:
            self.buildarch = options.machines[0].get_buildarchs()[0]
        elif options.arch:
            self.buildarch = options.arch
        self.tracedefs = os.path.join(self.builddir, self.buildarch,
                                      "trace_definitions", "trace_defs.json")
        self.pyaquarium = os.path.join(options.sourcedir, "tools", "pyaquarium")


    def get_finish_string(self):
        return "# Benchmark done!"

    def _get_trace_file(self, testdir):
        # get trace file from emmentaler1
        from paramiko import SSHClient
        from paramiko.client import WarningPolicy
        from scp import SCPClient, SCPException
        ssh = SSHClient()
        ssh.load_system_host_keys()
        # don't care about missing host keys
        ssh.set_missing_host_key_policy(WarningPolicy())
        ssh.connect(siteconfig.get('NFS_SSH_HOST'),
                port=siteconfig.get('NFS_SSH_PORT'))
        scp = SCPClient(ssh.get_transport())
        localpath = os.path.join(testdir, self.tracefile)
        try:
            scp.get(os.path.join(self.nfspath, self.tracefile),
                    local_path=localpath)
        except SCPException, e:
            print "scp exception:", e
            return None
        return localpath

    def process_data(self, testdir, rawiter):
        assert(self.start_evname is not None)
        assert(self.end_evname is not None)
        debug.verbose("Processing data for %s" % self.name)
        results = RowResults(['core', 'seqnum', 'event', 'latency',
                              'event core', 'event arg'])

        debug.verbose("Copying trace from NFS server")
        tracef = self._get_trace_file(testdir)

        self.traceprocessor = DistopsBreakdownTraceParser(self.pyaquarium,
                self.tracedefs)
        debug.verbose("Parsing trace file")
        self.traceprocessor.parse_trace_file(tracef)
        debug.verbose("Processing trace")
        breakdowns = self.traceprocessor.process_trace(
                self.start_evname, self.end_evname)

        debug.verbose("Creating results")
        for core in breakdowns.keys():
            for b in breakdowns[core]:
                off = 0
                for ev in b._events:
                    off = ev._timestamp - b._start_ts
                    if (off < 0):
                        print "Breakdown has negative components?"
                        print b
                        break
                if off < 0:
                    # don't process breakdowns with negative components
                    # further
                    continue
                for seqnum, evname, tsoff, ecore, earg in b.generate_breakdown_data():
                    results.add_row([core, seqnum, evname, tsoff, ecore, earg])

        return results

    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBreakdown, self).get_modules(build, machine)
        modules.add_module("net_sockets_server", ["auto"])
        # dump trace via nfs to /nfspath/<benchname>.trace
        modules.add_module("bfscope_nfs", [ "core=1", "nfs://" + self.nfsip +
            self.nfspath, os.path.join("/bfscope", self.tracefile) ])
        if self.binary_name is not None:
            modules.add_module(self.binary_name,
                               ["core=2", "mgmt", "%d" % 3])
            modules.add_module(self.binary_name, ["core=3-5", "node"])
        return modules

@tests.add_test
class DistopsBreakdownDeleteLocal(DistopsBreakdown):
    '''Breakdown latency benchmark for deleting local copy'''
    name = 'distops_breakdown_delete_local'
    binary_name = 'bench_delete_local_copy'
    start_evname = "user_delete_call"
    end_evname = "user_delete_resp"

@tests.add_test
class DistopsBreakdownDeleteForeign(DistopsBreakdown):
    '''Benchmark latency of deleting foreign copy of capability'''
    name = 'distops_breakdown_delete_foreign'
    binary_name = "bench_delete_foreign_copy"
    start_evname = "user_delete_call"
    end_evname = "user_delete_resp"

@tests.add_test
class DistopsBreakdownDeleteLast(DistopsBreakdown):
    '''Benchmark latency of deleting last copy of capability'''
    name = 'distops_breakdown_delete_last'
    binary_name = "bench_delete_last_copy"
    start_evname = "user_delete_call"
    end_evname = "user_delete_resp"

@tests.add_test
class DistopsBreakdownDeleteLastRemote(DistopsBreakdown):
    '''Benchmark latency of deleting last local copy of capability with remote copies'''
    name = 'distops_breakdown_delete_last_remote'
    binary_name = "bench_delete_last_copy_remote"
    start_evname = "user_delete_call"
    end_evname = "user_delete_resp"

@tests.add_test
class DistopsBreakdownRevokeNoRemote(DistopsBreakdown):
    '''Breakdown latency benchmark for revoking cap w/o remote relations'''
    name = 'distops_breakdown_revoke_no_remote'
    start_evname = "user_revoke_call"
    end_evname = "user_revoke_resp"

    # Use standalone version of benchmark, as distributed version has some bug
    def get_modules(self, build, machine):
        self.machine = machine.get_machine_name()
        modules = super(DistopsBreakdownRevokeNoRemote, self).get_modules(build, machine)
        modules.add_module("distops_standalone_runner",
                           ["core=2", "bench_revoke_no_remote_standalone" ] +
                            ("%d %d %d %d" % (3, 3, 4, 5)).split(" "))
        modules.add_module("bench_revoke_no_remote_standalone", ["nospawn"])
        return modules
