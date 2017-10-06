#!/usr/bin/env python
# -*- coding: utf-8 -*-

import aquarium
from trace_parser import cmp_events_by_timestamp

class DeleteBreakdown(object):
    def __init__(self, start_ev):
        self._start_ts = start_ev._timestamp
        self._seqnum = start_ev._arg
        self._events = [ start_ev ]

    @property
    def seqnum(self):
        return self._seqnum

    @property
    def last_event(self):
        # this should not be necessary...
        #l = sorted(self._events, cmp=cmp_events_by_timestamp, reverse=True)[0]
        return self._events[-1]

    def append_event(self, ev):
        self._events.append(ev)

    def generate_breakdown_data(self):
        '''Generator that produces a triple seqnum:eventname:tscoff for each
        chunk of work done inside the monitor for a delete call'''
        slices = []
        for e in self._events:
            slices.append(e._timestamp - self._start_ts)
        for v, e in zip(slices, self._events):
            yield "%d:%s:%d" % (self._seqnum, e._evname, v)

    def compute_overall_latency(self):
        return self.last_event()._timestamp - self._start_ts




if __name__ == "__main__":
    import sys
    if len(sys.argv) < 3:
        print "Usage: %s trace_defs.json trace.data" % sys.argv[0]
        sys.exit(1)

    aq = aquarium.Aquarium(sys.argv[1])

    t = aq.load_trace(sys.argv[2])
    print len(t._events)
    evtypes = aq.get_event_types()
    curdel_overall_start_ts=dict()
    curdel_overall_seqnum = -1
    curdel_monitor_start_ts=dict()
    overall_del_lats=dict()
    found_start = False

    event_order = [ 'delete_enter', 'delete_lock', 'delete_queue_retry',
            'delete_do_work', 'delete_remote_enq', 'delete_find_core_cont',
            'delete_move_result_cont', 'delete_last',
            'delete_queue_fin', 'delete_call_rx', 'delete_done' ]

    # current breakdown object indexed by coreid
    currbreak = dict()
    # list of complete breakdown objects indexed by coreid
    breakdowns = dict()

    with open("raw_parsed.txt", 'w') as rawf:
        for e in [ e for e in t._events if e.subsys.get_name() == "capops" ]:
            rawf.write("%r,%d,%d\n" % (e,e._coreid,e._timestamp))
            # find START signal
            if e._evname == "start":
                found_start = True
            if not found_start:
                # ignore events before start event
                continue

            # delete_enter is signalling start of new delete in monitor.
            if e._evname == "delete_enter":
                currbreak[e._coreid] = DeleteBreakdown(e)
                if e._coreid not in breakdowns.keys():
                    breakdowns[e._coreid] = []

            # delete_done is signalling end of delete in monitor
            # just skip delete_done events for which we're not tracking a breakdown
            elif e._evname == "delete_done" and e._coreid in currbreak.keys():
                if e._arg != currbreak[e._coreid].seqnum:
                    print "[core %d] found delete_done with seqnum %d, last delete_enter was %d" \
                            % (e._coreid, e._arg, currbreak[e._coreid].seqnum)
                    print "skipping this set of trace points"
                else:
                    currbreak[e._coreid].append_event(e)
                    breakdowns[e._coreid].append(currbreak[e._coreid])
                    # remove breakdown object for e._coreid from currbreak dict,
                    # so other code can check whether we're in the middle of a breakdown
                    # by checking whether the coreid is in the keyset of the dict
                    currbreak[e._coreid] = None
                    del currbreak[e._coreid]
            elif e._evname in event_order and \
                 e._coreid in currbreak.keys():
                if event_order.index(e._evname) > \
                   event_order.index(currbreak[e._coreid].last_event._evname):
                       currbreak[e._coreid].append_event(e)

            # handle trace point before call to cap_delete() in user code
            if e._evname == "user_delete_call":
                curdel_overall_start_ts[e._coreid] = e._timestamp
                curdel_overall_seqnum = e._arg
                if e._coreid not in overall_del_lats.keys():
                    overall_del_lats[e._coreid] = []
            # handle trace point after call to cap_delete() in user code
            if e._evname == "user_delete_resp":
                if curdel_overall_seqnum != e._arg:
                    print "[core %d] got delete_resp with seqnum %d, last delete_call was %d" \
                            % (e._coreid, e._arg & 0xFF, curdel_overall_seqnum & 0xFF)
                    print "skipping this set of trace points"
                else:
                    if e._coreid in curdel_overall_start_ts.keys():
                        overall_del_lats[e._coreid].append(
                                e._timestamp - curdel_overall_start_ts[e._coreid])


    for core in overall_del_lats.keys():
        with open("core%d_overall_latencies.data" % core, 'w') as rawf:
            for v in overall_del_lats[core]:
                rawf.write("%d\n" % v)

    for core in breakdowns.keys():
        print "core %d:" % core
        with open("core%d_monitor_latencies.data" % core, 'w') as rawf:
            for b in breakdowns[core]:
                rawf.write('\n'.join(list(b.generate_breakdown_data())))
                rawf.write('\n')
