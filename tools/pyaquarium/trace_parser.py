# -*- coding: utf-8 -*-
from inflection import camelize
import numpy

def ev_repr(ev):
    r = "EVENT " + str(ev.id) + ": subsys=" + ev.subsys.get_name() + ", label=" + ev._evname
    if ev.desc != ev._evname:
        r = r + ", desc=" + ev.desc
    r = r + ", arg=" + str(ev._arg)
    return r

def ev_init(ev, coreid, ts, arg, app):
    ev._coreid = coreid
    ev._timestamp = ts
    ev._arg = arg
    ev._application = app

def cmp_events_by_timestamp(ev1, ev2):
    if ev1._timestamp < ev2._timestamp:
        return -1
    if ev1._timestamp > ev2._timestamp:
        return 1
    return 0

class TraceEventType(type):

    def __new__(meta, name, parents, dct):
        dct['__repr__'] = ev_repr
        dct['__init__'] = ev_init
        dct['_evname'] = name.lower()
        clsname = "EventType"+camelize(name.lower())
        return super(TraceEventType, meta).__new__(meta, clsname, parents, dct)

def make_trace_event(name, evid, evdesc, subsys):
    return TraceEventType(name.encode('utf-8'), (),
            {'id':evid, 'desc':evdesc, 'subsys':subsys})

class TraceSubsystem(object):

    def __init__(self, id, events):
        self._events = dict()
        self._id = numpy.ushort(id)
        self._name = events.get("name").lower()
        if "events" in events.keys():
            self._parse_from_json(events.get("events"))
        elif self._name == "ump send":
            self._events[0] = make_trace_event("CHANNEL_SEND", 0, "ump send", self)
        elif self._name == "ump receive":
            self._events[0] = make_trace_event("CHANNEL_RECV", 0, "ump send", self)

    def _parse_from_json(self, json):
        for evid, event in json.items():
            evid = numpy.ushort(evid)
            self._events[evid] = make_trace_event(event[0], evid, event[1], self)

    def get_events(self):
        return self._events

    def get_name(self):
        return self._name

    def __repr__(self):
        return "SUBSYSTEM " + self._id + ": " + self._name + ", events: " + self._events.__str__()

class Trace(object):

    def __init__(self, event_types):
        # store parsed event type objects
        self._event_types = event_types
        # map from application id (DCB addr) to application name
        self._applications = dict()
        # This map stores for each core_ID which application is runnig at a given
        # time. It maps core_ID => timestamp =>> applicationName.
        self._running_applications = dict()
        # keep track of timestamp offsets of cores relative to core 0
        self._core_offsets = dict()
        # First recorded timestamp, subtract from everything, so traces start at 0
        self._first_timestamp = -1
        # array of events
        self._events = []

    def _parse_meta_line(self, line):
        """Meta lines are prefixed with # and can have different formats, we
        distinguish formats by looking at the first space-delimited word in a
        meta line"""
        elems = line.split(' ')
        if len(elems) < 2:
            return
        key = elems[1].lower()
        if key == "min_timestamp":
            self._first_timestamp = int(elems[2])
        if key == "core":
            # clear list of applications
            self._applications = dict()
        if key == "dcb":
            appid = int(elems[3], 16) & 0xFFFFFFFF
            self._applications[appid] = elems[4]
        if key == "offset":
            coreid = int(elems[2])
            if coreid not in self._core_offsets.keys():
                self._core_offsets[coreid] = int(elems[3])

    def _get_app_for_ts(self, ts):
        if ts not in self._running_applications:
            # find highest entry in _running_applications map with timestamp < ts
            currt = -1
            for t in sorted(self._running_applications.keys()):
                if t > ts:
                    break
                currt = t
            assert currt < ts
            return self._running_applications[currt]
        else:
            return self._running_applications[ts]

    def _make_event(self, coreid, ts, subsys, ev, arg):
        events = self._event_types[subsys]

        if events.get_name() == "ump send" or events.get_name() == "ump receive":
            data = ((subsys << 48) | (ev << 32) | arg)
            # data in ump send/recv events is in bits 12-55
            arg = data & 0x00FFFFFFFFFFF000
            # Only event in those classes is 0
            ev = 0

        if ev not in events.get_events().keys():
            print "skipping event with unknown type %d" % ev
            return None
        evtype = events.get_events()[ev]
        app = "Unknown application"

        if events.get_name() == "kernel" and evtype.__dict__['_evname'] == "cswitch":
            if arg in self._applications.keys():
                app = self._applications[arg]
        else:
            app = self._running_applications[coreid]

        return evtype(coreid, ts, arg, app)

    def _parse_data(self, data):
        # Data word is 64bits: 63-48 -> subsys, 47-32 -> event, 31-0 -> arg
        arg = data & 0xFFFFFFFF
        ev  = (data >> 32) & 0xFFFF
        sub = (data >> 48) & 0xFFFF
        return (sub, ev, arg)

    def parse_line(self, line):
        line = line.strip()
        if not line:
            raise ValueError("cannot parse None line")
        if '\n' in line:
            raise ValueError("cannot parse multiple lines in one go")
        if len(line) == 0:
            raise ValueError("cannot parse empty line")

        if line.startswith('#'):
            # TODO: parse metaline
            self._parse_meta_line(line)
        else:
            if not ' ' in line:
                print "Skipping line '%s'" % line
                return
            # coreid, and timestamp are base10, data is base16
            elems = line.split(' ')
            coreid = int(elems[0])
            ts = long(elems[1])
            data = int(elems[2], 16)

            ts -= self._first_timestamp

            if coreid not in self._running_applications:
                self._running_applications[coreid] = dict()

            subsys,ev,arg = self._parse_data(data)

            e = self._make_event(coreid, ts, subsys, ev, arg)
            if e is not None:
                if e.subsys.get_name() == "kernel" and e._evname == "cswitch":
                    self._running_applications[coreid][ts] = e._application
                self._events.append(e)

    def get_sorted_events(self, cmpfunc=cmp_events_by_timestamp):
        return sorted(self._events, cmp=cmpfunc)
