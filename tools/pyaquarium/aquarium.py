# -*- coding: utf-8 -*-
import json
from trace_parser import TraceSubsystem, Trace

class Aquarium(object):

    def __init__(self, json_defs):
        self._trace = None
        with open(json_defs) as f:
            json_data = json.load(f)
            self._event_types = dict()
            for typekey, evtype in json_data.items():
                t = TraceSubsystem(typekey, evtype)
                self._event_types[t._id] = t

    def load_trace(self, tracefile):
        if self._trace is None:
            t = Trace(self._event_types)
            with open(tracefile) as f:
                for line in f:
                    if '\0\0' in line:
                        lines = line.split('\0\0')
                        t.parse_line(lines[1])
                    else:
                        t.parse_line(line)
            self._trace = t

        return self._trace

    def get_event_types(self):
        return self._event_types
