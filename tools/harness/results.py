##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

from stats import Stats

class ResultsBase(object):
    def __init__(self, name=None, reason=""):
        self.name = name
        self.fail_reason = reason

    def reason(self):
        return self.fail_reason

    def passed(self):
        """Returns true iff the test is considered to have passed."""
        raise NotImplementedError

    def to_file(self, filehandle):
        """May be used to write formatted results to a file."""
        raise NotImplementedError


class PassFailResult(ResultsBase):
    """Stores results of test that is purely pass/fail."""
    def __init__(self, passed, reason=""):
        super(PassFailResult, self).__init__(reason=reason)
        self.passfail = passed

    def passed(self):
        return self.passfail


class PassFailMultiResult(ResultsBase):
    def __init__(self, name, errors=[]):
        self.errors = errors
        self.name = name

    def reason(self):
        return str(self.errors)

    def passed(self):
        return len(self.errors) == 0

    def to_file(self, fh):
        if len(self.errors) > 0:
            fh.write('%d error(s) in %s\n' % (len(self.errors), self.name))
            for error in self.errors:
                fh.write(error)

class RowResults(ResultsBase):
    """Results class that maintains numeric values grouped by rows.
    """
    def __init__(self, colnames, name=None):
        super(RowResults, self).__init__(name)
        self.colnames = colnames
        self.rows = []
        self.failed = False

    def passed(self):
        if self.failed:
            return False
        return self.rows != []

    def to_file(self, fh):
        """Produce a file in a format suitable for gnuplot."""
        fh.write('# %s\n' % '\t'.join(self.colnames))
        for r in self.rows:
            fh.write('\t'.join(map(str, r)) + '\n')

    def mark_failed(self, reason):
        """Mark this test as having failed."""
        self.failed = True
        self.fail_reason = reason

    def add_row(self, row):
        assert(len(row) == len(self.colnames))
        self.rows.append(row)


class RawResults(RowResults):
    """Results class suitable for processing statistics on raw data
    (eg. microbenchmarks where we have each individual measurement)"""

    def __init__(self, groupheader, name=None):
        headers = 'nvalues median mean stddev min max'.split()
        super(RawResults, self).__init__([groupheader] + headers, name=name)

    def add_group(self, identifier, data):
        st = Stats(data)
        self.add_row([identifier, st.nvalues, st.median, st.mean, st.stddev,
                      st.minimum, st.maximum])
