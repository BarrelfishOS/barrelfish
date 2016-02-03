##########################################################################
# Copyright (c) 2013, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
##########################################################################

import re
import tests
from common import TestCommon
from results import PassFailResult

MATCH = 'spantest.*Done.*cycles'

@tests.add_test
class SpanTest(TestCommon):
    '''Span a program on the twice each on the first and second core'''
    name = "spantest"

    def setup(self, build, machine, testdir):
        super(SpanTest, self).setup(build, machine, testdir)

        # XXX: track number of cores booted and seen for is_finished()
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(SpanTest, self).get_modules(build, machine)
        # span on all cores other than 0 -- matches spantest code
        modules.add_module("spantest", [ machine.get_ncores() - 1 ])
        return modules

    def is_finished(self, line):
        return re.match(MATCH, line)

    def process_data(self, testdir, rawiter):
        result = False
        for line in rawiter:
            if re.match(MATCH, line):
                result = True
        return PassFailResult(result)

@tests.add_test
class SpanTestInterleaved(TestCommon):
    '''Interleave span and thread create'''
    name = "spantest_interleaved"

    def setup(self, build, machine, testdir):
        super(SpanTestInterleaved, self).setup(build, machine, testdir)

        # XXX: track number of cores booted and seen for is_finished()
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(SpanTestInterleaved, self).get_modules(build, machine)
        # span on all cores other than 0 -- matches spantest code
        modules.add_module("tests/span-interleaved", [ machine.get_ncores() ])
        return modules

    def is_finished(self, line):
        return re.match('span-interleaved.*SUCCESS.*', line)

    def process_data(self, testdir, rawiter):
        result = False
        for line in rawiter:
            if re.match('span-interleaved.*SUCCESS.*', line) :
                result = True
        return PassFailResult(result)

@tests.add_test
class SpanTestExit(TestCommon):
    '''Span a program then exit and see if other dispatchers cleanup'''
    name = "spantest_exit"

    def setup(self, build, machine, testdir):
        super(SpanTestExit, self).setup(build, machine, testdir)

        # XXX: track number of cores booted and seen for is_finished()
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(SpanTestExit, self).get_modules(build, machine)
        # span on all cores other than 0 -- matches spantest code
        modules.add_module("tests/span-exit", [ machine.get_ncores() ])
        return modules

    def is_finished(self, line):
        return re.match('span-exit.*DONE.*', line)

    def process_data(self, testdir, rawiter):
        result = True
        for line in rawiter:
            if re.match("kernel *: user page fault*span-exit*", line):
                result = False
        return PassFailResult(result)
