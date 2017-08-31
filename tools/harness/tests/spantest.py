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
import debug

from common import TestCommon, TimeoutError
from results import PassFailResult

MATCH = 'spantest.*Done.*cycles'

@tests.add_test
class SpanTest(TestCommon):
    '''Span a program on the twice each on the first and second core'''
    name = "spantest"

    def setup(self, build, machine, testdir):
        super(SpanTest, self).setup(build, machine, testdir)

    def get_modules(self, build, machine):
        modules = super(SpanTest, self).get_modules(build, machine)
        # span on all cores other than 0 -- matches spantest code
        modules.add_module("spantest", [ machine.get_ncores() - 1 ])
        return modules

    def is_finished(self, line):
        return re.match(MATCH, line) or super(SpanTest, self).is_finished(line)

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

    def get_modules(self, build, machine):
        modules = super(SpanTestInterleaved, self).get_modules(build, machine)
        # span on all cores other than 0 -- matches spantest code
        modules.add_module("tests/span-interleaved", [ machine.get_ncores() ])
        return modules

    def get_finish_string(self):
        return 'SPAN_TEST_SUCCESS.'

    def process_data(self, testdir, rawiter):
        result = False
        for line in rawiter:
            if re.search('SPAN_TEST_SUCCESS.', line) :
                result = True
        return PassFailResult(result)

@tests.add_test
class SpanTestExit(TestCommon):
    '''Span a program then exit and see if other dispatchers cleanup'''
    name = "spantest_exit"
    
    is_done = False
    
    def setup(self, build, machine, testdir):
        super(SpanTestExit, self).setup(build, machine, testdir)

    def get_modules(self, build, machine):
        modules = super(SpanTestExit, self).get_modules(build, machine)
        # span on all cores other than 0 -- matches spantest code
        modules.add_module("tests/span-exit", [ machine.get_ncores() ])
        return modules

    def is_finished(self, line):
        if re.search('SPAN_TEST_DONE.', line) :
            self.is_done = True
        return re.match('kernel [0-9]*: user page fault WHILE DISABLED', line) or \
                super(SpanTestExit, self).is_finished(line)

    def process_data(self, testdir, rawiter):
        result = True
        for line in rawiter:
            if re.match('kernel [0-9]*: user page fault WHILE DISABLED', line):
                result = False
        return PassFailResult(result)

    def collect_data(self, machine):
        fh = machine.get_output()
        while True:
            try:
                line = self._readline(fh)
            except TimeoutError as e:
                if self.boot_phase:
                    if self.boot_attempts < MAX_BOOT_ATTEMPTS:
                        yield '[Error: boot timed out, retry]\n'
                        self.reboot(machine)
                        continue
                    else:
                        yield '[Error: boot timed out, retry limit reached]\n'
                else:
                    yield '[Error: test timed out]\n'
                debug.verbose("timeout encountered in collect_data");
                self.has_timeout = True
                if self.is_done :
                    break
                raise e

            yield line

            if not self.boot_phase:
                self.process_line(line)
                if self.is_finished(line):
                    debug.verbose("is_finished returned true for line %s" % line)
                    break
            elif self.is_booted(line):
                self.boot_phase = False
                self.set_timeout(self.test_timeout_delta)
                self.process_line(line)
