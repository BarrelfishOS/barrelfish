##########################################################################
# Copyright (c) 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
import re
from common import TestCommon
from results import PassFailResult

distops_tests = [
        { "testname": "retype",
          "finish_string": "distops_retype: test done",
          "error_regex": "^.*distops_retype: .* expected .*$" 
        },
        { "testname": "delete",
          "finish_string": "distops_delete: test done",
          "error_regex": "^.*distops_delete: .* failed: .*$",
        },
        { "testname": "revoke",
          "finish_string": "distops_revoke: test done",
          "error_regex": "^.*distops_revoke: .* expected .*$",
        },
]

def dist_test_factory(testname, finish_string, error_regex):
    class DistTest(TestCommon):
        name = "distops_%s" % testname

        def get_modules(self, build, machine):
            modules = super(DistTest, self).get_modules(build, machine)
            modules.add_module("test_remote_%s" % testname, [ "core=0", "server" ])
            modules.add_module("test_remote_%s" % testname, [ "core=1", "client" ])
            return modules

        def get_finish_string(self):
            return finish_string

        def process_data(self, testdir, rawiter):
            # the test passed iff we do not find a line matching the given
            # error regex
            error_re = re.compile(error_regex)
            passed = True
            found_test = False
            for line in rawiter:
                if error_re.match(line):
                    passed = False
                if self.get_finish_string() in line:
                    found_test = True
            return PassFailResult(found_test and passed)
    return DistTest

for t in distops_tests:
    klass = dist_test_factory(**t)
    tests.add_test(klass)
