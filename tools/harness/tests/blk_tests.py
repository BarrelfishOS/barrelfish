##########################################################################
# Copyright (c) 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re, datetime
import debug, tests
from common import TestCommon, TimeoutError
from results import RowResults, PassFailResult

BLK_TEST_TIMEOUT = datetime.timedelta(minutes=8) # XXX: tilsiter1 needs a bit longer (slow write speeds?)

bandwidth = {
    'tilsiter1': {
        'read': {
            512: 32.67,
            1024: 50.08,
            2048: 90.15,
            4096: 154.21,
            8192: 236.92,
            16384: 318.05,
            32768: 379.68,
            65536: 416.99,
            131072: 428.64,
            262144: 439.88,
            524288: 495.27,
            1048576: 528.16,
            2097152: 546.71
        },
        'write': {
            512: 5.41,
            1024: 1.70,
            2048: 3.32,
            4096: 6.68,
            8192: 26.50,
            16384: 49.92,
            32768: 88.74,
            65536: 143.63,
            131072: 212.54,
            262144: 281.08,
            524288: 377.55,
            1048576: 436.48,
            2097152: 49.25,
        },
    },

    'vacherin': {
        'read': {
            512: 15.24,
            1024: 15.35,
            2048: 30.32,
            4096: 57.29,
            8192: 102.01,
            16384: 167.28,
            32768: 237.24,
            65536: 316.74,
            131072: 361.41,
            262144: 405.19,
            524288: 490.97,
            1048576: 415.05,
            2097152: 424.57
        },
        'write': {
            512: 15.20,
            1024: 12.55,
            2048: 26.87,
            4096: 51.89,
            8192: 93.21,
            16384: 152.43,
            32768: 213.55,
            65536: 296.29,
            131072: 365.72,
            262144: 412.98,
            524288: 440.78,
            1048576: 454.21,
            2097152: 462.02,
        }
    },

    'babybel1': {
        'read': {
            512: 43.42,
            1024: 25.56,
            2048: 31.16,
            4096: 55.63,
            8192: 85.02,
            16384: 114.35,
            32768: 105.40,
            65536: 147.15,
            131072: 414.57,
            262144: 482.58,
            524288: 515.73,
            1048576: 534.20,
            2097152: 544.49,
            4194304: 549.79,
        },
        'write': {
            512: 42.52,
            1024: 38.18,
            2048: 41.29,
            4096: 75.26,
            8192: 221.12,
            16384: 267.90,
            32768: 426.09,
            65536: 494.36,
            131072: 516.22,
            262144: 526.34,
            524288: 533.67,
            1048576: 533.67,
            2097152: 522.25,
            4194304: 523.27
        },
    },

    'babybel2': {
        'read': {
            512: 43.32,
            1024: 14.08,
            2048: 31.10,
            4096: 55.14,
            8192: 83.53,
            16384: 108.78,
            32768: 95.90,
            65536: 115.02,
            131072: 159.26,
            262144: 427.62,
            524288: 514.74,
            1048576: 533.14,
            2097152: 503.16
        },
        'write': {
            512: 43.09,
            1024: 22.52,
            2048: 41.00,
            4096: 75.28,
            8192: 220.75,
            16384: 268.17,
            32768: 416.18,
            65536: 497.10,
            131072: 519.22,
            262144: 529.46,
            524288: 534.73,
            1048576: 537.95,
            2097152: 518.22,
        },
    },

    'babybel3': {
        'read': {
            512: 43.53,
            1024: 25.71,
            2048: 26.38,
            4096: 55.21,
            8192: 83.35,
            16384: 107.88,
            32768: 95.66,
            65536: 115.43,
            131072: 159.21,
            262144: 428.30,
            524288: 514.24,
            1048576: 532.87,
            2097152: 504.82,
        },
        'write': {
            512: 42.86,
            1024: 38.40,
            2048: 41.43,
            4096: 75.83,
            8192: 225.01,
            16384: 265.78,
            32768: 428.13,
            65536: 495.27,
            131072: 513.26,
            262144: 528.42,
            524288: 532.61,
            1048576: 532.61,
            2097152: 525.31,
        }
    },

    'babybel4': {
        'read': {
            512: 43.46,
            1024: 25.73,
            2048: 31.17,
            4096: 55.25,
            8192: 83.51,
            16384: 108.35,
            32768: 95.72,
            65536: 115.08,
            131072: 159.78,
            262144: 381.98,
            524288: 492.54,
            1048576: 512.53,
            2097152: 451.34
        },
        'write': {
            512: 42.49,
            1024: 38.40,
            2048: 41.47,
            4096: 76.96,
            8192: 225.20,
            16384: 265.51,
            32768: 430.19,
            65536: 496.18,
            131072: 517.22,
            262144: 529.46,
            524288: 534.73,
            1048576: 534.73,
            2097152: 524.29
        }
    }
}

class BlkTests(TestCommon):

    def __init__(self, options):
        super(BlkTests, self).__init__(options)

    def get_module_name(self):
        return "ahci_test"

    def boot(self, *args):
        super(BlkTests, self).boot(*args)
        self.set_timeout(BLK_TEST_TIMEOUT)

    def get_modules(self, build, machine):
        self.machine = machine.name
        modules = super(BlkTests, self).get_modules(build, machine)
        modules.add_module_arg("kaluga","add_device_db=device_db_ahcitest")
        modules.add_module(self.get_module_name(), ["auto", self.OP])

        return modules

    def get_finish_string(self):
        return "AHCI testing completed."

    def process_data(self, testdir, rawiter):
        self.regex = re.compile(self.REGEX)
        result = RowResults(['op', 'buffer', 'block', 'bandwidth'])
        if not bandwidth.has_key(self.machine):
            result.mark_failed('No data about this disk, please set the initial performance values.')
            return result

        matches = 0
        num_fail = 0
        for line in rawiter:
            match = self.regex.match(line)
            if match:
                matches += 1

                buffer_size, bs, bw = match.groups()
                buffer_size = int(buffer_size)
                bs = int(bs)
                bw = float(bw)
                operation = self.OP.lower()
                if not bandwidth[self.machine].has_key(operation):
                    result.mark_failed('No data about this benchmark, please set the initial performance values.')
                    return result
                if not bandwidth[self.machine][operation].has_key(bs):
                    result.mark_failed('No data for {} with bs {}.'.format(operation, bs))
                    return result

                lower_bound = bandwidth[self.machine][operation][bs] * (1 - 0.25)
                upper_bound = bandwidth[self.machine][operation][bs] * (1 + 0.25)

                result.add_row((operation, buffer_size, bs, bw))
                if bw <= lower_bound:
                    error = "{} for {} bytes blocks not within expected range (was {}, should be >= {}).".format(operation, bs, bw, lower_bound)
                    debug.log(error)
                    num_fail+= 1;
                    if num_fail > 1:
                        result.mark_failed(reason=error)
                elif bw >= upper_bound:
                    error = "Achieved {} bandwidth for {} bytes blocks was better ({}) than expected ({}).".format(operation, bs, bw, upper_bound)
                    debug.log(error)
                    debug.log("This is good, if you can explain it! Adjust the bandwidth numbers in blk_tests.py and re-run the test.")
                    num_fail+= 1
                    if num_fail > 1:
                        result.mark_failed(reason=error)
                else:
                    pass

            if line.startswith("AHCI testing completed.") and matches > 0:
                return result

        result.mark_failed('Did not see end of test or got no bandwidth numbers.')
        return result

@tests.add_test
class BlkAhciWriteBWTest(BlkTests):
    ''' AHCI Driver Write Bandwidth Test'''
    name = "blk_read_test"
    OP = "read"
    REGEX = r"\[ahci_perf_sequential\] Read sequential size (\d+) bs (\d+): (\d+\.\d+) \[MB/s\]"

@tests.add_test
class BlkAhciReadBWTest(BlkTests):
    ''' AHCI Driver Read Bandwidth Test'''
    name = "blk_write_test"
    OP = "write"
    REGEX = r"\[ahci_perf_sequential\] Write sequential size (\d+) bs (\d+): (\d+\.\d+) \[MB/s\]"

@tests.add_test
class BlkAhciVerifyTest(BlkTests):
    ''' AHCI Driver Correctness test '''
    name = "blk_verify_test"
    OP = "verify"
    REGEX = r"\[ahci_verify_sequential\] SUCCESS \((\d+) (\d+)\)"
    TESTS = 14

    def process_data(self, testdir, rawiter):
        self.regex = re.compile(self.REGEX)

        matches = 0
        for line in rawiter:
            match = self.regex.match(line)
            if match:
                matches += 1

        if matches == self.TESTS:
            return PassFailResult(True)
        elif matches < self.TESTS:
            return PassFailResult(False, "Some block/buffer size checks did not report back with SUCCESS.")
        elif matches > self.TESTS:
            return PassFailResult(False, "Got more SUCCESS lines than expected. If you changed the test you may need to increase self.TESTS.")
        else:
            assert "Should not come here"
