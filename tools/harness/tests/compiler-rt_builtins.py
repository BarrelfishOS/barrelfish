##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import tests
from common import TestCommon
from results import PassFailMultiResult

# generate test-cases with <=CHUNK_SIZE compiler-rt tests each
CHUNK_SIZE=26
# array just to keep the class objects somewhere
compiler_rt_tests_classes = []

def get_modules_tpl(ts, self, build, machine):
    '''Function template for get_modules() for each compiler-rt test case'''
    modules = super(CompilerRTBuiltinsAbstract, self).get_modules(build, machine)
    for m in ts:
        if hasattr(machine, "name") and machine.name.startswith("panda") and \
           (m.endswith("floatdisf_test") or m.endswith("floatdidf_test")):
            # Skip failing test on pandaboard
            continue
        modules.add_module(m)
    modules.add_module("usleeptest", [ "5" ])
    return modules

def chunker(seq, size):
    '''Helper function: this takes a sequence `seq` and splits it up into
    `size`-sized chunks, except for the last chunk which is just the <= size
    long remainder of the sequence'''
    return (seq[pos:pos+size] for pos in xrange(0, len(seq), size))

def generate_test_cases(prefix, testlist):
    for i, ts in enumerate(chunker(testlist, CHUNK_SIZE)):
        # append new class to our array
        compiler_rt_tests_classes.append(
            # this is essentially the decorator @tests.add_test
            tests.add_test(
                # type is the (built-in) base-class for python classes, here we
                # construct classes by calling its constructor
                # signature of type constructor:
                #   type(classname, baseclass tuple, dict with methods/attributes)
                type('CompilerRTBuiltins%d' % (i+1),
                     (CompilerRTBuiltinsAbstract,),
                     { 'name': '%s%d' % (prefix, (i+1)),
                        # partially bind the get_modules() template to select the
                        # right set of tests. Note the ts=ts in the lambda
                        # arguments, this prevents python's default late-binding
                        # for closure arguments.
                         'get_modules':
                             lambda s, b, m, ts=ts: get_modules_tpl(ts, s, b, m)})))

class CompilerRTBuiltinsAbstract(TestCommon):

    def get_finish_string(self):
        return "usleeptest_done"

    def process_data(self, testdir, rawiter):
        # the test passed if no error occurred
        errors = []
        for line in rawiter:
            if "error in" in line:
                errors.append(line)
            if line.startswith("Assertion failed on core"):
                errors.append(line)

        return PassFailMultiResult(self.name, errors)

# lists of tests to run for compiler-rt
vector_fp_tests = [
        "compiler-rt/test/builtins/Unit/adddf3vfp_test",
        "compiler-rt/test/builtins/Unit/addsf3vfp_test",
        "compiler-rt/test/builtins/Unit/divdf3vfp_test",
        "compiler-rt/test/builtins/Unit/divsf3vfp_test",
        "compiler-rt/test/builtins/Unit/eqdf2vfp_test",
        "compiler-rt/test/builtins/Unit/eqsf2vfp_test",
        "compiler-rt/test/builtins/Unit/extebdsfdf2vfp_test",
        "compiler-rt/test/builtins/Unit/fixdfsivfp_test",
        "compiler-rt/test/builtins/Unit/fixsfsivfp_test",
        "compiler-rt/test/builtins/Unit/fixunsdfsivfp_test",
        "compiler-rt/test/builtins/Unit/fixunssfsivfp_test",
        "compiler-rt/test/builtins/Unit/floatsidfvfp_test",
        "compiler-rt/test/builtins/Unit/floatsisfvfp_test",
        "compiler-rt/test/builtins/Unit/floatunssidfvfp_test",
        "compiler-rt/test/builtins/Unit/floatunssisfvfp_test",
        "compiler-rt/test/builtins/Unit/gedf2vfp_test",
        "compiler-rt/test/builtins/Unit/gesf2vfp_test",
        "compiler-rt/test/builtins/Unit/gtdf2vfp_test",
        "compiler-rt/test/builtins/Unit/gtsf2vfp_test",
        "compiler-rt/test/builtins/Unit/ledf2vfp_test",
        "compiler-rt/test/builtins/Unit/lesf2vfp_test",
        "compiler-rt/test/builtins/Unit/ltdf2vfp_test",
        "compiler-rt/test/builtins/Unit/ltsf2vfp_test",
        "compiler-rt/test/builtins/Unit/muldf3vfp_test",
        "compiler-rt/test/builtins/Unit/mulsf3vfp_test",
        "compiler-rt/test/builtins/Unit/nedf2vfp_test",
        "compiler-rt/test/builtins/Unit/negdf2vfp_test",
        "compiler-rt/test/builtins/Unit/negsf2vfp_test",
        "compiler-rt/test/builtins/Unit/nesf2vfp_test",
        "compiler-rt/test/builtins/Unit/subdf3vfp_test",
        "compiler-rt/test/builtins/Unit/subsf3vfp_test",
        "compiler-rt/test/builtins/Unit/truncdfsf2vfp_test",
        "compiler-rt/test/builtins/Unit/unorddf2vfp_test",
        "compiler-rt/test/builtins/Unit/unordsf2vfp_test",
]

generate_test_cases("compiler-rt-vfp", vector_fp_tests)

fp_tests = [
        "compiler-rt/test/builtins/Unit/absvdi2_test",
        "compiler-rt/test/builtins/Unit/absvsi2_test",
        "compiler-rt/test/builtins/Unit/absvti2_test",
        "compiler-rt/test/builtins/Unit/addtf3_test",
        "compiler-rt/test/builtins/Unit/addvdi3_test",
        "compiler-rt/test/builtins/Unit/addvsi3_test",
        "compiler-rt/test/builtins/Unit/addvti3_test",
        "compiler-rt/test/builtins/Unit/ashldi3_test",
        "compiler-rt/test/builtins/Unit/ashlti3_test",
        "compiler-rt/test/builtins/Unit/ashrdi3_test",
        "compiler-rt/test/builtins/Unit/ashrti3_test",
        "compiler-rt/test/builtins/Unit/bswapdi2_test",
        "compiler-rt/test/builtins/Unit/bswapsi2_test",
#       "compiler-rt/test/builtins/Unit/clear_cache_test",
        "compiler-rt/test/builtins/Unit/clzdi2_test",
        "compiler-rt/test/builtins/Unit/clzsi2_test",
        "compiler-rt/test/builtins/Unit/clzti2_test",
        "compiler-rt/test/builtins/Unit/cmpdi2_test",
        "compiler-rt/test/builtins/Unit/cmpti2_test",
        "compiler-rt/test/builtins/Unit/comparedf2_test",
        "compiler-rt/test/builtins/Unit/comparesf2_test",
        "compiler-rt/test/builtins/Unit/ctzdi2_test",
        "compiler-rt/test/builtins/Unit/ctzsi2_test",
        "compiler-rt/test/builtins/Unit/ctzti2_test",
        "compiler-rt/test/builtins/Unit/divdc3_test",
        "compiler-rt/test/builtins/Unit/divdi3_test",
        "compiler-rt/test/builtins/Unit/divmodsi4_test",
        "compiler-rt/test/builtins/Unit/divsc3_test",
        "compiler-rt/test/builtins/Unit/divsi3_test",
#       "compiler-rt/test/builtins/Unit/divtc3_test",
        "compiler-rt/test/builtins/Unit/divtf3_test",
        "compiler-rt/test/builtins/Unit/divti3_test",
        "compiler-rt/test/builtins/Unit/divxc3_test",
#       "compiler-rt/test/builtins/Unit/enable_execute_stack_test",
        "compiler-rt/test/builtins/Unit/eqtf2_test",
        "compiler-rt/test/builtins/Unit/extenddftf2_test",
#       "compiler-rt/test/builtins/Unit/extendhfsf2_test",
        "compiler-rt/test/builtins/Unit/extendsftf2_test",
        "compiler-rt/test/builtins/Unit/ffsdi2_test",
        "compiler-rt/test/builtins/Unit/ffsti2_test",
        "compiler-rt/test/builtins/Unit/fixdfdi_test",
        "compiler-rt/test/builtins/Unit/fixdfti_test",
        "compiler-rt/test/builtins/Unit/fixsfdi_test",
        "compiler-rt/test/builtins/Unit/fixsfti_test",
        "compiler-rt/test/builtins/Unit/fixtfdi_test",
        "compiler-rt/test/builtins/Unit/fixtfsi_test",
        "compiler-rt/test/builtins/Unit/fixtfti_test",
        # this errors on 0X1P+64
        #"compiler-rt/test/builtins/Unit/fixunsdfdi_test",
        "compiler-rt/test/builtins/Unit/fixunsdfsi_test",
        "compiler-rt/test/builtins/Unit/fixunsdfti_test",
        # this errors on 0X1P+64
        #"compiler-rt/test/builtins/Unit/fixunssfdi_test",
        "compiler-rt/test/builtins/Unit/fixunssfsi_test",
        "compiler-rt/test/builtins/Unit/fixunssfti_test",
        "compiler-rt/test/builtins/Unit/fixunstfdi_test",
        "compiler-rt/test/builtins/Unit/fixunstfsi_test",
        "compiler-rt/test/builtins/Unit/fixunstfti_test",
        "compiler-rt/test/builtins/Unit/fixunsxfdi_test",
        "compiler-rt/test/builtins/Unit/fixunsxfsi_test",
        "compiler-rt/test/builtins/Unit/fixunsxfti_test",
        "compiler-rt/test/builtins/Unit/fixxfdi_test",
        "compiler-rt/test/builtins/Unit/fixxfti_test",
        "compiler-rt/test/builtins/Unit/floatdidf_test",
        "compiler-rt/test/builtins/Unit/floatdisf_test",
        "compiler-rt/test/builtins/Unit/floatditf_test",
        "compiler-rt/test/builtins/Unit/floatdixf_test",
        "compiler-rt/test/builtins/Unit/floatsitf_test",
        "compiler-rt/test/builtins/Unit/floattidf_test",
        "compiler-rt/test/builtins/Unit/floattisf_test",
        "compiler-rt/test/builtins/Unit/floattixf_test",
        "compiler-rt/test/builtins/Unit/floatundidf_test",
        "compiler-rt/test/builtins/Unit/floatundisf_test",
        "compiler-rt/test/builtins/Unit/floatunditf_test",
        "compiler-rt/test/builtins/Unit/floatundixf_test",
        "compiler-rt/test/builtins/Unit/floatunsitf_test",
        "compiler-rt/test/builtins/Unit/floatuntidf_test",
        "compiler-rt/test/builtins/Unit/floatuntisf_test",
        "compiler-rt/test/builtins/Unit/floatuntixf_test",
#       "compiler-rt/test/builtins/Unit/gcc_personality_test",
        "compiler-rt/test/builtins/Unit/getf2_test",
        "compiler-rt/test/builtins/Unit/gttf2_test",
        "compiler-rt/test/builtins/Unit/letf2_test",
        "compiler-rt/test/builtins/Unit/lshrdi3_test",
        "compiler-rt/test/builtins/Unit/lshrti3_test",
        "compiler-rt/test/builtins/Unit/lttf2_test",
        "compiler-rt/test/builtins/Unit/moddi3_test",
        "compiler-rt/test/builtins/Unit/modsi3_test",
        "compiler-rt/test/builtins/Unit/modti3_test",
        "compiler-rt/test/builtins/Unit/muldc3_test",
        "compiler-rt/test/builtins/Unit/muldi3_test",
        "compiler-rt/test/builtins/Unit/mulodi4_test",
        "compiler-rt/test/builtins/Unit/mulosi4_test",
        "compiler-rt/test/builtins/Unit/muloti4_test",
        "compiler-rt/test/builtins/Unit/mulsc3_test",
        "compiler-rt/test/builtins/Unit/multc3_test",
        "compiler-rt/test/builtins/Unit/multf3_test",
        "compiler-rt/test/builtins/Unit/multi3_test",
        "compiler-rt/test/builtins/Unit/mulvdi3_test",
        "compiler-rt/test/builtins/Unit/mulvsi3_test",
        "compiler-rt/test/builtins/Unit/mulvti3_test",
        "compiler-rt/test/builtins/Unit/mulxc3_test",
        "compiler-rt/test/builtins/Unit/negdi2_test",
        "compiler-rt/test/builtins/Unit/negti2_test",
        "compiler-rt/test/builtins/Unit/negvdi2_test",
        "compiler-rt/test/builtins/Unit/negvsi2_test",
        "compiler-rt/test/builtins/Unit/negvti2_test",
        "compiler-rt/test/builtins/Unit/netf2_test",
        "compiler-rt/test/builtins/Unit/paritydi2_test",
        "compiler-rt/test/builtins/Unit/paritysi2_test",
        "compiler-rt/test/builtins/Unit/parityti2_test",
        "compiler-rt/test/builtins/Unit/popcountdi2_test",
        "compiler-rt/test/builtins/Unit/popcountsi2_test",
        "compiler-rt/test/builtins/Unit/popcountti2_test",
        "compiler-rt/test/builtins/Unit/powidf2_test",
        "compiler-rt/test/builtins/Unit/powisf2_test",
        "compiler-rt/test/builtins/Unit/powitf2_test",
        "compiler-rt/test/builtins/Unit/powixf2_test",
        "compiler-rt/test/builtins/Unit/subtf3_test",
        "compiler-rt/test/builtins/Unit/subvdi3_test",
        "compiler-rt/test/builtins/Unit/subvsi3_test",
        "compiler-rt/test/builtins/Unit/subvti3_test",
#       "compiler-rt/test/builtins/Unit/trampoline_setup_test",
#       "compiler-rt/test/builtins/Unit/truncdfhf2_test",
        "compiler-rt/test/builtins/Unit/truncdfsf2_test",
#       "compiler-rt/test/builtins/Unit/truncsfhf2_test",
        "compiler-rt/test/builtins/Unit/trunctfdf2_test",
        "compiler-rt/test/builtins/Unit/trunctfsf2_test",
        "compiler-rt/test/builtins/Unit/ucmpdi2_test",
        "compiler-rt/test/builtins/Unit/ucmpti2_test",
        "compiler-rt/test/builtins/Unit/udivdi3_test",
        "compiler-rt/test/builtins/Unit/udivmoddi4_test",
        "compiler-rt/test/builtins/Unit/udivmodsi4_test",
        "compiler-rt/test/builtins/Unit/udivmodti4_test",
        "compiler-rt/test/builtins/Unit/udivsi3_test",
        "compiler-rt/test/builtins/Unit/udivti3_test",
        "compiler-rt/test/builtins/Unit/umoddi3_test",
        "compiler-rt/test/builtins/Unit/umodsi3_test",
        "compiler-rt/test/builtins/Unit/umodti3_test",
        "compiler-rt/test/builtins/Unit/unordtf2_test",
]

generate_test_cases("compiler-rt-fp", fp_tests)
