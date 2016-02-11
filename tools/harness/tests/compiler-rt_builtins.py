##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import tests
from common import TestCommon
from results import PassFailResult

class CompilerRTBuiltinsAbstract(TestCommon):

    def get_finish_string(self):
        return "usleeptest_done"

    def process_data(self, testdir, rawiter):
        # the test passed if no error occurred
        passed = True
        for line in rawiter:
            if "error in" in line:
                passed = False
        return PassFailResult(passed)

@tests.add_test
class CompilerRTBuiltins1(CompilerRTBuiltinsAbstract):
    '''runs compiler-rt1 builins unit tests'''
    name = "compiler-rt1"

    def get_modules(self, build, machine):
        modules = super(CompilerRTBuiltins1, self).get_modules(build, machine)
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/absvdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/absvsi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/absvti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/adddf3vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/addsf3vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/addtf3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/addvdi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/addvsi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/addvti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ashldi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ashlti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ashrdi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ashrti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/bswapdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/bswapsi2_test")
#        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/clear_cache_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/clzdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/clzsi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/clzti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/cmpdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/cmpti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/comparedf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/comparesf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ctzdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ctzsi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ctzti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divdc3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divdf3vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divdi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divmodsi4_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divsc3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divsf3vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divsi3_test")
#        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divtc3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divtf3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/divxc3_test")
#        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/enable_execute_stack_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/eqdf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/eqsf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/eqtf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/extebdsfdf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/extenddftf2_test")
#        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/extendhfsf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/extendsftf2_test")
        modules.add_module("usleeptest", [ "5" ])
        return modules

@tests.add_test
class CompilerRTBuiltins2(CompilerRTBuiltinsAbstract):
    '''runs compiler-rt2 builins unit tests'''
    name = "compiler-rt2"

    def get_modules(self, build, machine):
        modules = super(CompilerRTBuiltins2, self).get_modules(build, machine)
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ffsdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ffsti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixdfdi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixdfsivfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixdfti_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixsfdi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixsfsivfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixsfti_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixtfdi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixtfsi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixtfti_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunsdfdi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunsdfsi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunsdfsivfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunsdfti_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunssfdi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunssfsi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunssfsivfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunssfti_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunstfdi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunstfsi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunstfti_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunsxfdi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunsxfsi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixunsxfti_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixxfdi_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/fixxfti_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatdidf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatdisf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatditf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatdixf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatsidfvfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatsisfvfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatsitf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floattidf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floattisf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floattixf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatundidf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatundisf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatunditf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatundixf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatunsitf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatunssidfvfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatunssisfvfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatuntidf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatuntisf_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/floatuntixf_test")
#        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/gcc_personality_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/gedf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/gesf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/getf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/gtdf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/gtsf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/gttf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ledf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/lesf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/letf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/lshrdi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/lshrti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ltdf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ltsf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/lttf2_test")
        modules.add_module("usleeptest", [ "5" ])
        return modules

@tests.add_test
class CompilerRTBuiltins3(CompilerRTBuiltinsAbstract):
    '''runs compiler-rt3 builins unit tests'''
    name = "compiler-rt3"

    def get_modules(self, build, machine):
        modules = super(CompilerRTBuiltins3, self).get_modules(build, machine)
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/moddi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/modsi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/modti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/muldc3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/muldf3vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/muldi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/mulodi4_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/mulosi4_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/muloti4_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/mulsc3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/mulsf3vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/multc3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/multf3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/multi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/mulvdi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/mulvsi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/mulvti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/mulxc3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/nedf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/negdf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/negdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/negsf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/negti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/negvdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/negvsi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/negvti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/nesf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/netf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/paritydi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/paritysi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/parityti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/popcountdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/popcountsi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/popcountti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/powidf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/powisf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/powitf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/powixf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/subdf3vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/subsf3vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/subtf3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/subvdi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/subvsi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/subvti3_test")
#        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/trampoline_setup_test")
#        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/truncdfhf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/truncdfsf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/truncdfsf2vfp_test")
#        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/truncsfhf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/trunctfdf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/trunctfsf2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ucmpdi2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/ucmpti2_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/udivdi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/udivmoddi4_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/udivmodsi4_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/udivmodti4_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/udivsi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/udivti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/umoddi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/umodsi3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/umodti3_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/unorddf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/unordsf2vfp_test")
        modules.add_module("$BUILD/compiler-rt/test/builtins/Unit/unordtf2_test")
        modules.add_module("usleeptest", [ "5" ])
        return modules
