
import tests
import re
from common import TestCommon
from results import PassFailResult

@tests.add_test
class RetypeTest(TestCommon):
    '''test new retype code'''
    name = "retype"

    def get_modules(self, build, machine):
        modules = super(RetypeTest, self).get_modules(build, machine)
        modules.add_module("test_retype")
        return modules

    def get_finish_string(self):
        return "retype: result:"

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        passed = False
        for line in rawiter:
            if line.startswith(self.get_finish_string()):
                _,_,results=line.split(':')
                results = results.strip()
                passed = results == "0"
        return PassFailResult(passed)

@tests.add_test
class RetypeMultiTest(TestCommon):
    '''test new retype code'''
    name = "retype_multi"

    def setup(self,build,machine,testdir):
        super(RetypeMultiTest, self).setup(build,machine,testdir)
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(RetypeMultiTest, self).get_modules(build, machine)
        for core in machine.get_coreids():
            modules.add_module("test_retype", ["core=%d" % core])
        return modules

    def is_finished(self, line):
        if line.startswith("retype: result:"):
            self._nseen += 1
        return self._nseen == self._ncores

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        nspawned = 0
        npassed = 0
        for line in rawiter:
            if re.match(r'.*pawning .*test_retype on core', line):
                nspawned += 1
            if line.startswith("retype: result:"):
                _,_,r=line.split(':')
                r = r.strip()
                if r == "0":
                    npassed += 1
        return PassFailResult(npassed == nspawned)
