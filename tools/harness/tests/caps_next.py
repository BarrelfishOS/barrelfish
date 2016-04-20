
import tests
import re
from common import TestCommon
from results import PassFailResult

@tests.add_test
class Retype2Test(TestCommon):
    '''test new retype code'''
    name = "retype2"

    def get_modules(self, build, machine):
        modules = super(Retype2Test, self).get_modules(build, machine)
        modules.add_module("test_retype2")
        return modules

    def get_finish_string(self):
        return "retype2: result:"

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
class Retype2MultiTest(TestCommon):
    '''test new retype code'''
    name = "retype2_multi"

    def setup(self,build,machine,testdir):
        super(Retype2MultiTest, self).setup(build,machine,testdir)
        self._ncores = machine.get_ncores()
        self._nseen = 0

    def get_modules(self, build, machine):
        modules = super(Retype2MultiTest, self).get_modules(build, machine)
        for core in machine.get_coreids():
            modules.add_module("test_retype2", ["core=%d" % core])
        return modules

    def is_finished(self, line):
        if line.startswith("retype2: result:"):
            self._nseen += 1
        return self._nseen == self._ncores

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        nspawned = 0
        npassed = 0
        for line in rawiter:
            if re.match(r'.*pawning .*test_retype2 on core', line):
                nspawned += 1
            if line.startswith("retype2: result:"):
                _,_,r=line.split(':')
                r = r.strip()
                if r == "0":
                    npassed += 1
        return PassFailResult(npassed == nspawned)
