
import tests
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
