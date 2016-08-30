import re
import tests
from common import TestCommon
from results import PassFailResult

@tests.add_test
class ListFacts(TestCommon):
    '''Lists facts in the SKB'''

    name = "listfacts"

    def get_modules(self, build, machine):
        modules = super(ListFacts, self).get_modules(build, machine)
        modules.add_module("e1000n", ["auto"])
        modules.add_module("NGD_mng", ["auto"])
        modules.add_module("netd", ["auto"])
        modules.add_module("listfacts")
        return modules

    def process_data(self, testdir, rawiter):
        for line in rawiter:
            if line.strip() == "SKB FACTS LISTING END":
                return PassFailResult(True)
        return PassFailResult(False)
