

import os
import tests
from common import TestCommon
from barrelfish import BootModules
from results import PassFailResult

@tests.add_test
class TftpClientTest(TestCommon):
    '''Barrelfish TFTP client test'''
    name = "tftpclient"

    _filename = "hello.txt"
    _filecontents = "Hello world via TFTP!"

    def setup_tftp_file(self, tftpdir):
        with open(os.path.join(tftpdir, self._filename), 'w') as f:
            f.write(self._filecontents)


    def setup(self, build, machine, testdir):
        super(TftpClientTest, self).setup(build, machine, testdir)
        self.setup_tftp_file(machine.get_tftp_dir())

    def get_modules(self, build, machine):
        modules = super(TftpClientTest, self).get_modules(build, machine)
        tftpdir = machine._operations.get_tftp_subdir()
        modules.add_module("e1000n", ["auto"])
        modules.add_module("net_sockets_server", ["auto"])
        modules.add_module("tests/tftpclient",
                ['--server=tftp://10.110.4.4:69',
                 '--file=/%s/hello.txt' % tftpdir ])
        return modules

    def get_finish_string(self):
        return 'TFTP TEST DONE.'

    def process_data(self, testdir, rawiter):
        passed = False
        for line in rawiter:
            if self._filecontents in line:
                passed = True

        return PassFailResult(passed)
