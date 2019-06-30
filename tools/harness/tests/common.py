##########################################################################
# Copyright (c) 2009, 2010, 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, shutil, select, datetime, pexpect, tempfile, signal
from pexpect import fdpexpect
import barrelfish, debug
from tests import Test
from harness import Harness

RAW_TEST_OUTPUT_FILENAME = Harness.RAW_FILE_NAME
DEFAULT_TEST_TIMEOUT = datetime.timedelta(seconds=600)
DEFAULT_BOOT_TIMEOUT = datetime.timedelta(seconds=300)
AFTER_FINISH_TIMEOUT = datetime.timedelta(seconds=30)
TEST_NO_OUTPUT_LINE = '[Error: could not read output from test]\n'
TEST_TIMEOUT_LINE = '[Error: test timed out]\n'
BOOT_TIMEOUT_LINE_RETRY = '[Error: boot timed out, retrying...]\n'
BOOT_TIMEOUT_LINE_FAIL = '[Error: boot timed out, retry limit reached]\n'
MAX_BOOT_ATTEMPTS = 3 # maximum number of times to attempt booting before giving up

class TimeoutError(Exception):
    def __init__(self, when=None):
        self.when = when
    def __str__(self):
        return 'timeout occurred%s' % (': ' + self.when if self.when else '')

class SignalledFdSpawn(fdpexpect.fdspawn):
    
    def read_nonblocking(self, size=1, timeout=None):
        """ This method uses OS signals to implement timeouts """
        # Get timeout from instance
        if timeout == -1:
            timeout = self.timeout

        if timeout is not None:
            # Assert there is no other alarm signal handler installed
            assert(signal.getsignal(signal.SIGALRM) == signal.SIG_DFL)

            # Timeout 0 actually cancels the alarm
            assert(int(timeout) > 0) 
            def timeout_handler(frame,signum):
                raise pexpect.TIMEOUT("Signalled Timeout") 

            # Install handler and setup alarm
            signal.signal(signal.SIGALRM, timeout_handler)
            signal.alarm(int(timeout))

        try:    
            return super(SignalledFdSpawn, self).read_nonblocking(size, None)
        finally:
            # Remove our handler
            signal.signal(signal.SIGALRM, signal.SIG_DFL)

class TestCommon(Test):
    name = None # should be overridden

    def __init__(self, options):
        super(TestCommon, self).__init__(options)
        self.timeout = None # current timeout (as absolute datetime)
        self.test_timeout_delta = DEFAULT_TEST_TIMEOUT # timeout delta for next test
        self.boot_phase = True # are we waiting for machine boot or test output?
        self.boot_attempts = 0 # how many times did we try to boot?

        # The default should be True, it causes harness to include extra
        # console output that appears after the test returns true for is_finished.
        # However, many tests currently rely on the the last line being exactly
        # the one that caused is_finished to become true. So disable
        # this for now by default
        self.read_after_finished = False

    def _setup_harness_dir(self, build, machine):
        dest_dir = machine.get_tftp_dir()
        debug.verbose('installing to %s' % dest_dir)
        if os.access(dest_dir, os.F_OK):
            debug.verbose('clearing out %s' % dest_dir)
            for e in os.listdir(dest_dir):
                p = os.path.join(dest_dir, e)
                if os.path.isdir(p):
                    shutil.rmtree(p, ignore_errors=True)
                elif not e.startswith('.nfs'):
                    os.unlink(p)
        else:
            debug.verbose('creating %s' % dest_dir)
            os.makedirs(dest_dir)
        return dest_dir

    def setup(self, build, machine, testdir):
        # build the default set of targets
        targets = self.get_build_targets(build, machine)
        # set custom test timeout if machine specifies one
        test_timeout_secs = machine.get_test_timeout()
        if not test_timeout_secs:
            test_timeout_secs = DEFAULT_TEST_TIMEOUT
        else:
            test_timeout_secs = datetime.timedelta(seconds=test_timeout_secs)
        self.test_timeout_delta = test_timeout_secs
        self.testdir = testdir
        build.build(targets)

        # lock the machine
        machine.lock()
        machine.setup()

        # setup the harness dir and install there
        dest_dir = self._setup_harness_dir(build, machine)
        build.install(targets, dest_dir)

    def get_modules(self, build, machine):
        return machine.default_bootmodules()

    def get_build_targets(self, build, machine):
        return self.get_modules(build, machine).get_build_targets()

    def set_timeout(self, delta=DEFAULT_TEST_TIMEOUT):
        self.test_timeout_delta = delta
        if not self.boot_phase:
            if delta:
                debug.verbose('setting timeout for %s' % delta)
                self.timeout = datetime.datetime.now() + delta
            else:
                debug.verbose('cancelling timeout')
                self.timeout = None

    def boot(self, machine, modules):
        machine.set_bootmodules(modules)
        self.boot_attempts = 0
        self.reboot(machine)

    def reboot(self, machine):
        # retry a failed boot, without changing the modules
        machine.reboot()
        self.boot_phase = True
        self.boot_attempts += 1
        timeout_secs = machine.get_boot_timeout()
        if timeout_secs:
            self.timeout = (datetime.datetime.now()
                                 + datetime.timedelta(seconds=timeout_secs))
        else:
            self.timeout = datetime.datetime.now() + DEFAULT_BOOT_TIMEOUT

    def get_finish_string(self):
        # default output from a test program when it completes
        # should be overridden by subclasses
        return "client done"

    def is_finished(self, line):
        # Exit test when we get an assertion failure or an abort, rather than
        # waiting for timeout
        return self.get_finish_string() in line or \
               line.startswith("Assertion failed on core") or \
               line.find("PANIC!") > 0 or \
               line.startswith("Aborted")

    def is_booted(self, line):
        # early boot output from Barrelfish kernel
        return "Barrelfish CPU driver starting" in line

    def process_line(self, rawline):
        """Can be used by subclasses to hook into the raw output stream."""
        pass

    def _readline(self, fh, timeout=None):
        """ if given, timeout parameter overrides self.timeout for call """
        if timeout is None:
            timeout = self.timeout

        # standard blocking readline if no timeout is set
        if not timeout:
            return fh.readline()

        line = ''
        fhArray = [fh]
        while not line.endswith('\n'):
            # wait until there is something to read, with a timeout
            (readlist, _, _) = select_timeout(timeout, fhArray)
            if not readlist:
                # if we have some partial data, return that first!
                # we'll be called again, and the next time can raise the error
                if line:
                    return line
                elif self.boot_phase:
                    raise TimeoutError('waiting for victim to boot')
                else:
                    raise TimeoutError('waiting for test output from victim')
            # read a single character, to avoid blocking
            # FIXME: there must be a better way to do nonblocking IO!
            c = fh.read(1)
            # can see EOF if fh is a telnet socket that was closed in the meantime
            if c == '':
                raise EOFError('read from sub-process returned EOF')
            line += c
        return line

    def _read_until_block(self, fh):
        """ Reads from the console until it blocks or 30 sec have passed """
        start = datetime.datetime.now()
        while start + AFTER_FINISH_TIMEOUT > datetime.datetime.now():
            try:
                timeout = datetime.timedelta(seconds=1) + datetime.datetime.now()
                yield self._readline(fh, timeout=timeout)
            except TimeoutError:
                return

    def collect_data(self, machine):
        fh = machine.get_output()
        if fh is None:
            yield TEST_NO_OUTPUT_LINE
            return
        while True:
            try:
                line = self._readline(fh)
            except TimeoutError as e:
                if self.boot_phase:
                    if self.boot_attempts < MAX_BOOT_ATTEMPTS:
                        yield BOOT_TIMEOUT_LINE_RETRY
                        self.reboot(machine)
                        continue
                    else:
                        yield BOOT_TIMEOUT_LINE_FAIL
                else:
                    yield TEST_TIMEOUT_LINE
                debug.verbose("timeout encountered in collect_data");
                raise e
            except EOFError as e:
                debug.verbose("got EOF from sub-process")
                break

            yield line

            if not self.boot_phase:
                self.process_line(line)
                if self.is_finished(line):
                    debug.verbose("is_finished returned true for line %s" % line)
                    # Read remaining lines from console until it blocks
                    if self.read_after_finished:
                        for x in self._read_until_block(fh):
                            self.process_line(x)
                            yield x
                    break
            elif self.is_booted(line):
                self.boot_phase = False
                self.set_timeout(self.test_timeout_delta)
                self.process_line(line)

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        self.boot(machine, modules)
        return self.collect_data(machine)

    def cleanup(self, machine):
        tftp_dir = machine.get_tftp_dir()
        machine.shutdown()
        machine.unlock()
        debug.verbose('removing %s' % tftp_dir)
        shutil.rmtree(tftp_dir, ignore_errors=True)


class InteractiveTest(TestCommon):
    """
    A interactive test class that allows a test-case to interact with
    the fish shell. Sub-classes should implement the interact method.

    As an example, have a look at coreboottest.py.
    """

    def get_modules(self, build, machine):
        modules = super(InteractiveTest, self).get_modules(build, machine)
        # Load the console
        serialargs = ["auto"]
        if machine.get_machine_name() == "tomme1" or \
           machine.get_machine_name() == "tomme2":
            serialargs = ["portbase=0x2f8", "irq=0x3"]

        modules.add_module(machine.get_serial_binary(), args=serialargs)

        modules.add_module("fish", args=["nospawn"])
        # use terminal type 'dumb' here to disable linenoise readline stuff
        modules.add_module("angler", args=['serial0.terminal', 'dumb'])
        return modules

    def wait_for_prompt(self):
        self.console.expect(">")

    def wait_for_fish(self):
        debug.verbose("Waiting for fish.")
        self.console.expect("fish v0.2 -- pleased to meet you!",
                timeout=self.test_timeout)
        self.wait_for_prompt()

    def interact(self):
        # Implement interaction with console
        pass

    def set_timeouts(self, machine):
        self.boot_timeout = machine.get_boot_timeout()
        if not self.boot_timeout:
            self.boot_timeout = DEFAULT_BOOT_TIMEOUT.seconds
        self.test_timeout = machine.get_test_timeout()
        if not self.test_timeout:
            self.test_timeout = DEFAULT_TEST_TIMEOUT.seconds

    def collect_data(self, machine):
        fh = machine.get_output()


        self.console = SignalledFdSpawn(fh, timeout=self.test_timeout)
        self.console.logfile = open(os.path.join(self.testdir, RAW_TEST_OUTPUT_FILENAME), 'wb+')

        while self.boot_attempts < MAX_BOOT_ATTEMPTS:
            index = self.console.expect(["Barrelfish CPU driver starting", 
                                 pexpect.TIMEOUT, pexpect.EOF],
                                 timeout=self.boot_timeout)
            if index == 0:
                self.boot_phase = False
                break
            if index == 1:
                self.console.logfile.write(BOOT_TIMEOUT_LINE_RETRY)
                self.reboot(machine)
            if index == 2:
                self.console.logfile.write(BOOT_TIMEOUT_LINE_FAIL)

        if not self.boot_phase:
            machine.force_write(self.console)
            try:
                self.interact()
            except (pexpect.TIMEOUT, OSError), e:
                self.console.logfile.seek(0)
                print "Interaction timed out:"
                print ''.join(self.console.logfile.readlines())
                raise e

        self.console.logfile.seek(0)
        return self.console.logfile.readlines()

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        self.set_timeouts(machine)
        self.boot(machine, modules)
        return self.collect_data(machine)

    def process_data(self, testdir, rawiter):
        """Implement in subclasses to check whether test passed"""
        raise Exception("Implement process_data in Test %s" % self.name)
        pass


# utility function used by other tests
def select_timeout(timeout, rlist=[], wlist=[], xlist=[]):
    """run select.select, with a timeout specified as a datetime object"""
    delta = timeout - datetime.datetime.now()
    if delta.days >= 0:
        assert(delta.days == 0) # unimplemented, and insane!
        secs = delta.seconds + delta.microseconds / 1000000.0
        assert(secs > 0)
        return select.select(rlist, wlist, xlist, secs)
    else: # already timed out
        return ([], [], [])
