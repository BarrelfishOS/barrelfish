#!/usr/bin/env python

#
# Copyright (c) 2009, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
#

import sys
from machines import MachineFactory

# check interpreter version to avoid confusion over syntax/module errors
if sys.version_info < (2, 6):
    sys.stderr.write('Error: Python 2.6 or greater is required\n')
    sys.exit(1)

import os
import codecs
import optparse
import traceback
import datetime
import getpass
import fnmatch
import harness
import debug
import checkout
import builds
import tests
import machines
from tests.common import TimeoutError
from socket import gethostname

try:
    from junit_xml import TestSuite, TestCase
    have_junit_xml = True
except:
    have_junit_xml = False

def list_all():
    print 'Build types:\t', ', '.join([b.name for b in builds.all_builds])
    print 'Machines:\t', ', '.join([m for m in MachineFactory.machineFactories.keys()])
    print 'Tests:'
    for t in sorted(tests.all_tests, key=lambda test: test.name):
        print '  %-20s %s' % (t.name, (t.__doc__ or '').strip())


def parse_args():
    p = optparse.OptionParser(
        usage='Usage: %prog [options] SOURCEDIR RESULTDIR',
        description='Barrelfish regression/benchmark harness')

    g = optparse.OptionGroup(p, 'Basic options')
    g.add_option('-b', '--build', action='append', dest='buildspecs',
                 metavar='BUILD', help='build types to perform [default: test]')
    g.add_option('-B', '--buildbase', dest='buildbase', metavar='DIR',
                 help='places builds under DIR [default: SOURCEDIR/builds]')
    g.add_option('-e', '--existingbuild', dest='existingbuild', metavar='DIR',
                 help='existing build directory (may not be used with -b)')
    g.add_option('-m', '--machine', action='append', dest='machinespecs',
                 metavar='MACHINE', help='victim machines to use')
    g.add_option('-t', '--test', action='append', dest='testspecs',
                 metavar='TEST', help='tests/benchmarks to run')
    g.add_option('-c', '--comment', dest='comment',
                 help='comment to store with all collected data')
    g.add_option('-x', '--xml', dest='xml', action='store_true',
                 default=False,
                 help='output summary of tests in Junit XML format')
    p.add_option_group(g)

    g = optparse.OptionGroup(p, 'Debugging options')
    g.add_option('-L', '--listall', action='store_true', dest='listall',
                 help='list available builds, machines and tests')
    debug.addopts(g, 'debuglevel')
    g.add_option('-k', '--keepgoing', action='store_true', dest='keepgoing',
                 help='attempt to continue on errors')
    p.add_option_group(g)
    p.set_defaults(debuglevel=debug.NORMAL)

    options, args = p.parse_args()

    debug.current_level = options.debuglevel

    if options.listall:
        list_all()
        sys.exit(0)

    if len(args) != 2:
        p.error('source and results directories must be specified')
    options.sourcedir, options.resultsdir = args

    # determine default buildbase if needed
    if options.buildbase is None:
        options.buildbase = os.path.join(options.sourcedir, 'builds')

    # check validity of source and results dirs
    if not os.path.isdir(os.path.join(options.sourcedir, 'hake')):
        p.error('invalid source directory %s' % options.sourcedir)
    if not (os.path.isdir(options.resultsdir)
            and os.access(options.resultsdir, os.W_OK)):
        p.error('invalid results directory %s' % options.resultsdir)

    if options.xml and not have_junit_xml:
        p.error('--xml requires junit-xml.\n'
                'Please install junit-xml through pip or easy_install')

    # resolve and instantiate all builds
    def _lookup(spec, classes, nameFn=lambda c: c.name.lower()):
        spec = spec.lower()
        return [c for c in classes if fnmatch.fnmatch(nameFn(c), spec)]

    if options.existingbuild:
        if options.buildspecs:
            p.error('existing build directory cannot be used together'
                    ' with build types (-b)')
        options.builds = [builds.existingbuild(options, options.existingbuild)]
        options.buildbase = options.existingbuild
    else:
        options.builds = []
        if not options.buildspecs:
            options.buildspecs = ['test']
        for spec in options.buildspecs:
            matches = _lookup(spec, builds.all_builds)
            if matches == []:
                p.error('no builds match "%s" (try -L for a list)' % spec)
            options.builds.extend(
                [b for b in matches if b not in options.builds])
        options.builds = [b(options) for b in options.builds]

    # resolve and instantiate all machines
    if options.machinespecs is None:
        p.error('no machines specified')
    options.machines = []
    for spec in options.machinespecs:
        matches = _lookup(spec, MachineFactory.machineFactories, nameFn=lambda fac: fac.lower())
        if matches == []:
            p.error('no machines match "%s" (try -L for a list)' % spec)
        options.machines.extend(
            [m for m in matches if m not in options.machines])
    options.machines = [MachineFactory.createMachineByName(m, options) for m in options.machines]

    # resolve and instantiate all tests
    if options.testspecs:
        options.tests = []
        for spec in options.testspecs:
            matches = _lookup(spec, tests.all_tests)
            if matches == []:
                p.error('no tests match "%s" (try -L for a list)' % spec)
            options.tests.extend(
                [t for t in matches if t not in options.tests])
    else:
        p.error('no tests specified (try -t memtest if unsure)')
    options.tests = [t(options) for t in options.tests]

    debug.verbose('Host:     ' + gethostname())
    debug.verbose('Builds:   ' + ', '.join([b.name for b in options.builds]))
    debug.verbose('Machines: ' + ', '.join([m.getName() for m in options.machines]))
    debug.verbose('Tests:    ' + ', '.join([t.name for t in options.tests]))

    return options

class Scalebench:

    def __init__(self, options):
        self._harness = harness.Harness()
        self._options = options

    def make_results_dir(self, build, machine, test):
        # Create a unique directory for the output from this test
        timestamp = datetime.datetime.now().strftime('%Y%m%d-%H%M%S')
        dirname = '-'.join([test.name, build.name, machine.getName(), timestamp])
        path = os.path.join(self._options.resultsdir, str(datetime.datetime.now().year), dirname)
        debug.verbose('create result directory %s' % path)
        os.makedirs(path)
        return path

    def make_run_dir(self, build, machine):
        # Create a unique directory for the output from this test
        timestamp = datetime.datetime.now().strftime('%Y%m%d-%H%M%S')
        dirname = '-'.join([build.name, machine.getName(), timestamp])
        path = os.path.join(self._options.resultsdir, str(datetime.datetime.now().year), dirname)
        debug.verbose('create result directory %s' % path)
        os.makedirs(path)
        return path

    def write_description(self, checkout, build, machine, test, path):
        debug.verbose('write description file')
        with codecs.open(os.path.join(path, 'description.txt'), 'w', 'utf-8') as f:
            f.write('test: %s\n' % test.name)
            f.write('revision: %s\n' % checkout.get_revision())
            f.write('build: %s\n' % build.name)
            f.write('machine: %s\n' % machine.getName())
            f.write('start time: %s\n' % datetime.datetime.now())
            f.write('user: %s\n' % getpass.getuser())
            for item in checkout.get_meta().items():
                f.write("%s: %s\n" % item)

            if self._options.comment:
                f.write('\n' + self._options.comment + '\n')

        diff = checkout.get_diff()
        if diff:
            with codecs.open(os.path.join(path, 'changes.patch'), 'w', 'utf-8') as f:
                f.write(diff)

    def write_errorcase(self, build, machine, test, path, msg, start_ts, end_ts):
        delta = end_ts - start_ts
        tc = { 'name': test.name,
               'time_elapsed': delta.total_seconds(),
               'class': machine.getName(),
               'stdout': '\n'.join(self._harness.process_output(test, path)),
               'stderr': "",
               'passed': False
        }
        if have_junit_xml:
            ju_tc = TestCase(
                    tc['name'],
                    tc['class'],
                    tc['time_elapsed'],
                    tc['stdout'],
                    )
            ju_tc.add_error_info(message=msg)
            return ju_tc
        else:
            return tc

    def write_testcase(self, build, machine, test, path, passed,
            start_ts, end_ts):
        delta = end_ts - start_ts
        tc = { 'name': test.name,
               'class': machine.getName(),
               'time_elapsed': delta.total_seconds(),
               'stdout': '\n'.join(self._harness.process_output(test, path)),
               'stderr': "",
               'passed': passed
        }
        if have_junit_xml:
            ju_tc = TestCase(
                    tc['name'],
                    tc['class'],
                    tc['time_elapsed'],
                    tc['stdout'],
                    )
            if not passed:
                errors = self._harness.extract_errors(test, path)
                errorstr = 'Failed'
                if errors is not None and len(errors) > 0:
                    errorstr += ': ' + ''.join([ unicode(l, errors='replace') for l in errors])
                ju_tc.add_failure_info(message=errorstr)
            return ju_tc
        else:
            return tc

    def testcase_passed(self, testcase):
        if have_junit_xml:
            return not (testcase.is_failure() or testcase.is_error() or testcase.is_skipped())
        else:
            return testcase['passed']

    def testcase_name(self, testcase):
        if have_junit_xml:
            return testcase.name
        else:
            return testcase['name']

    def write_xml_report(self, testcases, path):
        assert(have_junit_xml)
        debug.log("producing junit-xml report, file: %s" % path)
        ts = TestSuite('harness suite', testcases)
        with open(os.path.join(path, 'report.xml'), 'w') as f:
            TestSuite.to_file(f, [ts], prettyprint=False)

    def run_test(self, build, machine, test, co, testcases):
        debug.log('running test %s on %s, cwd is %s'
          % (test.name, machine.getName(), os.getcwd()))
        path = self.make_results_dir(build, machine, test)
        self.write_description(co, build, machine, test, path)
        start_timestamp = datetime.datetime.now()
        try:
            self._harness.run_test(build, machine, test, path)
        except TimeoutError:
            msg = 'Timeout while running test'
            if self._options.keepgoing:
                msg += ' (attempting to continue)'
            debug.error(msg)
            end_timestamp = datetime.datetime.now()
            testcases.append(self.write_errorcase(build, machine, test, path,
                msg + "\n" + traceback.format_exc(), start_timestamp, end_timestamp)
                )
            return False
        except Exception, e:
            msg = 'Exception while running test'
            if self._options.keepgoing:
                msg += ' (attempting to continue):'
            debug.error(msg)
            debug.error(str(e))
            end_timestamp = datetime.datetime.now()
            testcases.append(self.write_errorcase(build, machine, test, path,
                msg + "\n" + traceback.format_exc(), start_timestamp, end_timestamp)
                )
            traceback.print_exc()
            return False

        end_timestamp = datetime.datetime.now()
        debug.log('test complete, processing results')
        try:
            passed = self._harness.process_results(test, path)
            debug.log('result: %s' % ("PASS" if passed else "FAIL"))
        except Exception:
            passed = False
            msg = 'Exception while processing results'
            if self._options.keepgoing:
                msg += ' (attempting to continue):'
            debug.error(msg)
            if self._options.keepgoing:
                traceback.print_exc()

        testcases.append(
                self.write_testcase(build, machine, test, path, passed,
                    start_timestamp, end_timestamp))
        return passed

    def execute_tests(self, co, buildarchs, testcases):
        for build in self._options.builds:
            debug.log('starting build: %s' % build.name)
            build.configure(co, buildarchs)
            for machine in self._options.machines:
                passed = True
                for test in self._options.tests:
                    passed = self.run_test(build, machine, test, co, testcases)
                    if not passed and not self._options.keepgoing:
                        # Stop looping tests if keep going is not true and there
                        # was an error
                        break
                # produce JUnit style xml report if requested
                if self._options.xml:
                    path = self.make_run_dir(build, machine)
                    self.write_xml_report(testcases, path)
                # Did we encounter an error?
                if not passed and not self._options.keepgoing:
                    return

    def main(self):
        retval = True  # everything was OK
        co = checkout.create_for_dir(self._options.sourcedir)

        # determine build architectures
        buildarchs = set()
        for m in self._options.machines:
            buildarchs |= set(m.get_buildarchs())
        buildarchs = list(buildarchs)

        testcases = []

        self.execute_tests(co, buildarchs, testcases)

        pcount = len([ t for t in testcases if self.testcase_passed(t) ])
        debug.log('\n%d/%d tests passed' % (pcount, len(testcases)))
        if pcount < len(testcases):
            debug.log('Failed tests:')
            for t in [ t for t in testcases if not self.testcase_passed(t) ]:
                debug.log(' * %s' % self.testcase_name(t))
            # return False if we had test failures
            retval = False
        debug.log('all done!')
        return retval

if __name__ == "__main__":
    options = parse_args()
    scalebench = Scalebench(options)
    if not scalebench.main():
        sys.exit(1)  # one or more tests failed
