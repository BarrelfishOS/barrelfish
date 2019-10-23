#
# Copyright (c) 2009-2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
#

import os
import types
import string
import datetime
import debug
import re

class Harness:
    RAW_FILE_NAME = 'raw.txt'
    MENU_LST_FILE_NAME = 'menu.lst'
    BOOT_FILE_NAME = 'bootlog.txt'
    TERM_FILTER = re.compile("\[\d\d?m")

    def _clean_line(self, line):
        # filter output line of control characters
        filtered_out = filter(lambda c: c in string.printable, line.rstrip())
        # Delete terminal color codes from output
        filtered_out = self.TERM_FILTER.sub('', filtered_out)
        return filtered_out

    def _write_menu_lst_debug(self, test, build, machine, path):
        # Ignore for tests that do not implement get_modules
        if hasattr(test, "get_modules"):
            menu_lst_file_name = os.path.join(path, self.MENU_LST_FILE_NAME)
            debug.verbose("harness: writing menu.lst to %s" % menu_lst_file_name)
            with open(menu_lst_file_name, "w") as menu:
                menu.write( test.get_modules(build, machine).get_menu_data("/") )

    def run_test(self, build, machine, test, path):
        # Open files for raw output from the victim and log data from the test
        raw_file_name = os.path.join(path, self.RAW_FILE_NAME)
        debug.verbose('open %s for raw output' % raw_file_name)
        raw_file = open(raw_file_name, 'w')

        # run the test, dumping the output to the raw file as we go
        try:
            debug.verbose('harness: setup test')
            test.setup(build, machine, path)
            self._write_menu_lst_debug(test, build, machine, path)
            debug.verbose('harness: run test')
            starttime = datetime.datetime.now()
            for out in test.run(build, machine, path):
                # timedelta for the time this line was emitted from the start of the run
                timestamp = datetime.datetime.now() - starttime
                # format as string, discarding sub-second precision
                timestr = str(timestamp).split('.', 1)[0]
                debug.debug('[%s] %s' % (timestr, self._clean_line(out)))
                # log full raw line (without timestamp) to output file
                raw_file.write(out)
            debug.verbose('harness: output complete')
        except KeyboardInterrupt:
            # let the user know that we are on our way out
            debug.error('Interrupted! Performing cleanup...')
            raise
        finally:
            raw_file.close()
            debug.verbose('harness: cleanup test')
            test.cleanup(machine)

    def process_output(self, test, path):
        """Process raw.txt and return array of output lines that begins with grubs
        output, avoids having encoding issues when generating other report files"""

        raw_file_name = os.path.join(path, self.RAW_FILE_NAME)

        if os.path.exists(raw_file_name):
            idx = 0
            with open(raw_file_name, 'r') as rf:
                lines = rf.readlines()
                for idx, line in enumerate(lines):
                    if line.strip() == "root (nd)" or \
                       line.strip().startswith("Kernel starting at address") or \
                       "ARMv8-A: Barrelfish CPU driver starting on ARMv8" in line:
                            break
                if idx == len(lines)-1:
                    debug.verbose('magic string "root (nd)" or "Kernel starting at address" not found, assuming no garbage in output')
                    idx=0

            return [ unicode(self._clean_line(l), errors='replace') for l in lines[idx:] ]

        # file did not exist
        return ["could not open %s to process test output" % raw_file_name]

    def extract_errors(self, test, path):
        raw_file_name = os.path.join(path, self.RAW_FILE_NAME)
        debug.verbose('open %s for raw input' % raw_file_name)
        raw_file = open(raw_file_name, 'r')

        try:
            results = test.process_data(path, raw_file)
        finally:
            raw_file.close()

        errors = [results.reason()]
        try:
            errors += results.errors
        except:
            pass

        return errors


    def process_results(self, test, path):
        # open raw file for input processing
        raw_file_name = os.path.join(path, self.RAW_FILE_NAME)
        debug.verbose('open %s for raw input' % raw_file_name)
        raw_file = open(raw_file_name, 'r')

        try:
            results = test.process_data(path, raw_file)
        finally:
            raw_file.close()
        if not results:
            debug.verbose('no results')
            return True  # no results, assume success

        retval = True  # everything OK

        # Process raw.txt and make a bootlog.txt that begins with grubs or
        # Barrelfish's output, avoids having encoding issues when viewing logfiles
        boot_file_name = os.path.join(path, self.BOOT_FILE_NAME)
        if os.path.exists(raw_file_name):
            idx = 0
            with open(raw_file_name, 'r') as rf:
                lines = rf.readlines()
                for idx, line in enumerate(lines):
                    if line.strip() == "root (nd)" or \
                       "Barrelfish CPU driver starting" in line.strip():
                        break
            if idx > 0:
                with open(boot_file_name, 'w') as wf:
                    wf.writelines(lines[idx:])
            else:
                debug.verbose('Magic string root (nd) not found, do not write bootlog.txt')
        else:
            debug.verbose('No file named %s exists. Do not create bootlog.txt.' % raw_file_name)

        # if a single result, turn it into a list
        if not isinstance(results, types.ListType):
            results = [results]
        for result in results:
            # see if it passed
            try:
                passed = result.passed()
            except NotImplementedError:
                passed = None
            if passed is False:
                debug.log('Test %s FAILED %s' % (test.name, '(' + result.reason() + ')') )
                retval = False
            elif passed:
                debug.verbose('Test %s PASSED' % test.name)

            # write it to a file
            name = result.name if result.name else 'results'
            data_file_name = os.path.join(path, name + '.dat')
            debug.verbose('create %s for processed output' % data_file_name)
            data_file = open(data_file_name, 'w')
            try:
                result.to_file(data_file)
                data_file.close()
            except NotImplementedError:
                debug.verbose('no processed output, remove %s' % data_file_name)
                data_file.close()
                os.remove(data_file_name)

        return retval
