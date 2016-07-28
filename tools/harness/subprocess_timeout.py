##########################################################################
# Copyright (c) 2009-2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

from threading import Timer

# Wait for Popen instance p for timeout seconds and terminate/kill it after
# the timeout expires
# Adapted from
# http://stackoverflow.com/questions/1191374/using-module-subprocess-with-timeout
def wait_or_terminate(p, timeout=5):

    # Kill process if terminate doesn't exit in 1 second
    def cleanup(x):
        k = lambda y: y.kill()
        timer2 = Timer(1, k, [x])
        try:
            timer2.start()
            x.terminate()
        finally:
            timer2.cancel()

    # Termiate process if it doesn't voluntarily exit in `timeout` seconds
    timer = Timer(timeout, cleanup, [p])
    try:
        timer.start()
        p.wait()
    finally:
        timer.cancel()
