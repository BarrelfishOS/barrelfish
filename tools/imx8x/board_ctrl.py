#!/usr/bin/python3
##########################################################################
# Copyright (c) 2019 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
# Attn: Systems Group.
#
# Script to control the Toradex boards over the USB UART
#
##########################################################################

import os
import sys
import time
import usb
import argparse
import cmd

class AOSBoard:
    BITMODE_CBUS = 0x20
    SIO_SET_BITMODE_REQUEST = 0x0b
    
    def __init__(self, dev):
        self._dev = dev

    def _ftdi_set_bitmode(self, bitmask):
        """
        FTDIs CBUS bitmode expect the following value:
        CBUS Bits
        3210 3210
             |------ Output Control 0->LO, 1->HI
        |----------- Input/Output   0->Input, 1->Output

        This script assumes:
        - CBUS3 connected to RESET_EXT# 
        - CBUS2 connected to OE# (recovery mode)
        """
        bm_request_type = usb.util.build_request_type(
            usb.util.CTRL_OUT,
            usb.util.CTRL_TYPE_VENDOR,
            usb.util.CTRL_RECIPIENT_DEVICE
        )

        wValue = bitmask | (AOSBoard.BITMODE_CBUS << 32)
        self._dev.ctrl_transfer(bm_request_type, AOSBoard.SIO_SET_BITMODE_REQUEST, wValue)

    def on(self):
        # Set CBUS3 tristate, module run...
        self._ftdi_set_bitmode(0x00)

    def off(self):
        # Set CBUS3 low, module in reset...
        self._ftdi_set_bitmode(0x80)

    def reset(self):
        self.off()
        time.sleep(0.1)
        self.on()

    def enter_recovery(self):
        # Set recovery bit low
        self._ftdi_set_bitmode(0x40)
        time.sleep(0.2)
        # Set reset bit low
        self._ftdi_set_bitmode(0xC0)
        time.sleep(0.1)
        # Set reset bit tristate
        self._ftdi_set_bitmode(0x40)
        time.sleep(0.2)
        # Set recovery bit tristate
        self._ftdi_set_bitmode(0x00)

class BoardShell(cmd.Cmd):

    def __init__(self, board):
        super().__init__()
        self._board = board
        self.prompt = "aos-ctrl> "

    def do_on(self, args):
        "Turn the board on."
        self._board.on()

    def do_off(self, args):
        "Turn the board off."
        self._board.off()

    def do_reset(self, args):
        "Reset the board."
        self._board.reset()

    def do_recovery(self, args):
        "Reset the board and enter recovery mode."
        self._board.enter_recovery()

    def do_exit(self, args):
        "Exit the command prompt."
        return True

    def postcmd(self, stop, line):
        return stop

def main(args):
    dev = usb.core.find(
        custom_match=lambda d: \
            d.idVendor  == 0x0403 and
            d.idProduct == 0x6001 and
            (
                (args.board is None) or
                (d.serial_number == args.board)
            )
        )

    if dev is None:
        print("Board with serial '%s' not found." % (args.board), file=sys.stderr)
        exit(1)
    
    board = AOSBoard(dev)

    shell = BoardShell(board)
    if args.command is None:
        shell.cmdloop()
    else:
        shell.onecmd(args.command)
    
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Controls the Toradex Colibri boards")
    parser.add_argument(
        "--board", "-b", metavar="SERIAL", type=str,
        help="Serial of the board to control"
    )
    parser.add_argument(
        "command", metavar="CMD", type=str, nargs='?',
        help="Commands to run"
    )
    args = parser.parse_args()
    main(args)
