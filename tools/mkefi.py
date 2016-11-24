#!/usr/bin/python
##########################################################################
# Copyright (c) 2016 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
# Attn: Systems Group.
#
# Create a Barrelfish EFI image
#
##########################################################################

import argparse
import subprocess
import os

import harness.efiimage

class FileExists(object):

    def __call__(self, string):
        if os.path.isfile(string):
            return os.path.abspath(string)
        else:
            message = _("not a file '%s'")
            raise argparse.ArgumentTypeError(msg)

parser = argparse.ArgumentParser()
parser.add_argument("image", help="image file to create")
parser.add_argument("content",
                    help="files to include in the image",
                    nargs='+',
                    type=FileExists())
parser.add_argument("--size", help="image size", type=int, default=200)
parser.add_argument("--create", help="create image, even if exists",
                    action="store_true")

args = parser.parse_args()


efi = harness.efiimage.EFIImage(args.image, args.size)
if args.create or not os.path.isfile(args.image):
    efi.create()
for f in args.content:
    efi.addFile(f, f)

