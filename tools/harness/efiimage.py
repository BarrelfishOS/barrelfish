#!/usr/bin/env python

##########################################################################
# Copyright (c) 2016, 2019 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import subprocess
import os, argparse, re

class EFIImage:

    def __init__(self, image, size):
        """
        Size in MiB
        """
        self._image = image
        # Size of the disk
        self._sizeMB= size * 1024 * 1024
        # block size
        self._blockSize = 512
        # size of the disk in blocks
        self._sizeBlocks= self._sizeMB / self._blockSize
        # first block of partition
        self._startBlock = 2048

        # size of partition in blocks
        self._partSizeBlocks = self._sizeBlocks - self._startBlock

        # calculate byte offset to first partition and format as mformat name
        self._mformatImage="%s@@%d" % (self._image, self._startBlock*self._blockSize)

        self._dirs = None

    def _cmd(self, command, **kwargs):
        print(" ".join(command))
        return subprocess.check_call(command, **kwargs)


    def create(self):
        self._cmd(["dd", "if=/dev/zero", "of=%s" % self._image,
                  "bs=%d" % self._blockSize, "count=1",
                  "seek=%d" % (self._sizeBlocks - 1)])

        self._cmd(["/sbin/parted", "-s", self._image, "mktable", "gpt"])
        self._cmd(["/sbin/parted", "-s", self._image, "mkpart", "primary", "fat32",
                  "%ds" % self._startBlock, "%ds" % self._partSizeBlocks])
        self._cmd(["/sbin/parted", "-s", self._image, "align-check", "optimal", "1"])
        self._cmd(["/sbin/parted", "-s", self._image, "name", "1", "UEFI"])

        self._cmd(["mformat", "-i", self._mformatImage, "-T",
                  str(self._partSizeBlocks), "-h", "1", "-s", "1"])
        # mdir fails if the root directory is empty. We create a directory here
        # to make sure _initDirCache does not fail.
        self._cmd(["mmd", "-i", self._mformatImage, "dummy"])

        # reset directory cache
        self._dirs = None

    def _initDirCache(self):
        if not self._dirs is None:
            return
        self._dirs = set()
        cmd = ["mdir", "-i", self._mformatImage, "-/b"]
        print(" ".join(cmd))
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
        for line in proc.stdout:
            if line.endswith("/"):
                self._dirs.add(line[:-1])

    def _createParentDir(self, dirName):
        """ Create a parent directory for passed directory name """
        parentDir = os.path.dirname(dirName)
        if parentDir is "":
            return "::"
        basename = "::/%s" % parentDir
        if not basename in self._dirs:
            self._createParentDir(parentDir)
            self._cmd(["mmd", "-i", self._mformatImage, basename])
            self._dirs.add(basename)
        return basename

    def addFile(self, inFile, fileName):
        if self._dirs is None:
            self._initDirCache()
        dirName = self._createParentDir(fileName)
        targetFile = os.path.join(dirName, os.path.basename(fileName))
        self._cmd(["mcopy", "-o", "-s", "-i", self._mformatImage, inFile, targetFile])

    def writeFile(self, fileName, contents):
        if self._dirs is None:
            self._initDirCache()
        dirName = self._createParentDir(fileName)
        cmd = ["mcopy", "-o", "-s", "-i", self._mformatImage, "-", os.path.join(dirName, os.path.basename(fileName))]
        print(" ".join(cmd))
        proc = subprocess.Popen(cmd, stdin=subprocess.PIPE)
        proc.communicate(contents)
        proc.stdin.close()


# 
# Command line and interface functions
#

def build_bf_efi_img(img_file, build_dir, modules, menulst, hagfish):
    hagfish_default = os.path.abspath(os.path.join(
        os.path.dirname(os.path.realpath(__file__)),
        "..","hagfish","Hagfish.efi"))
    if hagfish is None:
        hagfish = hagfish_default

    efi = EFIImage(img_file, 200)
    efi.create()
    for module in modules:
        if module[0] == "/":
            module = module[1:]
        efi.addFile(os.path.join(build_dir, module), module)
    efi.writeFile("startup.nsh", "Hagfish.efi hagfish.cfg")
    efi.addFile(hagfish, "Hagfish.efi")
    efi.addFile(menulst, "hagfish.cfg")

def parse_menu_lst(menulst):
    with open(menulst) as f:
        res = []
        for line in f.readlines():
            line = line.strip()
            if line=="" or line[0] == "#": continue
            parts = re.split("\s+", line, 2)
            if len(parts) < 1:
                print("Ignoring line: %s" % line)
                continue
            if parts[0] in ["stack"]:
                # no module
                continue
            res.append(parts[1])

    return res

def parse_args():
    p = argparse.ArgumentParser(
        description='Build EFI image for ARMv8')
    p.add_argument('menulst', help='the menu.lst')
    p.add_argument('build', help='build directory')
    p.add_argument('file', help='output image')
    p.add_argument('--hagfish', help='hagfish location', default=None)
    return p.parse_args()

if __name__ == "__main__":
    print("Barrelfish EFI Image Builder")
    args = parse_args()
    print("Image File: %s" % args.file)
    print("menu.lst: %s" % args.menulst)
    modules = parse_menu_lst(args.menulst)
    print("Modules: %s" % ",".join(modules))
    build_bf_efi_img(args.file, args.build, modules, args.menulst, args.hagfish)
