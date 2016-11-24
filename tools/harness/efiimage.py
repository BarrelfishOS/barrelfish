##########################################################################
# Copyright (c) 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import subprocess
import os

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
