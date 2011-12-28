--------------------------------------------------------------------------
-- Copyright (c) 2009, 2010, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
--
-- Architectural definitions for Barrelfish on Beehive.
-- 
--------------------------------------------------------------------------

module Beehive where

import List
import HakeTypes
import Path
import qualified Config
import qualified ArchDefaults

-------------------------------------------------------------------------
--
-- Architecture specific definitions for Beehive
--
-------------------------------------------------------------------------

arch = "beehive"
archFamily = "beehive"

cpp      = "cpp"
compiler = "Bcc"
cxxcompiler = "Bcc"
objcopy  = "Beehive-is-not-a-gnu-bfd-target"
objdump  = "Beehive-is-not-a-gnu-bfd-target"
ar       = "Bar"
ranlib   = "Branlib"


ourCommonFlags = [ Str "-fno-omit-frame-pointer" ]
ourExcludeFlags = [ Str "-Ulinux", Str "-U__linux__", Str "-g" ]


cFlags = [ f | f <- ( ArchDefaults.commonCFlags ++ 
                      ArchDefaults.commonFlags ++ 
                      ourCommonFlags ) \\ ourExcludeFlags ]

cxxFlags = [ f | f <- ( ArchDefaults.commonCxxFlags ++ 
                        ArchDefaults.commonFlags ++ 
                        ourCommonFlags ) \\ ourExcludeFlags ]

cDefines = ArchDefaults.cDefines options
cStdIncs = ArchDefaults.cStdIncs arch archFamily

ldFlags = [ Str $ unwords $ (words Config.cOptFlags) \\ ["-g"],
            Str "-Wl,-N",
            Str "-Wl,-codebase=0", Str "-Wl,-datafloat",
            In InstallTree arch "/lib/crt0.o",
            In InstallTree arch "/lib/crtbegin.o", 
            Str "-fno-builtin" ]

ldCxxFlags = ldFlags

stdLibs = ArchDefaults.stdLibs arch 
          ++ [ Str "-lgcc" ]

options = (ArchDefaults.options arch archFamily) { 
            optFlags = cFlags,
            optCxxFlags = cxxFlags,
            optDefines = cDefines,
            optLdFlags = ldFlags,
            optLdCxxFlags = ldCxxFlags,
            optLibs = stdLibs,
            optInterconnectDrivers = ["lmp", "bmp"],
            optFlounderBackends = ["lmp", "bmp"]
          }

--
-- The kernel is "different"
--

kernelCFlags = [ Str s | s <- [ "-fno-builtin",
                                "-fno-omit-frame-pointer",
                                "-nostdinc",
                                "-std=c99",
                                "-Wall",
                                "-Wshadow",
                                "-Wstrict-prototypes",
                                "-Wold-style-definition",
                                "-Wmissing-prototypes",
                                "-Wmissing-declarations",
                                "-Wmissing-field-initializers",
                                "-Wredundant-decls",
                                "-Werror",
                                "-imacros deputy/nodeputy.h" ]]
                                -- "-Wmissing-noreturn" ]]

kernelLdFlags = [ Str "-Wl,-N",
                  Str "-Wl,-codebase=2000", Str "-Wl,-datafloat",
                  NStr "-Wl,-map,", Out arch "kernel.map",
                  Str "-fno-builtin"
                ]


------------------------------------------------------------------------
--
-- Now, commands to actually do something
--
------------------------------------------------------------------------

--
-- Compilers
--
cCompiler = ArchDefaults.cCompiler arch compiler
cPreprocessor = ArchDefaults.cPreprocessor arch compiler
cxxCompiler = ArchDefaults.cxxCompiler arch cxxcompiler
cToAssembler = ArchDefaults.cToAssembler arch compiler
assembler = ArchDefaults.assembler arch compiler
linker = ArchDefaults.linker arch compiler
cxxlinker = ArchDefaults.cxxlinker arch cxxcompiler

--
-- Create a library from a set of object files
--

--
-- Create a library from a set of object files
--
archive :: Options -> [String] -> [String] -> String -> [ RuleToken ]
archive opts objs libs libname =
    [ Str "rm -f ", Out arch libname ]
    ++ 
    [ NL, Str (ar ++ " cr "), Out arch libname ] 
    ++ 
    [ In BuildTree arch o | o <- objs ]
    ++ 
    [ NL, Str ranlib, Out arch libname ]

--
-- Create C file dependencies
--
makeDepend opts phase src obj depfile =
    let incls = (optIncludes opts) ++ (extraIncludes opts)
        flags = (optFlags opts) 
                ++ (optDefines opts)
                ++ [ Str f | f <- extraFlags opts ]
                ++ [ Str f | f <- extraDefines opts ]
        cpp_undef = "-undef -U__unix -U__unix__ -Uunix -U__CYGWIN__ -U__CYGWIN32__"
    in
      [ Str ('@':cpp), Str cpp_undef,
        Str "-imacros", NoDep SrcTree "src" "/hake/beehive-dM-noSTDC.txt" ]
      ++ filter (\f -> not (isPrefixOf "-Wmissing-field-initializers" (formatToken f))) flags
      ++ [ Str "-nostdinc -std=c99 -imacros" ] 
      ++ [ NoDep SrcTree "src" "/include/deputy/nodeputy.h" ]
      ++ concat [ [ NStr "-I", i ] | i <- incls ] 
      ++ (optDependencies opts) ++ (extraDependencies opts)
      ++ [ Str "-M -MF", 
           Out arch depfile,
           Str "-MQ", NoDep BuildTree arch obj, 
           Str "-MQ", NoDep BuildTree arch depfile,
           In (if phase == "src" then SrcTree else BuildTree) phase src
         ] 

makeCxxDepend = makeDepend

--
-- Link the kernel (CPU Driver)
-- 

linkKernel :: Options -> [String] -> [String] -> String -> HRule
linkKernel opts objs libs kbin = 
    Rules [ Rule (
           [ Str compiler ]
           ++ (optLdFlags opts)
           ++ (extraLdFlags opts)
           ++ [ Str "-o", Out arch kbin ]
           ++ [ In BuildTree arch o | o <- objs ]
           ++ [ In BuildTree arch l | l <- libs ]
           ++ [ Str "-lgcc" ]
    ) ]
