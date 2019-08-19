--------------------------------------------------------------------------
-- Copyright (c) 2015, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich.
-- Attn: Systems Group.
--
-- Architectural definitions for Barrelfish on ARMv8.
--
--------------------------------------------------------------------------

module ARMv8 where

import HakeTypes
import qualified Config
import qualified ArchDefaults
import Data.Char

-------------------------------------------------------------------------
--
-- Architecture specific definitions for ARM
--
-------------------------------------------------------------------------

arch = "armv8"
archFamily = "aarch64"

compiler    = Config.aarch64_cc
objcopy     = Config.aarch64_objcopy
objdump     = Config.aarch64_objdump
ar          = Config.aarch64_ar
ranlib      = Config.aarch64_ranlib
cxxcompiler = Config.aarch64_cxx

ourCommonFlags = [ Str "-fno-unwind-tables",
                   Str "-Wno-packed-bitfield-compat",
                   Str "-fno-stack-protector",
                   Str "-mcpu=cortex-a57",
                   Str "-march=armv8-a",
                   Str "-mabi=lp64",
                   Str "-mstrict-align",
                   Str "-fPIE",
                   -- The dispatcher needs a scratch register on ARMv8.
                   -- We use r18, which is reserved as the 'platform register'
                   -- by the ARM-64 ABI.
                   Str "-ffixed-x18",
                   Str "-D__ARM_CORTEX__",
                   Str "-D__ARM_ARCH_8A__",
              --     Str "-DPREFER_SIZE_OVER_SPEED",
                   Str "-Wno-unused-but-set-variable",
                   Str "-Wno-format"
 ]

cFlags = ArchDefaults.commonCFlags
         ++ ArchDefaults.commonFlags
         ++ ourCommonFlags

cxxFlags = ArchDefaults.commonCxxFlags
           ++ ArchDefaults.commonFlags
           ++ ourCommonFlags
           ++ [Str "-std=gnu++11"]

cDefines = ArchDefaults.cDefines options

ourLdFlags = [ Str "-Wl,--build-id=none", Str "-static" ]

ldFlags = ArchDefaults.ldFlags arch ++ ourLdFlags
ldCxxFlags = ArchDefaults.ldCxxFlags arch ++ ourLdFlags

stdLibs = ArchDefaults.stdLibs arch

options = (ArchDefaults.options arch archFamily) {
            optFlags = cFlags,
            optCxxFlags = cxxFlags,
            optDefines = cDefines,
            optDependencies =
                [ PreDep InstallTree arch "/include/trace_definitions/trace_defs.h",
                  PreDep InstallTree arch "/include/errors/errno.h",
                  PreDep InstallTree arch "/include/barrelfish_kpi/capbits.h",
                  PreDep InstallTree arch "/include/asmoffsets.h"
                   ],
            optLdFlags = ldFlags,
            optLdCxxFlags = ldCxxFlags,
            optLibs = stdLibs,
            optInterconnectDrivers = ["lmp", "ump", "multihop", "local"],
            optFlounderBackends = ["lmp", "ump", "multihop", "local"]
          }

--
-- Compilers
--
cCompiler = ArchDefaults.cCompiler arch compiler Config.cOptFlags
cxxCompiler = ArchDefaults.cxxCompiler arch cxxcompiler Config.cOptFlags
makeDepend = ArchDefaults.makeDepend arch compiler
makeCxxDepend  = ArchDefaults.makeCxxDepend arch cxxcompiler
cToAssembler = ArchDefaults.cToAssembler arch compiler Config.cOptFlags
assembler = ArchDefaults.assembler arch compiler Config.cOptFlags
archive = ArchDefaults.archive arch
linker = ArchDefaults.linker arch compiler
ldtLinker = ArchDefaults.ldtLinker arch compiler
strip = ArchDefaults.strip arch objcopy
debug = ArchDefaults.debug arch objcopy
cxxlinker = ArchDefaults.cxxlinker arch cxxcompiler
ldtCxxlinker = ArchDefaults.ldtCxxlinker arch cxxcompiler


--
-- The kernel needs different flags
--

kernelCFlags = [ Str s | s <- [ "-fno-builtin",
                                "-fno-unwind-tables",
                                "-nostdinc",
                                "-std=c99",
                                "-mcpu=cortex-a57",
                                "-march=armv8-a+nofp",
                                "-mgeneral-regs-only",
                                "-mabi=lp64",
                                "-mstrict-align",
                                "-U__linux__",
                                "-Wall",
                                "-Wshadow",
                                "-Wstrict-prototypes",
                                "-Wold-style-definition",
                                "-Wmissing-prototypes",
                                "-Wmissing-declarations",
                                "-Wmissing-field-initializers",
                                "-Wredundant-decls",
                                "-Werror",
                                "-imacros deputy/nodeputy.h",
                                "-fno-stack-check",
                                "-ffreestanding",
                                "-fomit-frame-pointer",
                                "-Wmissing-noreturn",
                                "-D__ARM_CORTEX__",
                                "-D__ARM_ARCH_8A__",
                                "-DPREFER_SIZE_OVER_SPEED",
                                "-Wno-unused-but-set-variable",
                                "-Wno-format"
                              ]]

kernelLdFlags = [ Str "-Wl,-N",
                  Str "-pie",
                  Str "-fno-builtin",
                  Str "-nostdlib",
                  Str "-Wl,--fatal-warnings",
                  Str "-Wl,--build-id=none"
                ]


--
-- Link the kernel (CPU Driver)
--
linkKernel :: Options -> [String] -> [String] -> String -> String -> HRule
linkKernel opts objs libs name driverType=
    let linkscript = "/kernel/" ++ name ++ ".lds"
        kernelmap  = "/kernel/" ++ name ++ ".map"
        kasmdump   = "/kernel/" ++ name ++ ".asm"
        kbinary    = "/sbin/" ++ name
        kdebug     = kbinary ++ ".debug"
        kfull      = kbinary ++ ".full"
    in
        Rules ([ Rule ([ Str compiler ] ++
                    map Str Config.cOptFlags ++
                    [ NStr "-T", In BuildTree arch linkscript,
                      Str "-o",
                      Out arch kfull,
                      NStr "-Wl,-Map,", Out arch kernelmap
                    ]
                    ++ (optLdFlags opts)
                    ++
                    [ In BuildTree arch o | o <- objs ]
                    ++
                    [ In BuildTree arch l | l <- libs ]
                    ++
                    (ArchDefaults.kernelLibs arch)
                    ++
                    [ NL, NStr "bash -c \"echo -e '\\0002'\" | dd of=",
                      Out arch kfull,
                      Str "bs=1 seek=16 count=1 conv=notrunc status=noxfer"
                    ]
                   ),
             Rule $ strip opts kfull kdebug kbinary,
             Rule $ debug opts kfull kdebug,
              -- Generate kernel assembly dump
              Rule [ Str objdump,
                     Str "-d",
                     Str "-M reg-names-raw",
                     In BuildTree arch kbinary,
                     Str ">",
                     Out arch kasmdump ],
              Rule [ Str "cpp",
                     NStr "-I", NoDep SrcTree "src" "/kernel/include/arch/armv8",
                     NStr "-I", NoDep SrcTree "src" "/kernel/include",
                     Str "-D__ASSEMBLER__",
                     Str "-P", In SrcTree "src"
                           ("/kernel/arch/armv8/"++driverType++".lds.in"),
                     Out arch linkscript
                   ]
            ] ++ [ Phony ((map toUpper arch) ++ "_All") False
                 [ Dep BuildTree arch kbinary]])
