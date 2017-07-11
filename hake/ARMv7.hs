--------------------------------------------------------------------------
-- Copyright (c) 2007-2010, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Universitaetstasse 6, CH-8092 Zurich. Attn: Systems Group.
--
-- Architectural definitions for Barrelfish on ARMv5 ISA.
--
-- The build target is the integratorcp board on QEMU with the default
-- ARM926EJ-S cpu.
--
--------------------------------------------------------------------------

module ARMv7 where

import HakeTypes
import qualified Config
import qualified ArchDefaults

-------------------------------------------------------------------------
--
-- Architecture specific definitions for ARM
--
-------------------------------------------------------------------------

arch = "armv7"
archFamily = "arm"

compiler    = Config.arm_cc
objcopy     = Config.arm_objcopy
objdump     = Config.arm_objdump
ar          = Config.arm_ar
ranlib      = Config.arm_ranlib
cxxcompiler = Config.arm_cxx

ourCommonFlags = [ Str "-fno-unwind-tables",
                   Str "-Wno-packed-bitfield-compat",
                   Str "-marm",
                   Str "-fno-stack-protector",
                   Str "-mcpu=cortex-a9",
                   Str "-march=armv7-a",
                   Str "-mapcs",
                   Str "-mabi=aapcs-linux",
                   Str "-mfloat-abi=hard",
                   Str "-mfpu=vfpv3",
                   Str "-msingle-pic-base",
                   Str "-mpic-register=r9",
                   Str "-DPIC_REGISTER=R9",
                   Str "-fPIE",
                   Str "-D__ARM_CORTEX__",
                   Str "-D__ARM_ARCH_7A__",
                   Str "-Wno-unused-but-set-variable",
                   Str "-Wno-suggest-attribute=noreturn",
                   Str "-Wno-format"
 ]

cFlags = ArchDefaults.commonCFlags
         ++ ArchDefaults.commonFlags
         ++ ourCommonFlags

cxxFlags = ArchDefaults.commonCxxFlags
           ++ ArchDefaults.commonFlags
           ++ ourCommonFlags

cDefines = ArchDefaults.cDefines options

ourLdFlags = [ Str "-Wl,-section-start,.text=0x400000",
               Str "-Wl,--build-id=none",
               Str "-static" ]

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
            optInterconnectDrivers = ["lmp", "ump", "local"],
            optFlounderBackends = ["lmp", "ump", "local"]
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
strip = ArchDefaults.strip arch objcopy
debug = ArchDefaults.debug arch objcopy
cxxlinker = ArchDefaults.cxxlinker arch cxxcompiler

--
-- The kernel is "different"
--

kernelCFlags = [ Str s | s <- [ "-fno-builtin",
                                "-fno-unwind-tables",
                                "-nostdinc",
                                "-std=c99",
                                "-marm",
                                "-mcpu=cortex-a9",
                                "-march=armv7-a",
                                "-mfpu=vfpv3",
                                "-mapcs",
                                "-mabi=aapcs-linux",
                                "-mfloat-abi=hard",
                                "-fPIE",
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
                                "-mno-long-calls",
                                "-Wmissing-noreturn",
                                "-mno-apcs-stack-check",
                                "-mno-apcs-reentrant",
                                "-msingle-pic-base",
                                "-mno-pic-data-is-text-relative",
                                "-mpic-register=r9",
                                "-DPIC_REGISTER=R9",
                                "-D__ARM_CORTEX__",
                                "-D__ARM_ARCH_7A__",
                                "-Wno-unused-but-set-variable",
                                "-Wno-suggest-attribute=noreturn",
                                "-Wno-format" ]]

kernelLdFlags = [ Str "-Wl,-N",
                  Str "-fno-builtin",
                  Str "-nostdlib",
                  Str "-pie",
                  Str "-Wl,--fatal-warnings",
                  Str "-Wl,--dynamic-list-data",
                  Str "-Wl,--export-dynamic"
                ]


--
-- Link the kernel (CPU Driver)
--
linkKernel :: Options -> [String] -> [String] -> String -> String -> HRule
linkKernel opts objs libs name driverType =
    let linkscript = "/kernel/" ++ name ++ ".lds"
        kernelmap  = "/kernel/" ++ name ++ ".map"
        kasmdump   = "/kernel/" ++ name ++ ".asm"
        kbinary    = "/sbin/" ++ name
        kbootable  = kbinary ++ ".bin"
    in
        Rules [ Rule ([ Str compiler ] ++
                    map Str Config.cOptFlags ++
                    [ NStr "-T", In BuildTree arch linkscript,
                      Str "-o", Out arch kbinary,
                      NStr "-Wl,-Map,", Out arch kernelmap
                    ]
                    ++ (optLdFlags opts)
                    ++
                    [ In BuildTree arch o | o <- objs ]
                    ++
                    [ In BuildTree arch l | l <- libs ]
                    ++
                    (ArchDefaults.kernelLibs arch)
                   ),
              -- Generate kernel assembly dump
              Rule [ Str objdump,
                     Str "-d",
                     Str "-M reg-names-raw",
                     In BuildTree arch kbinary,
                     Str ">", Out arch kasmdump ],
              Rule [ Str "cpp",
                     NStr "-I", NoDep SrcTree "src" "/kernel/include/arch/armv7",
                     Str "-D__ASSEMBLER__",
                     Str "-P",
                        In SrcTree "src"
                           ("/kernel/arch/armv7/"++driverType++".lds.in"),
                     Out arch linkscript
                   ],
              -- Produce a stripped binary
              Rule [ Str objcopy,
                     Str "-g",
                     In BuildTree arch kbinary,
                     Out arch (kbinary ++ ".stripped")
                   ]
            ]
