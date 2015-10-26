--------------------------------------------------------------------------
-- Copyright (c) 2007-2015, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Universitätstasse 6, CH-8092 Zurich. Attn: Systems Group.
--
-- Architectural definitions for Barrelfish on ARMv5 ISA.
--
-- The build target is the XScale with XScale cpu.
--
--------------------------------------------------------------------------

module XScale where

import HakeTypes
import qualified Config
import qualified ArchDefaults

-------------------------------------------------------------------------
--
-- Architecture specific definitions for ARM
--
-------------------------------------------------------------------------

arch = "xscale"
archFamily = "arm"

compiler    = Config.armeb_cc
objcopy     = Config.armeb_objcopy
objdump     = Config.armeb_objdump
ar          = Config.armeb_ar
ranlib      = Config.armeb_ranlib
cxxcompiler = Config.armeb_cxx

ourCommonFlags = [ Str "-Wno-packed-bitfield-compat",
                   Str "-Wno-format",
                   Str "-fno-unwind-tables",
                   Str "-fshort-enums",
                   Str "-mcpu=xscale",
                   Str "-mbig-endian",
                   Str "-mapcs",
                   Str "-mabi=aapcs-linux",
                   Str "-msingle-pic-base",
                   Str "-mpic-register=r10",
                   Str "-DPIC_REGISTER=R10",
                   Str "-fpic",
                   Str "-ffixed-r9",
                   Str "-DTHREAD_REGISTER=R9",
                   Str "-D__ARM_ARCH_5__" ]

cFlags = ArchDefaults.commonCFlags 
         ++ ArchDefaults.commonFlags
         ++ ourCommonFlags

cxxFlags = ArchDefaults.commonCxxFlags
           ++ ArchDefaults.commonFlags
           ++ ourCommonFlags

cDefines = ArchDefaults.cDefines options

ourLdFlags = [ Str "-Wl,-section-start,.text=0x400000",
               Str "-Wl,-section-start,.data=0x600000",
               Str "-mcpu=xscale",
               Str "-mbig-endian" ]

ldFlags = ArchDefaults.ldFlags arch ++ ourLdFlags
ldCxxFlags = ArchDefaults.ldCxxFlags arch ++ ourLdFlags

stdLibs = ArchDefaults.stdLibs arch ++ [ Str "-lgcc" ]

options = (ArchDefaults.options arch archFamily) { 
            optFlags = cFlags,
            optCxxFlags = cxxFlags,
            optDefines = cDefines,
            optDependencies = 
                [ PreDep BuildTree   arch "/include/errors/errno.h",
                  PreDep InstallTree arch "/include/barrelfish_kpi/capbits.h",
                  PreDep InstallTree arch "/include/asmoffsets.h", 
                  PreDep InstallTree arch "/include/romfs_size.h" ],
            optLdFlags = ldFlags,
            optLdCxxFlags = ldCxxFlags,
            optLibs = stdLibs,
            optInterconnectDrivers = ["lmp"],
            optFlounderBackends = ["lmp"]
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
cxxlinker = ArchDefaults.cxxlinker arch cxxcompiler

--
-- The kernel is "different"
--

kernelCFlags = [ Str s | s <- [ "-fno-builtin",
                                "-fno-unwind-tables",
                                "-fshort-enums",
                                "-nostdinc",
                                "-std=c99",
                                "-mcpu=xscale",
                                "-mbig-endian",
                                "-mapcs",
                                "-mabi=aapcs-linux",
                                "-fPIE",
                                "-U__linux__",
                                "-Wall",
                                "-Wshadow",
                                "-Wstrict-prototypes",
                                "-Wold-style-definition",
                                "-Wmissing-prototypes",
                                "-Wmissing-declarations",
                                "-Wmissing-field-initializers",
                                "-Wno-implicit-function-declaration",
                                "-Wredundant-decls",
                                "-Werror",
                                "-imacros deputy/nodeputy.h",
                                "-fpie",
                                "-fno-stack-check",
                                "-ffreestanding",
                                "-fomit-frame-pointer",
                                "-mno-long-calls",
                                "-Wno-missing-noreturn",
                                "-Wno-format",
                                "-mno-apcs-stack-check",
                                "-mno-apcs-reentrant",
                                "-msingle-pic-base",
                                "-mpic-register=r10",
                                "-DPIC_REGISTER=R10",
                                "-ffixed-r9",
                                "-DTHREAD_REGISTER=R9",
                                "-D__ARM_ARCH_5__" ]]

kernelLdFlags = [ Str "-Wl,-N",
                  NStr "-Wl,-Map,", Out arch "kernel.map",
                  Str "-fno-builtin",
                  Str "-nostdlib",
                  Str "-Wl,--fatal-warnings",
          Str "-mcpu=xscale",
          Str "-mbig-endian"
                ]


--
-- Link the kernel (CPU Driver)
--
linkKernel :: Options -> [String] -> [String] -> String -> HRule
linkKernel opts objs libs kbin =
    let linkscript = "/kernel/linker.lds"
        kbootable  = kbin ++ ".bin"
    in
        Rules [ Rule ([ Str compiler ] ++
                      map Str Config.cOptFlags ++
                      [ NStr "-T", In BuildTree arch linkscript,
                        Str "-o", Out arch kbin
                      ]
                    ++ (optLdFlags opts)
                    ++
                    [ In BuildTree arch o | o <- objs ]
                    ++
                    [ In BuildTree arch l | l <- libs ]
                    ++
                    [ Str "-lgcc" ]
                   ),
              -- Edit ELF header so qemu-system-arm will treat it as a Linux kernel
              Rule [ In SrcTree "src" "/tools/arm-mkbootelf.sh",
                     Str objdump, In BuildTree arch kbin, Out arch (kbootable)],
              -- Generate kernel assembly dump
              Rule [ Str (objdump ++ " -d -M reg-names-raw"),
                    In SrcTree arch kbin, Str ">", Out arch (kbin ++ ".asm")],
              Rule [ Str "cpp",
                     NStr "-I", NoDep SrcTree "src" "/kernel/include/arch/xscale",
                     Str "-D__ASSEMBLER__",
                     Str "-P", In SrcTree "src" "/kernel/arch/xscale/linker.lds.in",
                     Out arch linkscript
                   ]
            ]
