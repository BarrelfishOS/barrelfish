--------------------------------------------------------------------------
-- Copyright (c) 2007-2015, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Universitätstr. 6, CH-8092 Zurich. Attn: Systems Group.
--
-- Architectural definitions for Barrelfish on x86_32.
-- 
--------------------------------------------------------------------------

module X86_32 where

import HakeTypes
import qualified Config
import qualified ArchDefaults

-------------------------------------------------------------------------
--
-- Architecture specific definitions for x86_32
--
-------------------------------------------------------------------------

arch = "x86_32"
archFamily = "x86_32"

compiler    = Config.x86_cc
cxxcompiler = Config.x86_cxx
objcopy     = Config.x86_objcopy

ourCommonFlags = [ Str "-m32",
                   Str "-mno-red-zone",
                   Str "-fPIE",
                   Str "-fno-stack-protector", 
                   Str "-Wno-unused-but-set-variable",
                   Str "-Wno-packed-bitfield-compat",
                   Str "-D__x86__" ]

cFlags = ArchDefaults.commonCFlags
         ++ ArchDefaults.commonFlags
         ++ ourCommonFlags

cxxFlags = ArchDefaults.commonCxxFlags
           ++ ArchDefaults.commonFlags
           ++ ourCommonFlags

cDefines = ArchDefaults.cDefines options

ourLdFlags = [ Str "-Wl,-section-start,.text=0x300000",
               Str "-Wl,-section-start,.init_array=0x800000",
               Str "-Wl,-z,max-page-size=0x1000",
               Str "-Wl,--build-id=none",
               Str "-m32"]

ldFlags = ArchDefaults.ldFlags arch ++ ourLdFlags
ldCxxFlags = ArchDefaults.ldCxxFlags arch ++ ourLdFlags

options = (ArchDefaults.options arch archFamily) { 
            optFlags = cFlags,
            optCxxFlags = cxxFlags,
            optDefines = cDefines,
            optLibs = (ArchDefaults.stdLibs arch),
            optLdFlags = ldFlags,
            optLdCxxFlags = [],
            optInterconnectDrivers = ["lmp", "ump", "multihop"],
            optFlounderBackends = ["lmp", "ump", "multihop"]
          }

--
-- The kernel is "different"
--

kernelCFlags = [ Str s | s <- [ "-fno-builtin",
                                "-nostdinc",
                                "-std=c99",
                                "-m32",
                                "-mno-red-zone",
                                "-fPIE",
                                "-fno-stack-protector",
                                "-U__linux__",
                                "-Wall",
                                "-Wshadow",
                                "-Wstrict-prototypes",
                                "-Wold-style-definition",
                                "-Wmissing-prototypes",
                                "-Wmissing-declarations",
                                "-Wmissing-field-initializers",
                                "-Wredundant-decls",
                                "-Wno-packed-bitfield-compat",
                                "-Wno-unused-but-set-variable",
                                "-Werror",
                                "-imacros deputy/nodeputy.h",
                                "-mno-mmx",
                                "-mno-sse",
                                "-mno-sse2",
                                "-mno-sse3",
                                "-mno-sse4.1",
                                "-mno-sse4.2",
                                "-mno-sse4",
                                "-mno-sse4a",
                                "-mno-3dnow" ]]

kernelLdFlags = [ Str s | s <- [ "-Wl,-N",
                                 "-pie",
                                 "-fno-builtin",
                                 "-nostdlib",
                                 "-Wl,--fatal-warnings",
                                 "-m32" ] ]

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
-- Link the kernel (CPU Driver)
-- 
linkKernel :: Options -> [String] -> [String] -> String -> HRule
linkKernel opts objs libs kbin = 
    Rules [ Rule ([ Str compiler ] ++
                    map Str Config.cOptFlags ++
                  [ NStr "-T", In BuildTree arch "/kernel/linker.lds",
                    Str "-o", Out arch kbin 
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
                    Out arch kbin, 
                    Str "bs=1 seek=16 count=1 conv=notrunc status=noxfer"
                  ]
                 ),
            Rule [ Str "cpp", 
                   NStr "-I", NoDep SrcTree "src" "/kernel/include/",
                   Str "-D__ASSEMBLER__", 
                   Str "-P", In SrcTree "src" "/kernel/arch/x86_32/linker.lds.in",
                   Out arch "/kernel/linker.lds"
                 ],
            -- Produce a stripped binary
            Rule [ Str objcopy,
                   Str "-g",
                   In BuildTree arch kbin,
                   Out arch (kbin++ ".stripped")
                 ]
          ]

