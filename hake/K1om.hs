--------------------------------------------------------------------------
-- Copyright (c) 2007-2010, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
--
-- Architectural definitions for Barrelfish on x86_mic.
--
-- This architecture is used to build for the Intel Xeon Phi architecture.
-- 
--------------------------------------------------------------------------

module K1om where

import HakeTypes
import Path
import qualified Config
import qualified ArchDefaults

-------------------------------------------------------------------------
--
-- Architecture specific definitions for X86_64-k1om
--
-------------------------------------------------------------------------

arch = "k1om"
archFamily = "k1om"

compiler = "k1om-mpss-linux-gcc"
objcopy  = "k1om-mpss-linux-objcopy"
objdump  = "k1om-mpss-linux-objdump"
ar       = "k1om-mpss-linux-ar"
ranlib   = "k1om-mpss-linux-anlib"
cxxcompiler = "k1om-mpss-linux-g++"


ourCommonFlags = [ Str "-m64",
                   Str "-mno-red-zone",
                   Str "-fPIE",
                   Str "-fno-stack-protector", 
                   Str "-Wno-unused-but-set-variable",
                   Str "-Wno-packed-bitfield-compat",
                   Str "-fno-tree-vectorize",
                   Str "-Wa,-march=k1om",
                   Str "-mk1om",
		   Str "-mtune=k1om",
-- Apparently the MPSS gcc somehow incudes CMOVES?
		   Str "-fno-if-conversion",		   
 --                  Str "-mno-mmx",
 --                  Str "-mno-sse",
 --                  Str "-mno-sse2",
 --                  Str "-mno-sse3",
 --                  Str "-mno-sse4.1",
 --                  Str "-mno-sse4.2",
 --                  Str "-mno-sse4",
--                   Str "-mno-sse4a",
--                   Str "-mno-3dnow", 
-- specific Xeon Phi architecture
                   Str "-D__x86__",
                   Str "-D__k1om__" ]


cFlags = ArchDefaults.commonCFlags
               ++ ArchDefaults.commonFlags
               ++ ourCommonFlags
               ++ [Str "-fno-builtin" ]

cxxFlags = ArchDefaults.commonCxxFlags
                 ++ ArchDefaults.commonFlags
                 ++ ourCommonFlags
		 ++ [Str "-std=gnu++0x"]  -- XXX: with the Intel GCC 4.7.0 still experimental

cDefines = ArchDefaults.cDefines options

-- TODO> -m elf_i386
ourLdFlags = [ Str "-Wl,-z,max-page-size=0x1000",
               Str "-Wl,--build-id=none",
               Str "-Wl,-melf_k1om",               
               Str "-m64" ]


ldFlags = ArchDefaults.ldFlags arch ++ ourLdFlags
ldCxxFlags = ArchDefaults.ldCxxFlags arch ++ ourLdFlags

-- adding x86_64 includes to the K1OM architecture
kernelOptIncludes = [NoDep SrcTree "src" ("/kernel/include/arch/x86_64"),
                     NoDep SrcTree "src" ("/include/target/x86_64"), 
                     NoDep SrcTree "src" ("/include/arch/x86_64")] 

options = (ArchDefaults.options arch archFamily) { 
            optFlags = cFlags,
            optCxxFlags = cxxFlags,
            optDefines = cDefines,
            optLdFlags = ldFlags,
            optLdCxxFlags = ldCxxFlags,
            optInterconnectDrivers = ["lmp", "ump", "multihop"],
            optFlounderBackends = ["lmp", "ump", "multihop"],
            optIncludes = ArchDefaults.cStdIncs arch archFamily
                          ++
                          [NoDep SrcTree "src" ("/include/target/x86_64"), 
                           NoDep SrcTree "src" ("/include/arch/x86_64")]
          }

--
-- The kernel is "different"
--

kernelCFlags = [ Str s | s <- [ "-fno-builtin",
                                "-nostdinc",
                                "-std=c99",
                                "-m64",
                                "-fPIE", 
                                "-e start",
                                "-mno-red-zone",
                                "-mk1om",
                                "-Wa,-march=k1om",
                                "-fno-stack-protector",
                                "-fomit-frame-pointer",
                                "-U__linux__",
                                "-D__k1om__",
                                "-D__x86__",
                                "-mk1om",
                                "-Wall",
                                "-Wa,-march=k1om",
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
                                "-fno-tree-vectorize",
                                "-mno-mmx",
                                "-mno-sse",
                                "-mno-sse2",
                                "-mno-sse3",
                                "-mno-sse4.1",
                                "-mno-sse4.2",
                                "-mno-sse4",
                                "-mno-sse4a",
                                "-mno-3dnow", 
-- Apparently the MPSS gcc somehow incudes CMOVES?
		                "-fno-if-conversion" ] ]
	

kernelLdFlags = [ Str s | s <- [ "-Wl,-N ",
                                 "-pie ",
                                 "-Wl,-melf_k1om ",
                                 "-fno-builtin ",
                                 "-e start",
                                 "-nostdlib ",
                                 "-Wl,--fatal-warnings ",
                                 "-m64 " ] ]


------------------------------------------------------------------------
--
-- Now, commands to actually do something
--
------------------------------------------------------------------------

--
-- Compilers
--
cCompiler = ArchDefaults.cCompiler arch compiler
cxxCompiler = ArchDefaults.cxxCompiler arch cxxcompiler
makeDepend = ArchDefaults.makeDepend arch compiler
makeCxxDepend  = ArchDefaults.makeCxxDepend arch cxxcompiler
cToAssembler = ArchDefaults.cToAssembler arch compiler
assembler = ArchDefaults.assembler arch compiler
archive = ArchDefaults.archive arch
linker = ArchDefaults.linker arch compiler
cxxlinker = ArchDefaults.cxxlinker arch cxxcompiler

--
-- Link the kernel (CPU Driver)
-- 
linkKernel :: Options -> [String] -> [String]  -> String -> HRule
linkKernel opts objs libs kbin = 
    let linkscript = "/kernel/linker.lds"
    in
      Rules [ Rule ([ Str compiler, Str Config.cOptFlags,
                      NStr "-T", In BuildTree arch "/kernel/linker.lds",
                      Str "-o", Out arch kbin 
                    ]
                    ++ (optLdFlags opts)
                    ++
                    [ In BuildTree arch o | o <- objs ]
                    ++
                    [ In BuildTree arch l | l <- libs ]
                    ++ 
                    [ NL, NStr "/bin/echo -e '\\0002' | dd of=",
                      Out arch kbin, 
                      Str "bs=1 seek=16 count=1 conv=notrunc status=noxfer"
                    ]
                   ),
              Rule [ Str "cpp", 
                     NStr "-I", NoDep SrcTree "src" "/kernel/include/",
                     Str "-D__ASSEMBLER__", 
                     Str "-P", In SrcTree "src" "/kernel/arch/k1om/linker.lds.in",
                     Out arch linkscript 
                   ]
            ]
