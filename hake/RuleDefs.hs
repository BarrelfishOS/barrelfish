-------------------------------------------------------------------------
-- Copyright (c) 2007-2011, 2012, 2015 ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:-
-- ETH Zurich D-INFK CAB F.78, Universitaetstr 6, CH-8092 Zurich.
-- Attn: Systems Group.
--
-- Basic Hake rule definitions and combinators
--
--------------------------------------------------------------------------

module RuleDefs where
import Data.List (intersect, isSuffixOf, union, (\\), nub, sortBy, elemIndex)
import Data.Maybe (fromMaybe, fromJust)
import System.FilePath
import qualified X86_64
import qualified K1om
import qualified X86_32
import qualified ARMv7
import qualified ARMv8
import HakeTypes
import qualified Args
import qualified Config
import TreeDB
import Data.Char

import Debug.Trace
-- enable debug spew
-- should we move this to Config.hs? -AB
debugFlag = False

--
-- Is a token to be displayed in a rule?
--
inRule :: RuleToken -> Bool
inRule (Dep _ _ _) = False
inRule (PreDep _ _ _) = False
inRule (LDep _ _) = False
inRule (Target _ _) = False
inRule _ = True

--
-- Look for a set of files: this is called using the "find" combinator
--
withSuffix :: TreeDB -> String -> String -> [String]
withSuffix srcDB hakepath extension =
    map (\f -> "/" </> f) $
        fromMaybe [] $ tdbByDirExt (takeDirectory hakepath) extension srcDB

withSuffices :: TreeDB -> String -> [String] -> [String]
withSuffices srcDB hakepath extensions =
    map (\f -> "/" </> f) $
        fromMaybe [] $ tdbByDirExts (takeDirectory hakepath) extensions srcDB

--
-- Find files with a given suffix in a given dir
--
inDir :: TreeDB -> String -> String -> String -> [String]
inDir srcDB hakepath dir extension =
    map (\f -> "/" </> f) $
        fromMaybe [] $
            tdbByDirExt (dropTrailingPathSeparator $ normalise $
                            takeDirectory hakepath </> dir)
                        extension srcDB

cInDir :: TreeDB -> String -> String -> [String]
cInDir tdb tf dir = inDir tdb tf dir ".c"

cxxInDir :: TreeDB -> String -> String -> [String]
cxxInDir tdb tf dir = (inDir tdb tf dir ".cpp") ++ (inDir tdb tf dir ".cc")

sInDir :: TreeDB -> String -> String -> [String]
sInDir tdb tf dir = inDir tdb tf dir ".S"

-------------------------------------------------------------------------
--
-- Architecture specific definitions
--
-------------------------------------------------------------------------

options :: String -> Options
options "x86_64" = X86_64.options
options "k1om" = K1om.options
options "x86_32" = X86_32.options
options "armv7" = ARMv7.options
options "armv8" = ARMv8.options
options s = error $ "Unknown architecture " ++ s

kernelCFlags "x86_64" = X86_64.kernelCFlags
kernelCFlags "k1om" = K1om.kernelCFlags
kernelCFlags "x86_32" = X86_32.kernelCFlags
kernelCFlags "armv7" = ARMv7.kernelCFlags
kernelCFlags "armv8" = ARMv8.kernelCFlags
kernelCFlags s = error $ "Unknown architecture " ++ s

kernelLdFlags "x86_64" = X86_64.kernelLdFlags
kernelLdFlags "k1om" = K1om.kernelLdFlags
kernelLdFlags "x86_32" = X86_32.kernelLdFlags
kernelLdFlags "armv7" = ARMv7.kernelLdFlags
kernelLdFlags "armv8" = ARMv8.kernelLdFlags
kernelLdFlags s = error $ "Unknown architecture " ++ s

archFamily :: String -> String
archFamily arch = optArchFamily (options arch)

-------------------------------------------------------------------------
--
-- Options for compiling the kernel, which is special
--
-------------------------------------------------------------------------

kernelOptIncludes :: String -> [ RuleToken ]
kernelOptIncludes arch
    | arch == "k1om"  = K1om.kernelOptIncludes
    | otherwise = [ ]

kernelIncludes arch = [ NoDep BuildTree arch f | f <- [
                    "/include" ]]
                 ++
                 [ NoDep SrcTree "src" f | f <- [
                    "/kernel/include/arch" </> arch,
                    "/kernel/include/arch" </> archFamily arch,
                    "/kernel/include",
                    "/include",
                    "/include/arch" </> archFamily arch,
                    "/include/target" </> archFamily arch]]
                 ++ kernelOptIncludes arch

kernelOptions arch = Options {
            optArch = arch,
            optArchFamily = archFamily arch,
            optFlags = kernelCFlags arch,
            optCxxFlags = [],
            optLibDep = [],
            optCxxLibDep = [],
            optDefines = (optDefines (options arch)) ++ [ Str "-DIN_KERNEL",
                Str ("-DCONFIG_SCHEDULER_" ++ (show Config.scheduler)),
                Str ("-DCONFIG_TIMESLICE=" ++ (show Config.timeslice)) ],
            optIncludes = kernelIncludes arch,
            optDependencies =
                [ Dep InstallTree arch "/include/errors/errno.h",
                  Dep InstallTree arch "/include/barrelfish_kpi/capbits.h",
                  Dep InstallTree arch "/include/asmoffsets.h",
                  Dep InstallTree arch "/include/trace_definitions/trace_defs.h" ],
            optLdFlags = kernelLdFlags arch,
            optLdCxxFlags = [],
            optLibs = [],
            optCxxLibs = [],
            optSuffix = [],
            optInterconnectDrivers = [],
            optFlounderBackends = [],
            extraFlags = [],
            extraCxxFlags = [],
            extraDefines = [],
            extraIncludes = [],
            extraDependencies = [],
            extraLdFlags = [],
            optInstallPath = OptionsPath {
                optPathBin = "/sbin",
                optPathLib = "/lib"
            }
          }


-------------------------------------------------------------------------
--
-- IMPORTANT: This section contains extraction of functions from the
-- relevant architecture module.  The names and types should be
-- exactly the same as in the architecture.hs file.  This section
-- should not contain any logic; ony architecture extraction.
--
--------------------------------------------------------------------------

--
-- First, the default C compiler for an architecture
--

compiler :: Options -> String
compiler opts
    | optArch opts == "x86_64"  = X86_64.compiler
    | optArch opts == "k1om"    = K1om.compiler
    | optArch opts == "x86_32"  = X86_32.compiler
    | optArch opts == "armv7" = ARMv7.compiler
    | optArch opts == "armv8" = ARMv8.compiler

cCompiler :: Options -> String -> String -> String -> [ RuleToken ]
cCompiler opts phase src obj
    | optArch opts == "x86_64"  = X86_64.cCompiler opts phase src obj
    | optArch opts == "k1om"    = K1om.cCompiler opts phase src obj
    | optArch opts == "x86_32"  = X86_32.cCompiler opts phase src obj
    | optArch opts == "armv7" = ARMv7.cCompiler opts phase src obj
    | optArch opts == "armv8" = ARMv8.cCompiler opts phase src obj
    | otherwise = [ ErrorMsg ("no C compiler for " ++ (optArch opts)) ]

cPreprocessor :: Options -> String -> String -> String -> [ RuleToken ]
cPreprocessor opts phase src obj
    | otherwise = [ ErrorMsg ("no C preprocessor for " ++ (optArch opts)) ]

--
-- C++ compiler, where supported
--
cxxCompiler :: Options -> String -> String -> String -> [ RuleToken ]
cxxCompiler opts phase src obj
    | optArch opts == "x86_64"  = X86_64.cxxCompiler opts phase src obj
    | optArch opts == "k1om"  = K1om.cxxCompiler opts phase src obj
    | otherwise = [ ErrorMsg ("no C++ compiler for " ++ (optArch opts)) ]


--
-- makeDepend step; note that obj can be whatever the intended output is
--
makeDepend :: Options -> String -> String -> String -> String -> [ RuleToken ]
makeDepend opts phase src obj depfile
    | optArch opts == "x86_64" =
        X86_64.makeDepend opts phase src obj depfile
    | optArch opts == "k1om" =
        K1om.makeDepend opts phase src obj depfile
    | optArch opts == "x86_32" =
        X86_32.makeDepend opts phase src obj depfile
    | optArch opts == "armv7" =
        ARMv7.makeDepend opts phase src obj depfile
    | optArch opts == "armv8" =
        ARMv8.makeDepend opts phase src obj depfile
    | otherwise = [ ErrorMsg ("no dependency generator for " ++ (optArch opts)) ]

makeCxxDepend :: Options -> String -> String -> String -> String -> [ RuleToken ]
makeCxxDepend opts phase src obj depfile
    | optArch opts == "x86_64" =
        X86_64.makeCxxDepend opts phase src obj depfile
    | optArch opts == "k1om" =
        K1om.makeCxxDepend opts phase src obj depfile
    | optArch opts == "x86_32" =
        X86_32.makeCxxDepend opts phase src obj depfile
    | otherwise = [ ErrorMsg ("no C++ dependency generator for " ++ (optArch opts)) ]

cToAssembler :: Options -> String -> String -> String -> String -> [ RuleToken ]
cToAssembler opts phase src afile objdepfile
    | optArch opts == "x86_64"  = X86_64.cToAssembler opts phase src afile objdepfile
    | optArch opts == "k1om"  = K1om.cToAssembler opts phase src afile objdepfile
    | optArch opts == "x86_32"  = X86_32.cToAssembler opts phase src afile objdepfile
    | optArch opts == "armv7" = ARMv7.cToAssembler opts phase src afile objdepfile
    | optArch opts == "armv8" = ARMv8.cToAssembler opts phase src afile objdepfile
    | otherwise = [ ErrorMsg ("no C compiler for " ++ (optArch opts)) ]

--
-- Assemble an assembly language file
--
assembler :: Options -> String -> String -> [ RuleToken ]
assembler opts src obj
    | optArch opts == "x86_64"  = X86_64.assembler opts src obj
    | optArch opts == "k1om"  = K1om.assembler opts src obj
    | optArch opts == "x86_32"  = X86_32.assembler opts src obj
    | optArch opts == "armv7" = ARMv7.assembler opts src obj
    | optArch opts == "armv8" = ARMv8.assembler opts src obj
    | otherwise = [ ErrorMsg ("no assembler for " ++ (optArch opts)) ]

archive :: Options -> [String] -> [String] -> String -> String -> [ RuleToken ]
archive opts objs libs name libname
    | optArch opts == "x86_64"  = X86_64.archive opts objs libs name libname
    | optArch opts == "k1om"  = K1om.archive opts objs libs name libname
    | optArch opts == "x86_32"  = X86_32.archive opts objs libs name libname
    | optArch opts == "armv7" = ARMv7.archive opts objs libs name libname
    | optArch opts == "armv8" = ARMv8.archive opts objs libs name libname
    | otherwise = [ ErrorMsg ("Can't build a library for " ++ (optArch opts)) ]

linker :: Options -> [String] -> [String] -> [String] -> String -> [RuleToken]
linker opts objs libs mods bin
    | optArch opts == "x86_64" = X86_64.linker opts objs libs mods bin
    | optArch opts == "k1om" = K1om.linker opts objs libs mods bin
    | optArch opts == "x86_32" = X86_32.linker opts objs libs mods bin
    | optArch opts == "armv7" = ARMv7.linker opts objs libs mods bin
    | optArch opts == "armv8" = ARMv8.linker opts objs libs mods bin
    | otherwise = [ ErrorMsg ("Can't link executables for " ++ (optArch opts)) ]

ldtLinker :: Options -> [String] -> String -> String -> [RuleToken]
ldtLinker opts objs app bin
    | optArch opts == "x86_64" = X86_64.ldtLinker opts objs app bin
    | optArch opts == "k1om" = K1om.ldtLinker opts objs app bin
    | optArch opts == "armv7" = ARMv7.ldtLinker opts objs app bin
    | optArch opts == "armv8" = ARMv8.ldtLinker opts objs app bin
    | otherwise = [ ErrorMsg ("Can't link executables for " ++ (optArch opts)) ]

strip :: Options -> String -> String -> String -> [RuleToken]
strip opts src debuglink target
    | optArch opts == "x86_64" = X86_64.strip opts src debuglink target
    | optArch opts == "k1om" = K1om.strip opts src debuglink target
    | optArch opts == "x86_32" = X86_32.strip opts src debuglink target
    | optArch opts == "armv7" = ARMv7.strip opts src debuglink target
    | optArch opts == "armv8" = ARMv8.strip opts src debuglink target
    | otherwise = [ ErrorMsg ("Can't strip executables for " ++ (optArch opts)) ]

debug :: Options -> String -> String -> [RuleToken]
debug opts src target
    | optArch opts == "x86_64" = X86_64.debug opts src target
    | optArch opts == "k1om" = K1om.debug opts src target
    | optArch opts == "x86_32" = X86_32.debug opts src target
    | optArch opts == "armv7" = ARMv7.debug opts src target
    | optArch opts == "armv8" = ARMv8.debug opts src target
    | otherwise = [ ErrorMsg ("Can't extract debug symbols for " ++ (optArch opts)) ]

cxxlinker :: Options -> [String] -> [String] -> [String] -> String -> [RuleToken]
cxxlinker opts objs libs mods bin
    | optArch opts == "x86_64" = X86_64.cxxlinker opts objs libs mods bin
    | optArch opts == "k1om" = K1om.cxxlinker opts objs libs mods bin
    | otherwise = [ ErrorMsg ("Can't link C++ executables for " ++ (optArch opts)) ]

ldtCxxlinker :: Options -> [String] -> String -> String -> [RuleToken]
ldtCxxlinker opts objs app bin
    | optArch opts == "x86_64" = X86_64.ldtCxxlinker opts objs app bin
    | optArch opts == "k1om" = K1om.ldtCxxlinker opts objs app bin
    | otherwise = [ ErrorMsg ("Can't link C++ executables for " ++ (optArch opts)) ]

--
-- The C compiler for compiling things on the host
--
nativeCCompiler :: String
nativeCCompiler = "$(CC)"

nativeArchiver :: String
nativeArchiver = "ar"

-------------------------------------------------------------------------
--
-- Functions to create useful filenames
--

dependFilePath :: String -> String
dependFilePath obj = obj ++ ".depend"

objectFilePath :: Options -> String -> String
objectFilePath opts src = optSuffix opts </> replaceExtension src ".o"

generatedObjectFilePath :: Options -> String -> String
generatedObjectFilePath opts src = replaceExtension src ".o"

preprocessedFilePath :: Options -> String -> String
preprocessedFilePath opts src = optSuffix opts </> replaceExtension src ".i"

-- Standard convention is that human generated assembler is .S, machine generated is .s
assemblerFilePath :: Options -> String -> String
assemblerFilePath opts src = optSuffix opts </> replaceExtension src ".s"


-------------------------------------------------------------------------
--
-- Functions with logic to start doing things
--

--
-- Create C file dependencies
--

-- Since this is where we know what the depfile is called it is here that we also
-- decide to include it.  This stops many different places below trying to
-- guess what the depfile is called
--
makeDependArchSub :: Options -> String -> String -> String -> String -> [ RuleToken ]
makeDependArchSub opts phase src objfile depfile =
   [ Str ("@if [ -z $Q ]; then echo Generating $@; fi"), NL ] ++
     makeDepend opts phase src objfile depfile

makeDependArch :: Options -> String -> String -> String -> String -> HRule
makeDependArch opts phase src objfile depfile =
    Rules [ Rule (makeDependArchSub opts phase src objfile depfile),
            Include (Out (optArch opts) depfile)
          ]

-- Make depend for a standard object file
makeDependObj :: Options -> String -> String -> HRule
makeDependObj opts phase src =
    let objfile = (objectFilePath opts src)
    in
      makeDependArch opts phase src objfile (dependFilePath objfile)

-- Make depend for a C++ object file
makeDependCxxArchSub :: Options -> String -> String -> String -> String -> [ RuleToken ]
makeDependCxxArchSub opts phase src objfile depfile =
   [ Str ("@if [ -z $Q ]; then echo Generating $@; fi"), NL ] ++
     makeCxxDepend opts phase src objfile depfile

makeDependCxxArch :: Options -> String -> String -> String -> String -> HRule
makeDependCxxArch opts phase src objfile depfile =
    Rules [ Rule (makeDependCxxArchSub opts phase src objfile depfile),
            Include (Out (optArch opts) depfile)
          ]

makeDependCxxObj :: Options -> String -> String -> HRule
makeDependCxxObj opts phase src =
    let objfile = (objectFilePath opts src)
    in
      makeDependCxxArch opts phase src objfile (dependFilePath objfile)

-- Make depend for an assembler output
makeDependAssembler :: Options -> String -> String -> HRule
makeDependAssembler opts phase src =
    let objfile = (assemblerFilePath opts src)
    in
      makeDependArch opts phase src objfile (dependFilePath objfile)

--
-- Compile a C program to assembler
--
makecToAssembler :: Options -> String -> String -> String -> [ RuleToken ]
makecToAssembler opts phase src obj =
    cToAssembler opts phase src (assemblerFilePath opts src) (dependFilePath obj)

--
-- Assemble an assembly language file
--
assemble :: Options -> String -> [ RuleToken ]
assemble opts src =
    assembler opts src (objectFilePath opts src)

--
-- Create a library from a set of object files
--
archiveLibrary :: Options -> String -> [String] -> [String] -> [ RuleToken ]
archiveLibrary opts name objs libs =
    archive opts objs libs name (libraryPath opts name)

--
-- Link an executable, explicit libs/mods
--
linkExecutable :: Options -> [String] -> [String] -> [String] -> String -> [RuleToken]
linkExecutable opts objs libs mods bin =
    linker opts objs libs mods (applicationPath opts bin)

--
-- Link an executable, use ldt to calculate dependencies
--
ldtLinkExecutable :: Options -> [String] -> String -> String -> [RuleToken]
ldtLinkExecutable opts objs app bin =
    ldtLinker opts objs app (applicationPath opts bin)

--
-- Strip debug symbols from an executable
--
stripExecutable :: Options -> String -> String -> String -> [RuleToken]
stripExecutable opts src debuglink target =
    strip opts (applicationPath opts src) (applicationPath opts debuglink)
               (applicationPath opts target)

--
-- Extract debug symbols from an executable
--
debugExecutable :: Options -> String -> String -> [RuleToken]
debugExecutable opts src target =
    debug opts (applicationPath opts src) (applicationPath opts target)

--
-- Link a C++ executable
--
linkCxxExecutable :: Options -> [String] -> [String] -> [String] -> String -> [RuleToken]
linkCxxExecutable opts objs libs mods bin =
    cxxlinker opts objs libs mods (applicationPath opts bin)

--
-- Link a C++ executable using ldt.
-- XXX: This has not been tested
--
ldtLinkCxxExecutable :: Options -> [String] -> String -> String -> [RuleToken]
ldtLinkCxxExecutable opts objs app bin =
    ldtCxxlinker opts objs app (applicationPath opts bin)

-------------------------------------------------------------------------


--
-- Emit dependency tokens for libs in Options
stdLibDepsRules :: Options -> String -> [HRule]
stdLibDepsRules opts app =
  [Rule [LDep (DepApp arch app) (DepLib arch l) | l <- optLibDep opts]]
  where
    arch = optArch opts

stdCxxLibDepsRules :: Options -> String -> [HRule]
stdCxxLibDepsRules opts app =
  [Rule [LDep (DepApp arch app) (DepLib arch l) | l <- optCxxLibDep opts]]
  where
    arch = optArch opts




-------------------------------------------------------------------------
--
-- Hake macros (hacros?): each of these evaluates to HRule, i.e. a
-- list of templates for Makefile rules
--
-------------------------------------------------------------------------

--
-- Compile a C file for a particular architecture
-- We include cToAssembler to permit humans to type "make foo/bar.s"
--
compileCFile :: Options -> String -> HRule
compileCFile opts src =
    Rules [ Rule (cCompiler opts "src" src (objectFilePath opts src)),
            Rule (makecToAssembler opts "src" src (objectFilePath opts src)),
            makeDependObj opts "src" src
          ]

--
-- Compile a C++ file for a particular architecture
--
compileCxxFile :: Options -> String -> HRule
compileCxxFile opts src =
    Rules [ Rule (cxxCompiler opts "src" src (objectFilePath opts src)),
            makeDependCxxObj opts "src" src
          ]

--
-- Compile a C file for a particular architecture
--
compileGeneratedCFile :: Options -> String -> HRule
compileGeneratedCFile opts src =
    let o2 = opts { optSuffix = "" }
        arch = optArch o2
    in
      Rules [ Rule (cCompiler o2 arch src (objectFilePath o2 src) ),
              Rule (makecToAssembler o2 arch src (objectFilePath o2 src)),
              makeDependObj o2 arch src
            ]

compileGeneratedCxxFile :: Options -> String -> HRule
compileGeneratedCxxFile opts src =
    let o2 = opts { optSuffix = "" }
        arch = optArch o2
    in
      Rules [ Rule (cxxCompiler o2 arch src (objectFilePath o2 src) ),
              makeDependCxxObj o2 arch src
            ]

compileCFiles :: Options -> [String] -> HRule
compileCFiles opts srcs = Rules [ compileCFile opts s | s <- srcs ]
compileCxxFiles :: Options -> [String] -> HRule
compileCxxFiles opts srcs = Rules [ compileCxxFile opts s | s <- srcs ]
compileGeneratedCFiles :: Options -> [String] -> HRule
compileGeneratedCFiles opts srcs =
    Rules [ compileGeneratedCFile opts s | s <- srcs ]
compileGeneratedCxxFiles :: Options -> [String] -> HRule
compileGeneratedCxxFiles opts srcs =
    Rules [ compileGeneratedCxxFile opts s | s <- srcs ]

--
-- Add a set of C (or whatever) dependences on a *generated* file.
-- Somewhere else this file has to be defined as a target, of
-- course...
--
extraCDependencyForObj :: Options -> String -> String -> String -> [RuleToken]
extraCDependencyForObj opts file s obj =
    let arch = optArch opts
    in
      [ Target arch (dependFilePath obj),
        Target arch obj,
        Dep BuildTree arch file
      ]

extraCDependency :: Options -> String -> String -> HRule
extraCDependency opts file s = Rule (extraCDependencyForObj opts file s obj)
    where obj = objectFilePath opts s


extraCDependencies :: Options -> String -> [String] -> HRule
extraCDependencies opts file srcs =
    Rules [ extraCDependency opts file s | s <- srcs ]

extraGeneratedCDependency :: Options -> String -> String -> HRule
extraGeneratedCDependency opts file s =
    extraCDependency (opts { optSuffix = "" }) file s

--
-- Copy include files to the appropriate directory
--
includeFile :: [ String ] -> HRule
includeFile hdrs =
    Rules ([ Rule [ Str "cp", In SrcTree "src" hdr, Out "root" hdr] | hdr <- hdrs ]
    ++ [
        Phony "install_headers" False [ Dep BuildTree "root" hdr | hdr <- hdrs ]
    ])

--
-- Build a Mackerel header file from a definition.
--
mackerelProgLoc = In InstallTree "tools" "/bin/mackerel"
mackerelDevFileLoc d = In SrcTree "src" ("/devices" </> (d ++ ".dev"))
mackerelDevHdrPath d = "/include/dev/" </> (d ++ "_dev.h")

mackerel2 :: Options -> String -> HRule
mackerel2 opts dev = mackerel_generic opts dev "shift-driver"

mackerel :: Options -> String -> HRule
mackerel opts dev = mackerel_generic opts dev "bitfield-driver"

mackerel_generic :: Options -> String -> String -> HRule
mackerel_generic opts dev flag =
    let
        arch = optArch opts
    in
      Rule [ mackerelProgLoc,
             Str ("--" ++ flag),
             Str "-c", mackerelDevFileLoc dev,
             Str "-o", Out arch (mackerelDevHdrPath dev)
           ]

mackerelDependencies :: Options -> String -> [String] -> HRule
mackerelDependencies opts d srcs =
    extraCDependencies opts (mackerelDevHdrPath d) srcs

--
-- Basic Flounder definitions: where things are
--

flounderProgLoc = In InstallTree "tools" "/bin/flounder"
flounderIfFileLoc ifn = In SrcTree "src" ("/if" </> (ifn ++ ".if"))
flounderIfFileDep ifn = Dep SrcTree "src" ("/if" </> (ifn ++ ".if"))

-- new-style stubs: path for generic header
flounderIfDefsPath ifn = "/include/if" </> (ifn ++ "_defs.h")
-- new-style stubs: path for specific backend header
flounderIfDrvDefsPath ifn drv = "/include/if" </> (ifn ++ "_" ++ drv ++ "_defs.h")

-- new-style stubs: generated C code (for all default enabled backends)
flounderBindingPath opts ifn =
    (optSuffix opts) </> (ifn ++ "_flounder_bindings.c")
-- new-style stubs: generated C code (for extra backends enabled by the user)
flounderExtraBindingPath opts ifn =
    (optSuffix opts) </> (ifn ++ "_flounder_extra_bindings.c")

flounderTHCHdrPath ifn = "/include/if" </> (ifn ++ "_thc.h")
flounderTHCStubPath opts ifn =
    (optSuffix opts) </> (ifn ++ "_thc.c")

applicationPath :: Options -> String -> String
applicationPath opts name = optPathBin (optInstallPath opts) </> name

libraryPath :: Options -> String -> String
libraryPath opts libname = optPathLib (optInstallPath opts) </> ("lib" ++ libname ++ ".a")

kernelPath = "/sbin/cpu"

-- construct include arguments to flounder for common types
-- these are:
--  1. platform-specific types (if/platform/foo.if)
--  2. architecture-specific types (if/arch/foo.if)
--  3. generic types (if/types.if)
flounderIncludes :: Options -> [RuleToken]
flounderIncludes opts
    = concat [ [Str "-i", flounderIfFileLoc ifn]
               | ifn <- [ "platform" </> (optArch opts), -- XXX: optPlatform
                          "arch" </> (optArch opts),
                          "types" ] ]

flounderRule :: Options -> [RuleToken] -> HRule
flounderRule opts args
    = Rule $ [ flounderProgLoc ] ++ (flounderIncludes opts) ++ args


flounderTypesProgLoc = In InstallTree "tools" "/bin/floundertypes"
--
-- Build interface types header file
--
flounderGenIfTypes :: Options -> [String] -> HRule
flounderGenIfTypes opts ifs =
    let
        arch = optArch opts
        ifn_tokens = map flounderIfFileDep ifs
    in
      Rule $ [ flounderTypesProgLoc,
             Str Config.source_dir,
             Str Config.install_dir,
             Str arch,
             Out arch ("/include/if/if_types.h")
           ] ++ ifn_tokens
--
-- Build new-style Flounder header files from a definition
-- (generic header, plus one per backend)
--
flounderGenDefs :: Options -> String -> HRule
flounderGenDefs opts ifn =
    Rules $ flounderRule opts [
           Str "--generic-header", flounderIfFileLoc ifn,
           Out (optArch opts) (flounderIfDefsPath ifn),
           Dep BuildTree (optArch opts)  ("/include/if/if_types.h")
         ] : [ flounderRule opts [
           Str $ "--" ++ drv ++ "-header", flounderIfFileLoc ifn,
           Dep BuildTree (optArch opts)  ("/include/if/if_types.h"),
           Out (optArch opts) (flounderIfDrvDefsPath ifn drv)]
           | drv <- Args.allFlounderBackends ]

--
-- Build a new Flounder binding file from a definition.
-- This builds the binding for all enabled backends
--
flounderBinding :: Options -> String -> [String] -> HRule
flounderBinding opts ifn =
    flounderBindingHelper opts ifn backends (flounderBindingPath opts ifn)
    where
        backends = "generic" : (optFlounderBackends opts)

-- as above, but for a specific set of user-specified backends
flounderExtraBinding :: Options -> String -> [String] -> [String] -> HRule
flounderExtraBinding opts ifn backends =
    flounderBindingHelper opts ifn backends (flounderExtraBindingPath opts ifn)

flounderBindingHelper :: Options -> String -> [String] -> String -> [String] -> HRule
flounderBindingHelper opts ifn backends cfile srcs = Rules $
    [ flounderRule opts $ args ++ [flounderIfFileLoc ifn, Out arch cfile ],
        compileGeneratedCFile opts cfile,
        flounderDefsDepend opts ifn allbackends srcs]
    ++ [extraGeneratedCDependency opts (flounderIfDefsPath ifn) cfile]
    ++ [extraGeneratedCDependency opts (flounderIfDrvDefsPath ifn d) cfile
        | d <- allbackends]
    where
        arch = optArch opts
        archfam = optArchFamily opts
        args = [Str "-a", Str archfam] ++ [Str $ "--" ++ d ++ "-stub" | d <- backends]
        allbackends = backends `union` optFlounderBackends opts \\ ["generic"]

--
-- Build a Flounder THC header file from a definition.
--
flounderTHCFile :: Options -> String -> HRule
flounderTHCFile opts ifn =
    flounderRule opts [
           Str "--thc-header", flounderIfFileLoc ifn,
           Out (optArch opts) (flounderTHCHdrPath ifn)
         ]

--
-- Build a Flounder THC stubs file from a definition.
--
flounderTHCStub :: Options -> String -> [String] -> HRule
flounderTHCStub opts ifn srcs =
    let cfile = flounderTHCStubPath opts ifn
        hfile = flounderTHCHdrPath ifn
        arch = optArch opts
    in
      Rules [ flounderRule opts [
                     Str "--thc-stubs", flounderIfFileLoc ifn,
                     Out arch cfile
                   ],
              compileGeneratedCFile opts cfile,
              extraCDependencies opts hfile srcs,
              extraGeneratedCDependency opts hfile cfile,
              extraGeneratedCDependency opts (flounderIfDefsPath ifn) cfile
            ]

--
-- Create a dependency on a Flounder header file for a set of files,
-- but don't actually build either stub (useful for libraries)
--
flounderDefsDepend :: Options -> String -> [String] -> [String] -> HRule
flounderDefsDepend opts ifn backends srcs = Rules $
    (extraCDependencies opts (flounderIfDefsPath ifn) srcs) :
    [extraCDependencies opts (flounderIfDrvDefsPath ifn drv) srcs
           | drv <- backends, drv /= "generic" ]

--
-- Emit all the Flounder-related rules/dependencies for a given target
--

flounderRules :: Options -> Args.Args -> [String] -> [HRule]
flounderRules opts args csrcs =
    ([ flounderBinding opts f csrcs | f <- Args.flounderBindings args ]
     ++
     [ flounderExtraBinding opts f backends csrcs
       | (f, backends) <- Args.flounderExtraBindings args ]
     ++
     [ flounderTHCStub opts f csrcs | f <- Args.flounderTHCStubs args ]
     ++
     -- Flounder extra defs (header files) also depend on the base
     -- Flounder headers for the same interface
     [ flounderDefsDepend opts f baseBackends csrcs | f <- allIf ]
     ++
     -- Extra defs only for non-base backends (those were already emitted above)
     [ flounderDefsDepend opts f (backends \\ baseBackends) csrcs
       | (f, backends) <- Args.flounderExtraDefs args ]
    )
    where
      -- base backends enabled by default
      baseBackends = optFlounderBackends opts

      -- all interfaces mentioned in flounderDefs or ExtraDefs
      allIf = nub $ Args.flounderDefs args ++ [f | (f,_) <- Args.flounderExtraDefs args]


 --
 -- Build a Skate library and header file
 --


skateSchemaPath opts ifn = (optSuffix opts) </> (ifn ++ "_skate_schema.c")
skateProgLoc = In InstallTree "tools" "/bin/skate"
skateSksFileLoc schema = In SrcTree "src" ("/schemas" </> (schema ++ ".sks"))
skateSchemaDefsPath schema = "/include/schemas" </> (schema ++ "_schema.h")


skateSchemaHelper :: Options -> String -> String -> [String] -> HRule
skateSchemaHelper opts ifn cfile srcs = Rules $
    [ skateRule opts $ args ++ [
        Str "-o", Out arch cfile, skateSksFileLoc ifn],
        compileGeneratedCFile opts cfile,
        skateDefsDepend opts ifn srcs]
    ++ [extraGeneratedCDependency opts (skateSchemaDefsPath ifn) cfile]
    where
        arch = optArch opts
        archfam = optArchFamily opts
        args = [Str "-a", Str arch, Str "-C"]


skateSchema :: Options -> String -> [String] -> HRule
skateSchema opts schema =
    skateSchemaHelper opts schema (skateSchemaPath opts schema)


skateDefsDepend :: Options -> String -> [String] -> HRule
skateDefsDepend opts schema srcs = Rules $
    [(extraCDependencies opts (skateSchemaDefsPath schema) srcs)]


skateRules :: Options -> Args.Args -> [String] -> [HRule]
skateRules opts args csrcs =
    ([ skateSchema opts f csrcs | f <- Args.skateSchemas args ]
     ++
     [ skateDefsDepend opts f csrcs | f <- nub $ Args.skateSchemaDefs args ])


skateIncludes :: Options -> [RuleToken]
skateIncludes opts = []


skateRule :: Options -> [RuleToken] -> HRule
skateRule opts args = Rule $ [ skateProgLoc ] ++ (skateIncludes opts) ++ args


skateGenSchemas :: Options -> String -> HRule
skateGenSchemas opts schema =
 Rules $ [skateRule opts [
        Str "-H",
        Str "-o", Out (optArch opts) (skateSchemaDefsPath schema),
        skateSksFileLoc schema
      ]]


--
-- Build SKB facts from Sockeye file
--
sockeyeProgLoc = In InstallTree "tools" "/bin/sockeye"
sockeyeProgLoc2 = In InstallTree "tools" "/bin/sockeye2"
sockeyeSocDir = In SrcTree "src" "/socs"
sockeyeSocFileLoc d = In SrcTree "src" ("/socs" </> d <.> "soc")
sockeyeFactFilePath d = "/sockeyefacts" </> d <.> "pl"
sockeyeFactFileLoc d = In BuildTree "" $ sockeyeFactFilePath d

sockeyeNS :: String -> String -> HRule
sockeyeNS net rootns =
    let
        factFile = sockeyeFactFilePath net
        depFile = dependFilePath factFile
    in Rules
        [ Rule
            [ sockeyeProgLoc
            , Str "-i", sockeyeSocDir
            , Str "-o", Out "" factFile
            , Str "-d", Out "" depFile
            , Str "-r", Str rootns
            , sockeyeSocFileLoc net
            ]
        , Include (Out "" depFile)
        ]

sockeye :: String -> HRule
sockeye net =
    let
        factFile = sockeyeFactFilePath net
        depFile = dependFilePath factFile
    in Rules
        [ Rule
            [ sockeyeProgLoc
            , Str "-i", sockeyeSocDir
            , Str "-o", Out "" factFile
            , Str "-d", Out "" depFile
            , sockeyeSocFileLoc net
            ]
        , Include (Out "" depFile)
        ]

sockeye2 :: String -> HRule
sockeye2 net =
    let
        factFile = sockeyeFactFilePath net
        depFile = dependFilePath factFile
    in Rules
        [ Rule
            [ sockeyeProgLoc2
            , Str "-i", sockeyeSocDir
            , Str "-o", Out "" factFile
            , Str "-d", Out "" depFile
            , sockeyeSocFileLoc net
            ]
        , Include (Out "" depFile)
        ]

sockeye2Lisa :: String -> HRule
sockeye2Lisa net =
    let
        outFile = Out "" ("/platforms" </> net <.> ".lisa")
        inSgproj = "/socs" </> net <.> ".sgproj"
        outSgproj = "/platforms" </> net <.> ".sgproj"
    in Rules
        [ Rule
            [ sockeyeProgLoc2
            , Str "-o", outFile
            , Str "-L", sockeyeSocFileLoc net
            ],
          copyFile SrcTree "" inSgproj  "" outSgproj
        ]

--
-- Build a Fugu library
--
fuguCFile :: Options -> String -> HRule
fuguCFile opts file =
    let arch = optArch opts
        cfile = file ++ ".c"
        ofile = file ++ ".o"
    in
      Rules [ Rule [ In InstallTree "tools" "/bin/fugu",
                     In SrcTree "src" (file++".fugu"),
                     Str "-c",
                     Out arch cfile ],
              compileGeneratedCFile opts cfile,
              staticLibrary opts "errno" [ofile] []
         ]

fuguHFile :: Options -> String -> HRule
fuguHFile opts file =
    let arch = optArch opts
        hfile = "/include/errors/" ++ file ++ ".h"
    in
      Rule [ In InstallTree "tools" "/bin/fugu",
             In SrcTree "src" (file++".fugu"),
             Str "-h",
             Out arch hfile ]

--
-- Build a Pleco library
--
plecoFile :: Options -> String -> HRule
plecoFile opts file =
    let arch = optArch opts
        cfile = file ++ ".c"
        hfile = "/include/trace_definitions/" ++ file ++ ".h"
        jsonfile = "/trace_definitions/" ++ file ++ ".json"
    in
      Rules [ Rule [In InstallTree "tools" "/bin/pleco",
                    In SrcTree "src" (file++".pleco"),
                    Out arch hfile,
                    Out arch jsonfile,
                    Out arch cfile ],
              compileGeneratedCFile opts cfile
         ]

--
-- Build a Hamlet file
--
hamletFile :: Options -> String -> HRule
hamletFile opts file =
    let arch = optArch opts
        hfile = "/include/barrelfish_kpi/capbits.h"
        cfile = "cap_predicates.c"
        usercfile = "user_cap_predicates.c"
        ofile = "user_cap_predicates.o"
        nfile = "cap_predicates"
        afile = "/lib/libcap_predicates.a"
    in
      Rules [ Rule [In InstallTree "tools" "/bin/hamlet",
                    In SrcTree "src" (file++".hl"),
                    Out arch hfile,
                    Out arch cfile,
                    Out arch usercfile ],
              compileGeneratedCFile opts usercfile,
              Rule (archive opts [ ofile ] [] nfile afile)
         ]

--
-- Link a set of object files and libraries together
--
link :: Options -> [String] -> [String] -> [String] -> String -> HRule
link opts objs libs mods bin =
    let full = bin ++ ".full"
        debug = bin ++ ".debug"
    in Rules [
        Rule $ linkExecutable opts objs libs mods full,
        Rule $ debugExecutable opts full debug,
        Rule $ stripExecutable opts full debug bin
    ]

--
-- Link a set of object files and libraries together
--
ldtLink :: Options -> [String] -> String -> String -> HRule
ldtLink opts objs app bin =
    let full = bin ++ ".full"
        debug = bin ++ ".debug"
    in Rules [
        Rule $ ldtLinkExecutable opts objs app full,
        Rule $ debugExecutable opts full debug,
        Rule $ stripExecutable opts full debug bin
    ]

--
-- Link a set of C++ object files and libraries together
--
linkCxx :: Options -> [String] -> [String] -> [String] -> String -> HRule
linkCxx opts objs libs mods bin =
    Rule (linkCxxExecutable opts objs libs mods bin)

--
-- Link a set of object files and libraries together
--
ldtLinkCxx :: Options -> [String] -> String -> String -> HRule
ldtLinkCxx opts objs app bin =
    Rule (ldtLinkCxxExecutable opts objs app bin)

--
-- Link a CPU driver.  This is where it gets distinctly architecture-specific.
--
linkKernel :: Options -> String -> [String] -> [String] -> String -> HRule
linkKernel opts name objs libs driverType
    | optArch opts == "x86_64" = X86_64.linkKernel opts objs [libraryPath opts l | l <- libs ] ("/sbin" </> name)
    | optArch opts == "k1om" = K1om.linkKernel opts objs [libraryPath opts l | l <- libs ] ("/sbin" </> name)
    | optArch opts == "x86_32" = X86_32.linkKernel opts objs [libraryPath opts l | l <- libs ] ("/sbin" </> name)
    | optArch opts == "armv7" = ARMv7.linkKernel opts objs [libraryPath opts l | l <- libs ] name driverType
    | optArch opts == "armv8" = ARMv8.linkKernel opts objs [libraryPath opts l | l <- libs ] name driverType
    | otherwise = Rule [ Str ("Error: Can't link kernel for '" ++ (optArch opts) ++ "'") ]

--
-- Copy a file from one place to another
--
copy :: Options -> String -> String -> HRule
copy opts src dest =
    Rule [ Str "cp", In BuildTree (optArch opts) src, Out (optArch opts) dest ]

--
-- Assemble a list of S files for a particular architecture
--
assembleSFile :: Options -> String -> HRule
assembleSFile opts src =
    Rules [ Rule (assemble opts src),
            makeDependObj opts "src" src
          ]

assembleSFiles :: Options -> [String] -> HRule
assembleSFiles opts srcs = Rules [ assembleSFile opts s | s <- srcs ]

--
-- Archive a bunch of objects into a library
--
staticLibrary :: Options -> String -> [String] -> [String] -> HRule
staticLibrary opts libpath objs libs =
    Rule (archiveLibrary opts libpath objs libs)

--
-- Compile a Haskell binary (for the host architecture)
--
compileHaskell prog main deps = compileHaskellWithLibs prog main deps []
compileHaskellWithLibs prog main deps dirs =
  let
    tools_dir = (Dep InstallTree "tools" "/tools/.marker")
  in
    Rule ([ NStr "ghc -i",
            NoDep SrcTree "src" ".",
            Str "-odir ", NoDep BuildTree "tools" ".",
            Str "-hidir ", NoDep BuildTree "tools" ".",
            Str "-rtsopts=all",
            Str "--make ",
            In SrcTree "src" main,
            Str "-o ",
            Out "tools" ("/bin" </> prog),
            Str "$(LDFLAGS)" ]
          ++ concat [[ NStr "-i", NoDep SrcTree "src" d] | d <- dirs]
          ++ [ (Dep SrcTree "src" dep) | dep <- deps ]
          ++ [ tools_dir ])

nativeOptions = Options {
      optArch                = "",
      optArchFamily          = "",
      optFlags               = [],
      optCxxFlags            = [],
      optDefines             = [],
      optIncludes            = [],
      optDependencies        = [],
      optLdFlags             = [],
      optLdCxxFlags          = [],
      optLibDep              = [],
      optLibs                = [],
      optCxxLibDep           = [],
      optCxxLibs             = [],
      optInterconnectDrivers = [],
      optFlounderBackends    = [],
      extraFlags             = [],
      extraCxxFlags          = [],
      extraDefines           = [],
      extraIncludes          = [],
      extraDependencies      = [],
      extraLdFlags           = [],
      optSuffix              = "",
      optInstallPath         = OptionsPath {
            optPathBin = "/sbin",
            optPathLib = "/lib"
      }
    }

--
-- Compile (and link) a C binary (for the host architecture)
--
compileNativeC :: String -> [String] -> [String] -> [String] -> [String] ->
                  HRule
compileNativeC prog cfiles cflags ldflags localLibs =
    Rule ([ Str nativeCCompiler,
            Str "-o",
            Out "tools" ("/bin" </> prog),
            Str "$(CFLAGS)",
            Str "$(LDFLAGS)" ]
          ++ [ (Str flag) | flag <- cflags ]
          ++ [ (In SrcTree "src" dep) | dep <- cfiles ]
          -- source file needs to be left of ldflags for modern-ish GCC
          ++ [ (Str flag) | flag <- ldflags ]
          ++ [ In BuildTree "tools" ("/lib" </> ("lib" ++ l ++ ".a")) |
               l <- localLibs ])

--
-- Compile a static library for the host architecture
--
compileNativeLib :: String -> [String] -> [String] -> HRule
compileNativeLib name cfiles cflags =
    Rules (
        [ Rule ([ Str nativeCCompiler,
                  Str "-c", In SrcTree "src" s,
                  Str "-o", Out "tools" (objectFilePath nativeOptions s),
                  Str "$(CFLAGS)",
                  Str "$(LDFLAGS)" ]
                ++ [ (Str flag) | flag <- cflags ])
            | s <- cfiles ] ++
        [ Rule ([ Str nativeArchiver,
                  Str "rcs",
                  Out "tools" ("/lib" </> ("lib" ++ name ++ ".a")) ] ++
                [ In BuildTree "tools" o | o <- objs ]) ]
        )
    where
        objs = [ objectFilePath nativeOptions s | s <- cfiles ]
--
-- Build a Technical Note
--
buildTechNote :: String -> String -> Bool -> Bool -> [String] -> HRule
buildTechNote input output bib glo figs =
    buildTechNoteWithDeps input output bib glo figs []
buildTechNoteWithDeps :: String -> String -> Bool -> Bool -> [String] -> [RuleToken] -> HRule
buildTechNoteWithDeps input output bib glo figs deps =
    let
        working_dir = NoDep BuildTree "tools" "/tmp/"
        style_files = [ "bfish-logo.pdf", "bftn.sty", "defs.bib", "barrelfish.bib" ]
    in
      Rule ( [ Dep SrcTree "src" (f ++ ".pdf") | f <- figs]
             ++
             [ Dep SrcTree "src" ("/doc/style" </> f) | f <- style_files ]
             ++
             [ Str "mkdir", Str "-p", working_dir, NL ]
             ++
             deps
             ++
             [ In SrcTree "src" "/tools/run-pdflatex.sh",
               Str "--input-tex", In SrcTree "src" input,
               Str "--working-dir", working_dir,
               Str "--output-pdf", Out "docs" ("/" ++ output),
               Str "--texinput", NoDep SrcTree "src" "/doc/style",
               Str "--bibinput", NoDep SrcTree "src" "/doc/style"
             ]
             ++ (if bib then [ Str "--has-bib" ] else [])
             ++ (if glo then [ Str "--has-glo" ] else [])
           )

---------------------------------------------------------------------
--
-- Transformations on file names
--
----------------------------------------------------------------------

allObjectPaths :: Options -> Args.Args -> [String]
allObjectPaths opts args =
    [objectFilePath opts g
         | g <- (Args.cFiles args)++(Args.cxxFiles args)++(Args.assemblyFiles args)]
    ++
    [generatedObjectFilePath opts g
         | g <- [ flounderBindingPath opts f
                      | f <- (Args.flounderBindings args)]
                ++
                [ flounderExtraBindingPath opts f
                      | (f, _) <- (Args.flounderExtraBindings args)]
                ++
                [ flounderTHCStubPath opts f
                      | f <- (Args.flounderTHCStubs args)]
                ++
                [ skateSchemaPath opts f
                      | f <- (Args.skateSchemas args)]
                ++
                (Args.generatedCFiles args) ++ (Args.generatedCxxFiles args)
    ]

allLibraryPaths :: Options -> Args.Args -> [String]
allLibraryPaths opts args =
    [ libraryPath opts l | l <- Args.addLibraries args ]


allModulesPaths :: Options -> Args.Args -> [String]
allModulesPaths opts args =
    [ libraryPath opts l | l <- Args.addModules args ]

---------------------------------------------------------------------
--
-- Very large-scale macros
--
----------------------------------------------------------------------

--
-- Build an application binary
--

application :: Args.Args
application = Args.defaultArgs {
    Args.buildFunction = applicationBuildFn
    -- we let libraryOs default to Nothing, and grab the right default one for
    -- the architecture we're building in appGetOptionsForArch
}

system :: Args.Args -> Args.Args
system args = args { Args.installDirs = (Args.installDirs args) { Args.bindir = "/sbin" }}

applicationBuildFn :: TreeDB -> String -> Args.Args -> HRule
applicationBuildFn tdb tf args
    | debugFlag && trace (Args.showArgs (tf ++ " Application ") args) False
        = undefined
applicationBuildFn tdb tf args =
    -- The order is important: The application libs
    -- must be inserted before the optLibs (which are inserted in
    -- appBuildArch..). Otherwise it's impossible to override
    -- libc symbols. And we use that trick for instance in libposixcompat
    -- and the VFS.
    Rules (concat [ appLibDeps args arch | arch <- Args.architectures args ] ++
           [ appBuildArch tdb tf args arch | arch <- Args.architectures args ]
        )
    where
      appLibDeps :: Args.Args -> String -> [HRule]
      appLibDeps args arch =
        let
          app = DepApp arch (Args.target args)
          libs = Args.addLibraries args
          mods = Args.addModules args
        in
          [Rule ([LDep app (DepLib arch  l) | l <- (libs)])] ++
          [Rule ([LDep app (DepMod arch l) | l <- (mods)])]

extraIncs libs =
    [ NoDep SrcTree "src" ("/include" </> l) | l <- filter libNeedsInc libs ]
    where
        libNeedsInc lib
           | lib == "lwip"  = True
           | lib == "lwip2" = True
           | otherwise      = False

appGetOptionsForArch :: String -> Args.Args -> Options
appGetOptionsForArch arch args =
    (options arch) { extraIncludes =
                         [ NoDep SrcTree "src" a | a <- Args.addIncludes args]
                         ++
                         [ NoDep BuildTree arch a | a <- Args.addGeneratedIncludes args]
                         ++
                         -- Only add extra include directory for libraries
                         -- that actually need it. -SG,2017-09-19.
                         extraIncs (Args.addLibraries args)
                         ++
                         extraIncs (Args.addLibraries libos),
                     optIncludes = (optIncludes $ options arch) \\
                         [ NoDep SrcTree "src" i | i <- Args.omitIncludes args ],
                     optFlags = (optFlags $ options arch) \\
                                ([ Str f | f <- Args.omitCFlags args ] ++
                                 [ Str f | f <- Args.omitCFlags libos]),
                     optCxxFlags = (optCxxFlags $ options arch) \\
                                   ([ Str f | f <- Args.omitCxxFlags args ] ++
                                    [ Str f | f <- Args.omitCxxFlags libos]),
                     optSuffix = "_for_app_" ++ Args.target args,
                     optLibs = (library_os arch) ++ (optLibs $ options arch),
                     optCxxLibs = (library_os arch) ++ (optCxxLibs $ options arch),
                     extraFlags = (Args.addCFlags args) ++ (Args.addCFlags libos),
                     extraCxxFlags = (Args.addCxxFlags args) ++ (Args.addCFlags libos) ++
                                     (Args.addCxxFlags libos),
                     extraLdFlags = [ Str f | f <- Args.addLinkFlags args ] ++
                                    [ Str f | f <- Args.addLinkFlags libos ],
                     extraDependencies =
                         [Dep BuildTree arch s |
                            s <- Args.addGeneratedDependencies args],
                     optInstallPath = OptionsPath {
                        optPathBin = Args.bindir (Args.installDirs args),
                        optPathLib = Args.libdir (Args.installDirs args)
                     }
                   }
    where
        libos = fromMaybe (fromJust $ Config.libbarrelfish arch) (Args.libraryOs args)
        library_os arch =
          [ In InstallTree arch ("/lib" </> "lib" ++ (Args.target libos) ++ ".a") ] ++
          [ LDep (DepApp arch (Args.target args)) (DepLib arch (Args.target libos))]


fullTarget :: Options -> String -> String -> HRule
fullTarget opts arch appname =
    Phony ((map toUpper arch) ++ "_All") False
        [ Dep BuildTree arch (applicationPath opts appname) ]



appBuildArch tdb tf args arch =
    let -- Fiddle the options
        opts = appGetOptionsForArch arch args
        csrcs = Args.cFiles args
        cxxsrcs = Args.cxxFiles args
        gencsrc = Args.generatedCFiles args
        gencxxsrc = Args.generatedCxxFiles args


        appname = Args.target args
        -- XXX: Not sure if this is correct. Currently assuming that if the app
        -- contains C++ files, we have to use the C++ linker.
        mylink = if cxxsrcs == [] then ldtLink else ldtLinkCxx
        mystdlibs = if cxxsrcs == [] then stdLibDepsRules else stdCxxLibDepsRules
    in
      Rules (
              flounderRules opts args csrcs
              ++
              skateRules opts args csrcs
              ++
              [ mackerelDependencies opts m csrcs | m <- Args.mackerelDevices args ]
              ++
              [ compileCFiles opts csrcs,
                compileCxxFiles opts cxxsrcs,
                compileGeneratedCFiles opts gencsrc,
                compileGeneratedCxxFiles opts gencxxsrc,
                assembleSFiles opts (Args.assemblyFiles args),
                mylink opts (allObjectPaths opts args) appname appname,
                fullTarget opts arch appname
              ]
              ++
              mystdlibs opts (Args.target args)
            )

--
-- Build a driverdomain application binary
--
-- Similar to application, but adds driverdomain library (which implements
-- main) and adds linker script
--

driverdomain :: Args.Args
driverdomain = Args.defaultArgs {
    Args.buildFunction = applicationBuildFn,
    Args.addLinkFlags = ["-T" ++ Config.source_dir ++ "/lib/driverkit/bfdrivers.ld"],
    Args.addLibraries = ["driverdomain"]
}


-- This is pretty much libraryBuildFn, except that it produces the LDep token
-- for the module
drivermoduleBuildFn :: TreeDB -> String -> Args.Args -> HRule
drivermoduleBuildFn tdb tf args | debugFlag && trace (Args.showArgs (tf ++ " DriverModule ") args) False = undefined
drivermoduleBuildFn tdb tf args =
    Rules ([ libBuildArch tdb tf args arch | arch <- Args.architectures args ] ++
           concat [modLibDeps args arch | arch <- Args.architectures args])
    where
      modLibDeps :: Args.Args -> String -> [HRule]
      modLibDeps args arch =
        let
          me = DepMod arch (Args.target args)
          libs = Args.addLibraries args
        in
          [Rule ([LDep me (DepLib arch l) | l <- libs])]

drivermodule :: Args.Args
drivermodule = Args.defaultArgs {
    Args.buildFunction = drivermoduleBuildFn
}

--
-- Build an Arrakis application binary
--
-- The arrakisapplication macro still exists for convenience, new arrakis
-- applications should use application and override libraryOs.
--

arrakisapplication :: Args.Args
arrakisapplication = Args.defaultArgs {
    Args.buildFunction = applicationBuildFn,
    Args.libraryOs     = Config.libarrakis
}

--
-- Build a static library
--

library :: Args.Args
library = Args.defaultArgs {
    Args.buildFunction = libraryBuildFn
    -- we let libraryOs default to Nothing, and grab the right default one for
    -- the architecture we're building in libGetOptionsForArch
}

libraryBuildFn :: TreeDB -> String -> Args.Args -> HRule
libraryBuildFn tdb tf args | debugFlag && trace (Args.showArgs (tf ++ " Library ") args) False = undefined
libraryBuildFn tdb tf args =
    Rules ([ libBuildArch tdb tf args arch | arch <- Args.architectures args ] ++
           concat [libLibDeps args arch | arch <- Args.architectures args])
    where
      libLibDeps :: Args.Args -> String -> [HRule]
      libLibDeps args arch =
        let
          lib = DepLib arch (Args.target args)
          libs = Args.addLibraries args
        in
          [Rule ([LDep lib (DepLib arch l) | l <- libs])]


libGetOptionsForArch arch args =
    (options arch) { extraIncludes =
                         [ NoDep SrcTree "src" a | a <- Args.addIncludes args]
                         ++
                         [ NoDep SrcTree "src" ("/include" </> l) | l <- Args.addLibraries args ],
                     optIncludes = (optIncludes $ options arch) \\
                         [ NoDep SrcTree "src" i | i <- Args.omitIncludes args ],
                     optFlags = (optFlags $ options arch) \\
                                ([ Str f | f <- Args.omitCFlags args ] ++
                                 [ Str f | f <- Args.omitCFlags libos]),
                     optCxxFlags = (optCxxFlags $ options arch) \\
                                   ([ Str f | f <- Args.omitCxxFlags args ] ++
                                    [ Str f | f <- Args.omitCxxFlags libos]),
                     optSuffix = "_for_lib_" ++ Args.target args,
                     extraFlags = (Args.addCFlags args) ++ (Args.addCFlags libos),
                     extraCxxFlags = (Args.addCxxFlags args) ++ (Args.addCFlags libos) ++
                                     (Args.addCxxFlags libos),
                     extraDependencies =
                         [Dep BuildTree arch s | s <- Args.addGeneratedDependencies args]
                   }
    where
        libos = fromMaybe (fromJust $ Config.libbarrelfish arch) (Args.libraryOs args)

libBuildArch tdb tf args arch =
    let -- Fiddle the options
        opts = libGetOptionsForArch arch args
        csrcs = Args.cFiles args
        cxxsrcs = Args.cxxFiles args
        gencsrc = Args.generatedCFiles args
        gencxxsrc = Args.generatedCxxFiles args
    in
      Rules ( flounderRules opts args csrcs
              ++
              skateRules opts args csrcs
              ++
              [ mackerelDependencies opts m csrcs | m <- Args.mackerelDevices args ]
              ++
              [ compileCFiles opts csrcs,
                compileCxxFiles opts cxxsrcs,
                compileGeneratedCFiles opts gencsrc,
                compileGeneratedCxxFiles opts gencxxsrc,
                assembleSFiles opts (Args.assemblyFiles args),
                 -- we dont pass the libraries, since they will be added in the linker phase
                staticLibrary opts (Args.target args) (allObjectPaths opts args) []
              ]
            )

--
-- Library dependecies
--

-- The following code is under heavy construction, and also somewhat ugly
data LibDepTree = LibDep String | LibDeps [LibDepTree] deriving (Show,Eq)

-- manually add dependencies for now (it would be better if each library
-- defined each own dependencies locally, but that does not seem to be an
-- easy thing to do currently
libposixcompat_deps   = LibDeps [ LibDep "posixcompat",
                                  (libvfs_deps_all "vfs"), LibDep "term_server" ]
liblwip_deps          = LibDeps $ [ LibDep x | x <- deps ]
    where deps = ["lwip" ,"net_if_raw" ,"timer" ,"hashtable", "netbench" ]
libnetQmng_deps       = LibDeps $ [ LibDep x | x <- deps ]
    where deps = ["net_queue_manager"]
libnfs_deps           = LibDeps $ [ LibDep "nfs", liblwip_deps]

-- we need to make vfs more modular to make this actually useful
data VFSModules = VFS_RamFS | VFS_NFS | VFS_BlockdevFS | VFS_FAT
vfsdeps :: [VFSModules] -> String -> [LibDepTree]
vfsdeps [] t                  = [LibDep t]
vfsdeps (VFS_RamFS:xs) t      = [] ++ vfsdeps xs t
vfsdeps (VFS_NFS:xs) t        = [libnfs_deps] ++ vfsdeps xs t
vfsdeps (VFS_BlockdevFS:xs) t = [LibDep "ahci"] ++ vfsdeps xs t
vfsdeps (VFS_FAT:xs) t        = [] ++ vfsdeps xs t

libvfs_deps_all t        = LibDeps $ (vfsdeps [VFS_NFS, VFS_RamFS, VFS_BlockdevFS,
                                               VFS_FAT] t)
libvfs_deps_noblockdev t = LibDeps $ (vfsdeps [VFS_NFS, VFS_RamFS] t)
libvfs_deps_nonfs t      = LibDeps $ (vfsdeps [VFS_RamFS, VFS_BlockdevFS, VFS_FAT] t)
libvfs_deps_nfs t        = LibDeps $ (vfsdeps [VFS_NFS] t)
libvfs_deps_ramfs t      = LibDeps $ (vfsdeps [VFS_RamFS] t)
libvfs_deps_blockdevfs t = LibDeps $ (vfsdeps [VFS_BlockdevFS] t)
libvfs_deps_fat t        = LibDeps $ (vfsdeps [VFS_FAT, VFS_BlockdevFS] t)

-- flatten the dependency tree
flat :: [LibDepTree] -> [LibDepTree]
flat [] = []
flat ((LibDep  l):xs) = [LibDep l] ++ flat xs
flat ((LibDeps t):xs) = flat t ++ flat xs

str2dep :: String -> LibDepTree
str2dep  str
    | str == "vfs"           = libvfs_deps_all str
    | str == "vfs_ramfs"     = libvfs_deps_ramfs str
    | str == "vfs_nonfs"     = libvfs_deps_nonfs str
    | str == "vfs_noblockdev"= libvfs_deps_noblockdev str
    | str == "lwip"          = liblwip_deps
    | str == "netQmng"       = libnetQmng_deps
    | otherwise              = LibDep str

-- get library depdencies
--   we need a specific order for the .a, so we define a total order
libDeps :: [String] -> [String]
libDeps xs = [x | (LibDep x) <- (sortBy xcmp) . nub . flat $ map str2dep xs ]
    where xord = [  "crypto"
                  , "zlib"
                  , "posixcompat"
                  , "posixcompat_arrakis"
                  , "term_server"
                  , "vfs"
                  , "ahci"
                  , "megaraid"
                  , "nfs"
                  , "net_queue_manager"
                  , "bfdmuxvm"
                  , "lwip"
                  , "arranet"
                  , "e1000n"
                  , "e10k"
                  , "e10k_vf"
                  , "contmng"
                  , "procon"
                  , "net_if_raw"
                  , "vfsfd"
                  , "timer"
                  , "hashtable"]
          xcmp (LibDep a) (LibDep b) = compare (elemIndex a xord) (elemIndex b xord)


--
-- Build a CPU driver
--

cpuDriver :: Args.Args
cpuDriver = Args.defaultArgs { Args.buildFunction = cpuDriverBuildFn,
                               Args.target = "cpu",
                               Args.driverType = "cpu" }

bootDriver :: Args.Args
bootDriver = Args.defaultArgs { Args.buildFunction = cpuDriverBuildFn,
                                Args.driverType = "boot" }

-- CPU drivers are built differently
cpuDriverBuildFn :: TreeDB -> String -> Args.Args -> HRule
cpuDriverBuildFn tdb tf args = Rules []

bootDriverBuildFn :: TreeDB -> String -> Args.Args -> HRule
bootDriverBuildFn tdb tf args = Rules []

--
-- Build a platform
--
platform :: String -> [ String ] -> [ ( String, String ) ] -> String -> HRule
platform name archs files docstr =
  if null $ archs Data.List.\\ Config.architectures then
    Rules [
      Phony name False
      ([ NStr "@echo 'Built platform <",  NStr name, NStr ">'" ] ++
       [ Dep BuildTree arch file | (arch,file) <- files ]) ,
      Phony "clean-platform" True
      ([ NStr "@echo 'Cleaning platform <",  NStr name, NStr ">'", NL,
         Str "$(RM)" ] ++
       [ NoDep BuildTree arch file | (arch,file) <- files ]),
      Phony ("install_" ++ name) False
      ([ NStr "@echo 'Installing platform <",  NStr name, NStr ">'" ] ++
       [ NL, Str "rsync -v -a --relative" ] ++
       [ In BuildTree arch file | (arch,file) <- files ] ++
       [ Str "${INSTALL_PREFIX}" ]),
      Phony "help-platforms" True
      [ Str "@echo \"", NStr name, Str ":\\n\\t", NStr docstr, Str "\"",
        Dep BuildTree "root" "/help-platforms-header" ]
      ]
  else
    Rules []

--
-- Creates a
--
armv7Image ::[Char] -> [Char] -> [Char] -> [Char] -> [[Char]]-> [[Char]]  -> HRule
armv7Image target bootTarget cpuTarget physBase modules_generic modules =
    let bootDriver = "/sbin/boot_" ++ bootTarget
        cpuDriver  = "/sbin/cpu_"  ++ cpuTarget
        image      = "/" ++ target ++ "_image"
    in Rules [
        Rule ([ In BuildTree "tools" "/bin/arm_bootimage",
                In BuildTree "root" ("/platforms/arm/menu.lst."++target),
                In BuildTree "armv7" bootDriver,
                Out "root" image,
                NoDep BuildTree "root" "/",
                Str physBase ] ++
                [ (Dep BuildTree "armv7" m) | m <- modules ] ++
                [ (Dep BuildTree "" m) | m <- modules_generic ] ),

        Rule ([ Str Config.arm_objcopy,
                Str "-O binary",
                In BuildTree "root" image,
                Out "root" (image ++ ".bin") ]),
        Rule ([ In SrcTree "tools" "/tools/arm_boot/gdb_script.sh",
                Str Config.arm_objdump,
                In BuildTree "root" image,
                In BuildTree "armv7" bootDriver,
                In BuildTree "armv7" cpuDriver,
                Out "root" (image ++ "-gdb.gdb") ])
        ]

armv8Image ::[Char] -> [Char] -> [Char] -> [Char] -> [[Char]] -> [[Char]] -> HRule
armv8Image target menu bootTarget cpuTarget modules_generic modules =
    let bootDriver = "/sbin/boot_" ++ bootTarget
        cpuDriver  = "/sbin/cpu_"  ++ cpuTarget
        blob       = "/" ++ target ++ "blob"
        blob_o     = "/" ++ target ++ "blob.o"
        image_o    = "/" ++ target ++ "_image.o"
        image      = "/" ++ target ++ "_image.efi"
    in Rules [
        Rule ([ In BuildTree "tools" "/bin/armv8_bootimage",
            In BuildTree "root" ("/platforms/arm/menu.lst." ++ menu),
            Out "root" blob,
            NoDep BuildTree "root" "/" ] ++
            [ (Dep BuildTree "armv8" m) | m <- modules ] ++
            [ (Dep BuildTree "" m) | m <- modules_generic ] ),
        Rule ([ Str Config.aarch64_objcopy,
            Str "-I binary",
            Str "-O elf64-littleaarch64",
            Str "-B aarch64",
            Str ("--redefine-sym _binary_" ++ target ++ "blob_start=barrelfish_blob_start"),
            Str ("--redefine-sym _binary_" ++ target ++ "blob_end=barrelfish_blob_end"),
            Str ("--redefine-sym _binary_" ++ target ++ "blob_size=barrelfish_blob_size"),
            In BuildTree "root" blob,
            Out "root" blob_o ]),
        Rule ([ Str "aarch64-linux-gnu-ld",
            Str "/usr/lib/crt0-efi-aarch64.o",
            Str "-znocombreloc",
            Str "-Bsymbolic",
            Str "-T /usr/lib/elf_aarch64_efi.lds",
            Str "-shared",
            Str "--no-undefined",
            Str "--defsym=EFI_SUBSYSTEM=10",
            In BuildTree "root" blob_o,
            In BuildTree "armv8" "/tools/armv8_bootimage/efi_loader.o",
            Str "-o",
            Out "root" image_o,
            Str "/usr/lib/libgnuefi.a",
            Str "/usr/lib/libefi.a" ]),
        Rule ([ Str Config.aarch64_objcopy,
            Str "-O binary",
            In BuildTree "root" image_o,
            Out "root" image ])
        ]


armv8EFIImage ::[Char] -> [Char] -> [Char] -> [Char] -> [[Char]] -> [[Char]] -> HRule
armv8EFIImage target menu bootTarget cpuTarget modules_generic modules =
    let bootDriver = "/sbin/boot_" ++ bootTarget
        cpuDriver  = "/sbin/cpu_"  ++ cpuTarget
        target_image = "/" ++ target ++ "_image"
    in Rules [
        Rule $ [
        In SrcTree "tools" "/tools/harness/efiimage.py",
        In BuildTree "root" ("/platforms/arm/menu.lst." ++ menu),
        Str Config.install_dir,
        Out "root" target_image
        ]
        ++ [(Dep BuildTree "armv8" bootDriver)]
        ++ [(Dep BuildTree "armv8" cpuDriver)]
        ++ [(Dep BuildTree "armv8" f) | f <- modules ]
        ++ [(Dep BuildTree "" f) | f <- modules_generic ]
        ]
--
-- Boot an image.
--   name: the boot target name
--   archs: list of architectures required
--   tokens: the hake tokens for the target
--   docstr: description of the target
--
boot :: String -> [ String ] -> [ RuleToken ] -> String -> HRule
boot name archs tokens docstr =
  if null $ archs Data.List.\\ Config.architectures then
    Rules [
      Phony name False tokens,
      Phony "help-boot" True
      [ Str "@echo \"", NStr name, Str ":\\n\\t", NStr docstr, Str "\"",
        Dep BuildTree "root" "/help-boot-header"  ]
      ]
  else
    Rules []

barebones_simargs :: String -> [RuleToken]
barebones_simargs imgpath = [
        Str "-C terminal.start_telnet=0",
        Str "-C terminal1.start_telnet=0",
        Str "--data /home/netos/tools/fvp-uefi/bl1.bin@0x0000000000",
        Str "--data /home/netos/tools/fvp-uefi/fip.bin@0x0008000000",
        NStr "-C mmc.p_mmc_file=",
        In BuildTree "root" imgpath,
        Str "-C uart.unbuffered_output=1",
        Str "-C uart.out_file=-",
        Str "-C uart1.unbuffered_output=1",
        Str "-C uart1.out_file=-"]

fvp_simargs :: String -> [RuleToken]
fvp_simargs imgpath = [
        -- Don't try to pop an LCD window up
        Str "-C bp.vis.disable_visualisation=1",
        -- # Don't start a telnet xterm
        Str "-C bp.terminal_0.start_telnet=0",
        Str "-C bp.terminal_1.start_telnet=0",
        Str "-C bp.secureflashloader.fname=/home/netos/tools/fvp-uefi/bl1.bin",
        Str "-C bp.flashloader0.fname=/home/netos/tools/fvp-uefi/fip.bin",
        NStr "-C bp.mmc.p_mmc_file=",
        In BuildTree "root" imgpath,
        Str "-C bp.pl011_uart0.unbuffered_output=1",
        -- This has to be the last parameter because otherwise the command
        -- passed to the OS has incorrect parameters. Don't know why
        -- MH 11/2016
        Str "-C bp.pl011_uart0.out_file=-"]


-- Build buildpath/isim_system using the sgproj file
fastmodel_sim_target :: RuleToken -> String -> [RuleToken]
fastmodel_sim_target sgproj buildpath=
    let
        binary = buildpath </> "isim_system"
        build_dir = (Dep InstallTree "tools" (buildpath </> ".marker"))
    in
    [
        Str ("ARM_FM_ROOT=" ++ Config.fastmodels_root),
        In SrcTree "src" "/tools/fastmodels/simgen",
        Str "--num-comps-file 50",
        Str "--gen-sysgen",
        Str "--build-directory",
        NoDep BuildTree "tools" buildpath,
        Str "--warnings-as-errors ",
        Str "-p", sgproj,
        Str "-b", Target "tools" binary,
        build_dir ]

boot_fastmodels :: String -> [ String ] -> String -> String -> (String -> [RuleToken]) -> String -> HRule
boot_fastmodels name archs img sims simargs docstr =
    let
        sgproj = In SrcTree "src" ("/platforms" </> (sims ++ ".sgproj"))
    in
        boot_fastmodels_int name archs img sims sgproj simargs docstr


boot_fastmodels_lisa :: String -> [ String ] -> String -> String -> (String -> [RuleToken]) -> String -> HRule
boot_fastmodels_lisa name archs img sims simargs docstr =
    let
        sgproj = In BuildTree "" ("/platforms" </> sims <.> ".sgproj")
        -- We assume here that the  sgproj depends exactly on one lisa
        -- file with the same base name
        lisaDep = Dep BuildTree "" ("/platforms" </> sims <.> ".lisa")
        buildpath = "/fastmodels" </> (sims ++ "_Build")
        binary = buildpath </> "isim_system"
        imgpath = "/" ++ img

        boot_target = [In InstallTree "tools" binary]
                      ++ (simargs imgpath)
                      ++ [Dep InstallTree "tools" binary ]
      in
        if null $ archs Data.List.\\ Config.architectures then
            Rules [
            Rule $ [lisaDep] ++ fastmodel_sim_target sgproj buildpath,
            Phony name False boot_target,
            Phony "help-boot" True
            [ Str "@echo \"", NStr name, Str ":\\n\\t", NStr docstr, Str "\"",
                Dep BuildTree "root" "/help-boot-header"  ]
            ]
        else
            Rules []

boot_fastmodels_int :: String -> [ String ] -> String -> String -> RuleToken -> (String -> [RuleToken]) -> String -> HRule
boot_fastmodels_int name archs img sims sgproj simargs docstr =
  let
    imgpath = "/" ++ img
    buildpath = "/fastmodels" </> (sims ++ "_Build")
    binary = buildpath </> "isim_system"
    tools_dir = (Dep InstallTree "tools" "/tools/.marker")

    boot_target = [In InstallTree "tools" binary]
                  ++ (simargs imgpath)
                  ++ [Dep InstallTree "tools" binary ]
  in
    if null $ archs Data.List.\\ Config.architectures then
        Rules [
        Rule $ fastmodel_sim_target sgproj buildpath,
        Phony name False boot_target,
        Phony "help-boot" True
        [ Str "@echo \"", NStr name, Str ":\\n\\t", NStr docstr, Str "\"",
            Dep BuildTree "root" "/help-boot-header"  ]
        ]
    else
        Rules []

--
-- Copy a file from the source tree
--
copyFile :: TreeRef -> String -> String -> String -> String -> HRule
copyFile stree sarch spath darch dpath =
  Rule [ Str "cp", Str "-v", In stree sarch spath, Out darch dpath ]

getExternalDependency :: String -> String -> [ HRule ]
getExternalDependency url name =
    [
        Rule ( [
            Str "curl",
            Str "--insecure",
            Str "--create-dirs",
            Str "-o",
            Out "cache" name,
            Str url
        ] ),
        copyFile SrcTree "cache" name "" name
    ]

scalebenchProgLoc = In SrcTree "root" "/tools/harness/scalebench.py"

-- A testJob corresponds to a job in jenkins. It has name, description
-- and will execute a sequence of scalebench invocations
testJob :: String -> String -> [RuleToken] -> HRule
testJob name desc exec =
    Rules [
        Phony "help-tests" True [Str "@echo \"", NStr name, Str ":\\t", NStr desc, Str "\""],
        Phony name False exec
    ]


-- Execute scalebench

scalebenchO :: String -> [String] -> [String] -> [RuleToken]
scalebenchO buildType tests machines =
    [
       Str "mkdir", Str "-p", Str result_dir, NL,
       scalebenchProgLoc
    ] ++
    test_tokens ++
    machine_tokens ++
    [
       Str "--debug"
    ] ++
    build_args buildType ++
    [
       -- positional arguments
       Str Config.source_dir,          -- sourcedir
       Str result_dir,             -- resultdir
       NL
    ]
    where
        build_args "" = [Str "-e", Str "."]
        build_args x  = [Str "-b", Str x]

        result_dir = "results/"
        test_tokens = concat [[Str "-t", Str t] | t <- tests]
        machine_tokens = concat [[Str "-m", Str t] | t <- machines]

scalebench :: [String] -> [String] -> [RuleToken]
scalebench = scalebenchO ""

