--------------------------------------------------------------------------
-- Copyright (c) 2007-2015 ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Universitaetstasse 6, CH-8092 Zurich. Attn: Systems Group.
--
-- Arguments to major Hake targets
--
--------------------------------------------------------------------------

module Args where

import HakeTypes
import TreeDB

data Args = Args {
      buildFunction :: TreeDB -> String -> Args -> HRule,
      target :: String,
      driverType :: String,
      cFiles :: [String],
      generatedCFiles :: [String],
      cxxFiles :: [String],
      generatedCxxFiles :: [String],
      assemblyFiles :: [String],
      flounderDefs :: [String],
      flounderBindings :: [String], -- built stubs for all enabled backends
      flounderExtraDefs :: [(String, [String])],
      flounderExtraBindings :: [(String, [String])], -- build stubs for specific backends
      flounderTHCDefs :: [String], -- TODO: this can probably be subsumed into the above?
      flounderTHCStubs :: [String], -- TODO: this can probably be subsumed into the above?
      mackerelDevices :: [String],
      addCFlags :: [String],
      addCxxFlags :: [String],
      omitCFlags :: [String],
      omitCxxFlags :: [String],
      addIncludes :: [String],
      addGeneratedIncludes :: [String],
      omitIncludes :: [String],
      addLinkFlags :: [String],
      addLibraries :: [String],
      addModules :: [String],
      addGeneratedDependencies :: [String],
      architectures :: [String],
      skateSchemaDefs :: [String],  -- just the Skate Schema headers
      skateSchemas :: [String],      -- Schema headers and functions
      installDirs :: InstallDirs
}

data InstallDirs = InstallDirs {
    bindir :: String,
    libdir :: String
}

defaultArgs = Args {
      buildFunction = defaultBuildFn,
      target = "",
      driverType = "",
      cFiles = [],
      generatedCFiles = [],
      cxxFiles = [],
      generatedCxxFiles = [],
      assemblyFiles = [],
      flounderDefs = [],
      flounderBindings = [],
      flounderExtraDefs = [],
      flounderExtraBindings = [],
      flounderTHCDefs = [],
      flounderTHCStubs = [],
      mackerelDevices = [],
      addCFlags = [],
      addCxxFlags = [],
      omitCFlags = [],
      omitCxxFlags = [],
      addIncludes = [],
      addGeneratedIncludes = [],
      omitIncludes = [],
      addLinkFlags = [],
      addLibraries = [],
      addModules = [],
      addGeneratedDependencies = [],
      architectures = allArchitectures,
      skateSchemaDefs = [],
      skateSchemas = [],
      installDirs = InstallDirs {
            bindir = "/sbin",
            libdir = "/lib"
      }
}

allArchitectures = [ "x86_64", "x86_32", "armv7", "armv8", "k1om" ]
allArchitectureFamilies = [ "x86_64", "x86_32", "arm", "k1om" ]
-- architectures that currently support THC
thcArchitectures = ["x86_64", "x86_32"]

-- all known flounder backends that we might want to generate defs for
allFlounderBackends
    = [ "lmp", "ump", "ump_ipi", "loopback", "rpcclient", "msgbuf", "multihop", "ahci", "local" ]

defaultBuildFn :: TreeDB -> String -> Args -> HRule
defaultBuildFn _ f _ =
    Error ("Bad use of default Args in " ++ f)

showArgs :: String -> Args -> String
showArgs prefix a =
    prefix ++ "Args:"
    ++ "\n  target:                " ++ (show $ target a)
    ++ "\n  cFiles:                " ++ (show $ cFiles a)
    ++ "\n  generatedCFiles:       " ++ (show $ generatedCFiles a)
    ++ "\n  cxxFiles:              " ++ (show $ cxxFiles a)
    ++ "\n  generatedCxxFiles      " ++ (show $ generatedCxxFiles a)
    ++ "\n  assemblyFiles:         " ++ (show $ assemblyFiles a)
    ++ "\n  flounderDefs:          " ++ (show $ flounderDefs a)
    ++ "\n  flounderBindings:      " ++ (show $ flounderBindings a)
    ++ "\n  flounderExtraDefs:     " ++ (show $ flounderExtraDefs a)
    ++ "\n  flounderExtraBindings: " ++ (show $ flounderExtraBindings a)
    ++ "\n  flounderTHCDefs:       " ++ (show $ flounderTHCDefs a)
    ++ "\n  flounderTHCStubs:      " ++ (show $ flounderTHCStubs a)
    ++ "\n  addCFlags:             " ++ (show $ addCFlags a)
    ++ "\n  addCxxFlags:           " ++ (show $ addCxxFlags a)
    ++ "\n  omitCFlags:            " ++ (show $ omitCFlags a)
    ++ "\n  omitCxxFlags:          " ++ (show $ omitCxxFlags a)
    ++ "\n  addIncludes:           " ++ (show $ addIncludes a)
    ++ "\n  omitIncludes:          " ++ (show $ omitIncludes a)
    ++ "\n  addLinkFlags:          " ++ (show $ addLinkFlags a)
    ++ "\n  addLibraries:          " ++ (show $ addLibraries a)
    ++ "\n  addModules:            " ++ (show $ addModules a)
    ++ "\n  addDeps:               " ++ (show $ addGeneratedDependencies a)
    ++ "\n  architectures:         " ++ (show $ architectures a)
    ++ "\n  skateSchemaDefs:       " ++ (show $ skateSchemaDefs a)
    ++ "\n  skateSchemas:          " ++ (show $ skateSchemas a)
    ++ "\n"
