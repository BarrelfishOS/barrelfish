
{-
  Hake: a meta build system for Barrelfish

  Copyright (c) 2009, 2015, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universitaetstasse 6, CH-8092 Zurich. Attn: Systems Group.
-}



-- Asynchronous IO for walking directories
import Control.Concurrent.Async
import Control.DeepSeq

import Control.Exception.Base
import Control.Monad

import Exception

import Data.Dynamic
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import qualified Data.Map.Strict as Map

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Debug.Trace

-- The GHC API.  We use the mtl-compatible version in order to use liftIO
-- within the GHC monad.
import GHC hiding (Target, Ghc, runGhc, FunBind, Match)
import GHC.Paths (libdir)
import Control.Monad.Ghc

-- We parse and pretty-print Hakefiles.
import Language.Haskell.Exts


-- Hake components
import RuleDefs
import HakeTypes
import qualified Args
import qualified Config
import TreeDB
import LibDepTree

data HakeError = HakeError String Int
    deriving (Show, Typeable)
instance Exception HakeError

--
-- Command line options and parsing code
--
data Opts = Opts { opt_makefilename :: String,
                   opt_installdir :: String,
                   opt_sourcedir :: String,
                   opt_bfsourcedir :: String,
                   opt_builddir :: String,
                   opt_ghc_libdir :: String,
                   opt_abs_installdir :: String,
                   opt_abs_sourcedir :: String,
                   opt_abs_bfsourcedir :: String,
                   opt_abs_builddir :: String,
                   opt_usage_error :: Bool,
                   opt_architectures :: [String],
                   opt_verbosity :: Integer
                 }
          deriving (Show,Eq)

parse_arguments :: [String] -> Opts
parse_arguments [] =
  Opts { opt_makefilename = "Makefile",
         opt_installdir = Config.install_dir,
         opt_sourcedir = Config.source_dir,
         opt_bfsourcedir = Config.source_dir,
         opt_builddir = ".",
         opt_ghc_libdir = libdir,
         opt_abs_installdir = "",
         opt_abs_sourcedir = "",
         opt_abs_bfsourcedir = "",
         opt_abs_builddir = "",
         opt_usage_error = False,
         opt_architectures = [],
         opt_verbosity = 1 }
parse_arguments ("--install-dir" : (s : t)) =
  (parse_arguments t) { opt_installdir = s }
parse_arguments ("--source-dir" : s : t) =
  (parse_arguments t) { opt_sourcedir = s }
parse_arguments ("--bfsource-dir" : s : t) =
  (parse_arguments t) { opt_bfsourcedir = s }
parse_arguments ("--build-dir" : s : t) =
  (parse_arguments t) { opt_builddir = s }
parse_arguments ("--ghc-libdir" : (s : t)) =
  (parse_arguments t) { opt_ghc_libdir = s }
parse_arguments ("--output-filename" : s : t) =
  (parse_arguments t) { opt_makefilename = s }
parse_arguments ("--quiet" : t ) =
  (parse_arguments t) { opt_verbosity = 0 }
parse_arguments ("--verbose" : t ) =
  (parse_arguments t) { opt_verbosity = 2 }
parse_arguments ("--architecture" : a : t ) =
  let
    o2 = parse_arguments t
    arches = (a : opt_architectures o2)
  in
    o2 { opt_architectures = arches }
parse_arguments _ =
  (parse_arguments []) { opt_usage_error = True }

usage :: String
usage = unlines [ "Usage: hake <options>",
                  "   --source-dir <dir> (required)",
                  "   --bfsource-dir <dir> (defaults to source dir)",
                  "   --install-dir <dir> (defaults to source dir)",
                  "   --ghc-libdir <dir> (defaults to " ++ libdir ++ ")",
                  "   --quiet",
                  "   --verbose"
                ]

-- Check the configuration options, returning an error string if they're
-- invalid.
configErrors :: Maybe String
configErrors
    | unknownArchs /= [] =
        Just ("unknown architecture(s) specified: " ++
        (concat $ intersperse ", " unknownArchs))
    | Config.architectures == [] =
        Just "no architectures defined"
    | Config.lazy_thc && not Config.use_fp =
        Just "Config.use_fp must be true to use Config.lazy_thc."
    | otherwise =
        Nothing
    where
        unknownArchs = Config.architectures \\ Args.allArchitectures

-- Walk the source tree and build a complete list of pathnames, loading any
-- Hakefiles.
listFiles :: FilePath -> IO ([FilePath], [(FilePath, String)])
listFiles root = listFiles' root root

listFiles' :: FilePath -> FilePath -> IO ([FilePath], [(FilePath, String)])
listFiles' root current
    | ignore (takeFileName current) = return ([], [])
    | otherwise = do
        isdir <- doesDirectoryExist current
        if isdir then do
            children <- getDirectoryContents current
            walkchildren $ filter isRealChild children
        else do
            hake <- maybeHake current
            return ([makeRelative root current], hake)
    where
        -- Walk the child directories in parallel.  This speeds things up
        -- dramatically over NFS, with its high latency.
        walkchildren :: [FilePath] -> IO ([FilePath], [(FilePath, String)])
        walkchildren children = do
            children_async <- mapM (async.walkchild) children
            results <- mapM wait children_async
            return $ joinResults results

        joinResults :: [([a],[b])] -> ([a],[b])
        joinResults [] = ([],[])
        joinResults ((as,bs):xs) =
            let (as',bs') = joinResults xs in
                (as ++ as', bs ++ bs')

        walkchild :: FilePath -> IO ([FilePath], [(FilePath, String)])
        walkchild child = listFiles' root (current </> child)

        -- Load Hakfiles eagerly.  This amounts to <1MB for Barrelfish (2015).
        maybeHake path
            | takeFileName path == "Hakefile" = do
                contents <- readFile path
                return [(path, contents)]
            | otherwise = return []

        -- Don't descend into revision-control or build directories.
        ignore :: FilePath -> Bool
        ignore "CMakeFiles" = True
        ignore ".hg"        = True
        ignore ".git"       = True
        ignore ('.':[])     = False
        ignore ('.':xs)     = True
        ignore "build"      = True
        ignore _            = False

        -- We ignore self-links and parent-links
        isRealChild :: FilePath -> Bool
        isRealChild "."  = False
        isRealChild ".." = False
        isRealChild _    = True

--
-- Hake parsing using the GHC API
--

-- We invoke GHC to parse the Hakefiles in a preconfigured environment,
-- to implement the Hake DSL.
evalHakeFiles :: FilePath -> Opts -> TreeDB -> [(FilePath, String)] ->
                 (FilePath -> HRule -> Ghc a) -> IO ([a])
evalHakeFiles the_libdir o srcDB hakefiles rulef =
    --defaultErrorHandler defaultFatalMessager defaultFlushOut $
    errorHandler $
        runGhc (Just the_libdir) $
        driveGhc o srcDB hakefiles rulef

-- This is the code that executes in the GHC monad.
driveGhc :: forall a. Opts -> TreeDB -> [(FilePath, String)] ->
            (FilePath -> HRule -> Ghc a) -> Ghc ([a])
driveGhc o srcDB hakefiles rulef = do
    -- Set the RTS flags
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags {
        importPaths = module_paths,
        hiDir = Just "./hake",
        objectDir = Just "./hake"
    }

    -- Set compilation targets i.e. everything that needs to be built from
    -- source (*.hs).
    targets <- mapM (\m -> guessTarget m Nothing) source_modules
    setTargets targets
    load LoadAllTargets

    -- Import both system and Hake modules.
    setContext
        ([IIDecl $ simpleImportDecl $ mkModuleName m |
            m <- modules] ++
         [IIDecl $ (simpleImportDecl $ mkModuleName m) {
                ideclQualified = True
          } | m <- qualified_modules])

    -- Collect rules from Hakefiles
    collectRules hakefiles

    where
        module_paths = [ (opt_installdir o) </> "hake", ".",
                         (opt_bfsourcedir o) </> "hake" ]
        source_modules = [ "HakeTypes", "RuleDefs", "Args", "Config",
                           "TreeDB" ]
        modules = [ "Prelude", "System.FilePath", "HakeTypes", "RuleDefs",
                    "Args", "TreeDB"  ]
        qualified_modules = [ "Config", "Data.List" ]

        -- Evaluate one Hakefile, and emit its Makefile section.  We collect
        -- referenced directories as we go, to generate the 'directories'
        -- rules later.
        collectRules' :: [a] -> [(FilePath, String)] -> Ghc ([a])
        collectRules' rules [] = return rules
        collectRules' rules ((abs_hakepath, contents):hs) = do
            let hakepath = makeRelative (opt_sourcedir o) abs_hakepath
            rule <- evaluate hakepath contents
            ruleout <- rulef hakepath rule
            collectRules' (ruleout : rules) hs

        collectRules :: [(FilePath, String)] -> Ghc ([a])
        collectRules hs = collectRules' [] hs

        -- Evaluate a Hakefile, returning something of the form
        -- Rule [...]
        evaluate :: FilePath -> String -> Ghc HRule
        evaluate hakepath hake_raw = do
            case hake_parse of
                Left hake_expr -> do
                    let hake_wrapped =
                            prettyPrintWithMode (defaultMode {layout = PPNoLayout}) $
                                wrapHake hakepath hake_expr

                    -- Evaluate in GHC
                    val <- ghandle handleFailure $
                                dynCompileExpr -- $ traceShowId
                                $ hake_wrapped ++ " :: TreeDB -> HRule"



                    rule <-
                        case fromDynamic val of
                            Just r -> return r
                            Nothing -> throw $
                                HakeError (hakepath ++
                                           " - Compilation failed") 1

                    -- Path resolution
                    let resolved_rule =
                            resolvePaths o (takeDirectory hakepath)
                                           (rule srcDB)
                    return resolved_rule
                Right hake_error -> throw hake_error
            where
                hake_parse = parseHake hakepath hake_raw

                handleFailure :: SomeException -> Ghc Dynamic
                handleFailure e
                    = throw $ HakeError (hakepath ++ ":\n" ++ show e) 1

errorHandler :: (ExceptionMonad m, MonadIO m) => m a -> m a
errorHandler inner =
  ghandle (\exception -> liftIO $ do
           hFlush stdout
           handleIOException exception
           handleAsyncException exception
           handleExitException exception
           handleHakeError exception
           throw exception
          ) $

  -- error messages propagated as exceptions
  ghandle
            (\(ge :: GhcException) -> liftIO $ do
                hFlush stdout
                throw $ HakeError (show ge) 1
            ) $
  inner
  where
    handleIOException e =
        case fromException e of
            Just (ioe :: IOException) ->
                throw $ HakeError ("IO Exception: " ++ (show ioe)) 1
            _ -> return ()

    handleAsyncException e =
        case fromException e of
            Just UserInterrupt ->
                throw $ HakeError "Interrupted" 1
            Just StackOverflow ->
                throw $ HakeError ("Stack Overflow: use +RTS " ++
                                   "-K<size> to increase it") 1
            _ -> return ()

    handleExitException e =
        case fromException e of
            Just ExitSuccess ->
                throw $ HakeError "GHC terminated early" 1
            Just (ExitFailure n) ->
                throw $ HakeError "GHC terminated early" n
            _ -> return ()

    handleHakeError e =
        case fromException e of
            Just (HakeError s n) -> throw $ HakeError s n
            _ -> return ()

printSrcLoc :: Language.Haskell.Exts.SrcLoc -> String
printSrcLoc sl =
    srcFilename sl ++ ":" ++
    (show $ srcLine sl) ++ "." ++
    (show $ srcColumn sl)

-- Parse a Hakefile, prior to wrapping it with Hake definitions
parseHake :: FilePath -> String -> Either (Exp SrcSpanInfo) HakeError
parseHake filename contents =
    case result of
        ParseOk e -> Left e
        ParseFailed loc str ->
            Right $ HakeError (printSrcLoc loc ++ " - " ++ str) 1
    where
        result =
            parseExpWithMode
                (defaultParseMode {
                    parseFilename = filename,
                    baseLanguage = Haskell2010 })
                contents

-- Split a Hake rule up by token type.  It's more efficient to do this
-- in a single pass, than to filter each as it's required.
data CompiledRule =
    CompiledRule {
        ruleOutputs    :: S.Set RuleToken,
        ruleDepends    :: S.Set RuleToken,
        rulePreDepends :: S.Set RuleToken,
        ruleBody       :: [RuleToken],
        ruleDirs       :: S.Set FilePath
    }


-- Get the relative rule from an absolute rule pair
makeRelativeRule :: RuleToken -> RuleToken
makeRelativeRule (Abs _ t) = t
makeRelativeRule t = t

compileRule :: [RuleToken] -> CompiledRule
compileRule [] = CompiledRule S.empty  S.empty  S.empty  []  S.empty
compileRule (t:ts) =
    let CompiledRule outs deps predeps body dirs = compileRule ts
        outs'    = if isOutput t then S.insert (makeRelativeRule t) outs else outs
        deps'    = if isDependency t then S.insert (makeRelativeRule t) deps else deps
        predeps' = if isPredependency t then S.insert (makeRelativeRule t) predeps else predeps
        body'    = if inRule t then t:body else body
        dirs'    = if isFileRef t &&
                      inTree (frPath t) &&
                      takeDirectory (frPath t) /= "."
                   then S.insert (replaceFileName (frPath t) ".marker") dirs
                   else dirs
    in
    CompiledRule outs' deps' predeps' body' dirs'
    where
        inTree :: FilePath -> Bool
        inTree p =
            case splitDirectories p of
                "..":_ -> False
                "/":_ -> False
                _ -> True

-- We wrap the AST of the parsed Hakefile to defind the 'find' and 'build'
-- primitives, and generate the correct expression type (HRule).  The result
-- is an unevaluted function [FilePath] -> HRule, that needs to be supplied
-- with the list of all files in the source directory.
wrapHake :: FilePath -> Exp SrcSpanInfo -> Exp SrcSpanInfo
wrapHake hakefile hake_exp =
  Paren loc (
    Lambda loc [PVar loc (Ident loc "sourceDB")] (
      Let loc (
        BDecls loc [
          FunBind loc [
             -- This is 'find'
            Match loc (Ident loc "find")
              [PVar loc (Ident loc "fn"), PVar loc (Ident loc "arg")]
              -- Nothing
              (UnGuardedRhs  loc
                  (Paren  loc (App loc  (App loc  (App loc  (Var loc  (UnQual loc  (Ident loc  "fn")))
                                        (Var loc  (UnQual loc  (Ident loc  "sourceDB"))))
                                   (Lit loc  (String loc hakefile "")))
                         (Var loc  (UnQual loc  (Ident loc  "arg"))))))
              (Just (BDecls loc  []))
          ],

          FunBind loc [
            Match loc
              (Ident loc "build") -- This is 'build'
              [PVar loc (Ident loc "a")]
              (UnGuardedRhs loc
                  (App loc (App loc (App loc (Paren loc (App loc (Var loc (UnQual loc (Ident loc "buildFunction")))
                                             (Var loc (UnQual loc (Ident loc "a")))))
                                 (Var loc (UnQual loc (Ident loc "sourceDB"))))
                            (Lit  loc(String loc hakefile "")))
                       (Var loc (UnQual loc (Ident loc "a")))))
              (Just (BDecls loc []))
          ]
        ]
      ) (Paren loc (App loc (Con loc (UnQual loc (Ident loc "Rules"))) hake_exp))
    )
  )
    where
        dummy_loc = SrcLoc { srcFilename = "<hake internal>",
                                srcLine = 0, srcColumn = 0 }
        loc = Language.Haskell.Exts.noSrcSpan
--
-- Makefile generation
--

-- The Makefile header, generated once.
makefilePreamble :: Handle -> Opts -> [String] -> IO ()
makefilePreamble h opts args =
    mapM_ (hPutStrLn h)
          ([ "# This Makefile is generated by Hake.  Do not edit!",
             "# ",
             "# Hake was invoked with the following command line args:" ] ++
           [ "#        " ++ a | a <- args ] ++
           [ "# ",
             "Q=@",
             "SRCDIR=" ++ opt_sourcedir opts,
             "HAKE_ARCHS=" ++ intercalate " " Config.architectures,
             -- Disable built-in implicit rules. GNU make adds environment's MAKEFLAGS too.
             "MAKEFLAGS=r",
             -- Explicitly disable the flex and bison implicit rules
             "%.c : %.y",
             "%.c : %.l",
             "INSTALL_PREFIX ?= /home/netos/tftpboot/$(USER)" ])

-- There a several valid top-level build directores, apart from the
-- architecture-specific one.
arch_list :: S.Set String
arch_list = S.fromList (Config.architectures ++
                        ["", "src", "hake", "root", "tools", "docs", "cache"])

-- A rule is included if it applies to only "special" and configured
-- architectures.
allowedArchs :: [String] -> Bool
allowedArchs = all (\a -> a `S.member` arch_list)


-- The section corresponding to a Hakefile.  These routines all collect
-- and directories they see.
makefileSectionArr :: Handle -> Opts -> [(FilePath,HRule)] -> IO (S.Set FilePath)
makefileSectionArr h opts xs = makefileSectionArr' S.empty xs
  where
    makefileSectionArr' :: (S.Set FilePath) -> [(FilePath,HRule)] ->
      IO (S.Set FilePath)
    makefileSectionArr' dirs [] = return dirs
    makefileSectionArr' dirs ((fp,rule) : xs) = do
      dirs' <- makefileSection h opts fp rule
      makefileSectionArr' (S.union dirs' dirs) xs

makefileSection :: Handle -> Opts -> FilePath -> HRule -> IO (S.Set FilePath)
makefileSection h opts hakepath rule = do
    hPutStrLn h $ "# From: " ++ hakepath ++ "\n"
    makefileRule h rule

makefileRule :: Handle -> HRule -> IO (S.Set FilePath)
makefileRule h (Error s) = do
    hPutStrLn h $ "$(error " ++ s ++ ")\n"
    return S.empty
makefileRule h (Rules rules) = do
    dir_lists <- mapM (makefileRule h) rules
    return $! S.unions dir_lists
makefileRule h (Include token) = do
    when (allowedArchs [frArch token]) $
        mapM_ (hPutStrLn h) [
            "ifeq ($(MAKECMDGOALS),clean)",
            "else ifeq ($(MAKECMDGOALS),rehake)",
            "else ifeq ($(MAKECMDGOALS),Makefile)",
            "else",
            "include " ++ (formatToken token),
            "endif",
            "" ]
    return S.empty
makefileRule h (HakeTypes.Rule tokens) =
    if allowedArchs (map frArch tokens)
        then makefileRuleInner h tokens False
        else return S.empty
makefileRule h (Phony name double_colon tokens) = do
    if allowedArchs (map frArch tokens)
        then do
            hPutStrLn h $ ".PHONY: " ++ name
            makefileRuleInner h (Target "build" name : tokens) double_colon
        else return S.empty

printTokens :: Handle -> S.Set RuleToken -> IO ()
printTokens h tokens =
    S.foldr (\t m -> hPutStr h (formatToken t) >> m) (return ()) tokens

printDirs :: Handle -> S.Set FilePath -> IO ()
printDirs h dirs =
    S.foldr (\d m -> hPutStr h (d ++ " ") >> m) (return ()) dirs



makefileRuleInner :: Handle -> [RuleToken] -> Bool -> IO (S.Set FilePath)
makefileRuleInner h tokens double_colon = do
    if S.null (ruleOutputs compiledRule)
    then do
        return $ ruleDirs compiledRule
    else do
        printTokens h $ ruleOutputs compiledRule
        if double_colon then hPutStr h ":: " else hPutStr h ": "
        printTokens h $ ruleDepends compiledRule
        hPutStr h " | directories "
        printTokens h $ rulePreDepends compiledRule
        hPutStrLn h ""
        doBody
    where
        compiledRule = compileRule tokens

        doBody :: IO (S.Set FilePath)
        doBody = do
            when (ruleBody compiledRule /= []) $ do
                hPutStr h "\t"
                mapM_ (hPutStr h . formatToken) $ ruleBody compiledRule
            hPutStrLn h "\n"
            return $ ruleDirs compiledRule

--
-- Functions to resolve path names in rules.
--
-- Absolute paths are interpreted relative to one of the three trees: source,
-- build or install.  Relative paths are interpreted relative to the directory
-- containing the Hakefile that referenced them, within one of the above tree.
-- Both build and install trees are divided by architecture, while the source
-- tree is not.  All paths are output relative to the build directory.
--
-- For example, if we are building for architecture 'x86_64', with build tree
-- '/home/user/barrelfish/build' and build tree '/home/user/barrelfish'
-- relative path '../', and we are compiling a Hakefile at 'apps/init/Hakefile'
-- relative path  '../apps/init/Hakefile', we would resolve as follows:
--
--   In SourceTree "../apps/init" "x86_64" "main.c"
--      -> "../apps/init/main.c"
--   In BuildTree "../apps/init" "x86_64" "/include/generated.h"
--      -> "./x86_64/include/generated.h"
--   Out BuildTree "../apps/init" "root" "/doc/manual.pdf"
--      -> "./doc/manual.pdf"
--
-- Note that the 'root' architecture is special, and always refers to the root
-- of the relevant tree.

-- Recurse through the Hake AST
resolvePaths :: Opts -> FilePath -> HRule -> HRule
resolvePaths o hakepath (Rules hrules)
    = Rules $ map (resolvePaths o hakepath) hrules
resolvePaths o hakepath (HakeTypes.Rule tokens)
    = HakeTypes.Rule $ map (resolveTokenPath o hakepath) tokens
resolvePaths o hakepath (Include token)
    = Include $ resolveTokenPath o hakepath token
resolvePaths o hakepath (Error s)
    = Error s
resolvePaths o hakepath (Phony name dbl tokens)
    = Phony name dbl $ map (resolveTokenPath o hakepath) tokens

-- Now resolve at the level of individual rule tokens.  At this level,
-- we need to take into account the tree (source, build, or install).
resolveTokenPath :: Opts -> FilePath -> RuleToken -> RuleToken
-- An input token specifies which tree it refers to.
resolveTokenPath o hakepath (In tree arch path) =
    (In tree arch (treePath o tree arch path hakepath))
-- An output token implicitly refers to the build tree.
resolveTokenPath o hakepath (Out arch path) =
    (Out arch (treePath o BuildTree arch path hakepath))
-- A dependency token specifies which tree it refers to.
resolveTokenPath o hakepath (Dep tree arch path) =
    (Dep tree arch (treePath o tree arch path hakepath))
-- A non-dependency token specifies which tree it refers to.
resolveTokenPath o hakepath (NoDep tree arch path) =
    (NoDep tree arch (treePath o tree arch path hakepath))
-- A pre-dependency token specifies which tree it refers to.
resolveTokenPath o hakepath (PreDep tree arch path) =
    (PreDep tree arch (treePath o tree arch path hakepath))
-- An target token implicitly refers to the build tree.
resolveTokenPath o hakepath (Target arch path) =
    (Target arch (treePath o BuildTree arch path hakepath))
-- A target token referring to an absolute resource
resolveTokenPath o hakepath (Abs rule rule2) =
    let o' = o {
            opt_sourcedir = opt_abs_sourcedir o,
            opt_installdir = opt_abs_installdir o,
            opt_builddir = opt_abs_builddir o,
            opt_bfsourcedir = opt_abs_bfsourcedir o
        }
    in Abs (resolveTokenPath o' hakepath rule) (resolveTokenPath o hakepath rule2)
-- Other tokens don't contain paths to resolve.
resolveTokenPath _ _ token = token

-- Now we get down to the nitty gritty.  We have, in order:
--   o:        The options in force
--   tree:     The tree (source, build, or install)
--   arch:     The architecture (e.g. armv7)
--   path:     The pathname we want to resolve
--   hakepath: The directory containing the Hakefile
-- If the tree is SrcTree or the architecture is "root", everything
-- is relative to the top-level directory for that tree.  Otherwise,
-- it's relative to the top-level directory plus the architecture.
treePath :: Opts -> TreeRef -> FilePath -> FilePath -> FilePath -> FilePath
-- The architecture 'root' is special.
treePath o SrcTree "root" path hakepath =
    relPath (opt_sourcedir o) path hakepath
treePath o BFSrcTree "root" path hakepath =
    relPath (opt_bfsourcedir o) path hakepath
treePath o BuildTree "root" path hakepath =
    relPath (opt_builddir o) path hakepath
treePath o InstallTree "root" path hakepath =
    relPath (opt_installdir o) path hakepath
-- The architecture 'cache' is special.
treePath o SrcTree "cache" path hakepath =
    relPath Config.cache_dir path hakepath
treePath o BFSrcTree "cache" path hakepath =
    relPath Config.cache_dir path hakepath
treePath o BuildTree "cache" path hakepath =
    relPath Config.cache_dir path hakepath
treePath o InstallTree "cache" path hakepath =
    relPath Config.cache_dir path hakepath
-- Source-tree paths don't get an architecture.
treePath o SrcTree arch path hakepath =
    relPath (opt_sourcedir o) path hakepath
treePath o BFSrcTree arch path hakepath =
    relPath (opt_bfsourcedir o) path hakepath
treePath o BuildTree arch path hakepath =
    relPath ((opt_builddir o) </> arch) path hakepath
treePath o InstallTree arch path hakepath =
    relPath (opt_installdir o </> arch) path hakepath

-- First evaluate the given path 'path', relative to the Hakefile directory
-- 'hakepath'.  If 'path' is absolute (i.e. begins with a /), it is unchanged.
-- Otherwise it is appended to 'hakepath'.  We then treat this as a relative
-- path (by removing any initial /), and append it to the relevant tree root
-- (which may or may not have an architecture path appended already).
relPath :: String -> String -> String -> String
-- The first rule prevents a path of / to be reduced to the empty string
relPath "." "/" hakepath =
    "."
relPath "." path hakepath =
    stripSlash (hakepath </> path)
relPath treeroot path hakepath =
    treeroot </> stripSlash (hakepath </> path)

-- Strip any leading slash from the filename.  This is much faster than
-- 'makeRelative "/"'.
stripSlash :: FilePath -> FilePath
stripSlash ('/':cs) = cs
stripSlash cs = cs

-- Emit the rule to rebuild the Hakefile.
makeHakeDeps :: Handle -> Opts -> [String] -> IO ()
makeHakeDeps h o l = do
    hPutStrLn h "ifneq ($(MAKECMDGOALS),rehake)"
    makefileRule h rule
    hPutStrLn h "endif"
    hPutStrLn h ".DELETE_ON_ERROR:\n" -- this applies to following targets.
    where
        hake = resolveTokenPath o "" (In InstallTree "root" "/hake/hake")
        makefile = resolveTokenPath o "/" (Out "root" (opt_makefilename o))
        rule = HakeTypes.Rule
                    ( [ hake,
                        Str "--source-dir", Str (opt_sourcedir o),
                        Str "--install-dir", Str (opt_installdir o),
                        Str "--bfsource-dir", Str (opt_bfsourcedir o),
                        Str "--output-filename", makefile,
                        Str "--ghc-libdir", Str (opt_ghc_libdir o)
                      ] ++
                      [ Dep SrcTree "root" h | h <- l ]
                    )

-- Emit the rules to create the build directories
makeDirectories :: Handle -> S.Set FilePath -> IO ()
makeDirectories h dirs = do
    hPutStrLn h "# Directories follow"
    hPutStrLn h "DIRECTORIES=\\"
    mapM_ (\d -> hPutStrLn h $ "    " ++ d ++ " \\") (S.toList dirs)
    hPutStrLn h "\n"
    hPutStrLn h ".PHONY: directories"
    hPutStr h "directories: $(DIRECTORIES)"
    hPutStrLn h ""
    hPutStrLn h "%.marker:"
    hPutStrLn h "\t$(Q)echo \"MKDIR $@\""
    hPutStrLn h "\t$(Q)mkdir -p `dirname $@`"
    hPutStrLn h "\t$(Q)touch $@"

makeDriverDomainDb :: String -> LibDepTree2 -> IO()
makeDriverDomainDb build t = do
  let fileName = build ++ "/sockeyefacts/ddomain_db.pl"
  let dirName = build ++ "/sockeyefacts"
  createDirectoryIfMissing True dirName
  writeFile fileName ""
  h <- openFile(fileName) WriteMode
  mapM_ (hPutStrLn h . pairToPl) (ldtDriverModules t)
  hFlush h
  hClose h
  return ()
  where
    pairToPl :: (DepEl, DepEl) -> String
    pairToPl (a,b) = "drivermodule(" ++ toPl a ++ "," ++ toPl b ++ ")."
    toPl :: DepEl -> String
    toPl x = "(\"" ++ depElArch x ++ "\",\"" ++ depElName x ++ "\")"


--
-- The top level
--

extractrule :: FilePath -> HRule -> Ghc (HRule)
extractrule fp hr = return hr

extractDep :: FilePath -> HRule -> Ghc (DepElMap)
extractDep fp hr = return $ ldtHRuleToDepElMap Config.architectures hr

writeMF :: Handle -> Opts -> (HRule -> HRule) -> FilePath -> HRule -> Ghc (S.Set FilePath)
writeMF h o rule_transform fp rule = liftIO $ makefileSection h o fp (rule_transform rule)

body :: IO ()
body =  do
    -- Parse arguments; architectures default to config file
    args <- System.Environment.getArgs
    let o1 = parse_arguments args
        al = if opt_architectures o1 == []
             then Config.architectures
             else opt_architectures o1
        opts' = o1 { opt_architectures = al }

    when (opt_usage_error opts') $
        throw (HakeError usage 1)

    -- Check configuration settings.
    -- This is currently known at compile time, but might not always be!
    when (isJust configErrors) $
        throw (HakeError ("Error in configuration: " ++
                         (fromJust configErrors)) 2)

    -- Canonicalise directories
    abs_sourcedir   <- canonicalizePath $ opt_sourcedir opts'
    abs_bfsourcedir <- canonicalizePath $ opt_bfsourcedir opts'
    abs_installdir  <- canonicalizePath $ opt_installdir opts'
    abs_builddir    <- canonicalizePath $ "."
    let opts = opts' { opt_abs_sourcedir   = abs_sourcedir,
                       opt_abs_bfsourcedir = abs_bfsourcedir,
                       opt_abs_installdir  = abs_installdir,
                       opt_abs_builddir    = abs_builddir }

    putStrLn ("Source directory: " ++ opt_sourcedir opts ++
                       " (" ++ opt_abs_sourcedir opts ++ ")")
    putStrLn ("BF Source directory: " ++ opt_bfsourcedir opts ++
                       " (" ++ opt_abs_bfsourcedir opts ++ ")")
    putStrLn ("Install directory: " ++ opt_installdir opts ++
                       " (" ++ opt_abs_installdir opts ++ ")")
    putStrLn ("GHC libdir: " ++ opt_ghc_libdir opts)

    -- Find Hakefiles
    putStrLn "Scanning directory tree..."
    (relfiles, hakefiles) <- listFiles (opt_sourcedir opts)
    let srcDB = tdbBuild relfiles

    -- Open the Makefile and write the preamble
    putStrLn $ "Creating " ++ (opt_makefilename opts) ++ "..."
    makefile <- openFile(opt_makefilename opts) WriteMode
    makefilePreamble makefile opts args
    makeHakeDeps makefile opts $ map fst hakefiles

    -- Evaluate Hakefiles
    putStrLn $ "Evaluating " ++ show (length hakefiles) ++ " Hakefiles for dependencies..."
    depElMap <- evalHakeFiles (opt_ghc_libdir opts) opts srcDB hakefiles extractDep

    let dep_graph = ldtEmToGraph (foldr ldtDepElMerge Map.empty depElMap)
    let rtrans = ldtRuleExpand $ dep_graph
    putStrLn $ "Evaluating " ++ show (length hakefiles) ++ " Hakefiles..."

    dirs_a <- evalHakeFiles (opt_ghc_libdir opts) opts srcDB hakefiles (writeMF makefile opts rtrans)

    let dirs = foldr S.union S.empty dirs_a

    putStrLn "Generating build directory dependencies..."
    makeDirectories makefile dirs

    makeDriverDomainDb abs_builddir dep_graph

    hFlush makefile
    hClose makefile
    return ()

main :: IO ()
main = do
    r <- body `catch` handleHakeError
    exitWith ExitSuccess
    where
        handleHakeError :: HakeError -> IO ()
        handleHakeError (HakeError str n) = do
            putStrLn str
            exitWith $ ExitFailure n
