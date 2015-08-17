import Control.Monad.Error

import Data.Dynamic
import Data.List
import Data.Maybe
import qualified Data.Set as S

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Mem

import GHC hiding (Target, Ghc, GhcT, runGhc, runGhcT, FunBind, Match)
import GHC.Paths (libdir)
import Control.Monad.Ghc
import DynFlags (defaultFatalMessager, defaultFlushOut,
                 xopt_set, ExtensionFlag (Opt_DeriveDataTypeable))
import GHC.Stats

import Language.Haskell.Exts

import RuleDefs
import HakeTypes
import qualified Args
import qualified Config
import qualified Path

data HakeError = HakeError String Int
instance Error HakeError
type HakeMonad = ErrorT HakeError IO

--
-- Command line options and parsing code
--
data Opts = Opts { opt_makefilename :: String,
                   opt_installdir :: String,
                   opt_sourcedir :: String,
                   opt_bfsourcedir :: String,
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
         opt_usage_error = False, 
         opt_architectures = [],
         opt_verbosity = 1 }
parse_arguments ("--install-dir" : (s : t)) =
  (parse_arguments t) { opt_installdir = s }
parse_arguments ("--source-dir" : s : t) =  
  (parse_arguments t) { opt_sourcedir = s }
parse_arguments ("--bfsource-dir" : s : t) =  
  (parse_arguments t) { opt_bfsourcedir = s }
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
                  "   --quiet",
                  "   --verbose"
                ]

-- check the configuration options, returning an error string if they're insane
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

--
-- Walk all over a directory tree and build a complete list of pathnames
--
listFiles :: FilePath -> IO ([FilePath], [(FilePath, String)])
listFiles root = do
    isdir <- doesDirectoryExist root
    if isdir then do
        children <- getDirectoryContents root
        walkchildren children
    else
        return ([], [])
    where
        walkchildren :: [FilePath] -> IO ([FilePath], [(FilePath, String)])
        walkchildren [] = return ([], [])
        walkchildren (child:siblings) = do
            (allfiles, hakefiles) <- walkchild child
            (allfilesS, hakefilesS) <- walkchildren siblings
            return $ (allfiles ++ allfilesS, hakefiles ++ hakefilesS)

        walkchild :: FilePath -> IO ([FilePath], [(FilePath, String)])
        walkchild child = do
            if ignore child
            then return ([], [])
            else do
                (allfiles, hakefiles) <- listFiles (root </> child)
                hake <- maybeHake child
                return $ ((root </> child) : allfiles,
                          hake ++ hakefiles)
            where
                maybeHake "Hakefile" = do
                    contents <- readFile (root </> child)
                    return [(root </> child, contents)]
                maybeHake _ = return []

        ignore :: FilePath -> Bool
        ignore "."          = True
        ignore ".."         = True
        ignore "CMakeFiles" = True
        ignore ".hg"        = True
        ignore "build"      = True
        ignore ".git"       = True
        ignore _            = False

instance Show SuccessFlag
instance Show RunResult

driveGhc :: Opts -> [(FilePath, String)] -> Ghc ([(String, [String] -> HRule)])
driveGhc o hakefiles = do
    -- Set the RTS flags
    dflags <- getSessionDynFlags
    let dflags' = foldl xopt_set dflags [ Opt_DeriveDataTypeable ]
    _ <- setSessionDynFlags dflags'{
        importPaths = module_paths,
        hiDir = Just "./hake",
        objectDir = Just "./hake"
    }

    -- Set compilation targets
    targets <- mapM (\m -> guessTarget m Nothing) source_modules
    setTargets targets
    load LoadAllTargets

    -- Import modules
    setContext
        ([IIDecl $ simpleImportDecl $ mkModuleName m |
            m <- modules] ++
         [IIDecl $ (simpleImportDecl $ mkModuleName m) {
                ideclQualified = True
          } | m <- qualified_modules])

    mapM evaluate hakefiles

    where
        module_paths = [ (opt_installdir o) </> "hake", ".", 
                         (opt_bfsourcedir o) </> "hake" ]
        source_modules = [ "HakeTypes", "RuleDefs", "Path", "Args", "Config" ]
        modules = [ "Prelude", "HakeTypes", "RuleDefs", "Path", "Args" ]
        qualified_modules = [ "Config", "Data.List" ]

        evaluate :: (FilePath, String) -> Ghc ((String, [String] -> HRule))
        evaluate (hake_name, hake_raw) = do
            case hake_parse of
                Left hake_expr -> do
                    let hake_wrapped =
                            prettyPrintWithMode (defaultMode {layout = PPNoLayout}) $
                                wrapHake hake_name hake_expr

                    val <- dynCompileExpr $ hake_wrapped ++ " :: [String] -> HRule"
                    liftIO $ putStrLn ( "Success: " ++ hake_name )
                    return $ (hake_name, fromDyn val (\_ -> Error "failed"))
                Right hake_error -> do
                    return $ (hake_name, \_ -> Error "failed")
            where
                hake_parse = parseHake (hake_name, hake_raw)

evalHakeFiles :: Opts -> [(FilePath, String)] ->
                 IO ([(String, [String] -> HRule)])
evalHakeFiles o hakefiles =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
        runGhc (Just libdir) $
        driveGhc o hakefiles

parseHake :: (FilePath, String) -> Either Exp HakeError
parseHake (filename, contents) =
    case result of
        ParseOk e -> Left e
        ParseFailed loc str ->
            Right $ HakeError (show loc ++ ": " ++ str) 2
    where
        result =
            parseExpWithMode
                (defaultParseMode {
                    parseFilename = filename,
                    baseLanguage = Haskell2010 })
                contents

wrapHake :: FilePath -> Exp -> Exp
wrapHake hakefile hake_exp =
    Paren (
    Lambda dummy_loc [PVar (Ident "allfiles")] (
    Let (BDecls
        [FunBind [Match
            dummy_loc
            (Ident "find")
            [PVar (Ident "fn"), PVar (Ident "arg")]
            Nothing
            (UnGuardedRhs
                (Paren (App (App (App (Var (UnQual (Ident "fn")))
                                      (Var (UnQual (Ident "allfiles"))))
                                 (Lit (String hakefile)))
                       (Var (UnQual (Ident "arg"))))))
            (BDecls [])],

        FunBind [Match
            dummy_loc
            (Ident "build")
            [PVar (Ident "a")]
            Nothing
            (UnGuardedRhs
                (App (App (App (Paren (App (Var (UnQual (Ident "buildFunction")))
                                           (Var (UnQual (Ident "a")))))
                               (Var (UnQual (Ident "allfiles"))))
                          (Lit (String hakefile)))
                     (Var (UnQual (Ident "a")))))
            (BDecls [])]
        ])
        (Paren (App (Con (UnQual (Ident "Rules")))
                    hake_exp))
    ))
    where
        dummy_loc = SrcLoc { srcFilename = "<hake internal>",
                                srcLine = 0, srcColumn = 0 }

makefilePreamble :: Handle -> Opts -> [String] -> IO ()
makefilePreamble h opts args = 
    mapM_ (hPutStrLn h)
          ([ "# This Makefile is generated by Hake.  Do not edit!",
             "# ",
             "# Hake was invoked with the following command line args:" ] ++
           [ "#        " ++ a | a <- args ] ++
           [ "# ",
             "SRCDIR=" ++ (opt_sourcedir opts),
             "HAKE_ARCHS=" ++ (concat $ intersperse " " Config.architectures),
             "include ./symbolic_targets.mk" ])

-- a rule is included if it has only "special" architectures and enabled architectures
allowedArchs :: [String] -> Bool
allowedArchs = all (\a -> a `elem` (Config.architectures ++ specialArchitectures))
    where specialArchitectures = ["", "src", "hake", "root", "tools", "docs"]

makefileSection :: Handle -> Opts -> [FilePath] ->
                   (String, [String] -> HRule) -> IO (S.Set FilePath)
makefileSection h opts allfiles (hake_name, rule_schema) = do
    hPutStrLn h $ "# From: " ++ hake_name ++ "\n"
    makefileRule h $
        resolveRelativePaths opts (rule_schema allfiles) hake_name

makefileRule :: Handle -> HRule -> IO (S.Set FilePath)
makefileRule h (Error s) = do
    hPutStrLn h $ "$(error " ++ s ++ ")\n"
    return S.empty
makefileRule h (Rules rules) = do
    dir_lists <- mapM (makefileRule h) rules
    return $ S.unions dir_lists
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
    hPutStrLn h $ ".PHONY: " ++ name
    makefileRuleInner h (Target "build" name : tokens) double_colon

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
        hPutStr h "# hake: omitted rule with no output: "
        doBody
    else do
        printTokens h $ ruleOutputs compiledRule
        if double_colon then hPutStr h ":: " else hPutStr h ": "
        printTokens h $ ruleDepends compiledRule
        printDirs h $ ruleDirs compiledRule
        when (not (S.null (rulePreDepends compiledRule))) $ do
            hPutStr h " | "
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

---
--- Functions to resolve relative path names in rules. 
---
--- First, the outer function: resolve path names in an HRule. The
--- third argument, 'root', is frequently the pathname of the Hakefile
--- relative to the source tree - since relative pathnames in
--- Hakefiles are interpreted relative to the Hakefile location.
---
resolveRelativePaths :: Opts -> HRule -> String -> HRule
resolveRelativePaths o (Rules hrules) root 
    = Rules [ resolveRelativePaths o r root | r <- hrules ]
resolveRelativePaths o (HakeTypes.Rule tokens) root
    = HakeTypes.Rule [ resolveRelativePath o t root | t <- tokens ]
resolveRelativePaths o (Include token) root
    = Include ( resolveRelativePath o token root )
resolveRelativePaths o (Error s) root 
    = Error s
resolveRelativePaths o (Phony name dbl tokens) root 
    = Phony name dbl [ resolveRelativePath o t root | t <- tokens ]

--- Now resolve at the level of individual rule tokens.  At this
--- level, we need to take into account the tree (source, build, or
--- install).
resolveRelativePath :: Opts -> RuleToken -> String -> RuleToken
resolveRelativePath o (In t a f) root = 
    (In t a (resolveRelativePathName o t a f root))
resolveRelativePath o (Out a f) root = 
    (Out a (resolveRelativePathName o BuildTree a f root))
resolveRelativePath o (Dep t a f) root = 
    (Dep t a (resolveRelativePathName o t a f root))
resolveRelativePath o (NoDep t a f) root = 
    (NoDep t a (resolveRelativePathName o t a f root))
resolveRelativePath o (PreDep t a f) root = 
    (PreDep t a (resolveRelativePathName o t a f root))
resolveRelativePath o (Target a f) root = 
    (Target a (resolveRelativePathName o BuildTree a f root))
resolveRelativePath _ (Str s) _ = (Str s)
resolveRelativePath _ (NStr s) _ = (NStr s)
resolveRelativePath _ (ErrorMsg s) _ = (ErrorMsg s)
resolveRelativePath _ NL _ = NL

--- Now we get down to the nitty gritty.  We have, in order:
---   o: The options in force.
---   t: The tree (source, build, or install)
---   a: The architecture (e.g. armv7)
---   p: The pathname we want to resolve to a full path, and
---   h: The dirname of the Hakefile in which it occurs.
--- If the tree is SrcTree or the architecture is "root", everything
--- is relative to the top-level directory for that tree.  Otherwise,
--- it's relative to the top-level directory plus the architecture.
resolveRelativePathName :: Opts -> TreeRef -> String -> String -> String -> String

resolveRelativePathName o SrcTree "root" f h = 
    resolveRelativePathName' ((opt_sourcedir o)) f h
resolveRelativePathName o BuildTree "root" f h = 
    resolveRelativePathName' "." f h
resolveRelativePathName o InstallTree "root" f h = 
    resolveRelativePathName' ((opt_installdir o)) f h

resolveRelativePathName o SrcTree a f h =
    resolveRelativePathName' (opt_sourcedir o) f h
resolveRelativePathName o BuildTree a f h =
    resolveRelativePathName' ("." </> a) f h
resolveRelativePathName o InstallTree a f h =
    resolveRelativePathName' ((opt_installdir o) </> a) f h

--- This is where the work is done: take 'hd' (pathname relative to
--- us of the Hakefile) and resolve the filename we're interested in
--- relative to this.  This gives us a pathname relative to some root
--- of some architecture tree, then return this relative to the actual
--- tree we're interested in.  It's troubling that this takes more
--- bytes to explain than to code.
---   d:    Pathname of top directory of the tree (source, build, install)
---   f:    Filename we are interested in, relative to 'root' below
---   hd:   Directory containing the Hakefile
---   
resolveRelativePathName' d f hd = 
    let af = Path.relToFile f hd
        rf = Path.makeRel $ Path.relToDir af "/" 
    in Path.relToDir rf d

makeHakeDeps :: Handle -> Opts -> [String] -> IO ()
makeHakeDeps h o l = do
    makefileRule h rule
    hPutStrLn h ".DELETE_ON_ERROR:\n" -- this applies to all targets in the Makefile
    where
        hake = resolveRelativePath o (In InstallTree "root" "/hake/hake") ""
        makefile = resolveRelativePath o (Out "root" (opt_makefilename o)) "/Hakefile"
        rule = HakeTypes.Rule
                    ( [ hake, 
                        Str "--source-dir", Str (opt_sourcedir o),
                        Str "--install-dir", Str (opt_installdir o),
                        Str "--output-filename", makefile
                      ] ++
                      [ Dep SrcTree "root" h | h <- l ]
                    )

makeDirectories :: Handle -> S.Set FilePath -> IO ()
makeDirectories h dirs = do
    hPutStrLn h "# Directories follow"
    mapM_ (makeDir h) (S.toList (S.delete ("." </> ".marker") dirs))

makeDir :: Handle -> FilePath -> IO ()
makeDir h dir = do
    hPutStrLn h $ "hake_dirs: " ++ dir ++ "\n"
    hPutStrLn h $ dir ++ ":"
    hPutStrLn h $ "\tmkdir -p " ++ (takeDirectory dir)
    hPutStrLn h $ "\ttouch " ++ dir
    hPutStrLn h ""

scanTokens :: [RuleToken] -> (S.Set RuleToken, S.Set RuleToken,
                              S.Set RuleToken, [RuleToken],
                              S.Set FilePath)
scanTokens [] = (S.empty, S.empty, S.empty, [], S.empty)
scanTokens (t:ts) =
    case t of
        Out _ f      -> (S.insert t outs, deps, predeps, body',
                         S.insert (dirOf f) dirs)
        Target _ f   -> (S.insert t outs, deps, predeps, body',
                         S.insert (dirOf f) dirs)
        In _ _ _     -> (outs, S.insert t deps, predeps, body', dirs)
        Dep _ _ _    -> (outs, S.insert t deps, predeps, body', dirs)
        PreDep _ _ _ -> (outs, deps, S.insert t predeps, body', dirs)
        _            -> (outs, deps, predeps, body', dirs)
    where
        (outs, deps, predeps, body, dirs) = scanTokens ts
        body' = if inRule t then t:body else body

        dirOf :: FilePath -> FilePath
        dirOf f = (takeDirectory f) </> ".marker"

data CompiledRule =
    CompiledRule {
        ruleOutputs    :: S.Set RuleToken,
        ruleDepends    :: S.Set RuleToken,
        rulePreDepends :: S.Set RuleToken,
        ruleBody       :: [RuleToken],
        ruleDirs       :: S.Set FilePath
    }

compileRule :: [RuleToken] -> CompiledRule
compileRule tokens
    = CompiledRule {
        ruleOutputs    = outs,
        ruleDepends    = deps,
        rulePreDepends = predeps,
        ruleBody       = body,
        ruleDirs       = dirs
        }
    where
        (outs, deps, predeps, body, dirs) = scanTokens tokens

gcStats :: IO ()
gcStats = do
    performGC
    gc_stats <- getGCStats
    putStrLn $ show (currentBytesUsed gc_stats) ++ " - " ++
               show (numGcs gc_stats) ++ " - " ++
               show (maxBytesUsed gc_stats) ++ " - " ++
               show (wallSeconds gc_stats)

body :: HakeMonad ()
body =  do
    -- parse arguments; architectures default to config file
    args <- liftIO $ System.Environment.getArgs
    let o1 = parse_arguments args
        al = if opt_architectures o1 == [] 
             then Config.architectures 
             else opt_architectures o1
        opts = o1 { opt_architectures = al }

    when (opt_usage_error opts) $
        throwError (HakeError usage 1)

    -- sanity-check configuration settings
    -- this is currently known at compile time, but might not always be!
    when (isJust configErrors) $
        throwError (HakeError ("Error in configuration: " ++
                               (fromJust configErrors)) 2)

    liftIO $ putStrLn ("Source directory: " ++ opt_sourcedir opts)
    liftIO $ putStrLn ("BF Source directory: " ++ opt_bfsourcedir opts)
    liftIO $ putStrLn ("Install directory: " ++ opt_installdir opts)

    liftIO gcStats

    liftIO $ putStrLn "Reading directory tree..."
    (allfiles, hakefiles) <- liftIO $ listFiles (opt_sourcedir opts)

    liftIO gcStats

    rules <- liftIO $ evalHakeFiles opts hakefiles
    liftIO $ putStrLn $ show (length rules)

    liftIO gcStats

    liftIO $ putStrLn $ "Generating " ++ (opt_makefilename opts)
    makefile <- liftIO $ openFile(opt_makefilename opts) WriteMode
    liftIO $ makefilePreamble makefile opts args
    liftIO $ makeHakeDeps makefile opts $ map fst hakefiles
    dirs <- liftIO $ mapM (makefileSection makefile opts allfiles) rules
    liftIO $ makeDirectories makefile (S.unions dirs)

    liftIO gcStats

    return ()

main :: IO () 
main = do
    r <- runErrorT $ body `catchError` handleFailure
    exitWith ExitSuccess
    where
        handleFailure (HakeError str n) = do
            liftIO $ putStrLn str
            liftIO $ exitWith (ExitFailure n)
