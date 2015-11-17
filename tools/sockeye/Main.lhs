%include polycode.fmt

%if false
  Flounder2: an even more simpler IDL for Barrelfish
   
  Copyright (c) 2009, 2010 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif



> module Main where

> import System.Environment
> import System.Exit
> import System.Console.GetOpt
> import System.IO
> import System.FilePath.Posix
> import Data.Maybe
> import Control.Monad

> import Text.ParserCombinators.Parsec as Parsec
> import qualified SockeyeParser
> import qualified SockeyeSyntax
> import qualified SockeyeCodeBackend
> import qualified SockeyeHeaderBackend
> import qualified SockeyeDocBackend
> import qualified SockeyeTools


> data Target = Header
>             | Code
>             | Documentation
>             deriving (Show)

> data Options = Options {
>     optTargets :: [Target],
>     optIncludes :: [String]
> }

> defaultOptions :: Options
> defaultOptions = Options { optTargets = [],  optIncludes = [] }

> generator :: Options -> Target -> String -> String -> SockeyeSyntax.Schema -> String
> generator _ Header = SockeyeHeaderBackend.compile
> generator _ Code = SockeyeCodeBackend.compile
> generator _ Documentation = SockeyeDocBackend.compile
>
>
> addTarget :: Target -> Options -> IO Options
> addTarget t o = return o { optTargets = (optTargets o) ++ [t] }



> addInclude :: String -> Options -> IO Options
> addInclude s o = return o { optIncludes = (optIncludes o) ++ [s] }

> options :: [OptDescr (Options -> IO Options)]
> options = [ 
>             Option ['H'] ["generic-header"] (NoArg $ addTarget Header) "Create a header file",
>             Option ['C'] ["generic-stub"] (NoArg $ addTarget Code) "Create code",
>             Option ['i'] ["import"] (ReqArg addInclude "FILE")      "Include a given file before processing", 
>             Option ['D'] ["documentation"] (NoArg $ addTarget Documentation)      "add documentation target"
>            ]



> compile :: Options -> Target -> SockeyeSyntax.Schema -> String -> String -> Handle -> IO ()
> compile opts fl ast infile outfile outfiled =
>     hPutStr outfiled $ (generator opts fl) infile outfile ast'
>   where
>       ast' = SockeyeTools.rewireTypes ast (SockeyeTools.collectTypes ast)

> parseFile :: (String -> IO (Either Parsec.ParseError a)) -> String -> IO a
> parseFile parsefn fname = do
>    input <- parsefn fname
>    case input of
>        Left err -> do
>            hPutStrLn stderr $ "Parse error at: " ++ (show err)
>            exitWith $ ExitFailure 1
>        Right x -> return x

> parseIncludes :: Options -> IO [(String, SockeyeSyntax.Declaration)]
> parseIncludes opts
>     = foldM (\d -> parseFile $ SockeyeParser.parse_include d) [] (optIncludes opts)

> checkFilename :: SockeyeSyntax.Schema -> String -> IO ()
> checkFilename schema fname = do
>                                 let SockeyeSyntax.Schema sname _ _ = schema
>                                 if sname == takeBaseName fname then return () else ioError $ userError ("Schema name name '" ++ sname ++ "' has to equal filename in " ++ fname)

> main :: IO ()
> main = do 
>        argv <- System.Environment.getArgs
>        case getOpt RequireOrder options argv of
>          (optf, [ inFile, outFile ], []) -> do
>              opts <- foldM (flip id) defaultOptions optf
>              includeDecls <- parseIncludes opts
>              ast <- parseFile (SockeyeParser.parse_intf includeDecls (takeBaseName inFile)) inFile
>              outFileD <- openFile outFile WriteMode
>              checkFilename ast inFile
>              sequence_ $ map (\target
>                               -> compile opts target ast inFile outFile outFileD)
>                            (optTargets opts)
>              hClose outFileD
>          (_, _, errors) -> do
>              hPutStr stderr (concat errors ++ usageInfo usage options)
>              exitWith (ExitFailure 1)
>       where
>           usage = "Usage: sockeye [OPTION...] input.fact output"
