{-
  SockeyeParser.hs: Parser for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeParser
( parseSockeye ) where

import Control.Monad

import Data.Maybe (fromMaybe)

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)

import qualified SockeyeASTFrontend as AST

{- Setup the lexer -}
lexer = P.makeTokenParser (
    javaStyle  {
        {- list of reserved Names -}
        P.reservedNames = keywords,

        {- valid identifiers -}
        P.identStart = letter,
        P.identLetter = identLetter,

        {- comment start and end -}
        P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "//",
        P.nestedComments = False
    })

{- Parser main function -}
parseSockeye :: String -> String -> Either ParseError AST.SockeyeSpec
parseSockeye = parse sockeyeFile

{- Sockeye parsing -}
sockeyeFile = do
    whiteSpace
    spec <- sockeyeSpec
    eof
    return spec

sockeyeSpec = do
    modules <- many sockeyeModule
    net <- many netSpec
    return AST.SockeyeSpec
        { AST.modules = modules
        , AST.net     = net
        }

sockeyeModule = do
    reserved "module"
    name <- moduleName
    params <- option [] $ parens (commaSep moduleParam)
    body <- braces moduleBody
    return AST.Module
        { AST.name       = name
        , AST.parameters = params
        , AST.moduleBody = body
        }

moduleParam = do
    paramType <- choice [intType, addrType] <?> "parameter type"
    paramName <- parameterName
    return AST.ModuleParam
        { AST.paramName = paramName
        , AST.paramType = paramType 
        }
    where
        intType = do
            symbol "int"
            return AST.NumberParam
        addrType = do
            symbol "addr" 
            return AST.AddressParam

moduleBody = do
    ports <- many $ portDef
    net <- many netSpec
    return AST.ModuleBody
        { AST.ports     = concat ports
        , AST.moduleNet = net
        }

portDef = choice [inputPorts, outputPorts]
    where
        inputPorts = do
            reserved "input"
            ports <- commaSep1 identifierFor
            return $ map toInDef ports
        toInDef (forFn, iden) =
            let
                portDef = AST.InputPortDef iden
            in case forFn of
                Nothing -> portDef
                Just f  -> AST.MultiPortDef $ f portDef
        outputPorts = do
            reserved "output"
            ports <- commaSep1 identifierFor
            return $ map toOutDef ports
        toOutDef (forFn, iden) =
            let
                portDef = AST.OutputPortDef iden
            in case forFn of
            Nothing -> portDef
            Just f  -> AST.MultiPortDef $ f portDef

netSpec = choice [ inst <?> "module instantiation"
                 , decl <?> "node declaration"
                 , multiSpecs
                 ]
    where
        inst = do
            moduleInst <- moduleInst
            return $ AST.ModuleInstSpec moduleInst
        decl = do
            nodeDecl <- nodeDecl
            return $ AST.NodeDeclSpec nodeDecl
        multiSpecs = do
            for <- for $ many1 netSpec
            return $ AST.MultiNetSpec for

moduleInst = do
    (name, args) <- try $ do
        name <- moduleName
        args <- option [] $ parens (commaSep moduleArg)
        symbol "as"
        return (name, args)
    nameSpace <- identifier
    portMappings <- option [] $ symbol "with" *> many1 portMapping
    return AST.ModuleInst
        { AST.moduleName = name
        , AST.nameSpace  = nameSpace
        , AST.arguments  = args 
        , AST.portMappings = portMappings
        }

moduleArg = choice [addressArg, numberArg, paramArg]
    where
        addressArg = do
            addr <- addressLiteral
            return $ AST.AddressArg (fromIntegral addr)
        numberArg = do
            num <- numberLiteral
            return $ AST.NumberArg (fromIntegral num)
        paramArg = do
            name <- parameterName
            return $ AST.ParamArg name

portMapping = choice [inputMapping, outputMapping, multiMapping]
    where
        inputMapping = do
            nodeId <- try $ identifier <* symbol ">"
            port <- identifier
            return AST.InputPortMap
                { AST.port   = port
                , AST.nodeId = nodeId
                }
        outputMapping = do
            nodeId <- try $ identifier <* symbol "<"
            port <- identifier
            return AST.OutputPortMap
                { AST.port   = port
                , AST.nodeId = nodeId
                }
        multiMapping = do
            for <- for $ many1 portMapping
            return $ AST.MultiPortMap for

nodeDecl = do
    nodeIds <- choice [try single, try multiple]
    nodeSpec <- nodeSpec
    return $ AST.NodeDecl
        { AST.nodeIds = nodeIds
        , AST.nodeSpec = nodeSpec
        }
    where single = do
            nodeId <- identifier
            reserved "is"
            return [nodeId]
          multiple = do
            nodeIds <- commaSep1 identifier
            reserved "are"
            return nodeIds

identifier = choice [template identifierName, simple identifierName] <* whiteSpace
    where
        simple ident = do
            name <- ident
            return $ AST.SimpleIdent name
        template ident = do
            prefix <- try $ ident <* char '{'
            varName <- whiteSpace *> variableName <* char '}'
            suffix <- optionMaybe $ choice [ template $ many identLetter
                                           , simple $ many1 identLetter
                                           ]
            return AST.TemplateIdent
                { AST.prefix  = prefix
                , AST.varName = varName
                , AST.suffix   = suffix
                }

nodeSpec = do
    nodeType <- optionMaybe $ try nodeType
    accept <- option [] accept 
    translate <- option [] tranlsate 
    overlay <- optionMaybe overlay
    return AST.NodeSpec 
        { AST.nodeType  = nodeType
        , AST.accept    = accept
        , AST.translate = translate
        , AST.overlay   = overlay
        }
    where
        accept = do
            reserved "accept"
            brackets $ many blockSpec
        tranlsate = do
            reserved "map"
            brackets $ many mapSpec
        overlay = do
            reserved "over"
            identifier

nodeType = choice [memory, device]
    where memory = do
            symbol "memory"
            return AST.Memory
          device = do
            symbol "device"
            return AST.Device

blockSpec = choice [range, length, singleton]
    where
        singleton = do
            address <- address
            return $ AST.Singleton address
        range = do
            base <- try $ address <* symbol "-"
            limit <- address
            return $ AST.Range base limit
        length = do
            base <- try $ address <* symbol "/"
            bits <- decimal <?> "number of bits"
            return $ AST.Length base (fromIntegral bits)

address = choice [address, param]
    where
        address = do
            addr <- addressLiteral
            return $ AST.NumberAddress (fromIntegral addr)
        param = do
            name <- parameterName
            return $ AST.ParamAddress name

mapSpec = do
    block <- blockSpec
    reserved "to"
    dests <- commaSep1 $ mapDest
    return $ AST.MapSpec block dests

mapDest = choice [baseAddress, direct]
    where
        direct = do
            destNode <- identifier
            return $ AST.Direct destNode
        baseAddress = do
            destNode <- try $ identifier <* reserved "at"
            destBase <- address
            return $ AST.BaseAddress destNode destBase

for body = do
    reserved "for"
    varRanges <- commaSep1 $ forVarRange False
    body <- braces body
    return AST.For
        { AST.varRanges = varRanges
        , AST.body      = body
        }

identifierFor = do
    (varRanges, Just ident) <- choice [template identifierName, simple identifierName] <* whiteSpace
    let
        forFn = case varRanges of
         [] -> Nothing
         _  -> Just $ \body -> AST.For
                { AST.varRanges = varRanges
                , AST.body      = body
                }
    return (forFn, ident)
    where
        simple ident = do
            name <- ident
            return $ ([], Just $ AST.SimpleIdent name)
        template ident = do
            prefix <- try $ ident <* symbol "{"
            optVarRange <- forVarRange True
            char '}'
            (subRanges, suffix) <- option ([], Nothing) $ choice
                [ template $ many identLetter
                , simple $ many1 identLetter
                ]
            let
                varName = mapOptVarName subRanges (AST.var optVarRange)
                varRange = optVarRange { AST.var = varName }
                ident = Just AST.TemplateIdent
                    { AST.prefix = prefix
                    , AST.varName = varName
                    , AST.suffix = suffix
                    }
                ranges = varRange:subRanges
            return (ranges, ident)
        mapOptVarName prev "#" = "#" ++ (show $ (length prev) + 1)
        mapOptVarName _ name = name


forVarRange optVarName
    | optVarName = do 
        var <- option "#" (try $ variableName <* reserved "in")
        range var
    | otherwise = do
        var <- variableName
        reserved "in"
        range var
    where
        range var = brackets $ do
            start <- index
            symbol ".."
            end <- index
            return AST.ForVarRange
                { AST.var   = var
                , AST.start = start
                , AST.end   = end
                }
        index = choice [numberIndex, paramIndex]
        numberIndex = do
            num <- numberLiteral
            return $ AST.NumberLimit (fromIntegral num)
        paramIndex = do
            name <- parameterName
            return $ AST.ParamLimit name

{- Helper functions -}
whiteSpace    = P.whiteSpace lexer
reserved      = P.reserved lexer
parens        = P.parens lexer
brackets      = P.brackets lexer
braces        = P.braces lexer
symbol        = P.symbol lexer
stringLiteral = P.stringLiteral lexer
commaSep      = P.commaSep lexer
commaSep1     = P.commaSep1 lexer
identString    = P.identifier lexer
hexadecimal   = symbol "0" *> P.hexadecimal lexer <* whiteSpace
decimal       = P.decimal lexer <* whiteSpace

keywords = ["module",
            "input", "output",
            "for", "in",
            "as", "with",
            "is", "are",
            "accept", "map",
            "over",
            "to", "at"]   

identStart     = letter
identLetter    = alphaNum <|> char '_' <|> char '-'

moduleName     = identString <?> "module name"
parameterName  = identString <?> "parameter name"
variableName   = identString <?> "variable name"
identifierName = try ident <?> "identifier"
    where
        ident = do
            start <- identStart
            rest <- many identLetter
            let ident = start:rest
            if ident `elem` keywords
                then unexpected $ "reserved word \"" ++ ident ++ "\""
                else return ident

numberLiteral  = try decimal <?> "number literal"
addressLiteral = try hexadecimal <?> "address literal (hex)"
