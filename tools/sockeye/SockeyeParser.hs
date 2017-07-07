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

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

import qualified SockeyeASTParser as AST

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
    net <- many netSpecs
    return AST.SockeyeSpec
        { AST.modules = modules
        , AST.net     = concat net
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
            symbol "nat"
            return AST.NumberParam
        addrType = do
            symbol "addr" 
            return AST.AddressParam

moduleBody = do
    ports <- many $ portDef
    net <- many netSpecs
    return AST.ModuleBody
        { AST.ports     = concat ports
        , AST.moduleNet = concat net
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

netSpecs = choice [ inst <?> "module instantiation"
                 , decl <?> "node declaration"
                 ]
    where
        inst = do
            moduleInst <- moduleInst
            return $ [AST.ModuleInstSpec moduleInst]
        decl = do
            nodeDecls <- nodeDecls
            return $ [AST.NodeDeclSpec decl | decl <- nodeDecls]

moduleInst = do
    (name, args) <- try $ do
        name <- moduleName
        args <- option [] $ parens (commaSep moduleArg)
        symbol "as"
        return (name, args)
    (forFn, nameSpace) <- identifierFor
    portMappings <- option [] $ symbol "with" *> many1 portMapping
    return $ let
        moduleInst = AST.ModuleInst
            { AST.moduleName = name
            , AST.nameSpace  = nameSpace
            , AST.arguments  = args 
            , AST.portMappings = portMappings
            }
        in case forFn of
            Nothing -> moduleInst
            Just f  -> AST.MultiModuleInst $ f moduleInst

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

portMapping = choice [inputMapping, outputMapping]
    where
        inputMapping = do
            (forFn, mappedId) <- try $ identifierFor <* symbol ">"
            portId <- identifier
            return $ let
                portMap = AST.InputPortMap
                    { AST.mappedId   = mappedId
                    , AST.mappedPort = portId
                    }
                in case forFn of
                    Nothing -> portMap
                    Just f  -> AST.MultiPortMap $ f portMap
        outputMapping = do
            (forFn, mappedId) <- try $ identifierFor <* symbol "<"
            portId <- identifier
            return $ let
                portMap = AST.OutputPortMap
                    { AST.mappedId   = mappedId
                    , AST.mappedPort = portId
                    }
                in case forFn of
                    Nothing -> portMap
                    Just f  -> AST.MultiPortMap $ f portMap

nodeDecls = do
    nodeIds <- choice [try single, try multiple]
    nodeSpec <- nodeSpec
    return $ map (toNodeDecl nodeSpec) nodeIds
    where
        single = do
            nodeId <- identifier
            reserved "is"
            return [(Nothing, nodeId)]
        multiple = do
            nodeIds <- commaSep1 identifierFor
            reserved "are"
            return nodeIds
        toNodeDecl nodeSpec (forFn, ident) = let
            nodeDecl = AST.NodeDecl
                { AST.nodeId = ident
                , AST.nodeSpec = nodeSpec
                }
            in case forFn of
                Nothing -> nodeDecl
                Just f  -> AST.MultiNodeDecl $ f nodeDecl

identifier = do
    (_, ident) <- identifierHelper False
    return ident

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
            specs <- brackets $ many mapSpecs
            return $ concat specs
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
            return $ AST.SingletonBlock address
        range = do
            base <- try $ address <* symbol "-"
            limit <- address
            return $ AST.RangeBlock base limit
        length = do
            base <- try $ address <* symbol "/"
            bits <- decimal <?> "number of bits"
            return $ AST.LengthBlock base (fromIntegral bits)

address = choice [address, param]
    where
        address = do
            addr <- addressLiteral
            return $ AST.NumberAddress (fromIntegral addr)
        param = do
            name <- parameterName
            return $ AST.ParamAddress name

mapSpecs = do
    block <- blockSpec
    reserved "to"
    dests <- commaSep1 $ mapDest
    return $ map (toMapSpec block) dests
    where
        mapDest = do
            destNode <- identifier
            destBase <- optionMaybe $ reserved "at" *> address
            return (destNode, destBase)
        toMapSpec block (destNode, destBase) = AST.MapSpec
            { AST.block    = block
            , AST.destNode = destNode
            , AST.destBase = destBase
            }

identifierFor = identifierHelper True

forVarRange optVarName
    | optVarName = do 
        var <- option "#" (try $ variableName <* reserved "in")
        range var
    | otherwise = do
        var <- variableName
        reserved "in"
        range var
    where
        range var = brackets (do
            start <- index
            symbol ".."
            end <- index
            return AST.ForVarRange
                { AST.var   = var
                , AST.start = start
                , AST.end   = end
                }
            )
            <?> "range ([a..b])"
        index = choice [numberIndex, paramIndex]
        numberIndex = do
            num <- numberLiteral
            return $ AST.NumberLimit (fromIntegral num)
        paramIndex = do
            name <- parameterName
            return $ AST.ParamLimit name

{- Helper functions -}
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
            "in",
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

identifierHelper inlineFor = do
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
            (ranges, varName, suffix) <- if inlineFor 
                then choice [forTemplate, varTemplate]
                else varTemplate
            let
                ident = Just AST.TemplateIdent
                    { AST.prefix = prefix
                    , AST.varName = varName
                    , AST.suffix = suffix
                    }
            return (ranges, ident)
        varTemplate = do
            varName <- variableName
            char '}'
            (ranges, suffix) <- templateSuffix
            return (ranges, varName, suffix)
        forTemplate = do
            optVarRange <- forVarRange True
            char '}'
            (subRanges, suffix) <- templateSuffix
            return $ let
                varName = mapOptVarName subRanges (AST.var optVarRange)
                varRange = optVarRange { AST.var = varName }
                ranges = varRange:subRanges
                in (ranges, varName, suffix)
        templateSuffix = option ([], Nothing) $ choice
            [ template $ many identLetter
            , simple $ many1 identLetter
            ]
        mapOptVarName prev "#" = "#" ++ (show $ (length prev) + 1)
        mapOptVarName _ name = name
