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
        P.reservedNames = [
            "module",
            "input", "output",
            "as", "with",
            "is", "are",
            "accept", "map",
            "over",
            "to", "at"
        ],

        {- valid identifiers -}
        P.identStart = letter,
        P.identLetter = alphaNum <|> char '_' <|> char '-',

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
    paramType <- choice [intType, addrType]
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
    inputPorts <- many inputPort
    outputPorts <- many outputPort
    net <- many netSpec
    return AST.ModuleBody
        { AST.inputPorts  = concat inputPorts
        , AST.outputPorts = concat outputPorts
        , AST.moduleNet   = net
        }
    where
        inputPort = do
            reserved "input"
            commaSep1 identifier
        outputPort = do
            reserved "output"
            commaSep1 identifier

netSpec = choice [inst, decl]
    where
        inst = do
            moduleInst <- moduleInst
            return $ AST.ModuleInstSpec moduleInst
        decl = do
            nodeDecl <- nodeDecl
            return $ AST.NodeDeclSpec nodeDecl

moduleInst = do
    (name, args) <- try $ do
        name <- moduleName
        args <- option [] $ parens (commaSep moduleArg)
        symbol "as"
        return (name, args)
    nameSpace <- nonIndexedIdentifier
    symbol "with"
    inputMappings <- many inputMapping
    outputMappings <- many outputMapping
    return AST.ModuleInst
        { AST.moduleName = name
        , AST.nameSpace  = nameSpace
        , AST.arguments  = args 
        , AST.inputMappings = inputMappings
        , AST.outputMappings = outputMappings
        }
    where
        inputMapping = do
            nodeId <- try $ identifier <* symbol ">"
            port <- identifier
            return AST.ModulePortMap
                { AST.port   = port
                , AST.nodeId = nodeId
                }
        outputMapping = do
            nodeId <- try $ identifier <* symbol "<"
            port <- identifier
            return AST.ModulePortMap
                { AST.port   = port
                , AST.nodeId = nodeId
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

nodeDecl = do
    nodeIds <- try $ choice [single, multiple]
    nodeSpec <- nodeSpec
    return $ AST.NodeDecl
        { AST.nodeIds = nodeIds
        , AST.nodeSpec = nodeSpec
        }
    where single = do
            nodeId <- singleIdentifier
            reserved "is"
            return [nodeId]
          multiple = do
            nodeIds <- many1 $ nonIndexedIdentifier
            reserved "are"
            return nodeIds

singleIdentifier = do
    prefix <- identifierName
    return $ AST.Single prefix

indexedIdentifier = do
    prefix <- try $ identifierName <* symbol "#"
    return $ AST.Indexed prefix

multiIdentifier = do
    prefix <- try $ identifierName <* symbol "["
    start <- index
    symbol ".."
    end <- index
    symbol "]"
    return AST.Multi
        { AST.prefix = prefix
        , AST.start  = start
        , AST.end    = end
        }
    where
        index = choice [numberIndex, paramIndex]
        numberIndex = do
            num <- numberLiteral
            return $ AST.NumberIndex (fromIntegral num)
        paramIndex = do
            name <- parameterName
            return $ AST.ParamIndex name

identifier = choice [ multiIdentifier
                    , indexedIdentifier
                    , singleIdentifier
                    ]

nonIndexedIdentifier = choice [multiIdentifier, singleIdentifier]

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
            singleIdentifier

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
idenString    = P.identifier lexer
hexadecimal   = symbol "0" *> P.hexadecimal lexer <* whiteSpace
decimal       = P.decimal lexer <* whiteSpace

moduleName     = idenString <?> "module name"
parameterName  = idenString <?> "parameter name"
identifierName = idenString <?> "identifier"

numberLiteral  = try decimal <?> "number literal"
addressLiteral = try hexadecimal <?> "address literal (hex)"