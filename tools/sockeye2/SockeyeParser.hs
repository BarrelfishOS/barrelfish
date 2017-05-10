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

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)

import qualified Data.Map as Map

import SockeyeAST as AST

lexer = P.makeTokenParser (
    javaStyle  {
        {- list of reserved Names -}
        P.reservedNames = [
            "is", "are",
            "accept", "map",
            "over",
            "to", "at"
        ],

        {- valid identifiers -}
        P.identStart = letter,
        P.identLetter = alphaNum,

        {- comment start and end -}
        P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "//",
        P.nestedComments = False
    })

identifier = P.identifier lexer
reserved = P.reserved lexer
address = P.natural lexer <?> "address"
brackets = P.brackets lexer
symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
commaSep = P.commaSep lexer

sockeyeFile = do
    nodes <- many net
    eof
    return $ AST.Net (Map.fromList $ concat nodes)

net = do
    try single <|> multiple
    where single = do
            nodeId <- identifier
            reserved "is"
            node <- node
            return [(nodeId, node)]
          multiple = do
            nodeIds <- commaSep identifier
            reserved "are"
            node <- node
            return $ map (\nodeId -> (nodeId, node)) nodeIds

node = do
    accept <- try accept <|> return []
    translate <- try tranlsate <|> return []
    overlay <- try overlay <|> return Nothing
    return AST.Node { accept    = accept
                    , translate = translate
                    }
    where accept = do
            reserved "accept"
            brackets $ commaSep addrBlock
          tranlsate = do
            reserved "map"
            brackets $ commaSep mapping
          overlay = do
            reserved "over"
            nodeId <- identifier
            return (Just nodeId)


mapping = do
    fromBlock <- addrBlock
    reserved "to"
    name <- name
    return (fromBlock, name)

name = do
    nodeId <- identifier
    reserved "at"
    block <- addrBlock
    return AST.Name { nodeId = nodeId
                    , block  = block
                    }

addrBlock = do
    try realBlock
    <|> singletonBlock
    where realBlock = do
            base <- address
            symbol "-"
            limit <- address
            return AST.Block { base  = fromIntegral base
                             , limit = fromIntegral limit
                             }
          singletonBlock = do
            address <- address
            return AST.Block { base  = fromIntegral address
                             , limit = fromIntegral address
                             }

parseSockeye :: String -> String -> Either ParseError AST.Net
parseSockeye = parse sockeyeFile
