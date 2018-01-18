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

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import qualified SockeyeASTParser as AST

{- Parser main function -}
parseSockeye :: String -> String -> Either ParseError AST.Sockeye
parseSockeye = parse sockeyeFile

data TopLevel
    = ModuleDecl AST.Module
    | TypeDecl AST.NamedType

data ModuleBody
    = ConstDecl AST.NamedConstant
    | InstDecl AST.InstanceDeclaration
    | NodeDecl AST.NodeDeclaration
    | Def AST.Definition

{- Sockeye parsing -}
sockeyeFile = do
    whiteSpace
    (modules, types) <- do
        stmts <- many $ choice [moduleDecl, typeDecl]
        return $ foldr splitDecl ([], []) stmts
    eof
    return AST.Sockeye
        { AST.modules = modules
        , AST.types   = types
        }
    where
        moduleDecl = do
            m <- sockeyeModule
            return $ ModuleDecl m
        typeDecl = do
            t <- namedType
            return $ TypeDecl t
        splitDecl (ModuleDecl m) (ms, ts) = (m:ms, ts)
        splitDecl (TypeDecl t)   (ms, ts) = (ms, t:ts)

sockeyeModule = do
    reserved "module"
    name <- moduleName
    params <- option [] (parens $ commaSep moduleParam)
    (consts, insts, nodes, defs) <- braces moduleBody
    return AST.Module
        { AST.moduleName  = name
        , AST.parameters  = params
        , AST.constDecls  = consts
        , AST.instDecls   = insts
        , AST.nodeDecls   = nodes
        , AST.definitions = defs
        }
    <?> "module specification"

moduleParam = do
    range <- parens naturalSet <?> "parameter range"
    paramName <- parameterName
    return AST.ModuleParameter
        { AST.paramName = paramName
        , AST.paramRange = range
        }

moduleBody = do
    body <- many $ choice [constDecl, instDecl, nodeDecl]
    return $ foldr splitBody ([], [], [], []) body
    where
        constDecl = do
            c <- namedConstant
            return $ ConstDecl c
        instDecl = do
            i <- instanceDeclaration
            return $ InstDecl i
        nodeDecl = do
            n <- nodeDeclaration
            return $ NodeDecl n
        splitBody (ConstDecl c) (cs, is, ns, ds) = (c:cs, is, ns, ds)
        splitBody (InstDecl i)  (cs, is, ns, ds) = (cs, i:is, ns, ds)
        splitBody (NodeDecl n)  (cs, is, ns, ds) = (cs, is, n:ns, ds)
        splitBody (Def d)       (cs, is, ns, ds) = (cs, is, ns, d:ds)

instanceDeclaration = do
    reserved "instance"
    (name, size) <- arrayDecl
    reserved "of"
    modName <- moduleName
    return $ case size of
        Nothing -> AST.SingleInstance
            { AST.instanceName   = name
            , AST.instanceModule = modName
            }
        Just s -> AST.ArrayInstance
            { AST.instanceName   = name
            , AST.instanceModule = modName
            , AST.instArrSize = s
            }
    <?> "instance declaration"

nodeDeclaration = do
    kind <- nodeKind
    originDomain <- domain
    originType <- addressType
    (targetDomain, targetType) <- option (originDomain, Nothing) $ do
        reserved "to"
        d <- domain
        t <- optionMaybe addressType
        return (d, t)
    (name, size) <- arrayDecl
    return $ case size of
        Nothing -> AST.SingleNode
            { AST.nodeKind     = kind
            , AST.originDomain = originDomain
            , AST.originType   = originType
            , AST.targetDomain = targetDomain
            , AST.targetType   = targetType
            , AST.nodeName     = name
            }
        Just s -> AST.ArrayNode
            { AST.nodeKind     = kind
            , AST.originDomain = originDomain
            , AST.originType   = originType
            , AST.targetDomain = targetDomain
            , AST.targetType   = targetType
            , AST.nodeName     = name
            , AST.nodeArrSize  = s 
            }
    <?> "node declaration"

nodeKind = option AST.InternalNode $ choice [input, output]
    where
        input = do
            reserved "input"
            return AST.InputPort
        output = do
            reserved "output"
            return AST.OutputPort

domain = choice [memory, intr, power, clock] <?> "domain"
    where
        memory = do
            reserved "memory"
            return AST.Memory
        intr = do
            reserved "intr"
            return AST.Interrupt
        power = do
            reserved "power"
            return AST.Power
        clock = do
            reserved "clock"
            return AST.Clock

arrayDecl = do
    name <- identifierName
    size <- optionMaybe (brackets $ semiSep1 naturalSet)
    whiteSpace
    return (name, size)

namedType = do
    reserved "type"
    name <- typeName
    namedType <- addressType
    return AST.NamedType
        { AST.typeName  = name
        , AST.namedType = namedType
        }
    <?> "named type"

namedConstant = do
    reserved "const"
    name <- constName
    value <- natural
    return AST.NamedConstant
        { AST.constName  = name
        , AST.namedConst = value
        }
    <?> "namedConstant"

addressType = do
    parens $ semiSep1 naturalSet

naturalRange = do
    base <- naturalLeaf
    choice [bits base, limit base, singleton base]
    <?> "range of naturals"
    where
        bits base = do
            reserved "bits"
            bits <- naturalLeaf
            return AST.BitsRange
                { AST.base = base
                , AST.bits = bits
                }
        limit base = do
            reserved "to"
            limit <- naturalLeaf
            return AST.LimitRange
                { AST.base = base
                , AST.limit = limit
                }
        singleton base = return $ AST.SingletonRange base

naturalSet = commaSep1 naturalRange <?> "set of naturals"

naturalLeaf = do
    value <- natural
    return (AST.NaturalLeaf $ AST.Literal value)

{- Helper functions -}
lexer = P.makeTokenParser (
    emptyDef {
        {- List of reserved Names -}
        P.reservedNames = keywords,

        {- List of operators -}
        P.reservedOpNames = operators,

        {- Valid identifiers -}
        P.identStart = letter,
        P.identLetter = identLetter,

        {- comment start and end -}
        P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "//",
        P.nestedComments = False,

        {- Sockeye is case sensitive -}
        P.caseSensitive = True
    })

whiteSpace    = P.whiteSpace lexer
reserved      = P.reserved lexer
parens        = P.parens lexer
brackets      = P.brackets lexer
braces        = P.braces lexer
symbol        = P.symbol lexer
commaSep      = P.commaSep lexer 
commaSep1     = P.commaSep1 lexer
semiSep       = P.semiSep lexer
semiSep1      = P.semiSep1 lexer
identString   = P.identifier lexer
natural       = P.natural lexer

keywords =
    [ "import", "module"
    , "input", "output"
    , "type", "const"
    , "memory", "intr", "power", "clock", "instance"
    , "of"
    , "forall", "in"
    , "accepts", "maps", "converts", "overlays", "binds"
    , "to", "at"
    , "bits"
    ]

operators =
    [ "+", "-", "*", "/", "++"
    , "!", "&&", "||"
    ]

identStart     = letter
identLetter    = alphaNum <|> char '_'

importPath     = many (identLetter <|> char '/') <* whiteSpace
typeName       = identString <?> "type name"
constName      = identString <?> "constant name"
moduleName     = identString <?> "module name"
parameterName  = identString <?> "parameter name"
variableName   = identString <?> "variable name"
propertyName   = identString <?> "property name"
identifierName = ident <?> "identifier"
    where
        ident = do
            start <- identStart
            rest <- many identLetter
            let ident = start:rest
            if ident `elem` keywords
                then (unexpected $ "reserved word \"" ++ ident ++ "\"")
                else return ident
