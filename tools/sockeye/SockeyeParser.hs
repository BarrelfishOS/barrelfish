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
import Text.Parsec.Expr
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
        moduleDecl = fmap ModuleDecl sockeyeModule
        typeDecl = fmap TypeDecl namedType
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
    body <- many $ choice [constDecl, instDecl, nodeDecl, def]
    return $ foldr splitBody ([], [], [], []) body
    where
        constDecl = fmap ConstDecl namedConstant
        instDecl = fmap InstDecl instanceDeclaration
        nodeDecl = fmap NodeDecl nodeDeclaration
        def = fmap Def definition
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
    originType <- nodeType
    (targetDomain, targetType) <- option (originDomain, Nothing) $ do
        reserved "to"
        d <- domain
        t <- optionMaybe nodeType
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

nodeType = choice [literal, named]
    where
        literal = do
            t <- typeLiteral
            return $ AST.TypeLiteral t
        named = do
            n <- parens typeName <?> "(<type name>)"
            return $ AST.TypeName n


definition = choice [forall, def]
    where
        def = do
            receiver <- arrayAccess
            choice
                [ accepts receiver
                , maps receiver
                , converts receiver
                , overlays receiver
                , instantiates receiver
                , binds receiver
                ]

accepts (name, index) = do
    reserved "accepts"
    blocks <- brackets $ semiSep addressBlock
    let node = case index of
            Nothing -> AST.SingleNodeRef name
            Just i -> AST.ArrayNodeRef
                { AST.refName  = name
                , AST.refRange = i
                }
    return AST.Accepts
        { AST.node    = node
        , AST.accepts = blocks
        }

maps (name, index) = do
    reserved "maps"
    brackets specs
    let node = case index of
            Nothing -> AST.SingleNodeRef name
            Just i -> AST.ArrayNodeRef
                { AST.refName  = name
                , AST.refRange = i
                }
    return AST.Maps
        { AST.node = node
        , AST.maps = []
        }
    where
        specs = many (many1 (noneOf "[]") <* optional (brackets specs))

converts (name, index) = do
    reserved "converts"
    brackets specs
    let node = case index of
            Nothing -> AST.SingleNodeRef name
            Just i -> AST.ArrayNodeRef
                { AST.refName  = name
                , AST.refRange = i
                }
    return AST.Converts
        { AST.node     = node
        , AST.converts = []
        }
    where
        specs = many (many1 (noneOf "[]") <* optional (brackets specs))

overlays (name, index) = do
    reserved "overlays"
    overlay
    let node = case index of
            Nothing -> AST.SingleNodeRef name
            Just i -> AST.ArrayNodeRef
                { AST.refName  = name
                , AST.refRange = i
                }
    return AST.Overlays
        { AST.node     = node
        , AST.overlays = AST.InternalNodeRef node
        }
    where
        overlay = many (alphaNum <|> oneOf "_.[]") <* whiteSpace

instantiates (name, index) = do
    reserved "instantiates"
    modName <- moduleName
    args <- option [] (parens $ commaSep naturalExpr)
    let inst = case index of
            Nothing -> AST.SingleInstRef name
            Just i -> AST.ArrayInstRef
                { AST.instanceRef   = name
                , AST.instanceRange = i
                }
    return AST.Instantiates
        { AST.inst       = inst
        , AST.instModule = modName
        , AST.arguments  = args
    }

binds (name, index) = do
    reserved "binds"
    brackets bindings
    let inst = case index of
            Nothing -> AST.SingleInstRef name
            Just i -> AST.ArrayInstRef
                { AST.instanceRef   = name
                , AST.instanceRange = i
                }
    return AST.Binds
        { AST.inst  = inst
        , AST.binds = []
        }
    where
        bindings = many (many1 (noneOf "[]") <* optional (brackets bindings))

forall = do
    reserved "forall"
    var <- variableName
    reserved "in"
    range <- parens $ semiSep1 naturalSet
    body <- braces $ many definition
    return AST.Forall
        { AST.boundVarName   = var
        , AST.varRange       = range
        , AST.quantifierBody = body
        }

namedType = do
    reserved "type"
    name <- typeName
    namedType <- typeLiteral
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
    <?> "named constant"

typeLiteral = parens (semiSep1 naturalSet) <?> "type literal"

addressBlock = do
    addr <- parens (semiSep1 wildcardSet) <?> "address tuple"
    props <- option AST.True propertyExpr
    return AST.AddressBlock
        { AST.addresses  = addr
        , AST.properties = props
        }

arrayDecl = do
    name <- identifierName
    size <- optionMaybe (brackets $ semiSep1 naturalSet)
    return (name, size)

arrayAccess = do
    name <- identifierName
    index <- optionMaybe (brackets $ semiSep1 wildcardSet)
    return (name, index)

naturalSet = commaSep1 naturalRange <?> "set of naturals"

wildcardSet = choice [explicit, wildcard]
    where
        explicit = do
            set <- naturalSet
            return $ AST.ExplicitSet set
        wildcard = do
            reservedOp "*"
            return AST.Wildcard

naturalRange = do
    base <- naturalExpr
    choice [bits base, limit base, singleton base]
    <?> "range of naturals"
    where
        bits base = do
            reserved "bits"
            bits <- naturalExpr
            return AST.BitsRange
                { AST.base = base
                , AST.bits = bits
                }
        limit base = do
            reserved "to"
            limit <- naturalExpr
            return AST.LimitRange
                { AST.base = base
                , AST.limit = limit
                }
        singleton base = return $ AST.SingletonRange base

naturalExpr = buildExpressionParser opTable term <?> "arithmetic expression"
    where
        term = parens naturalExpr <|> fmap AST.Variable variableName <|> fmap AST.Literal natural
        opTable =
            [ [ Postfix (brackets naturalSet >>= return . flip AST.Slice) ]
            , [ Infix (reservedOp "*" >> return AST.Multiplication) AssocLeft ]
            , [ Infix (reservedOp "+" >> return AST.Addition) AssocLeft
              , Infix (reservedOp "-" >> return AST.Subtraction) AssocLeft
              ]
            , [ Infix (reservedOp "++" >> return AST.Concat) AssocLeft ]
            ]

propertyExpr = buildExpressionParser opTable term <?> "property expression"
    where
        term = parens propertyExpr <|> fmap AST.Property propertyName
        opTable =
            [ [ Prefix (reservedOp "!" >> return AST.Not) ]
            , [ Infix (reservedOp "&&" >> return AST.And) AssocLeft
              , Infix (reservedOp "||" >> return AST.Or) AssocLeft
              ]
            ]

{- Helper functions -}
lexer = P.makeTokenParser (
    emptyDef {
        {- List of reserved Names -}
        P.reservedNames = keywords,

        {- List of operators -}
        P.reservedOpNames = operators,

        {- Valid identifiers -}
        P.identStart = letter,
        P.identLetter = alphaNum <|> char '_',

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
reservedOp    = P.reservedOp lexer
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
    , "accepts", "maps", "converts", "overlays"
    , "instantiates", "binds"
    , "to", "at"
    , "bits"
    ]

operators =
    [ "+", "-", "*", "/", "++"
    , "!", "&&", "||"
    ]

typeName       = identString <?> "type name"
constName      = identString <?> "constant name"
moduleName     = identString <?> "module name"
parameterName  = identString <?> "parameter name"
variableName   = identString <?> "variable name"
propertyName   = identString <?> "property name"
identifierName = identString <?> "identifier"
