%include polycode.fmt

%if false
  Sockeye: an fact specification language
   
  Copyright (c) 2015 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. 
  Attn: Systems Group.
%endif


\section{The Abstract Syntax}

In this module, we define the combinators to embed the Flounder
language into Haskell. So, we will mix the design the Abstract-Syntax
Tree with the definition of some handy combinators.

> module SockeyeSyntax where



% =============================================================================
% Schema
% =============================================================================

\subsection{Interface Header}

First, we define the abstract syntax of our embedded language. At the
top-level is the \emph{schema} definition. It consists of a @name@
and a @description@. It contains a list of\emph{facts} or \emph{queries}.

> data Schema = Schema String String [ Declaration ] 
>
> schema :: String -> String -> [ Declaration ] -> Schema
> schema name description declarations = 
>     Schema name description declarations


Finally, various getters:

> schemaName :: Schema -> String
> schemaName (Schema name _ _) = name

\subsection{Declarations}

A declaration is either a \emph{type definition}, a \emph{fact
definition} or a \emph{query definition}. In the next subsections, 
we define these terms in turn.

> data Declaration = Typedef TypeDef
>                  | Factdef FactDef
>                  | Querydef QueryDef

% =============================================================================
% Types
% =============================================================================


\subsubsection{Declaring types}

We can define new types out of existing ones thanks to five
constructs:
\begin{description}
        \item[Enumeration:] like C @enum@, defines a sum-type over some elements
        \item[Alias:] redefine the name of an already defined type
\end{description}

> data TypeDef = TEnum String [String] 
>              | TAlias String TypeRef
>              | TAliasT String TypeBuiltin

In this definition, we notice the presence of @TypeRef@: indeed, when
we construct a new type, it can use either built-in types, such as
@uint8_t@, or previously defined types, identified by their name.

> data TypeRef = Builtin TypeBuiltin
>              | TypeVar String
>              | FactType String
>              | TypeAlias String TypeBuiltin
>     deriving (Show)

The builtin types being:

> data TypeBuiltin = UInt8
>                  | UInt16
>                  | UInt32
>                  | UInt64
>                  | UIntPtr
>                  | Int8
>                  | Int16
>                  | Int32
>                  | Int64
>                  | IntPtr
>                  | Size
>                  | Char
>                  | Bool
>                  | String
>                  | IRef
>                    deriving (Enum, Eq)

Which are shown with:

> instance Show TypeBuiltin where
>     show UInt8 = "uint8"
>     show UInt16 = "uint16"
>     show UInt32 = "uint32"
>     show UInt64 = "uint64"
>     show UIntPtr = "uintptr"
>     show Int8 = "int8"
>     show Int16 = "int16"
>     show Int32 = "int32"
>     show Int64 = "int64"
>     show IntPtr = "intptr"
>     show Size = "size"
>     show Bool = "bool"
>     show String = "string"
>     show Char = "char"
>     show IRef = "iref"

> instance Read TypeBuiltin where
>     readsPrec _ = \s -> case s of 
>                                "uint8" -> [(UInt8, "")]
>                                "uint16" -> [(UInt16, "")]
>                                "uint32" -> [(UInt32, "")]
>                                "uint64" -> [(UInt64, "")]
>                                "uintptr" -> [(UIntPtr, "")]
>                                "int8" -> [(Int8, "")]
>                                "int16" -> [(Int16, "")]
>                                "int32" -> [(Int32, "")]
>                                "int64" -> [(Int64, "")]
>                                "intptr" -> [(IntPtr, "")]
>                                "size" -> [(Size, "")]
>                                "bool" -> [(Bool, "")]
>                                "string" -> [(String, "")]
>                                "char" -> [(Char, "")]
>                                "iref" -> [(IRef, "")]
>                                _ -> error  $ "Undefined builtin type " ++ s


> builtin_fmt_wr :: TypeBuiltin -> String
> builtin_fmt_wr (UInt8) = "PRIu8"
> builtin_fmt_wr (UInt16) = "PRIu16"
> builtin_fmt_wr (UInt32) = "PRIu32"
> builtin_fmt_wr (UInt64) = "PRIu64"
> builtin_fmt_wr (UIntPtr) = "PRIuPTR"
> builtin_fmt_wr (Int8) = "PRIi8"
> builtin_fmt_wr (Int16) = "PRIi16"
> builtin_fmt_wr (Int32) = "PRIi32"
> builtin_fmt_wr (Int64) = "PRIi64"
> builtin_fmt_wr (IntPtr) = "PRIuPTR"
> builtin_fmt_wr (Size) = "PRIuSIZE"
> builtin_fmt_wr (Bool) = "i"
> builtin_fmt_wr (String) = "s"
> builtin_fmt_wr (Char) = "c"
> builtin_fmt_wr (IRef) = "PRIuIREF"


> builtin_fmt_rd :: TypeBuiltin -> String
> builtin_fmt_rd (UInt8) = "SCNu8"
> builtin_fmt_rd (UInt16) = "SCNu16"
> builtin_fmt_rd (UInt32) = "SCNu32"
> builtin_fmt_rd (UInt64) = "SCNu64"
> builtin_fmt_rd (UIntPtr) = "SCNuPTR"
> builtin_fmt_rd (Int8) = "SCNi8"
> builtin_fmt_rd (Int16) = "SCNi16"
> builtin_fmt_rd (Int32) = "SCNi32"
> builtin_fmt_rd (Int64) = "SCNi64"
> builtin_fmt_rd (IntPtr) = "SCNuPTR"
> builtin_fmt_rd (Size) = "SCNuSIZE"
> builtin_fmt_rd (Bool) = "i"
> builtin_fmt_rd (String) = "s"
> builtin_fmt_rd (Char) = "c"
> builtin_fmt_rd (IRef) = "SCNuIREF"

Hence, we can define:

> isBuiltin :: TypeRef -> Bool
> isBuiltin (Builtin _) = True
> isBuiltin _ = False

And the usual combinators:

> uint8, uint16, uint32, uint64, uintptr :: TypeRef
> uint8 = Builtin UInt8
> uint16 = Builtin UInt16
> uint32 = Builtin UInt32
> uint64 = Builtin UInt64
> uintptr = Builtin UIntPtr

> int8, int16, int32, int64, intptr :: TypeRef
> int8 = Builtin Int8
> int16 = Builtin Int16
> int32 = Builtin Int32
> int64 = Builtin Int64
> intptr = Builtin IntPtr

> size, string, iref :: TypeRef
> size = Builtin Size
> string = Builtin String
> iref = Builtin IRef

> var :: String -> TypeRef
> var typeRef = TypeVar typeRef

> als :: String -> TypeBuiltin -> TypeRef
> als typeRef origin = TypeAlias typeRef origin

Then, we can build a type definition out of these special cases with:

> typedef :: TypeDef -> Declaration
> typedef typeDefinition = Typedef typeDefinition


Here's a utility function to resolve a named type (which may be an alias) to
its canonical definition:

> lookup_type_name :: [TypeDef] -> String -> TypeDef
> lookup_type_name types name = case def of
>         (TAlias _ (Builtin b)) -> TAliasT name b
>         (TAlias _ (TypeVar v)) -> lookup_type_name types v
>         (TAlias _ (TypeAlias _ b)) -> TAliasT name b
>         d -> d
>     where
>         def
>             | null defs = error $ "lookup_type_name: " ++ name ++ " not defined"
>             | null $ tail defs = head defs
>             | otherwise = error $ "lookup_type_name: " ++ name ++ " multiply defined"
>         defs = [t | t <- types, typedef_name t == name]
> 
>         typedef_name :: TypeDef -> String
>         typedef_name (TEnum n _) = n
>         typedef_name (TAlias n _) = n
>         typedef_name (TAliasT n _) = n

As above, but for a TypeRef:

> lookup_typeref :: [TypeDef] -> TypeRef -> TypeDef
> lookup_typeref _ (Builtin b) = TAliasT (show b) b
> lookup_typeref _ (TypeAlias n b) = TAliasT n b
> lookup_typeref types (TypeVar v) = lookup_type_name types v


\paragraph{Enumeration}

An enumeration is, as always, identified by a @name@. The content of
an enumeration is a list of tags, the @elements@.

> enum :: [String] -> String -> TypeDef
> enum elements name = TEnum name elements

\paragraph{Aliasing}

Finally, we can do type aliasing: we can give a @newName@ to a type,
which was previously known as @originalName@. Note that the names are
switched between the combinator and the data-type.

> alias :: TypeRef -> String -> TypeDef
> alias originalName newName = TAlias newName originalName


% =============================================================================
% Facts
% =============================================================================

\subsubsection{Declaration of a Fact}
A @fact@ is identified by a @name@ and has a @description@ and a set of 
@attributes@ which are described by a list of @FactAttribute@


> data FactDef = Fact String String [ FactAttribute ] 

> fact :: String -> String -> [ FactAttribute ] -> Declaration
> fact name desc args = Factdef $ Fact name desc args

> data FactAttribute = FAttrib TypeRef Variable String
>     deriving (Show)
>
> data Variable = Name String
>                 deriving (Show)
>

% =============================================================================
% Queries
% =============================================================================

\subsubsection{Declaration of a Query}

A @fact@ is identified by a @name@ and has a description and a set of 
@attributes@ which are described by a list of @FactArgument@

> data QueryDef = Query String String [ QueryParam ] 

> query :: String -> String -> [ QueryParam ] -> Declaration
> query name desc args = Querydef $ Query name desc args 

> data QueryParam = QParam Variable String
>     deriving (Show)

