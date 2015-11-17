%if false
  Copyright (c) 2015 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Libbarrelfish.GetAddress where

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile
> import IL.FoF.Run

%endif

\section{Get Address}

This construct embeds the Hamlet-generated function @get_address@ into FoF, so
that we can recursively call @get_address@ in Hamlet address expressions.


\subsection{Smart Constructors}

As for |HasDescendants|, both named and anonymous function are
provided. They are direct wrappers around the @get_address@ function.

> get_address :: PureExpr -> FoFCode PureExpr
> get_address cte = inject (GetAddress Nothing cte return)
>
> get_addressN :: String -> PureExpr -> FoFCode PureExpr
> get_addressN name cte = inject (GetAddress (Just name) cte return)

\subsection{Compile Instantiation}

Compiling is straightforward: just declare a foreign function.

> compileGetAddress (GetAddress mName arg r) binding = 
>     let (loc, binding1) = getFreshVar binding in
>     let name = case mName of
>                  Just x -> Provided x
>                  Nothing -> makeVarName Local loc in
>     let ref = CLRef Local uint64T name in
>     let (cont, binding2) = r ref binding1 in
>     (FStatement (FFFICall "get_address" [ref, arg]) cont,
>      binding2)


\subsection{Run Instantiation}

However, the semantics remains to be defined.

> runGetAddress (GetAddress _ a r) heap = error "GetAddress: eval not implemented"
