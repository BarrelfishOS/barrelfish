%include polycode.fmt

%if false
  Error: DSL for error definition
   
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%if false

> {-# LANGUAGE BangPatterns #-}

> module HamletAst where

> import Debug.Trace
> import Text.PrettyPrint.HughesPJ as Pprinter 

%endif


> class Pretty a where
>     pretty :: a -> Doc

> data Capabilities = Capabilities { capWordSize :: !Int,
>                                    defines :: ![Define],
>                                    capabilities :: ![Capability] }
>                   deriving Show

> vcat' :: [Doc] -> Doc
> vcat' = foldr ($+$) empty 


> instance Pretty Capabilities where
>     pretty (Capabilities size defs caps) =
>         text "Capabilities:" $+$
>         nest 4 ( text "Cap word size:" <+> int size $+$
>                  text "Defines:" $+$
>                  nest 4 (vcat' $ map pretty defs) $+$
>                  text "Caps:" $+$
>                  nest 4 (vcat' $ map pretty caps))
                 

> data Define = Define !String !Int
>             deriving Show

> instance Pretty Define where
>     pretty (Define name val) = text name <+> char '=' <+> int val


> data Capability = Capability { name :: !CapName,
>                                generalEquality :: !(Maybe Bool),
>                                fields :: ![CapField],
>                                retypeCap :: !(Maybe Multiplicity),
>                                retypePath :: ![RetypePath] }
>                 deriving Show
>                                

> instance Pretty Capability where
>     pretty (Capability (CapName name)
>                        genEq 
>                        fields
>                        retypeCap
>                        retypePath) =
>        text name $+$
>        nest 4 ( text "General Equality:" <+> text (show genEq) $+$
>                 vcat' (map pretty fields) $+$
>                 text "Retype cap:" <+> text (show retypeCap) $+$
>                 vcat' (map pretty retypePath))


> instance Pretty CapField where
>     pretty (CapField decideEq typ (NameField name)) = 
>         text (show typ) <+> text name <+> colon <+> text (show decideEq)

> data CapField = CapField !DecideEq !Type !NameField 
>               deriving Show
> data NameField = NameField !String
>                deriving Show
> data DecideEq = IgnoredField
>               | DecideEquality
>                 deriving (Eq, Show)


> data Type = UInt8
>           | UInt16
>           | UInt32
>           | UInt64
>           | Int
>           | GenPAddr
>           | LPAddr
>           | GenVAddr
>           | LVAddr
>           | CAddr
>           | Pointer String
>           | CapRights
>             deriving Show

> instance Read Type where
>     readsPrec _ s 
>         | s == "uint8" = [(UInt8, "")]
>         | s == "uint16" = [(UInt16, "")]
>         | s == "uint32" = [(UInt32, "")]
>         | s == "uint64" = [(UInt64, "")]
>         | s == "int" = [(Int, "")]
>         | s == "genpaddr" = [(GenPAddr, "")]
>         | s == "lpaddr" = [(LPAddr, "")]
>         | s == "genvaddr" = [(GenVAddr, "")]
>         | s == "lvaddr" = [(LVAddr, "")]
>         | s == "caddr" = [(CAddr, "")]
>         | s == "caprights" = [(CapRights, "")]
>         | otherwise = [(Pointer s, "")]


> data RetypePath = RetypePath !CapName !DecideLeq
>                 deriving Show

> instance Pretty RetypePath where
>     pretty (RetypePath (CapName name) decideLeq) =
>             text name <+> text (show decideLeq)


> data Multiplicity = Unique
>                   | Multiple
>                     deriving Show


> data DecideLeq = Address !NameField
>                | Interval !LeqField !LeqField
>                | NoDecideLeq
>                  deriving Show



> data LeqField = LeqName !String
>               | Sum !String !String
>               | MemToPhysOp !String
>               | SizeOfOp !String
>                 deriving Show

> data CapName = CapName !String
>              deriving Show


