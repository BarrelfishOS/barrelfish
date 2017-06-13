{- 
  AbsSyntaxWiki: Represents a parsed Skate Schema
   
  Part of Skate: a Schema specification languge
   
  Copyright (c) 2017, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-} 

module AbsSyntaxWiki where


import Data.List


tableOfContent :: String
tableOfContent = (heading "Table of contents:")  ++ "<<TableOfContents()>>"

footnote :: String -> String
footnote s = "<<FootNote("++ s ++")>>"


newLine :: String
newLine = "\n"

lineBreak :: String
lineBreak = newLine ++ newLine

hline :: String
hline = "----"


{- issue = title = -}
headingGeneric :: String -> Int -> String
headingGeneric n l = 
    hardbreak ++ prefix ++ " " ++ n ++ " " ++ prefix ++ lineBreak
    where 
        hardbreak = newLine ++ "<<BR>>" ++ newLine
        prefix = concat $ replicate l "="

title :: String -> String
title n = (headingGeneric n 1) 

heading :: String -> String
heading n = (headingGeneric n 2) 

subheading :: String -> String
subheading n = headingGeneric n 3
        
{-lists-}
unOrderedList :: Int -> [String] -> String
unOrderedList n elms = concat (elms2)
    where
        indent = concat $ replicate n " "
        listPrefix = newLine ++ indent ++ "* "
        elms2 = [listPrefix ++ e | e <- elms ]

orderedList :: Int -> [String] -> String
orderedList n elms = concat (elms2)
    where
        indent = concat $ replicate n " "
        listPrefix = newLine ++ indent ++ "1. "
        elms2 = [listPrefix ++ e | e <- elms ]



{-code-}
code:: String -> String
code c = "{{{\n" ++ c ++ "\n}}}\n"

inlinecode :: String -> String
inlinecode c = "{{{" ++ c ++ "}}}"



{-formats-}

textit :: String -> String
textit s = "''" ++ s ++ "''"

textbf :: String -> String
textbf s = "'''" ++ s ++ "'''"

texttt :: String -> String
texttt s = "`" ++ s ++ "`"

textul :: String -> String
textul s = "__" ++ s ++ "__"


{-table-}


tableHeading :: [String] -> String
tableHeading cols = "||" ++ colstr ++ "||\n"
    where
        colstr = concat (intersperse "||" [textbf s | s <- cols])

tableRow :: [String] -> String
tableRow cols = "||" ++ colstr ++ "||\n"
    where
        colstr = concat (intersperse "||" cols)