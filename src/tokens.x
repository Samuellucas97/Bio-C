{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+                         ;
  "//".*.                         ;
  "return"                        { \p s -> Return (getLC p) }
  "{"                             { \p s -> BeginScope (getLC p) }
  "}"                             { \p s -> EndScope (getLC p) }
  "("                             { \p s -> BeginBracket (getLC p) }
  ")"                             { \p s -> EndBracket (getLC p) }
  ";"                             { \p s -> SemiColon (getLC p) }
  ","                             { \p s -> Comma (getLC p) }

  int                             { \p s -> Type s (getLC p) }
  float                           { \p s -> Type s (getLC p) }
  char                            { \p s -> Type s (getLC p) }
  boolean                         { \p s -> Type s (getLC p) }

  "=="                            { \p s -> Equal (getLC p) }
  "="                             { \p s -> Assign (getLC p) }
  "!="                            { \p s -> Different (getLC p) }
  ">"                             { \p s -> Greater (getLC p) }
  "<"                             { \p s -> Less (getLC p) }
  ">="                            { \p s -> GreaterOrEqual (getLC p) }
  "<="                            { \p s -> LessOrEqual (getLC p) }
  "+"                             { \p s -> Plus (getLC p) }
  "*"                             { \p s -> Mult (getLC p) }
  "/"                             { \p s -> Div (getLC p) }
  
--  while                           { \p s -> While (getLC p) }
--  if                              { \p s -> If (getLC p) }
--  else                            { \p s -> Else (getLC p) }
--  or                              { \p s -> OpOr (getLC p) }
--  not                             { \p s -> OpXor (getLC p) }
--  and                             { \p s -> OpAnd (getLC p) }
--  $digit+	                        { \p s -> Int p (read s) }
--  $digit+\.$digit+                { \p s -> Float (read s)  (getLC p) }
--  \'.\'                           { \p s -> Char (read s)  (getLC p) }
--  "True"                          { \p s -> Boolean (read s) (getLC p) }
--  "False"                         { \p s -> Boolean (read s) (getLC p) }
--  $alpha [$alpha $digit \_ \']*	  { \p s -> Var s  (getLC p)}

{
-- Each right-hand side has type :: String -> AlexPosn -> Token
-- Some action helpers:

-- The token type:
data Token =
  Return          (Int, Int) |
  BeginScope      (Int, Int) |
  EndScope        (Int, Int) |
  BeginBracket    (Int, Int) |
  EndBracket      (Int, Int) |
  SemiColon       (Int, Int) |
  Comma           (Int, Int) |
  Type String     (Int, Int) |
  Int  Int        (Int, Int) |
  Float Float     (Int, Int) |
  Char Char       (Int, Int) |
  Boolean Bool    (Int, Int) |

  Equal (Int, Int)            |
  Assign (Int, Int)           |
  Different (Int, Int)        | 
  Greater (Int, Int)          |
  Less (Int, Int)             | 
  GreaterOrEqual (Int, Int)   |     
  LessOrEqual (Int, Int)      | 
  Plus (Int, Int)             |  
  Mult (Int, Int)             |
  Div (Int, Int)               


getLC (AlexPn _ l c) = (l, c)  


main = do
  s <- getContents
  print (alexScanTokens s)
}
