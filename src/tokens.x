{
module Main (main, Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+                         ;
  "//".*.                         ;
  "return"                        { \p s -> Return p }
  "{"                             { \p s -> BeginScope p }
  "}"                             { \p s -> EndScope p }
  "("                             { \p s -> BeginBracket p }
  ")"                             { \p s -> EndBracket p }
  ";"                             { \p s -> SemiColon p }
  ","                             { \p s -> Comma p }
  int                             { \p s -> Type p s }
  float                           { \p s -> Type p s }
  char                            { \p s -> Type p s }
  boolean                         { \p s -> Type p s }
  "=="                            { \p s -> Equal p }
  "="                             { \p s -> Assign p }
  "!="                            { \p s -> Different p }
  ">"                             { \p s -> Greater p }
  "<"                             { \p s -> Less p }
  ">="                            { \p s -> GreaterOrEqual p }
  "<="                            { \p s -> LessOrEqual p }
  "+"                             { \p s -> Plus p }
  "*"                             { \p s -> Mult p }
  "/"                             { \p s -> Div p }
  while                           { \p s -> While p }
  if                              { \p s -> If p }
  else                            { \p s -> Else p }
  or                              { \p s -> OpOr p }
  not                             { \p s -> OpNot p }
  and                             { \p s -> OpAnd p }
  $digit+	                        { \p s -> Int p ( read s) }
  $digit+\.$digit+                { \p s -> Float p (read s) }
  \'.\'                           { \p s -> Char p ( head s ) }
  "True"                          { \p s -> Boolean p (read s) }
  "False"                         { \p s -> Boolean p (read s) }
  $alpha [$alpha $digit \_ \']*	  { \p s -> Var p s }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  Return          AlexPosn |
  BeginScope      AlexPosn |
  EndScope        AlexPosn |
  BeginBracket    AlexPosn |
  EndBracket      AlexPosn |
  SemiColon       AlexPosn |
  Comma           AlexPosn |
  Type     AlexPosn String |
  Equal           AlexPosn |
  Assign          AlexPosn |
  Different       AlexPosn |
  Greater         AlexPosn |
  Less            AlexPosn | 
  GreaterOrEqual  AlexPosn |     
  LessOrEqual     AlexPosn | 
  Plus            AlexPosn |  
  Mult            AlexPosn |
  Div             AlexPosn |
  While           AlexPosn |
  If              AlexPosn |
  Else            AlexPosn |
  OpOr            AlexPosn |
  OpNot           AlexPosn |
  OpAnd           AlexPosn |
  Int         AlexPosn Int |
  Float     AlexPosn Float |
  Char       AlexPosn Char |
  Boolean AlexPosn Bool    |
  Var AlexPosn String 
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
