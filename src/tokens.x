{
  module Main (
                main, 
                Token(..), 
                AlexPosn(..), 
                alexScanTokens, 
                token_posn
              ) where
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
  ","                             { \p s -> Comma  p }
  int                             { \p s -> Type s p }
  float                           { \p s -> Type s p }
  char                            { \p s -> Type s p }
  boolean                         { \p s -> Type s p }
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
  xor                             { \p s -> OpXor p }
  and                             { \p s -> OpAnd p }
  $digit+	                        { \p s -> Int p (read s) }
  $digit+\.$digit+                { \p s -> Float (read s)  p }
  \'.\'                           { \p s -> Char (read s)  p }
  "True"                          { \p s -> Boolean (read s) p }
  "False"                         { \p s -> Boolean (read s) p }
  [\=\+\-\*\/\(\)]			          { \p s -> Sym p (head s) }
  $alpha [$alpha $digit \_ \']*	  { \p s -> Var p s }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =

  Return AlexPosn        |
  In  AlexPosn        |
  Sym AlexPosn Char   |
  Var AlexPosn String |
  Int AlexPosn Int
  deriving (Eq,Show)

token_posn (Let p) = p
token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p

main = do
  s <- getContents
  print (alexScanTokens s)
}
