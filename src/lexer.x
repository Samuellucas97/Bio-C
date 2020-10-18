{
--module Main (main, Token(..), AlexPosn(..), alexScanTokens) where
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$dna = [ACGTRYSWKMBDHVN\.]
$rna = [ACGURYSWKMBDHVN\.]
$protein = [ABCDEFGHIKLMNPQRSTVWXYZ]
--@dna = \" ($dna)* \"
--@rna = \" ($rna)* \"
--@protein = \" ($protein)* \"
-- http://bioinformatics.org/sms2/iupac.html  

tokens :-

  $white+                         ;
  "//".*.                         ;
  "return"                        { \p s -> Return p }
  "define"                        { \p s -> Define p }
  "main"                          { \p s -> Main p }
  "{"                             { \p s -> BeginScope p }
  "}"                             { \p s -> EndScope p }
  "("                             { \p s -> BeginBracket p }
  ")"                             { \p s -> EndBracket p }
  "["                             { \p s -> BeginSquareBracket p }
  "]"                             { \p s -> EndSquareBracket p }
  ";"                             { \p s -> SemiColon p }
  ","                             { \p s -> Comma p }
  int                             { \p s -> Type p s }
  float                           { \p s -> Type p s }
  char                            { \p s -> Type p s }
  boolean                         { \p s -> Type p s }
  string                          { \p s -> Type p s }
  dna                             { \p s -> Type p s }
  rna                             { \p s -> Type p s }
  protein                         { \p s -> Type p s }
  "=="                            { \p s -> Equal p }
  "="                             { \p s -> Assign p }
  "!="                            { \p s -> Different p }
  ">"                             { \p s -> Greater p }
  "<"                             { \p s -> Less p }
  ">="                            { \p s -> GreaterOrEqual p }
  "<="                            { \p s -> LessOrEqual p }
  "+"                             { \p s -> Plus p }
  "-"                             { \p s -> Minus p }
  "*"                             { \p s -> Mult p }
  "/"                             { \p s -> Div p }
  "%"                             { \p s -> Mod p }
  "^"                             { \p s -> Pow p }
  while                           { \p s -> While p }
  if                              { \p s -> If p }
  else                            { \p s -> Else p }
  or                              { \p s -> OpOr p }
  not                             { \p s -> OpNot p }
  and                             { \p s -> OpAnd p }
  $digit+	                        { \p s -> Int p ( read s) }
  $digit+\.$digit+                { \p s -> Float p (read s) }
  \'.\'                           { \p s -> Char p ( read s ) }
  "True"                          { \p s -> Boolean p (read s) }
  "False"                         { \p s -> Boolean p (read s) }
--  @dna                            { \p s -> Dna p (read s) }
--  @rna                            { \p s -> Rna p (read s) }
--  @protein                        { \p s -> Protein p (read s) }
  \".*.\"                         { \p s -> String p (read s) }
  $alpha [$alpha $digit \_ \']*	  { \p s -> Var p s }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  Return             AlexPosn |
  Define             AlexPosn |
  Main               AlexPosn | 
  BeginScope         AlexPosn |
  EndScope           AlexPosn |
  BeginBracket       AlexPosn |
  EndBracket         AlexPosn |
  BeginSquareBracket AlexPosn |
  EndSquareBracket   AlexPosn |
  SemiColon          AlexPosn |
  Comma              AlexPosn |
  Type        AlexPosn String |
  Equal              AlexPosn |
  Assign             AlexPosn |
  Different          AlexPosn |
  Greater            AlexPosn |
  Less               AlexPosn | 
  GreaterOrEqual     AlexPosn |     
  LessOrEqual        AlexPosn | 
  Plus               AlexPosn |  
  Minus              AlexPosn |  
  Mult               AlexPosn |
  Div                AlexPosn |
  Mod                AlexPosn |
  Pow                AlexPosn |
  While              AlexPosn |
  If                 AlexPosn |
  Else               AlexPosn |
  OpOr               AlexPosn |
  OpNot              AlexPosn |
  OpAnd              AlexPosn |
  Int            AlexPosn Int |
  Float        AlexPosn Float |
  Char          AlexPosn Char |
  Boolean       AlexPosn Bool |
  String      AlexPosn String |
  Dna         AlexPosn String |
  Rna         AlexPosn String |
  Protein     AlexPosn String |
  Var AlexPosn String 
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}

