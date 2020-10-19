{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
--$dna = [ACGTRYSWKMBDHVN\.]
--$rna = [ACGURYSWKMBDHVN\.]
--$protein = [ABCDEFGHIKLMNPQRSTVWXYZ]
--@dna = \" ($dna)* \"
--@rna = \" ($rna)* \"
--@protein = \" ($protein)* \"
-- http://bioinformatics.org/sms2/iupac.html  

tokens :-

  $white+                         ;
  "//".*.                         ;
  "return"                        { \s -> Return}
  "define"                        { \s -> Define}
  "main"                          { \s -> Main}
  "{"                             { \s -> BeginScope}
  "}"                             { \s -> EndScope}
  "("                             { \s -> BeginBracket}
  ")"                             { \s -> EndBracket}
  "["                             { \s -> BeginSquareBracket}
  "]"                             { \s -> EndSquareBracket}
  ";"                             { \s -> SemiColon}
  ","                             { \s -> Comma}
  int                             { \s -> Type s}
  float                           { \s -> Type s}
  char                            { \s -> Type s}
  boolean                         { \s -> Type s}
  string                          { \s -> Type s}
  dna                             { \s -> Type s}
  rna                             { \s -> Type s}
  protein                         { \s -> Type s}
  "=="                            { \s -> Equal}
  "="                             { \s -> Assign}
  "!="                            { \s -> Different}
  ">"                             { \s -> Greater}
  "<"                             { \s -> Less}
  ">="                            { \s -> GreaterOrEqual}
  "<="                            { \s -> LessOrEqual}
  "+"                             { \s -> Plus}
  "-"                             { \s -> Minus}
  "*"                             { \s -> Mult}
  "/"                             { \s -> Div}
  "%"                             { \s -> Mod}
  "^"                             { \s -> Pow}
  while                           { \s -> While}
  if                              { \s -> If}
  else                            { \s -> Else}
  or                              { \s -> OpOr}
  not                             { \s -> OpNot}
  and                             { \s -> OpAnd}
  $digit+	                        { \s -> Int( read s) }
  $digit+\.$digit+                { \s -> Float(read s) }
  \'.\'                           { \s -> Char( read s ) }
  "True"                          { \s -> Boolean(read s) }
  "False"                         { \s -> Boolean(read s) }
--  @dna                            { \s -> Dna(read s) }
--  @rna                            { \s -> Rna(read s) }
--  @protein                        { \s -> Protein(read s) }
  \".*.\"                         { \s -> String(read s) }
  $alpha [$alpha $digit \_ \']*	  { \s -> Var s}

{
-- Each right-hand side has type ::  -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  Return              |
  Define              |
  Main                | 
  BeginScope          |
  EndScope            |
  BeginBracket        |
  EndBracket          |
  BeginSquareBracket  |
  EndSquareBracket    |
  SemiColon           |
  Comma               |
  Type         String |
  Equal               |
  Assign              |
  Different           |
  Greater             |
  Less                | 
  GreaterOrEqual      |     
  LessOrEqual         | 
  Plus                |  
  Minus               |  
  Mult                |
  Div                 |
  Mod                 |
  Pow                 |
  While               |
  If                  |
  Else                |
  OpOr                |
  OpNot               |
  OpAnd               |
  Int             Int |
  Float         Float |
  Char           Char |
  Boolean        Bool |
  String       String |
  Dna          String |
  Rna          String |
  Protein      String |
  Var  String 
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}

