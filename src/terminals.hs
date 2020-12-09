module Terminals where

import Data.Functor.Identity
import Lexer
import Text.Parsec

import Control.Monad.IO.Class
import System.IO.Unsafe


-- parsers para os tokens

reverseTranscriptionToken :: ParsecT [Token] st IO(Token)
reverseTranscriptionToken = tokenPrim show update_pos get_token where
  get_token (ReverseTranscription p)    = Just (ReverseTranscription p)
  get_token _       = Nothing

translateToken :: ParsecT [Token] st IO(Token)
translateToken = tokenPrim show update_pos get_token where
  get_token (Translate p)    = Just (Translate p)
  get_token _       = Nothing

transcriptionToken :: ParsecT [Token] st IO(Token)
transcriptionToken = tokenPrim show update_pos get_token where
  get_token (Transcription p)    = Just (Transcription p)
  get_token _       = Nothing

reverseToken :: ParsecT [Token] st IO(Token)
reverseToken = tokenPrim show update_pos get_token where
  get_token (Reverse p)    = Just (Reverse p)
  get_token _       = Nothing


complementToken :: ParsecT [Token] st IO(Token)
complementToken = tokenPrim show update_pos get_token where
  get_token (Complement p)    = Just (Complement p)
  get_token _       = Nothing


complement_reverseToken :: ParsecT [Token] st IO(Token)
complement_reverseToken = tokenPrim show update_pos get_token where
  get_token (ReverseComplement p)    = Just (ReverseComplement p)
  get_token _       = Nothing

readToken :: ParsecT [Token] st IO(Token)
readToken = tokenPrim show update_pos get_token where
  get_token (Read p)    = Just (Read p)
  get_token _       = Nothing

mainToken :: ParsecT [Token] st IO(Token)
mainToken = tokenPrim show update_pos get_token where
  get_token (Main p)    = Just (Main p)
  get_token _       = Nothing

idToken :: ParsecT [Token] st IO(Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Var p s) = Just (Var p s)
  get_token _      = Nothing

typeToken :: ParsecT [Token] st IO(Token)
typeToken = tokenPrim show update_pos get_token where
  get_token (Type p s) = Just (Type p s)
  get_token _       = Nothing

beginScopeToken :: ParsecT [Token] st IO(Token)
beginScopeToken = tokenPrim show update_pos get_token where
  get_token (BeginScope p) = Just (BeginScope p)
  get_token _     = Nothing

endScopeToken :: ParsecT [Token] st IO(Token)
endScopeToken = tokenPrim show update_pos get_token where
  get_token (EndScope p) = Just (EndScope p)
  get_token _   = Nothing

beginBracketToken :: ParsecT [Token] st IO(Token)
beginBracketToken = tokenPrim show update_pos get_token where
  get_token (BeginBracket p) = Just (BeginBracket p)
  get_token _     = Nothing

endBracketToken :: ParsecT [Token] st IO(Token)
endBracketToken = tokenPrim show update_pos get_token where
  get_token (EndBracket p) = Just (EndBracket p)
  get_token _   = Nothing

beginSquareBracketToken :: ParsecT [Token] st IO(Token)
beginSquareBracketToken = tokenPrim show update_pos get_token where
  get_token (BeginSquareBracket p) = Just (BeginSquareBracket p)
  get_token _     = Nothing

endSquareBracketToken :: ParsecT [Token] st IO(Token)
endSquareBracketToken = tokenPrim show update_pos get_token where
  get_token (EndSquareBracket p) = Just (EndSquareBracket p)
  get_token _   = Nothing

semiColonToken :: ParsecT [Token] st IO(Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing

commaToken :: ParsecT [Token] st IO(Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _         = Nothing
{-
colonToken :: ParsecT [Token] st IO(Token)
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _         = Nothing
-}

dotToken :: ParsecT [Token] st IO(Token)
dotToken = tokenPrim show update_pos get_token where
  get_token (Dot p) = Just (Dot p)
  get_token _         = Nothing

ifToken :: ParsecT [Token] st IO(Token)
ifToken = tokenPrim show update_pos get_token where
  get_token (If p) = Just (If p)
  get_token _         = Nothing

elseToken :: ParsecT [Token] st IO(Token)
elseToken = tokenPrim show update_pos get_token where
  get_token (Else p) = Just (Else p)
  get_token _         = Nothing

assignToken :: ParsecT [Token] st IO(Token)
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _      = Nothing


returnToken :: ParsecT [Token] st IO(Token)
returnToken = tokenPrim show update_pos get_token where
  get_token (Return p) = Just (Return p)
  get_token _     = Nothing

structToken :: ParsecT [Token] st IO(Token)
structToken = tokenPrim show update_pos get_token where
  get_token (Struct p) = Just (Struct p)
  get_token _     = Nothing

defToken :: ParsecT [Token] st IO(Token)
defToken = tokenPrim show update_pos get_token where
  get_token (Def p) = Just (Def p)
  get_token _     = Nothing

constToken :: ParsecT [Token] st IO(Token)
constToken = tokenPrim show update_pos get_token where
  get_token (Const p) = Just (Const p)
  get_token _     = Nothing

notToken :: ParsecT [Token] st IO(Token)
notToken = tokenPrim show update_pos get_token where
  get_token (OpNot p) = Just (OpNot p)
  get_token _     = Nothing

minusToken :: ParsecT [Token] st IO(Token)
minusToken = tokenPrim show update_pos get_token where
  get_token (Minus p) = Just (Minus p)
  get_token _     = Nothing

plusToken :: ParsecT [Token] st IO(Token)
plusToken = tokenPrim show update_pos get_token where
  get_token (Plus p) = Just (Plus p)
  get_token _     = Nothing

divToken :: ParsecT [Token] st IO(Token)
divToken = tokenPrim show update_pos get_token where
  get_token (Div p) = Just (Div p)
  get_token _     = Nothing

multToken :: ParsecT [Token] st IO(Token)
multToken = tokenPrim show update_pos get_token where
  get_token (Mult p) = Just (Mult p)
  get_token _     = Nothing

modToken :: ParsecT [Token] st IO(Token)
modToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _     = Nothing

andToken :: ParsecT [Token] st IO(Token)
andToken = tokenPrim show update_pos get_token where
  get_token (OpAnd p) = Just (OpAnd p)
  get_token _     = Nothing

orToken :: ParsecT [Token] st IO(Token)
orToken = tokenPrim show update_pos get_token where
  get_token (OpOr p) = Just (OpOr p)
  get_token _     = Nothing

powToken :: ParsecT [Token] st IO(Token)
powToken = tokenPrim show update_pos get_token where
  get_token (Pow p) = Just (Pow p)
  get_token _     = Nothing

equalToken :: ParsecT [Token] st IO(Token)
equalToken = tokenPrim show update_pos get_token where
  get_token (Equal p) = Just (Equal p)
  get_token _     = Nothing

differentToken :: ParsecT [Token] st IO(Token)
differentToken = tokenPrim show update_pos get_token where
  get_token (Different p) = Just (Different p)
  get_token _     = Nothing

greaterToken :: ParsecT [Token] st IO(Token)
greaterToken = tokenPrim show update_pos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _     = Nothing

lessToken :: ParsecT [Token] st IO(Token)
lessToken = tokenPrim show update_pos get_token where
  get_token (Less p) = Just (Less p)
  get_token _     = Nothing

greaterEqualToken :: ParsecT [Token] st IO(Token)
greaterEqualToken = tokenPrim show update_pos get_token where
  get_token (GreaterOrEqual p) = Just (GreaterOrEqual p)
  get_token _     = Nothing

lessEqualToken :: ParsecT [Token] st IO(Token)
lessEqualToken = tokenPrim show update_pos get_token where
  get_token (LessOrEqual p) = Just (LessOrEqual p)
  get_token _     = Nothing

whileToken :: ParsecT [Token] st IO(Token)
whileToken = tokenPrim show update_pos get_token where
  get_token (While p) = Just (While p)
  get_token _     = Nothing

intToken :: ParsecT [Token] st IO(Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int p s) = Just (Int p s)
  get_token _       = Nothing

floatToken :: ParsecT [Token] st IO(Token)
floatToken = tokenPrim show update_pos get_token where
  get_token (Float p s) = Just (Float p s)
  get_token _       = Nothing

charToken :: ParsecT [Token] st IO(Token)
charToken = tokenPrim show update_pos get_token where
  get_token (Char p s) = Just (Char p s)
  get_token _       = Nothing

booleanToken :: ParsecT [Token] st IO(Token)
booleanToken = tokenPrim show update_pos get_token where
  get_token (Boolean p s) = Just (Boolean p s)
  get_token _       = Nothing

dnaToken :: ParsecT [Token] st IO(Token)
dnaToken = tokenPrim show update_pos get_token where
  get_token (Dna p s) = Just (Dna p s)
  get_token _       = Nothing

rnaToken :: ParsecT [Token] st IO(Token)
rnaToken = tokenPrim show update_pos get_token where
  get_token (Rna p s) = Just (Rna p s)
  get_token _       = Nothing

proteinToken :: ParsecT [Token] st IO(Token)
proteinToken = tokenPrim show update_pos get_token where
  get_token (Protein p s) = Just (Protein p s)
  get_token _       = Nothing 

stringToken :: ParsecT [Token] st IO(Token)
stringToken = tokenPrim show update_pos get_token where
  get_token (String p s) = Just (String p s)
  get_token _       = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos
