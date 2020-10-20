module Terminals where

import Data.Functor.Identity
import Lexer
import Text.Parsec


-- parsers para os tokens

mainToken :: ParsecT [Token] st Identity Token
mainToken = tokenPrim show update_pos get_token where
  get_token (Main p)    = Just (Main p)
  get_token _       = Nothing

idToken :: ParsecT [Token] st Identity Token
idToken = tokenPrim show update_pos get_token where
  get_token (Var p s) = Just (Var p s)
  get_token _      = Nothing

beginScopeToken :: ParsecT [Token] st Identity Token
beginScopeToken = tokenPrim show update_pos get_token where
  get_token (BeginScope p) = Just (BeginScope p)
  get_token _     = Nothing

endScopeToken :: ParsecT [Token] st Identity Token
endScopeToken = tokenPrim show update_pos get_token where
  get_token (EndScope p) = Just (EndScope p)
  get_token _   = Nothing

beginBracketToken :: ParsecT [Token] st Identity Token
beginBracketToken = tokenPrim show update_pos get_token where
  get_token (BeginBracket p) = Just (BeginBracket p)
  get_token _     = Nothing

endBracketToken :: ParsecT [Token] st Identity Token
endBracketToken = tokenPrim show update_pos get_token where
  get_token (EndBracket p) = Just (EndBracket p)
  get_token _   = Nothing

semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing

commaToken :: Parsec [Token] st Token
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _         = Nothing

colonToken :: Parsec [Token] st Token
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _         = Nothing

assignToken :: ParsecT [Token] st Identity Token
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _      = Nothing

typeToken :: ParsecT [Token] st Identity Token
typeToken = tokenPrim show update_pos get_token where
  get_token (Type p s) = Just (Type p s)
  get_token _       = Nothing

returnToken :: ParsecT [Token] st Identity Token
returnToken = tokenPrim show update_pos get_token where
  get_token (Return p) = Just (Return p)
  get_token _     = Nothing

structToken :: ParsecT [Token] st Identity Token
structToken = tokenPrim show update_pos get_token where
  get_token (Struct p) = Just (Struct p)
  get_token _     = Nothing

defToken :: ParsecT [Token] st Identity Token
defToken = tokenPrim show update_pos get_token where
  get_token (Def p) = Just (Def p)
  get_token _     = Nothing

constToken :: ParsecT [Token] st Identity Token
constToken = tokenPrim show update_pos get_token where
  get_token (Const p) = Just (Const p)
  get_token _     = Nothing

intToken :: ParsecT [Token] st Identity Token
intToken = tokenPrim show update_pos get_token where
  get_token (Int p s) = Just (Int p s)
  get_token _       = Nothing

floatToken :: ParsecT [Token] st Identity Token
floatToken = tokenPrim show update_pos get_token where
  get_token (Float p s) = Just (Float p s)
  get_token _       = Nothing

charToken :: ParsecT [Token] st Identity Token
charToken = tokenPrim show update_pos get_token where
  get_token (Char p s) = Just (Char p s)
  get_token _       = Nothing

booleanToken :: ParsecT [Token] st Identity Token
booleanToken = tokenPrim show update_pos get_token where
  get_token (Boolean p s) = Just (Boolean p s)
  get_token _       = Nothing
{-
dnaToken :: ParsecT [Token] st Identity Token
dnaToken = tokenPrim show update_pos get_token where
  get_token (Dna p s) = Just (Dna p s)
  get_token _       = Nothing

rnaToken :: ParsecT [Token] st Identity Token
rnaToken = tokenPrim show update_pos get_token where
  get_token (Rna p s) = Just (Rna p s)
  get_token _       = Nothing

proteinToken :: ParsecT [Token] st Identity Token
proteinToken = tokenPrim show update_pos get_token where
  get_token (Protein p s) = Just (Protein p s)
  get_token _       = Nothing -}

stringToken :: ParsecT [Token] st Identity Token
stringToken = tokenPrim show update_pos get_token where
  get_token (String p s) = Just (String p s)
  get_token _       = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos
