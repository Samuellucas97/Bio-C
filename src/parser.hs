module Main (main) where
--module Parser where

import Data.Functor.Identity
import Lexer
import Text.Parsec


--typeToken :: ParsecT [Token] st Identity Token
--typeToken = tokenPrim show update_pos get_token where
--  get_token (Type p s) = Just (Type p s)
--  get_token _       = Nothing

--semiColonToken :: Parsec [Token] st Token
--semiColonToken = tokenPrim show update_pos get_token where
--  get_token (SemiColon p) = Just (SemiColon p)
--  get_token _         = Nothing
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

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos

-- parsers para os não-terminais


program :: Parsec [Token] st [Token]
program = do
--    a <- 
--    b <- idToken 
    c <- main_
    eof
    return c

main_ :: Parsec [Token] st [Token]
main_ = do
  a <- typeToken
  b <- mainToken
  c <- beginBracketToken
--  d <- 
  e <- endBracketToken
  f <- block
  return ([b] ++ [c] ++ [e] ++ f)

block :: Parsec [Token] st [Token]
block = do
  a <- beginScopeToken
  b <- stmts
  c <- endScopeToken
  return ([a] ++ b ++ [c])

 
stmts :: Parsec [Token] st [Token]
stmts = (do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)) <|>
        (do 
          first <- declaration
          next <- remaining_stmts
          return (first ++ next))
          <|> (return [])

declaration :: Parsec [Token] st [Token]
declaration = (do
          a <- typeToken
          b <- idToken
          c <- optAssign
          d <- semiColonToken
          return ([a] ++ [b] ++ c ++ [d]))

optAssign :: Parsec [Token] st [Token]
optAssign  = (do
          a <- assignToken
          b <- expr
          return([a] ++ b)) <|> (return [])

expr :: Parsec [Token] st [Token]
expr  = (do 
        a <- simpleExpr
        return (a)) <|> (return [])

simpleExpr :: Parsec [Token] st [Token]
simpleExpr  = (do 
        a <- idToken
        return ([a]))

assign :: Parsec [Token] st [Token]
assign = do
          a <- idToken
          b <- assignToken
          c <- semiColonToken
          return ([a] ++ [b] ++ [c])

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do b <- stmts
                      return (b)) <|> (return [])
--program :: Parsec [Token] st [Token]
--program = do
--    a <- 
--    b <- idToken 
--    c <- main_
--    eof
--    return c

--main_ :: Parsec [Token] st [Token]
--main_ = do
--    a <- typeToken
--    b <- semiColonToken
--    return ([a] ++ [b])
--	a <- typeToken
--	b <- mainToken
--	c <- beginBracketToken
--	d <- 
--	e <- endBracketToken
--	f <- block
--	return (a ++ b ++ c ++ e ++ f)
{-}
block :: Parsec [Token] st [Token]
block = do
	a <- beginScopeToken
	--b <- stmts
	c <- endScopeToken
	return ([a] ++ [c])
-}

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "programaV0.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }