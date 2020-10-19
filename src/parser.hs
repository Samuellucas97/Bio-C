module Main (main) where

import Lexer
import Text.Parsec


-- parsers para os tokens

mainToken = tokenPrim show update_pos get_token where
  get_token Main    = Just Main
  get_token _       = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Var x) = Just (Var x)
  get_token _      = Nothing

beginScopeToken = tokenPrim show update_pos get_token where
  get_token BeginScope = Just BeginScope
  get_token _     = Nothing

endScopeToken = tokenPrim show update_pos get_token where
  get_token EndScope = Just EndScope
  get_token _   = Nothing

beginBracketToken = tokenPrim show update_pos get_token where
  get_token BeginBracket = Just BeginBracket
  get_token _     = Nothing

endBracketToken = tokenPrim show update_pos get_token where
  get_token EndBracket = Just EndBracket
  get_token _   = Nothing

semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token SemiColon = Just SemiColon
  get_token _         = Nothing

{-assignToken = tokenPrim show update_pos get_token where
  get_token Assign = Just Assign
  get_token _      = Nothing-}

intToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
  get_token _       = Nothing

{-returnToken = tokenPrim show update_pos get_token where
  get_token Return = Just Return
  get_token _     = Nothing-}

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
	b <- mainToken
	c <- beginBracketToken
--	d <- 
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
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

assign :: Parsec [Token] st [Token]
assign = do
          a <- intToken
          b <- idToken
          c <- semiColonToken
          return ([a] ++ [b] ++ [c])

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do b <- stmts
                      return (b)) <|> (return [])




-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "programaV0.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }
