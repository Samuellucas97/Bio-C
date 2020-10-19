module Main (main) where
--module Parser where

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

defineToken :: ParsecT [Token] st Identity Token
defineToken = tokenPrim show update_pos get_token where
  get_token (Define p) = Just (Define p)
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

-- parsers para os não-terminais


program :: Parsec [Token] st [Token]
program = do
        a <- structs
        b <- declarations
        --c < functions
        d <- main_
        eof
        return (a++b++d)

structs :: Parsec [Token] st [Token]
structs = (do
        first <- struct
        next <- remaining_struct
        return (first ++ next)) <|> (return [])

struct :: Parsec [Token] st [Token]
struct = (do
        a <- structToken
        b <- idToken
        c <- struct_block
        return([a]++[b]++c))

struct_block :: Parsec [Token] st [Token]
struct_block = do
        a <- beginScopeToken
        b <- attributes
        c <- endScopeToken
        return ([a] ++ b ++ [c])

attributes :: Parsec [Token] st [Token]
attributes = (do
        first <- attribute
        next <- remaining_attribute
        return (first ++ next)) -- <|> (return [])

attribute :: Parsec [Token] st [Token]
attribute = (do 
        a <- typeToken <|> idToken
        b <- idToken
        c <- semiColonToken
        return ([a] ++ [b] ++ [c]))

remaining_struct :: Parsec [Token] st [Token]
remaining_struct = (do 
        a <- structs
        return (a)) <|> (return [])

remaining_attribute :: Parsec [Token] st [Token]
remaining_attribute = (do 
        b <- attributes
        return (b)) <|> (return [])

declarations :: Parsec [Token] st [Token]
declarations = (do
        first <- declaration
        next <- remaining_declaration
        return (first ++ next)) <|> (return [])

declaration :: Parsec [Token] st [Token]
declaration = (do
        a <- constDeclaration <|> varAssign 
        return(a))

constDeclaration :: Parsec [Token] st [Token]
constDeclaration = do 
        a <- defineToken
        b <- typeToken
        c <- idToken
        d <- assignToken
        e <- literal
        f <- semiColonToken
        --return(a:b:c:d:e ++ [f])
        return ([a] ++ [b] ++ [c] ++ [d] ++ e ++ [f])

literal :: Parsec [Token] st [Token]
literal = do 
        a <- intToken <|> charToken  <|> booleanToken <|> {- dnaToken <|> dnaToken <|> rnaToken <|> proteinToken <|> -} stringToken
        return ([a])

remaining_declaration :: Parsec [Token] st [Token]
remaining_declaration = (do a <- declarations
                            return (a)) <|> (return [])

main_ :: Parsec [Token] st [Token]
main_ = do
        a <- typeToken
        b <- mainToken
        c <- beginBracketToken
      --  d <- 
        e <- endBracketToken
        f <- block
        return ( [a] ++ [b] ++ [c] ++ [e] ++ f)

block :: Parsec [Token] st [Token]
block = do
        a <- beginScopeToken
        b <- stmts
        c <- endScopeToken
        return ([a] ++ b ++ [c])

 
stmts :: Parsec [Token] st [Token]
stmts = (do
          first <- varAssign
          next <- remaining_stmts
          return (first ++ next)) <|>
        (do 
          first <- varDeclaration
          next <- remaining_stmts
          return (first ++ next))
          <|> (return [])

varDeclaration :: Parsec [Token] st [Token]
varDeclaration = (do
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
        return ([a])) <|> (do 
        a <- literal
        return (a))

varAssign :: Parsec [Token] st [Token]
varAssign = do
          a <- idToken
          --b <- squareBrackets
          c <- assignToken
          d <- expr
          e <- semiColonToken
          return ([a] ++ [c] ++ d ++[e])

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do a <- stmts
                      return (a)) <|> (return [])
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