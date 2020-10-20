module Main (main) where
--module Parser where

import Data.Functor.Identity
import Lexer
import Text.Parsec
import Terminals

-- program structure

program :: Parsec [Token] st [Token]
program = do
        a <- structs
        b <- declarations
        --c < functions
        d <- main_
        eof
        return (a++b++d)

-- structs

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

square_brackets :: Parsec [Token] st [Token]
square_brackets = (do 
        a <- beginBracketToken
        b <- square_brackets_values
        c <- endBracketToken
        return ([a] ++ b ++ [c])) <|> (return [])

square_brackets_values :: Parsec [Token] st [Token]
square_brackets_values = (do
        first <- square_brackets_value
        next <- remaining_square_bracket_values
        return (first ++ next))

square_brackets_value :: Parsec [Token] st [Token]
square_brackets_value = (do 
        b <- idToken
        return (b)

remaining_square_brackets_values :: Parsec [Token] st [Token]
remaining_square_brackets_values = (do 
        a <- commaToken
        b <- square_brackets_values
        return ([a] ++ [b])) <|> (return [])

-- declarations

declarations :: Parsec [Token] st [Token]
declarations = (do
        first <- declaration
        next <- remaining_declaration
        return (first ++ next)) <|> (return [])

declaration :: Parsec [Token] st [Token]
declaration = (do
        a <- constDeclaration <|> varAssign <|> varDeclaration
        return(a))

constDeclaration :: Parsec [Token] st [Token]
constDeclaration = do 
        a <- constToken
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
        a <- defToken
        b <- mainToken
        c <- beginBracketToken
      --  d <- 
        e <- endBracketToken
        f <- colonToken
        g <- typeToken
        h <- block
        return ( [a] ++ [b] ++ [c] ++ [e] ++ [f] ++ [g] ++ h)

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