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
        c <- functions
        d <- main_
        eof
        return (a++b++c++d)

-- aux

extended_type :: Parsec [Token] st [Token]
extended_type = (do 
        a <- typeToken <|> idToken
        return ([a]))

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
        a <- extended_type
        d <- square_brackets
        b <- idToken
        c <- semiColonToken
        return (a ++ d ++ [b] ++ [c]))

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
        a <- beginSquareBracketToken
        b <- square_brackets_values
        c <- endSquareBracketToken
        return ([a] ++ b ++ [c])) <|> (return [])

square_brackets_values :: Parsec [Token] st [Token]
square_brackets_values = (do
        first <- square_brackets_value
        next <- remaining_square_brackets_values
        return (first ++ next))

square_brackets_value :: Parsec [Token] st [Token]
square_brackets_value = (do 
        b <- idToken
        return ([b]))

remaining_square_brackets_values :: Parsec [Token] st [Token]
remaining_square_brackets_values = (do 
        a <- commaToken
        b <- square_brackets_values
        return ([a] ++ b)) <|> (return [])

-- declarations

declarations :: Parsec [Token] st [Token]
declarations = (do
        first <- declaration
        next <- remaining_declaration
        return (first ++ next)) <|> (return [])

declaration :: Parsec [Token] st [Token]
declaration = (do
        a <- varDeclaration <|> varAssign <|> constDeclaration
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
        a <- intToken <|> charToken  <|> booleanToken <|> stringToken {- dnaToken <|> dnaToken <|> rnaToken <|> proteinToken <|> -} 
        return ([a])

remaining_declaration :: Parsec [Token] st [Token]
remaining_declaration = (do a <- declarations
                            return (a)) <|> (return [])

main_ :: Parsec [Token] st [Token]
main_ = do
        b <- mainToken
        c <- beginBracketToken
      --  d <- 
        e <- endBracketToken
        f <- colonToken
        g <- typeToken
        h <- block
        return ( [b] ++ [c] ++ [e] ++ [f] ++ [g] ++ h)

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



functions :: Parsec [Token] st [Token]
functions = (do
        first <- function
        next <- remaining_function
        return (first ++ next)) <|> (return [])

function :: Parsec [Token] st [Token]
function = (do
        a <- defToken
        b <- idToken
        c <- beginBracketToken
        d <- params
        e <- endBracketToken
        f <- colonToken
        g <- extended_type
        h <- block
        return([a] ++ [b] ++ [c] ++ d ++ [e] ++ [f] ++ g ++ h))

remaining_function :: Parsec [Token] st [Token]
remaining_function = (do 
                      a <- functions
                      return (a)) <|> (return [])


params :: Parsec [Token] st [Token]
params = (do
        first <- param
        next <- remaining_param
        return (first ++ next)) <|> (return [])

param :: Parsec [Token] st [Token]
param = (do
        a <- extended_type
        b <- sb_param
        c <- idToken
        return(a ++ b ++ [c]))

remaining_param :: Parsec [Token] st [Token]
remaining_param = (do 
                  a <- commaToken
                  b <- params
                  return ([a] ++ b)) <|> (return [])


sb_param :: Parsec [Token] st [Token]
sb_param = (do 
            a <- beginSquareBracketToken
            b <- sb_param_values
            c <- endSquareBracketToken
            return ([a] ++ b ++ [c])) <|> (return [])

sb_param_values :: Parsec [Token] st [Token]
sb_param_values = (do
        first <- sb_param_value
        next <- remaining_sb_param_value
        return (first ++ next)) <|> (return [])

sb_param_value :: Parsec [Token] st [Token]
sb_param_value = (do
        a <- expr
        return(a)) <|> (return [])

remaining_sb_param_value :: Parsec [Token] st [Token]
remaining_sb_param_value = (do 
                            a <- commaToken
                            b <- sb_param_values
                            return ([a] ++ b)) <|> (return [])

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