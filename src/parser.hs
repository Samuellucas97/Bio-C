module Main (main) where
--module Parser where

import Data.Functor.Identity
import Lexer
import Text.Parsec
import Terminals

import System.Environment
import Data.List

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

extended_id :: Parsec [Token] st [Token]
extended_id = (do 
        a <- idToken
        b <- square_brackets
        c <- attribute_access
        return ([a] ++ b ++ c))

attribute_access :: Parsec [Token] st [Token]
attribute_access = (do 
        a <- dotToken
        b <- extended_id
        return ([a] ++ b)) <|> (return[])

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
        b <- expr
        return (b))

remaining_square_brackets_values :: Parsec [Token] st [Token]
remaining_square_brackets_values = (do 
        a <- commaToken
        b <- square_brackets_values
        return ([a] ++ b)) <|> (return [])

-- declarations

declarations :: Parsec [Token] st [Token]
declarations = try (do
        first <- declaration
        second <- semiColonToken
        next <- remaining_declaration
        return (first ++ [second] ++ next)) <|> (return [])

declaration :: Parsec [Token] st [Token]
declaration = try (do
        a <- varAssign <|> varDeclaration <|> constDeclaration
        return(a))

constDeclaration :: Parsec [Token] st [Token]
constDeclaration = do 
        a <- constToken
        b <- typeToken
        c <- idToken
        d <- assignToken
        e <- literal
        --f <- semiColonToken
        --return(a:b:c:d:e ++ [f])
        return ([a] ++ [b] ++ [c] ++ [d] ++ e)

literal :: Parsec [Token] st [Token]
literal = do 
        a <- intToken <|> charToken  <|> booleanToken <|> stringToken <|> dnaToken <|> rnaToken <|> proteinToken 
        return ([a])

remaining_declaration :: Parsec [Token] st [Token]
remaining_declaration = (do a <- declarations
                            return (a)) <|> (return [])

main_ :: Parsec [Token] st [Token]
main_ = try (do
        a <- typeToken
        b <- mainToken
        c <- beginBracketToken
      --  d <- 
        e <- endBracketToken
        h <- block
        return ( [a] ++ [b] ++ [c] ++ [e] ++ h))

block :: Parsec [Token] st [Token]
block = do
        a <- beginScopeToken
        b <- stmts
        c <- endScopeToken
        return ([a] ++ b ++ [c])

 
stmts :: Parsec [Token] st [Token]
stmts = (do
          first <- stmt
          next <- remaining_stmts
          return (first ++ next)) <|> (return [])

stmt :: Parsec [Token] st [Token]
stmt = try (do
        a <- try function_call  <|> varAssign <|> varDeclaration <|> return_

        b <- semiColonToken
        return (a ++ [b])) <|> (do
        a <- loop <|> condition
        return (a))

function_call :: Parsec [Token] st [Token]
function_call = (do
        a <- idToken
        b <- beginBracketToken
        c <- opt_args
        d <- endBracketToken
        return ([a] ++ [b] ++ c ++ [d])
        )

opt_args :: Parsec [Token] st [Token]
opt_args = (do
        a <- args
        return (a)) <|> (return [])

args :: Parsec [Token] st [Token]
args = (do
        a <- expr
        return (a)
        ) <|> (do 
        a <- expr
        b <- semiColonToken
        c <- args
        return(a ++ [b] ++ c))

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do 
                  a <- stmts
                  return (a)) <|> (return [])

varDeclaration :: Parsec [Token] st [Token]
varDeclaration = try (do
          a <- extended_type
          d <- square_brackets
          b <- idToken
          c <- optAssign
          --d <- semiColonToken
          return (a ++ d ++[b] ++ c))

optAssign :: Parsec [Token] st [Token]
optAssign  = (do 
          a <- assignToken
          b <- array_def <|> expr
          return ([a] ++ b)
          ) <|>
          (do
          a <- assign_type
          b <- expr
          return(a ++ b)) <|> (return [])

assign_type :: Parsec [Token] st [Token]
assign_type = (do 
          a  <- assignToken
          return([a]))

array_def :: Parsec [Token] st [Token]
array_def = (do 
          a <- beginSquareBracketToken
          b <- elements
          c <- endSquareBracketToken
          return ([a] ++ b ++ [c])
          )

elements :: Parsec [Token] st [Token]
elements = (do 
          a <- element
          b <- remaining_elements
          return (a ++ b))

remaining_elements :: Parsec [Token] st [Token]
remaining_elements = (do 
                  b <- commaToken
                  a <- elements
                  return ([b] ++ a)) <|> (return [])

element :: Parsec [Token] st [Token]
element = (do 
          a <- array_def <|> expr
          return (a))
{-
expr :: Parsec [Token] st [Token]
expr  = (do 
        a <- simpleExpr <|> bracketExpr
        return (a)) <|>  
        (do 
        a <- un_operator
        b <- expr
        return (a ++ b)) <|>  
        (do 
        a <- simpleExpr
        c <- remaining_expr
        return (a ++ c))

remaining_expr :: Parsec [Token] st [Token]
remaining_expr  = 
        (do 
        a <- simpleExpr
        b <- bin_operator
        c <- simpleExpr
        return (a++ b ++ c))
-}
expr :: Parsec [Token] st [Token]
expr  = try bin_operation <|> un_operation <|> simpleExpr <|> bracketExpr

bin_operation :: Parsec [Token] st [Token]
bin_operation  =  
        (do 
        a <- simpleExpr <|> bracketExpr
        b <- bin_operator
        c <- expr
        return (a ++ b ++ c))

un_operation :: Parsec [Token] st [Token]
un_operation  =  
        (do 
        b <- un_operator
        c <- expr
        return (b ++ c))

simpleExpr :: Parsec [Token] st [Token]
simpleExpr  = (do 
        a <- try function_call <|> extended_id
        return (a)) <|> (do 
        a <- literal
        return (a))

varAssign :: Parsec [Token] st [Token]
varAssign = try (do
          a <- extended_id
          --b <- squareBrackets
          c <- assignToken
          d <- expr
          --e <- semiColonToken
          return (a ++ [c] ++ d))


un_operator :: Parsec [Token] st [Token]
un_operator  = (do 
        a <- notToken <|> minusToken
        return ([a]))

bin_operator :: Parsec [Token] st [Token]
bin_operator  = (do 
        a <- minusToken <|> plusToken <|> divToken <|> multToken <|> modToken <|> powToken <|> andToken <|> orToken <|> equalToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterEqualToken <|> lessEqualToken
        return ([a]))

functions :: Parsec [Token] st [Token]
functions = (do
        first <- function
        next <- remaining_function
        return (first ++ next)) <|> (return [])

function :: Parsec [Token] st [Token]
function = try (do
        a <- extended_type
        b <- idToken
        c <- beginBracketToken
        d <- params
        e <- endBracketToken
        h <- block
        return(a ++ [b] ++ [c] ++ d ++ [e] ++ h))

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

loop :: Parsec [Token] st [Token]
loop = (do 
        a <- whileToken 
        b <- bracketExpr
        e <- block
        return ([a] ++ b ++ e))

condition :: Parsec [Token] st [Token]
condition = (do 
        a <- ifToken 
        b <- bracketExpr
        e <- block
        --f <- [] <|> elseBlock <|> elseToken ++ condition
        f <- opt_condition
        return ([a] ++ b ++ e ++ f))

opt_condition :: Parsec [Token] st [Token]
opt_condition = (do 
        a <- elseToken
        b <- block <|> condition
        return ([a] ++ b)) <|> (return [])

bracketExpr :: Parsec [Token] st [Token]
bracketExpr = (do 
        b <- beginBracketToken
        c <- expr
        d <- endBracketToken
        return ([b] ++ c ++ [d]))

return_ :: Parsec [Token] st [Token]
return_ = (do 
        a <- returnToken 
        b <- expr
        return ([a] ++ b))

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
{-
main :: IO ()
main = case parser (getTokens "programaV0.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }


-}

main :: IO ()
main = do 
  args <- getArgs
  case parser (getTokens (head args)) of
  {
    Left err -> print err; 
    Right ans -> print ans
  }