module Main (main) where
--module Parser where

import Data.Functor.Identity
import Lexer
import Text.Parsec
import Terminals

import System.Environment
import Data.List

import Control.Monad.IO.Class
import System.IO.Unsafe

type Cell = (Token, Token)

-- program structure

program :: ParsecT [Token] [Cell] IO([Token])
program = do
        a <- structs
        b <- declarations
        c <- functions
        d <- main_
        eof
        return (a++b++c++d)

-- aux

extended_type :: ParsecT [Token] [Cell] IO([Token])
extended_type = (do 
        a <- typeToken <|> idToken
        return ([a]))

extended_id :: ParsecT [Token] [Cell] IO([Token])
extended_id = (do 
        a <- idToken
        b <- square_brackets
        c <- attribute_access
        return ([a] ++ b ++ c))

attribute_access :: ParsecT [Token] [Cell] IO([Token])
attribute_access = (do 
        a <- dotToken
        b <- extended_id
        return ([a] ++ b)) <|> (return[])

-- structs

structs :: ParsecT [Token] [Cell] IO([Token])
structs = (do
        first <- struct
        next <- remaining_struct
        return (first ++ next)) <|> (return [])

struct :: ParsecT [Token] [Cell] IO([Token])
struct = (do
        a <- structToken
        b <- idToken
        c <- struct_block
        return([a]++[b]++c))

struct_block :: ParsecT [Token] [Cell] IO([Token])
struct_block = do
        a <- beginScopeToken
        b <- attributes
        c <- endScopeToken
        return ([a] ++ b ++ [c])

attributes :: ParsecT [Token] [Cell] IO([Token])
attributes = (do
        first <- attribute
        next <- remaining_attribute
        return (first ++ next)) -- <|> (return [])

attribute :: ParsecT [Token] [Cell] IO([Token])
attribute = (do 
        a <- extended_type
        d <- square_brackets
        b <- idToken
        c <- semiColonToken
        return (a ++ d ++ [b] ++ [c]))

remaining_struct :: ParsecT [Token] [Cell] IO([Token])
remaining_struct = (do 
        a <- structs
        return (a)) <|> (return [])

remaining_attribute :: ParsecT [Token] [Cell] IO([Token])
remaining_attribute = (do 
        b <- attributes
        return (b)) <|> (return [])

square_brackets :: ParsecT [Token] [Cell] IO([Token])
square_brackets = (do 
        a <- beginSquareBracketToken
        b <- square_brackets_values
        c <- endSquareBracketToken
        return ([a] ++ b ++ [c])) <|> (return [])

square_brackets_values :: ParsecT [Token] [Cell] IO([Token])
square_brackets_values = (do
        first <- square_brackets_value
        next <- remaining_square_brackets_values
        return (first ++ next))

square_brackets_value :: ParsecT [Token] [Cell] IO([Token])
square_brackets_value = (do 
        b <- expr
        return (b))

remaining_square_brackets_values :: ParsecT [Token] [Cell] IO([Token])
remaining_square_brackets_values = (do 
        a <- commaToken
        b <- square_brackets_values
        return ([a] ++ b)) <|> (return [])

-- declarations

declarations :: ParsecT [Token] [Cell] IO([Token])
declarations = try (do
        first <- declaration
        second <- semiColonToken
        next <- remaining_declaration
        return (first ++ [second] ++ next)) <|> (return [])

declaration :: ParsecT [Token] [Cell] IO([Token])
declaration = try (do
        a <- varAssign <|> varDeclaration <|> constDeclaration
        return(a))

constDeclaration :: ParsecT [Token] [Cell] IO([Token])
constDeclaration = do 
        a <- constToken
        b <- typeToken
        c <- idToken
        d <- assignToken
        e <- literal
        --f <- semiColonToken
        --return(a:b:c:d:e ++ [f])
        return ([a] ++ [b] ++ [c] ++ [d] ++ e)

literal :: ParsecT [Token] [Cell] IO([Token])
literal = do 
        a <- intToken <|> charToken  <|> booleanToken <|> stringToken <|> dnaToken <|> rnaToken <|> proteinToken 
        return ([a])

remaining_declaration :: ParsecT [Token] [Cell] IO([Token])
remaining_declaration = (do a <- declarations
                            return (a)) <|> (return [])

main_ :: ParsecT [Token] [Cell] IO([Token])
main_ = try (do
        a <- typeToken
        b <- mainToken
        c <- beginBracketToken
      --  d <- 
        e <- endBracketToken
        h <- block
        s <- getState
        liftIO (print s)
        return ( [a] ++ [b] ++ [c] ++ [e] ++ h))

block :: ParsecT [Token] [Cell] IO([Token])
block = do
        a <- beginScopeToken
        b <- stmts
        c <- endScopeToken
        return ([a] ++ b ++ [c])

 
stmts :: ParsecT [Token] [Cell] IO([Token])
stmts = (do
          first <- stmt
          next <- remaining_stmts
          return (first ++ next)) <|> (return [])

stmt :: ParsecT [Token] [Cell] IO([Token])
stmt = try (do
        a <- try function_call  <|> varAssign <|> varDeclaration <|> return_

        b <- semiColonToken
        return (a ++ [b])) <|> (do
        a <- loop <|> condition
        return (a))

function_call :: ParsecT [Token] [Cell] IO([Token])
function_call = (do
        a <- idToken
        b <- beginBracketToken
        c <- opt_args
        d <- endBracketToken
        return ([a] ++ [b] ++ c ++ [d])
        )

opt_args :: ParsecT [Token] [Cell] IO([Token])
opt_args = (do
        a <- args
        return (a)) <|> (return [])

args :: ParsecT [Token] [Cell] IO([Token])
args = (do
        a <- expr
        return (a)
        ) <|> (do 
        a <- expr
        b <- semiColonToken
        c <- args
        return(a ++ [b] ++ c))

remaining_stmts :: ParsecT [Token] [Cell] IO([Token])
remaining_stmts = (do 
                  a <- stmts
                  return (a)) <|> (return [])

varDeclaration :: ParsecT [Token] [Cell] IO([Token])
varDeclaration = try (do
          a <- extended_type
          d <- square_brackets
          b <- idToken
          updateState(symtable_insert (b, b))
          c <- optAssign
          --d <- semiColonToken
          return (a ++ d ++[b] ++ c))

optAssign :: ParsecT [Token] [Cell] IO([Token])
optAssign  = (do 
          a <- assignToken
          b <- array_def <|> expr
          return ([a] ++ b)
          ) <|>
          (do
          a <- assign_type
          b <- expr
          return(a ++ b)) <|> (return [])

assign_type :: ParsecT [Token] [Cell] IO([Token])
assign_type = (do 
          a  <- assignToken
          return([a]))

array_def :: ParsecT [Token] [Cell] IO([Token])
array_def = (do 
          a <- beginSquareBracketToken
          b <- elements
          c <- endSquareBracketToken
          return ([a] ++ b ++ [c])
          )

elements :: ParsecT [Token] [Cell] IO([Token])
elements = (do 
          a <- element
          b <- remaining_elements
          return (a ++ b))

remaining_elements :: ParsecT [Token] [Cell] IO([Token])
remaining_elements = (do 
                  b <- commaToken
                  a <- elements
                  return ([b] ++ a)) <|> (return [])

element :: ParsecT [Token] [Cell] IO([Token])
element = (do 
          a <- array_def <|> expr
          return (a))

expr :: ParsecT [Token] [Cell] IO([Token])
expr  = try bin_operation <|> un_operation <|> simpleExpr <|> bracketExpr

bin_operation :: ParsecT [Token] [Cell] IO([Token])
bin_operation  =  
        (do 
        a <- simpleExpr <|> bracketExpr
        b <- bin_operator
        c <- expr
        return (a ++ b ++ c))

un_operation :: ParsecT [Token] [Cell] IO([Token])
un_operation  =  
        (do 
        b <- un_operator
        c <- expr
        return (b ++ c))

simpleExpr :: ParsecT [Token] [Cell] IO([Token])
simpleExpr  = (do 
        a <- try function_call <|> extended_id
        return (a)) <|> (do 
        a <- literal
        return (a))

varAssign :: ParsecT [Token] [Cell] IO([Token])
varAssign = try (do
          a <- extended_id
          --b <- squareBrackets
          c <- assignToken
          d <- expr
          --e <- semiColonToken
          return (a ++ [c] ++ d))


un_operator :: ParsecT [Token] [Cell] IO([Token])
un_operator  = (do 
        a <- notToken <|> minusToken
        return ([a]))

bin_operator :: ParsecT [Token] [Cell] IO([Token])
bin_operator  = (do 
        a <- minusToken <|> plusToken <|> divToken <|> multToken <|> modToken <|> powToken <|> andToken <|> orToken <|> equalToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterEqualToken <|> lessEqualToken
        return ([a]))

functions :: ParsecT [Token] [Cell] IO([Token])
functions = (do
        first <- function
        next <- remaining_function
        return (first ++ next)) <|> (return [])

function :: ParsecT [Token] [Cell] IO([Token])
function = try (do
        a <- extended_type
        b <- idToken
        c <- beginBracketToken
        d <- params
        e <- endBracketToken
        h <- block
        return(a ++ [b] ++ [c] ++ d ++ [e] ++ h))

remaining_function :: ParsecT [Token] [Cell] IO([Token])
remaining_function = (do 
                      a <- functions
                      return (a)) <|> (return [])


params :: ParsecT [Token] [Cell] IO([Token])
params = (do
        first <- param
        next <- remaining_param
        return (first ++ next)) <|> (return [])

param :: ParsecT [Token] [Cell] IO([Token])
param = (do
        a <- extended_type
        b <- sb_param
        c <- idToken
        return(a ++ b ++ [c]))

remaining_param :: ParsecT [Token] [Cell] IO([Token])
remaining_param = (do 
                  a <- commaToken
                  b <- params
                  return ([a] ++ b)) <|> (return [])


sb_param :: ParsecT [Token] [Cell] IO([Token])
sb_param = (do 
            a <- beginSquareBracketToken
            b <- sb_param_values
            c <- endSquareBracketToken
            return ([a] ++ b ++ [c])) <|> (return [])

sb_param_values :: ParsecT [Token] [Cell] IO([Token])
sb_param_values = (do
        first <- sb_param_value
        next <- remaining_sb_param_value
        return (first ++ next)) <|> (return [])

sb_param_value :: ParsecT [Token] [Cell] IO([Token])
sb_param_value = (do
        a <- expr
        return(a)) <|> (return [])

remaining_sb_param_value :: ParsecT [Token] [Cell] IO([Token])
remaining_sb_param_value = (do 
                            a <- commaToken
                            b <- sb_param_values
                            return ([a] ++ b)) <|> (return [])

loop :: ParsecT [Token] [Cell] IO([Token])
loop = (do 
        a <- whileToken 
        b <- bracketExpr
        e <- block
        return ([a] ++ b ++ e))

condition :: ParsecT [Token] [Cell] IO([Token])
condition = (do 
        a <- ifToken 
        b <- bracketExpr
        e <- block
        --f <- [] <|> elseBlock <|> elseToken ++ condition
        f <- opt_condition
        return ([a] ++ b ++ e ++ f))

opt_condition :: ParsecT [Token] [Cell] IO([Token])
opt_condition = (do 
        a <- elseToken
        b <- block <|> condition
        return ([a] ++ b)) <|> (return [])

bracketExpr :: ParsecT [Token] [Cell] IO([Token])
bracketExpr = (do 
        b <- beginBracketToken
        c <- expr
        d <- endBracketToken
        return ([b] ++ c ++ [d]))

return_ :: ParsecT [Token] [Cell] IO([Token])
return_ = (do 
        a <- returnToken 
        b <- expr
        return ([a] ++ b))


-- invocação do parser para o símbolo de partida 

get_default_value :: [Token] -> Token
get_default_value ((Type p "int"):t) = (Int p 0)

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then (id1, v1) : t
                               else (id2, v2) : symtable_update (id1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove (id1, v1) t                               


parser :: [Token] -> IO(Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens


main :: IO ()
main = case unsafePerformIO (parser (getTokens "simple_program.pe")) of
            { Left err -> print err; 
              Right ans -> print ans
            }