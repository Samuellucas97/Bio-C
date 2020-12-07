module Main (main) where
--module Parser where

import Data.Functor.Identity
import Lexer
import Text.Parsec
import Terminals
import Semantics

import System.Environment
import Data.List

import Control.Monad.IO.Class
import System.IO.Unsafe

--type var = (id, (type, valor), escopo )
--type func = (id, [param], return)
--type new_type = (id, [types])


--array = (id, size, type, [(type, value)])

-- program structure

program :: ParsecT [Token] StateCode IO([Token])
program = do
        a <- structs
        updateState(enable_execution)
        b <- declarations
        updateState(disable_execution)
        c <- functions
        updateState(enable_execution)
        d <- main_
        k <- getState
        liftIO(print k)
        eof
        return (a++b++c++d)

-- aux

extended_type :: ParsecT [Token] StateCode IO([Token])
extended_type = (do 
        a <- typeToken <|> idToken
        return ([a]))

extended_id :: ParsecT [Token] StateCode IO([Token])
extended_id = (do 
        a <- idToken
        b <- square_brackets
        c <- attribute_access
        return ([a] ++ b ++ c))

attribute_access :: ParsecT [Token] StateCode IO([Token])
attribute_access = (do 
        a <- dotToken
        b <- extended_id
        return ([a] ++ b)) <|> (return[])

-- structs

structs :: ParsecT [Token] StateCode IO([Token])
structs = (do
        first <- struct
        next <- remaining_struct
        return (first ++ next)) <|> (return [])

struct :: ParsecT [Token] StateCode IO([Token])
struct = (do
        a <- structToken
        b <- idToken
        updateState(register_struct b)
        c <- struct_block
        return([a]++[b]++c))

struct_block :: ParsecT [Token] StateCode IO([Token])
struct_block = do
        a <- beginScopeToken
        b <- attributes
        c <- endScopeToken
        return ([a] ++ b ++ [c])

attributes :: ParsecT [Token] StateCode IO([Token])
attributes = (do
        first <- attribute
        next <- remaining_attribute
        return (first ++ next)) -- <|> (return [])

attribute :: ParsecT [Token] StateCode IO([Token])
attribute = (do 
        a <- extended_type
        d <- square_brackets
        b <- idToken
        updateState(add_struct_attribute (head a,b))
        c <- semiColonToken
        return (a ++ d ++ [b] ++ [c]))

remaining_struct :: ParsecT [Token] StateCode IO([Token])
remaining_struct = (do 
        a <- structs
        return (a)) <|> (return [])

remaining_attribute :: ParsecT [Token] StateCode IO([Token])
remaining_attribute = (do 
        b <- attributes
        return (b)) <|> (return [])

square_brackets :: ParsecT [Token] StateCode IO([Token])
square_brackets = (do 
        a <- beginSquareBracketToken
        b <- square_brackets_values
        c <- endSquareBracketToken
        return ([a] ++ b ++ [c])) <|> (return [])

square_brackets_values :: ParsecT [Token] StateCode IO([Token])
square_brackets_values = (do
        first <- square_brackets_value
        next <- remaining_square_brackets_values
        return (first ++ next))

square_brackets_value :: ParsecT [Token] StateCode IO([Token])
square_brackets_value = (do 
        b <- expr
        return (b))

remaining_square_brackets_values :: ParsecT [Token] StateCode IO([Token])
remaining_square_brackets_values = (do 
        a <- commaToken
        b <- square_brackets_values
        return ([a] ++ b)) <|> (return [])

-- declarations

declarations :: ParsecT [Token] StateCode IO([Token])
declarations = try (do
        first <- declaration
        second <- semiColonToken
        next <- remaining_declaration
        return (first ++ [second] ++ next)) <|> (return [])

declaration :: ParsecT [Token] StateCode IO([Token])
declaration = try (do
        a <- varAssign <|> varDeclaration <|> constDeclaration
        return(a))

constDeclaration :: ParsecT [Token] StateCode IO([Token])
constDeclaration = do 
        a <- constToken
        b <- typeToken
        c <- idToken
        d <- assignToken
        e <- literal
        --f <- semiColonToken
        --return(a:b:c:d:e ++ [f])
        return ([a] ++ [b] ++ [c] ++ [d] ++ e)

literal :: ParsecT [Token] StateCode IO([Token])
literal = do 
        a <- intToken <|> floatToken <|> charToken  <|> booleanToken <|> stringToken <|> dnaToken <|> rnaToken <|> proteinToken 

        return ([a])

remaining_declaration :: ParsecT [Token] StateCode IO([Token])
remaining_declaration = (do a <- declarations
                            return (a)) <|> (return [])

main_ :: ParsecT [Token] StateCode IO([Token])
main_ = try (do
        a <- typeToken
        b <- mainToken
        c <- beginBracketToken
      --  d <- 
        e <- endBracketToken
        h <- block
        return ( [a] ++ [b] ++ [c] ++ [e] ++ h))

block :: ParsecT [Token] StateCode IO([Token])
block = do
        a <- beginScopeToken
        b <- stmts
        c <- endScopeToken
        return ([a] ++ b ++ [c])

 
stmts :: ParsecT [Token] StateCode IO([Token])
stmts = (do
          first <- stmt
          next <- remaining_stmts
          return (first ++ next)) <|> (return [])

stmt :: ParsecT [Token] StateCode IO([Token])
stmt = try (do
        a <- try function_call  <|> varAssign <|> varDeclaration <|> return_
        --liftIO(print a)
        b <- semiColonToken
        return (a ++ [b])) <|> (do
        a <- loop <|> condition

        return (a))

function_call :: ParsecT [Token] StateCode IO([Token])
function_call = (do
        a <- idToken
        b <- beginBracketToken
        c <- opt_args
        d <- endBracketToken
        --updateState(enter_in_function a)
        k <- getState
        if (is_executing k == 1) then do
          c_state <- getInput
          updateState(add_current_state c_state)
          --func_block <- 
          setInput(get_function_block a k)
          l <- block
          setInput c_state
          return ([a] ++ [b] ++ c ++ [d])
        else         
          return ([a] ++ [b] ++ c ++ [d])
        )

opt_args :: ParsecT [Token] StateCode IO([Token])
opt_args = (do
        a <- args
        return (a)) <|> (return [])

args :: ParsecT [Token] StateCode IO([Token])
args = (do
        a <- expr
        return (a)
        ) <|> (do 
        a <- expr
        b <- semiColonToken
        c <- args
        return(a ++ [b] ++ c))

remaining_stmts :: ParsecT [Token] StateCode IO([Token])
remaining_stmts = (do 
                  a <- stmts
                  return (a)) <|> (return [])

varDeclaration :: ParsecT [Token] StateCode IO([Token])
varDeclaration = try (do
          a <- extended_type
          d <- square_brackets
          b <- idToken
          c <- optAssign
          s <- getState
          --liftIO(print c)
          --if(is_executing s == 0) then do
          updateState(register_variable (head a) b)
          --liftIO(print s)
          if(is_executing s == 1) then do
            if(c /= [] && not (compatible(get_type a s ) ( get_type c s))) then fail "type mismatch"
              else do         --d <- semiColonToken 
                if(c == []) then do
                  return (a ++ d ++[b] ++ c)
                else do
                  updateState(update_variable_value [b] [(get_type c s)])
                  return (a ++ d ++[b] ++ c)
          else
            do
              return (a ++ d ++[b] ++ c))

get_type :: [Token] -> StateCode -> Token
get_type [(Type p1 id1)] _ = (Type p1 id1)
get_type [_,(Lexer.Int a b)] _ = (Lexer.Int a b)
get_type [_,(Lexer.Boolean a b)] _ = (Lexer.Boolean a b)
get_type [_,(Lexer.Float a b)] _ = (Lexer.Float a b)
get_type [_,(Lexer.String a b)] _ = (Lexer.String a b)
get_type (_:((Lexer.Var a b):t)) s = (get_function_type b s)
get_type _ _ = error "type not found"

compatible :: Token -> Token -> Bool
compatible (Type _ "int") (Lexer.Int _ _) = True
compatible (Type _ "float") (Lexer.Float _ _) = True
compatible (Type _ "float") (Lexer.Int _ _) = True
compatible (Type _ "boolean") (Lexer.Boolean _ _) = True
compatible (Type _ "string") (Lexer.String _ _) = True
compatible (Type _ "char") (Lexer.Char _ _) = True
compatible (Type _ "dna") (Lexer.Dna _ _) = True
compatible (Type _ "rna") (Lexer.Rna _ _) = True
compatible (Type _ "protein") (Lexer.Protein _ _) = True
compatible _ _ = False


optAssign :: ParsecT [Token] StateCode IO([Token])
optAssign  = (do 
          a <- assignToken
          b <- array_def <|> expr
          return ([a] ++ b)
          ) <|>
          (do
          a <- assign_type
          b <- expr
          return(a ++ b)) <|> (return [])

assign_type :: ParsecT [Token] StateCode IO([Token])
assign_type = (do 
          a  <- assignToken
          return([a]))

array_def :: ParsecT [Token] StateCode IO([Token])
array_def = (do 
          a <- beginSquareBracketToken
          b <- elements
          c <- endSquareBracketToken
          return ([a] ++ b ++ [c])
          )

elements :: ParsecT [Token] StateCode IO([Token])
elements = (do 
          a <- element
          b <- remaining_elements
          return (a ++ b))

remaining_elements :: ParsecT [Token] StateCode IO([Token])
remaining_elements = (do 
                  b <- commaToken
                  a <- elements
                  return ([b] ++ a)) <|> (return [])

element :: ParsecT [Token] StateCode IO([Token])
element = (do 
          a <- array_def <|> expr
          return (a))

expr :: ParsecT [Token] StateCode IO([Token])
expr  = try  bin_operation <|> un_operation <|> simpleExpr <|>  bracketExpr

bin_operation :: ParsecT [Token] StateCode IO([Token])
bin_operation  =  
        (do 
        a <- simpleExpr <|> bracketExpr
        result <- eval_remaining_bin a
        return (result))
        --b <- bin_operator
        --c <- expr
        --if (get_type a == (Var _ "int")) then eval_remaining_int a b c 
          --else error "TESTE")
          --else if 
        --return (a ++ b ++ c))

eval_remaining_bin :: [Token] -> ParsecT [Token] StateCode IO([Token])
eval_remaining_bin n1 = try (do
                      op <- multToken <|> divToken
                      n2 <- literal
                      result <- eval_remaining_bin (eval_bin n1 [op] n2)
                      --liftIO(print result)
                      return (result) )
                    <|>(do
                      op <- multToken <|> divToken
                      n2 <- idToken
                      s <- getState
                      result <- eval_remaining_bin (eval_bin n1 [op] (get_variable_value [n2] s))
                      --liftIO(print result)
                      return (result) )
                    <|>(do
                      op <- minusToken
                      n2 <- idToken
                      s <- getState
                      result <- eval_remaining_bin (eval_bin n1 [op] (get_variable_value [n2] s))
                      --liftIO(print result)
                      return (result) )
                    <|>(do
                                          op <- bin_operator
                                          n2 <- expr
                                          result <- eval_remaining_bin (eval_bin n1 op n2)
                                          --liftIO(print result)
                                          return (result) )
                    <|> (return (n1))       

eval_bin :: [Token] -> [Token] -> [Token] -> [Token]
eval_bin [(Lexer.Int p x)] [(Plus p1)] [(Lexer.Int _ y)] = [Lexer.Int p (x + y)]
eval_bin [(Lexer.Int p x)] [(Minus p1)] [(Lexer.Int _ y)] = [Lexer.Int p (x - y)]
eval_bin [(Lexer.Int p x)] [(Mult p1)] [(Lexer.Int _ y)] = [Lexer.Int p (x * y)]
eval_bin [(Lexer.Int p x)] [(Mod p1)] [(Lexer.Int _ y)] = [Lexer.Int p (mod x y)]
eval_bin [(Lexer.Int p x)] [(Div p1)] [(Lexer.Int _ y)] = [Lexer.Int p (div x y)]
eval_bin [(Lexer.Int p x)] [(Equal p1)] [(Lexer.Int _ y)] = [Lexer.Boolean p (x == y)]
eval_bin [(Lexer.Int p x)] [(Different p1)] [(Lexer.Int _ y)] = [Lexer.Boolean p (x /= y)]
eval_bin [(Lexer.Int p x)] [(GreaterOrEqual p1)] [(Lexer.Int _ y)] = [Lexer.Boolean p (x >= y)] 
eval_bin [(Lexer.Int p x)] [(LessOrEqual p1)] [(Lexer.Int _ y)] = [Lexer.Boolean p (x <= y)]
eval_bin [(Lexer.Int p x)] [(Greater p1)] [(Lexer.Int _ y)] = [Lexer.Boolean p (x > y)] 
eval_bin [(Lexer.Int p x)] [(Less p1)] [(Lexer.Int _ y)] = [Lexer.Boolean p (x < y)]
eval_bin [(Lexer.Float p x)] [(Plus p1)] [(Lexer.Float _ y)] = [Lexer.Float p (x + y)]
eval_bin [(Lexer.Float p x)] [(Minus p1)] [(Lexer.Float _ y)] = [Lexer.Float p (x - y)]
eval_bin [(Lexer.Float p x)] [(Mult p1)] [(Lexer.Float _ y)] = [Lexer.Float p (x * y)]
eval_bin [(Lexer.Float p x)] [(Div p1)] [(Lexer.Float _ y)] = [Lexer.Float p (x / y)]
eval_bin [(Lexer.Float p x)] [(Plus p1)] [(Lexer.Int _ y)] = [Lexer.Float p (x + fromIntegral y)] -- QUESTÃO 1 HACK
eval_bin [(Lexer.Int p x)] [(Plus p1)] [(Lexer.Float _ y)] = [Lexer.Float p (y + fromIntegral x)] 
eval_bin [(Lexer.Float p x)] [(Different p1)] [(Lexer.Float _ y)] = [Lexer.Boolean p (x /= y)]
eval_bin [(Lexer.Float p x)] [(Equal p1)] [(Lexer.Float _ y)] = [Lexer.Boolean p (x == y)]
eval_bin [(Lexer.Float p x)] [(GreaterOrEqual p1)] [(Lexer.Float _ y)] = [Lexer.Boolean p (x >= y)] 
eval_bin [(Lexer.Float p x)] [(LessOrEqual p1)] [(Lexer.Float _ y)] = [Lexer.Boolean p (x <= y)]
eval_bin [(Lexer.Float p x)] [(Greater p1)] [(Lexer.Float _ y)] = [Lexer.Boolean p (x > y)] 
eval_bin [(Lexer.Float p x)] [(Less p1)] [(Lexer.Float _ y)] = [Lexer.Boolean p (x < y)]
eval_bin [(Lexer.Boolean p x)] [(Different p1)] [(Lexer.Boolean _ y)] = [Lexer.Boolean p (x /= y)]
eval_bin [(Lexer.Boolean p x)] [(Equal p1)] [(Lexer.Boolean _ y)] = [Lexer.Boolean p (x == y)]


un_operation :: ParsecT [Token] StateCode IO([Token])
un_operation  =  
        (do 
        b <- un_operator
        --c <- expr
        result <- (eval_remaining_un b)
        return ([result]))
        --return (b ++ c))
eval_un :: Token -> [Token] -> Token
eval_un (Minus p1) [(Lexer.Int p y)] = (Lexer.Int p (-y))
eval_un (OpNot p1) [(Lexer.Boolean p y)] = (Lexer.Boolean p (not y))
eval_un (Minus p1) [(Lexer.Boolean p y)] = error ("Operador inválido para boolean")
eval_un (OpNot p1) [(Lexer.Int p y)] = error ("Operador inválido para inteiros")

eval_remaining_un :: Token -> ParsecT [Token] StateCode IO(Token)
eval_remaining_un n1 = try (do
                      c <- expr
                      result <- eval_remaining_un (eval_un n1 c)
                      return(result))
                    <|>return (n1) 

simpleExpr :: ParsecT [Token] StateCode IO([Token])
simpleExpr  = try (do 
        a <- try function_call
        return (a)) <|> (do 
        a <- try literal
        return (a)) <|> (do 
        a <- try extended_id
        s <- getState
        return (get_variable_value a s))
        --return (a))

varAssign :: ParsecT [Token] StateCode IO([Token])
varAssign = try (do
          a <- extended_id
          --b <- squareBrackets
          c <- assignToken
          d <- expr
          --e <- semiColonToken
          return (a ++ [c] ++ d))


un_operator :: ParsecT [Token] StateCode IO(Token)
un_operator  = (do 
        a <- notToken <|> minusToken
        return (a))

bin_operator :: ParsecT [Token] StateCode IO([Token])
bin_operator  = (do 
        a <- minusToken <|> plusToken <|> divToken <|> multToken <|> modToken <|> powToken <|> andToken <|> orToken <|> equalToken <|> differentToken <|> greaterToken <|> lessToken <|> greaterEqualToken <|> lessEqualToken
        return ([a]))

functions :: ParsecT [Token] StateCode IO([Token])
functions = (do
        first <- function
        next <- remaining_function
        return (first ++ next)) <|> (return [])

function :: ParsecT [Token] StateCode IO([Token])
function = try (do
        a <- extended_type
        b <- idToken
        updateState(register_function (head a, b))
        c <- beginBracketToken
        d <- params
        e <- endBracketToken
        --w <- getInput
        h <- block
        updateState(add_function_block h)
        return(a ++ [b] ++ [c] ++ d ++ [e] ++ h))

remaining_function :: ParsecT [Token] StateCode IO([Token])
remaining_function = (do 
                      a <- functions
                      return (a)) <|> (return [])


params :: ParsecT [Token] StateCode IO([Token])
params = (do
        first <- param
        next <- remaining_param
        return (first ++ next)) <|> (return [])

param :: ParsecT [Token] StateCode IO([Token])
param = (do
        a <- extended_type
        b <- sb_param
        c <- idToken
        updateState(add_function_param (head a, c))
        return(a ++ b ++ [c]))

remaining_param :: ParsecT [Token] StateCode IO([Token])
remaining_param = (do 
                  a <- commaToken
                  b <- params
                  return ([a] ++ b)) <|> (return [])


sb_param :: ParsecT [Token] StateCode IO([Token])
sb_param = (do 
            a <- beginSquareBracketToken
            b <- sb_param_values
            c <- endSquareBracketToken
            return ([a] ++ b ++ [c])) <|> (return [])

sb_param_values :: ParsecT [Token] StateCode IO([Token])
sb_param_values = (do
        first <- sb_param_value
        next <- remaining_sb_param_value
        return (first ++ next)) <|> (return [])

sb_param_value :: ParsecT [Token] StateCode IO([Token])
sb_param_value = (do
        a <- expr
        return(a)) <|> (return [])

remaining_sb_param_value :: ParsecT [Token] StateCode IO([Token])
remaining_sb_param_value = (do 
                            a <- commaToken
                            b <- sb_param_values
                            return ([a] ++ b)) <|> (return [])

loop :: ParsecT [Token] StateCode IO([Token])
loop = (do 
        a <- whileToken 
        b <- bracketExpr
        e <- block
        return ([a] ++ b ++ e))

condition :: ParsecT [Token] StateCode IO([Token])
condition = (do 
        a <- ifToken 
        b <- bracketExpr
        e <- block
        --f <- [] <|> elseBlock <|> elseToken ++ condition
        f <- opt_condition
        return ([a] ++ b ++ e ++ f))

opt_condition :: ParsecT [Token] StateCode IO([Token])
opt_condition = (do 
        a <- elseToken
        b <- block <|> condition
        return ([a] ++ b)) <|> (return [])

bracketExpr :: ParsecT [Token] StateCode IO([Token])
bracketExpr = (do 
        b <- beginBracketToken
        c <- expr
        d <- endBracketToken
        return ([b] ++ c ++ [d]))

return_ :: ParsecT [Token] StateCode IO([Token])
return_ = (do 
        a <- returnToken 
        b <- expr
        return ([a] ++ b))


-- invocação do parser para o símbolo de partida                       

parser :: [Token] -> IO(Either ParseError [Token])
parser tokens = runParserT program (0,[],[],[],[],[],[],0) "Error message" tokens


main :: IO ()
main = case unsafePerformIO (parser (getTokens "simple_program.pe")) of
            { Left err -> print err; 
              Right ans -> print "ans"
            }