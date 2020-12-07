module Semantics where

import Data.Functor.Identity
import Lexer
import Text.Parsec

import Control.Monad.IO.Class
import System.IO.Unsafe

import Data.Data


-- Flags de execução
-- 0 : Processando o código
-- 1 : Executando o código

data TypeVal = Int Int | Float Float deriving (Show)--(Show,Eq)

type Variable = (String,    --Id
				 [TypeVal], --Tipos e valores
				 Bool,		--É const?
				 String, 	--Escopo
				 String,    --Referência
				 Integer) 	--Contador de chamadas de uma função

type Func = (String,  --ID da função
	         [(TypeVal, String)],  --protocolo (cabeça é o retorno)
	         [Token], --Corpo da função
	         Integer) --Contador de chamadas da função

type Struct = (String,  --ID do Struct
			   [(TypeVal, String)])	--Atributos

type StateCode = (Integer,  	--flag de execução
	              [[Token]], 	--contador de programa
	              [Func], 		--funções
	              [Variable], 	--variáveis
	              [Struct], 	--structs
	              [String], 	--pilha de escopos
	              [TypeVal],	--pilha de retorno
	              Integer)		--flags auxiliares


{-
print_state :: StateCode -> StateCode
print_state a = --(do
			s <- getState
        	liftIO (print s)
        	return (a))
-}

enable_execution :: StateCode -> StateCode
enable_execution (a,b,c,d,e,f,g,h) = (1,b,c,d,e,f,g,h)

disable_execution :: StateCode -> StateCode
disable_execution (a,b,c,d,e,f,g,h) = (0,b,c,d,e,f,g,h)

--INTERNAL
string_of_token :: Token -> String
string_of_token (Var _ c) = c

register_struct :: Token -> StateCode -> StateCode
register_struct id (a,b,c,d,e,f,g,h) = 
	(a,b,c,d, (string_of_token id,[]):e, f,g,h)

--INTERNAL
add_struct_attribute :: (Token, Token) -> StateCode -> StateCode
add_struct_attribute (t, id) (a,b,c,d,(x,atrs):e,f,g,h) = 
	(a,b,c,d, (x, atrs++[(get_type_of t, string_of_token id)]):e, f,g,h)

--enter_function :: StateCode -> StateCode

--INTERNAL
get_type_of :: Token -> TypeVal
get_type_of (Type _ "int") = (Semantics.Int 0)
get_type_of (Type _ "float") = (Semantics.Float 0.0)

{-
tokens_to_type  :: [Token] -> [TypeVal] -> [TypeVal]
tokens_to_type [] x = x
tokens_to_type (h1:t1) x = tokens_to_type(t1, (get_type_of h1):x)
-}

register_function :: (Token, Token) -> StateCode -> StateCode
register_function x (a,b,c,d,e,f,g,h) = 
	(a,b,(create_function x):c,d,e,f,g,h)

--INTERNAL
create_function :: (Token, Token) -> Func
create_function (r,id) = 
	(string_of_token id, [(get_type_of r,"rfunc")],[],0)


add_function_param :: (Token, Token) -> StateCode -> StateCode
add_function_param x (a,b,c,d,e,f,g,h) = 
	(a,b,update_fuction_param x c,d,e,f,g,h)

--INTERNAL
update_fuction_param :: (Token, Token) -> [Func] -> [Func]
update_fuction_param (t,id) ((a,b,c,d):x) = 
	(a,b ++ [(get_type_of t, string_of_token id)],c,d):x


add_function_block :: [Token] -> StateCode -> StateCode
add_function_block x (a,b,c,d,e,f,g,h) = 
		(a,b,update_fuction_block x c,d,e,f,g,h)

--INTERNAL
update_fuction_block :: [Token] -> [Func] -> [Func]
update_fuction_block block ((a,b,_,d):x) = (a,b,block,d):x


get_function_block :: Token -> StateCode -> [Token]
get_function_block id (_,_,c,_,_,_,_,_) = 
	get_block_of_function(search_function (string_of_token id) c)

--INTERNAL
search_function :: String -> [Func] -> Func
search_function _ [] = error "function not found"
search_function id ((x,a,b,c):t) = 
	if (id == x) then (id,a,b,c)
	else search_function id t

--INTERNAL
get_block_of_function :: Func -> [Token]
get_block_of_function (_,_,b,_) = b


add_current_state :: [Token] -> StateCode -> StateCode
add_current_state x (a,b,c,d,e,f,g,h) = (a,x:b,c,d,e,f,g,h)

remove_current_state :: StateCode -> StateCode
remove_current_state (a,(k:t),c,d,e,f,g,h) = (a,t,c,d,e,f,g,h)

is_executing :: StateCode -> Integer
is_executing (i,_,_,_,_,_,_,_) = i

{-
enter_in_function :: Token -> StateCode -> StateCode
enter_in_function id (a,b,c,d,e,f,g,h) = (do
	x <- 2
	return (a,b,c,d,e,f,g,h)
	)




-}







--get_default_value :: [Token] -> Token
--get_default_value ((Type p "int"):t) = (Int p 0)

{-
symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]
-}



{-
symtable_insert :: (Token,Token) -> StateCode -> StateCode
symtable_insert symbol (True,[])  = (True,[symbol])
symtable_insert symbol (True,b) = (True,b ++ [symbol])
symtable_insert symbol (a,b) = (a,b)


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

-}