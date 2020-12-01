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

type Variable = ([TypeVal], 	--Tipos e valores
				 Bool,		--É const?
				 String, 	--Escopo
				 Integer) 	--Contador de chamadas de uma função

type Func = (String,  --ID da função
	         [TypeVal],  --protocolo (cabeça é o retorno)
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
	              [TypeVal], 		--pilha de retorno
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

string_of_token :: Token -> String
string_of_token (Var _ c) = c

register_struct :: Token -> StateCode -> StateCode
register_struct id (a,b,c,d,e,f,g,h) = (a,b,c,d, (string_of_token id,[]):e, f,g,h)

add_struct_attribute :: (Token, Token) -> StateCode -> StateCode
add_struct_attribute (t, id) (a,b,c,d,(x,atrs):e,f,g,h) = 
	(a,b,c,d, (x, atrs++[(get_type_of t, string_of_token id)]):e, f,g,h)

--enter_function :: StateCode -> StateCode

get_type_of :: Token -> TypeVal
get_type_of (Type _ "int") = (Semantics.Int 0)
get_type_of (Type _ "float") = (Semantics.Float 0.0)

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