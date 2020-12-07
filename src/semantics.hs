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

data TypeVal = Int Int | Float Float | String String | Boolean Bool |
               Char Char | Dna String | Rna String | Protein String deriving (Show, Eq)--(Show,Eq)

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

string_of_token :: Token -> String
string_of_token (Var _ c) = c

register_struct :: Token -> StateCode -> StateCode
register_struct id (a,b,c,d,e,f,g,h) = 
	(a,b,c,d, (string_of_token id,[]):e, f,g,h)

add_struct_attribute :: (Token, Token) -> StateCode -> StateCode
add_struct_attribute (t, id) (a,b,c,d,(x,atrs):e,f,g,h) = 
	(a,b,c,d, (x, atrs++[(get_type_of t, string_of_token id)]):e, f,g,h)

register_variable :: Token -> Token -> StateCode -> StateCode
register_variable t id (a,b,c,d,e,f,g,h) = 
    (a,b,c,((get_variable_from_type_id t id):d),e,f,g,h)

get_variable_from_type_id :: Token -> Token -> Variable
get_variable_from_type_id (Type p t) (Var p1 id) = (id,[get_type_of (Type p t)], False,"","",0)
--enter_function :: StateCode -> StateCode

get_variable_value :: [Token] -> StateCode-> [Token]
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Int t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.Int (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Float t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.Float (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Boolean t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.Boolean (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,[],e,f,g,h) = error "variable not found"

update_variable_value :: [Token] -> [Token] -> StateCode -> StateCode
update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.Int p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h)
update_variable_value [(Var p1 id)] [(Lexer.Float p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.Float p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h)
update_variable_value [(Var p1 id)] [(Lexer.Boolean p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.Boolean p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h) 
update_variable_value _ _ (a,b,c,[],e,f,g,h) = error "variable not found"

get_type_of :: Token -> TypeVal
get_type_of (Type _ "int") = (Semantics.Int 0)
get_type_of (Lexer.Int _ value) = (Semantics.Int value)
get_type_of (Type _ "float") = (Semantics.Float 0.0)
get_type_of (Lexer.Float _ value) = (Semantics.Float value)
get_type_of (Type _ "string") = (Semantics.String "")
get_type_of (Lexer.String _ value) = (Semantics.String value)
get_type_of (Type _ "boolean") = (Semantics.Boolean False)
get_type_of (Lexer.Boolean _ value) = (Semantics.Boolean value)
get_type_of (Type _ "char") = (Semantics.Char ' ')
get_type_of (Type _ "dna") = (Semantics.Dna "d:")
get_type_of (Type _ "rna") = (Semantics.Rna "r:")
get_type_of (Type _ "protein") = (Semantics.Protein "p:")

{-
tokens_to_type  :: [Token] -> [TypeVal] -> [TypeVal]
tokens_to_type [] x = x
tokens_to_type (h1:t1) x = tokens_to_type(t1, (get_type_of h1):x)
-}

register_function :: (Token, Token) -> StateCode -> StateCode
register_function x (a,b,c,d,e,f,g,h) = 
	(a,b,(create_function x):c,d,e,f,g,h)

create_function :: (Token, Token) -> Func
create_function (r,id) = 
	(string_of_token id, [(get_type_of r,"rfunc")],[],0)

get_function_type :: String -> StateCode -> Token
get_function_type id1 (a,b,(id2,(((Semantics.Int t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.Int (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type id1 (a,b,(id2,(((Semantics.String t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.String (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type _ (a,b,[],d,e,f,g,h) = error "variable not found"

add_function_param :: (Token, Token) -> StateCode -> StateCode
add_function_param x (a,b,c,d,e,f,g,h) = 
	(a,b,update_fuction_param x c,d,e,f,g,h)

update_fuction_param :: (Token, Token) -> [Func] -> [Func]
update_fuction_param (t,id) ((a,b,c,d):x) = 
	(a,b ++ [(get_type_of t, string_of_token id)],c,d):x


add_function_block :: [Token] -> StateCode -> StateCode
add_function_block x (a,b,c,d,e,f,g,h) = 
		(a,b,update_fuction_block x c,d,e,f,g,h)

update_fuction_block :: [Token] -> [Func] -> [Func]
update_fuction_block block ((a,b,_,d):x) = (a,b,block,d):x

is_executing :: StateCode -> Integer
is_executing (i,_,_,_,_,_,_,_) = i
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