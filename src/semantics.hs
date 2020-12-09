module Semantics where

import Data.Functor.Identity
import Lexer
import Genetics
import Text.Parsec

import Control.Monad.IO.Class
import System.IO.Unsafe

import Data.Data


-- Flags de execução
-- 0 : Processando o código
-- 1 : Executando o código

data TypeVal = Int Int | Float Float | String String | Boolean Bool |
               Char Char | Dna String | Rna String | Protein String | Array [TypeVal] deriving (Show, Eq)--(Show,Eq)

type Variable = (String,    --Id
                 [TypeVal], --Tipos e valores
                 Bool,      --É const?
                 String,    --Escopo
                 String,    --Referência
                 Integer)   --Contador de chamadas de uma função

type Func = (String,  --ID da função
             [(TypeVal, String)],  --protocolo (cabeça é o retorno)
             [Token], --Corpo da função
             Integer) --Contador de chamadas da função

type Struct = (String,              --ID do Struct
              [(TypeVal, String)])  --Atributos

type StateCode = (Integer,      --flag de execução
                  [[Token]],   --contador de programa
                  [Func],      --funções
                  [Variable],  --variáveis
                  [Struct],    --structs
                  [String],    --pilha de escopos
                  [TypeVal],   --pilha de retorno
                  Integer)     --flags auxiliares


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

register_variable :: Token -> Token -> StateCode -> StateCode
register_variable t id (a,b,c,d,e,f,g,h) = 
    (a,b,c,((get_variable_from_type_id t id (head f)):d),e,f,g,h)

un_register_variable :: StateCode -> StateCode
un_register_variable (a,b,c,[],e,f,g,h) = (a,b,c,[],e,f,g,h)
un_register_variable (a,b,c,d,e,f,g,h) = (a,b,c,tail d,e,f,g,h)

get_variable_from_type_id :: Token -> Token -> String -> Variable
get_variable_from_type_id (Type p t) (Var p1 id) s = (id,[get_type_of (Type p t)], False,s,"",0)
--enter_function :: StateCode -> StateCode


get_variable_value :: [Token] -> StateCode-> [Token]
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Int t2):_,_,s,_,_):d,e,f,g,h) = 
    if(id == id1) then 
        if(s == head f) then do
            [(Lexer.Int (AlexPn 0 0 0) t2)]
        else 
            if(head f == "print" && s == head(tail f)) then do
                [(Lexer.Int (AlexPn 0 0 0) t2)]
            else
                get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
                --error "variable not in scope"
    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Float t2):_,_,s,_,_):d,e,f,g,h) = 
    if(id == id1) then 
        if(s == head f) then do
            [(Lexer.Float (AlexPn 0 0 0) t2)]
        else 
            if(head f == "print" && s == head(tail f)) then do
                [(Lexer.Float (AlexPn 0 0 0) t2)]
            else
                get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
                --error "variable not in scope"
    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Boolean t2):_,_,s,_,_):d,e,f,g,h) = 
    if(id == id1) then 
        if(s == head f) then do
            [(Lexer.Boolean (AlexPn 0 0 0) t2)]
        else 
            if(head f == "print" && s == head(tail f)) then do
                [(Lexer.Boolean (AlexPn 0 0 0) t2)]
            else
                get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
                --error "variable not in scope"
    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.String t2):_,_,s,_,_):d,e,f,g,h) = 
    if(id == id1) then 
        if(s == head f) then do
            [(Lexer.String (AlexPn 0 0 0) t2)]
        else 
            if(head f == "print" && s == head(tail f)) then do
                [(Lexer.String (AlexPn 0 0 0) t2)]
            else
                get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
                --error "variable not in scope"
    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Char t2):_,_,s,_,_):d,e,f,g,h) = 
    if(id == id1) then 
        if(s == head f) then do
            [(Lexer.Char (AlexPn 0 0 0) t2)]
        else 
            if(head f == "print" && s == head(tail f)) then do
                [(Lexer.Char (AlexPn 0 0 0) t2)]
            else
                get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
                --error "variable not in scope"
    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Protein t2):_,_,s,_,_):d,e,f,g,h) = 
    if(id == id1) then 
        if(s == head f) then do
            [(Lexer.Protein (AlexPn 0 0 0) t2)]
        else 
            if(head f == "print" && s == head(tail f)) then do
                [(Lexer.Protein (AlexPn 0 0 0) t2)]
            else
                get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
                --error "variable not in scope"
    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Rna t2):_,_,s,_,_):d,e,f,g,h) = 
    if(id == id1) then 
        if(s == head f) then do
            [(Lexer.Rna (AlexPn 0 0 0) t2)]
        else 
            if(head f == "print" && s == head(tail f)) then do
                [(Lexer.Rna (AlexPn 0 0 0) t2)]
            else
                get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
                --error "variable not in scope"
    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Dna t2):_,_,s,_,_):d,e,f,g,h) = 
    if(id == id1) then 
        if(s == head f) then do
            [(Lexer.Dna (AlexPn 0 0 0) t2)]
        else 
            if(head f == "print" && s == head(tail f)) then do
                [(Lexer.Dna (AlexPn 0 0 0) t2)]
            else
                get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
                --error "variable not in scope"
    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
{-get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Float t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.Float (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Boolean t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.Boolean (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Dna t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.Dna (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Rna t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.Rna (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.Protein t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.Protein (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,(id1,(Semantics.String t2):_,_,_,_,_):d,e,f,g,h) = if(id == id1) then [(Lexer.String (AlexPn 0 0 0) t2)]
                    else get_variable_value [(Var p1 id)] (a,b,c,d,e,f,g,h)
get_variable_value [(Var p1 id)] (a,b,c,[],e,f,g,h) = error ("variable " ++id++ " not found " ++ (show p1))
-}

pre_update_variable :: [Token] -> StateCode ->  StateCode
pre_update_variable x s = puv x s []
    


puv :: [Token] -> StateCode -> [Variable] ->  StateCode
puv [(Var p1 id)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) rem =
    if(id == id1) then do
       (a,b,c,((id1,tp,co,es,ref,cont):d ++ rem),e,f,g,h)
    else
       puv [(Var p1 id)] (a,b,c,d,e,f,g,h) ([(id1,tp,co,es,ref,cont)]++rem)
puv [(Var p1 id)] (a,b,c,[],e,f,g,h) _ = error (("variable " ++ id ++ " not found") ++ (show p1))

update_variable_value :: [Token] -> [Token] -> StateCode -> StateCode
update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            --if(f /= [] && es == head f) then do
                        if(id == id1) then do
                            if(es == head f) then
                                (a,b,c,([(id1,[get_type_of (Lexer.Int p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else 
                                error ("variable a not found " ++ (show p1))
                                --update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,d,e,f,g,h)
                                --error("variable not in scope")
                        else error ("variable a not found " ++ (show p1))
update_variable_value [(Var p1 id)] [(Lexer.Float p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            --if(f /= [] && es == head f) then do
                        if(id == id1) then do
                            if(es == head f) then
                                (a,b,c,([(id1,[get_type_of (Lexer.Float p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else 
                                error ("variable a not found " ++ (show p1))
                                --update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,d,e,f,g,h)
                                --error("variable not in scope")
                        else error ("variable a not found " ++ (show p1))
update_variable_value [(Var p1 id)] [(Lexer.Boolean p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            --if(f /= [] && es == head f) then do
                        if(id == id1) then do
                            if(es == head f) then
                                (a,b,c,([(id1,[get_type_of (Lexer.Boolean p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else 
                                error ("variable a not found " ++ (show p1))
                                --update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,d,e,f,g,h)
                                --error("variable not in scope")
                        else error ("variable a not found " ++ (show p1))
update_variable_value [(Var p1 id)] [(Lexer.String p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            --if(f /= [] && es == head f) then do
                        if(id == id1) then do
                            if(es == head f) then
                                (a,b,c,([(id1,[get_type_of (Lexer.String p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else 
                                error ("variable a not found " ++ (show p1))
                                --update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,d,e,f,g,h)
                                --error("variable not in scope")
                        else error ("variable a not found " ++ (show p1))
update_variable_value [(Var p1 id)] [(Lexer.Dna p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            --if(f /= [] && es == head f) then do
                        if(id == id1) then do
                            if(es == head f) then
                                (a,b,c,([(id1,[get_type_of (Lexer.Dna p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else 
                                error ("variable a not found " ++ (show p1))
                                --update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,d,e,f,g,h)
                                --error("variable not in scope")
                        else error ("variable a not found " ++ (show p1))
update_variable_value [(Var p1 id)] [(Lexer.Rna p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            --if(f /= [] && es == head f) then do
                        if(id == id1) then do
                            if(es == head f) then
                                (a,b,c,([(id1,[get_type_of (Lexer.Rna p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else 
                                error ("variable a not found " ++ (show p1))
                                --update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,d,e,f,g,h)
                                --error("variable not in scope")
                        else error ("variable a not found " ++ (show p1))
update_variable_value [(Var p1 id)] [(Lexer.Protein p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            --if(f /= [] && es == head f) then do
                        if(id == id1) then do
                            if(es == head f) then
                                (a,b,c,([(id1,[get_type_of (Lexer.Protein p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else 
                                error ("variable a not found " ++ (show p1))
                                --update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,d,e,f,g,h)
                                --error("variable not in scope")
                        else error ("variable a not found " ++ (show p1))
                            --update_variable_value [(Var p1 id)] [(Lexer.Int p value)] (a,b,c,d,e,f,g,h)
                            --else error ("variable not in add_scope" ++ show p1)
{-update_variable_value [(Var p1 id)] [(Lexer.Float p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.Float p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h)
update_variable_value [(Var p1 id)] [(Lexer.Boolean p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.Boolean p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h) 
update_variable_value [(Var p1 id)] [(Lexer.String p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.String p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h) 
update_variable_value [(Var p1 id)] [(Lexer.Dna p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.Dna p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h) 
update_variable_value [(Var p1 id)] [(Lexer.Rna p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.Rna p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h)
update_variable_value [(Var p1 id)] [(Lexer.Protein p value)] (a,b,c,((id1,tp,co,es,ref,cont):d),e,f,g,h) = 
                            if(id == id1) then (a,b,c,([(id1,[get_type_of (Lexer.Protein p value)],co,es,ref,cont)]++d),e,f,g,h)
                            else (a,b,c,d,e,f,g,h)  -}
update_variable_value _ _ (a,b,c,[],e,f,g,h) = error "variable a not found"


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
get_type_of (Lexer.Char _ value) = (Semantics.Char value)
get_type_of (Type _ "dna") = (Semantics.Dna "d:")
get_type_of (Lexer.Dna _ value) = (Semantics.Dna value)
get_type_of (Type _ "rna") = (Semantics.Rna "r:")
get_type_of (Lexer.Rna _ value) = (Semantics.Rna value)
get_type_of (Type _ "protein") = (Semantics.Protein "p:")
get_type_of (Lexer.Protein _ value) = (Semantics.Protein value)
get_type_of a = error (show a)

get_variable_type :: String -> StateCode -> Token
get_variable_type id1 (a,b,c,(id2,[(Semantics.Int t2)],_,_,_,_):d,e,f,g,h) = if(id1 == id2) then (Lexer.Int (AlexPn 0 0 0) t2)
                                                else get_variable_type id1 (a,b,c,d,e,f,g,h)
get_variable_type id1 (a,b,c,(id2,[(Semantics.Float t2)],_,_,_,_):d,e,f,g,h) = if(id1 == id2) then (Lexer.Float (AlexPn 0 0 0) t2)
                                                else get_variable_type id1 (a,b,c,d,e,f,g,h)
get_variable_type id1 (a,b,c,(id2,[(Semantics.String t2)],_,_,_,_):d,e,f,g,h) = if(id1 == id2) then (Lexer.String (AlexPn 0 0 0) t2)
                                                else get_variable_type id1 (a,b,c,d,e,f,g,h)
get_variable_type id1 (a,b,c,(id2,[(Semantics.Boolean t2)],_,_,_,_):d,e,f,g,h) = if(id1 == id2) then (Lexer.Boolean (AlexPn 0 0 0) t2)
                                                else get_variable_type id1 (a,b,c,d,e,f,g,h)
get_variable_type id1 (a,b,c,(id2,[(Semantics.Dna t2)],_,_,_,_):d,e,f,g,h) = if(id1 == id2) then (Lexer.Dna (AlexPn 0 0 0) t2)
                                                else get_variable_type id1 (a,b,c,d,e,f,g,h)
get_variable_type id1 (a,b,c,(id2,[(Semantics.Rna t2)],_,_,_,_):d,e,f,g,h) = if(id1 == id2) then (Lexer.Rna (AlexPn 0 0 0) t2)
                                                else get_variable_type id1 (a,b,c,d,e,f,g,h)
get_variable_type id1 (a,b,c,(id2,[(Semantics.Protein t2)],_,_,_,_):d,e,f,g,h) = if(id1 == id2) then (Lexer.Protein (AlexPn 0 0 0) t2)
                                                else get_variable_type id1 (a,b,c,d,e,f,g,h)
get_variable_type id1 (a,b,c,[],e,f,g,h) = error ("variable "++id1 ++" not found")

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

get_function_type :: String -> StateCode -> Token
get_function_type id1 (a,b,(id2,(((Semantics.Int t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.Int (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type id1 (a,b,(id2,(((Semantics.String t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.String (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type id1 (a,b,(id2,(((Semantics.Boolean t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.Boolean (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type id1 (a,b,(id2,(((Semantics.Float t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.Float (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type id1 (a,b,(id2,(((Semantics.Dna t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.Dna (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type id1 (a,b,(id2,(((Semantics.Rna t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.Rna (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type id1 (a,b,(id2,(((Semantics.Protein t2),s):_),_,_):tail,d,e,f,g,h) = if(id1 == id2) then (Lexer.Protein (AlexPn 0 0 0) t2)
                                                else get_function_type id1 (a,b,tail,d,e,f,g,h)
get_function_type id1 (a,b,[],d,e,f,g,h) = error ("function "++id1 ++" not found")

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

is_reserved_function :: Token -> Bool
is_reserved_function x = 
    if (string_of_token x == "print") then True
    else if (string_of_token x == "read")  then True
       else False


add_scope :: Token -> StateCode -> StateCode
add_scope _ (0,b,c,d,e,y,g,h) = (0,b,c,d,e,y,g,h)
add_scope (Main _) (a,b,c,d,e,y,g,h) = (a,b,c,d,e,"main":y,g,h)
add_scope (Var _ "read") (a,b,c,d,e,y,g,h) = (a,b,c,d,e,"main":y,g,h)
add_scope (Var _ s) (a,b,c,d,e,y,g,h) = (a,b,c,d,e,s:y,g,h)
add_scope id (1,b,c,d,e,y,g,h) = (1,b,c,d,e,(string_of_token id):y,g,h)

add_scope_global :: StateCode -> StateCode
add_scope_global (a,b,c,d,e,y,g,h) = (a,b,c,d,e,"global":y,g,h)

remove_current_scope :: StateCode -> StateCode
remove_current_scope (a,b,c,d,e,[],g,h) = (a,b,c,d,e,[],g,h)
remove_current_scope (a,b,c,d,e,y,g,h) = (a,b,c,d,e,tail y,g,h)

--data OutValue = Int | String | Float deriving (show)

value_of_token :: Token -> IO()
value_of_token (Lexer.Int _ c) = putStrLn(show c)
value_of_token (Lexer.Float _ c) = putStrLn(show c)
value_of_token (Lexer.String _ c) = putStrLn(show c)
value_of_token (Lexer.Boolean _ c) = putStrLn(show c)
value_of_token (Lexer.Char _ c) = putStrLn(show c)
value_of_token (Lexer.Dna _ c) = putStrLn(show (tail(tail(c))))
value_of_token (Lexer.Rna _ c) = putStrLn(show (tail(tail(c))))
value_of_token (Lexer.Protein _ c) = putStrLn(show (tail(tail(c))))


boolean_of_expresison :: Token -> Bool
boolean_of_expresison (Lexer.Boolean _ c) = c


get_reserved_function :: Token -> Integer
get_reserved_function x = 
    if (string_of_token x == "print") then 1
    else 
       if(string_of_token x == "read") then 2
       else 0

{-
grf2 :: Token -> Integer
grf2 x = 
    if (string_of_token x == "read") then 0
    else 0
-}

execute_read :: [Token] -> String -> StateCode -> StateCode
execute_read x v sc = 
    update_read x v (get_variable_type (string_of_token (head x)) sc) sc
--execute_read x v sc = update_read x v (get_variable_type (string_of_token (head x)) sc) sc
    

update_read :: [Token] -> String -> Token -> StateCode -> StateCode
update_read x v (Lexer.Int _ _) sc = update_variable_value x [(Lexer.Int (AlexPn 0 0 0) (read v::Int))] sc
update_read x v (Lexer.Float _ _) sc = update_variable_value x [(Lexer.Float (AlexPn 0 0 0) (read v::Float))] sc
update_read x v (Lexer.Boolean _ _) sc = update_variable_value x [(Lexer.Boolean (AlexPn 0 0 0) (read v::Bool))] sc
update_read x v (Lexer.String _ _) sc = update_variable_value x [(Lexer.String (AlexPn 0 0 0) v)] sc
