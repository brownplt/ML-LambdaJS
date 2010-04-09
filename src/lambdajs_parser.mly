%{
open Prelude
open Lambdajs_syntax
open  JavaScript_syntax

(* All free variables "x" in the environment are renamed to "[[x]]" *)
let rename_env exp : exp  =
  let ren v exp = rename v ("[[" ^ v ^ "]]") exp in
  IdSet.fold ren (fv exp) exp
%}

%token <int> INT
%token <float> NUM
%token <string> STRING
%token <bool> BOOL
%token <Prelude.id> ID
%token UNDEFINED NULL FUNC LET DELETE LBRACE RBRACE LPAREN RPAREN LBRACK
  RBRACK EQUALS COMMA DEREF REF COLON COLONEQ PRIM IF ELSE SEMI
  LABEL BREAK TRY CATCH FINALLY THROW LLBRACK RRBRACK EQEQEQUALS TYPEOF
  AMPAMP PIPEPIPE RETURN BANGEQEQUALS FUNCTION FIX


%token EOF
%left COLONEQ
%left LPAREN
%left PIPEPIPE
%left AMPAMP
%left EQEQEQUALS BANGEQEQUALS
%left LBRACK

/* http://stackoverflow.com/questions/1737460/
   how-to-find-shift-reduce-conflict-in-this-yacc-file */

%type <Lambdajs_syntax.exp> prog
%type <Lambdajs_syntax.exp -> Lambdajs_syntax.exp> env

%start prog
%start env


%%

const :
 | NUM { JavaScript_syntax.CNum $1 }
 | INT {  JavaScript_syntax.CInt $1 }
 | STRING {  JavaScript_syntax.CString $1 }
 | UNDEFINED { JavaScript_syntax.CUndefined }
 | NULL { JavaScript_syntax.CNull }
 | BOOL { JavaScript_syntax.CBool $1 }

prop :
 | STRING COLON exp { (($startpos, $endpos), $1, $3) }
 | ID COLON exp { (($startpos, $endpos), $1, $3) }

props :
 | { [] }
 | prop { [$1] }
 | prop COMMA props { $1 :: $3 }

exps :
 | { [] }
 | seq_exp { [$1] }
 | seq_exp COMMA exps { $1 :: $3 }

ids :
 | { [] }
 | ID { [$1] }
 | ID COMMA ids { $1 :: $3 }


func :
 | FUNC LPAREN ids RPAREN LBRACE RETURN seq_exp RBRACE
   { ELambda (($startpos, $endpos), $3, $7) }

atom :
 | const { EConst (($startpos, $endpos), $1) }
 | ID { EId (($startpos, $endpos), $1) }
 | LBRACE props RBRACE 
   { EObject (($startpos, $endpos), $2) }
 | LBRACE seq_exp RBRACE
   { $2 }
 | LPAREN seq_exp RPAREN { $2 }
 | func { $1 }
 | FUNCTION LPAREN ids RPAREN LBRACE RETURN seq_exp RBRACE
   { let p = ($startpos, $endpos) in
     let args = $3 in
     let get_arg x n exp =
       ELet (p, x, 
             EOp1 (p, Ref,
                   EOp2 (p, GetField, EOp1 (p, Deref, EId (p, "arguments")),
                         EConst (p, CString (string_of_int n)))), exp) in
     let fields = 
       [ (p, "__code__", ELambda (p, ["this"; "arguments"], 
                                  List.fold_right2 get_arg args
                                    (iota (List.length args)) $7));
         (p, "arguments", EConst (p, CNull));
         (p, "prototype", EOp1 (p, Ref,
                                EObject 
                                  (p, [(p, "__proto__",
                                        EId (p, "Object_prototype"))])));
         (p, "__proto__", EId (p, "Function_prototype"));
         (p, "length", EConst (p, CInt (List.length args)));
         (p, "__string__", EConst (p, CString "[ builtin function ]")) ] in
       EOp1 (p, Ref, EObject (p, fields)) }
 | DEREF atom
   { EOp1 (($startpos, $endpos), Deref, $2) }
 | REF atom
   { EOp1 (($startpos, $endpos), Ref, $2) }
 | TYPEOF atom
     { EOp1 (($startpos, $endpos), Prim1 "typeof", $2) }


exp :
 | atom { $1 }
 | exp LPAREN exps RPAREN 
   { EApp (($startpos, $endpos), $1, $3) }
 | PRIM LPAREN STRING COMMA seq_exp COMMA seq_exp RPAREN
   { EOp2 (($startpos, $endpos), Prim2 $3, $5, $7) }
 | PRIM LPAREN STRING COMMA seq_exp RPAREN
   { EOp1 (($startpos, $endpos), Prim1 $3, $5) }
 | exp COLONEQ exp
   { EOp2 (($startpos, $endpos), SetRef, $1, $3) }
 | exp EQEQEQUALS exp
     { EOp2 (($startpos, $endpos), Prim2 "stx=", $1, $3) }
 | exp BANGEQEQUALS exp
     { let p = ($startpos, $endpos) in
         EIf (p, EOp2 (p, Prim2 "stx=", $1, $3),
              EConst (p, CBool false),
              EConst (p, CBool true)) }
 | exp LBRACK seq_exp EQUALS seq_exp RBRACK
   { EUpdateField (($startpos, $endpos), $1, $3, $5) }
 | exp LBRACK seq_exp RBRACK
   { EOp2 (($startpos, $endpos), GetField, $1, $3) }
 | exp LBRACK DELETE seq_exp RBRACK
   { EOp2 (($startpos, $endpos), DeleteField, $1, $4) }

 | exp AMPAMP exp
     { EIf (($startpos, $endpos), $1, 
            $3,
            EConst (($startpos, $endpos), CBool false)) }
 | exp PIPEPIPE exp
     { let p = ($startpos, $endpos) in
         ELet (p, "%or", $1,
               EIf (p, EId (p, "%or"), EId (p, "%or"), $3)) }


cexp :
 | exp { $1 }
 | IF LPAREN seq_exp RPAREN seq_exp ELSE seq_exp
     { EIf (($startpos, $endpos), $3, $5, $7) }
 | LABEL ID COLON cexp
     { ELabel (($startpos, $endpos), $2, $4) } 
 | BREAK ID cexp
   { EBreak (($startpos, $endpos), $2, $3) }
 | THROW cexp
   { EThrow (($startpos, $endpos), $2) }
 | TRY LBRACE seq_exp RBRACE CATCH LBRACE seq_exp RBRACE
   { ETryCatch (($startpos, $endpos), $3, $7) }
 | TRY LBRACE seq_exp RBRACE FINALLY LBRACE seq_exp RBRACE
   { ETryFinally (($startpos, $endpos), $3, $7) }

seq_exp :
 | cexp { $1 }
 | LET LPAREN ID EQUALS seq_exp RPAREN seq_exp
   { ELet (($startpos, $endpos), $3, $5, $7) }
 | cexp SEMI seq_exp
   { ESeq (($startpos, $endpos), $1, $3) }


fix_bind :
 | LLBRACK ID RRBRACK EQUALS func { ("[[" ^ $2 ^ "]]", rename_env $5) }

fix_binds :
 | { [] }
 | fix_bind fix_binds { $1 :: $2 }

env :
 | EOF
     { fun x -> x }
 | FIX fix_binds env 
     { fun x -> EFix (($startpos, $endpos), $2, $3 x) }
 | LET LLBRACK ID RRBRACK EQUALS seq_exp env
     { fun x -> 
         ELet (($startpos, $endpos), "[[" ^ $3 ^ "]]", rename_env $6, $7 x) }
 | LBRACE seq_exp RBRACE env
     { fun x -> ESeq (($startpos, $endpos), rename_env $2, $4 x) }

prog :
 | seq_exp EOF { $1 }
%%
