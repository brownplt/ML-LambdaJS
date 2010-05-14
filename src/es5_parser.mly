%{
open Prelude
open Es5_syntax
open JavaScript_syntax

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

%type <Es5_syntax.exp> prog
%type <Es5_syntax.exp -> Es5_syntax.exp> env

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

attr :
 | STRING COLON exp { ($1, $3) }
 | ID COLON exp { ($1, $3) }

attrs :
 | { [] }
 | attr { [$1] }
 | attr COMMA attrs { $1 :: $3 }

prop :
 | STRING COLON LBRACE attrs RBRACE { (($startpos, $endpos), $1, $4) }
 | ID COLON LBRACE attrs RBRACE { (($startpos, $endpos), $1, $4) }

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
 | LBRACE LBRACK attrs RBRACK props RBRACE 
   { EObject (($startpos, $endpos), $3, $5 )}
 | LBRACE seq_exp RBRACE
   { $2 }
 | LPAREN seq_exp RPAREN { $2 }
 | func { $1 }
 | FUNCTION LPAREN ids RPAREN LBRACE RETURN seq_exp RBRACE
     {
       let args = $3 in
       let body = $7 in
       let rec func_expr_lambda p ids body =
	 let folder id ix e = 
	   ELet (p, 
		 id,
		 EGetFieldSurface (p, 
				   EId (p, "args"),
				   EConst (p, CString (string_of_int ix))),
		 e) in
	   ELambda (p, 
		    ["this"; "args"],
		    List.fold_right2 folder ids (iota (List.length ids)) body) in
       let true_c p = EConst (p, CBool (true)) in 
       let false_c p = EConst (p, CBool (false)) in
       let p = ($startpos, $endpos) in
	 ELet (p, "$prototype", 
	       EObject (p,
			[("proto", EId (p, "Object_prototype"));
			 ("extensible", true_c p);
			 ("Class", EConst (p, CString ("Object")))],
			[(p, "constructor", 
				   [("value", EConst (p, CUndefined));
				    ("writable", true_c p);
				    ("enumerable", false_c p);
				    ("configurable", true_c p)])]),
		     ELet (p, "$funobj", 
			   EObject (p,
				    [("code", func_expr_lambda p args body);
				     ("proto", EId (p, "Function_prototype"));
				     ("extensible", true_c p)],
				    [(p,"length", 
				      [("value", EConst 
					  (p, 
					   CNum (float_of_int
						   (List.length args))));
				       ("writable", false_c p);
				       ("enumerable", false_c p);
				       ("configurable", false_c p)]);
				     (p,"prototype",
				      [("value", EId (p, "$prototype")); 
						("writable", true_c p);
						("configurable", false_c p);
						("enumerable", false_c p)])]),
			   ESeq (p, EUpdateFieldSurface (p, 
							 EId (p, "$prototype"),
							 EConst (p, CString ("constructor")),
						  EId (p, "$funobj")),
				 EId (p, "$funobj"))))
     }
 | TYPEOF atom
     { EOp1 (($startpos, $endpos), Prim1 "typeof", $2) }
     
exp :
 | atom { $1 }
 | exp LPAREN exps RPAREN 
   { EApp (($startpos, $endpos), $1, $3) }
 | PRIM LPAREN STRING COMMA seq_exp COMMA seq_exp COMMA seq_exp RPAREN
   { EOp3 (($startpos, $endpos), Prim3 $3, $5, $7, $9) }
 | PRIM LPAREN STRING COMMA seq_exp COMMA seq_exp RPAREN
   { EOp2 (($startpos, $endpos), Prim2 $3, $5, $7) }
 | PRIM LPAREN STRING COMMA seq_exp RPAREN
   { EOp1 (($startpos, $endpos), Prim1 $3, $5) }
 | ID COLONEQ exp
   { ESet (($startpos, $endpos), $1, $3) }
 | exp EQEQEQUALS exp
     { EOp2 (($startpos, $endpos), Prim2 "stx=", $1, $3) }
 | exp BANGEQEQUALS exp
     { let p = ($startpos, $endpos) in
         EIf (p, EOp2 (p, Prim2 "stx=", $1, $3),
              EConst (p, CBool false),
              EConst (p, CBool true)) }
 | exp LBRACK seq_exp EQUALS seq_exp RBRACK
   { EUpdateFieldSurface (($startpos, $endpos), $1, $3, $5) }
 | exp LBRACK seq_exp RBRACK
   { EGetFieldSurface (($startpos, $endpos), $1,  $3) }
 | exp LBRACK DELETE seq_exp RBRACK
   { EDeleteField (($startpos, $endpos), $1, $4) }

 | exp AMPAMP exp
     { EIf (($startpos, $endpos), $1, 
            $3,
            EConst (($startpos, $endpos), CBool false)) }
 | exp PIPEPIPE exp
     { let p = ($startpos, $endpos) in
         ELet (p, "%or", $1,
               EIf (p, EId (p, "%or"), EId (p, "%or"), $3)) }
 | FIX ID exp 
     { EFix (($startpos, $endpos), $2, $3) }


cexp :
 | exp { $1 }
 | IF LPAREN seq_exp RPAREN seq_exp ELSE seq_exp
     { EIf (($startpos, $endpos), $3, $5, $7) }
 | IF LPAREN seq_exp RPAREN seq_exp
     { EIf (($startpos, $endpos), $3, $5, 
	    EConst (($startpos, $endpos), CUndefined)) }
 | LABEL ID COLON seq_exp
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

env :
 | EOF
     { fun x -> x }
 | LET LLBRACK ID RRBRACK EQUALS seq_exp env
     { fun x -> 
         ELet (($startpos, $endpos), "[[" ^ $3 ^ "]]", rename_env $6, $7 x) }
 | LBRACE seq_exp RBRACE env
     { fun x -> ESeq (($startpos, $endpos), rename_env $2, $4 x) }

prog :
 | seq_exp EOF { $1 }
%%
