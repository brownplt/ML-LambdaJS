open Prelude
open Es5_syntax
module S = JavaScript_syntax

open Format
open FormatExt

let pretty_attr a = match a with
  | Writable -> ":writable"
  | Setter -> ":setter"
  | Getter -> ":getter"
  | Value -> ":value"
  | Config -> ":config"

let rec exp e = match e with
  | EConst (_, c) -> const c
  | EId (p, x) -> text x
  | EObject (p, internals, fields) ->
      parens (vert [text "object";
		    vert (map attr internals);
		    vert (map field fields)])
  | EUpdateFieldSurface (p, o, f, v, args) ->
      parens (horz 
		 [text "update-field";
		 exp o;
		 exp f;
		 exp v])
  | EGetFieldSurface (p, o, f, args) ->
      parens (horz 
		 [text "get-field";
		 exp o;
		 exp f;])
  | EUpdateField (p, o1, o2, f, v, args) ->
      parens (horz 
		 [text "update-field";
		 exp o1;
		 exp o2;
		 exp f;
		 exp v])
  | EGetField (p, o1, o2, f, args) ->
      parens (horz 
		 [text "get-field";
		 exp o1;
		 exp o2;
		 exp f;])
  | EDeleteField (p, o1, f) ->
      parens (horz
		 [text "delete-field";
		 exp o1;
		 exp f])
  | EAttr (p, a, o, f) ->
      parens (horz
		[text "attr-get";
		 text (pretty_attr a);
		 exp o;
		 exp f])
  | ESetAttr (p, a, o, f, v) ->
      parens (horz 
		[text "attr-get";
		 text (pretty_attr a);
		 exp o;
		 exp f;
		 exp v])
  | ESet (p, x, e) ->
      parens (horz
		[text "set!";
		 text x;
		 exp e])
  | EOp1 (p, op, e) -> 
      let op_string = op1_string op in
	parens (horz [text op_string; exp e])
  | EOp2 (p, op, e1, e2) ->
      let op_string = op2_string op in
	parens (horz [text op_string; exp e1; exp e2])
  | EOp3 (p, op, e1, e2, e3) ->
      let op_string = op3_string op in
	parens (horz [text op_string; exp e1; exp e2; exp e3])
  | EIf (p, c, t, e) -> 
      parens (vert [horz [text "if"; exp c];
		    exp t;
		    exp e;])
  | EApp (p, f, args) ->
      parens (horz (exp f :: map exp args))
  | ESeq (p, e1, e2) ->
      parens (vert [text "begin"; exp e1; exp e2])
  | ELet (p, x, e, body) ->
      parens (vert [horz [text "let"; parens (horz [text x; exp e])];
		    exp body])
  | EFix (p, x, e) -> 
      parens (horz [text "fix"; text x; exp e])
  | ELabel (p, l, e) ->
      parens (vert [text "label"; text l; exp e])
  | EBreak (p, l, e) ->
      parens (horz [text "break"; text l; exp e])
  | ETryCatch (p, body, catch) ->
      parens (vert [text "try-catch"; exp body; exp catch])
  | ETryFinally (p, body, fin) ->
      parens (vert [text "try-finally"; exp body; exp fin])
  | EThrow (p, e) ->
      parens (horz [text "throw"; exp e])
  | ELambda (p, xs, e) ->
      parens (vert [horz [text "lambda";
			  parens (horz (map text xs))];
		    exp e])

and op1_string op = match op with
  | Op1Prefix op_id -> op_id
  | Prim1 str -> "~" ^ str ^ "~"

and op2_string op = match op with
  | Op2Infix op_id -> op_id
  | Prim2 str -> "~" ^ str ^ "~"

and op3_string op = match op with
  | Op3Prefix op_id -> op_id
  | Prim3 str -> "~" ^ str ^ "~"
    
and attr (name, value) = parens (horz [text ("\"" ^ name ^ "\""); exp value])

and field (p, f, attrs) =
  brackets (vert (text ("\"" ^ f ^ "\"") :: map attr attrs))

and const c = match c with
  | S.CString (s) -> text ("\"" ^ s ^ "\"")
  | S.CRegexp (s, b1, b2) -> 
      parens (horz [text "regexp";
		    text s;
		    text (string_of_bool b1);
		    text (string_of_bool b2)])
  | S.CNum (d) -> text (string_of_float d)
  | S.CInt (d) -> text (string_of_int d)
  | S.CBool (b) -> text (string_of_bool b)
  | S.CNull -> text "null"
  | S.CUndefined -> text "undefined"
  
