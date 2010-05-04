open Prelude
open Exprjs_syntax
open Es5_syntax
module S = JavaScript_syntax

type env = bool IdMap.t

let true_c p = EConst (p, S.CBool (true))
let false_c p = EConst (p, S.CBool (false))

let rec str p s = 
  EConst (p, S.CString (s))

let to_object e = e
let to_string e = e

let rec mk_val p v =
    [("value", v);
     ("enumerable", true_c p);
     ("configurable", true_c p);
     ("writable", true_c p)]

let rec mk_array (p, exps) = 
  let mk_field n v = (p, string_of_int n, 
		      mk_val p v) in
      EObject (p, [("proto", EId (p, "Array.prototype"));
		   ("extensible", true_c p)],
	       List.map2 mk_field (iota (List.length exps)) exps)

let rec mk_field (p, s, e) =
    (p, s, mk_val p e)

let new_obj p proto_id = 
    EObject (p,
	     [("proto", EId (p, proto_id));
	      ("extensible", true_c p);
	      ("Class", true_c p)],
	     [])

(* Same idea as in original \JS --- use the args array *)
(* In strict mode, we aren't supposed to access the args array... *)

let rec func_exp_body p ids body =
  let folder id ix e = 
    ELet (p, 
	  id,
	  EGetField (p, 
		     EId (p, "args"), 
		     EId (p, "args"),
		     EConst (p, S.CString (string_of_int ix))),
	  e) in
    List.fold_right2 folder ids (iota (List.length ids)) body

(* Small extension to allow a named function to call itself.  I have
   not considered the repercussions of functions with the same name as
   an argument. *)

let rec func_stmt_body p func_name ids body =
    ELet (p, 
	  func_name, 
	  EId (p, "$funobj"),
	  func_exp_body p ids body)
	  
(* 13.2 *)
(* for strict mode add getter and setter errors for "caller" and
  "arguments" *)
let rec func_object p ids body_exp =
    ELetAlloc (p, "$prototype", 
	       EObject (p,
			[("proto", EId (p, "Object.prototype"));
			 ("extensible", true_c p);
			 ("Class", EConst (p, S.CString ("Object")))],
			[(p, "constructor", 
			  [("value", EConst (p, S.CUndefined));
			   ("writable", true_c p);
			   ("enumerable", false_c p);
			   ("configurable", true_c p)])]),
	       ELetAlloc (p, "$funobj", 
			  EObject (p,
				   [("code", ELambda (p, 
						      ["this"; "args"],
						      body_exp));
				    ("proto", EId (p, "Function.prototype"));
				    ("extensible", true_c p)],
				   [(p,"length", 
				     [("value", EConst (p, S.CNum
							  (float_of_int
							     (List.length ids))));
				      ("writable", false_c p);
				      ("enumerable", false_c p);
				      ("configurable", false_c p)]);
				    (p,"prototype",
				     [("value", EId (p, "$prototype")); 
				      ("writable", true_c p);
				      ("configurable", false_c p);
				      ("enumerable", false_c p)])]),
			  ESeq (p, EUpdateField (p, 
					      EId (p, "$prototype"),
					      EId (p, "$prototype"),
					      EConst (p, S.CString ("constructor")),
					      EId (p, "$funobj")),
				EId (p, "$funobj"))))

let rec ds expr =
    match expr with
  | ConstExpr (p,c) -> EConst (p,c)

  | ArrayExpr (p,expr_list) -> mk_array (p, map ds expr_list)

  | ObjectExpr (p,exprs) -> 
      let ds_tuple (p,s,e) = (p,s,ds e) in
	EObject (p, 
		 [("proto", str p "Object.prototype");
		  ("extensible", true_c p)],
		 List.map mk_field (List.map ds_tuple exprs))

  | ThisExpr (p) -> EId (p, "this")

  | VarExpr (p, x) -> EId (p, x)

  | IdExpr (p, x) -> EId (p, x)

  | BracketExpr (p, obj, f) ->
    ELet (p, "$x", to_object (ds obj),
	  EGetField (p, EId (p,"$x"), EId (p,"$x"), to_string (ds f)))

  | AssignExpr (p1, VarLValue (p2, x), e) ->
    ESetRef (p1, x, ds e)

  | AssignExpr (p1, PropLValue (p2, obj, f), e) ->
    ELet (p2, "$x", to_object (ds obj),
	  EUpdateField (p1, 
			EId (p2,"$x"), 
			EId (p2,"$x"), 
			to_string (ds f), ds e))

  (* 11.2.2 *)
  | NewExpr (p, e, args) ->
    ELet (p, "$constructor", ds e,
	  ELet (p, "$proto", 
		EGetField (p,
			   EId (p, "$constructor"),
			   EId (p, "$constructor"),
			   str p "prototype"),
		ELetAlloc (p,
			   "$newObj", 
			   new_obj p "$proto",
			   ELet (p, 
				 "$resObj", 
				 EApp (p, 
				       EId (p, "$constructor"),
				       (EId (p, "$newObj"))::
					 (map ds args)),
				 EIf (p, 
				      EOp2 (p, 
					    Op2Infix ("==="),
					    EOp1 (p, 
						  Op1Prefix ("typeof"),
					    EId (p, "$resObj")),
					    EConst (p, S.CString ("Object"))),
				      EId (p, "$resObj"),
				      EId (p, "$newObj"))))))
				
	  

  | PrefixExpr (p, op, e) ->
      EConst (p, S.CString ("NYI---Prefix"))

  | InfixExpr (p, op, e1, e2) ->
      EConst (p, S.CString ("NYI---Infix"))

  | IfExpr (p, c, t, e) ->
    EIf (p, ds c, ds t, ds e)

  | AppExpr (p, e, es) ->
    EApp (p, ds e, map ds es)

  | FuncExpr (p, ids, body) ->
    func_object p ids (func_exp_body p ids (ds body))

  | FuncStmtExpr (p, func_name, ids, body) ->
    func_object p ids (func_stmt_body p func_name ids (ds body))

  | LetExpr (p, x, e1, e2) -> ELet (p, x, ds e1, ds e2)

  | SeqExpr (p,
	     VarDeclExpr (pv, x, e),
	     e2) ->
      ELetAlloc (p, x, ds e, ds e2)

  | SeqExpr (p, e1, e2) -> ESeq (p, ds e1, ds e2)

  | WhileExpr (p, body, check) ->
    ELet (p, 
	  "$check", 
	  EFix (p, 
		"$check", 
		ELambda (p, [], 
			 EIf (p,
			      ds check,
			      ESeq (p, 
				    ds body,
				    EApp (p, EId (p, "$check"), [])),
			      EConst (p, S.CUndefined)))),
	  EApp (p, EId (p, "$check"), []))

  | DoWhileExpr (p, body, check) ->
    let body_exp = ds body in
    ESeq (p, body_exp,
	  ELet (p, "$check",
		EFix (p, "$check", 
		      ELambda (p, [], 
			       EIf (p, ds check,
				    ESeq (p, body_exp,
					  EApp (p, EId (p, "$check"), [])),
				    EConst (p, S.CUndefined)))),
		EApp (p, EId (p, "$check"), [])))

  | LabelledExpr (p, l, e) ->
      ELabel (p, l, ds e)

  | BreakExpr (p, l, e) ->
      EBreak (p, l, ds e)

  | ForInExpr (p, x, obj, body) -> str p "NYI---ForInExpr"

  | VarDeclExpr (p, x, e) ->
      ELetAlloc (p, x, ds e, EConst (p, S.CUndefined))

  | TryCatchExpr (p, body, x, catch) ->
    ETryCatch (p, ds body, ELambda (p, [x], ds catch))

  | TryFinallyExpr (p, body, fin) ->
    ETryFinally (p, ds body, ds fin)

  | ThrowExpr (p, e) ->
    EThrow (p, ds e)

  | HintExpr (p, e1, e2) -> str p "NYI---Hints"
