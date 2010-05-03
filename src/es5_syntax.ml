open Prelude

type op1 = 
  | Op1Prefix of id
  | Prim1 of string

type op2 = 
  | Op2Infix of id
  | Prim2 of string

type exp =
  | EConst of pos * JavaScript_syntax.const
  | EId of pos * id
  | EObject of pos * (string * exp) list *
	       (pos * string * (string * exp) list) list
  | EUpdateField of pos * exp * exp * exp * exp
  | EGetField of pos * exp * exp * exp
  | EDeleteField of pos * exp * exp
  | ESetRef of pos * id * exp
  | EOp1 of pos * op1 * exp
  | EOp2 of pos * op2 * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
  | ESeq of pos * exp * exp
  | ELet of pos * id * exp * exp
  | ELetAlloc of pos * id * exp * exp
  | EFix of pos * id * exp * exp 
  | ELabel of pos * id * exp
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * exp
      (** Catch block must be an [ELambda] *)
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ELambda of pos * id list * exp

(******************************************************************************)

open Exprjs_syntax
module S = JavaScript_syntax

type env = bool IdMap.t

let true_c p = EConst (p, S.CBool (true))
let false_c p = EConst (p, S.CBool (false))

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
		 [("proto", 
		   EConst (p, S.CString ("Object.prototype")));
		("extensible", true_c p)],
		 (List.map mk_field (List.map ds_tuple exprs)))

  | ThisExpr (p) -> EId (p, "this")

  | VarExpr (p, x) -> EId (p, x)

  | IdExpr (p, x) -> EId (p, x)

  | BracketExpr (p, obj, f) ->
    ELet (p, "$x", to_object obj,
	  EGetField (p, EId (p,"$x"), EId (p,"$x"), to_string f))

  | AssignExpr (p1, VarLValue (p2, x), e) ->
    EOp2 (p1, SetRef, ((EId (p2, x)), ds e))

  | AssignExpr (p1, PropLValue (p2, obj, f), e) ->
    ELet (p, "$x", to_object obj,
	  ESetField (p, EId (p,"$x"), EId (p,"$x"), to_string f, ds e))

  (* 11.2.2 *)
  | NewExpr (p, e, args) ->
    ELet (p, "$constructor", ds e,
	  ELet (p, "$proto", 
		EGetField (p,
			   EId (p, "$constructor"),
			   EId (p, "$constructor"),
			   EString (p, "prototype")),
		ELetAlloc (p, "$newObj", new_obj p "$proto",
			   ELet (p, "$resObj", EApp (p, 
						     EId (p, "$constructor"),
						     (EId (p, "$newObj"))::
						     (map ds args))),
			   EIf (p, 
				EOp2 (p, 
				      Op2Infix ("==="),
				      EOp1 (p, 
					    Op1Prefix ("typeof"),
					    EId (p, "$resObj")),
				      EConst (p, S.CString ("Object"))),
				EId (p, "$resObj"),
				EId (p, "$newObj")))))
				
	  

  | PrefixExpr (p, op, e) ->
      EConst (p, S.CString ("NYI---Prefix"))

  | InfixExpr (p, op, e1, e2) ->
      EConst (p, S.CString ("NYI---Infix"))

  | IfExpr (p, c, t, e) ->
    EIf (p, ds c, ds t, ds e)

  | AppExpr (p, e, es) ->
    EApp (p, ds e, map ds es)

  | FuncExpr (p, ids, body) ->
    func_obj p ids (func_exp_body p ids (ds body))

  | FuncStmtExpr (p, func_name, args, body) ->
    func_obj p ids (func_stmt_body p func_name args (ds body))

  | LetExpr (p, x, e1, e2) -> ELet (p, x, ds e1, ds e2)

  | SeqExpr (p, e1, e2) -> ESeq (p, ds e1, ds e2)

  | WhileExpr (p, check, body) ->
    ELet (p, 
	  EId (p, "$check"), 
	  EFix (p, "$check", ELambda (p, [], ds check)),
	  EIf (p, 
	       EApp (p, EId (p, "$check")), 
	       ds body,
	       EUndefined (p)))

  | DoWhileExpr (p, check, body) ->
    let body_exp = ds body in
    ESeq (p, 
	  body_exp
              ELet (p, 
		    EId (p, "$check"), 
		    EFix (p, "$check", ELambda (p, [], ds check)),
			 EIf (p,
			      EApp (p, EId (p, "$check"), []), 
			      ds body,
			      EUndefined (p))))

  | LabelledExpr (p, l, e) ->
      ELabel (p, l, ds e)

  | BreakExpr (p, l, e) ->
      EBreak (p, l, ds e)

  | ForInExpr (p, x, obj, body) -> EConst (p, "NYI---ForInExpr")

  | VarDeclExpr (p, x, e) ->
      ELetAlloc (p, x, ds e)

  | TryCatchExpr (p, body, x, catch) ->
    ECatch (p, ds body, ELambda (p, [x], ds catch))

  | TryFinallyExpr (p, body, fin) ->
    EFinally (p, ds body, ds fin)

  | ThrowExpr (p, e) ->
    EThrow (p, ds e)

  | HintExpr (p, e1, e2) -> EConst (p, "NYI---Hints")
