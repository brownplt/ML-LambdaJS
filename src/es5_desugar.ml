open Prelude
open Exprjs_syntax
open Es5_syntax
module S = JavaScript_syntax

type env = bool IdMap.t

let true_c p = EConst (p, S.CBool (true))
let false_c p = EConst (p, S.CBool (false))

let rec str p s = 
  EConst (p, S.CString (s))

let rec num_c p d = 
  EConst (p, S.CNum (d))

let rec int_c p d =
  EConst (p, S.CInt (d))

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

let rec func_expr_lambda p ids body =
  let folder id ix e = 
    ELet (p, 
	  id,
	  EGetField (p, 
		     EId (p, "args"), 
		     EId (p, "args"),
		     EConst (p, S.CString (string_of_int ix))),
	  e) in
    ELambda (p, 
	     ["this"; "args"],
	     List.fold_right2 folder ids (iota (List.length ids)) body)


(* Small extension to allow a named function to call itself.  I have
   not considered the repercussions of functions with the same name as
   an argument. *)

let rec func_stmt_lambda p func_name ids body =
    ELet (p, 
	  func_name, 
	  EId (p, "$funobj"),
	  func_expr_lambda p ids body)
	  
(* 13.2 *)
(* for strict mode add getter and setter errors for "caller" and
  "arguments" *)
let rec func_object p ids lambda_exp =
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
				   [("code", lambda_exp);
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
      EOp1 (p, Op1Prefix (op), ds e)

  | InfixExpr (p, op, e1, e2) ->
      EOp2 (p, Op2Infix (op), ds e1, ds e2)

  | IfExpr (p, c, t, e) ->
    EIf (p, ds c, ds t, ds e)

  | AppExpr (p, e, es) ->
    EApp (p, ds e, map ds es)

  | FuncExpr (p, ids, body) ->
    func_object p ids (func_expr_lambda p ids (ds body))

  | FuncStmtExpr (p, func_name, ids, body) ->
    func_object p ids (func_stmt_lambda p func_name ids (ds body))

  | LetExpr (p, x, e1, e2) -> ELet (p, x, ds e1, ds e2)

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
      ESetRef (p, x, ds e)
  | TryCatchExpr (p, body, x, catch) ->
      ETryCatch (p, ds body, ELambda (p, [x], ds catch))
  | TryFinallyExpr (p, body, fin) ->
      ETryFinally (p, ds body, ds fin)
  | ThrowExpr (p, e) ->
      EThrow (p, ds e)
  | HintExpr (p, e1, e2) -> str p "NYI---Hints"



let rec ds_op exp = match exp with
  | EOp1 (p, Op1Prefix op, e) -> begin match op with
      | "prefix:typeof" ->
	  EOp1 (p, Prim1 "surface-typeof", ds_op e)
      | "prefix:!" -> 
          EIf (p, EOp1 (p, Prim1 "prim->bool", ds_op e),
               false_c p,
               true_c p)
      | "prefix:~" ->
          EOp1 (p, Prim1 "~",
                EApp (p, EId (p, "[[toInt]]"), [ ds_op e ]))
      | "prefix:+" ->
          EApp (p, EId (p, "[[toNumber]]"), [ ds_op e ])
      | "prefix:-" ->
          EOp2 (p, Prim2 "-", 
                num_c p 0.0,
                EApp (p, EId (p, "[[toNumber]]"), [ ds_op e ]))
      | "prefix:typeof" ->
          EOp1 (p, Prim1 "surface-typeof", 
                EApp (p, EId (p, "[[getValue]]"), [ ds_op e ]))
      | "prefix:void" ->
          ESeq (p, e, EConst (p, S.CUndefined))
      | _ -> failwith ("unknown prefix operator: " ^ op)
    end
  | EOp2 (p, Op2Infix op, e1, e2) -> begin match op with
      | "*" -> numnum p op e1 e2
      | "/" -> numnum p op e1 e2
      | "%" -> numnum p op e1 e2
      | "-" -> numnum p op e1 e2
      | "&" -> int_int p op e1 e2
      | "|" -> int_int p op e1 e2
      | "^" -> int_int p op e1 e2
      | "<<" -> int_uint p op e1 e2
      | ">>" -> int_uint p op e1 e2
      | ">>>" -> uint_uint p op e1 e2
      | "<" -> EOp2 (p, Prim2 "<", ds_op e1, ds_op e2)
      | ">" -> EOp2 (p, Prim2 ">", ds_op e1, ds_op e2)
      | ">=" -> EIf (p, EOp2 (p, Prim2 "<", ds_op e1, ds_op e2),
		     false_c p, true_c p)
      | "<=" -> EIf (p, EOp2 (p, Prim2 ">", ds_op e1, ds_op e2),
		     false_c p, true_c p)
      | "instanceof" -> EApp (p, EId (p, "[[instanceof]]"), 
			      [ ds_op e1; ds_op e2])
      | "in" -> EApp (p, EId (p, "[[in]]"),
		      [ ds_op e1; ds_op e2])
	  (* The equality operators are implemented in \JS *)
      | "==" -> EOp2 (p, Prim2 "==",
		      ds_op e1, ds_op e2)
      | "!=" -> EIf (p, EOp2 (p, Prim2 "==",
			      ds_op e1, ds_op e2),
		     false_c p, true_c p)
      | "===" -> EOp2 (p, Prim2 "===",
		       ds_op e1, ds_op e2)
      | "!==" -> EIf (p, EOp2 (p, Prim2 "===",
			      ds_op e1, ds_op e2),
		     false_c p, true_c p)
	  (* 11.11 *)
      | "&&" -> ELet (p, "$lAnd", ds_op e1,
			EIf (p, EApp (p, EId (p, "[[toBoolean]]"), 
				      [ EId (p, "$lAnd") ]),
			     ds_op e2,
			     EId (p, "$lAnd")))
      | "||" -> ELet (p, "$lOr", ds_op e1,
			EIf (p, EApp (p, EId (p, "[[toBoolean]]"), 
				      [ EId (p, "$lOr") ]),
			     EId (p, "$lOr"),
			     ds_op e2))
      | "+" -> EApp (p, EId (p, "[[plus]]"), [ds_op e1; ds_op e2])
      | _ -> failwith ("unknown infix operator: " ^ op)
    end
  | EOp1 (p, Prim1 op, e) -> EOp1 (p, Prim1 op, ds_op e)
  | EOp2 (p, prim, e1, e2) -> EOp2 (p, prim, ds_op e1, ds_op e2)
  | EConst (p, c) -> EConst (p, c)
  | EId (p, x) -> EId (p, x)
  | EObject (p, internals, fields) -> 
      let ds_op_attr (name, value) = (name, ds_op value) in
      let ds_op_field (p, name, attrs) = (p, name, map ds_op_attr attrs) in
	EObject (p, map ds_op_attr internals, map ds_op_field fields)
  | EUpdateField (p, o1, o2, f, v) -> 
      EUpdateField (p, ds_op o1, ds_op o2, ds_op f, ds_op v)
  | EGetField (p, o1, o2, f) -> 
      EGetField (p, ds_op o1, ds_op o2, ds_op f)
  | EDeleteField (p, o, f) -> 
      EDeleteField (p, ds_op o, ds_op f)
  | ESetRef (p, x, v) -> 
      ESetRef (p, x, ds_op v)
  | EIf (p, c, t, e) -> 
      EIf (p, ds_op c, ds_op t, ds_op e)
  | EApp (p, func, args) -> 
      EApp (p, ds_op func, map ds_op args)
  | ESeq (p, e1, e2) -> 
      ESeq (p, ds_op e1, ds_op e2)
  | ELet (p, x, e1, body) -> 
      ELet (p, x, ds_op e1, ds_op body)
  | ELetAlloc (p, x, e1, body) ->
      ELetAlloc (p, x, ds_op e1, ds_op body)
  | EFix (p, x, e) -> EFix (p, x, ds_op e)
  | ELabel (p, l, e) -> ELabel (p, l, ds_op e)
  | EBreak (p, l, e) -> EBreak (p, l, ds_op e)
  | ETryCatch (p, body, catch) -> 
      ETryCatch (p, ds_op body, ds_op catch)
  | ETryFinally (p, body, fin) ->
      ETryFinally (p, ds_op body, ds_op fin)
  | EThrow (p, e) -> EThrow (p, ds_op e)
  | ELambda (p, ids, body) -> ELambda (p, ids, ds_op body)

and numnum p op e1 e2 = 
    EOp2 (p, Prim2 op, 
          EApp (p, EId (p, "[[toNumber]]"), [ ds_op e1 ]),
          EApp (p, EId (p, "[[toNumber]]"), [ ds_op e2 ]))

and int_int p op e1 e2 =
    EOp2 (p, Prim2 op, 
          EApp (p, EId (p, "[[toInt]]"), [ ds_op e1 ]),
          EApp (p, EId (p, "[[toInt]]"), [ ds_op e2 ]))

and int_uint p op e1 e2 = 
    EOp2 (p, Prim2 op, 
          EApp (p, EId (p, "[[toInt]]"), [ ds_op e1 ]),
          EApp (p, EId (p, "[[toUInt]]"), [ ds_op e2 ]))

and uint_uint p op e1 e2 = 
    EOp2 (p, Prim2 op, 
          EApp (p, EId (p, "[[toUInt]]"), [ ds_op e1 ]),
          EApp (p, EId (p, "[[toUInt]]"), [ ds_op e2 ]))
