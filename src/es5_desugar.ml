open Prelude
open Exprjs_syntax
open Es5_syntax
module S = JavaScript_syntax

type env = bool IdMap.t

let true_c p = EConst (p, S.CBool (true))
let false_c p = EConst (p, S.CBool (false))

let undef_c p = EConst (p, S.CUndefined)

let str p s = 
  EConst (p, S.CString (s))

let num_c p d = 
  EConst (p, S.CNum (d))

let int_c p d =
  EConst (p, S.CInt (d))

let obj_proto p = EId (p, "[[Object_prototype]]")
let fun_proto p = EId (p, "[[Function_prototype]]")

let to_object p e = EApp (p, EId (p, "[[toObject]]"), [e])
let to_string p e = EApp (p, EId (p, "[[ToString]]"), [e])

let rec mk_val p v =
    [("value", v);
     ("enumerable", true_c p);
     ("configurable", true_c p);
     ("writable", true_c p)]

let rec mk_array (p, exps) = 
  let mk_field n v = (p, string_of_int n, 
		      mk_val p v) in
    EObject (p, [("proto", EId (p, "[[Array_prototype]]"));
		 ("extensible", true_c p)],
	     List.map2 mk_field (iota (List.length exps)) exps)

let rec mk_field (p, s, e) =
    (p, s, mk_val p e)

let args_obj p arg_list = 
  let mk_field n v = (p, string_of_int n, 
		      mk_val p v) in
  EObject 
    (p, [("proto", obj_proto p);
	 ("class", str p "Arguments");
	 ("extensible", false_c p)],
     ((p, "length", [("value", int_c p (List.length arg_list));
		     ("writable", false_c p);
		     ("enumerable", false_c p);
		     ("configurable", false_c p)]) ::
	(List.map2 mk_field (iota (List.length arg_list)) arg_list)))

let new_obj p proto_id = 
    EObject (p,
	     [("proto", EId (p, proto_id));
	      ("extensible", true_c p);
	      ("Class", str p "Object")],
	     [])

(* Same idea as in original \JS --- use the args array *)
(* In strict mode, we aren't supposed to access the args array... *)

let rec func_expr_lambda p ids body =
  let folder id ix e = 
    ELet (p, 
	  id,
	  EGetFieldSurface (p, 
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
    ELet (p, "$prototype", 
	  EObject (p,
		   [("proto", obj_proto p);
		    ("extensible", true_c p);
		    ("Class", EConst (p, S.CString ("Object")))],
		   [(p, "constructor", 
		     [("value", EConst (p, S.CUndefined));
		      ("writable", true_c p);
		      ("enumerable", false_c p);
		      ("configurable", true_c p)])]),
	  ELet (p, "$funobj", 
		EObject (p,
			 [("code", lambda_exp);
			  ("proto", fun_proto p);
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
		ESeq (p, EUpdateFieldSurface (p, 
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
		   [("proto", obj_proto p);
		    ("extensible", true_c p)],
		   List.map mk_field (List.map ds_tuple exprs))
	    
    | ThisExpr (p) -> EId (p, "this")
	
    | VarExpr (p, x) -> EId (p, x)
	
    | IdExpr (p, x) -> EId (p, x)

  | BracketExpr (p, obj, f) ->
      EGetFieldSurface (p, to_object p (ds obj), to_string p (ds f))

  | AssignExpr (p1, VarLValue (p2, x), e) ->
    ESet (p1, x, ds e)

  | AssignExpr (p1, PropLValue (p2, obj, f), e) ->
      EUpdateFieldSurface (p1, 
			   to_object p2 (ds obj),
			   to_string p2 (ds f), ds e)

  (* 11.2.2 *)
  | NewExpr (p, e, args) ->
    ELet (p, "$constructor", ds e,
	  ELet (p, "$proto", 
		EGetFieldSurface (p,
				  EId (p, "$constructor"),
				  str p "prototype"),
		ELet (p,
		      "$newObj", 
		      new_obj p "$proto",
		      ELet (p, 
			    "$resObj", 
			    EApp (p, 
				  EId (p, "$constructor"),
				  [EId (p, "$newObj");
				   args_obj p (map ds args)]),
			    EIf (p, 
				 EOp2 (p, 
				       Prim2 ("stx="),
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

  | AppExpr (p, BracketExpr (p', obj, prop), es) ->
      ELet (p, "$obj", ds obj,
	    ELet (p, "$fun", EGetFieldSurface (p', EId (p, "$obj"), ds prop),
		  EApp (p, EId (p', "$fun"), 
			[EId (p', "$obj"); args_obj p (map ds es)])))
	
  | AppExpr (p, func, es) ->
      EApp (p, ds func, map ds es)

  | FuncExpr (p, ids, body) ->
    func_object p ids (func_expr_lambda p ids (var_lift body))

  | FuncStmtExpr (p, func_name, ids, body) ->
    func_object p ids (func_stmt_lambda p func_name ids (var_lift body))

  | LetExpr (p, x, e1, e2) -> ELet (p, x, ds e1, ds e2)

  | SeqExpr (p, e1, e2) -> ESeq (p, ds e1, ds e2)

  | WhileExpr (p, check, body) ->
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
      ESet (p, x, ds e)
  | TryCatchExpr (p, body, x, catch) ->
      ETryCatch (p, ds body, ELambda (p, [x], ds catch))
  | TryFinallyExpr (p, body, fin) ->
      ETryFinally (p, ds body, ds fin)
  | ThrowExpr (p, e) ->
      EThrow (p, ds e)
  | HintExpr (p, e1, e2) -> str p "NYI---Hints"

and var_lift expr =
  let folder (p,id) e = 
    ELet (p, id, undef_c p, e) in
    List.fold_right folder (vars_in expr) (ds expr)

(* Collect all the vars (and their source locations) in the
   expression.  Recur inside everything that isn't a function.  We
   just collect the names, and add them as undefined, let-alloced
   values at the top level.  In desugaring, we turn the VarDeclExpr
   into an assignment statement. *)

and vars_in expr = match expr with
    (* remember the source loc of vars *)
  | VarDeclExpr (p,x,e) -> [(p,x)]
      (* don't go inside functions *)
  | FuncExpr (_, _, _) -> []
  | FuncStmtExpr (_,_,_,_) -> []
      (* the rest is boilerplate *)
  | ConstExpr (_, _) -> []
  | ArrayExpr (_, elts) -> List.concat (map vars_in elts)
  | ObjectExpr (_, fields) ->
      let field_vars (p,n,v) = vars_in v in
	List.concat (map field_vars fields)
  | ThisExpr (_) -> []
  | VarExpr (_,_) -> []
  | IdExpr (_,_) -> []
  | BracketExpr (_, o, f) -> List.concat (map vars_in [o; f])
  | NewExpr (_, o, args) -> List.concat (map vars_in (o :: args))
  | PrefixExpr (_, _, e) -> vars_in e
  | InfixExpr (_, _, e1, e2) -> List.concat (map vars_in [e1; e2])
  | IfExpr (_,c,t,e) -> List.concat (map vars_in [c; t; e])
  | AssignExpr (_,_,e) -> vars_in e
  | AppExpr (_, f, args) -> List.concat (map vars_in (f :: args))
  | LetExpr (_, _, e, body) -> List.concat (map vars_in [e; body])
  | SeqExpr (_, e1, e2) -> List.concat (map vars_in [e1; e2])
  | WhileExpr (_, e1, e2) -> List.concat (map vars_in [e1; e2])
  | DoWhileExpr (_, e1, e2) -> List.concat (map vars_in [e1; e2])
  | LabelledExpr (_, _, e) -> vars_in e
  | BreakExpr (_, _, e) -> vars_in e
  | ForInExpr (_, _, e1, e2) -> List.concat (map vars_in [e1; e2])
  | TryCatchExpr (_, e1, _, e2) -> List.concat (map vars_in [e1; e2])
  | TryFinallyExpr (_, e1, e2) -> List.concat (map vars_in [e1; e2])
  | ThrowExpr (_, e) -> vars_in e
  | HintExpr (_,_,_) -> []


let rec ds_op exp = match exp with
  | EOp1 (p, Op1Prefix op, e) -> begin match op with
      | "prefix:delete" -> begin match e with
	  | EGetFieldSurface (p, obj, field) ->
	      EDeleteField (p, obj, field)
	  | _ -> true_c p
	end
      | "prefix:!" -> 
          EIf (p, EOp1 (p, Prim1 "prim->bool", ds_op e),
               false_c p,
               true_c p)
      | "prefix:~" ->
          EOp1 (p, Prim1 "~",
                EApp (p, EId (p, "[[toInt]]"), [ ds_op e ]))
      | "prefix:+" ->
          EApp (p, EId (p, "[[ToNumber]]"), [ ds_op e ])
      | "prefix:-" ->
          EOp2 (p, Prim2 "-", 
                num_c p 0.0,
                EApp (p, EId (p, "[[ToNumber]]"), [ ds_op e ]))
      | "typeof" ->
          EOp1 (p, Prim1 "typeof", ds_op e)
      | "prefix:typeof" ->
          EOp1 (p, Prim1 "surface-typeof", ds_op e)
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
      | "==" -> EOp2 (p, Prim2 "abs=",
		      ds_op e1, ds_op e2)
      | "!=" -> EIf (p, EOp2 (p, Prim2 "==",
			      ds_op e1, ds_op e2),
		     false_c p, true_c p)
      | "===" -> EOp2 (p, Prim2 "stx=",
		       ds_op e1, ds_op e2)
      | "!==" -> EIf (p, EOp2 (p, Prim2 "stx=",
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
  | EOp3 (p, prim, e1, e2, e3) -> EOp3 (p, prim, ds_op e1, ds_op e2, ds_op e3)
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
  | EUpdateFieldSurface (p, o, f, v) -> 
      EUpdateFieldSurface (p, ds_op o, ds_op f, ds_op v)
  | EGetFieldSurface (p, o, f) -> 
      EGetFieldSurface (p, ds_op o, ds_op f)
  | EDeleteField (p, o, f) -> 
      EDeleteField (p, ds_op o, ds_op f)
  | ESet (p, x, v) -> 
      ESet (p, x, ds_op v)
  | EIf (p, c, t, e) -> 
      EIf (p, ds_op c, ds_op t, ds_op e)
  | EApp (p, func, args) -> 
      EApp (p, ds_op func, map ds_op args)
  | ESeq (p, e1, e2) -> 
      ESeq (p, ds_op e1, ds_op e2)
  | ELet (p, x, e1, body) -> 
      ELet (p, x, ds_op e1, ds_op body)
  | EFix (p, x, e) -> EFix (p, x, ds_op e)
  | ELabel (p, l, e) -> ELabel (p, l, ds_op e)
  | EBreak (p, l, e) -> EBreak (p, l, ds_op e)
  | ETryCatch (p, body, catch) -> 
      ETryCatch (p, ds_op body, ds_op catch)
  | ETryFinally (p, body, fin) ->
      ETryFinally (p, ds_op body, ds_op fin)
  | EThrow (p, e) -> EThrow (p, ds_op e)
  | ELambda (p, ids, body) -> ELambda (p, ids, ds_op body)
  | _ -> failwith "Unmatched in ds_op"

and numnum p op e1 e2 = 
    EOp2 (p, Prim2 op, 
          EApp (p, EId (p, "[[ToNumber]]"), [ ds_op e1 ]),
          EApp (p, EId (p, "[[ToNumber]]"), [ ds_op e2 ]))

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

let rec ds_global exp env = match exp with
  | EApp (p, EId (p', x), es) -> begin
      try
	if IdMap.find x env
	then EApp (p, EId (p', x), map (fun e -> ds_global e env) es)
	else ELet (p, "$funobj", EGetFieldSurface (p, EId (p, "[[global]]"), str p x),
		   EApp (p, EId (p, "$funobj"), 
			 [EId (p, "$funobj");
			  args_obj p (map (fun e -> ds_global e env) es)]))
      with Not_found -> ELet (p, "$funobj", EGetFieldSurface (p, EId (p, "[[global]]"), str p x),
		   EApp (p, EId (p, "$funobj"), 
			 [EId (p, "$funobj");
			  args_obj p (map (fun e -> ds_global e env) es)]))
    end
  | EApp (p, e, es) ->
      EApp (p, ds_global e env, map (fun e -> ds_global e env) es)
  | EId (p, x) -> begin
      try
	if IdMap.find x env 
	then EId (p, x)
	else EGetFieldSurface (p, EId (p, "[[global]]"), str p x)
      with Not_found ->
	EGetFieldSurface (p, EId (p, "[[global]]"), str p x)
    end
  | ESet (p, x, e) -> begin
      try
	if IdMap.find x env 
	then ESet (p, x, ds_global e env)
	else EUpdateFieldSurface (p, EId (p, "[[global]]"), str p x, ds_global e env)
      with Not_found ->
	EUpdateFieldSurface (p, EId (p, "[[global]]"), str p x, ds_global e env)
    end
  | ELambda (p, ids, e) ->
      let new_env = fold_left (fun env x -> IdMap.add x true env) env ids in
	ELambda (p, ids, ds_global e new_env)
  | ELet (p, x, e1, e2) ->
      ELet (p, x, ds_global e1 env, ds_global e2 (IdMap.add x true env))
  | EFix (p, x, e) -> 
      EFix (p, x, ds_global e (IdMap.add x true env))
  | EConst (p, c) -> exp
  | EObject (p, attrs, props) ->
      let attr (name, value) = (name, ds_global value env) in
      let prop (p, name, attrs) = (p, name, map attr attrs) in
	EObject (p, map attr attrs, map prop props)
  | EUpdateFieldSurface (p, o, f, e) ->
      EUpdateFieldSurface (p, ds_global o env, ds_global f env, ds_global e env)
  | EGetFieldSurface (p, o, f) ->
      EGetFieldSurface (p, ds_global o env, ds_global f env)
  | EUpdateField (p, o1, o2, f, e) ->
      EUpdateField (p, ds_global o1 env, ds_global o2 env, 
		    ds_global f env, ds_global e env)
  | EGetField (p, o1, o2, f) ->
      EGetField (p, ds_global o1 env, ds_global o2 env, ds_global f env)
  | EDeleteField (p, o, f) ->
      EDeleteField (p, ds_global o env, ds_global f env)
  | EOp1 (p, op, e) -> EOp1 (p, op, ds_global e env)
  | EOp2 (p, op, e1, e2) -> EOp2 (p, op, ds_global e1 env, ds_global e2 env)
  | EOp3 (p, op, e1, e2, e3) -> EOp3 (p, op, ds_global e1 env, 
				      ds_global e2 env,
				      ds_global e3 env)
  | EIf (p, c, t, e) -> 
      EIf (p, ds_global c env, ds_global t env, ds_global e env)
  | ESeq (p, e1, e2) -> ESeq (p, ds_global e1 env, ds_global e2 env)
  | ELabel (p, l, e) -> ELabel (p, l, ds_global e env)
  | EBreak (p, l, e) -> EBreak (p, l, ds_global e env)
  | ETryCatch (p, e1, e2) -> ETryCatch (p, ds_global e1 env, ds_global e2 env)
  | ETryFinally (p, e1, e2) -> 
      ETryFinally (p, ds_global e1 env, ds_global e2 env)
  | EThrow (p, e) -> EThrow (p, ds_global e env)


let rec ds_top expr = ds expr
let rec desugar expr = ds_global (ds_op expr) IdMap.empty
