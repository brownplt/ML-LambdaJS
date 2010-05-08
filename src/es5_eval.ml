open Prelude
open Es5_syntax
open JavaScript_syntax

module SMap = Map.Make (String)

type value =
  | Const of JavaScript_syntax.const
      (* A VarCell can contain an ObjCell, but not vice versa.  This
      mimics the semantics of heap-like object refs alongside mutable
      variables *)
  | VarCell of value ref 
      (* Objects shouldn't have VarCells in them, but can have any of
      the other kinds of values *)
  | ObjCell of (value IdMap.t * ((value IdMap.t) IdMap.t)) ref
  | Closure of (value list -> value)

type env = value IdMap.t
type label = string

exception Break of label * value
exception Throw of value

let rec apply func args = match func with
  | Closure c -> c args
  | _ -> failwith ("[interp] Applied non-function")

let rec args_object args =
  let add_arg arg n m = IdMap.add (string_of_int n)
    (IdMap.add "value" arg 
       (IdMap.add "configurable" (Const (CBool false))
	  (IdMap.add "enumerable" (Const (CBool true))
	     (IdMap.add "writable" (Const (CBool false))
		IdMap.empty)))) m in
  let attrs = IdMap.add "Class" (Const (CString "Arguments"))
    (IdMap.add "extensible" (Const (CBool false))
       IdMap.empty) in
    ObjCell (ref (attrs, 
		  List.fold_right2 add_arg args 
		    (iota (List.length args)) IdMap.empty))

let rec apply_obj o this args = match o with
  | ObjCell c -> 
      let (attrs, props) = !c in
	begin
	  try
	    let code_attr = IdMap.find "code" attrs in
	      apply code_attr [this; (args_object args)]
	  with Not_found ->
	    failwith ("[interp] Applied an object with no code attr")
	end
  | _ -> failwith ("[interp] apply_obj given non-object")
	  

let rec get_field obj1 obj2 field = match obj1 with
  | Const (CNull) -> Const (CUndefined) (* nothing found *)
  | ObjCell c ->
      let (attrs, props) = !c in begin
	try
	  let prop_attrs = IdMap.find field props in
	    try 
	      let value = IdMap.find "value" prop_attrs in
		value
	    with Not_found ->
	      try
		let getter = IdMap.find "get" prop_attrs in
		  apply_obj getter obj2 []
	      with Not_found -> Const (CUndefined) (* No getting attributes *)
	with Not_found ->
	  try
	    get_field (IdMap.find "proto" attrs) obj2 field
	  with Not_found ->
	    Const (CUndefined) (* No prototype found *)
	end
  | _ -> failwith ("[interp] get_field received (or found) a non-object")


let rec update_field obj1 obj2 field newval = match obj1 with
    (* The "add" case *)
  | Const (CNull) -> begin match obj2 with
	ObjCell c -> let (attrs, props) = !c in
	  if IdMap.mem "extensible" attrs &&
	    IdMap.find "extensible" attrs = (Const (CBool true))
	  then begin
	    c := (attrs, IdMap.add field 
		    (IdMap.add "value" newval
		       (IdMap.add "configurable" (Const (CBool true))
			  (IdMap.add "writable" (Const (CBool true))
			     (IdMap.add "enumerable" (Const (CBool true))
				IdMap.empty))))
		    props);
	    newval
	  end
	  else
	    Const (CUndefined)
    end
  | ObjCell c -> 
      let (attrs, props) = !c in begin
	  try
	    let prop = IdMap.find field props in begin
		try
		  if (IdMap.find "writable" prop) = Const (CBool true) then
		    begin
		      c := (attrs, IdMap.add field
			      (IdMap.add "value" newval prop)
			      props);
		      newval
		    end
		  else
		    Const (CUndefined) (* TypeError? *)
		with Not_found -> begin
		  try
		    let setter = IdMap.find "set" prop in
		      apply_obj setter obj2 [newval]
		  with Not_found -> Const (CUndefined) (* TypeError? *)
		end
	      end
	  with Not_found -> begin
	    try
	      let proto = IdMap.find "proto" attrs in
		update_field proto obj2 field newval
	    with Not_found -> Const (CUndefined)
	  end
	end
				   
	    
  | ObjCell c ->
      let (attrs, props) = !c in begin
	try
	  let prop_attrs = IdMap.find field props in
	    try 
	      let value = IdMap.find "value" prop_attrs in
		value
	    with Not_found ->
	      try
		let getter = IdMap.find "get" prop_attrs in
		  apply_obj getter obj2 []
	      with Not_found -> Const (CUndefined) (* No getting attributes *)
	with Not_found ->
	  try
	    get_field (IdMap.find "proto" attrs) obj2 field
	  with Not_found ->
	    Const (CUndefined) (* No prototype found *)
	end
  | _ -> failwith ("[interp] get_field received (or found) a non-object")


let rec eval exp env = match exp with
  | EConst (p, c) -> Const (c)
  | EId (p, x) -> begin
      try
	match IdMap.find x env with
	  | VarCell v -> !v
	  | _ -> failwith ("[interp] Expected a VarCell for variable " ^ x ^ 
			     " at " ^ (string_of_position p) ^ 
			     ", but found something else.")
      with Not_found ->
	failwith ("[interp] Unbound identifier: " ^ x ^ " at " ^
		    (string_of_position p))
    end
  | ESet (p, x, e) -> begin
      try
	match IdMap.find x env with
	  | VarCell v -> v := eval e env; !v
	  | _ -> failwith ("[interp] Expected a VarCell for variable " ^ x ^ 
			     " at " ^ (string_of_position p) ^ 
			     ", but found something else.")
      with Not_found ->
	failwith ("[interp] Unbound identifier: " ^ x ^ " at " ^
		    (string_of_position p))
    end
  | EObject (p, attrs, props) ->
      let eval_attr (name, e) m = IdMap.add name (eval e env) m in
      let eval_prop (p, name, attrs) m = 
	IdMap.add name (fold_right eval_attr attrs IdMap.empty) m in
	ObjCell (ref (fold_right eval_attr attrs IdMap.empty,
		      fold_right eval_prop props IdMap.empty))
  | EUpdateFieldSurface (p, obj, f, v) ->
      let obj_value = eval obj env in
      let f_value = eval f env in
      let v_value = eval v env in begin
	match (obj_value, f_value) with
	  | (ObjCell o, Const (CString s)) ->
	      update_field obj_value 
		obj_value 
		s
		v_value
	  | _ -> failwith ("[interp] Update field didn't get an object and a string")
	end
  | EGetFieldSurface (p, obj, f) ->
      let obj_value = eval obj env in
      let f_value = eval f env in begin
	match (obj_value, f_value) with
	  | (ObjCell o, Const (CString s)) ->
	      get_field obj_value obj_value s
	  | _ -> failwith ("[interp] Get field didn't get an object and a string")
	end
  | EDeleteField (p, obj, f) ->
      let obj_val = eval obj env in
      let f_val = eval f env in begin
	match (obj_val, f_val) with
	  | (ObjCell c, Const (CString s)) ->
	      let (attrs,props) = !c in
		if IdMap.mem s props 
		  && IdMap.mem "configurable" attrs
		  && (IdMap.find "configurable" attrs) = Const (CBool true)
		then begin
		  c := (attrs, IdMap.remove s props);
		  Const (CBool true)
		end
		else Const (CBool false)
	  | _ -> failwith ("[interp] EDeleteField didn't get an object and string at " ^
			     string_of_position p)
	end
  | EOp1 (p, op, e) ->
      let eval_e = eval e env in
	Const (CString ("( ... )"))
  | EOp2 (p, op, e1, e2) ->
      let e1_val = eval e1 env in
      let e2_val = eval e2 env in
	Const (CString ("( ... )"))
  | EIf (p, c, t, e) ->
      let c_val = eval c env in
	if (c_val = Const (CBool true))
	then eval t env
	else eval e env
  | EApp (p, func, args) -> apply (eval func env) (map (fun e -> eval e env) args)
  | ESeq (p, e1, e2) ->
      let e1_val = eval e1 env in
	eval e2 env
  | ELet (p, x, e, body) ->
      let e_val = eval e env in
	eval body (IdMap.add x (VarCell (ref e_val)) env)
  | EFix (p, x, e) ->
      eval e (IdMap.add x (Closure (fun args -> eval (EFix (p, x, e)) env)) env)
  | ELabel (p, l, e) -> begin
      try
	eval e env
      with Break (l', v) ->
	if l = l' then v
	else raise (Break (l', v))
    end
  | EBreak (p, l, e) ->
      raise (Break (l, eval e env))
  | ETryCatch (p, body, catch) -> begin
      try
	eval body env
      with Throw v -> apply (eval catch env) [v]
    end
  | ETryFinally (p, body, fin) -> begin
      try
	eval body env
      with
	| Throw v -> eval fin env; raise (Throw v)
	| Break (l, v) -> eval fin env; raise (Break (l, v))
    end
  | EThrow (p, e) -> raise (Throw (eval e env))
  | ELambda (p, xs, e) ->
      Closure (fun args -> 
		 let set_arg arg x m = IdMap.add x arg m in
		   eval e (List.fold_right2 set_arg args xs env))
  | EUpdateField (_,_,_,_,_) -> failwith ("Not implemented---EUpdateField")
  | EGetField (_,_,_,_) -> failwith ("Not implemented---EGetField")


let rec eval_expr expr = eval expr IdMap.empty
