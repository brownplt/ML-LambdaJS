open Prelude
open Es5_syntax
open JavaScript_syntax
open Es5_values

let pretty_value v = match v with 
  | Const c -> begin match c with
      | CInt d -> string_of_int d
      | CNum d -> string_of_float d
      | CString s -> s
      | CBool b -> string_of_bool b
      | CUndefined -> "undefined"
      | CNull -> "null"
    end
  | Closure c -> "function"
  | ObjCell o -> "object"
  | _ -> failwith ("Only prims defined")

let rec apply func args = match func with
  | Closure c -> c args
  | _ -> failwith ("[interp] Applied non-function, was actually " ^ 
		     pretty_value func)

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


(* EUpdateField-Add *)
(* ES5 8.12.5, step 6 *)
let rec add_field obj field newval = match obj with
  | ObjCell c -> let (attrs, props) = !c in
      if IdMap.mem "extensible" attrs &&
	((IdMap.find "extensible" attrs) = (Const (CBool true))) then begin
	  c := (attrs, IdMap.add field 
		  (IdMap.add "value" newval
		     (IdMap.add "configurable" (Const (CBool true))
			(IdMap.add "writable" (Const (CBool true))
			   (IdMap.add "enumerable" (Const (CBool true))
			      IdMap.empty))))
		  props);
	  newval
	end
      else Const CUndefined	
  | _ -> failwith ("[interp] add_field given non-object.")

let rec writable prop = 
  (IdMap.mem "writable" prop) &&
    ((IdMap.find "writable" prop) = Const (CBool true))

let rec not_writable prop = 
  (IdMap.mem "writable" prop) &&
    ((IdMap.find "writable" prop) = Const (CBool false))

(* EUpdateField *)
(* ES5 8.12.4, 8.12.5 *)
let rec update_field obj1 obj2 field newval = match obj1 with
    (* 8.12.4, step 4 *)
  | Const (CNull) -> add_field obj2 field newval
  | ObjCell c -> let (attrs, props) = !c in
      if (not (IdMap.mem field props)) then
	if (IdMap.mem "proto" attrs) then
	  (* EUpdateField-Proto *)
	  update_field (IdMap.find "proto" attrs) obj2 field newval
	else
	  (* 8.12.4, step 4, sort of.  Handles if proto doesn't exist *)
	  add_field obj2 field newval
      else
	let prop = (IdMap.find field props) in
	  if writable prop then 
	    if (not (obj1 = obj2)) then
	      (* 8.12.4, last step where inherited.[[writable]] is true *)
	      add_field obj2 field newval
	    else begin
	      (* 8.12.5, step 3 *)
	      c := (attrs, IdMap.add field
		      (IdMap.add "value" newval prop)
		      props);
	      newval
	    end
	  else begin try
	    (* 8.12.5, step 5 *)
	    let setter = IdMap.find "set" prop in
	      apply_obj setter obj2 [newval]
	  with Not_found -> 
	    (* TODO: Make type error for strict. Not writable, no setter. *)
	    Const CUndefined
	  end
  | _ -> failwith ("[interp] set_field received (or found) a non-object")


let rec eval exp env = match exp with
  | EConst (p, c) -> Const (c)
  | EId (p, x) -> begin
      try
	match IdMap.find x env with
	  | VarCell v -> !v
	  | _ -> failwith ("[interp] (EId) Expected a VarCell for variable " ^ x ^ 
			     " at " ^ (string_of_position p) ^ 
			     ", but found something else: " ^ pretty_value (IdMap.find x env))
      with Not_found ->
	failwith ("[interp] Unbound identifier: " ^ x ^ " at " ^
		    (string_of_position p))
    end
  | ESet (p, x, e) -> begin
      try
	match IdMap.find x env with
	  | VarCell v -> v := eval e env; !v
	  | _ -> failwith ("[interp] (ESet) Expected a VarCell for variable " ^ x ^ 
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
  | EApp (p, func, args) -> 
      let func_value = eval func env in
      let args_values = map (fun e -> eval e env) args in begin
	match func_value with
	  | ObjCell o -> 
	      if List.length args_values < 1 then
		failwith ("[interp] Need to provide at least a this-value")
	      else
		apply_obj func_value 
		  (List.hd args_values) (List.tl args_values)
	  | Closure c -> apply func_value args_values
	  | _ -> failwith ("[interp] Inapplicable value.")
	end
  | ESeq (p, e1, e2) -> 
      eval e1 env;
      eval e2 env
  | ELet (p, x, e, body) ->
      let e_val = eval e env in
	eval body (IdMap.add x (VarCell (ref e_val)) env)
  | EFix (p, x, e) ->
      let x_var = ref (Const CUndefined) in
      let e_val = eval e (IdMap.add x (VarCell x_var) env) in begin
	  x_var := e_val;
	  e_val
	end
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
		 let set_arg arg x m = IdMap.add x (VarCell (ref arg)) m in
		   eval e (List.fold_right2 set_arg args xs env))
  | EUpdateField (_,_,_,_,_) -> failwith ("Not implemented---EUpdateField")
  | EGetField (_,_,_,_) -> failwith ("Not implemented---EGetField")


let rec eval_expr expr = eval expr IdMap.empty
