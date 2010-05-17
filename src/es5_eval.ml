open Prelude
open Es5_syntax
open JavaScript_syntax
open Es5_values
open Es5_delta

let rec apply func args = match func with
  | Closure c -> c args
  | _ -> failwith ("[interp] Applied non-function, was actually " ^ 
		     pretty_value func)

(* args should always be a single args object *)
let rec apply_obj o this args = match o with
  | ObjCell c -> 
      let (attrs, props) = !c in
	begin
	  try
	    let code_attr = IdMap.find "code" attrs in
	      apply code_attr [this; args]
	  with Not_found ->
	    failwith ("[interp] Applied an object with no code attr")
	end
  | _ -> failwith ("[interp] apply_obj given non-object")
	  

let rec get_field obj1 obj2 field args = match obj1 with
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
		    apply_obj getter obj2 args
		with Not_found -> Const (CUndefined) (* No getting attributes *)
	  with Not_found ->
	    try
	      get_field (IdMap.find "proto" attrs) obj2 field args
	    with Not_found ->
	      Const (CUndefined) (* No prototype found *)
	end
  | _ -> failwith ("[interp] get_field received (or found) a non-object.  The call was (get-field " ^ pretty_value obj1 ^ " " ^ pretty_value obj2 ^ " " ^ field)


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
let rec update_field obj1 obj2 field newval args = match obj1 with
    (* 8.12.4, step 4 *)
  | Const (CNull) -> add_field obj2 field newval
  | ObjCell c -> let (attrs, props) = !c in
      if (not (IdMap.mem field props)) then
	if (IdMap.mem "proto" attrs) then
	  (* EUpdateField-Proto *)
	  update_field (IdMap.find "proto" attrs) obj2 field newval args
	else
	  (* 8.12.4, step 4, sort of.  Handles if proto doesn't exist *)
	  add_field obj2 field newval
      else
	let prop = (IdMap.find field props) in
	  if writable prop then 
	    if (not (obj1 == obj2)) then
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
	      apply_obj setter obj2 args
	  with Not_found -> 
	    (* TODO: Make type error for strict. Not writable, no setter. *)
	    Const CUndefined
	  end
  | _ -> failwith ("[interp] set_field received (or found) a non-object.  The call was (set-field " ^ pretty_value obj1 ^ " " ^ pretty_value obj2 ^ " " ^ field ^ " " ^ pretty_value newval ^ ")" )



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
	failwith ("[interp] Unbound identifier: " ^ x ^ " in identifier lookup at " ^
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
	failwith ("[interp] Unbound identifier: " ^ x ^ " in set! at " ^
		    (string_of_position p))
    end
  | EObject (p, attrs, props) ->
      let eval_attr (name, e) m = IdMap.add name (eval e env) m in
      let eval_prop (p, name, attrs) m = 
	IdMap.add name (fold_right eval_attr attrs IdMap.empty) m in
	ObjCell (ref (fold_right eval_attr attrs IdMap.empty,
		      fold_right eval_prop props IdMap.empty))
  | EUpdateFieldSurface (p, obj, f, v, args) ->
      let obj_value = eval obj env in
      let f_value = eval f env in
      let v_value = eval v env in 
      let args_value = eval args env in begin
	match (obj_value, f_value) with
	  | (ObjCell o, Const (CString s)) ->
	      update_field obj_value 
		obj_value 
		s
		v_value
		args_value
	  | _ -> failwith ("[interp] Update field didn't get an object and a string")
	end
  | EGetFieldSurface (p, obj, f, args) ->
      let obj_value = eval obj env in
      let f_value = eval f env in 
      let args_value = eval args env in begin
	match (obj_value, f_value) with
	  | (ObjCell o, Const (CString s)) ->
	      get_field obj_value obj_value s args_value
	  | _ -> failwith ("[interp] Get field didn't get an object and a string at " ^ string_of_position p ^ ". Instead, it got " ^ pretty_value obj_value ^ " and " ^ pretty_value f_value)
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
      let e_val = eval e env in
	begin match op with
	  | Prim1 str -> op1 str e_val
	  | _ -> failwith ("[interp] Invalid EOp1 form")
	end
  | EOp2 (p, op, e1, e2) -> 
      let e1_val = eval e1 env in
      let e2_val = eval e2 env in
	begin match op with
	  | Prim2 str -> op2 str e1_val e2_val
	  | _ -> failwith ("[interp] Invalid EOp2 form")
	end
  | EOp3 (p, op, e1, e2, e3) -> 
      let e1_val = eval e1 env in
      let e2_val = eval e2 env in
      let e3_val = eval e3 env in
	begin match op with
	  | Prim3 str -> op3 str e1_val e2_val e3_val
	  | _ -> failwith ("[interp] Invalid EOp3 form")
	end
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
	      if List.length args_values < 2 then
		failwith ("[interp] Need to provide this and args for a call to a function object at " ^ string_of_position p)
	      else
		apply_obj func_value 
		  (List.hd args_values) 
		  (List.nth args_values 1)
	  | Closure c -> apply func_value args_values
	  | _ -> failwith ("[interp] Inapplicable value: " ^ pretty_value func_value ^ ", at " ^ string_of_position p)
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
	ignore (eval body env)
      with
	| Throw v -> ignore (eval fin env); raise (Throw v)
	| Break (l, v) -> ignore (eval fin env); raise (Break (l, v))
    end;
      eval fin env
  | EThrow (p, e) -> raise (Throw (eval e env))
  | ELambda (p, xs, e) -> 
      let set_arg arg x m = IdMap.add x (VarCell (ref arg)) m in
	Closure (fun args -> 
		     if (List.length args) != (List.length xs) then
		       arity_mismatch_err p xs args
		     else
		     eval e (List.fold_right2 set_arg args xs env))
  | EUpdateField (_,_,_,_,_,_) -> failwith ("Not implemented---EUpdateField")
  | EGetField (_,_,_,_,_) -> failwith ("Not implemented---EGetField")

and arity_mismatch_err p xs args = failwith ("Arity mismatch, supplied " ^ string_of_int (List.length args) ^ " arguments and expected " ^ string_of_int (List.length xs) ^ " at " ^ string_of_position p ^ ". Arg names were: " ^ (List.fold_right (^) (map (fun s -> " " ^ s ^ " ") xs) "") ^ ". Values were: " ^ (List.fold_right (^) (map (fun v -> " " ^ pretty_value v ^ " ") args) ""))

let rec eval_expr expr = try 
  eval expr IdMap.empty
with
  | Throw v -> failwith ("Uncaught exception of type " ^ pretty_value v)
  | Break (l, v) -> failwith ("Broke to top of execution, missed label: " ^ l)
