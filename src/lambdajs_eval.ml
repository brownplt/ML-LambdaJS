open Prelude
open Lambdajs_syntax

type value = 
  | Const of JavaScript_syntax.const
  | Cell of value ref
  | Closure of (value list -> value)
  | Object of value IdMap.t

type label = string

exception Break of label * value
exception Throw of value

let undef = Const JavaScript_syntax.CUndefined

let init_bind (x, _) env' = IdMap.add x (ref undef) env'

let bind_arg x v env = IdMap.add x (ref v) env

let rec get_field obj x = 
  try 
    IdMap.find x obj
  with Not_found -> 
    try
      begin match IdMap.find "__proto__" obj with
        | Cell c -> begin match !c with
            | Object proto -> get_field proto x
            | _ -> undef
          end
        | _ -> undef
      end
    with Not_found -> undef

let rec eval env exp = match exp with
  | EConst (_, c) -> Const c
  | EId (p, x) ->
      begin try !(IdMap.find x env)
      with Not_found -> failwith ("unbound identifier " ^ x)
      end
  | EObject (_, fields) ->
      let eval_field (_, x, e) map = IdMap.add x (eval env e) map in
        Object (List.fold_right eval_field fields IdMap.empty)
  | EUpdateField (_, obj, f_name, f_value) ->
      begin match eval env obj, eval env f_name with
        | Object map, Const (JavaScript_syntax.CString x) ->
            Object (IdMap.add x (eval env f_value) map)
        | _ -> failwith "EUpdateField"
      end
  | EIf (_, e1, e2, e3) ->
      begin match eval env e1 with
        | Const (JavaScript_syntax.CBool b) ->
            if b then eval env e2 else eval env e3
        | _ -> failwith "E-If"
      end
  | EApp (_, f, args) -> 
      begin match eval env f with
        | Closure proc -> proc (map (eval env) args)
        | _ -> failwith "E-App"
      end
  | ESeq (_, e1, e2) -> ignore (eval env e1); eval env e2
  | ELet (p, x, e1, e2) -> eval (IdMap.add x (ref (eval env e1)) env) e2
  | EFix (_, binds, body) ->
      let env = fold_right init_bind binds env in
      let set_bind (x, e) = 
        let bind_cell = IdMap.find x env in
          bind_cell := eval env e in
        List.iter set_bind binds;
        eval env body
  | ELabel (_, label, e) ->
      begin try
        eval env e
      with Break (label', v) ->
        if label = label' then v
        else raise (Break (label', v))
      end
  | EBreak (_, label, e) -> raise (Break (label, eval env e))
  | ETryCatch (_, body, ELambda (_, [x], catch_body)) ->
      begin try
        eval env body
      with Throw v -> eval (IdMap.add x (ref v) env) catch_body
      end
  | ETryCatch _ -> failwith "expected a Lambda for the catch handler"
  | ETryFinally (_, body, finally) ->
      begin try
        ignore (eval env body)
      with
        | Throw v -> ignore (eval env finally); raise (Throw v)
        | Break (l, v) -> ignore (eval env finally); raise (Break (l, v))
      end;
      eval env finally
  | EThrow (_, e) -> raise (Throw (eval env e))
  | ELambda (_, xs, e) ->
      let func vs = 
        (if (List.length vs != List.length xs) then failwith "arity-error");
        eval (List.fold_right2 bind_arg xs vs env) e in
      Closure func
  | EOp1 (_, op, e) ->
      begin match op, eval env e with
        | Op1Prefix s, _ -> failwith ("unelaborated operator: " ^ s)
        | Deref, Cell c -> !c
        | Ref, v -> Cell (ref v)
      end
  | EOp2 (_, op, e1, e2) -> 
      begin match op, eval env e1, eval env e2 with
        | Op2Infix s, _, _ -> failwith ("unelaborated operator: " ^ s)
        | UnsafeGetField, _, _ -> failwith "UnsafeGetField"
        | GetField, Object obj, Const (JavaScript_syntax.CString x) ->
            get_field obj x
        | DeleteField, Object obj, Const (JavaScript_syntax.CString x) ->
            Object (IdMap.remove x obj)
        | SetRef, Cell c, v -> c := v; (Cell c)
      end

          

        
      

