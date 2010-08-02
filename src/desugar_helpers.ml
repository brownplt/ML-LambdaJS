open Prelude
open Es5_syntax
open Exprjs_syntax

module S = JavaScript_syntax

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

let to_object p e = EApp (p, EId (p, "[[ToObject]]"), [e])
let to_string p e = EApp (p, EId (p, "[[ToString]]"), [e])

let rec mk_val p v =
  [(Value, v);
   (Enum, true_c p);
   (Config, true_c p);
   (Writable, true_c p)]

and mk_field (p, s, e) =
  match e with
    | _ -> (s, mk_val p e)

and mk_array (p, exps) = 
  let mk_num_field n v = (string_of_int n, 
		      mk_val p v) in
    EObject (p, [("proto", EId (p, "[[Array_prototype]]"));
		 ("extensible", true_c p);
		 ("class", str p "Array")],
	     ((mk_field (p, "length", int_c p (List.length exps))) ::
		List.map2 mk_num_field (iota (List.length exps)) exps))

(* 10.6 *)
and args_obj p arg_list = 
  let mk_field n v = (string_of_int n, 
		      mk_val p v) in
    EObject 
      (* 10.6 steps 4, 6 *)
      (p, [("proto", obj_proto p);
	   ("class", str p "Arguments");
	   ("extensible", false_c p)],
       (* 10.6 steps 1, 7 *)
       (("length", [(Value, int_c p (List.length arg_list));
		     (Writable, true_c p);
		     (Enum, false_c p);
		     (Config, true_c p)])::
	 (* 10.6 step 13a *)
	 ("callee", [(Getter, 
		      EId (p, "[[ThrowTypeError]]"));
		     (Setter, 
		      EId (p, "[[ThrowTypeError]]"));
		     (Enum, false_c p);
		     (Config, false_c p)])::
	 ("caller", [(Getter, 
		      EId (p, "[[ThrowTypeError]]"));
		     (Setter, 
		      EId (p, "[[ThrowTypeError]]"));
		     (Enum, false_c p);
		     (Config, false_c p)])::
	  (List.map2 mk_field (iota (List.length arg_list)) arg_list)))


(* Used by getters and setters---the function will be known at
runtime *)
and args_thunk p arg_list = 
  ELambda (p, ["func"],
	   args_obj p arg_list)


(* Same idea as in original \JS --- use the args array *)
and func_expr_lambda p ids body =
  let folder id ix e = 
    ELet (p, 
	  id,
	  EGetFieldSurface (p, 
			    EId (p, "arguments"),
			    EConst (p, S.CString (string_of_int ix)),
			    args_thunk p []),
	  e) in
    ELambda (p, 
	     ["this"; "arguments"],
	     List.fold_right2 folder ids (iota (List.length ids)) body)


(* Small extension to allow a named function to call itself.  I have
   not considered the repercussions of functions with the same name as
   an argument. *)

and func_stmt_lambda p func_name ids body = func_expr_lambda p ids body
(*  ELet (p, 
	func_name, 
	EId (p, "$funobj"),
	func_expr_lambda p ids body) *)
    
(* 13.2 *)
and func_object p ids lambda_exp =
  ELet (p, "$prototype", 
	EObject (p,
		 [("proto", obj_proto p);
		  ("extensible", true_c p);
		  ("class", str p "Object")],
		 [("constructor", 
		   [(Value, EConst (p, S.CUndefined));
		    (Writable, true_c p);
		    (Enum, false_c p);
		    (Config, true_c p)])]),
	ELet (p, "$funobj", 
	      EObject (p,
		       [("code", lambda_exp);
			("proto", fun_proto p);
			("extensible", true_c p);
			("class", str p "Function")],
		       [("length", 
			 [(Value, EConst (p, S.CNum
					      (float_of_int
						 (List.length ids))));
			  (Writable, false_c p);
			  (Enum, false_c p);
			  (Config, false_c p)]);
			("prototype",
			 [(Value, EId (p, "$prototype"));
			  (Writable, true_c p);
			  (Config, false_c p);
			  (Enum, false_c p)])]),
	      ESeq (p, EUpdateFieldSurface (p, 
					    EId (p, "$prototype"),
					    EConst (p, S.CString ("constructor")),
					    EId (p, "$funobj"),
					    args_thunk p [EId (p, "$funobj")]),
		    EId (p, "$funobj"))))
and new_obj p proto_id = 
  EObject (p,
	   [("proto", EId (p, proto_id));
	    ("extensible", true_c p);
	    ("class", str p "Object")],
	   [])
