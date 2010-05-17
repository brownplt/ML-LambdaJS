open Prelude
open Es5_syntax
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

(* Same idea as in original \JS --- use the args array *)
(* In strict mode, we aren't supposed to access the args array... *)

let rec func_expr_lambda p ids body =
  let folder id ix e = 
    ELet (p, 
	  id,
	  EGetFieldSurface (p, 
			    EId (p, "args"), 
			    EConst (p, S.CString (string_of_int ix)),
			    args_obj p []),
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
					    EId (p, "$funobj"),
					    args_obj p [EId (p, "$funobj")]),
		    EId (p, "$funobj"))))
let new_obj p proto_id = 
  EObject (p,
	   [("proto", EId (p, proto_id));
	    ("extensible", true_c p);
	    ("Class", str p "Object")],
	   [])
