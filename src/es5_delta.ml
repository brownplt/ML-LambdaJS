open Prelude
open Es5_syntax
open JavaScript_syntax
open Es5_values

let undef = Const JavaScript_syntax.CUndefined

let str s = Const (JavaScript_syntax.CString s)

let num f = Const (JavaScript_syntax.CNum f)

let bool b = Const (JavaScript_syntax.CBool b)

let get_const v = match v with
  | Const c -> c
  | _ -> raise (Throw (str "expected primtive constant"))

let to_int v = match v with
  | Const (CInt n) -> n
  | Const (CNum x) -> int_of_float x
  | _ -> raise (Throw (str "expected number"))

let to_float v = match v with
  | Const (CInt n) -> float_of_int n
  | Const (CNum x) -> x
  | _ -> raise (Throw (str "expected number"))

let typeof v = str begin match v with
  | Const c -> begin match c with
      | CUndefined -> "undefined"
      | CNull -> "null"
      | CString _ -> "string"
      | CNum _ -> "number"
      | CInt _ -> "number"
      | CBool _ -> "boolean"
    end
  | ObjCell _ -> "object"
  | Closure _ -> "lambda"
end

let surface_typeof v = str begin match v with
  | Const c -> begin match c with
      | CUndefined -> "undefined"
      | CNull -> "null"
      | CString _ -> "string"
      | CNum _ -> "number"
      | CInt _ -> "number"
      | CBool _ -> "boolean"
    end
  | ObjCell _ -> "object"
  | _ -> raise (Throw (str "surface_typeof"))
end
  
let is_primitive v = match v with
  | Const _ -> Const (CBool true)
  | _ -> Const (CBool false)

let prim_to_str v = str begin match v with
  | Const c -> begin match c with
      | CUndefined -> "undefined"
      | CNull -> "null"
      | CString s -> s
      | CNum n -> string_of_float n (* TODO: Fix for infs and nan (9.3.1) *)
      | CInt n -> string_of_int n
      | CBool b -> string_of_bool b
    end
  | _ -> raise (Throw (str "prim_to_str"))
end

(* Section 9.3, excluding objects *)
let prim_to_num v = num begin match v with
  | Const c -> begin match c with
      | CUndefined -> nan 
      | CNull -> 0.0
      | CBool true -> 1.0
      | CBool false -> 0.0
      | CNum x -> x
      | CInt n -> float_of_int n
      | CString s -> begin try float_of_string s
        with Failure _ -> nan end
    end
  | _ -> raise (Throw (str "prim_to_str"))
end
  
let prim_to_bool v = bool begin match v with
  | Const c -> begin match c with
      | CBool b -> b
      | CUndefined -> false
      | CNull -> false
      | CNum x -> not (x == nan || x = 0.0 || x = -0.0)
      | CInt n -> not (n = 0)
      | CString s -> not (String.length s = 0)
    end
  | _ -> true
end

let print v = match v with
  | Const (CString s) -> 
      printf "%S\n" s; Const CUndefined
  | _ -> failwith ("[interp] Print received non-string: " ^ pretty_value v)

let op1 op = match op with
  | "typeof" -> typeof
  | "surface-typeof" -> surface_typeof
  | "primitive?" -> is_primitive
  | "prim->str" -> prim_to_str
  | "prim->num" -> prim_to_num
  | "prim->bool" -> prim_to_bool
  | "print" -> print
  | _ -> failwith ("no implementation of unary operator: " ^ op)

let arith i_op f_op v1 v2 = match v1, v2 with
  | Const (CInt m), Const (CInt n) -> Const (CInt (i_op m n))
  | Const (CNum x), Const (CNum y) -> Const (CNum (f_op x y))
  | Const (CNum x), Const (CInt n) -> Const (CNum (f_op x (float_of_int n)))
  | Const (CInt m), Const (CNum y) -> Const (CNum (f_op (float_of_int m) y))
  | _ -> raise (Throw (str "arithmetic operator"))


let arith_sum = arith (+) (+.)

let arith_sub = arith (-) (-.)

(* OCaml syntax failure! Operator section syntax lexes as a comment. *)
let arith_mul = arith (fun m n -> m * n) (fun x y -> x *. y)

let arith_div x y = try arith (/) (/.) x y
with Division_by_zero -> Const (CNum infinity)

let arith_mod x y = try arith (mod) mod_float x y
with Division_by_zero -> Const (CNum nan)

let arith_lt x y = bool (to_float x < to_float y)

let arith_le x y = bool (to_float x <= to_float y)

let arith_gt x y = bool (to_float x > to_float y)

let arith_ge x y = bool (to_float x >= to_float y)

let bitwise_and v1 v2 = Const (CInt ((to_int v1) land (to_int v2)))

let bitwise_or v1 v2 = Const (CInt ((to_int v1) lor (to_int v2)))

let bitwise_xor v1 v2 = Const (CInt ((to_int v1) lxor (to_int v2)))

let bitwise_shiftl v1 v2 = Const (CInt ((to_int v1) lsl (to_int v2)))

let bitwise_zfshiftr v1 v2 = Const (CInt ((to_int v1) lsr (to_int v2)))

let bitwise_shiftr v1 v2 = Const (CInt ((to_int v1) asr (to_int v2)))

let string_plus v1 v2 = match v1, v2 with
  | Const (CString s1), Const (CString s2) ->
      Const (CString (s1 ^ s2))
  | _ -> raise (Throw (str "string concatenation"))

let stx_eq v1 v2 = bool begin match v1, v2 with
  | Const c1, Const c2 -> c1 = c2 (* syntactic on primitives *)
  | _ -> v1 == v2 (* otherwise, pointer equality *)
end

(* Algorithm 11.9.3, steps 1 through 19. Steps 20 and 21 are desugared to
   access the heap. *)
let abs_eq v1 v2 = bool begin
  let c1 = get_const v1 in
  let c2 = get_const v2 in
    if c1 = c2 then (* works correctly on floating point values *)
      true
    else match c1, c2 with
      | CNull, CUndefined
      | CUndefined, CNull -> true
      | CString s, CNum x
      | CNum x, CString s ->
          (try x = float_of_string s with Failure _ -> false)
      | CString s, CInt n
      | CInt n, CString s ->
          (try float_of_int n = float_of_string s with Failure _ -> false)
      | CNum x, CBool b
      | CBool b, CNum x -> x = (if b then 1.0 else 0.0)
      | CInt n, CBool b
      | CBool b, CInt n -> n = (if b then 1 else 0)
      | _ -> false
end

let has_own_property obj field = match obj, field with
  | ObjCell o, Const (CString s) -> 
      let (attrs, props) = !o in
	bool (IdMap.mem s props)
  | _ -> raise (Throw (str "has_own_property?"))

let op2 op = match op with
  | "+" -> arith_sum
  | "-" -> arith_sub
  | "/" -> arith_div
  | "*" -> arith_mul
  | "%" -> arith_mod
  | "&" -> bitwise_and
  | "|" -> bitwise_or
  | "^" -> bitwise_xor
  | "<<" -> bitwise_shiftl
  | ">>" -> bitwise_shiftr
  | ">>>" -> bitwise_zfshiftr
  | "<" -> arith_lt
  | "<=" -> arith_le
  | ">" -> arith_gt
  | ">=" -> arith_ge
  | "stx=" -> stx_eq
  | "abs=" -> abs_eq
  | "has-own-property?" -> has_own_property
  | "string+" -> string_plus
  | _ -> failwith ("no implementation of binary operator: " ^ op)


let define_property obj field attrobj = obj

let op3 op = match op with
  | "define_property" -> define_property
  | _ -> failwith ("no implementation of ternary operator: " ^ op)
